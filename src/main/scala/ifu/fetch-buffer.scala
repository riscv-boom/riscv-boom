//******************************************************************************
// Copyright (c) 2018 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Fetch Buffer
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Takes a FetchBundle and converts into a vector of MicroOps.

package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.{dontTouch}
import chisel3.core.{DontCare}

import freechips.rocketchip.config.{Parameters}

import boom.common._
import boom.util.{BoolToChar, WrapInc}

/**
 * Bundle that is made up of converted MicroOps from the Fetch Bundle
 * input to the Fetch Buffer. This is handed to the Decode stage.
 */
class FetchBufferResp(implicit p: Parameters) extends BoomBundle
{
  val uops = Vec(coreWidth, Valid(new MicroOp()))
}

/**
 * Buffer to hold fetched packets and convert them into a vector of MicroOps
 * to give the Decode stage
 *
 * @param num_entries effectively the number of full-sized fetch packets we can hold.
 */
class FetchBuffer(numEntries: Int)(implicit p: Parameters) extends BoomModule
  with HasBoomCoreParameters
  with HasL1ICacheBankedParameters
{
  val io = IO(new BoomBundle {
    val enq = Flipped(Decoupled(new FetchBundle()))
    val deq = new DecoupledIO(new FetchBufferResp())

    // Was the pipeline redirected? Clear/reset the fetchbuffer.
    val clear = Input(Bool())
  })

  require (numEntries > fetchWidth)
  require (numEntries % coreWidth == 0)
  val numRows = numEntries / coreWidth

  val ram = Reg(Vec(numEntries, new MicroOp))
  ram.suggestName("fb_uop_ram")
  val deq_vec = Wire(Vec(numRows, Vec(coreWidth, new MicroOp)))

  val head = RegInit(0.U(log2Ceil(numRows).W))
  val tail = RegInit(1.U(numEntries.W))

  val count = RegInit(0.U(log2Ceil(numEntries).W))

  //-------------------------------------------------------------
  // **** Enqueue Uops ****
  //-------------------------------------------------------------
  // Step 1: Convert FetchPacket into a vector of MicroOps.
  // Step 2: Generate one-hot write indices.
  // Step 3: Write MicroOps into the RAM.

  val do_enq = count < (numEntries-fetchWidth).U
  io.enq.ready := do_enq

  // Input microops.
  val in_mask = Wire(Vec(fetchWidth, Bool()))
  val in_uops = Wire(Vec(fetchWidth, new MicroOp()))

  // Step 1: Convert FetchPacket into a vector of MicroOps.
  for (i <- 0 until fetchWidth) {
    in_uops(i)                := DontCare
    in_mask(i)                := io.enq.valid && io.enq.bits.mask(i)
    in_uops(i).edge_inst      := false.B
    in_uops(i).pc             := (alignToFetchBoundary(io.enq.bits.pc)
                                + (i << log2Ceil(coreInstBytes)).U)
    in_uops(i).pc_lob         := in_uops(i).pc // LHS width will cut off high-order bits.
    if (i == 0) {
      when (io.enq.bits.edge_inst) {
        assert(usingCompressed.B)
        in_uops(i).pc       := alignToFetchBoundary(io.enq.bits.pc) - 2.U
        in_uops(i).pc_lob   := alignToFetchBoundary(io.enq.bits.pc)
        in_uops(i).edge_inst:= true.B
      }
    }
    in_uops(i).ftq_idx        := io.enq.bits.ftq_idx
    in_uops(i).debug_inst     := io.enq.bits.insts(i)
    in_uops(i).is_rvc         := io.enq.bits.insts(i)(1,0) =/= 3.U && usingCompressed.B
    in_uops(i).xcpt_pf_if     := io.enq.bits.xcpt_pf_if
    in_uops(i).xcpt_ae_if     := io.enq.bits.xcpt_ae_if
    in_uops(i).replay_if      := io.enq.bits.replay_if
    in_uops(i).xcpt_ma_if     := io.enq.bits.xcpt_ma_if_oh(i)
    in_uops(i).br_prediction  := io.enq.bits.bpu_info(i)
    in_uops(i).debug_events   := io.enq.bits.debug_events(i)
  }

  // Step 2. Generate one-hot write indices.
  val enq_idxs = Wire(Vec(fetchWidth, UInt(numEntries.W)))

  def inc(ptr: UInt) = {
    val n = ptr.getWidth
    Cat(ptr(n-2,0), ptr(n-1))
  }

  var enq_idx = head
  for (i <- 0 until fetchWidth) {
    enq_idxs(i) := enq_idx
    enq_idx = Mux(in_mask(i), inc(enq_idx), enq_idx)
  }

  // Step 3: Write MicroOps into the RAM.
  for (i <- 0 until fetchWidth) {
    for (j <- 0 until numEntries) {
      when (do_enq && in_mask(i) && enq_idxs(i)(j)) {
        ram(j) := in_uops(i)
      }
    }
  }

  // all enqueuing uops have been compacted.
  // How many incoming uops are there?
  val popc_enqmask = PopCount(in_mask)
  // What is the count of uops being added to the ram?
  val enq_count = Mux(do_enq, popc_enqmask, 0.U)

  //-------------------------------------------------------------
  // **** Dequeue Uops ****
  //-------------------------------------------------------------

  val do_deq = io.deq.ready && count >= coreWidth.U

  // Generate vec for dequeue read port.
  for (i <- 0 until numEntries) {
    deq_vec(i/coreWidth)(i%coreWidth) := ram(i)
  }

  val val_count = Wire(UInt(coreWidth.W))
  val_count := Mux(count < coreWidth.U, 0.U, coreWidth.U)
  val deq_count = Mux(do_deq, coreWidth.U, 0.U)

  val val_count_oh = UIntToOH(val_count)
  val deq_vals = (1 to coreWidth).map(i => val_count_oh >> i.U).reduce(_|_).asBools
  io.deq.bits.uops zip deq_vals      map {case (d,q) => d.valid := q}
  io.deq.bits.uops zip deq_vec(head) map {case (d,q) => d.bits  := q}

  //-------------------------------------------------------------
  // **** Update State ****
  //-------------------------------------------------------------

  count := count + enq_count - deq_count

  when (do_enq) {
    tail := enq_idx
  }

  when (do_deq) {
    head := WrapInc(head, numRows)
  }

  when (io.clear) {
    count := 0.U
    head := 0.U
    tail := 1.U
  }

  when (reset.toBool) {
    io.deq.bits.uops map { u => u.valid := false.B }
  }

  //-------------------------------------------------------------
  // **** Printfs ****
  //-------------------------------------------------------------

  if (DEBUG_PRINTF) {
    printf("FetchBuffer:\n")
    // TODO a problem if we don't check the f3_valid?
    printf("    Fetch3: Enq:(V:%c Msk:0x%x PC:0x%x EnqCnt:%d) Clear:%c\n",
      BoolToChar(io.enq.valid, 'V'),
      io.enq.bits.mask,
      io.enq.bits.pc,
      enq_count,
      BoolToChar(io.clear, 'C'))

    printf("    RAM: Cnt:%d WPtr:%d RPtr:%d\n",
      count,
      tail,
      head)

    printf("    Fetch4: Deq:(V:%c DeqCnt:%d PC:0x%x)\n",
      BoolToChar(io.deq.valid, 'V'),
      deq_count,
      io.deq.bits.uops(0).bits.pc)
  }

  //-------------------------------------------------------------
  // **** Asserts ****
  //-------------------------------------------------------------

  assert (count >= deq_count, "[fetchbuffer] Trying to dequeue more uops than are available.")
  //assert (!(count === 0.U && OHToUInt(tail) =/= head), "[fetchbuffer] pointers should match if count is zero.")
}
