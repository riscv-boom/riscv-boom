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
import boom.util.{BoolToChar, MaskUpper}

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

  val head = RegInit(1.U(numRows.W))
  val tail = RegInit(1.U(numEntries.W))

  val maybe_full = RegInit(false.B)

  //-------------------------------------------------------------
  // **** Enqueue Uops ****
  //-------------------------------------------------------------
  // Step 1: Convert FetchPacket into a vector of MicroOps.
  // Step 2: Generate one-hot write indices.
  // Step 3: Write MicroOps into the RAM.

  def rotateLeft(in: UInt, k: Int) = {
    val n = in.getWidth
    Cat(in(n-k-1,0), in(n-1, n-k))
  }

  val might_hit_head = (1 until fetchWidth).map(k => VecInit(rotateLeft(tail, k).asBools.zipWithIndex.filter
    {case (e,i) => i % coreWidth == 0}.map {case (e,i) => e}).asUInt).map(tail => head & tail).reduce(_|_).orR
  val at_head = (VecInit(tail.asBools.zipWithIndex.filter {case (e,i) => i % coreWidth == 0}
    .map {case (e,i) => e}).asUInt & head).orR
  val do_enq = !(at_head && maybe_full || might_hit_head)

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

  var enq_idx = tail
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

  //-------------------------------------------------------------
  // **** Dequeue Uops ****
  //-------------------------------------------------------------

  val tail_collisions = VecInit((0 until numEntries).map(i =>
                          head(i/coreWidth) && (!maybe_full || (i % coreWidth != 0).B))).asUInt & tail
  val slot_will_hit_tail = (0 until numRows).map(i => tail_collisions((i+1)*coreWidth-1, i*coreWidth)).reduce(_|_)
  val will_hit_tail = slot_will_hit_tail.orR

  val do_deq = io.deq.ready && !will_hit_tail

  val deq_valids = (~MaskUpper(slot_will_hit_tail)).asBools

  // Generate vec for dequeue read port.
  for (i <- 0 until numEntries) {
    deq_vec(i/coreWidth)(i%coreWidth) := ram(i)
  }

  io.deq.bits.uops zip deq_valids           map {case (d,v) => d.valid := v}
  io.deq.bits.uops zip Mux1H(head, deq_vec) map {case (d,q) => d.bits  := q}
  io.deq.valid := deq_valids.reduce(_||_)

  //-------------------------------------------------------------
  // **** Update State ****
  //-------------------------------------------------------------

  when (do_enq) {
    tail := enq_idx
    when (in_mask.reduce(_||_)) {
      maybe_full := true.B
    }
  }

  when (do_deq) {
    head := inc(head)
    maybe_full := false.B
  }

  when (io.clear) {
    head := 1.U
    tail := 1.U
    maybe_full := false.B
  }

  // TODO Is this necessary?
  when (reset.toBool) {
    io.deq.bits.uops map { u => u.valid := false.B }
  }

  //-------------------------------------------------------------
  // **** Printfs ****
  //-------------------------------------------------------------

  if (DEBUG_PRINTF) {
    printf("FetchBuffer:\n")
    // TODO a problem if we don't check the f3_valid?
    printf("    Fetch3: Enq:(V:%c Msk:0x%x PC:0x%x) Clear:%c\n",
      BoolToChar(io.enq.valid, 'V'),
      io.enq.bits.mask,
      io.enq.bits.pc,
      BoolToChar(io.clear, 'C'))

    printf("    RAM: WPtr:%d RPtr:%d\n",
      tail,
      head)

    printf("    Fetch4: Deq:(V:%c PC:0x%x)\n",
      BoolToChar(io.deq.valid, 'V'),
      io.deq.bits.uops(0).bits.pc)
  }
}
