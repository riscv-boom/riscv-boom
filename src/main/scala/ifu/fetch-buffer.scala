//******************************************************************************
// Copyright (c) 2018 - 2018, The Regents of the University of California (Regents).
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
import chisel3.experimental.dontTouch
import chisel3.core.DontCare

import freechips.rocketchip.config.Parameters

import boom.common._

/**
 * Bundle that is made up of converted MicroOps from the Fetch Bundle
 * input to the Fetch Buffer. This is handed to the Decode stage.
 */
class FetchBufferResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val uops = Vec(decodeWidth, new MicroOp())
}

/**
 * Buffer to hold fetched packets and convert then into a vector of MicroOps
 * to give the Decode stage
 *
 * @param num_entries effectively the number of full-sized fetch packets we can hold.
 */
class FetchBuffer(num_entries: Int)(implicit p: Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
   with HasL1ICacheBankedParameters
{
   val io = IO(new BoomBundle()(p)
   {
      val enq = Flipped(Decoupled(new FetchBundle()))
      val deq = new DecoupledIO(new FetchBufferResp())

      // Was the pipeline redirected? Clear/reset the fetchbuffer.
      val clear = Input(Bool())
   })

   require (num_entries > 1)
   private val num_elements = num_entries*fetchWidth
   private val ram = Mem(num_elements, new MicroOp())
   private val write_ptr = RegInit(0.U(log2Ceil(num_elements).W))
   private val read_ptr = RegInit(0.U(log2Ceil(num_elements).W))

   // How many uops are stored within the ram? If zero, bypass to the output flops.
   private val count = RegInit(0.U(log2Ceil(num_elements).W))

   //-------------------------------------------------------------
   // **** Enqueue Uops ****
   //-------------------------------------------------------------
   // Step 1: convert FetchPacket into a vector of MicroOps.
   // Step 2: Compact/shift all MicroOps down towards index=0 (compress out any invalid MicroOps).
   // Step 3: Write CompactedMicroOps into the RAM.

   io.enq.ready := count < (num_elements-fetchWidth).U

   // Input microops.
   val in_uops = Wire(Vec(fetchWidth, new MicroOp()))

   // Compacted/shifted microops (and the shifted valid mask).
   val compact_mask = Wire(Vec(fetchWidth, Bool()))
   val compact_uops = Wire(Vec(fetchWidth, new MicroOp()))

   for (i <- 0 until fetchWidth)
   {
      compact_mask(i) := false.B
      compact_uops(i) := DontCare
      compact_uops(i).inst := 7.U
   }

   // Step 1. Convert input FetchPacket into an array of MicroOps.
   for (i <- 0 until fetchWidth)
   {
      in_uops(i)                := DontCare
      in_uops(i).valid          := io.enq.valid && io.enq.bits.mask(i)
      in_uops(i).edge_inst      := false.B
      in_uops(i).pc             := (alignToFetchBoundary(io.enq.bits.pc)
                                  + (i << log2Ceil(coreInstBytes)).U)
      in_uops(i).pc_lob         := in_uops(i).pc // LHS width will cut off high-order bits.
      if (i == 0)
      {
         when (io.enq.bits.edge_inst)
         {
            assert(usingCompressed.B)
            in_uops(i).pc       := alignToFetchBoundary(io.enq.bits.pc) - 2.U
            in_uops(i).pc_lob   := alignToFetchBoundary(io.enq.bits.pc)
            in_uops(i).edge_inst:= true.B
         }
      }
      in_uops(i).ftq_idx        := io.enq.bits.ftq_idx
      in_uops(i).inst           := io.enq.bits.insts(i)
      in_uops(i).is_rvc         := io.enq.bits.insts(i)(1,0) =/= 3.U && usingCompressed.B
      in_uops(i).xcpt_pf_if     := io.enq.bits.xcpt_pf_if
      in_uops(i).xcpt_ae_if     := io.enq.bits.xcpt_ae_if
      in_uops(i).replay_if      := io.enq.bits.replay_if
      in_uops(i).xcpt_ma_if     := io.enq.bits.xcpt_ma_if_oh(i)
      in_uops(i).br_prediction  := io.enq.bits.bpu_info(i)
      in_uops(i).debug_events   := io.enq.bits.debug_events(i)
   }

   // Step 2. Shift valids towards 0.
   // ASSUMPTION: this assumes fetch-packet is aligned to a fetch boundary,
   // such that index=0 corresponds to AlignedPC(fetch-pc) + (0 << lg(inst_sz)).
   // The "mask" arrives too late for our purposes, and we only need to know
   // where the first valid instruction is anyways.
   val lsb = log2Ceil(coreInstBytes)
   val msb =
      if (icIsBanked) log2Ceil(fetchWidth)+lsb-1-1
      else log2Ceil(fetchWidth)+lsb-1

   val first_index =
      if (fetchWidth==1) 0.U
      else io.enq.bits.pc(msb, lsb)
   var compact_idx = 0.U(log2Ceil(fetchWidth).W)
   for (i <- 0 until fetchWidth)
   {
      val use_uop = i.U >= first_index && in_uops(i.U).valid
      when (use_uop)
      {
         compact_uops(compact_idx) := in_uops(i.U)
         compact_mask(compact_idx) := true.B
      }
      compact_idx = compact_idx + use_uop

//      if (DEBUG_PRINTF)
//      {
//         printf(" shift [" + i + "] pc: 0x%x first: %d enq: %x compact: %d, selects_oh: %x, oob: %d\n",
//            io.enq.bits.pc, first_index, io.enq.bits.mask, compact_mask(i), selects_oh, invalid)
//      }
   }

   // all enqueuing uops have been compacted.
   // How many incoming uops are there?
   val popc_enqmask = PopCount(in_uops.map(_.valid))
   // What is the count of uops being added to the ram. Subtract off the bypassed uops.
   // But only bypass if ram is empty AND dequeue flops will be consumed.
   val enq_count =
      Mux(io.enq.fire() && (!io.deq.ready || count =/= 0.U),
         popc_enqmask,
      Mux(io.enq.fire() && count === 0.U && popc_enqmask > decodeWidth.U,
         popc_enqmask - decodeWidth.U,
         0.U)) // !enq.fire || (count===0 and popc <= decodeWIdth)

   // If the ram is empty, bypass the first decodeWidth uops to the flops,
   // and only write the remaining uops into the ram.
   val start_idx = Wire(UInt((log2Ceil(fetchWidth)+1).W))
   start_idx := Mux(count === 0.U && io.deq.ready, decodeWidth.U, 0.U)
   for (i <- 0 until fetchWidth)
   {
      when (io.enq.fire() && i.U < enq_count)
      {
         ram(write_ptr + i.U) := compact_uops(start_idx + i.U)
         assert (compact_mask(start_idx + i.U), s"compact_mask[$i] is invalid.")
      }
      .otherwise
      {
         assert (!io.enq.fire() || ((start_idx+i.U) >= fetchWidth.U) || !compact_mask(start_idx + i.U),
            "[fetchbuffer] mask(" + i + ") is valid but isn't being written to the RAM.")
      }
   }

   //-------------------------------------------------------------
   // **** Dequeue Uops ****
   //-------------------------------------------------------------

   val r_valid = RegInit(false.B)
   val r_uops = Reg(Vec(decodeWidth, new MicroOp()))

   for (w <- 0 until decodeWidth)
   {
      when (io.deq.ready)
      {
         r_valid := count > 0.U || io.enq.valid
         r_uops(w) := Mux(count === 0.U, compact_uops(w), ram(read_ptr + w.U))
         r_uops(w).valid := Mux(count === 0.U, compact_mask(w), count > w.U)
      }
   }

   io.deq.valid := r_valid
   io.deq.bits.uops := r_uops

   //-------------------------------------------------------------
   // **** Update State ****
   //-------------------------------------------------------------

   val deq_count =
      Mux(io.deq.ready,
         Mux(count < decodeWidth.U, count, decodeWidth.U),
         0.U)
   count := count + enq_count - deq_count

   // TODO turn into bit-vector
   write_ptr := write_ptr + enq_count
   read_ptr := read_ptr + deq_count

   when (io.clear)
   {
      count := 0.U
      write_ptr := 0.U
      read_ptr := 0.U
      r_valid := false.B
   }

   when (reset.toBool)
   {
      io.deq.bits.uops map { u => u.valid := false.B }
   }

   //-------------------------------------------------------------
   // **** Printfs ****
   //-------------------------------------------------------------

   if (DEBUG_PRINTF)
   {
      // TODO a problem if we don't check the f3_valid?
      printf(" Fetch3 : (%d mask: %x [%d] smask: %x) pc=0x%x enq_count (%d) %d\n",
         io.enq.valid,
         io.enq.bits.mask,
         first_index,
         compact_mask.asUInt,
         io.enq.bits.pc,
         enq_count,
         io.clear
         )

      printf(" FB RAM :     count (%d) WA: %d, RA: %d ",
         count,
         write_ptr,
         read_ptr
         )

      printf("\n Fetch4 : %d deq_count (%d) pc=0x%x\n",
         io.deq.valid,
         deq_count,
         io.deq.bits.uops(0).pc
         )
   }

   //-------------------------------------------------------------
   // **** Asserts ****
   //-------------------------------------------------------------

   assert (count >= deq_count, "[fetchbuffer] Trying to dequeue more uops than are available.")
   assert (!(count === 0.U && write_ptr =/= read_ptr), "[fetchbuffer] pointers should match if count is zero.")
}
