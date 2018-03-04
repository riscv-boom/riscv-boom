//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Abstracted Memory 1r/1w Helper for TAGE Tag Memory
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2016 Nov
//
// Goal:
//    - Provide a wrapper to a 1 read/1 write memory that is oblivious to the implementation.
//
// Notes:
//    - Handles read requests being stalled.
//    - Assumes that writes can be delayed (or even dropped).
//    - Does not support masking.

package boom

import Chisel._


class TageTagMemory(
   num_entries: Int,
   memwidth: Int,
   dualported: Boolean = false
   ) extends Module
{
   private val index_sz = log2Ceil(num_entries)
   val io = IO(new Bundle
   {
      // the reader is not ready; stall the read pipeline.
      val stall = Bool(INPUT)

      // send read addr on cycle 1, get data out on cycle 2.
      val s1_r_idx = UInt(INPUT, width = index_sz)
      val s2_r_out = UInt(OUTPUT, width = memwidth)

      val w_en = Bool(INPUT)
      val w_idx = UInt(INPUT, width = index_sz)
      val w_data = UInt(INPUT, width = memwidth)
      def write(idx: UInt, data: UInt) =
      {
         this.w_en := Bool(true)
         this.w_idx := idx
         this.w_data := data
      }

      def InitializeIo(dummy: Int=0) =
      {
         this.w_en := Bool(false)
         this.w_idx := UInt(0)
         this.w_data := UInt(0)
      }
   })

   //------------------------------------------------------------

   val smem = SeqMem(num_entries, UInt(width = memwidth))

   //------------------------------------------------------------

   val idx = Wire(UInt())
   val last_idx = RegNext(idx)

   idx := Mux(io.stall, last_idx, io.s1_r_idx)

   val r_s2_out = smem.read(idx, !io.stall)
   io.s2_r_out := r_s2_out

   when (io.w_en)
   {
      smem(io.w_idx) := io.w_data
   }
}

