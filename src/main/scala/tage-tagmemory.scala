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
//    - Provide a wrapper to a 1 read, 1 write memory that is oblivious to the implementation.
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
   private val index_sz = log2Up(num_entries)
   val io = new Bundle
   {
      // the reader is not ready; stall the read pipeline.
      val stall = Bool(INPUT)

      // send read addr on cycle 0, get data out on cycle 2.
      val s0_r_idx = UInt(INPUT, width = index_sz)
      val s2_r_out = UInt(OUTPUT, width = memwidth)
      def read(idx: UInt) =
      {
         this.s0_r_idx := idx
      }

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
         this.s0_r_idx := UInt(0)
         this.w_en := Bool(false)
         this.w_idx := UInt(0)
         this.w_data := UInt(0)
      }
   }

   val smem = SeqMem(num_entries, UInt(width = memwidth))
   val cmem = Mem(num_entries, UInt(width = memwidth))
   val valids = Reg(init=Vec.fill(num_entries){Bool(false)})


   //------------------------------------------------------------

   val idx = Wire(UInt())
   val last_idx = RegNext(idx)

   idx := Mux(io.stall, last_idx, io.s0_r_idx)

   val r_s0_idx = idx
   val r_s1_idx = RegEnable(r_s0_idx, UInt(0), !io.stall)
   val r_s2_idx = RegEnable(r_s1_idx, UInt(0), !io.stall)

   val s0_valid = valids(io.s0_r_idx)
   val s1_valid = RegEnable(valids(io.s0_r_idx), Bool(false), !io.stall)
   val s2_valid = RegEnable(s1_valid, Bool(false), !io.stall)
   val r_valid = RegEnable(RegEnable(valids(io.s0_r_idx), Bool(false), !io.stall), Bool(false), !io.stall)

   val r_s1_out = smem.read(io.s0_r_idx, !io.stall)
   val r_s2_out = Mux(r_valid, RegEnable(r_s1_out, !io.stall), UInt(0))


   val c_s0_out = cmem(io.s0_r_idx)
   val c_s1_out = RegEnable(c_s0_out, UInt(0), !io.stall)
   val c_s2_out = Mux(r_valid, RegEnable(c_s1_out, UInt(0), !io.stall), UInt(0))
   io.s2_r_out := c_s2_out

   val start = Reg(init=Bool(true), next=Bool(false))
   val trigger = Reg(init=Bool(false))
   when (r_valid && !io.stall && !RegNext(start) && !RegNext(RegNext(start)))
   {
      when ((r_s2_out =/= io.s2_r_out) &&
            (r_s2_out =/= cmem(r_s2_idx)))
      {
         printf("About to crash: r_s2: %x, io.s2: %x\n"
         , r_s2_out
         , io.s2_r_out)
         trigger := Bool(true)
      }
   }
   assert(!RegNext(trigger, Bool(false)), "[memhelper] mismatch bewteen smem and cmem")


   when (io.w_en)
   {
      cmem(io.w_idx) := io.w_data
      smem(io.w_idx) := io.w_data
      valids(io.w_idx) := Bool(true)
   }

   //------------------------------------------------------------
}

