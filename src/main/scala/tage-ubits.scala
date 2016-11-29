//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// TAGE U-Bits
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2016 Nov
//
// Goal:
//    - U-bits provide a "usefulness" metric for each entry in a TAGE predictor.
//
// TODO:
//    - XXX allow occasional zeroing of u-bits
//    - Allow 1-bit and 2-bit implementations.
//    - Allow for changing out zeroing policies.

package boom

import Chisel._


class TageUbitMemory(
   num_entries: Int,
   ubit_sz: Int
   ) extends Module
{
   private val index_sz = log2Up(num_entries)
   val io = new Bundle
   {
      // send read addr on cycle 0, get data out on cycle 2.
      val s0_read_idx = UInt(INPUT, width = index_sz)
      val s1_read_out = UInt(OUTPUT, width = ubit_sz)


      val allocate_valid  = Bool(INPUT)
      val allocate_idx = UInt(INPUT, width = index_sz)
      def allocate(idx: UInt) =
      {
         this.allocate_valid  := Bool(true)
         this.allocate_idx := idx
      }

      val update_valid  = Bool(INPUT)
      val update_idx = UInt(INPUT, width = index_sz)
      val update_old_value  = UInt(INPUT, width = ubit_sz)
      val update_inc = Bool(INPUT)
      def update(idx: UInt, old_value: UInt, inc: Bool) =
      {
         this.update_valid  := Bool(true)
         this.update_idx := idx
         this.update_old_value := old_value
         this.update_inc := inc
      }

      def InitializeIo(dummy: Int=0) =
      {
         this.allocate_valid := Bool(false)
         this.allocate_idx := UInt(0)
         this.update_valid := Bool(false)
         this.update_idx := UInt(0)
         this.update_old_value := UInt(0)
         this.update_inc := Bool(false)
      }
   }

   private val UBIT_MAX = (1 << ubit_sz) - 1
   private val UBIT_INIT_VALUE = 1

   //------------------------------------------------------------

   val ubit_table = SeqMem(num_entries, UInt(width = ubit_sz))

   // maintain an async copy purely for assertions
   val debug_ubit_table = Mem(num_entries, UInt(width = ubit_sz))
   val debug_valids     = Reg(init=Vec.fill(num_entries){Bool(false)})

   //------------------------------------------------------------

//   val idx = Wire(UInt())
//   val last_idx = RegNext(idx)

//   idx := Mux(io.stall, last_idx, io.s0_r_idx)

//   val r_s1_out = smem.read(idx, !io.stall)
//   val r_s2_out = RegEnable(r_s1_out, !io.stall)
//   io.s2_r_out := r_s2_out
   // TODO XXX add a read_enable (only reads on commit.valid within TAGE)
   val s1_out = ubit_table.read(io.s0_read_idx, Bool(true))
   io.s1_read_out :=
      s1_out |
      RegNext(
         Mux(io.allocate_valid && io.allocate_idx === io.s0_read_idx, UInt(UBIT_INIT_VALUE), UInt(0)) |
         Mux(io.update_valid && io.update_inc && io.update_idx === io.s0_read_idx, UInt(UBIT_INIT_VALUE), UInt(0)))


   // Compute update values.
   val inc = io.update_inc
   val u = io.update_old_value
   val next_u =
      Mux(inc && u < UInt(UBIT_MAX),
         u + UInt(1),
      Mux(!inc && u > UInt(0),
         u - UInt(1),
         u))

   // Perform write.
   val w_en   = io.allocate_valid || io.update_valid
   val w_addr = Mux(io.allocate_valid, io.allocate_idx, io.update_idx)
   val w_data = Mux(io.allocate_valid, UInt(UBIT_INIT_VALUE), next_u)
   when (w_en)
   {
      ubit_table(w_addr) := w_data
      debug_ubit_table(w_addr) := w_data
   }


   when (io.allocate_valid)
   {
      debug_valids(io.allocate_idx) := Bool(true)
   }

   val r_debug_allocate_value = RegNext(debug_ubit_table(io.allocate_idx))
   when (RegNext(io.allocate_valid && debug_valids(io.allocate_idx)))
   {
      assert(r_debug_allocate_value === UInt(0), "[ubits] Tried to allocate a useful entry")
   }

   assert(!(io.allocate_valid && io.update_valid), "[ubits] trying to update and allocate simultaneously.")
}

