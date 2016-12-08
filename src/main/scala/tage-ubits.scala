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
//    - Only allocate for entries that are "not useful".
//    - Occasionally, degrade entries to prevent unused entries from never leaving.
//
// TODO:
//    - Allow 1-bit and 2-bit implementations.
//    - Allow for changing out zeroing policies.

package boom

import Chisel._


abstract class TageUbitMemory(
   num_entries: Int,
   ubit_sz: Int
   ) extends Module
{
   val index_sz = log2Up(num_entries)
   val io = new Bundle
   {
      // send read addr on cycle 0, get data out on cycle 2.
      val s0_read_idx = UInt(INPUT, width = index_sz)
      val s1_read_out = UInt(OUTPUT, width = ubit_sz)


      val allocate_valid  = Bool(INPUT)
      val allocate_idx = UInt(INPUT, width = index_sz)
      def allocate(idx: UInt) =
      {
         this.allocate_valid := Bool(true)
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

      val degrade_valid = Bool(INPUT)
      def degrade(dummy: Int=0) =
      {
         this.degrade_valid := Bool(true)
      }

//      // Degrading may take many cycles. Tell the tage-table if we are degrading.
//      val is_degrading = Bool(OUTPUT)
//      def areDegrading(dummy: Int=0) =
//      {
//         this.is_degrading
//      }

      def InitializeIo(dummy: Int=0) =
      {
         this.allocate_valid := Bool(false)
         this.allocate_idx := UInt(0)
         this.update_valid := Bool(false)
         this.update_idx := UInt(0)
         this.update_old_value := UInt(0)
         this.update_inc := Bool(false)
         this.degrade_valid := Bool(false)
//         this.is_degrading := Bool(false)
      }
   }

   val UBIT_MAX = (1 << ubit_sz) - 1
   val UBIT_INIT_VALUE = 1
   require(ubit_sz < 4) // What are you doing? You're wasting bits!
   assert(!(io.allocate_valid && io.update_valid), "[ubits] trying to update and allocate simultaneously.")
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// This version implements the u-bits in sequential memory -- can be placed into SRAM.
// However, degrading the u-bits takes many cycles.
class TageUbitMemorySeqMem(
   num_entries: Int,
   ubit_sz: Int
   ) extends TageUbitMemory(num_entries, ubit_sz)
{
   // TODO XXX
   // we don't currently support >1 ubit_sz versions until we finish supporting
   // the clearing of the u-bits. As this takes many cycles, we need to add
   // support for the ubit table to tell TAGE to not start counting
   // failed_allocs until we finish the degrading.

   //------------------------------------------------------------

//   val ubit_table = SeqMem(num_entries, UInt(width = ubit_sz))
   val ubit_table       = Mem(num_entries, UInt(width = ubit_sz))

   // maintain an async copy purely for assertions
   val debug_ubit_table = Mem(num_entries, UInt(width = ubit_sz))
   val debug_valids     = Reg(init=Vec.fill(num_entries){Bool(false)})

   //------------------------------------------------------------
   // Manage clearing u-bits over time
   // (to prevent entries from never leaving the predictor).

   // TODO

   //------------------------------------------------------------

   // TODO add a read_enable (only reads on commit.valid within TAGE)
//   val s1_out = ubit_table.read(io.s0_read_idx, Bool(true))
   val s0_out = (ubit_table(io.s0_read_idx))
   io.s1_read_out := RegNext(
      s0_out |
         Mux(io.allocate_valid && io.allocate_idx === io.s0_read_idx, UInt(UBIT_INIT_VALUE), UInt(0)) |
         Mux(io.update_valid && io.update_inc && io.update_idx === io.s0_read_idx, UInt(UBIT_INIT_VALUE), UInt(0)))


   when (RegNext(debug_valids(io.s0_read_idx)))
   {
      assert (RegNext(s0_out) === RegNext(debug_ubit_table(io.s0_read_idx)),
         "[ubits] value doesn't match debug copy.")
   }

   //------------------------------------------------------------
   // Compute update values.
   val inc = io.update_inc
//   val u = io.update_old_value //TODO XXX
   val u = ubit_table(io.update_idx)
   val next_u =
      Mux(inc && u < UInt(UBIT_MAX),
         u + UInt(1),
      Mux(!inc && u > UInt(0),
         u - UInt(1),
         u))

   when (io.allocate_valid)
   {
      val a_idx = io.allocate_idx
      ubit_table(a_idx) := UInt(UBIT_INIT_VALUE)
      debug_ubit_table(a_idx) := UInt(UBIT_INIT_VALUE)
   }
   when (io.update_valid)
   {
      val up_idx = io.update_idx
      ubit_table(up_idx) := next_u
      debug_ubit_table(up_idx) := next_u
      assert (!io.allocate_valid, "[ubits] trying to allocate.")

   }

   //------------------------------------------------------------
   when (io.allocate_valid)
   {
      debug_valids(io.allocate_idx) := Bool(true)
   }

   val r_debug_allocate_value = RegNext(debug_ubit_table(io.allocate_idx))
   when (RegNext(io.allocate_valid && debug_valids(io.allocate_idx)))
   {
      assert(r_debug_allocate_value === UInt(0), "[ubits] Tried to allocate a useful entry")
   }
}

