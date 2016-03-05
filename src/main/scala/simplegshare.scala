//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV GShare Branch Predictor (simplified)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2016 Mar 4

// This version of gshare demonstrates how to create a prototype branch
// predictor that interfaces properly with the rest of BOOM. It is
// combinational.
//
// A more realistic version would:
//    - be implemented using single-ported synchronous memory.
//    - separate each bit of the two-bit counters into separate memory banks.
//    - banked to allow simultaneous updates and predictions.
//   
// NOTE: global history is already handled automatically in the BrPredictor
// super-class.
             
package boom

import Chisel._
import Node._
import cde.Parameters

// TODO: need to change BpdResp() bundle in brpredictor.scala to use this, and not the regular GShareResp()
class SimpleGShareResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val history = Bits(width = GHIST_LENGTH) // stored in snapshots (dealloc after Execute)
   val index = Bits(width = GHIST_LENGTH) // needed to update predictor at Commit
}

class SimpleGShareBrPredictor(
   fetch_width: Int,
   num_entries: Int = 4096,
   history_length: Int = 12
   )(implicit p: Parameters) extends BrPredictor(fetch_width, history_length)(p)
{
   println ("\tBuilding Simple GShare Predictor, with " 
      + history_length + " bits of history for (" 
      + fetch_width + "-wide fetch) and " 
      + num_entries + " entries.")

   //------------------------------------------------------------
   // constants
   val CNTR_SZ = 2
   val CNTR_MAX = (1 << CNTR_SZ) - 1

   //------------------------------------------------------------
   // helper functions

   private def Hash (addr: UInt, hist: Bits): UInt =
   {
      (addr >> UInt(log2Up(fetch_width*coreInstBytes))) ^ hist
   }

   private def GetPrediction(cntr: UInt): Bool = 
   {
      // return highest-order bit
      (cntr >> UInt(CNTR_SZ-1)).toBool
   }

   private def UpdateCounters(valid: Bool, counter_row: Vec[UInt], enables: Vec[Bool], takens: Vec[Bool]): Vec[UInt] =
   {
      val updated_row = Wire(Vec(fetch_width, UInt(width=CNTR_SZ)))
      for (i <- 0 until fetch_width)
      {
         updated_row(i) := counter_row(i)
         when (valid)
         {
            when (enables(i) && takens(i) && counter_row(i) =/= UInt(CNTR_MAX))
            {
               updated_row(i) := counter_row(i) + UInt(1)
            }
            .elsewhen (enables(i) && !takens(i) && counter_row(i) =/= UInt(0))
            {
               updated_row(i) := counter_row(i) - UInt(1)
            }
         }
      }
      updated_row
   }

   //------------------------------------------------------------
   // state

   // CNTR_SZ-bit counters, one counter per instruction in the fetch packet
   val counters = Mem(num_entries, Vec(fetch_width, UInt(width=CNTR_SZ)))

   //------------------------------------------------------------
   // get prediction (delay response 2 cycles to match fetch pipeline)

   val p_idx = Hash(io.req_pc, this.ghistory)
   io.resp.valid             := Bool(true)
   io.resp.bits.info.history := RegNext(RegNext(this.ghistory))
   io.resp.bits.info.index   := RegNext(RegNext(p_idx))
   io.resp.bits.takens       := RegNext(RegNext(Vec(counters(p_idx).map(GetPrediction(_))).toBits))

   //------------------------------------------------------------
   // update predictor

   val u_idx = commit.bits.info.info.index
   counters(u_idx) := UpdateCounters(commit.valid, counters(u_idx), commit.bits.executed, commit.bits.taken)


   //------------------------------------------------------------

}

