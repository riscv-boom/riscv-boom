//******************************************************************************
// Copyright (c) 2017, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Memory Disambiguator Predictor
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Track which load instructions have caused disambiguation store->load exceptions.
// 

package boom

import Chisel._
import config.Parameters

import util.Str

case class DisambParameters(
   enable: Boolean = true,
   nEntries: Int = 1024,
   resetTime: Int = 1 << 16
)

   // TODO make sure wait bit comes out of LAQ, not ExeStage.


// Set-associative branch target buffer.
class MemoryDisambiguatorPredictor(pl_width: Int, nEntries: Int, resetTime: Int, coreInstBytes: Int) extends Module
{
   private val idx_sz = log2Up(nEntries)
   private val lsb_sz = log2Ceil(coreInstBytes)
   private val b_sz = log2Ceil(pl_width)
   val io = new Bundle
   {
      // Decode Stage asks if a load should wait for older stores to resolve.
      val dec_pcs = Vec(pl_width, UInt(width=idx_sz+b_sz+lsb_sz)).asInput
      val dec_wait = Vec(pl_width, Bool()).asOutput

      // Update table (only ever mark entries to slow, don't unlearn them).
      val update_pc = Valid(UInt(width=idx_sz+b_sz+lsb_sz)).flip
   }
   //-------------------------------------------------------------

   private def getBankIdx (addr: UInt): UInt = 
   {
      if (pl_width == 1) { return 0.U }
      else               { addr(b_sz+lsb_sz-1, lsb_sz) }
   }
   private def getRowIdx (addr: UInt): UInt = addr(idx_sz+b_sz+lsb_sz-1, b_sz+lsb_sz)

   //-------------------------------------------------------------
   // reset table after reset timer hits 0.

   val s_clear :: s_wait :: Nil = Enum(UInt(),2)
   val clr_cntr = util.WideCounter(log2Ceil(resetTime), true.B)
   val clr_state = Reg(init = s_clear)
   val clr_idx = Counter(clr_state === s_clear, nEntries/pl_width)._1

   switch (clr_state)
   {
      is (s_clear)
      {
         when (clr_idx === ((nEntries/pl_width)-1).U)
         {
            clr_state := s_wait
         }
      }
      is (s_wait)
      {
         when (clr_cntr === 0.U)
         {
            clr_state := s_clear
         }
      }
   }

   //-------------------------------------------------------------

   val wen = io.update_pc.valid
   val widx = getRowIdx(io.update_pc.bits)
   val wbank = getBankIdx(io.update_pc.bits)

   for (w <- 0 until pl_width)
   {
      val table = Mem(nEntries/pl_width, Bool())

      when (clr_state === s_clear)
      {
         table(clr_idx) := false.B
      }
      .elsewhen (wen && wbank === w.U)
      {
         table(widx) := true.B
      }

      io.dec_wait(w) := table(getRowIdx(io.dec_pcs(w)))

      // this coupling of banks to uops(w) fails if ISA is variable length.
      require (coreInstBytes == 4) 
      assert (getBankIdx(io.dec_pcs(w)) === w.U)
   }
}
