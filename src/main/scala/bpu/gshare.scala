//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV GShare Branch Predictor
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2015 Apr 28


package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}


case class GShareParameters(
   enabled: Boolean = true,
   history_length: Int = 12
   )


trait HasGShareParameters extends HasBoomCoreParameters
{
   val gsParams = boomParams.gshare.get
   val nSets = 1 << gsParams.history_length

   val idx_sz = log2Up(nSets)
//   val row_idx_sz = log2Up(nSets)-log2Up(nBanks)
   val row_sz = fetchWidth*2
}


abstract class GShareBundle(implicit val p: Parameters) extends freechips.rocketchip.util.ParameterizedBundle()(p)
  with HasGShareParameters


class GShareResp(implicit p: Parameters) extends GShareBundle()(p)
{
   val debug_index = UInt(width = idx_sz) // Can recompute index during update (but let's check for errors).
   val rowdata = UInt(width = row_sz) // Store to prevent a re-read during an update.

   def isTaken(cfi_idx: UInt) =
   {
      val cntr = getCounterValue(cfi_idx)
      val taken = cntr(1)
      taken
   }
 
   def getCounterValue(cfi_idx: UInt) =
   {
      val cntr = (rowdata >> (cfi_idx << 1.U)) & 0x3.U
      cntr
   }
        
   // Get a fetchWidth length bit-vector of taken/not-takens.
   def getTakens(): UInt =
   {
      val takens = Wire(init=Vec.fill(fetchWidth){false.B})
      for (i <- 0 until fetchWidth)
      {
         // assumes 2-bits per branch.
         takens(i) := rowdata(2*i+1)
      }
      takens.asUInt
   }
}

object GShareBrPredictor
{
   def GetRespInfoSize(p: Parameters, hlen: Int): Int =
   {
      val dummy = new GShareResp()(p)
      dummy.getWidth
   }
}

class GShareBrPredictor(
   fetch_width: Int,
   history_length: Int = 12
   )(implicit p: Parameters)
   extends BrPredictor(fetch_width, history_length)(p)
   with HasGShareParameters
{
   println ("\tBuilding (" + (nSets * fetch_width * 2/8/1024) +
      " kB) GShare Predictor, with " + history_length + " bits of history for (" +
      fetch_width + "-wide fetch) and " + nSets + " entries.")

   //------------------------------------------------------------

   private def Hash (addr: UInt, hist: UInt) =
      (addr >> UInt(log2Up(fetch_width*coreInstBytes))) ^ hist

   // for initializing the counter table, this is the value to reset the row to.
   private def initRowValue (): UInt =
   {
      val row = Wire(UInt(width=row_sz))
      row := Fill(fetchWidth, 2.U)
      row
   }


   //------------------------------------------------------------
   // reset/initialization

   val s_reset :: s_wait :: s_clear :: s_idle :: Nil = Enum(UInt(), 4)
   val fsm_state = Reg(init = s_reset)
   val nResetLagCycles = 128
   val nBanks = 1
   val (lag_counter, lag_done) = Counter(fsm_state === s_wait, nResetLagCycles)
   val (clear_row_addr, clear_done) = Counter(fsm_state === s_clear, nSets/nBanks)

   switch (fsm_state)
   {
      is (s_reset) { fsm_state := s_wait }
      is (s_wait)  { when (lag_done) { fsm_state := s_clear } }
      is (s_clear) { when (clear_done) { fsm_state := s_idle } }
      is (s_idle)  { when (io.do_reset) { fsm_state := s_clear } }
   }


   //------------------------------------------------------------
   // Predictor state.

//   val counters = Module(new TwobcCounterTable(fetch_width, nSets, dualported))

   val counter_table = SeqMem(nSets, UInt(width = row_sz))

   //------------------------------------------------------------
   // Get prediction.

   // Perform hash on F1
   // TODO XXX need to work out buffering to match I$/rest of the frontend.

   val s1_ridx = Hash(this.r_f1_fetchpc, this.r_f1_history)

   // Access predictor on F2 and return data to superclass (via f2_resp bundle).

   val s2_out = counter_table.read(s1_ridx, true.B)

//   val stall = !io.resp.ready
//   counters.io.s1_r_idx := s1_ridx
//   counters.io.stall := stall

   val resp_info = Wire(new GShareResp())
   resp_info.debug_index   := RegNext(s1_ridx)
   resp_info.rowdata := s2_out

   f2_resp.valid := fsm_state === s_idle
   f2_resp.bits.takens := s2_out
   f2_resp.bits.info := resp_info.asUInt


   //------------------------------------------------------------
   // Update counter table.

   val wen = false.B // TODO XXX
   when (wen)
   {
      val waddr = Mux(fsm_state === s_clear, clear_row_addr, 0.U)
      val wdata = Mux(fsm_state === s_clear, initRowValue(), 0.U)
      counter_table.write(waddr, wdata)
   }

//
//   val commit_info = new GShareResp(log2Up(num_entries)).fromBits(this.commit.bits.info.info)
//
//   counters.io.update.valid                 := this.commit.valid && !this.disable_bpd
//   counters.io.update.bits.index            := commit_info.index
//   counters.io.update.bits.executed         := this.commit.bits.ctrl.executed
//   counters.io.update.bits.takens           := this.commit.bits.ctrl.taken
//   counters.io.update.bits.was_mispredicted := this.commit.bits.ctrl.mispredicted.reduce(_|_)
//   counters.io.update.bits.do_initialize    := Bool(false)
//
   //------------------------------------------------------------
}

