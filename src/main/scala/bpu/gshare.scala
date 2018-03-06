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

// Notes:
//    - Implement gshare in a a 1r1w SRAM (need to bank to get to 1rw).


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
   // prevent us from building something too big (and taking millions of cycles to initialize!)
   val maxSets = 131072
   val nSets = (1 << gsParams.history_length) min maxSets

   val idx_sz = log2Up(nSets)
   val row_sz = fetchWidth*2
}


class GShareResp(fetch_width: Int, idx_sz: Int) extends Bundle
{
   val debug_index = UInt(width = idx_sz) // Can recompute index during update (but let's check for errors).
   val rowdata = UInt(width = fetch_width*2) // Store to prevent a re-read during an update.

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

   override def cloneType: this.type = new GShareResp(fetch_width, idx_sz).asInstanceOf[this.type]
}

object GShareBrPredictor
{
   def GetRespInfoSize(fetch_width: Int, hlen: Int): Int =
   {
      val dummy = new GShareResp(fetch_width, idx_sz = hlen)
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
   require (log2Ceil(nSets) == idx_sz)

   println ("\tBuilding (" + (nSets * fetch_width * 2/8/1024) +
      " kB) GShare Predictor, with " + history_length + " bits of history for (" +
      fetch_width + "-wide fetch) and " + nSets + " entries.")

   //------------------------------------------------------------

   private def Fold (input: UInt, compressed_length: Int) =
   {
      val clen = compressed_length
      val hlen = history_length
      if (hlen <= clen)
      {
         input
      }
      else
      {
         var res = UInt(0,clen)
         var remaining = input.asUInt
         for (i <- 0 to hlen-1 by clen)
         {
            val len = if (i + clen > hlen ) (hlen - i) else clen
            require(len > 0)
            res = res(clen-1,0) ^ remaining(len-1,0)
            remaining = remaining >> UInt(len)
         }
         res
      }
   }

   private def Hash (addr: UInt, hist: UInt) =
   {
      // fold history if too big for our table
      val folded_history = Fold (hist, idx_sz)
      ((addr >> UInt(log2Up(fetch_width*coreInstBytes))) ^ folded_history)(idx_sz-1,0)
   }

   // for initializing the counter table, this is the value to reset the row to.
   private def initRowValue (): UInt =
   {
      val row = Wire(UInt(width=row_sz))
      row := Fill(fetchWidth, 0.U) // TODO XXX change to 2.U
      row
   }

   // Given old counter value, provide the new counter value.
   private def updateCounter(cntr: UInt, taken: Bool): UInt =
   {
      val next = Wire(UInt(2.W))
      next :=
         Mux(taken && cntr =/= 3.U, cntr + 1.U,
         Mux(!taken && cntr =/= 0.U, cntr - 1.U,
            cntr))
      next
   }


   // Get a fetchWidth length bit-vector of taken/not-takens.
   def getTakensFromRow(row: UInt): UInt =
   {
      val takens = Wire(init=Vec.fill(fetch_width){false.B})
      for (i <- 0 until fetch_width)
      {
         // assumes 2-bits per branch.
         takens(i) := row(2*i+1)
      }
      takens.asUInt
   }


   // Pick out the old counter value from a full row and increment it.
   // Return the new row.
   private def updateCounterInRow(old_row: UInt, cfi_idx: UInt, taken: Bool): UInt =
   {
      val row = Wire(UInt(width=row_sz))
      val shamt = cfi_idx << 1.U
      val mask = Wire(UInt(width=row_sz))
      mask := ~(0x3.U << shamt)
      val old_cntr = (old_row >> shamt) & 0x3.U
      val new_cntr = updateCounter(old_cntr, taken)
      row := (old_row & mask) | (new_cntr << shamt)
      row
   }


   // Update the counters in a row (only one if mispredicted or all counters to strenthen).
   // Return the new row.
   private def updateEntireCounterRow (old_row: UInt, was_mispredicted: Bool, cfi_idx: UInt, was_taken: Bool): UInt =
   {
      val row = Wire(UInt(width=row_sz))

      when (was_mispredicted)
      {
         row := updateCounterInRow(old_row, cfi_idx, was_taken)
      }
      .otherwise
      {
         // strengthen hysteresis bits on correct
         val h_mask = Fill(fetch_width, 0x1.asUInt(width=2.W))
         row := old_row | h_mask
      }
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

   val counter_table = SeqMem(nSets, UInt(width = row_sz))


   //------------------------------------------------------------
   // Perform hash on F1.

   val s1_ridx = Hash(this.r_f1_fetchpc, this.r_f1_history)

   //------------------------------------------------------------
   // Get prediction on F2.

   // return data to superclass (via f2_resp bundle).
   val s2_out = counter_table.read(s1_ridx, this.f1_valid)

   val resp_info = Wire(new GShareResp(fetch_width, idx_sz))
   resp_info.debug_index   := RegNext(s1_ridx)
   resp_info.rowdata := s2_out

   f2_resp.valid := fsm_state === s_idle
   f2_resp.bits.takens := getTakensFromRow(s2_out)
   f2_resp.bits.info := resp_info.asUInt


   //------------------------------------------------------------
   // Update counter table.

   val com_info = new GShareResp(fetch_width, history_length).fromBits(io.commit.bits.info)

   val com_idx = Hash(io.commit.bits.fetch_pc, io.commit.bits.history)(idx_sz-1,0)


   val wen = io.commit.valid || (fsm_state === s_clear)

   when (wen)
   {
      val new_row = updateEntireCounterRow(
         com_info.rowdata,
         io.commit.bits.mispredict,
         io.commit.bits.miss_cfi_idx,
         io.commit.bits.taken)

      val waddr = Mux(fsm_state === s_clear, clear_row_addr, com_idx)
      val wdata = Mux(fsm_state === s_clear, initRowValue(), new_row)

      counter_table.write(waddr, wdata)
   }


   // First commit will have garbage so ignore it.
   val enable_assert = RegInit(false.B); when (io.commit.valid) { enable_assert := true.B }
   when (enable_assert && io.commit.valid)
   {
      assert (com_idx === com_info.debug_index, "[gshare] disagreement on update indices.")
   }

}

