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

// TODO bank (or dual-port) the p-table to allow predictions AND updates (optionally)
// TODO don't read SRAM every cycle if stalled (need extra state to store data while stalled)

package boom

import Chisel._
import Node._
import cde.Parameters

class GShareResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val history = Bits(width = GHIST_LENGTH) // stored in snapshots (dealloc after Execute)
   val index = Bits(width = GHIST_LENGTH) // needed to update predictor at Commit
}

class GShareBrPredictor(fetch_width: Int
                        , num_entries: Int = 4096
                        , history_length: Int = 12
   )(implicit p: Parameters) extends BrPredictor(fetch_width, history_length)(p)
{
   println ("\tBuilding (" + (num_entries * fetch_width * 2/8/1024) +
      " kB) GShare Predictor, with " + history_length + " bits of history for (" +
      fetch_width + "-wide fetch) and " + num_entries + " entries.")

   private def Hash (addr: UInt, hist: Bits) =
      (addr >> UInt(log2Up(fetch_width*coreInstBytes))) ^ hist

   //------------------------------------------------------------
   // prediction bits
   // hysteresis bits
   val p_table = SeqMem(num_entries, Vec(fetch_width, Bits(width=1)))
   val h_table = SeqMem(num_entries, Vec(fetch_width, Bits(width=1)))


   // buffer writes to the h-table as required
   val hwq = Module(new Queue(new BrobEntry(fetch_width), entries=4))
   hwq.io.enq <> commit

   val u_addr = commit.bits.info.info.index


   //------------------------------------------------------------
   // p-table
   val p_addr = Wire(UInt())
   val last_p_addr = RegNext(p_addr)
   val p_out = Reg(Bits())

   val stall = !io.resp.ready // TODO FIXME this feels too low-level


   class BrTableUpdate extends Bundle
   {
      val idx        = UInt(width = log2Up(num_entries))
      val executed   = Bits(width = FETCH_WIDTH) // which words in the fetch packet does the update correspond to?
      val new_value  = Bits(width=FETCH_WIDTH)
   }


   // TODO add 2nd port
   // TODO add banking
   val pwq = Module(new Queue(new BrTableUpdate, entries=2))
   pwq.io.deq.ready := Bool(true)
   val p_wmask = pwq.io.deq.bits.executed.toBools

   io.resp.valid := Bool(true)
   when (pwq.io.deq.valid)
   {
//      io.resp.valid := RegNext(RegNext(Bool(false)))
      val waddr = pwq.io.deq.bits.idx
      val wdata = Vec.fill(fetch_width)(pwq.io.deq.bits.new_value)
      p_table.write(waddr, wdata, p_wmask)
   }
   .otherwise // must always read this or SRAM gives garbage
   {
      // get prediction
//      io.resp.valid := RegNext(RegNext(Bool(true)))
   }

   p_addr := Mux(stall, last_p_addr, Hash(io.req_pc, this.ghistory))

//   when (!stall)
//   {
      p_out := p_table.read(p_addr).toBits
//   }

   io.resp.bits.takens := p_out
   io.resp.bits.info.history := RegNext(RegNext(this.ghistory))
   io.resp.bits.info.index := RegNext(RegNext(p_addr))

   require (coreInstBytes == 4)

   //------------------------------------------------------------
   // h-table
   // read table to update the p-table (only if a mispredict occurred)
   val h_ren = commit.valid && commit.bits.mispredicted.reduce(_|_)
   hwq.io.deq.ready := !h_ren
   when (!h_ren && hwq.io.deq.valid)
   {
      // TODO post ChiselIssue. Chisel needs to be able to have SeqMem take a Vec of Bools
      val waddr = hwq.io.deq.bits.info.info.index
      val wmask = hwq.io.deq.bits.executed
      val wdata = Vec(hwq.io.deq.bits.taken.map(_.toUInt))
      h_table.write(waddr, wdata, wmask)
   }
   pwq.io.enq.valid          := RegNext(h_ren)
   pwq.io.enq.bits.idx       := RegNext(u_addr)
   pwq.io.enq.bits.executed  := RegNext(commit.bits.executed.toBits)
   pwq.io.enq.bits.new_value := h_table.read(u_addr, h_ren).toBits

   //------------------------------------------------------------

}

