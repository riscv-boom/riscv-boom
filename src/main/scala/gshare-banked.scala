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
//
// The prediction table requires two ports (1 read, 1 write). This version of 
// gshare banks the prediction table and uses a queue to buffer the writes in 
// case of structural hazards.


package boom

import Chisel._
import cde.{Parameters, Field}


class GShareBankedBrPredictor(fetch_width: Int,
                        history_length: Int = 12
   )(implicit p: Parameters) extends BrPredictor(fetch_width, history_length)(p)
{
   val num_entries = 1 << history_length
   println ("\tBuilding (" + (num_entries * fetch_width * 2/8/1024) +
      " kB) Banked GShare Predictor, with " + history_length + " bits of history for (" +
      fetch_width + "-wide fetch) and " + num_entries + " entries.")

   private def Hash (addr: UInt, hist: UInt) =
      (addr >> UInt(log2Up(fetch_width*coreInstBytes))) ^ hist

   // which p-table bank does the hash addr map to?
   private def getBank (addr: UInt): UInt =
      addr(0)
   
   // which p-table index does the hash addr map to?
   private def getIdx (addr: UInt): UInt =
      addr >> UInt(1)

   //------------------------------------------------------------
   // prediction response information
   val commit_info = new GShareResp(log2Up(num_entries)).fromBits(commit.bits.info.info)

   //------------------------------------------------------------
   // prediction bits
   // hysteresis bits

   // use tow banks for the p-table to manage the structural hazard on the single port.
   val p_table_0 = SeqMem(num_entries/2, Vec(fetch_width, Bool()))
   val p_table_1 = SeqMem(num_entries/2, Vec(fetch_width, Bool()))
   val h_table   = SeqMem(num_entries,   Vec(fetch_width, Bool()))


   // buffer writes to the h-table as required
   val hwq = Module(new Queue(new BrobEntry(fetch_width), entries=4))
   hwq.io.enq <> commit

   val u_addr = commit_info.index


   //------------------------------------------------------------
   // p-table
   val p_raddr = Wire(UInt())
   val last_p_raddr = RegNext(p_raddr)
   val p_out_0 = Reg(UInt())
   val p_out_1 = Reg(UInt())

   val stall = !io.resp.ready // TODO FIXME this feels too low-level


   class BrTableUpdate extends Bundle
   {
      val idx        = UInt(width = log2Up(num_entries))
      val executed   = UInt(width = FETCH_WIDTH) // which words in the fetch packet does the update correspond to?
      val new_value  = UInt(width=FETCH_WIDTH)

      override def cloneType: this.type = new BrTableUpdate().asInstanceOf[this.type]
   }


   val pwq = Module(new Queue(new BrTableUpdate, entries=2))
   val p_wmask = pwq.io.deq.bits.executed.toBools

   // Always overrule the BTB, which will almost certainly have less history.
   io.resp.valid := Bool(true)
 
   p_raddr := Mux(stall, last_p_raddr, Hash(io.req_pc, this.ghistory))
   val p_waddr = pwq.io.deq.bits.idx
   val rbank = getBank(p_raddr)
   val wbank = getBank(p_waddr)
   val wdata = Vec(pwq.io.deq.bits.new_value.toBools)
   pwq.io.deq.ready := rbank =/= wbank

   val p_ren_0 = rbank === UInt(0)
   val p_ren_1 = rbank === UInt(1)
   p_out_0 := p_table_0.read(getIdx(p_raddr), p_ren_0).toBits
   p_out_1 := p_table_1.read(getIdx(p_raddr), p_ren_1).toBits
   when (!p_ren_0 && wbank === UInt(0) && pwq.io.deq.valid)
   {
      p_table_0.write(getIdx(p_waddr), wdata, p_wmask)
   }
   when (!p_ren_1 && wbank === UInt(1) && pwq.io.deq.valid)
   {
      p_table_1.write(getIdx(p_waddr), wdata, p_wmask)
   }
    
   val resp_info = Wire(new GShareResp(log2Up(num_entries)))
   io.resp.bits.takens := Mux(RegNext(RegNext(p_ren_0)), p_out_0, p_out_1)
   io.resp.bits.history := RegNext(RegNext(this.ghistory))
   resp_info.index := RegNext(RegNext(p_raddr))
   io.resp.bits.info := resp_info.toBits

   require (coreInstBytes == 4)

   //------------------------------------------------------------
   // h-table
   // read table to update the p-table (only if a mispredict occurred)
   val h_ren = commit.valid && commit.bits.ctrl.mispredicted.reduce(_|_)
   hwq.io.deq.ready := !h_ren
   when (!h_ren && hwq.io.deq.valid)
   {
      // TODO post ChiselIssue. Chisel needs to be able to have SeqMem take a Vec of Bools
      val u_info = new GShareResp(log2Up(num_entries)).fromBits(hwq.io.deq.bits.info.info)
      val waddr = u_info.index
      val wmask = hwq.io.deq.bits.ctrl.executed
      val wdata = Vec(hwq.io.deq.bits.ctrl.taken.map(_.toBool))
      h_table.write(waddr, wdata, wmask)
   }
   pwq.io.enq.valid          := RegNext(h_ren)
   pwq.io.enq.bits.idx       := RegNext(u_addr)
   pwq.io.enq.bits.executed  := RegNext(commit.bits.ctrl.executed.toBits)
   pwq.io.enq.bits.new_value := h_table.read(u_addr, h_ren).toBits

   //------------------------------------------------------------

}

