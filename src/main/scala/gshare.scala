//**************************************************************************
// RISCV GShare Branch Predictor
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2015 Apr 28

// TODO bank (or dual-port) the p-table to allow predictions AND updates (optionally)
// TODO don't read SRAM every cycle if stalled (need extra state to store data while stalled)

package BOOM

import Chisel._
import Node._

class BrTableUpdate extends BOOMCoreBundle
{
   val hash_idx   = UInt(width = vaddrBits)
   val executed   = Bits(width = FETCH_WIDTH) // which words in the fetch packet does the update correspond to?
//   val br_pc      = UInt(width = log2Up(FETCH_WIDTH)+log2Ceil(coreInstBytes)) // which word in the fetch packet does the update correspond to?
   val new_value  = Bits(width=FETCH_WIDTH)
}


class GShareResp extends BOOMCoreBundle
{
   val history = Bits(width = GHIST_LENGTH) // stored in snapshots (dealloc after Execute)
   val index =  Bits(width = GHIST_LENGTH) // needed to update predictor at Commit
}

class GshareBrPredictor(fetch_width: Int
                        , num_entries: Int = 4096
                        , history_length: Int = 12
   ) extends BrPredictor(fetch_width, history_length)
{
   println ("\tBuilding (" + (num_entries * 2/8/1024) +
      " kB) GShare Predictor, with " + history_length + " bits of history for (" +
      fetch_width + "-wide fetch)")

   private def Hash (addr: UInt, hist: Bits) =
      (addr >> UInt(log2Up(fetch_width*coreInstBytes))) ^ hist

//   private def GenWMask(addr: UInt) =
//      if (FETCH_WIDTH == 1) Bits(1).toBools
//      else (Bits(1) << ((addr >> UInt(log2Ceil(coreInstBytes))) & SInt(-1,FETCH_WIDTH))).toBools

   //------------------------------------------------------------
   // prediction bits
   // hysteresis bits
   val p_table = SeqMem(num_entries/fetch_width, Vec(fetch_width, Bits(width=1)))
   val h_table = SeqMem(num_entries/fetch_width, Vec(fetch_width, Bits(width=1)))


   // buffer writes to the h-table as required
//   val hwq = Module(new Queue(new BpdUpdate, entries=4))
   val hwq = Module(new Queue(new BrobEntry(fetch_width), entries=4))
//   hwq.io.enq <> io.br_resolution garbage
   hwq.io.enq <> commit

//   val u_addr = hash(io.br_resolution.bits.pc, io.br_resolution.bits.history)
   val u_addr = commit.bits.info.info.index


   //------------------------------------------------------------
   // p-table
   val p_addr = Wire(UInt())
   val last_p_addr = RegNext(p_addr)
   val p_out = Reg(Bits())

   val stall = !io.resp.ready // TODO FIXME this feels too low-level

   // TODO verify this produces a single-ported p_table. Then add 2nd port.
   val pwq = Module(new Queue(new BrTableUpdate, entries=2))
   pwq.io.deq.ready := Bool(true)
//   val pwq_deq_br_pc = pwq.io.deq.bits.br_pc
//   val p_wmask = GenWMask(pwq_deq_br_pc)
   val p_wmask = pwq.io.deq.bits.executed.toBools
   when (pwq.io.deq.valid)
   {
      io.resp.valid := RegNext(RegNext(Bool(false)))
      val waddr = pwq.io.deq.bits.hash_idx
      val wdata = Vec.fill(fetch_width)(pwq.io.deq.bits.new_value)
      p_table.write(waddr, wdata, p_wmask)
   }
   .otherwise // must always read this or SRAM gives garbage
   {
      // get prediction
      io.resp.valid := RegNext(RegNext(Bool(true)))
   }

   p_addr := Mux(stall, last_p_addr, Hash(io.req_pc, this.ghistory))

   when (!stall)
   {
      p_out := p_table.read(p_addr).toBits
   }

   io.resp.bits.takens := p_out
   io.resp.bits.info.history := RegNext(RegNext(this.ghistory))
   io.resp.bits.info.index := RegNext(RegNext(p_addr))

   require (coreInstBytes == 4)

   //------------------------------------------------------------
   // h-table
   // read table to update the p-table (only if a mispredict occurred)
//   val h_ren = io.br_resolution.valid && io.br_resolution.bits.bpd_mispredict
   val h_ren = commit.valid && commit.bits.mispredict.reduce(_|_)
   hwq.io.deq.ready := !h_ren
   when (!h_ren && hwq.io.deq.valid)
   {
//      val waddr = hash(hwq.io.deq.bits.pc, hwq.io.deq.bits.history)
//      val wmask = GenWMask(hwq.io.deq.bits.br_pc)
//      val wdata = Vec.fill(fetch_width)(hwq.io.deq.bits.taken.toUInt)

      val waddr = hwq.io.deq.bits.info.info.index
      // TODO post ChiselIssue, so that SeqMem to take a Vec of Bools
      //val wdata = hwq.io.deq.bits.executed
      val wmask = hwq.io.deq.bits.executed
      val wdata = Vec(hwq.io.deq.bits.executed.map(_.toUInt))

//      val wmask = hwq.io.deq.bits.executed.toBits.toBools
//      val wdata = hwq.io.deq.bits.taken
//      val wmask = commit.executed
//      val wdata = commit.taken
      h_table.write(waddr, wdata, wmask)
   }
   pwq.io.enq.valid          := Reg(next=h_ren)
   pwq.io.enq.bits.hash_idx  := Reg(next=u_addr)
   pwq.io.enq.bits.executed  := Reg(next=commit.bits.executed.toBits)
   pwq.io.enq.bits.new_value := h_table.read(u_addr, h_ren).toBits
//   pwq.io.enq.bits.br_pc     := Reg(next=io.br_resolution.bits.br_pc)

   //------------------------------------------------------------

}

