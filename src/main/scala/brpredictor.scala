//**************************************************************************
// RISCV Branch Predictor
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2015 Apr 28

// TODO bank (or dual-port) the p-table to allow predictions AND updates (optionally)
// TODO pass history down now (not hooked up to anything in bpd.scala)
// TODO store history with branch tag
// TODO don't read SRAM every cycle if stalled (need extra state to store data while stalled)

package BOOM
{

import Chisel._
import Node._

class BpdResp extends BOOMCoreBundle
{
   val takens = Bits(width = FETCH_WIDTH)
   val history = Bits(width = GHIST_LENGTH)
}

// update comes from the branch-unit with the actual outcome
// it needs to do three things:
//    - 1) correct the history if the processor mispredicted
//    - 2) correct the p-table if it mispredicted (the processor may have been correct though)
//    - 3) strengthen the h-table (on all branch resolutions)
class BpdUpdate extends BOOMCoreBundle
{
   // the fetch pc (points to start of the fetch packet)
   // which word in the fetch packet does the update correspond to?
   // processor mispredicted -> reset history
   // what was the history our branch saw?
   // bpd mispredicted -> correct predictor
   // was the branch taken?
   // is the new target PC after a misprediction found within the same fetch
   // packet as the mispredicting branch? If yes, then we have to be careful
   // which "history" we show the new PC.
   val pc = UInt(width = vaddrBits)
   val br_pc = UInt(width = log2Up(FETCH_WIDTH)+log2Ceil(coreInstBytes))
   val mispredict = Bool()
   val history = Bits(width = GHIST_LENGTH)
   val bpd_mispredict = Bool()
   val taken = Bool()
   val new_pc_same_packet = Bool()
}

class BrTableUpdate extends BOOMCoreBundle
{
   val hash_idx   = UInt(width = vaddrBits)
   val br_pc      = UInt(width = log2Up(FETCH_WIDTH)+log2Ceil(coreInstBytes)) // which word in the fetch packet does the update correspond to?
   val new_value  = Bits(width=FETCH_WIDTH) // TODO BUG XXX is this broken? delete comment if I forget what this means
}



// BP2 stage needs to speculatively update the history register with what the
// processor decided to do (takes BTB's effect into account)
class GHistUpdate extends BOOMCoreBundle
{
   val taken = Bool()
}

class BrPredictorIo extends BOOMCoreBundle
{
   val req_pc = UInt(INPUT, width = vaddrBits)
   val resp   = Decoupled(new BpdResp)
   val ghist  = Valid(new GHistUpdate).flip // speculative update
   val update = Valid(new BpdUpdate).flip // from branch-unit (actual update)
}

abstract class BrPredictor extends Module with BOOMCoreParameters
{
   val io = new BrPredictorIo
}

class GshareBrPredictor(fetch_width: Int
                        , num_entries: Int = 4096
                        , history_length: Int = 12
   ) extends BrPredictor
{
   println ("Building (" + (num_entries * 2/8/1024) +
      " kB) GShare Predictor, with " + history_length + " bits of history for(" +
      fetch_width + "-wide fetch)")

   //------------------------------------------------------------

   private def hash (addr: UInt, hist: Bits) =
      (addr >> UInt(log2Up(fetch_width*coreInstBytes))) ^ hist

   private def GenWMask(addr: UInt) =
      if (FETCH_WIDTH == 1) Bits(1).toBools
      else (Bits(1) << ((addr >> UInt(log2Ceil(coreInstBytes))) & SInt(-1,FETCH_WIDTH))).toBools

   //------------------------------------------------------------
   val r_ghist = Reg(Bits(width = history_length))
   val curr_ghist = Wire(Bits(width = history_length))

   // "massage" the history for the scenario where the mispredted branch is not taken
   // AND we're having to refetch the rest of the fetch_packet,  or something XXX BUG
   val fixed_history = Cat(io.update.bits.history, io.update.bits.taken)
   curr_ghist :=
         Mux(io.update.valid &&
             io.update.bits.bpd_mispredict &&
             io.update.bits.new_pc_same_packet, io.update.bits.history,
         Mux(io.update.valid &&
             io.update.bits.bpd_mispredict    , fixed_history,
                                                r_ghist))

   // prediction bits
   // hysteresis bits
   val p_table = SeqMem(num_entries/fetch_width, Vec(Bits(width=1), fetch_width))
   val h_table = SeqMem(num_entries/fetch_width, Vec(Bits(width=1), fetch_width))


   // buffer writes to the h-table as required
   val hwq = Module(new Queue(new BpdUpdate, entries=4))
   hwq.io.enq <> io.update

   val u_addr = hash(io.update.bits.pc, io.update.bits.history)


   //------------------------------------------------------------
   // p-table
   val p_addr = Wire(UInt())
   val last_p_addr = RegNext(p_addr)
   val p_out = Reg(Bits())

   val stall = !io.resp.ready // TODO FIXME this feels too low-level

   val pwq = Module(new Queue(new BrTableUpdate, entries=2))
   pwq.io.deq.ready := Bool(true)
   val pwq_deq_br_pc = pwq.io.deq.bits.br_pc
   val p_wmask = GenWMask(pwq_deq_br_pc)
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

   p_addr := Mux(stall, last_p_addr, hash(io.req_pc, curr_ghist))

   when (!stall)
   {
      p_out := p_table.read(p_addr).toBits
   }

   io.resp.bits.takens := p_out
   io.resp.bits.history := RegNext(RegNext(curr_ghist))

   require (coreInstBytes == 4)

   //------------------------------------------------------------
   // h-table
   // read table to update the p-table
   val h_ren = io.update.valid && io.update.bits.bpd_mispredict
   hwq.io.deq.ready := !h_ren
   when (!h_ren && hwq.io.deq.valid)
   {
      val waddr = hash(hwq.io.deq.bits.pc, hwq.io.deq.bits.history)
      val wmask = GenWMask(hwq.io.deq.bits.br_pc)
      val wdata = Vec.fill(fetch_width)(hwq.io.deq.bits.taken.toUInt)
      h_table.write(waddr, wdata, wmask)
   }
   pwq.io.enq.valid          := Reg(next=h_ren)
   pwq.io.enq.bits.hash_idx  := Reg(next=u_addr)
   pwq.io.enq.bits.br_pc     := Reg(next=io.update.bits.br_pc)
   pwq.io.enq.bits.new_value := h_table.read(u_addr, h_ren).toBits

   //------------------------------------------------------------

   when (io.update.valid && io.update.bits.mispredict)
   {
      r_ghist := fixed_history
   }
   .elsewhen (io.ghist.valid)
   {
      r_ghist := Cat(r_ghist, io.ghist.bits.taken)
   }

}

}

