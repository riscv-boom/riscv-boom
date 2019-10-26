package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix}

// class BIMMeta extends BoomBundle()(p)
// {
//   val bim   = UInt(2.W)
// }

class BIMEntry(implicit p: Parameters) extends BoomBundle()(p)
{
  val valid = Bool()
  val bim   = UInt(2.W)
  val is_br = Bool()
}

class BIMBranchPredictorBank(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  def bimWrite(v: UInt, taken: Bool): UInt = {
    val old_bim_sat_taken  = v === 3.U
    val old_bim_sat_ntaken = v === 0.U
    Mux(old_bim_sat_taken  &&  taken, 3.U,
      Mux(old_bim_sat_ntaken && !taken, 0.U,
      Mux(taken, v + 1.U, v - 1.U)))
  }



  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nBIMSets).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nBIMSets-1).U) { doing_reset := false.B }


  val data  = SyncReadMem(nBIMSets, Vec(bankWidth, new BIMEntry))

  val s1_req_rdata    = data.read(s0_req_idx   , io.f0_req.valid)
  val s1_update_rdata = data.read(s0_update_idx, io.update.valid)

  val s2_req_rdata      = RegNext(s1_req_rdata)

  val s2_update_rdata   = RegNext(s1_update_rdata)
  val s2_update_wdata   = WireInit(s2_update_rdata)


  for (w <- 0 until bankWidth) {

    io.f2_resp(w).taken        := s2_req.valid && s2_req_rdata(w).valid && (s2_req_rdata(w).bim(1) || !s2_req_rdata(w).is_br) && !doing_reset
    io.f2_resp(w).is_br        := s2_req.valid && s2_req_rdata(w).valid &&  s2_req_rdata(w).is_br
    io.f2_resp(w).is_jal       := s2_req.valid && s2_req_rdata(w).valid && !s2_req_rdata(w).is_br

    val update_pc = s2_update.bits.pc + (w << 1).U
    when (s2_update.bits.br_mask(w) || (s2_update.bits.cfi_idx.valid && s2_update.bits.cfi_idx.bits === w.U)) {
      val was_taken = s2_update.bits.cfi_idx.valid && (s2_update.bits.cfi_idx.bits === w.U) &&
        ((s2_update.bits.cfi_is_br && s2_update.bits.br_mask(w) && s2_update.bits.cfi_taken) || s2_update.bits.cfi_is_jal)
      val old_bim_value    = Mux(s2_update_rdata(w).valid, s2_update_rdata(w).bim, 2.U)

      s2_update_wdata(w).valid   := true.B
      s2_update_wdata(w).is_br   := s2_update.bits.br_mask(w)

      s2_update_wdata(w).bim     := bimWrite(old_bim_value, was_taken)
    }
  }

  when (doing_reset ||
    (s2_update.valid && (s2_update.bits.br_mask =/= 0.U || s2_update.bits.cfi_idx.valid))) {
    data.write(
      Mux(doing_reset, reset_idx, s2_update_idx),
      Mux(doing_reset, VecInit(Seq.fill(bankWidth) { (0.U).asTypeOf(new BIMEntry) }), s2_update_wdata)
    )
  }

  io.f3_resp := RegNext(io.f2_resp)
}
