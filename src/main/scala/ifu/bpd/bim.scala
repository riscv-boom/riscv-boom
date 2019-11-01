package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix}


case class BoomBIMParams(
  nSets: Int = 512,
  micro: Boolean = false
)

class BIMMeta(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val bims  = Vec(bankWidth, UInt(2.W))
}

class BIMEntry(implicit p: Parameters) extends BoomBundle()(p)
{
  val valid = Bool()
  val bim   = UInt(2.W)
  val is_br = Bool()
}

class BIMBranchPredictorBank(params: BoomBIMParams)(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  override val nSets = params.nSets
  require(isPow2(nSets))

  def bimWrite(v: UInt, taken: Bool): UInt = {
    val old_bim_sat_taken  = v === 3.U
    val old_bim_sat_ntaken = v === 0.U
    Mux(old_bim_sat_taken  &&  taken, 3.U,
      Mux(old_bim_sat_ntaken && !taken, 0.U,
      Mux(taken, v + 1.U, v - 1.U)))
  }
  val s1_meta           = Wire(new BIMMeta)
  override val metaSz   = s1_meta.asUInt.getWidth

  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nSets).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nSets-1).U) { doing_reset := false.B }


  val data  = Seq.fill(bankWidth) { SyncReadMem(nSets, new BIMEntry) }

  val s1_req_rdata    = VecInit(data.map(_.read(s0_req_idx   , io.f0_req.valid)))



  val s1_update_wdata   = Wire(Vec(bankWidth, new BIMEntry))
  val s1_update_wmask   = Wire(Vec(bankWidth, Bool()))
  val s1_update_meta    = s1_update.bits.meta.asTypeOf(new BIMMeta)


  val s1_resp           = Wire(Vec(bankWidth, new BranchPrediction))
  for (w <- 0 until bankWidth) {

    s1_resp(w).taken        := s1_req.valid && s1_req_rdata(w).valid && (s1_req_rdata(w).bim(1) || !s1_req_rdata(w).is_br) && !doing_reset
    s1_resp(w).is_br        := s1_req.valid && s1_req_rdata(w).valid &&  s1_req_rdata(w).is_br
    s1_resp(w).is_jal       := s1_req.valid && s1_req_rdata(w).valid && !s1_req_rdata(w).is_br
    s1_resp(w).predicted_pc.valid := false.B
    s1_resp(w).predicted_pc.bits  := DontCare
    s1_meta.bims(w)            := Mux(s1_req.valid && s1_req_rdata(w).valid, s1_req_rdata(w).bim, 2.U)

    s1_update_wmask(w)         := false.B
    s1_update_wdata(w)         := DontCare

    val update_pc = s1_update.bits.pc + (w << 1).U

    when (s1_update.bits.br_mask(w) || (s1_update.bits.cfi_idx.valid && s1_update.bits.cfi_idx.bits === w.U)) {
      val was_taken = s1_update.bits.cfi_idx.valid && (s1_update.bits.cfi_idx.bits === w.U) &&
        ((s1_update.bits.cfi_is_br && s1_update.bits.br_mask(w) && s1_update.bits.cfi_taken) || s1_update.bits.cfi_is_jal)
      val old_bim_value    = s1_update_meta.bims(w)

      s1_update_wmask(w)         := true.B
      s1_update_wdata(w).valid   := true.B
      s1_update_wdata(w).is_br   := s1_update.bits.br_mask(w)

      s1_update_wdata(w).bim     := bimWrite(old_bim_value, was_taken)
    }

  }
  for (w <- 0 until bankWidth) {
    when (doing_reset || (s1_update_wmask(w) && s1_update.valid)) {
      data(w).write(
        Mux(doing_reset, reset_idx, s1_update_idx),
        Mux(doing_reset, (0.U).asTypeOf(new BIMEntry), s1_update_wdata(w))
      )
    }
  }

  if (params.micro)
    io.f1_resp := RegNext(s1_resp)
  io.f2_resp := RegNext(s1_resp)
  io.f3_resp := RegNext(io.f2_resp)
  io.f3_meta := RegNext(RegNext(s1_meta.asUInt))
}
