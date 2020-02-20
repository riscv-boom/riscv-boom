package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix}

case class BoomLocalBPDParams(
  nSets: Int = 128
)

class LocalBranchPredictorBank(params: BoomLocalBPDParams = BoomLocalBPDParams())(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  override val nSets = params.nSets

  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nSets).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nSets-1).U) { doing_reset := false.B }

  val lbim = Module(new HBIMBranchPredictorBank(
    BoomHBIMParams(histLength = localHistoryLength)
  ))


  class LocalMeta extends Bundle {
    val lhist = UInt(localHistoryLength.W)
  }

  val f3_meta  = Wire(new LocalMeta)
  override val metaSz = lbim.metaSz + f3_meta.asUInt.getWidth

  val entries = SyncReadMem(nSets, UInt(localHistoryLength.W))

  val s1_rhist = entries.read(s0_idx, s0_valid)
  val s2_rhist = RegNext(s1_rhist)
  val s3_rhist = RegNext(s2_rhist)

  f3_meta.lhist := s3_rhist
  io.f3_meta  := Cat(lbim.io.f3_meta, f3_meta.asUInt)

  val f3_do_update    = Wire(Bool())
  val f3_update_idx   = Wire(UInt(log2Ceil(nSets).W))
  val f3_update_lhist = Wire(UInt(localHistoryLength.W))


  f3_do_update := false.B
  f3_update_idx := DontCare
  f3_update_lhist := DontCare

  when (s1_update.valid &&
        (s1_update.bits.is_mispredict_update || s1_update.bits.is_repair_update) &&
        s1_update.bits.br_mask =/= 0.U) {

    f3_do_update    := true.B
    f3_update_idx   := s1_update_idx
    f3_update_lhist := s1_update.bits.meta.asTypeOf(new LocalMeta).lhist

  } .elsewhen (io.f3_fire && io.resp_in(0).f3.map(e => e.is_br && e.predicted_pc.valid).reduce(_||_)) {

    f3_do_update    := true.B
    val f3_update_bidx = PriorityEncoder(io.resp_in(0).f3.map(e => e.is_br && e.predicted_pc.valid))
    f3_update_idx   := s3_idx
    f3_update_lhist := s3_rhist << 1 | io.resp_in(0).f3(f3_update_bidx).taken

  }

  val s0_update_meta = io.update.bits.meta.asTypeOf(new LocalMeta)
  lbim.io.resp_in := io.resp_in
  lbim.io.f0_valid  := io.f0_valid
  lbim.io.f0_pc     := io.f0_pc
  lbim.io.f0_mask   := io.f0_mask
  lbim.io.f1_hist   := s1_rhist
  lbim.io.f3_fire      := io.f3_fire
  lbim.io.update       := io.update
  lbim.io.update.bits.hist := s0_update_meta.lhist
  lbim.io.update.bits.meta := io.update.bits.meta >> f3_meta.asUInt.getWidth
  io.resp := lbim.io.resp

  when (doing_reset || f3_do_update) {
    entries.write(Mux(doing_reset, reset_idx, f3_update_idx),
                  Mux(doing_reset, 0.U, f3_update_lhist))
  }

}
