package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix}

case object BoomBPDComposition extends Field[Function2[BranchPredictionBankResponse, Parameters, Tuple2[Seq[BranchPredictorBank], BranchPredictionBankResponse]]](
  (resp_in: BranchPredictionBankResponse, p: Parameters) => {
    val loop = Module(new LoopBranchPredictorBank()(p))
    val tage = Module(new TageBranchPredictorBank()(p))
    val btb = Module(new BTBBranchPredictorBank()(p))
    val ubtb = Module(new FAMicroBTBBranchPredictorBank()(p))
    val bim = Module(new BIMBranchPredictorBank()(p))
    ubtb.io.resp_in  := resp_in
    bim.io.resp_in   := ubtb.io.resp
    btb.io.resp_in   := bim.io.resp
    tage.io.resp_in  := btb.io.resp
    loop.io.resp_in  := tage.io.resp

    (Seq(loop, tage, btb, ubtb, bim), loop.io.resp)
  }
)

class ComposedBranchPredictorBank(implicit p: Parameters) extends BranchPredictorBank()(p)
{

  val (components, resp) = p(BoomBPDComposition)(io.resp_in, p)
  io.resp := resp

  var metas = 0.U(1.W)
  var meta_sz = 0
  for (c <- components) {
    c.io.f0_req  := io.f0_req
    c.io.f3_fire := io.f3_fire
    if (c.metaSz > 0) {
      metas = (metas << c.metaSz) | c.io.f3_meta(c.metaSz-1,0)
    }
    meta_sz = meta_sz + c.metaSz
  }
  require(meta_sz < bpdMaxMetaLength)
  io.f3_meta := metas


  var update_meta = io.update.bits.meta
  for (c <- components.reverse) {
    c.io.update := io.update
    c.io.update.bits.meta := update_meta
    update_meta = update_meta >> c.metaSz
  }

}
