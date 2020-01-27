package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix}


class ComposedBranchPredictorBank(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  val tage = Module(new TageBranchPredictorBank)

  val btb = Module(new BTBBranchPredictorBank(BoomBTBParams()))

  val ubtb = Module(new FAMicroBTBBranchPredictorBank(
    BoomFAMicroBTBParams(nWays = 16, offsetSz = 13)
  ))

  val bim = Module(new BIMBranchPredictorBank(2048))

  val components = Seq(tage, btb, ubtb, bim)

  var metas = 0.U(1.W)
  var meta_sz = 0
  for (c <- components) {
    c.io.f0_req  := io.f0_req

    metas = (metas << c.metaSz) | c.io.f3_meta(c.metaSz-1,0)
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


  ubtb.io.resp_in  := io.resp_in
  bim.io.resp_in   := ubtb.io.resp
  btb.io.resp_in   := bim.io.resp
  tage.io.resp_in  := btb.io.resp
  io.resp          := tage.io.resp

}
