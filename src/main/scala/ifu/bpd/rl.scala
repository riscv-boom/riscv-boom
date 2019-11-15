package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix, MaskLower}

class RLMeta(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
}

class NeuralNetwork(implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFrontendParameters
{
  val io = IO(new Bundle {
    val f0_req = Input(Valid(new BranchPredictionBankRequest)) // pc + history
    val f3_resp = Output(Bool()) // taken or not taken
  })

  io.f3_resp := true.B
}

/**
 * Each bank is giving out "bankWidth" worth of predictions (aka fetchWidth/i$banks)
 */
class RLBranchPredictorBank(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  // default predictors (btb and micro-btb)

  val base = Module(new BTBBranchPredictorBank(BoomBTBParams()))
  val micro = Module(new BTBBranchPredictorBank(
    BoomBTBParams(nSets = 64, offsetSz = 13, extendedNSets = 0, micro = true))
  )
  base.io.f1_kill := io.f1_kill
  base.io.f2_kill := io.f2_kill
  base.io.f3_kill := io.f3_kill
  micro.io.f1_kill := io.f1_kill
  micro.io.f2_kill := io.f2_kill
  micro.io.f3_kill := io.f3_kill

  base.io.f0_req := io.f0_req
  base.io.update := io.update
  base.io.update.bits.meta := io.update.bits.meta(base.metaSz-1,0)
  micro.io.f0_req := io.f0_req
  micro.io.update := io.update
  micro.io.update.bits.meta := io.update.bits.meta(base.metaSz+micro.metaSz-1,base.metaSz)

  io.f1_resp := micro.io.f1_resp
  io.f2_resp := base.io.f2_resp
  io.f3_resp := base.io.f3_resp

  // rl bpu

  //val f3_meta = Wire(new RLMeta)

  override val metaSz = base.metaSz + micro.metaSz //+ f3_meta.asUInt.getWidth
  require(metaSz <= bpdMaxMetaLength)

  // here have bankWidth worth of models
  val networks = Seq.fill(bankWidth) { Module(new NeuralNetwork) }

  networks.map(_.io.f0_req  := io.f0_req)
  val f3_resps = VecInit(networks.map(_.io.f3_resp))

  for (w <- 0 until bankWidth) {
    io.f3_resp(w).taken := f3_resps(w)

    // update logic
  }

  // update logic

  // send out the metadata that is carried through the pipeline
  //io.f3_meta := Cat(f3_meta.asUInt, micro.io.f3_meta(micro.metaSz-1,0), base.io.f3_meta(base.metaSz-1, 0))
  io.f3_meta := Cat(micro.io.f3_meta(micro.metaSz-1,0), base.io.f3_meta(base.metaSz-1, 0))
}
