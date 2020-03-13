package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix}


abstract class AbstractLocalBranchPredictorBank(implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFrontendParameters
{
  val io = IO(new Bundle {
    val f0_valid = Input(Bool())
    val f0_pc    = Input(UInt(vaddrBitsExtended.W))

    val f1_lhist  = Output(UInt(localHistoryLength.W))

    val f3_lhist = Output(UInt(localHistoryLength.W))

    val f3_taken_br = Input(Bool())
    val f3_fire = Input(Bool())

    val update = Input(new Bundle {
      val valid      = Input(Bool())
      val mispredict = Input(Bool())
      val repair     = Input(Bool())
      val pc         = Input(UInt(vaddrBitsExtended.W))
      val lhist      = Input(UInt(localHistoryLength.W))
    })
  })
}

class NullLocalBranchPredictorBank(implicit p: Parameters) extends AbstractLocalBranchPredictorBank
{
  io.f1_lhist := 0.U
  io.f3_lhist := 0.U
}

