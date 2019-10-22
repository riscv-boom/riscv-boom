package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix}

class BranchPrediction(implicit p: Parameters) extends BoomBundle()(p)
{
  // If this is a branch, do we take it?
  val taken           = Bool()

  // Is this a branch? (typically returned by BTB)
  val is_br           = Bool()
  // Is this a JAL? (typically returned by BTB)
  val is_jal          = Bool()
  // What is the target of his branch/jump? (typically returned by BTB)
  val predicted_pc    = Valid(UInt(vaddrBitsExtended.W))

}

class BranchPredictionBundle(implicit p: Parameters) extends BoomBundle()(p)
{
  val pc = UInt(vaddrBitsExtended.W)
  val preds = Vec(fetchWidth, new BranchPrediction)
}

class BranchPredictionUpdate(implicit p: Parameters) extends BoomBundle()(p)
{
  val pc            = UInt(vaddrBitsExtended.W)
  // Mask of instructions which are branches.
  val br_mask       = UInt(fetchWidth.W)
  val cfi_idx       = Valid(UInt(log2Ceil(fetchWidth).W))    // Which CFI was taken (if any)

  val cfi_is_br     = Bool()
  val cfi_is_jal  = Bool()
  //val cfi_is_ret  = Bool()

  val target        = UInt(vaddrBitsExtended.W) // What did this CFI jump to?
}

abstract class BranchPredictor(implicit p: Parameters) extends BoomModule()(p)
 with HasBoomFrontendParameters
{
  val io = IO(new Bundle {

    // Requests and responses
    val f0_req  = Input(Valid(UInt(vaddrBitsExtended.W)))

    val f1_resp = Output(new BranchPredictionBundle)
    val f2_resp = Output(new BranchPredictionBundle)
    val f3_resp = Output(new BranchPredictionBundle)

    // Update
    val update = Input(Valid(new BranchPredictionUpdate))
  })

  io.f1_resp := DontCare
  io.f2_resp := DontCare
  io.f3_resp := DontCare

  io.f1_resp.preds.map(_.is_br := false.B)
  io.f2_resp.preds.map(_.is_br := false.B)
  io.f3_resp.preds.map(_.is_br := false.B)

  io.f1_resp.preds.map(_.is_jal := false.B)
  io.f2_resp.preds.map(_.is_jal := false.B)
  io.f3_resp.preds.map(_.is_jal := false.B)

  io.f1_resp.preds.map(_.predicted_pc.valid := false.B)
  io.f2_resp.preds.map(_.predicted_pc.valid := false.B)
  io.f3_resp.preds.map(_.predicted_pc.valid := false.B)

  io.f1_resp.pc := RegNext(io.f0_req.bits)
  io.f2_resp.pc := RegNext(io.f1_resp.pc)
  io.f3_resp.pc := RegNext(io.f2_resp.pc)

  dontTouch(io.f1_resp)
  dontTouch(io.f2_resp)
  dontTouch(io.f3_resp)

  when (io.update.valid) {
    when (io.update.bits.cfi_is_br && io.update.bits.cfi_idx.valid) {
      assert(io.update.bits.br_mask(io.update.bits.cfi_idx.bits))
    }
  }
}

class NullBranchPredictor(implicit p: Parameters) extends BranchPredictor()(p)
{
  when (io.update.valid) {
    for (i <- 0 until fetchWidth) {
      val pc = io.update.bits.pc + (i << 1).U
      // when (io.update.bits.br_mask(i)) {
      //   printf("Branch update %x %x\n", pc,
      //     i.U === io.update.bits.cfi_idx.bits && io.update.bits.cfi_idx.valid)
      // }
    }
  }
}

