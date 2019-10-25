package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix}

// A branch prediction for a single instruction
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

// A branch prediction for a entire fetch-width worth of instructions
// This is typically merged from individual predictions from the banked
// predictor
class BranchPredictionBundle(implicit p: Parameters) extends BoomBundle()(p)
{
  val pc = UInt(vaddrBitsExtended.W)
  val preds = Vec(fetchWidth, new BranchPrediction)
}


// A branch update for a fetch-width worth of instructions
class BranchPredictionUpdate(implicit p: Parameters) extends BoomBundle()(p)
{
  val pc            = UInt(vaddrBitsExtended.W)
  // Mask of instructions which are branches.
  val br_mask       = UInt(fetchWidth.W)
  val cfi_idx       = Valid(UInt(log2Ceil(fetchWidth).W))    // Which CFI was taken (if any)
  val cfi_taken     = Bool()
  val cfi_mispredicted = Bool()

  val cfi_is_br     = Bool()
  val cfi_is_jal  = Bool()
  //val cfi_is_ret  = Bool()

  val target        = UInt(vaddrBitsExtended.W) // What did this CFI jump to?
}

// A branch update to a single bank
class BranchPredictionBankUpdate(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val pc               = UInt(vaddrBitsExtended.W)

  val br_mask          = UInt(bankWidth.W)
  val cfi_idx          = Valid(UInt(log2Ceil(bankWidth).W))
  val cfi_taken        = Bool()
  val cfi_mispredicted = Bool()

  val cfi_is_br        = Bool()
  val cfi_is_jal       = Bool()

  val target           = UInt(vaddrBitsExtended.W)
}

class BranchPredictionRequest(implicit p: Parameters) extends BoomBundle()(p)
{
  val pc    = UInt(vaddrBitsExtended.W)
//  val ghist = UInt(globalHistoryLength.W)
}

abstract class BranchPredictorBank(implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFrontendParameters
{
  val io = IO(new Bundle {
    val f0_req = Input(Valid(UInt(vaddrBitsExtended.W)))

    val f1_resp = Output(Vec(bankWidth, new BranchPrediction))
    val f2_resp = Output(Vec(bankWidth, new BranchPrediction))
    val f3_resp = Output(Vec(bankWidth, new BranchPrediction))

    val update = Input(Valid(new BranchPredictionBankUpdate))
  })
  io.f1_resp := (0.U).asTypeOf(Vec(bankWidth, new BranchPrediction))
  io.f2_resp := (0.U).asTypeOf(Vec(bankWidth, new BranchPrediction))
  io.f3_resp := (0.U).asTypeOf(Vec(bankWidth, new BranchPrediction))

  val s0_req_pc   = io.f0_req.bits
  val s0_req_idx  = fetchIdx(io.f0_req.bits)

  val s0_update     = io.update
  val s0_update_idx = fetchIdx(io.update.bits.pc)


  val s1_req_pc   = RegNext(s0_req_pc)
  val s1_req_idx  = RegNext(s0_req_idx)

  val s1_update     = RegNext(s0_update)
  val s1_update_idx = RegNext(s0_update_idx)


  val s2_req_pc  = RegNext(s1_req_pc)
  val s2_req_idx = RegNext(s1_req_idx)

  val s2_update     = RegNext(s1_update)
  val s2_update_idx = RegNext(s1_update_idx)
}

