package boom.ifu

import chisel3._
import chisel3.util._

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

  // Is this a branch?
  val is_br           = Bool()
  // Is this a JAL?
  val is_jal          = Bool()
  // What is the target of his branch/jump? Do we know the target?
  val predicted_pc    = Valid(UInt(vaddrBitsExtended.W))


}

// A branch prediction for a entire fetch-width worth of instructions
// This is typically merged from individual predictions from the banked
// predictor
class BranchPredictionBundle(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val pc = UInt(vaddrBitsExtended.W)
  val preds = Vec(fetchWidth, new BranchPrediction)
  val meta  = Vec(nBanks, UInt(bpdMaxMetaLength.W))
}


// A branch update for a fetch-width worth of instructions
class BranchPredictionUpdate(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val pc            = UInt(vaddrBitsExtended.W)
  // Mask of instructions which are branches.
  // If these are not cfi_idx, then they were predicted not taken
  val br_mask       = UInt(fetchWidth.W)
  // Which CFI was taken/mispredicted (if any)
  val cfi_idx       = Valid(UInt(log2Ceil(fetchWidth).W))
  // Was the cfi taken?
  val cfi_taken     = Bool()
  // Was the cfi mispredicted from the original prediction?
  val cfi_mispredicted = Bool()
  // Was the cfi a br?
  val cfi_is_br     = Bool()
  // Was the cfi a jal/jalr?
  val cfi_is_jal  = Bool()
  //val cfi_is_ret  = Bool()

  val ghist = new GlobalHistory


  // What did this CFI jump to?
  val target        = UInt(vaddrBitsExtended.W)

  val meta          = Vec(nBanks, UInt(bpdMaxMetaLength.W))
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

  val hist             = UInt(globalHistoryLength.W)

  val target           = UInt(vaddrBitsExtended.W)

  val meta             = UInt(bpdMaxMetaLength.W)
}

class BranchPredictionRequest(implicit p: Parameters) extends BoomBundle()(p)
{
  val pc    = UInt(vaddrBitsExtended.W)
  val ghist = new GlobalHistory
}

class BranchPredictionBankRequest(implicit p: Parameters) extends BoomBundle()(p)
{
  val pc    = UInt(vaddrBitsExtended.W)
  val hist  = UInt(globalHistoryLength.W)
}

abstract class BranchPredictorBank(implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFrontendParameters
{
  val metaSz = 0

  val io = IO(new Bundle {
    val f0_req = Input(Valid(new BranchPredictionBankRequest))

    val f1_kill = Input(Bool())
    val f2_kill = Input(Bool())
    val f3_kill = Input(Bool())

    val f1_resp = Output(Vec(bankWidth, new BranchPrediction))
    val f2_resp = Output(Vec(bankWidth, new BranchPrediction))
    val f3_resp = Output(Vec(bankWidth, new BranchPrediction))

    // Store the meta as a UInt, use width inference to figure out the shape
    val f3_meta = Output(UInt(bpdMaxMetaLength.W))

    val update = Input(Valid(new BranchPredictionBankUpdate))
  })
  io.f1_resp := (0.U).asTypeOf(Vec(bankWidth, new BranchPrediction))
  io.f2_resp := (0.U).asTypeOf(Vec(bankWidth, new BranchPrediction))
  io.f3_resp := (0.U).asTypeOf(Vec(bankWidth, new BranchPrediction))

  io.f3_meta := 0.U

  val s0_req       = io.f0_req
  val s0_req_idx   = fetchIdx(io.f0_req.bits.pc)

  val s0_update     = io.update
  val s0_update_idx = fetchIdx(io.update.bits.pc)


  val s1_req      = RegNext(s0_req)
  val s1_req_idx  = RegNext(s0_req_idx)

  val s1_update     = RegNext(s0_update)
  val s1_update_idx = RegNext(s0_update_idx)


  val s2_req     = RegNext(s1_req)
  val s2_req_idx = RegNext(s1_req_idx)

  val f1_kill = io.f1_kill
  val f2_kill = io.f2_kill || RegNext(f1_kill)
  val f3_kill = io.f3_kill || RegNext(f2_kill)

}

