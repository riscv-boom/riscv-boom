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
  val meta = Output(Vec(nBanks, UInt(bpdMaxMetaLength.W)))
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

class BranchPredictionBankResponse(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val f1 = Vec(bankWidth, new BranchPrediction)
  val f2 = Vec(bankWidth, new BranchPrediction)
  val f3 = Vec(bankWidth, new BranchPrediction)
}

abstract class BranchPredictorBank(implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFrontendParameters
{
  val metaSz = 0
  

  val io = IO(new Bundle {
    val f0_req = Input(Valid(new BranchPredictionBankRequest))

    val resp = Output(new BranchPredictionBankResponse)
    val resp_in = Input(new BranchPredictionBankResponse)

    // Store the meta as a UInt, use width inference to figure out the shape
    val f3_meta = Output(UInt(bpdMaxMetaLength.W))

    val f3_fire = Input(Bool())

    val update = Input(Valid(new BranchPredictionBankUpdate))
  })
  io.resp := io.resp_in

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
}



class BranchPredictor(implicit p: Parameters) extends BoomModule()(p)
 with HasBoomFrontendParameters
{
  val io = IO(new Bundle {

    // Requests and responses
    val f0_req  = Input(Valid(new BranchPredictionRequest))

    val resp = Output(new Bundle {
      val f1 = new BranchPredictionBundle
      val f2 = new BranchPredictionBundle
      val f3 = new BranchPredictionBundle
    })

    val f3_fire = Input(Bool())

    // Update
    val update = Input(Valid(new BranchPredictionUpdate))
  })

  val banked_predictors = Seq.fill(nBanks) { Module(new ComposedBranchPredictorBank) }
  for (b <- 0 until nBanks) {
    dontTouch(banked_predictors(b).io)
  }

  if (nBanks == 1) {
    banked_predictors(0).io.f0_req.bits.hist  := io.f0_req.bits.ghist.histories(0)
    banked_predictors(0).io.f0_req.bits.pc    := bankAlign(io.f0_req.bits.pc)
    banked_predictors(0).io.f0_req.valid      := io.f0_req.valid

    banked_predictors(0).io.resp_in           := (0.U).asTypeOf(new BranchPredictionBankResponse)
  } else {
    require(nBanks == 2)
    banked_predictors(0).io.resp_in           := (0.U).asTypeOf(new BranchPredictionBankResponse)
    banked_predictors(1).io.resp_in           := (0.U).asTypeOf(new BranchPredictionBankResponse)
    when (bank(io.f0_req.bits.pc) === 0.U) {
      banked_predictors(0).io.f0_req.bits.hist := io.f0_req.bits.ghist.histories(0)
      banked_predictors(0).io.f0_req.bits.pc   := bankAlign(io.f0_req.bits.pc)
      banked_predictors(0).io.f0_req.valid     := io.f0_req.valid

      banked_predictors(1).io.f0_req.bits.hist := io.f0_req.bits.ghist.histories(1)
      banked_predictors(1).io.f0_req.bits.pc   := nextBank(io.f0_req.bits.pc)
      banked_predictors(1).io.f0_req.valid     := io.f0_req.valid
    } .otherwise {
      banked_predictors(0).io.f0_req.bits.hist := io.f0_req.bits.ghist.histories(1)
      banked_predictors(0).io.f0_req.bits.pc   := nextBank(io.f0_req.bits.pc)
      banked_predictors(0).io.f0_req.valid     := io.f0_req.valid && !mayNotBeDualBanked(io.f0_req.bits.pc)

      banked_predictors(1).io.f0_req.bits.hist := io.f0_req.bits.ghist.histories(0)
      banked_predictors(1).io.f0_req.bits.pc   := bankAlign(io.f0_req.bits.pc)
      banked_predictors(1).io.f0_req.valid     := io.f0_req.valid
    }
  }

  if (nBanks == 1) {
    io.resp.f1.preds   := banked_predictors(0).io.resp.f1
    io.resp.f2.preds   := banked_predictors(0).io.resp.f2
    io.resp.f3.preds   := banked_predictors(0).io.resp.f3
    io.resp.f3.meta(0) := banked_predictors(0).io.f3_meta

    banked_predictors(0).io.f3_fire := io.f3_fire
  } else {
    require(nBanks == 2)
    banked_predictors(0).io.f3_fire := (
      io.f3_fire && RegNext(RegNext(RegNext(banked_predictors(0).io.f0_req.valid)))
    )
    banked_predictors(1).io.f3_fire := (
      io.f3_fire && RegNext(RegNext(RegNext(banked_predictors(1).io.f0_req.valid)))
    )

    // The branch prediction metadata is stored un-shuffled
    io.resp.f3.meta(0)    := banked_predictors(0).io.f3_meta
    io.resp.f3.meta(1)    := banked_predictors(1).io.f3_meta

    when (bank(io.resp.f1.pc) === 0.U) {
      for (i <- 0 until bankWidth) {
        io.resp.f1.preds(i)           := banked_predictors(0).io.resp.f1(i)
        io.resp.f1.preds(i+bankWidth) := banked_predictors(1).io.resp.f1(i)
      }
    } .otherwise {
      for (i <- 0 until bankWidth) {
        io.resp.f1.preds(i)           := banked_predictors(1).io.resp.f1(i)
        io.resp.f1.preds(i+bankWidth) := banked_predictors(0).io.resp.f1(i)
      }
    }

    when (bank(io.resp.f2.pc) === 0.U) {
      for (i <- 0 until bankWidth) {
        io.resp.f2.preds(i)           := banked_predictors(0).io.resp.f2(i)
        io.resp.f2.preds(i+bankWidth) := banked_predictors(1).io.resp.f2(i)
      }
    } .otherwise {
      for (i <- 0 until bankWidth) {
        io.resp.f2.preds(i)           := banked_predictors(1).io.resp.f2(i)
        io.resp.f2.preds(i+bankWidth) := banked_predictors(0).io.resp.f2(i)
      }
    }

    when (bank(io.resp.f3.pc) === 0.U) {
      for (i <- 0 until bankWidth) {
        io.resp.f3.preds(i)           := banked_predictors(0).io.resp.f3(i)
        io.resp.f3.preds(i+bankWidth) := banked_predictors(1).io.resp.f3(i)
      }
    } .otherwise {
      for (i <- 0 until bankWidth) {
        io.resp.f3.preds(i)           := banked_predictors(1).io.resp.f3(i)
        io.resp.f3.preds(i+bankWidth) := banked_predictors(0).io.resp.f3(i)
      }
    }
  }

  io.resp.f1.pc := RegNext(io.f0_req.bits.pc)
  io.resp.f2.pc := RegNext(io.resp.f1.pc)
  io.resp.f3.pc := RegNext(io.resp.f2.pc)

  // We don't care about meta from the f1 and f2 resps
  // Use the meta from the latest resp
  io.resp.f1.meta := DontCare
  io.resp.f2.meta := DontCare

  dontTouch(io.f0_req)
  dontTouch(io.resp.f1)
  dontTouch(io.resp.f2)
  dontTouch(io.resp.f3)
  dontTouch(io.update)

  if (nBanks == 1) {
    banked_predictors(0).io.update.valid                 := io.update.valid
    banked_predictors(0).io.update.bits.pc               := bankAlign(io.update.bits.pc)
    banked_predictors(0).io.update.bits.br_mask          := io.update.bits.br_mask
    banked_predictors(0).io.update.bits.cfi_idx          := io.update.bits.cfi_idx
    banked_predictors(0).io.update.bits.cfi_taken        := io.update.bits.cfi_taken
    banked_predictors(0).io.update.bits.cfi_mispredicted := io.update.bits.cfi_mispredicted
    banked_predictors(0).io.update.bits.cfi_is_br        := io.update.bits.cfi_is_br
    banked_predictors(0).io.update.bits.cfi_is_jal       := io.update.bits.cfi_is_jal
    banked_predictors(0).io.update.bits.hist             := io.update.bits.ghist.histories(0)
    banked_predictors(0).io.update.bits.target           := io.update.bits.target
    banked_predictors(0).io.update.bits.meta             := io.update.bits.meta(0)
  } else {
    require(nBanks == 2)
    // Split the single update bundle for the fetchpacket into two updates
    // 1 for each bank.

    // The meta was not interleaved
    banked_predictors(0).io.update.bits.meta := io.update.bits.meta(0)
    banked_predictors(1).io.update.bits.meta := io.update.bits.meta(1)

    when (bank(io.update.bits.pc) === 0.U) {
      banked_predictors(0).io.update.valid := io.update.valid
      banked_predictors(1).io.update.valid := io.update.valid &&
        (!io.update.bits.cfi_idx.valid || io.update.bits.cfi_idx.bits >= bankWidth.U)

      banked_predictors(0).io.update.bits.pc := bankAlign(io.update.bits.pc)
      banked_predictors(1).io.update.bits.pc := nextBank(io.update.bits.pc)

      banked_predictors(0).io.update.bits.br_mask := io.update.bits.br_mask
      banked_predictors(1).io.update.bits.br_mask := io.update.bits.br_mask >> bankWidth

      banked_predictors(0).io.update.bits.cfi_idx.valid := io.update.bits.cfi_idx.valid && io.update.bits.cfi_idx.bits < bankWidth.U
      banked_predictors(1).io.update.bits.cfi_idx.valid := io.update.bits.cfi_idx.valid && io.update.bits.cfi_idx.bits >= bankWidth.U

      banked_predictors(0).io.update.bits.hist := io.update.bits.ghist.histories(0)
      banked_predictors(1).io.update.bits.hist := io.update.bits.ghist.histories(1)
    } .otherwise {
      banked_predictors(1).io.update.valid := io.update.valid
      banked_predictors(0).io.update.valid := io.update.valid && !mayNotBeDualBanked(io.update.bits.pc) &&
        (!io.update.bits.cfi_idx.valid || io.update.bits.cfi_idx.bits >= bankWidth.U)

      banked_predictors(1).io.update.bits.pc := bankAlign(io.update.bits.pc)
      banked_predictors(0).io.update.bits.pc := nextBank(io.update.bits.pc)

      banked_predictors(1).io.update.bits.br_mask := io.update.bits.br_mask
      banked_predictors(0).io.update.bits.br_mask := io.update.bits.br_mask >> bankWidth

      banked_predictors(1).io.update.bits.cfi_idx.valid := io.update.bits.cfi_idx.valid && io.update.bits.cfi_idx.bits < bankWidth.U
      banked_predictors(0).io.update.bits.cfi_idx.valid := io.update.bits.cfi_idx.valid && io.update.bits.cfi_idx.bits >= bankWidth.U

      banked_predictors(1).io.update.bits.hist := io.update.bits.ghist.histories(0)
      banked_predictors(0).io.update.bits.hist := io.update.bits.ghist.histories(1)
    }

    banked_predictors(0).io.update.bits.cfi_idx.bits  := io.update.bits.cfi_idx.bits
    banked_predictors(1).io.update.bits.cfi_idx.bits  := io.update.bits.cfi_idx.bits

    banked_predictors(0).io.update.bits.cfi_taken  := io.update.bits.cfi_taken
    banked_predictors(1).io.update.bits.cfi_taken  := io.update.bits.cfi_taken

    banked_predictors(0).io.update.bits.cfi_mispredicted  := io.update.bits.cfi_mispredicted
    banked_predictors(1).io.update.bits.cfi_mispredicted  := io.update.bits.cfi_mispredicted

    banked_predictors(0).io.update.bits.cfi_is_br := io.update.bits.cfi_is_br
    banked_predictors(1).io.update.bits.cfi_is_br := io.update.bits.cfi_is_br

    banked_predictors(0).io.update.bits.cfi_is_jal := io.update.bits.cfi_is_jal
    banked_predictors(1).io.update.bits.cfi_is_jal := io.update.bits.cfi_is_jal

    banked_predictors(0).io.update.bits.target := io.update.bits.target
    banked_predictors(1).io.update.bits.target := io.update.bits.target
  }

  when (io.update.valid) {
    when (io.update.bits.cfi_is_br && io.update.bits.cfi_idx.valid) {
      assert(io.update.bits.br_mask(io.update.bits.cfi_idx.bits))
    }
  }
}

class NullBranchPredictorBank(implicit p: Parameters) extends BranchPredictorBank()(p)


