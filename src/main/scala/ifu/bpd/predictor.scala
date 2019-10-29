package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix}

class BranchPredictor(implicit p: Parameters) extends BoomModule()(p)
 with HasBoomFrontendParameters
{
  val io = IO(new Bundle {

    // Requests and responses
    val f0_req  = Input(Valid(new BranchPredictionRequest))

    val f1_resp = Output(new BranchPredictionBundle)
    val f2_resp = Output(new BranchPredictionBundle)
    val f3_resp = Output(new BranchPredictionBundle)

    // Update
    val update = Input(Valid(new BranchPredictionUpdate))
  })

  val banked_predictors = Seq.fill(nBanks) { Module(new TageBranchPredictorBank) }
  for (b <- 0 until nBanks) {
    dontTouch(banked_predictors(b).io)
  }

  if (nBanks == 1) {
    banked_predictors(0).io.f0_req.bits.hist  := io.f0_req.bits.ghist.histories(0)
    banked_predictors(0).io.f0_req.bits.pc    := bankAlign(io.f0_req.bits.pc)
    banked_predictors(0).io.f0_req.valid      := io.f0_req.valid
  } else {
    require(nBanks == 2)
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
    io.f1_resp.preds   := banked_predictors(0).io.f1_resp
    io.f2_resp.preds   := banked_predictors(0).io.f2_resp
    io.f3_resp.preds   := banked_predictors(0).io.f3_resp
    io.f3_resp.meta(0) := banked_predictors(0).io.f3_meta
  } else {
    require(nBanks == 2)

    // The branch prediction metadata is stored un-shuffled
    io.f3_resp.meta(0)    := banked_predictors(0).io.f3_meta
    io.f3_resp.meta(1)    := banked_predictors(1).io.f3_meta

    when (bank(io.f1_resp.pc) === 0.U) {
      for (i <- 0 until bankWidth) {
        io.f1_resp.preds(i)           := banked_predictors(0).io.f1_resp(i)
        io.f1_resp.preds(i+bankWidth) := banked_predictors(1).io.f1_resp(i)
      }
    } .otherwise {
      for (i <- 0 until bankWidth) {
        io.f1_resp.preds(i)           := banked_predictors(1).io.f1_resp(i)
        io.f1_resp.preds(i+bankWidth) := banked_predictors(0).io.f1_resp(i)
      }
    }

    when (bank(io.f2_resp.pc) === 0.U) {
      for (i <- 0 until bankWidth) {
        io.f2_resp.preds(i)           := banked_predictors(0).io.f2_resp(i)
        io.f2_resp.preds(i+bankWidth) := banked_predictors(1).io.f2_resp(i)
      }
    } .otherwise {
      for (i <- 0 until bankWidth) {
        io.f2_resp.preds(i)           := banked_predictors(1).io.f2_resp(i)
        io.f2_resp.preds(i+bankWidth) := banked_predictors(0).io.f2_resp(i)
      }
    }

    when (bank(io.f3_resp.pc) === 0.U) {
      for (i <- 0 until bankWidth) {
        io.f3_resp.preds(i)           := banked_predictors(0).io.f3_resp(i)
        io.f3_resp.preds(i+bankWidth) := banked_predictors(1).io.f3_resp(i)
      }
    } .otherwise {
      for (i <- 0 until bankWidth) {
        io.f3_resp.preds(i)           := banked_predictors(1).io.f3_resp(i)
        io.f3_resp.preds(i+bankWidth) := banked_predictors(0).io.f3_resp(i)
      }
    }
  }

  io.f1_resp.pc := RegNext(io.f0_req.bits.pc)
  io.f2_resp.pc := RegNext(io.f1_resp.pc)
  io.f3_resp.pc := RegNext(io.f2_resp.pc)

  // We don't care about meta from the f1 and f2 resps
  // Use the meta from the latest resp
  io.f1_resp.meta := DontCare
  io.f2_resp.meta := DontCare

  dontTouch(io.f0_req)
  dontTouch(io.f1_resp)
  dontTouch(io.f2_resp)
  dontTouch(io.f3_resp)
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
        (!io.update.bits.cfi_idx.valid || io.update.bits.cfi_idx.bits < bankWidth.U)

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
        (!io.update.bits.cfi_idx.valid || io.update.bits.cfi_idx.bits < bankWidth.U)

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


