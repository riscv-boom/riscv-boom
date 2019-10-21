package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.exu.{CommitExceptionSignals}
import boom.util.{BoomCoreStringPrefix}


class SwBranchPredictor(implicit p: Parameters) extends BranchPredictor()(p)
{
  for (w <- 0 until fetchWidth) {
    val btb_harness = Module(new BTBHarness)
    val pred_harness = Module(new BranchPredictorHarness)

    btb_harness.io.clock := clock
    btb_harness.io.reset := reset.toBool

    pred_harness.io.clock := clock
    pred_harness.io.reset := reset.toBool


    btb_harness.io.req_valid := io.f0_req.valid
    btb_harness.io.req_pc    := io.f0_req.bits + (w << 1).U

    pred_harness.io.req_valid := io.f0_req.valid
    pred_harness.io.req_pc    := io.f0_req.bits + (w << 1).U


    btb_harness.io.update_valid  := io.update.valid && io.update.bits.cfi_idx.valid && (w == 0).B
    btb_harness.io.update_pc     := alignToFetchBoundary(io.update.bits.pc) + (io.update.bits.cfi_idx.bits << 1)
    btb_harness.io.update_target := io.update.bits.target
    btb_harness.io.update_is_br  := io.update.bits.cfi_is_br
    btb_harness.io.update_is_jal := io.update.bits.cfi_is_jal

    pred_harness.io.update_valid := io.update.valid && io.update.bits.br_mask(w)
    pred_harness.io.update_pc    := alignToFetchBoundary(io.update.bits.pc) + (w << 1).U
    pred_harness.io.update_taken := w.U === io.update.bits.cfi_idx.bits &&
                                    io.update.bits.cfi_idx.valid


    io.f1_resp.preds(w).taken := pred_harness.io.req_taken
    io.f1_resp.preds(w).predicted_pc.valid := btb_harness.io.req_target_valid
    io.f1_resp.preds(w).predicted_pc.bits  := btb_harness.io.req_target_pc
    io.f1_resp.preds(w).is_br              := btb_harness.io.req_is_br  && btb_harness.io.req_target_valid
    io.f1_resp.preds(w).is_jal             := btb_harness.io.req_is_jal && btb_harness.io.req_target_valid

    // The Harness to software assumes output comes out in f1
    io.f3_resp.preds(w).is_br := RegNext(RegNext(pred_harness.io.req_taken))
    io.f3_resp.preds(w).taken := RegNext(RegNext(pred_harness.io.req_taken))
  }
}

class BranchPredictorHarness(implicit p: Parameters)
    extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())

    val req_valid = Input(Bool())
    val req_pc = Input(UInt(64.W))
    val req_taken = Output(Bool())

    val update_valid = Input(Bool())
    val update_pc = Input(UInt(64.W))
    val update_taken = Input(Bool())
  })

  addResource("/vsrc/predictor_harness.v")
  addResource("/csrc/basic_predictor_sw.cc")
}

class BTBHarness(implicit p: Parameters)
    extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())

    val req_valid = Input(Bool())
    val req_pc = Input(UInt(64.W))
    val req_target_valid = Output(Bool())
    val req_target_pc = Output(UInt(64.W))
    val req_is_br = Output(Bool())
    val req_is_jal = Output(Bool())

    val update_valid = Input(Bool())
    val update_pc = Input(UInt(64.W))
    val update_target = Input(UInt(64.W))
    val update_is_br = Input(Bool())
    val update_is_jal = Input(Bool())
  })

  addResource("/vsrc/btb_harness.v")
  addResource("/csrc/perfect_btb_sw.cc")
}
