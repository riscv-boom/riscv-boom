//******************************************************************************
// Copyright (c) 2018 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Abraham Gonzalez
//------------------------------------------------------------------------------

package boom.tests

import scala.util.Random

import org.scalatest._

import chisel3._
import chisel3.util._
import chisel3.iotesters._

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.system._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import boom.system._
import boom.common.{BoomCoreParams}

import boom.ifu.{BranchChecker}

/**
 * Main BranchChecker tester
 *
 * Tests the combinational logic BranchChecker class
 */
class BranchCheckerTester extends ChiselFlatSpec
{
  // boom parameters
  val boomParams: Parameters = BoomTestUtils.getBoomParameters("BoomConfig")
  implicit val p: Parameters = boomParams
  val localBoomParams: BoomCoreParams = p(freechips.rocketchip.tile.TileKey).core.asInstanceOf[BoomCoreParams]

  // note: the "it" keyword copies the "behavior"
  behavior of "Branch Checker"

  // ----------------
  // TESTING SECTION
  // ----------------

  it should s"mask outputs on invalid inputs" in {
    chisel3.iotesters.Driver(() => new BranchChecker, "verilator") {
      (c) => new ValidInputsTest(c, fetchWidth=localBoomParams.fetchWidth)
    } should be (true)
  }

  it should s"not correct on matching decoded type and btb prediction (no bpd)" in {
    chisel3.iotesters.Driver(() => new BranchChecker, "verilator") {
      (c) => new MatchingDecodeAndBTBTest(c, fetchWidth=localBoomParams.fetchWidth)
    } should be (true)
  }

  it should s"correct if bpd differs from btb" in {
    chisel3.iotesters.Driver(() => new BranchChecker, "verilator") {
      (c) => new NotMatchingBTBandBPD(c, fetchWidth=localBoomParams.fetchWidth)
    } should be (true)
  }
}

/**
 * Make sure that is the valid is set low, all outputs are set to invalid
 */
class ValidInputsTest(c: BranchChecker, fetchWidth: Int) extends PeekPokeTester(c)
{
  poke(c.io.insts.valid, false.B)
  step(1)
  expect(c.io.resp.valid, false.B)
}

class NotMatchingBTBandBPD(c: BranchChecker, fetchWidth: Int) extends PeekPokeTester(c)
{
  def NONE = 0
  def BRANCH = 1
  def JAL = 2
  def JALR = 3

  def decodedInstClear() = {
    for (i <- 0 until fetchWidth) {
      poke(c.io.insts.valid, false.B)
      poke(c.io.insts.bits.mask(i), false.B)
      poke(c.io.insts.bits.decode_signals(i).is_br,   false.B)

      poke(c.io.insts.bits.decode_signals(i).is_jal,  false.B)
      poke(c.io.insts.bits.decode_signals(i).is_jalr, false.B)
      poke(c.io.insts.bits.decode_signals(i).is_call, false.B)
      poke(c.io.insts.bits.decode_signals(i).is_ret,  false.B)
      poke(c.io.insts.bits.decode_signals(i).is_rvc,  false.B)
      poke(c.io.insts.bits.decode_signals(i).br_target, 0.U)
      poke(c.io.insts.bits.decode_signals(i).jal_target, 0.U)
    }

    poke(c.io.insts.bits.fetch_pc, 0.U)
    poke(c.io.insts.bits.aligned_pc, 0.U)
  }

  def btbClear() = {
    poke(c.io.btb_resp.valid, false.B)
    poke(c.io.btb_resp.bits.taken, false.B)
    poke(c.io.btb_resp.bits.target, 0.U)
    poke(c.io.btb_resp.bits.mask, 0.U)
    poke(c.io.btb_resp.bits.cfi_idx, 0.U)
    poke(c.io.btb_resp.bits.bpd_type, 0.U)
    poke(c.io.btb_resp.bits.cfi_type, 0.U)
    poke(c.io.btb_resp.bits.fetch_pc, 0.U)
  }

  def bpdClear() = {
    poke(c.io.bpd_resp.valid, false.B)
    poke(c.io.bpd_resp.bits.takens, 0.U)
    poke(c.io.bpd_resp.bits.history, 0.U)
    poke(c.io.bpd_resp.bits.info, 0.U)
  }

  decodedInstClear()
  btbClear()
  bpdClear()

  val inTarget = BigInt(0x2000)
  val fetchPc  = BigInt(0x1000)
  val cfiIdx = 0

  // -----------------------------------------------------
  // When the inst is a BR (btb says take and bpd says no)
  // -----------------------------------------------------
  poke(c.io.insts.valid, true.B)
  poke(c.io.insts.bits.mask(cfiIdx), true.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).is_br,   true.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).is_jal,  false.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).is_jalr, false.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).is_call, false.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).is_ret,  false.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).is_rvc,  false.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).br_target, inTarget.U)
  poke(c.io.insts.bits.fetch_pc, fetchPc.U)

  poke(c.io.btb_resp.valid, true.B)
  poke(c.io.btb_resp.bits.cfi_type, BRANCH)
  poke(c.io.btb_resp.bits.cfi_idx, cfiIdx)
  poke(c.io.btb_resp.bits.target, inTarget.U)
  poke(c.io.btb_resp.bits.taken, true.B)

  poke(c.io.bpd_resp.valid, true.B)
  poke(c.io.bpd_resp.bits.takens, 0.U) // first cfi is not a taken

  expect(c.io.resp.valid, 0.U) // the BTB resp is correct with the inst
                               //   thus no need to redir since BTB did it earlier

  step(1)

  // -----------------------------------------------------
  // When the inst is a BR (btb says no and bpd says take)
  // -----------------------------------------------------
  decodedInstClear()
  btbClear()
  bpdClear()

  poke(c.io.insts.valid, true.B)
  poke(c.io.insts.bits.mask(cfiIdx), true.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).is_br,   true.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).is_jal,  false.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).is_jalr, false.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).is_call, false.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).is_ret,  false.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).is_rvc,  false.B)
  poke(c.io.insts.bits.decode_signals(cfiIdx).br_target, inTarget.U)
  poke(c.io.insts.bits.fetch_pc, fetchPc.U)

  poke(c.io.btb_resp.valid, true.B)
  poke(c.io.btb_resp.bits.cfi_type, BRANCH)
  poke(c.io.btb_resp.bits.cfi_idx, cfiIdx)
  poke(c.io.btb_resp.bits.target, inTarget.U)
  poke(c.io.btb_resp.bits.taken, false.B)

  poke(c.io.bpd_resp.valid, true.B)
  poke(c.io.bpd_resp.bits.takens, 1.U) // first cfi is not a taken

  expect(c.io.resp.valid, 0.U) // the BTB targ matches inst
                               //   the BPD will redirect with this
                               //   thus no correctional redirect

  step(1)
}

class MatchingDecodeAndBTBTest(c: BranchChecker, fetchWidth: Int) extends PeekPokeTester(c)
{
  def NONE = 0
  def BRANCH = 1
  def JAL = 2
  def JALR = 3

  def decodedInstClear() = {
    for (i <- 0 until fetchWidth) {
      poke(c.io.insts.valid, false.B)
      poke(c.io.insts.bits.mask(i), false.B)
      poke(c.io.insts.bits.decode_signals(i).is_br,   false.B)

      poke(c.io.insts.bits.decode_signals(i).is_jal,  false.B)
      poke(c.io.insts.bits.decode_signals(i).is_jalr, false.B)
      poke(c.io.insts.bits.decode_signals(i).is_call, false.B)
      poke(c.io.insts.bits.decode_signals(i).is_ret,  false.B)
      poke(c.io.insts.bits.decode_signals(i).is_rvc,  false.B)
      poke(c.io.insts.bits.decode_signals(i).br_target, 0.U)
      poke(c.io.insts.bits.decode_signals(i).jal_target, 0.U)
    }

    poke(c.io.insts.bits.fetch_pc, 0.U)
    poke(c.io.insts.bits.aligned_pc, 0.U)
  }

  def btbClear() = {
    poke(c.io.btb_resp.valid, false.B)
    poke(c.io.btb_resp.bits.taken, false.B)
    poke(c.io.btb_resp.bits.target, 0.U)
    poke(c.io.btb_resp.bits.mask, 0.U)
    poke(c.io.btb_resp.bits.cfi_idx, 0.U)
    poke(c.io.btb_resp.bits.bpd_type, 0.U)
    poke(c.io.btb_resp.bits.cfi_type, 0.U)
    poke(c.io.btb_resp.bits.fetch_pc, 0.U)
  }

  def bpdClear() = {
    poke(c.io.bpd_resp.valid, false.B)
    poke(c.io.bpd_resp.bits.takens, 0.U)
    poke(c.io.bpd_resp.bits.history, 0.U)
    poke(c.io.bpd_resp.bits.info, 0.U)
  }
  val sameAddr = BigInt(0x1000)

  for (cfiIdx <- 0 until fetchWidth) {
    // ---------------
    // Matching Branch
    // ---------------
    decodedInstClear()
    btbClear()
    bpdClear()

    poke(c.io.insts.valid, true.B)
    poke(c.io.insts.bits.mask(cfiIdx), true.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_br,   true.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_jal,  false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_jalr, false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_call, false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_ret,  false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_rvc,  false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).br_target, sameAddr.U)

    poke(c.io.btb_resp.valid, true.B)
    poke(c.io.btb_resp.bits.cfi_type, BRANCH)
    poke(c.io.btb_resp.bits.cfi_idx, cfiIdx)
    poke(c.io.btb_resp.bits.target, sameAddr.U)
    poke(c.io.btb_resp.bits.taken, true.B)

    poke(c.io.bpd_resp.valid, false.B)

    expect(c.io.resp.valid, 0.U) // no redirection needed
    expect(c.io.btb_update.valid, 0.U) // dont update btb
    expect(c.io.ras_update.valid, 0.U) // dont update ras

    step(1)

    // ---------------
    // Matching JAL
    // ---------------
    decodedInstClear()
    btbClear()
    bpdClear()

    poke(c.io.insts.valid, true.B)
    poke(c.io.insts.bits.mask(cfiIdx), true.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_br,   false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_jal,  true.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_jalr, false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_call, false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_ret,  false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_rvc,  false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).jal_target, sameAddr.U)

    poke(c.io.btb_resp.valid, true.B)
    poke(c.io.btb_resp.bits.cfi_type, JAL)
    poke(c.io.btb_resp.bits.cfi_idx, cfiIdx)
    poke(c.io.btb_resp.bits.target, sameAddr.U)
    poke(c.io.btb_resp.bits.taken, true.B)

    poke(c.io.bpd_resp.valid, false.B)

    expect(c.io.resp.valid, 0.U) // no redirection needed
    expect(c.io.btb_update.valid, 0.U) // dont update btb
    expect(c.io.ras_update.valid, 0.U) // dont update ras

    step(1)

    // ---------------
    // Matching JALR
    // ---------------
    decodedInstClear()
    btbClear()
    bpdClear()

    poke(c.io.insts.valid, true.B)
    poke(c.io.insts.bits.mask(cfiIdx), true.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_br,   false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_jal,  false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_jalr, true.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_call, false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_ret,  false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).is_rvc,  false.B)
    poke(c.io.insts.bits.decode_signals(cfiIdx).jal_target, sameAddr.U)

    poke(c.io.btb_resp.valid, true.B)
    poke(c.io.btb_resp.bits.cfi_type, JALR)
    poke(c.io.btb_resp.bits.cfi_idx, cfiIdx)
    poke(c.io.btb_resp.bits.target, sameAddr.U)

    poke(c.io.bpd_resp.valid, false.B)

    expect(c.io.resp.valid, 0.U) // no redirection needed
    expect(c.io.btb_update.valid, 0.U) // dont update btb
    expect(c.io.ras_update.valid, 0.U) // dont update ras

    step(1)
  }
}

