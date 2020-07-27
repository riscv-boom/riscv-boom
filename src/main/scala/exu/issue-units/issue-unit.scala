//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.{Str}

import boom.common._
import boom.exu.FUConstants._
import boom.util.{BoolToChar}

/**
 * Class used for configurations
 *
 * @param issueWidth amount of things that can be issued
 * @param numEntries size of issue queue
 * @param iqType type of issue queue
 */
case class IssueParams(
  dispatchWidth: Int = 1,
  issueWidth: Int = 1,
  numEntries: Int = 8,
  useFullIssueSel: Boolean = true,
  numSlowEntries: Int = 0,
  iqType: BigInt
)


/**
 * IO bundle to interact with the issue unit
 *
 * @param issueWidth amount of operations that can be issued at once
 * @param numWakeupPorts number of wakeup ports for issue unit
 */
class IssueUnitIO(
  val issueWidth: Int,
  val numWakeupPorts: Int,
  val dispatchWidth: Int)
  (implicit p: Parameters) extends BoomBundle
{
  val dis_uops         = Vec(dispatchWidth, Flipped(Decoupled(new MicroOp)))

  val iss_uops         = Output(Vec(issueWidth, Valid(new MicroOp())))
  val wakeup_ports     = Flipped(Vec(numWakeupPorts, Valid(new ExeUnitResp(xLen))))
  val pred_wakeup_port = Flipped(Valid(UInt(log2Ceil(ftqSz).W)))

  val spec_ld_wakeup   = Flipped(Vec(lsuWidth, Valid(UInt(width=maxPregSz.W))))

  // tell the issue unit what each execution pipeline has in terms of functional units
  val fu_types         = Input(Vec(issueWidth, Bits(width=FUC_SZ.W)))

  val brupdate         = Input(new BrUpdateInfo())
  val flush_pipeline   = Input(Bool())
  val ld_miss          = Input(Bool())
  val squash_grant     = Input(Bool())

  val tsc_reg          = Input(UInt(width=xLen.W))
}

/**
 * Abstract top level issue unit
 */
abstract class IssueUnit(
  val numIssueSlots: Int,
  val issueWidth: Int,
  val numWakeupPorts: Int,
  val iqType: BigInt,
  val dispatchWidth: Int)
  (implicit p: Parameters)
  extends BoomModule
{
  val io = IO(new IssueUnitIO(issueWidth, numWakeupPorts, dispatchWidth))

  //-------------------------------------------------------------
  // Set up the dispatch uops
  // special case "storing" 2 uops within one issue slot.

  val dis_uops = Array.fill(dispatchWidth) {Wire(new MicroOp())}
  for (w <- 0 until dispatchWidth) {
    dis_uops(w) := io.dis_uops(w).bits

    dis_uops(w).iw_p1_poisoned := false.B
    dis_uops(w).iw_p2_poisoned := false.B
    dis_uops(w).iw_p1_bypass_hint := false.B
    dis_uops(w).iw_p2_bypass_hint := false.B

    // Handle wakeups on dispatch
    for (wakeup <- io.wakeup_ports) {
      when (wakeup.valid) {
        when (wakeup.bits.uop.pdst === io.dis_uops(w).bits.prs1) {
          dis_uops(w).prs1_busy := false.B
          dis_uops(w).iw_p1_bypass_hint := wakeup.bits.uop.bypassable
        }
        when (wakeup.bits.uop.pdst === io.dis_uops(w).bits.prs2) {
          dis_uops(w).prs2_busy := false.B
          dis_uops(w).iw_p2_bypass_hint := wakeup.bits.uop.bypassable
        }
        when (wakeup.bits.uop.pdst === io.dis_uops(w).bits.prs3) {
          dis_uops(w).prs3_busy := false.B
        }
      }
    }
    when (io.pred_wakeup_port.valid && io.pred_wakeup_port.bits === io.dis_uops(w).bits.ppred) {
      dis_uops(w).ppred_busy := false.B
    }
    for (spec_wakeup <- io.spec_ld_wakeup) {
      when (spec_wakeup.valid) {
        when (spec_wakeup.bits === io.dis_uops(w).bits.prs1) {
          dis_uops(w).prs1_busy := false.B
          dis_uops(w).iw_p1_poisoned := true.B
        }
        when (spec_wakeup.bits === io.dis_uops(w).bits.prs2) {
          dis_uops(w).prs2_busy := false.B
          dis_uops(w).iw_p2_poisoned := true.B
        }
      }
    }

    if (iqType == IQT_MEM.litValue || iqType == IQT_INT.litValue) {
      // For store addr gen for FP, rs2 is the FP register, and we don't wait for that here
      when (io.dis_uops(w).bits.uses_stq && io.dis_uops(w).bits.lrs2_rtype === RT_FLT) {
        dis_uops(w).lrs2_rtype := RT_X
        dis_uops(w).prs2_busy  := false.B
      }
      dis_uops(w).prs3_busy := false.B
    } else if (iqType == IQT_FP.litValue) {
      // FP "StoreAddrGen" is really storeDataGen, and rs1 is the integer address register
      when (io.dis_uops(w).bits.uses_stq) {
        dis_uops(w).lrs1_rtype := RT_X
        dis_uops(w).prs1_busy  := false.B
      }
    }

    if (iqType != IQT_INT.litValue) {
      assert(!(io.dis_uops(w).bits.ppred_busy && io.dis_uops(w).valid))
      dis_uops(w).ppred_busy := false.B
    }

  }

  //-------------------------------------------------------------
  // Issue Table

  val slots = for (i <- 0 until numIssueSlots) yield { val slot = Module(new IssueSlot(numWakeupPorts, iqType == IQT_MEM.litValue, iqType == IQT_FP.litValue)); slot }
  val issue_slots = VecInit(slots.map(_.io))

  for (i <- 0 until numIssueSlots) {
    issue_slots(i).wakeup_ports     := io.wakeup_ports
    issue_slots(i).pred_wakeup_port := io.pred_wakeup_port
    issue_slots(i).spec_ld_wakeup   := io.spec_ld_wakeup
    issue_slots(i).ldspec_miss      := io.ld_miss
    issue_slots(i).squash_grant     := io.squash_grant
    issue_slots(i).brupdate         := io.brupdate
    issue_slots(i).kill             := io.flush_pipeline
  }


  for (w <- 0 until issueWidth) {
    io.iss_uops(w).valid := false.B
  }


  //-------------------------------------------------------------

  assert (PopCount(issue_slots.map(s => s.grant)) <= issueWidth.U, "[issue] window giving out too many grants.")


  //-------------------------------------------------------------


  def getType: String =
    if (iqType == IQT_INT.litValue) "int"
    else if (iqType == IQT_MEM.litValue) "mem"
    else if (iqType == IQT_FP.litValue) " fp"
    else "unknown"
}
