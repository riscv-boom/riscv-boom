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
  iqType: Int
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
  val wakeup_ports     = Flipped(Vec(numWakeupPorts, Valid(new Wakeup)))
  val pred_wakeup_port = Flipped(Valid(UInt(log2Ceil(ftqSz).W)))

  val child_rebusys    = Input(UInt(intWidth.W))

  // tell the issue unit what each execution pipeline has in terms of functional units
  val fu_types         = Input(Vec(issueWidth, Vec(FC_SZ, Bool())))

  val brupdate         = Input(new BrUpdateInfo())
  val flush_pipeline   = Input(Bool())
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
  val iqType: Int,
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
    dis_uops(w).iw_issued := false.B
    dis_uops(w).iw_issued_partial_agen := false.B
    dis_uops(w).iw_issued_partial_dgen := false.B
    dis_uops(w).iw_p1_bypass_hint := false.B
    dis_uops(w).iw_p2_bypass_hint := false.B

    // Handle wakeups on dispatch
    val prs1_matches = io.wakeup_ports.map { wu => wu.bits.uop.pdst === io.dis_uops(w).bits.prs1 }
    val prs2_matches = io.wakeup_ports.map { wu => wu.bits.uop.pdst === io.dis_uops(w).bits.prs2 }
    val prs3_matches = io.wakeup_ports.map { wu => wu.bits.uop.pdst === io.dis_uops(w).bits.prs3 }
    val prs1_wakeups = (io.wakeup_ports zip prs1_matches).map { case (wu,m) => wu.valid && m }
    val prs2_wakeups = (io.wakeup_ports zip prs2_matches).map { case (wu,m) => wu.valid && m }
    val prs3_wakeups = (io.wakeup_ports zip prs3_matches).map { case (wu,m) => wu.valid && m }
    val prs1_rebusys = (io.wakeup_ports zip prs1_matches).map { case (wu,m) => wu.bits.rebusy && m }
    val prs2_rebusys = (io.wakeup_ports zip prs2_matches).map { case (wu,m) => wu.bits.rebusy && m }
    val bypassables  = io.wakeup_ports.map { wu => wu.bits.bypassable }
    val speculative_masks = io.wakeup_ports.map { wu => wu.bits.speculative_mask }



    when (prs1_wakeups.reduce(_||_)) {
      dis_uops(w).prs1_busy := false.B
      dis_uops(w).iw_p1_speculative_child := Mux1H(prs1_wakeups, speculative_masks)
      dis_uops(w).iw_p1_bypass_hint := Mux1H(prs1_wakeups, bypassables)
    }
    when (prs1_rebusys.reduce(_||_) || ((io.child_rebusys & io.dis_uops(w).bits.iw_p1_speculative_child) =/= 0.U)) {
      dis_uops(w).prs1_busy := io.dis_uops(w).bits.lrs1_rtype === RT_FIX
    }
    when (prs2_wakeups.reduce(_||_)) {
      dis_uops(w).prs2_busy := false.B
      dis_uops(w).iw_p2_speculative_child := Mux1H(prs2_wakeups, speculative_masks)
      dis_uops(w).iw_p2_bypass_hint := Mux1H(prs2_wakeups, bypassables)
    }

    when (prs2_rebusys.reduce(_||_) || ((io.child_rebusys & io.dis_uops(w).bits.iw_p2_speculative_child) =/= 0.U)) {
      dis_uops(w).prs2_busy := io.dis_uops(w).bits.lrs2_rtype === RT_FIX
    }


    when (prs3_wakeups.reduce(_||_)) {
      dis_uops(w).prs3_busy := false.B
    }
    when (io.pred_wakeup_port.valid && io.pred_wakeup_port.bits === io.dis_uops(w).bits.ppred) {
      dis_uops(w).ppred_busy := false.B
    }


    if (iqType == IQ_INT) {
      when (io.dis_uops(w).bits.fu_code(FC_I2F)) {
        dis_uops(w).prs2 := Cat(io.dis_uops(w).bits.fp_rm, io.dis_uops(w).bits.fp_typ)
      }
      when (io.dis_uops(w).bits.uopc === uopSFENCE) {
        dis_uops(w).pimm := io.dis_uops(w).bits.mem_size
      }
    }

    if (iqType == IQ_MEM|| iqType == IQ_INT) {
      // For store addr gen for FP, rs2 is the FP register, and we don't wait for that here
      when (io.dis_uops(w).bits.uses_stq && io.dis_uops(w).bits.lrs2_rtype === RT_FLT) {
        dis_uops(w).lrs2_rtype := RT_X
        dis_uops(w).prs2_busy  := false.B
      }
      dis_uops(w).prs3_busy := false.B
    } else if (iqType == IQ_FP) {
      // FP "StoreAddrGen" is really storeDataGen, and rs1 is the integer address register
      when (io.dis_uops(w).bits.uses_stq) {
        dis_uops(w).lrs1_rtype := RT_X
        dis_uops(w).prs1_busy  := false.B
      }
    }

    if (iqType != IQ_INT) {
      assert(!(io.dis_uops(w).bits.ppred_busy && io.dis_uops(w).valid))
      dis_uops(w).ppred_busy := false.B
    }

  }

  //-------------------------------------------------------------
  // Issue Table

  val slots = (0 until numIssueSlots) map { w => Module(new IssueSlot(numWakeupPorts, iqType == IQ_MEM, iqType == IQ_FP)) }
  val issue_slots = VecInit(slots.map(_.io))

  for (i <- 0 until numIssueSlots) {
    issue_slots(i).wakeup_ports     := io.wakeup_ports
    issue_slots(i).pred_wakeup_port := io.pred_wakeup_port
    issue_slots(i).child_rebusys    := io.child_rebusys
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
    if (iqType == IQ_INT) "int"
    else if (iqType == IQ_MEM) "mem"
    else if (iqType == IQ_FP) " fp"
    else "unknown"
}
