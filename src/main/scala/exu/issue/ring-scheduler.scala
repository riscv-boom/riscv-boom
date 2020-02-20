//******************************************************************************
// Copyright (c) 2015 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Ring Microarchitecture Scheduler
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import FUConstants._
import boom.common._
import boom.util._

class RingScheduler(numSlots: Int, columnDispatchWidth: Int)
  (implicit p: Parameters) extends BoomModule with IssueUnitConstants
{
  val io = IO(new BoomBundle{
    val dis_uops = Flipped(Vec(coreWidth, DecoupledIO(new MicroOp)))
    val iss_uops = Output(Vec(coreWidth, Valid(new MicroOp)))

    val wakeups  = Input(Vec(coreWidth*2, Valid(UInt(ipregSz.W))))
    val ld_miss  = Input(UInt(coreWidth.W)) // Per-column load miss vector

    val fu_avail = Input(UInt(FUC_SZ.W))

    val brupdate = Input(new BrUpdateInfo)
    val kill     = Input(Bool())
  })

  val numSlotsPerColumn = numSlots / coreWidth
  require (numSlots % coreWidth == 0)

  //----------------------------------------------------------------------------------------------------
  // Generate table of issue slots

  val issue_table = Seq.fill(coreWidth)( Seq.fill(numSlotsPerColumn)( Module(new RingIssueSlot) ) )
  val slots = VecInit(issue_table.map(col => VecInit(col.map(_.io))))

  for (w <- 0 until coreWidth) {
    for (i <- 0 until numSlotsPerColumn) {
      slots(w)(i).slow_wakeups := io.wakeups
      slots(w)(i).ld_miss      := io.ld_miss((w - 1 + coreWidth) % coreWidth)

      slots(w)(i).brupdate := io.brupdate
      slots(w)(i).kill     := io.kill

      slots(w)(i).fu_avail := io.fu_avail
    }
  }

  //----------------------------------------------------------------------------------------------------
  // Dispatch

  // TODO clean this up
  val dis_uops_setup = Wire(Vec(coreWidth, new MicroOp))
  for (w <- 0 until coreWidth) {
    dis_uops_setup(w) := io.dis_uops(w).bits
    dis_uops_setup(w).iw_state := s_valid_1

    dis_uops_setup(w).prs1_status := Mux(io.dis_uops(w).bits.prs1_busy, 0.U, 1.U)
    dis_uops_setup(w).prs2_status := Mux(io.dis_uops(w).bits.prs2_busy, 0.U, 1.U)

    // For StoreAddrGen for Int, or AMOAddrGen, we go to addr gen state
    when ((io.dis_uops(w).bits.uopc === uopSTA && io.dis_uops(w).bits.lrs2_rtype === RT_FIX) ||
           io.dis_uops(w).bits.uopc === uopAMO_AG) {
      dis_uops_setup(w).iw_state := s_valid_2
      // For store addr gen for FP, rs2 is the FP register, and we don't wait for that here
    } .elsewhen (io.dis_uops(w).bits.uopc === uopSTA && io.dis_uops(w).bits.lrs2_rtype =/= RT_FIX) {
      dis_uops_setup(w).lrs2_rtype := RT_X
      dis_uops_setup(w).prs2_status := 1.U
      dis_uops_setup(w).prs2_busy   := false.B
    }
    dis_uops_setup(w).prs3_busy := false.B
  }

  val dis_uops = Wire(Vec(coreWidth, Vec(columnDispatchWidth, new MicroOp)))
  val dis_vals = Wire(Vec(coreWidth, Vec(columnDispatchWidth, Bool())))

  require (columnDispatchWidth == coreWidth) // TODO implement an arbitration / compaction mechanism

  for (w <- 0 until coreWidth) {
    dis_uops(w) := dis_uops_setup
  }

  dis_vals := Transpose(VecInit(io.dis_uops.map(uop => VecInit((uop.bits.pdst_col & Fill(coreWidth, uop.valid)).asBools))))

  val col_readys = Transpose(VecInit((0 until coreWidth).map(w =>
    VecInit((0 until columnDispatchWidth).map(k => PopCount(slots(w).map(_.valid)) + k.U < numSlotsPerColumn.U)).asUInt)))

  for (w <- 0 until coreWidth) {
    io.dis_uops(w).ready := (io.dis_uops(w).bits.pdst_col & col_readys(w)).orR
  }

  //----------------------------------------------------------------------------------------------------
  // Selection

  val iss_sels = Wire(Vec(coreWidth, Vec(numSlotsPerColumn, Bool())))
  val sel_uops = Wire(Vec(coreWidth, new MicroOp))
  val sel_vals = Wire(Vec(coreWidth, Bool()))

  for (w <- 0 until coreWidth) {
    val col_reqs = slots(w).map(_.request)
    val col_uops = slots(w).map(_.uop)

    iss_sels(w) := PriorityEncoderOH(col_reqs)
    sel_uops(w) := Mux1H(iss_sels(w), col_uops)
    sel_vals(w) := iss_sels(w).reduce(_||_)
  }

  //----------------------------------------------------------------------------------------------------
  // Arbitration

  val rrd_arb = Module(new RegisterReadArbiter)
  val exu_arb = Module(new ExecutionArbiter)
  val wb_arb  = Module(new WritebackArbiter)

  val arbiters = Seq(rrd_arb, exu_arb, wb_arb)

  var arb_gnts = ~(0.U(coreWidth.W))

  for (arb <- arbiters) {
    arb.io.uops := sel_uops
    arb.io.reqs := sel_vals
    arb.io.fire := DontCare
    arb_gnts = arb_gnts & arb.io.gnts.asUInt
  }

  wb_arb.io.fire := arb_gnts.asBools

  //----------------------------------------------------------------------------------------------------
  // Grant, Fast Wakeup, and Issue

  val do_issue = arb_gnts & ~RotateLeft(io.ld_miss)

  // Grant signals
  for (w <- 0 until coreWidth) {
    for (i <- 0 until numSlotsPerColumn) {
      slots(w)(i).grant := iss_sels(w)(i) && do_issue(w)
    }
  }

  // Connect fast wakeups
  for (w <- 0 until coreWidth) {
    for (slot <- slots((w + 1) % coreWidth)) {
      slot.fast_wakeup := sel_uops(w).fast_wakeup(do_issue(w))
    }
  }

  // Hookup issue output
  for (w <- 0 until coreWidth) {
    io.iss_uops(w).bits  := sel_uops(w)
    io.iss_uops(w).valid := do_issue(w)
  }

  //----------------------------------------------------------------------------------------------------
  // Compaction

  for (w <- 0 until coreWidth) {
    val valids = slots(w).map(_.valid) ++ dis_vals(w)
    val uops = slots(w).map(_.out_uop) ++ dis_uops(w)
    val next_valids = slots(w).map(_.will_be_valid) ++ dis_vals(w)

    val max = columnDispatchWidth
    def Inc(count: UInt, inc: Bool) = Mux(inc && !count(max), count << 1, count)(max,0)

    val counts = valids.scanLeft(1.U((max+1).W))((c,v) => Inc(c,!v))
    val sels = (counts zip valids).map { case (c,v) => c(max,1) & Fill(max,v) }
                .takeRight(numSlotsPerColumn + max - 1)

    for (i <- 0 until numSlotsPerColumn) {
      val uop_sel = (0 until max).map(j => sels(i+j)(j))

      slots(w)(i).in_uop.bits  := Mux1H(uop_sel, uops.slice(i+1,i+max+1))
      slots(w)(i).in_uop.valid := Mux1H(uop_sel, next_valids.slice(i+1,i+max+1))

      slots(w)(i).clear := !counts(i)(0)
    }
  }
}
