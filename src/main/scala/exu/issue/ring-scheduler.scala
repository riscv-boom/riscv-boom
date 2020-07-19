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
  val io = IO(new BoomBundle {
    val dis_uops = Flipped(Vec(coreWidth, DecoupledIO(new MicroOp)))
    val iss_uops = Output(Vec(coreWidth, Valid(new MicroOp)))

    val dis_valids = Input(Vec(coreWidth, Bool()))

    val slow_wakeups = Input(Vec(2*coreWidth, Valid(UInt(ipregSz.W))))
    val load_wakeups = Input(Vec(   memWidth, Valid(UInt(ipregSz.W))))
    val   ll_wakeups = Input(Vec(   memWidth, Valid(UInt(ipregSz.W))))
    val load_nacks   = Input(Vec(   memWidth, Bool()))
    val pred_wakeup  = Input(Valid(UInt(ftqSz.W)))

    val fast_wakeups = Output(Vec(coreWidth , Valid(UInt(ipregSz.W))))

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
      slots(w)(i).load_wakeups := io.load_wakeups
      slots(w)(i).ll_wakeups   := io.ll_wakeups
      slots(w)(i).load_nacks   := io.load_nacks
      slots(w)(i).pred_wakeup  := io.pred_wakeup

      slots(w)(i).brupdate := io.brupdate
      slots(w)(i).kill     := io.kill

      slots(w)(i).fu_avail := io.fu_avail
    }
  }

  //----------------------------------------------------------------------------------------------------
  // Dispatch

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
      dis_uops_setup(w).lrs2_rtype  := RT_X
      dis_uops_setup(w).prs2_status := 1.U
      dis_uops_setup(w).prs2_busy   := false.B
    }

    dis_uops_setup(w).prs3_busy := false.B

    assert (!(Mux(dis_uops_setup(w).dst_rtype === RT_FIX && io.dis_valids(w), dis_uops_setup(w).pdst_col, 0.U) &
            ~dis_uops_setup(w).column).orR, "[iss] uop column should match pdst column when the uop writes an int pdst")
  }

  val dis_reqs = Transpose(dis_uops_setup zip io.dis_valids map { case (u,v) =>
    Mux(v, u.column | Mux(u.prs1_busy && u.prs2_busy && !u.prs2_load, RotateLeft(u.prs2_col), 0.U), 0.U) })
  val dis_uops = Wire(Vec(coreWidth, Vec(columnDispatchWidth, new MicroOp)))
  val dis_vals = Wire(Vec(coreWidth, Vec(columnDispatchWidth, Bool())))
  val dis_gnts = Wire(Vec(coreWidth, UInt(coreWidth.W)))

  for (w <- 0 until coreWidth) {
    val dis_sels = SelectFirstN(dis_reqs(w), columnDispatchWidth)

    for (d <- 0 until columnDispatchWidth) {
      val dis_uop     = Mux1H(dis_sels(d), dis_uops_setup)
      dis_uops(w)(d) := dis_uop
      dis_vals(w)(d) := Mux1H(dis_sels(d), io.dis_uops.map(_.valid))

      when (!dis_uop.column(w)) {
        dis_uops(w)(d).iw_state := s_valid_3
      }
    }

    val col_rdys = (0 until columnDispatchWidth).map(d => PopCount(slots(w).map(_.valid)) +& d.U < numSlotsPerColumn.U)
    dis_gnts(w) := dis_sels zip col_rdys map { case (s,r) => Mux(r, s, 0.U) } reduce(_|_)
  }

  val dis_stalls = dis_reqs zip dis_gnts map { case (r,g) => r & ~g } reduce(_|_)

  for (w <- 0 until coreWidth) {
    io.dis_uops(w).ready := !dis_stalls(w)
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
    sel_vals(w) := col_reqs.reduce(_||_)
  }

  //-------------------------
  // Chain selection

  val chain_sels = Wire(Vec(coreWidth, Vec(numSlotsPerColumn, Bool())))
  val chain_uops = Wire(Vec(coreWidth, new MicroOp))
  val chain_vals = Wire(Vec(coreWidth, Bool()))

  for (w <- 0 until coreWidth) {
    val chain_reqs = slots(w).map(_.request_chain)
    val col_uops   = slots(w).map(_.uop)

    chain_sels(w) := PriorityEncoderOH(chain_reqs)
    chain_uops(w) := Mux1H(chain_sels(w), col_uops)
    chain_vals(w) := chain_reqs.reduce(_||_)
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

  for (w <- 0 until coreWidth) {
    sel_uops(w).prs1_port := rrd_arb.io.prs1_port(w)
    sel_uops(w).prs2_port := rrd_arb.io.prs2_port(w)
  }

  //-------------------------
  // Chain arbitration

  val chain_arb = Module(new ChainedWakeupArbiter)
  chain_arb.io.uops := chain_uops
  chain_arb.io.reqs := chain_vals
  chain_arb.io.fire := DontCare

  val r_chain_uops = RegNext(chain_uops)
  val r_chain_vals = RegNext(chain_arb.io.gnts)

  //----------------------------------------------------------------------------------------------------
  // Grant, Fast Wakeup, and Issue

  val do_issue = arb_gnts & ~VecInit(sel_uops.map(_.load_wakeup_nacked(io.load_nacks))).asUInt

  // Grant signals
  for (w <- 0 until coreWidth) {
    for (i <- 0 until numSlotsPerColumn) {
      slots(w)(i).grant := iss_sels(w)(i) && do_issue(w) || chain_sels(w)(i) && chain_arb.io.gnts(w)
    }
  }

  // Generate fast wakeups
  for (w <- 0 until coreWidth) {
    val fast_wakeup  = sel_uops(w).fast_wakeup(do_issue(w))

    // Broadcast to slots in next column
    for (slot <- slots((w + 1) % coreWidth)) {
      slot.fast_wakeup     := fast_wakeup
      slot.slow_wakeups(0) := io.slow_wakeups(w)
      slot.slow_wakeups(1) := io.slow_wakeups(w + coreWidth)
    }

    // Send to rename (ALU only)
    io.fast_wakeups(w).valid := RegNext(fast_wakeup.valid && fast_wakeup.bits.alu)
    io.fast_wakeups(w).bits  := RegNext(fast_wakeup.bits.pdst)
  }

  // Generate chained wakeups
  val chain_xbar_reqs = Transpose(r_chain_uops zip r_chain_vals map { case (u,v) => Mux(v, u.column, 0.U) })
  for (w <- 0 until coreWidth) {
    val chain_uop    = Mux1H(chain_xbar_reqs(w), r_chain_uops)
    val chain_wakeup = Wire(Valid(UInt(ipregSz.W)))
    chain_wakeup.bits  := chain_uop.prs2
    chain_wakeup.valid := chain_xbar_reqs(w).orR

    for (slot <- slots(w)) {
      slot.chain_wakeup := chain_wakeup
    }
  }

  // Hookup issue output
  for (w <- 0 until coreWidth) {
    io.iss_uops(w).bits  := sel_uops(w)
    io.iss_uops(w).valid := do_issue(w)

    assert (PopCount(sel_uops(w).prs1_status) === 1.U || sel_uops(w).lrs1_rtype === RT_X || !do_issue(w),
            "Operand status should be one-hot at issue")
    assert (PopCount(sel_uops(w).prs2_status) === 1.U || sel_uops(w).lrs2_rtype === RT_X || !do_issue(w),
            "Operand status should be one-hot at issue")
  }

  //----------------------------------------------------------------------------------------------------
  // Compaction and Dispatch Ports

  val numCompactionPorts = 1
  val numDispatchPorts   = columnDispatchWidth - numCompactionPorts

  for (w <- 0 until coreWidth) {
    val valids        = slots(w).map(_.valid)         ++ dis_vals(w).takeRight(numCompactionPorts)
    val uops          = slots(w).map(_.out_uop)       ++ dis_uops(w).takeRight(numCompactionPorts)
    val will_be_valid = slots(w).map(_.will_be_valid) ++ Seq.fill(numCompactionPorts) (true.B)

    val max = numCompactionPorts
    def Inc(count: UInt, inc: Bool) = Mux(inc && !count(max), count << 1, count)(max,0)

    val slot_counts = valids.dropRight(max).scanLeft(1.U((max+1).W)) ((c,v) => Inc(c,!v))
    val sel_counts  = slot_counts ++ Seq.fill(max-1)(slot_counts.last)
    val comp_sels   = (sel_counts zip valids).map{ case (c,v) => c(max,1) & Fill(max,v) }
                        .takeRight(numSlotsPerColumn + max - 1)

    // Which slots might be valid after compaction?
    var compacted_valids = Wire(Vec(numSlotsPerColumn, Bool()))
    for (i <- 0 until numSlotsPerColumn) {
      compacted_valids(i) := valids(i) && slot_counts(i)(0) || (0 until max).map(j =>
                             if (i+j+1 < numSlotsPerColumn) comp_sels(i+j)(j) else false.B).reduce(_||_)
    }

    // Select the lowest post-compaction free slots for the main dispatch ports
    val dispatch_slots = SelectFirstN(~compacted_valids.asUInt, numDispatchPorts)

    // Generate the slot writeport muxes
    for (i <- 0 until numSlotsPerColumn) {
      var uop_sel = (0 until max).map(j => comp_sels(i+j)(j))
      if (numDispatchPorts > 0) uop_sel = uop_sel ++ dispatch_slots.map(d => d(i))

      slots(w)(i).in_uop.bits  := Mux1H(uop_sel,          uops.slice(i+1,i+max+1) ++ dis_uops(w).dropRight(max))
      slots(w)(i).in_uop.valid := Mux1H(uop_sel, will_be_valid.slice(i+1,i+max+1) ++ dis_vals(w).dropRight(max))

      slots(w)(i).clear := !slot_counts(i)(0)
    }
  }
}
