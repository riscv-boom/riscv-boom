//******************************************************************************
// Copyright (c) 2012 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Datapath: Rename Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Supports 1-cycle and 2-cycle latencies. (aka, passthrough versus registers between ren1 and ren2).
//    - ren1: read the map tables and allocate a new physical register from the freelist.
//    - ren2: read the busy table for the physical operands.
//
// Ren1 data is provided as an output to be fed directly into the ROB.

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util._

/**
 * IO bundle to interface with the Register Rename logic
 *
 * @param plWidth pipeline width
 * @param numIntPregs number of int physical registers
 * @param numFpPregs number of FP physical registers
 * @param numIntWbPorts number of int writeback ports
 * @param numFpWbPorts number of FP writeback ports
 */
class RenameStageIO(
  val plWidth: Int,
  val numIntPregs: Int,
  val numFpPregs: Int,
  val numIntWbPorts: Int,
  val numFpWbPorts: Int)
  (implicit p: Parameters) extends BoomBundle
{
  private val int_preg_sz = log2Ceil(numIntPregs)
  private val fp_preg_sz = log2Ceil(numFpPregs)

  val inst_can_proceed = Output(Vec(plWidth, Bool()))

  val kill = Input(Bool())

  val dec_will_fire = Input(Vec(plWidth, Bool())) // will commit state updates
  val dec_uops  = Input(Vec(plWidth, new MicroOp()))

  // physical specifiers now available (but not the busy/ready status of the operands).
  val ren1_mask = Vec(plWidth, Output(Bool())) // mask of valid instructions
  val ren1_uops = Vec(plWidth, Output(new MicroOp()))

  // physical specifiers available AND busy/ready status available.
  val ren2_mask = Vec(plWidth, Output(Bool())) // mask of valid instructions
  val ren2_uops = Vec(plWidth, Output(new MicroOp()))

  // branch resolution (execute)
  val brinfo = Input(new BrResolutionInfo())

  val dis_inst_can_proceed = Input(Vec(coreWidth, Bool()))

  // wakeup ports
  val int_wakeups = Flipped(Vec(numIntWbPorts, Valid(new ExeUnitResp(xLen))))
  val fp_wakeups = Flipped(Vec(numFpWbPorts, Valid(new ExeUnitResp(fLen+1))))

  // commit stage
  val com_valids = Input(Vec(plWidth, Bool()))
  val com_uops = Input(Vec(plWidth, new MicroOp()))
  val rbk_valids = Input(Vec(plWidth, Bool()))
  val rollback = Input(Bool())

  val flush_pipeline = Input(Bool()) // only used for SCR (single-cycle reset)

  val debug_rob_empty = Input(Bool())
  val debug = Output(new DebugRenameStageIO(numIntPregs, numFpPregs))
}

/**
 * IO bundle to debug the rename stage
 *
 * @param int_numPregs number of int physical registers
 * @param fp_numPregs number of FP physical registers
 */
class DebugRenameStageIO(val int_numPregs: Int, val fp_numPregs: Int)(implicit p: Parameters) extends BoomBundle
{
  val ifreelist  = Bits(int_numPregs.W)
  val iisprlist  = Bits(int_numPregs.W)
  val ibusytable = UInt(int_numPregs.W)
  val ffreelist  = Bits(fp_numPregs.W)
  val fisprlist  = Bits(fp_numPregs.W)
  val fbusytable = UInt(fp_numPregs.W)
}

/**
 * Rename stage that connets the map table, free list, and busy table.
 * Can be used in both the FP pipeline and the normal execute pipeline.
 *
 * @param plWidth pipeline width
 * @param numIntWbPorts number of int writeback ports
 * @param numFpWbPorts number of FP writeback ports
 */
class RenameStage(
  plWidth: Int,
  numIntWbPorts: Int,
  numFpWbPorts: Int)
(implicit p: Parameters) extends BoomModule
{
  val io = IO(new RenameStageIO(plWidth, numIntPhysRegs, numFpPhysRegs, numIntWbPorts, numFpWbPorts))

  // integer registers
  val imaptable = Module(new RenameMapTable(
    plWidth,
    32,
    numIntPhysRegs,
    false))
  val ifreelist = Module(new RenameFreeList(
    plWidth,
    numIntPhysRegs,
    false))
  val ibusytable = Module(new RenameBusyTable(
    plWidth,
    numIntPhysRegs,
    numIntWbPorts,
    false))

  // floating point registers
  var fmaptable: RenameMapTable = null
  var ffreelist: RenameFreeList = null
  var fbusytable: RenameBusyTable = null

  if (usingFPU) {
    fmaptable = Module(new RenameMapTable(
      plWidth,
      32,
      numFpPhysRegs,
      true))
    ffreelist = Module(new RenameFreeList(
      plWidth,
      numFpPhysRegs,
      true))
    fbusytable = Module(new RenameBusyTable(
      plWidth,
      numFpPhysRegs,
      numFpWbPorts,
      true))
  }

  //-------------------------------------------------------------
  // Pipeline State & Wires

  val ren1_br_tags   = Wire(Vec(plWidth, Valid(UInt(BR_TAG_SZ.W))))
  val ren1_will_fire = Wire(Vec(plWidth, Bool()))
  val ren1_uops      = Wire(Vec(plWidth, new MicroOp()))

  val ren1_int_req_alloc = Wire(Vec(plWidth, Bool()))
  val ren1_fp_req_alloc  = Wire(Vec(plWidth, Bool()))

  val rob_int_ldst_vals  = Wire(Vec(plWidth, Bool()))
  val rob_fp_ldst_vals   = Wire(Vec(plWidth, Bool()))

  val ren2_int_req_alloc = Wire(Vec(plWidth, Bool()))
  val ren2_fp_req_alloc  = Wire(Vec(plWidth, Bool()))

  val ren2_valids    = Wire(Vec(plWidth, Bool()))
  val ren2_uops      = Wire(Vec(plWidth, new MicroOp()))
  val ren2_will_fire = ren2_valids zip io.dis_inst_can_proceed map {case (v,c) => v && c && !io.kill}

  for (w <- 0 until plWidth) {
    ren1_will_fire(w) := io.dec_will_fire(w)
    ren1_uops(w)      := io.dec_uops(w)
    ren1_br_tags(w).valid := ren1_will_fire(w) && io.dec_uops(w).allocate_brtag
    ren1_br_tags(w).bits  := io.dec_uops(w).br_tag

    ren1_int_req_alloc(w) := ren1_uops(w).ldst_val && ren1_uops(w).dst_rtype === RT_FIX && ren1_will_fire(w)
    ren1_fp_req_alloc(w)  := ren1_uops(w).ldst_val && ren1_uops(w).dst_rtype === RT_FLT && ren1_will_fire(w)

    ren2_int_req_alloc(w) := ren2_uops(w).ldst_val && ren2_uops(w).dst_rtype === RT_FIX && ren2_will_fire(w)
    ren2_fp_req_alloc(w)  := ren2_uops(w).ldst_val && ren2_uops(w).dst_rtype === RT_FLT && ren2_will_fire(w)

    rob_int_ldst_vals(w)  := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === RT_FIX
    rob_fp_ldst_vals(w)   := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === RT_FLT
  }

  var ren1_alloc_reqs = Seq(ren1_int_req_alloc)
  if (usingFPU) ren1_alloc_reqs ++= Seq(ren1_fp_req_alloc)

  var rob_ldst_vals = Seq(rob_int_ldst_vals)
  if (usingFPU) rob_ldst_vals ++= Seq(rob_fp_ldst_vals)

  //-------------------------------------------------------------
  // Free List

  var freelists = Seq(ifreelist)
  if (usingFPU) freelists ++= Seq(ffreelist)
  for ((list, i) <- freelists.zipWithIndex) {
    list.io.reqs := ren1_alloc_reqs(i)
    list.io.brinfo := io.brinfo
    list.io.ren_br_tags := ren1_br_tags
    list.io.rob_uops := io.com_uops
    list.io.com_valids := io.com_valids zip rob_ldst_vals(i) map {case (v,l) => v && l}
    list.io.rbk_valids := io.rbk_valids zip rob_ldst_vals(i) map {case (r,l) => r && l}
    list.io.rollback := io.rollback
    list.io.debug.rob_empty := io.debug_rob_empty
  }

  for ((uop, w) <- ren1_uops.zipWithIndex) {
    val i_preg = ifreelist.io.alloc_pregs(w)
    val f_preg = if (usingFPU) ffreelist.io.alloc_pregs(w) else 0.U
    uop.pdst := Mux(uop.dst_rtype === RT_FLT, f_preg,
                Mux(uop.ldst =/= 0.U, i_preg, 0.U))
  }

  //-------------------------------------------------------------
  // Rename Table

  var maptables = Seq(imaptable)
  if (usingFPU) maptables ++= Seq(fmaptable)
  for ((table, i) <- maptables.zipWithIndex) {
    table.io.brinfo := io.brinfo
    table.io.ren_uops := ren1_uops // expects pdst to be set up
    table.io.ren_remap_reqs := ren1_alloc_reqs(i)
    table.io.ren_br_tags := ren1_br_tags
    table.io.rbk_uops := io.com_uops
    table.io.rbk_valids := io.rbk_valids zip rob_ldst_vals(i) map {case (r,l) => r && l}
    table.io.rollback := io.rollback
  }

  for ((uop, w) <- ren1_uops.zipWithIndex) {
    val imap = imaptable.io.map_resps(w)
    val fmap = if (usingFPU) fmaptable.io.map_resps(w) else Wire(new MapResp(FPREG_SZ))
    if (!usingFPU) fmap := DontCare

    uop.pop1       := Mux(uop.lrs1_rtype === RT_FLT, fmap.prs1,
                      Mux(uop.lrs1_rtype === RT_FIX, imap.prs1, uop.lrs1)) // lrs1 can "pass through" to prs1
    uop.pop2       := Mux(uop.lrs2_rtype === RT_FLT, fmap.prs2, imap.prs2)
    uop.pop3       := fmap.prs3 // only FP has 3rd operand
    uop.stale_pdst := Mux(uop.dst_rtype  === RT_FLT, fmap.stale_pdst, imap.stale_pdst)
  }

  //-------------------------------------------------------------
  // pipeline registers

  // will ALL ren2 uops proceed to dispatch?
  val ren2_will_proceed =
    if (renameLatency == 2) (ren2_valids zip ren2_will_fire map {case (v,f) => (v === f)}).reduce(_&_)
    else io.dis_inst_can_proceed.reduce(_&_)

  val ren2_imap_resps = if (renameLatency == 2) RegEnable(imaptable.io.map_resps, ren2_will_proceed)
                        else imaptable.io.map_resps
  val ren2_fmap_resps = if (renameLatency == 2 && usingFPU) RegEnable(fmaptable.io.map_resps, ren2_will_proceed)
                        else if (usingFPU) fmaptable.io.map_resps
                        else new MapResp(1)

  for (w <- 0 until plWidth) {
    if (renameLatency == 1) {
      ren2_valids(w) := ren1_will_fire(w)
      ren2_uops(w)   := ren1_uops(w)
    } else {
      require (renameLatency == 2)
      val r_valid = RegInit(false.B)
      val r_uop   = Reg(new MicroOp())

      when (io.kill) {
        r_valid := false.B
      } .elsewhen (ren2_will_proceed) {
        r_valid := ren1_will_fire(w)
        r_uop := GetNewUopAndBrMask(ren1_uops(w), io.brinfo)
      } .otherwise {
        r_valid := r_valid && !ren2_will_fire(w) // clear bit if uop gets dispatched
        r_uop := GetNewUopAndBrMask(r_uop, io.brinfo)
      }

      ren2_valids(w) := r_valid
      ren2_uops(w)   := r_uop
    }
  }

  //-------------------------------------------------------------
  // Busy Table

  ibusytable.io.ren_uops := ren2_uops  // expects pdst to be set up.
  ibusytable.io.busy_reqs := ren2_imap_resps
  ibusytable.io.rebusy_reqs := ren2_int_req_alloc
  ibusytable.io.wb_valids := io.int_wakeups.map(_.valid)
  ibusytable.io.wb_pdsts := io.int_wakeups.map(_.bits.uop.pdst)

  assert (!(io.int_wakeups.map(x => x.valid && x.bits.uop.dst_rtype =/= RT_FIX).reduce(_|_)),
   "[rename] int wakeup is not waking up a Int register.")

  for (w <- 0 until plWidth) {
    assert (!(
      ren2_will_fire(w) &&
      ren2_uops(w).lrs1_rtype === RT_FIX &&
      ren2_uops(w).pop1 =/= ibusytable.io.busy_reqs(w).prs1),
      "[rename] ren2 maptable prs1 value don't match uop's values.")
    assert (!(
      ren2_will_fire(w) &&
      ren2_uops(w).lrs2_rtype === RT_FIX &&
      ren2_uops(w).pop2 =/= ibusytable.io.busy_reqs(w).prs2),
      "[rename] ren2 maptable prs2 value don't match uop's values.")
  }

  if (usingFPU) {
    fbusytable.io.ren_uops := ren2_uops  // expects pdst to be set up.
    fbusytable.io.busy_reqs := ren2_fmap_resps
    fbusytable.io.rebusy_reqs := ren2_fp_req_alloc
    fbusytable.io.wb_valids := io.fp_wakeups.map(_.valid)
    fbusytable.io.wb_pdsts := io.fp_wakeups.map(_.bits.uop.pdst)

    assert (!(io.fp_wakeups.map(x => x.valid && x.bits.uop.dst_rtype =/= RT_FLT).reduce(_|_)),
      "[rename] fp wakeup is not waking up a FP register.")
  }

  for ((uop, w) <- ren2_uops.zipWithIndex) {
    val ibusy = ibusytable.io.busy_resps(w)
    val fbusy = if (usingFPU) fbusytable.io.busy_resps(w) else Wire(new BusyResp)
    if (!usingFPU) fbusy := DontCare

    uop.prs1_busy := uop.lrs1_rtype === RT_FIX && ibusy.prs1_busy || uop.lrs1_rtype === RT_FLT && fbusy.prs1_busy
    uop.prs2_busy := uop.lrs2_rtype === RT_FIX && ibusy.prs2_busy || uop.lrs2_rtype === RT_FLT && fbusy.prs2_busy
    uop.prs3_busy := uop.frs3_en && fbusy.prs3_busy

    val valid = ren2_valids(w)
    assert (!(valid && ibusy.prs1_busy && uop.lrs1_rtype === RT_FIX && uop.lrs1 === 0.U), "[rename] x0 is busy??")
    assert (!(valid && ibusy.prs2_busy && uop.lrs2_rtype === RT_FIX && uop.lrs2 === 0.U), "[rename] x0 is busy??")
  }

  //-------------------------------------------------------------
  // Outputs

  io.ren1_mask := ren1_will_fire
  io.ren1_uops := ren1_uops

  io.ren2_mask := ren2_will_fire
  io.ren2_uops := ren2_uops map {u => GetNewUopAndBrMask(u, io.brinfo)}

  for (w <- 0 until plWidth) {
    val ifl_can_allocate = ifreelist.io.can_allocate(w)
    val ffl_can_allocate =
      if (usingFPU) {
        ffreelist.io.can_allocate(w)
      } else {
        false.B
      }
      // Push back against Decode stage if Rename1 can't proceed (and Rename2/Dispatch can't receive).
      io.inst_can_proceed(w) :=
        ren2_will_proceed &&
        (ren1_uops(w).dst_rtype =/= RT_FIX || ifl_can_allocate) &&
        (ren1_uops(w).dst_rtype =/= RT_FLT || ffl_can_allocate)
  }

  //-------------------------------------------------------------
  // Debug signals

  io.debug.ifreelist  := ifreelist.io.debug.freelist
  io.debug.iisprlist  := ifreelist.io.debug.isprlist
  io.debug.ibusytable := ibusytable.io.debug.busytable

  io.debug.ffreelist  := (if (usingFPU) ffreelist.io.debug.freelist else 0.U)
  io.debug.fisprlist  := (if (usingFPU) ffreelist.io.debug.isprlist else 0.U)
  io.debug.fbusytable := (if (usingFPU) fbusytable.io.debug.busytable else 0.U)
}
