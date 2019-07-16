//******************************************************************************
// Copyright (c) 2012 - 2019, The Regents of the University of California (Regents).
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
  val numIntWbPorts: Int,
  val numFpWbPorts: Int)
  (implicit p: Parameters) extends BoomBundle
{
  val inst_can_proceed = Output(Vec(plWidth, Bool()))

  val kill = Input(Bool())

  val dec_fire  = Input(Vec(plWidth, Bool())) // will commit state updates
  val dec_uops  = Input(Vec(plWidth, new MicroOp()))

  // physical specifiers available AND busy/ready status available.
  val ren2_mask = Vec(plWidth, Output(Bool())) // mask of valid instructions
  val ren2_uops = Vec(plWidth, Output(new MicroOp()))

  // branch resolution (execute)
  val brinfo = Input(new BrResolutionInfo())

  val dis_fire  = Input(Vec(coreWidth, Bool()))
  val dis_ready = Input(Bool())

  // wakeup ports
  val int_wakeups = Flipped(Vec(numIntWbPorts, Valid(new ExeUnitResp(xLen))))
  val fp_wakeups = Flipped(Vec(numFpWbPorts, Valid(new ExeUnitResp(fLen+1))))

  // commit stage
  val com_valids = Input(Vec(plWidth, Bool()))
  val com_uops = Input(Vec(plWidth, new MicroOp()))
  val rbk_valids = Input(Vec(plWidth, Bool()))
  val rollback = Input(Bool())

  val flush = Input(Bool())

  val debug_rob_empty = Input(Bool())
  val debug = Output(new DebugRenameStageIO)
}

/**
 * IO bundle to debug the rename stage
 */
class DebugRenameStageIO(implicit p: Parameters) extends BoomBundle
{
  val ifreelist  = Bits(numIntPhysRegs.W)
  val iisprlist  = Bits(numIntPhysRegs.W)
  val ibusytable = UInt(numIntPhysRegs.W)
  val ffreelist  = Bits(numFpPhysRegs.W)
  val fisprlist  = Bits(numFpPhysRegs.W)
  val fbusytable = UInt(numFpPhysRegs.W)
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
  val io = IO(new RenameStageIO(plWidth, numIntWbPorts, numFpWbPorts))

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

  // Stage 1
  val ren1_br_tags        = Wire(Vec(plWidth, Valid(UInt(brTagSz.W))))
  val ren1_fire           = Wire(Vec(plWidth, Bool()))
  val ren1_uops           = Wire(Vec(plWidth, new MicroOp))

  val ren1_int_alloc_reqs = Wire(Vec(plWidth, Bool()))
  val ren1_fp_alloc_reqs  = Wire(Vec(plWidth, Bool()))

  // Stage 2
  val ren2_int_alloc_reqs = Wire(Vec(plWidth, Bool()))
  val ren2_fp_alloc_reqs  = Wire(Vec(plWidth, Bool()))

  val ren2_valids         = Wire(Vec(plWidth, Bool()))
  val ren2_uops           = Wire(Vec(plWidth, new MicroOp))
  val ren2_fire           = io.dis_fire
  val ren2_ready          = io.dis_ready

  // Commit/Rollback
  val int_com_valids      = Wire(Vec(plWidth, Bool()))
  val fp_com_valids       = Wire(Vec(plWidth, Bool()))

  val int_rbk_valids      = Wire(Vec(plWidth, Bool()))
  val fp_rbk_valids       = Wire(Vec(plWidth, Bool()))

  val int_ren2_rbk_valids = Wire(Vec(plWidth, Bool()))
  val fp_ren2_rbk_valids  = Wire(Vec(plWidth, Bool()))

  // TODO: Simplify this by splitting off FP rename from INT rename.
  // Ideally, this will take the form of a decoupled FPU which lives behind a queue.
  for (w <- 0 until plWidth) {
    ren1_fire(w)           := io.dec_fire(w)
    ren1_uops(w)           := io.dec_uops(w)
    ren1_br_tags(w).valid  := ren1_fire(w) && io.dec_uops(w).allocate_brtag
    ren1_br_tags(w).bits   := io.dec_uops(w).br_tag

    ren1_int_alloc_reqs(w) := ren1_uops(w).ldst_val && ren1_uops(w).dst_rtype === RT_FIX && ren1_fire(w)
    ren1_fp_alloc_reqs(w)  := ren1_uops(w).ldst_val && ren1_uops(w).dst_rtype === RT_FLT && ren1_fire(w)

    ren2_int_alloc_reqs(w) := ren2_uops(w).ldst_val && ren2_uops(w).dst_rtype === RT_FIX && ren2_fire(w)
    ren2_fp_alloc_reqs(w)  := ren2_uops(w).ldst_val && ren2_uops(w).dst_rtype === RT_FLT && ren2_fire(w)

    int_com_valids(w)      := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === RT_FIX && io.com_valids(w)
    fp_com_valids(w)       := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === RT_FLT && io.com_valids(w)

    int_rbk_valids(w)      := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === RT_FIX && io.rbk_valids(w)
    fp_rbk_valids(w)       := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === RT_FLT && io.rbk_valids(w)

    int_ren2_rbk_valids(w) := ren2_uops(w).ldst_val && ren2_uops(w).dst_rtype === RT_FIX && io.flush && ren2_valids(w)
    fp_ren2_rbk_valids(w)  := ren2_uops(w).ldst_val && ren2_uops(w).dst_rtype === RT_FLT && io.flush && ren2_valids(w)
  }

  var ren1_alloc_reqs = Seq(ren1_int_alloc_reqs)
  if (usingFPU) ren1_alloc_reqs ++= Seq(ren1_fp_alloc_reqs)

  var com_valids = Seq(int_com_valids)
  if (usingFPU) com_valids ++= Seq(fp_com_valids)
  var rbk_valids = Seq(int_rbk_valids)
  if (usingFPU) rbk_valids ++= Seq(fp_rbk_valids)
  var ren2_rbk_valids = Seq(int_ren2_rbk_valids)
  if (usingFPU) ren2_rbk_valids ++= Seq(fp_ren2_rbk_valids)

  //-------------------------------------------------------------
  // Rename Table

  var maptables = Seq(imaptable)
  if (usingFPU) maptables ++= Seq(fmaptable)

  // Maptable inputs.
  for ((table, i) <- maptables.zipWithIndex) {
    val pregSz = log2Ceil(if (i == 0) numIntPhysRegs else numFpPhysRegs)
    val map_reqs   = Wire(Vec(plWidth, new MapReq(lregSz)))
    val remap_reqs = Wire(Vec(plWidth, new RemapReq(lregSz, pregSz)))

    // Generate maptable requests.
    for ((((ren1,ren2),com),w) <- ren1_uops zip ren2_uops zip io.com_uops.reverse zipWithIndex) {
      map_reqs(w).lrs1 := ren1.lrs1
      map_reqs(w).lrs2 := ren1.lrs2
      map_reqs(w).lrs3 := ren1.lrs3
      map_reqs(w).ldst := ren1.ldst

      remap_reqs(w).ldst := Mux(io.rollback, com.ldst      , ren2.ldst)
      remap_reqs(w).pdst := Mux(io.rollback, com.stale_pdst, ren2.pdst)
    }
    ren2_alloc_reqs(i) zip rbk_valids(i).reverse zip remap_reqs map {
      case ((r2a,rbk),rr) => rr.valid := r2a || rbk}

    // Hook up inputs.
    table.io.map_reqs    := map_reqs
    table.io.remap_reqs  := remap_reqs
    table.io.ren_br_tags := ren1_br_tags
    table.io.brinfo      := io.brinfo
    table.io.rollback    := io.flush || io.rollback
  }

  // Maptable outputs.
  for ((uop, w) <- ren1_uops.zipWithIndex) {
    val imap = imaptable.io.map_resps(w)
    val fmap = if (usingFPU) fmaptable.io.map_resps(w) else Wire(new MapResp(fpregSz))
    if (!usingFPU) fmap := DontCare

    uop.prs1       := Mux(uop.lrs1_rtype === RT_FLT, fmap.prs1,
                      Mux(uop.lrs1_rtype === RT_FIX, imap.prs1, uop.lrs1)) // lrs1 can "pass through" to prs1
    uop.prs2       := Mux(uop.lrs2_rtype === RT_FLT, fmap.prs2, imap.prs2)
    uop.prs3       := fmap.prs3 // only FP has 3rd operand
    uop.stale_pdst := Mux(uop.dst_rtype  === RT_FLT, fmap.stale_pdst, imap.stale_pdst)
  }

  //-------------------------------------------------------------
  // pipeline registers

  val ren2_imap_resps = RegEnable(imaptable.io.map_resps, ren2_ready)
  val ren2_fmap_resps = if (usingFPU) RegEnable(fmaptable.io.map_resps, ren2_ready)
                        else new MapResp(1)

  for (w <- 0 until plWidth) {
    require (renameLatency == 2)
    val r_valid = RegInit(false.B)
    val r_uop   = Reg(new MicroOp())

    when (io.kill) {
      r_valid := false.B
    } .elsewhen (ren2_ready) {
      r_valid := ren1_fire(w)
      r_uop := GetNewUopAndBrMask(ren1_uops(w), io.brinfo)
    } .otherwise {
      r_valid := r_valid && !ren2_fire(w) // clear bit if uop gets dispatched
      r_uop := GetNewUopAndBrMask(r_uop, io.brinfo)
    }

    ren2_valids(w) := r_valid
    ren2_uops(w)   := r_uop
  }

  //-------------------------------------------------------------
  // Free List

  var freelists = Seq(ifreelist)
  if (usingFPU) freelists ++= Seq(ffreelist)

  // Freelist inputs.
  for ((list, i) <- freelists.zipWithIndex) {
    list.io.reqs := ren2_alloc_reqs(i)
    list.io.dealloc_pregs zip com_valids(i) zip rbk_valids(i) map
      {case (((d,c),r)) => d.valid := c || r}
    list.io.dealloc_pregs zip io.com_uops map
      {case (d,c) => d.bits := Mux(io.rollback, c.pdst, c.stale_pdst)}
    list.io.ren_br_tags := ren1_br_tags
    list.io.brinfo := io.brinfo
    list.io.debug.pipeline_empty := io.debug_rob_empty

    assert (ren2_alloc_reqs(i) zip list.io.alloc_pregs map {case (r,p) => !r || p.bits =/= 0.U} reduce (_&&_),
             "[rename-stage] A uop is trying to allocate the zero physical register.")
  }

  // Freelist outputs.
  for ((uop, w) <- ren2_uops.zipWithIndex) {
    val i_preg = ifreelist.io.alloc_pregs(w).bits
    val f_preg = if (usingFPU) ffreelist.io.alloc_pregs(w).bits else 0.U
    uop.pdst := Mux(uop.dst_rtype === RT_FLT, f_preg,
                Mux(uop.ldst =/= 0.U, i_preg, 0.U))
  }

  //-------------------------------------------------------------
  // Busy Table

  ibusytable.io.ren_uops := ren2_uops  // expects pdst to be set up.
  ibusytable.io.busy_reqs := ren2_imap_resps
  ibusytable.io.rebusy_reqs := ren2_int_alloc_reqs
  ibusytable.io.wb_valids := io.int_wakeups.map(_.valid)
  ibusytable.io.wb_pdsts := io.int_wakeups.map(_.bits.uop.pdst)

  assert (!(io.int_wakeups.map(x => x.valid && x.bits.uop.dst_rtype =/= RT_FIX).reduce(_|_)),
   "[rename] int wakeup is not waking up a Int register.")

  for (w <- 0 until plWidth) {
    assert (!(
      ren2_fire(w) &&
      ren2_uops(w).lrs1_rtype === RT_FIX &&
      ren2_uops(w).prs1 =/= ibusytable.io.busy_reqs(w).prs1),
      "[rename] ren2 maptable prs1 value don't match uop's values.")
    assert (!(
      ren2_fire(w) &&
      ren2_uops(w).lrs2_rtype === RT_FIX &&
      ren2_uops(w).prs2 =/= ibusytable.io.busy_reqs(w).prs2),
      "[rename] ren2 maptable prs2 value don't match uop's values.")
  }

  if (usingFPU) {
    fbusytable.io.ren_uops := ren2_uops  // expects pdst to be set up.
    fbusytable.io.busy_reqs := ren2_fmap_resps
    fbusytable.io.rebusy_reqs := ren2_fp_alloc_reqs
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

  io.ren2_mask := ren2_valids
  io.ren2_uops := ren2_uops map {u => GetNewUopAndBrMask(u, io.brinfo)}

  for (w <- 0 until plWidth) {
    val ifl_can_allocate = ifreelist.io.alloc_pregs(w).valid
    val ffl_can_allocate =
      if (usingFPU) {
        ffreelist.io.alloc_pregs(w).valid
      } else {
        false.B
      }
      // Push back against Decode stage if Rename1 can't proceed.
      io.inst_can_proceed(w) :=
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
