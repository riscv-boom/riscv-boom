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
 * @param numWbPorts number of int writeback ports
 * @param numWbPorts number of FP writeback ports
 */
class RenameStageIO(
  val plWidth: Int,
  val numWbPorts: Int,
  val numWbPorts: Int)
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
  val wakeups = Flipped(Vec(numWbPorts, Valid(new ExeUnitResp(xLen))))

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
  val freelist  = Bits(numPhysRegs.W)
  val isprlist  = Bits(numPhysRegs.W)
  val busytable = UInt(numPhysRegs.W)
}

/**
 * Rename stage that connets the map table, free list, and busy table.
 * Can be used in both the FP pipeline and the normal execute pipeline.
 *
 * @param plWidth pipeline width
 * @param numWbPorts number of int writeback ports
 * @param numWbPorts number of FP writeback ports
 */
class RenameStage(
  plWidth: Int,
  numPhysRegs: Int,
  numWbPorts: Int)
(implicit p: Parameters) extends BoomModule
{
  val pregSz = log2Ceil(numPhysRegs)

  val io = IO(new RenameStageIO(plWidth, numWbPorts, numWbPorts))

  //-------------------------------------------------------------
  // Rename Structures

  val maptable = Module(new RenameMapTable(
    plWidth,
    32,
    numPhysRegs,
    false))
  val freelist = Module(new RenameFreeList(
    plWidth,
    numPhysRegs,
    false))
  val busytable = Module(new RenameBusyTable(
    plWidth,
    numPhysRegs,
    numWbPorts,
    false))

  //-------------------------------------------------------------
  // Pipeline State & Wires

  // Stage 1
  val ren1_br_tags    = Wire(Vec(plWidth, Valid(UInt(brTagSz.W))))
  val ren1_fire       = Wire(Vec(plWidth, Bool()))
  val ren1_uops       = Wire(Vec(plWidth, new MicroOp))
  val ren1_alloc_reqs = Wire(Vec(plWidth, Bool()))

  // Stage 2
  val ren2_valids     = Wire(Vec(plWidth, Bool()))
  val ren2_uops       = Wire(Vec(plWidth, new MicroOp))
  val ren2_fire       = io.dis_fire
  val ren2_ready      = io.dis_ready
  val ren2_alloc_reqs = Wire(Vec(plWidth, Bool()))

  // Commit/Rollback
  val com_valids      = Wire(Vec(plWidth, Bool()))
  val rbk_valids      = Wire(Vec(plWidth, Bool()))
  val ren2_rbk_valids = Wire(Vec(plWidth, Bool()))

  for (w <- 0 until plWidth) {
    ren1_fire(w)          := io.dec_fire(w)
    ren1_uops(w)          := io.dec_uops(w)
    ren1_br_tags(w).valid := ren1_fire(w) && io.dec_uops(w).allocate_brtag
    ren1_br_tags(w).bits  := io.dec_uops(w).br_tag

    ren1_alloc_reqs(w)    := ren1_uops(w).ldst_val && ren1_uops(w).dst_rtype === RT_FIX && ren1_fire(w)
    ren2_alloc_reqs(w)    := ren2_uops(w).ldst_val && ren2_uops(w).dst_rtype === RT_FIX && ren2_fire(w)

    int_com_valids(w)     := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === RT_FIX && io.com_valids(w)
    int_rbk_valids(w)     := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === RT_FIX && io.rbk_valids(w)
    ren2_rbk_valids(w)    := ren2_uops(w).ldst_val && ren2_uops(w).dst_rtype === RT_FIX && io.flush && ren2_valids(w)
  }

  //-------------------------------------------------------------
  // Free List

  // Freelist inputs.
  freelist.io.reqs := ren1_alloc_reqs
  freelist.io.dealloc_pregs zip com_valids zip rbk_valids zip ren2_rbk_valids map
    {case (((d,c),r1),r2) => d.valid := c || r1 || r2}
  freelist.io.dealloc_pregs zip io.com_uops zip ren2_uops map
    {case ((d,c),r) => d.bits := Mux(io.rollback, c.pdst, Mux(io.flush, r.pdst, c.stale_pdst))}
  freelist.io.ren_br_tags := ren1_br_tags
  freelist.io.brinfo := io.brinfo
  freelist.io.debug.pipeline_empty := io.debug_rob_empty && !ren2_valids.reduce(_||_)

  assert (ren1_alloc_reqs zip list.io.alloc_pregs map {case (r,p) => !r || p.bits =/= 0.U} reduce (_&&_),
           "[rename-stage] A uop is trying to allocate the zero physical register.")

  // Freelist outputs.
  for ((uop, w) <- ren1_uops.zipWithIndex) {
    val preg = freelist.io.alloc_pregs(w).bits
    uop.pdst := Mux(uop.ldst =/= 0.U, preg, 0.U)
  }

  //-------------------------------------------------------------
  // Rename Table

  // Maptable inputs.
  val map_reqs   = Wire(Vec(plWidth, new MapReq(lregSz)))
  val remap_reqs = Wire(Vec(plWidth, new RemapReq(lregSz, pregSz)))

  // Generate maptable requests.
  for ((((ren1,ren2),com),w) <- ren1_uops zip ren2_uops.reverse zip io.com_uops.reverse zipWithIndex) {
    map_reqs(w).lrs1 := ren1.lrs1
    map_reqs(w).lrs2 := ren1.lrs2
    map_reqs(w).lrs3 := ren1.lrs3
    map_reqs(w).ldst := ren1.ldst

    remap_reqs(w).ldst := Mux(io.rollback, com.ldst,       Mux(io.flush, ren2.ldst,       ren1.ldst))
    remap_reqs(w).pdst := Mux(io.rollback, com.stale_pdst, Mux(io.flush, ren2.stale_pdst, ren1.pdst))
  }
  ren1_alloc_reqs zip rbk_valids.reverse zip ren2_rbk_valids.reverse zip remap_reqs map {
    case (((a,r1),r2),rr) => rr.valid := a || r1 || r2}

  // Hook up inputs.
  maptable.io.map_reqs    := map_reqs
  maptable.io.remap_reqs  := remap_reqs
  maptable.io.ren_br_tags := ren1_br_tags
  maptable.io.brinfo      := io.brinfo
  maptable.io.rollback    := io.flush || io.rollback

  // Maptable outputs.
  for ((uop, w) <- ren1_uops.zipWithIndex) {
    val mappings = imaptable.io.map_resps(w)

    uop.prs1       := Mux(uop.lrs1_rtype === RT_FIX, mappigns.prs1, uop.lrs1)) // lrs1 can "pass through" to prs1
    uop.prs2       := mappings.prs2
    uop.prs3       := mappings.prs3 // only FP has 3rd operand
    uop.stale_pdst := mappings.stale_pdst
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
  // Busy Table

  busytable.io.ren_uops := ren2_uops  // expects pdst to be set up.
  busytable.io.busy_reqs := ren2_imap_resps
  busytable.io.rebusy_reqs := ren2_alloc_reqs
  busytable.io.wb_valids := io.wakeups.map(_.valid)
  busytable.io.wb_pdsts := io.wakeups.map(_.bits.uop.pdst)

  assert (!(io.wakeups.map(x => x.valid && x.bits.uop.dst_rtype =/= RT_FIX).reduce(_|_)),
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

  for ((uop, w) <- ren2_uops.zipWithIndex) {
    val busy = ibusytable.io.busy_resps(w)

    uop.prs1_busy := uop.lrs1_rtype === RT_FIX && busy.prs1_busy
    uop.prs2_busy := uop.lrs2_rtype === RT_FIX && busy.prs2_busy
    uop.prs3_busy := uop.frs3_en && busy.prs3_busy

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
