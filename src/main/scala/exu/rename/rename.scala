//******************************************************************************
// Copyright (c) 2012 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
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
  val numPhysRegs: Int,
  val numWbPorts: Int)
  (implicit p: Parameters) extends BoomBundle
{
  val pregSz = log2Ceil(numPhysRegs)

  val ren_stalls = Output(Vec(plWidth, Bool()))

  val kill = Input(Bool())

  val dec_fire  = Input(Vec(plWidth, Bool())) // will commit state updates
  val dec_uops  = Input(Vec(plWidth, new MicroOp()))

  // physical specifiers available AND busy/ready status available.
  val ren2_mask = Vec(plWidth, Output(Bool())) // mask of valid instructions
  val ren2_uops = Vec(plWidth, Output(new MicroOp()))

  // branch resolution (execute)
  val brupdate = Input(new BrUpdateInfo())

  val dis_fire  = Input(Vec(coreWidth, Bool()))
  val dis_ready = Input(Bool())

  // wakeup ports
  val wakeups = Flipped(Vec(numWbPorts, Valid(new ExeUnitResp(xLen))))

  // commit stage
  val com_valids = Input(Vec(plWidth, Bool()))
  val com_uops = Input(Vec(plWidth, new MicroOp()))
  val rbk_valids = Input(Vec(plWidth, Bool()))
  val rollback = Input(Bool())

  val debug_rob_empty = Input(Bool())
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
  numWbPorts: Int,
  float: Boolean)
(implicit p: Parameters) extends BoomModule
{
  val pregSz = log2Ceil(numPhysRegs)
  val rtype = Mux(float.B, RT_FLT, RT_FIX)

  val io = IO(new RenameStageIO(plWidth, numPhysRegs, numWbPorts))

  //-------------------------------------------------------------
  // Helper Functions

  def BypassAllocations(uop: MicroOp, older_uops: Seq[MicroOp], valids: Seq[Bool]): MicroOp = {
    val bypassed_uop = Wire(new MicroOp)
    bypassed_uop := uop

    val bypass_hits_rs1 = (older_uops zip valids) map { case (u,v) => v && u.ldst === uop.lrs1 && u.dst_rtype === rtype }
    val bypass_hits_rs2 = (older_uops zip valids) map { case (u,v) => v && u.ldst === uop.lrs2 && u.dst_rtype === rtype }
    val bypass_hits_rs3 = (older_uops zip valids) map { case (u,v) => v && u.ldst === uop.lrs3 && u.dst_rtype === rtype }
    val bypass_hits_dst = (older_uops zip valids) map { case (u,v) => v && u.ldst === uop.ldst && u.dst_rtype === rtype }

    val bypass_sel_rs1 = PriorityEncoderOH(bypass_hits_rs1.reverse).reverse
    val bypass_sel_rs2 = PriorityEncoderOH(bypass_hits_rs2.reverse).reverse
    val bypass_sel_rs3 = PriorityEncoderOH(bypass_hits_rs3.reverse).reverse
    val bypass_sel_dst = PriorityEncoderOH(bypass_hits_dst.reverse).reverse

    val do_bypass_rs1 = bypass_hits_rs1.reduce(_||_)
    val do_bypass_rs2 = bypass_hits_rs2.reduce(_||_)
    val do_bypass_rs3 = bypass_hits_rs3.reduce(_||_)
    val do_bypass_dst = bypass_hits_dst.reduce(_||_)

    val bypass_pdsts = older_uops.map(_.pdst)

    when (do_bypass_rs1) { bypassed_uop.prs1       := Mux1H(bypass_sel_rs1, bypass_pdsts) }
    when (do_bypass_rs2) { bypassed_uop.prs2       := Mux1H(bypass_sel_rs2, bypass_pdsts) }
    when (do_bypass_rs3) { bypassed_uop.prs3       := Mux1H(bypass_sel_rs3, bypass_pdsts) }
    when (do_bypass_dst) { bypassed_uop.stale_pdst := Mux1H(bypass_sel_dst, bypass_pdsts) }

    bypassed_uop.prs1_busy := uop.prs1_busy || do_bypass_rs1
    bypassed_uop.prs2_busy := uop.prs2_busy || do_bypass_rs2
    bypassed_uop.prs3_busy := uop.prs3_busy || do_bypass_rs3

    if (!float) {
      bypassed_uop.prs3      := DontCare
      bypassed_uop.prs3_busy := false.B
    }

    bypassed_uop
  }

  //-------------------------------------------------------------
  // Rename Structures

  val maptable = Module(new RenameMapTable(
    plWidth,
    32,
    numPhysRegs,
    false,
    float))
  val freelist = Module(new RenameFreeList(
    plWidth,
    numPhysRegs,
    float))
  val busytable = Module(new RenameBusyTable(
    plWidth,
    numPhysRegs,
    numWbPorts,
    false,
    float))

  //-------------------------------------------------------------
  // Pipeline State & Wires

  // Stage 1
  val ren1_fire       = Wire(Vec(plWidth, Bool()))
  val ren1_uops       = Wire(Vec(plWidth, new MicroOp))

  // Stage 2
  val ren2_valids     = Wire(Vec(plWidth, Bool()))
  val ren2_uops       = Wire(Vec(plWidth, new MicroOp))
  val ren2_fire       = io.dis_fire
  val ren2_ready      = io.dis_ready
  val ren2_alloc_reqs = Wire(Vec(plWidth, Bool()))
  val ren2_br_tags    = Wire(Vec(plWidth, Valid(UInt(brTagSz.W))))

  // Commit/Rollback
  val com_valids      = Wire(Vec(plWidth, Bool()))
  val rbk_valids      = Wire(Vec(plWidth, Bool()))

  for (w <- 0 until plWidth) {
    ren1_fire(w)          := io.dec_fire(w)
    ren1_uops(w)          := io.dec_uops(w)

    ren2_alloc_reqs(w)    := ren2_uops(w).ldst_val && ren2_uops(w).dst_rtype === rtype && ren2_fire(w)
    ren2_br_tags(w).valid := ren2_fire(w) && ren2_uops(w).allocate_brtag

    com_valids(w)         := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === rtype && io.com_valids(w)
    rbk_valids(w)         := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === rtype && io.rbk_valids(w)
    ren2_br_tags(w).bits  := ren2_uops(w).br_tag
  }

  //-------------------------------------------------------------
  // Rename Table

  // Maptable inputs.
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
  ren2_alloc_reqs zip rbk_valids.reverse zip remap_reqs map {
    case ((a,r),rr) => rr.valid := a || r}

  // Hook up inputs.
  maptable.io.map_reqs    := map_reqs
  maptable.io.remap_reqs  := remap_reqs
  maptable.io.ren_br_tags := ren2_br_tags
  maptable.io.brupdate      := io.brupdate
  maptable.io.rollback    := io.rollback

  // Maptable outputs.
  for ((uop, w) <- ren1_uops.zipWithIndex) {
    val mappings = maptable.io.map_resps(w)

    uop.prs1       := mappings.prs1
    uop.prs2       := mappings.prs2
    uop.prs3       := mappings.prs3 // only FP has 3rd operand
    uop.stale_pdst := mappings.stale_pdst
  }

  //-------------------------------------------------------------
  // pipeline registers

  for (w <- 0 until plWidth) {
    val r_valid  = RegInit(false.B)
    val r_uop    = Reg(new MicroOp)

    when (io.kill) {
      r_valid := false.B
    } .elsewhen (ren2_ready) {
      r_valid := ren1_fire(w)
      r_uop   := GetNewUopAndBrMask(BypassAllocations(ren1_uops(w), ren2_uops, ren2_valids), io.brupdate)
    } .otherwise {
      r_valid := r_valid && !ren2_fire(w) // clear bit if uop gets dispatched
      r_uop   := io.ren2_uops(w)
    }

    ren2_valids(w) := r_valid
    ren2_uops(w)   := r_uop
  }

  //-------------------------------------------------------------
  // Free List

  // Freelist inputs.
  freelist.io.reqs := ren2_alloc_reqs
  freelist.io.dealloc_pregs zip com_valids zip rbk_valids map
    {case ((d,c),r) => d.valid := c || r}
  freelist.io.dealloc_pregs zip io.com_uops map
    {case (d,c) => d.bits := Mux(io.rollback, c.pdst, c.stale_pdst)}
  freelist.io.ren_br_tags := ren2_br_tags
  freelist.io.brupdate := io.brupdate
  freelist.io.pipeline_empty := io.debug_rob_empty

  assert (ren2_alloc_reqs zip freelist.io.alloc_pregs map {case (r,p) => !r || p.bits =/= 0.U} reduce (_&&_),
           "[rename-stage] A uop is trying to allocate the zero physical register.")

  // Freelist outputs.
  for ((uop, w) <- ren2_uops.zipWithIndex) {
    val preg = freelist.io.alloc_pregs(w).bits
    uop.pdst := Mux(uop.ldst =/= 0.U || float.B, preg, 0.U)
  }

  //-------------------------------------------------------------
  // Busy Table

  busytable.io.ren_uops := ren2_uops  // expects pdst to be set up.
  busytable.io.rebusy_reqs := ren2_alloc_reqs
  busytable.io.wb_valids := io.wakeups.map(_.valid)
  busytable.io.wb_pdsts := io.wakeups.map(_.bits.uop.pdst)

  for ((uop, w) <- ren2_uops.zipWithIndex) {
    val busy = busytable.io.busy_resps(w)

    uop.prs1_busy := uop.lrs1_rtype === rtype && busy.prs1_busy
    uop.prs2_busy := uop.lrs2_rtype === rtype && busy.prs2_busy
    uop.prs3_busy := uop.frs3_en && busy.prs3_busy

    val valid = ren2_valids(w)
    assert (!(valid && busy.prs1_busy && rtype === RT_FIX && uop.lrs1 === 0.U), "[rename] x0 is busy??")
    assert (!(valid && busy.prs2_busy && rtype === RT_FIX && uop.lrs2 === 0.U), "[rename] x0 is busy??")
  }

  //-------------------------------------------------------------
  // Outputs

  io.ren2_mask := ren2_valids

  for (w <- 0 until plWidth) {
    val can_allocate = freelist.io.alloc_pregs(w).valid

    // Push back against Decode stage if Rename1 can't proceed.
    io.ren_stalls(w) := (ren2_uops(w).dst_rtype === rtype) && !can_allocate

    val bypassed_uop = Wire(new MicroOp)
    if (w > 0) bypassed_uop := BypassAllocations(ren2_uops(w), ren2_uops.slice(0,w), ren2_valids.slice(0,w))
    else       bypassed_uop := ren2_uops(w)

    io.ren2_uops(w) := GetNewUopAndBrMask(bypassed_uop, io.brupdate)
  }
}
