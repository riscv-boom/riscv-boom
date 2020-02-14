//******************************************************************************
// Copyright (c) 2012 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Column-Steering Renamer for the Ring Microarchitecture
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util._

/**
 * IO bundle to interface with the Register Rename logic
 */
class RingRenameIO(implicit p: Parameters) extends BoomBundle
{
  val ren_stalls = Output(Vec(coreWidth, Bool()))

  val dec_fire  = Input(Vec(coreWidth, Bool())) // will commit state updates
  val dec_uops  = Input(Vec(coreWidth, new MicroOp()))

  // physical specifiers available AND busy/ready status available.
  val ren2_mask = Output(Vec(coreWidth, Bool())) // mask of valid instructions
  val ren2_uops = Output(Vec(coreWidth, new MicroOp))

  // branch resolution (execute)
  val brinfo = Input(new BrResolutionInfo())
  val kill = Input(Bool())

  val dis_fire  = Input(Vec(coreWidth, Bool()))
  val dis_ready = Input(Bool())

  // wakeup ports
  val wakeups = Flipped(Vec(coreWidth*2, Valid(UInt(ipregSz.W))))

  // commit stage
  val com_valids = Input(Vec(coreWidth, Bool()))
  val com_uops = Input(Vec(coreWidth, new MicroOp()))
  val rbk_valids = Input(Vec(coreWidth, Bool()))
  val rollback = Input(Bool())

  val debug_rob_empty = Input(Bool())
}

/**
 * Rename stage that connets the map table, free list, and busy table.
 */
class RingRename(implicit p: Parameters) extends BoomModule
{
  val rtype = RT_FIX

  val io = IO(new RingRenameIO)

  //----------------------------------------------------------------------------------------------------
  // Helper Functions

  def BypassAllocations(uop: MicroOp, older_uops: Seq[MicroOp], alloc_reqs: Seq[Bool]): MicroOp = {
    val bypassed_uop = Wire(new MicroOp)
    bypassed_uop := uop

    val bypass_hits_rs1 = (older_uops zip alloc_reqs) map { case (r,a) => a && r.ldst === uop.lrs1 }
    val bypass_hits_rs2 = (older_uops zip alloc_reqs) map { case (r,a) => a && r.ldst === uop.lrs2 }
    val bypass_hits_dst = (older_uops zip alloc_reqs) map { case (r,a) => a && r.ldst === uop.ldst }

    val bypass_sel_rs1 = PriorityEncoderOH(bypass_hits_rs1.reverse).reverse
    val bypass_sel_rs2 = PriorityEncoderOH(bypass_hits_rs2.reverse).reverse
    val bypass_sel_dst = PriorityEncoderOH(bypass_hits_dst.reverse).reverse

    val do_bypass_rs1 = bypass_hits_rs1.reduce(_||_)
    val do_bypass_rs2 = bypass_hits_rs2.reduce(_||_)
    val do_bypass_dst = bypass_hits_dst.reduce(_||_)

    val bypass_pdsts = older_uops.map(_.pdst)

    when (do_bypass_rs1) { bypassed_uop.prs1       := Mux1H(bypass_sel_rs1, bypass_pdsts) }
    when (do_bypass_rs2) { bypassed_uop.prs2       := Mux1H(bypass_sel_rs2, bypass_pdsts) }
    when (do_bypass_dst) { bypassed_uop.stale_pdst := Mux1H(bypass_sel_dst, bypass_pdsts) }

    bypassed_uop.prs1_busy := uop.prs1_busy || do_bypass_rs1
    bypassed_uop.prs2_busy := uop.prs2_busy || do_bypass_rs2

    bypassed_uop
  }

  //----------------------------------------------------------------------------------------------------
  // Rename Structures

  val maptable = Module(new RenameMapTable(
    coreWidth,
    32,
    numIntPhysRegs,
    false,
    false))
  val freelists = Seq.fill(coreWidth) {
    Module(new RingFreeList(
      coreWidth,
      numIntPhysRegs / coreWidth))
  }
  val busytable = Module(new RenameBusyTable(
    coreWidth,
    numIntPhysRegs,
    coreWidth*2,
    false,
    false))

  //----------------------------------------------------------------------------------------------------
  // Pipeline State & Wires

  // Stage 1
  val ren1_fire       = Wire(Vec(coreWidth, Bool()))
  val ren1_uops       = Wire(Vec(coreWidth, new MicroOp))

  // Stage 2
  val ren2_valids     = Wire(Vec(coreWidth, Bool()))
  val ren2_uops       = Wire(Vec(coreWidth, new MicroOp))
  val ren2_fire       = io.dis_fire
  val ren2_ready      = io.dis_ready
  val ren2_alloc_reqs = Wire(Vec(coreWidth, Bool()))
  val ren2_br_tags    = Wire(Vec(coreWidth, Valid(UInt(brTagSz.W))))

  // Commit/Rollback
  val com_valids      = Wire(Vec(coreWidth, Bool()))
  val rbk_valids      = Wire(Vec(coreWidth, Bool()))

  for (w <- 0 until coreWidth) {
    ren1_fire(w)          := io.dec_fire(w)
    ren1_uops(w)          := io.dec_uops(w)

    ren2_alloc_reqs(w)    := ren2_uops(w).ldst_val && ren2_uops(w).dst_rtype === rtype && ren2_fire(w)
    ren2_br_tags(w).valid := ren2_fire(w) && ren2_uops(w).allocate_brtag

    com_valids(w)         := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === rtype && io.com_valids(w)
    rbk_valids(w)         := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === rtype && io.rbk_valids(w)
    ren2_br_tags(w).bits  := ren2_uops(w).br_tag
  }

  //----------------------------------------------------------------------------------------------------
  // Map Table

  // Maptable inputs.
  val map_reqs   = Wire(Vec(coreWidth, new MapReq(lregSz)))
  val remap_reqs = Wire(Vec(coreWidth, new RemapReq(lregSz, ipregSz)))

  // Generate maptable requests.
  for ((((ren1,ren2),com),w) <- ren1_uops zip ren2_uops zip io.com_uops.reverse zipWithIndex) {
    map_reqs(w).lrs1 := ren1.lrs1
    map_reqs(w).lrs2 := ren1.lrs2
    map_reqs(w).lrs3 := DontCare
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
  maptable.io.brinfo      := io.brinfo
  maptable.io.rollback    := io.rollback

  // Maptable outputs.
  for ((uop, w) <- ren1_uops.zipWithIndex) {
    val mappings = maptable.io.map_resps(w)

    uop.prs1       := mappings.prs1
    uop.prs2       := mappings.prs2
    uop.stale_pdst := mappings.stale_pdst
  }

  //----------------------------------------------------------------------------------------------------
  // Pipeline Registers

  for (w <- 0 until coreWidth) {
    val r_valid  = RegInit(false.B)
    val r_uop    = Reg(new MicroOp)
    val next_uop = Wire(new MicroOp)

    next_uop := r_uop

    when (io.kill) {
      r_valid := false.B
    } .elsewhen (ren2_ready) {
      r_valid := ren1_fire(w)
      next_uop := ren1_uops(w)
    } .otherwise {
      r_valid := r_valid && !ren2_fire(w) // clear bit if uop gets dispatched
      next_uop := r_uop
    }

    r_uop := GetNewUopAndBrMask(BypassAllocations(next_uop, ren2_uops, ren2_alloc_reqs), io.brinfo)

    ren2_valids(w) := r_valid
    ren2_uops(w)   := r_uop
  }

  //----------------------------------------------------------------------------------------------------
  // Busy Table

  busytable.io.ren_uops := ren2_uops  // expects pdst to be set up.
  busytable.io.rebusy_reqs := ren2_alloc_reqs
  busytable.io.wb_valids := io.wakeups.map(_.valid)
  busytable.io.wb_pdsts := io.wakeups.map(_.bits)

  for ((uop, w) <- ren2_uops.zipWithIndex) {
    val busy = busytable.io.busy_resps(w)

    uop.prs1_busy := uop.lrs1_rtype === rtype && busy.prs1_busy
    uop.prs2_busy := uop.lrs2_rtype === rtype && busy.prs2_busy

    val valid = ren2_valids(w)
    assert (!(valid && busy.prs1_busy && rtype === RT_FIX && uop.lrs1 === 0.U), "[rename] x0 is busy??")
    assert (!(valid && busy.prs2_busy && rtype === RT_FIX && uop.lrs2 === 0.U), "[rename] x0 is busy??")
  }

  //----------------------------------------------------------------------------------------------------
  // Column Arbitration

  val column_arbiter = Module(new ColumnArbiter)

  for (w <- 0 until coreWidth) {
    column_arbiter.io.uops(w).bits  := ren2_uops(w)
    column_arbiter.io.uops(w).valid := ren2_valids(w)
  }

  val col_gnts = column_arbiter.io.gnts

  //----------------------------------------------------------------------------------------------------
  // Free Lists

  for (c <- 0 until coreWidth) {
    for (w <- 0 until coreWidth) {
      freelists(c).io.reqs(w)                := col_gnts(w)(c) && ren2_alloc_reqs(w)
      freelists(c).io.dealloc_pregs(w).valid := io.com_uops(w).stale_col(c) && com_valids(w) || io.com_uops(w).pdst_col(c) && rbk_valids(w)
      freelists(c).io.dealloc_pregs(w).bits  := Mux(io.rollback, io.com_uops(w).pdst, io.com_uops(w).stale_pdst)
    }
    freelists(c).io.ren_br_tags    := ren2_br_tags
    freelists(c).io.brinfo         := io.brinfo
  }

  // Freelist outputs.
  for ((uop, w) <- ren2_uops.zipWithIndex) {
    val preg = Mux1H(col_gnts(w), freelists.map(_.io.alloc_pregs(w).bits))
    uop.pdst := Mux(uop.ldst =/= 0.U, Cat(OHToUInt(col_gnts(w)), preg), 0.U)
  }

  assert (ren2_alloc_reqs zip ren2_uops map {case (r,u) => !r || u.pdst =/= 0.U} reduce (_&&_),
           "[rename-stage] A uop is trying to allocate the zero physical register.")

  assert (!io.debug_rob_empty ||
          freelists.foldLeft(0.U) ((c,f) => c +& PopCount(f.io.debug_freelist)) >= (numIntPhysRegs - 31 - coreWidth).U,
          "[freelist] Leaking physical registers.")

  //----------------------------------------------------------------------------------------------------
  // Outputs

  io.ren2_mask := ren2_valids

  for (w <- 0 until coreWidth) {
    val can_allocate = (col_gnts(w) & VecInit(freelists.map(_.io.alloc_pregs(w).valid)).asUInt).orR

    // Push back against Decode stage if Rename1 can't proceed.
    io.ren_stalls(w) := (ren2_uops(w).dst_rtype === rtype) && !can_allocate

    val bypassed_uop = Wire(new MicroOp)
    if (w > 0) bypassed_uop := BypassAllocations(ren2_uops(w), ren2_uops.slice(0,w), ren2_alloc_reqs.slice(0,w))
    else       bypassed_uop := ren2_uops(w)

    io.ren2_uops(w) := GetNewUopAndBrMask(bypassed_uop, io.brinfo)

    // Need to know which prs was used to decide on a column. A bit of a hack.
    io.ren2_uops(w).busy_operand_sel := bypassed_uop.prs2_busy
  }
}
