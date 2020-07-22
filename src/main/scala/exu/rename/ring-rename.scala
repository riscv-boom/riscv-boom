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
  val dec_uops  = Input(Vec(coreWidth, new MicroOp))

  // physical specifiers available AND busy/ready status available.
  val ren2_mask = Output(Vec(coreWidth, Bool())) // mask of valid instructions
  val ren2_uops = Output(Vec(coreWidth, new MicroOp))

  // branch resolution (execute)
  val brupdate  = Input(new BrUpdateInfo)
  val kill      = Input(Bool())
  val flashback = Input(Bool())

  val dis_fire  = Input(Vec(coreWidth, Bool()))
  val dis_ready = Input(Bool())

  // wakeup ports
  val wakeups = Flipped(Vec(coreWidth*3, Valid(UInt(ipregSz.W))))

  // commit stage
  val com_valids = Input(Vec(coreWidth, Bool()))
  val com_uops   = Input(Vec(coreWidth, new MicroOp))

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

  def BypassAllocations(uop: MicroOp, older_uops: Seq[MicroOp], valids: Seq[Bool]): MicroOp = {
    val bypassed_uop = Wire(new MicroOp)
    bypassed_uop := uop

    val bypass_hits_rs1 = (older_uops zip valids) map { case (u,v) => v && u.ldst === uop.lrs1 && u.dst_rtype === rtype && u.ldst_val }
    val bypass_hits_rs2 = (older_uops zip valids) map { case (u,v) => v && u.ldst === uop.lrs2 && u.dst_rtype === rtype && u.ldst_val }
    val bypass_hits_dst = (older_uops zip valids) map { case (u,v) => v && u.ldst === uop.ldst && u.dst_rtype === rtype && u.ldst_val }

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

    when (do_bypass_rs1) { bypassed_uop.prs1_load := Mux1H(bypass_sel_rs1, older_uops.map(_.uses_ldq)) }
    when (do_bypass_rs2) { bypassed_uop.prs2_load := Mux1H(bypass_sel_rs2, older_uops.map(_.uses_ldq)) }

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
  val busytable = Module(new RingBusyTable(
    coreWidth,
    numIntPhysRegs,
    coreWidth*3))

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

  // Commit
  val com_valids      = Wire(Vec(coreWidth, Bool()))

  for (w <- 0 until coreWidth) {
    ren1_fire(w)          := io.dec_fire(w)
    ren1_uops(w)          := io.dec_uops(w)

    ren2_alloc_reqs(w)    := ren2_uops(w).ldst_val && ren2_uops(w).dst_rtype === rtype && ren2_fire(w)
    ren2_br_tags(w).valid := ren2_fire(w) && ren2_uops(w).allocate_brtag

    com_valids(w)         := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === rtype && io.com_valids(w)
    ren2_br_tags(w).bits  := ren2_uops(w).br_tag
  }

  //----------------------------------------------------------------------------------------------------
  // Map Table

  // Hook up inputs.
  for (w <- 0 until coreWidth) {
    maptable.io.map_reqs(w).lrs1 := ren1_uops(w).lrs1
    maptable.io.map_reqs(w).lrs2 := ren1_uops(w).lrs2
    maptable.io.map_reqs(w).lrs3 := ren1_uops(w).lrs3
    maptable.io.map_reqs(w).ldst := ren1_uops(w).ldst

    maptable.io.remap_reqs(w).bits.ldst := ren2_uops(w).ldst
    maptable.io.remap_reqs(w).bits.pdst := ren2_uops(w).pdst
    maptable.io.remap_reqs(w).valid     := ren2_alloc_reqs(w)

    maptable.io.commit_reqs(w).bits.ldst := io.com_uops(w).ldst
    maptable.io.commit_reqs(w).bits.pdst := io.com_uops(w).pdst
    maptable.io.commit_reqs(w).valid     := com_valids(w)
  }

  maptable.io.ren_br_tags := ren2_br_tags
  maptable.io.brupdate    := io.brupdate
  maptable.io.flashback   := io.flashback

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
      r_uop   := GetNewUopAndBrMask(BypassAllocations(ren1_uops(w), ren2_uops, ren2_valids), io.brupdate)
    } .otherwise {
      r_valid := r_valid && !ren2_fire(w) // clear bit if uop gets dispatched
      r_uop   := io.ren2_uops(w)
    }

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

    uop.prs1_load := busy.prs1_load
    uop.prs2_load := busy.prs2_load

    val valid = ren2_valids(w)
    assert (!(valid && busy.prs1_busy && rtype === RT_FIX && uop.lrs1 === 0.U), "[rename] x0 is busy??")
    assert (!(valid && busy.prs2_busy && rtype === RT_FIX && uop.lrs2 === 0.U), "[rename] x0 is busy??")
  }

  //----------------------------------------------------------------------------------------------------
  // Column Arbitration

  val column_arbiter = Module(new ColumnArbiter)

  val col_gnts = column_arbiter.io.gnts

  for (w <- 0 until coreWidth) {
    column_arbiter.io.uops(w).bits  := ren2_uops(w)
    column_arbiter.io.uops(w).valid := ren2_valids(w)

    ren2_uops(w).column := col_gnts(w)
  }

  //----------------------------------------------------------------------------------------------------
  // Free Lists

  for (c <- 0 until coreWidth) {
    for (w <- 0 until coreWidth) {
      freelists(c).io.reqs(w) := col_gnts(w)(c) && ren2_alloc_reqs(w)
      freelists(c).io.stale_pdsts(w).valid := io.com_uops(w).stale_col(c) && com_valids(w)
      freelists(c).io.stale_pdsts(w).bits  := io.com_uops(w).stale_pdst
      freelists(c).io.com_pdsts  (w).valid := io.com_uops(w).pdst_col (c) && com_valids(w)
      freelists(c).io.com_pdsts  (w).bits  := io.com_uops(w).pdst
    }
    freelists(c).io.ren_br_tags := ren2_br_tags
    freelists(c).io.brupdate    := io.brupdate
    freelists(c).io.flashback   := io.flashback
  }

  // Freelist outputs.
  for ((uop, w) <- ren2_uops.zipWithIndex) {
    val preg = Mux1H(col_gnts(w), freelists.map(_.io.alloc_pregs(w).bits))
    uop.pdst := Cat(OHToUInt(col_gnts(w)), Mux(uop.ldst_val, preg, 0.U))
  }

  assert (ren2_alloc_reqs zip ren2_uops map {case (r,u) => !r || u.pdst_spec =/= 0.U} reduce (_&&_),
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
    if (w > 0) bypassed_uop := BypassAllocations(ren2_uops(w), ren2_uops.slice(0,w), ren2_valids.slice(0,w))
    else       bypassed_uop := ren2_uops(w)

    io.ren2_uops(w) := GetNewUopAndBrMask(bypassed_uop, io.brupdate)

    // Need to know which prs was used to decide on a column. A bit of a hack.
    io.ren2_uops(w).busy_operand_sel := !bypassed_uop.prs1_busy
  }

  //----------------------------------------------------------------------------------------------------
  // Performance Counters

  val zero_waiting_count = RegInit(0.U(32.W))
  val one_waiting_count  = RegInit(0.U(32.W))
  val two_waiting_count  = RegInit(0.U(32.W))

  zero_waiting_count := zero_waiting_count + PopCount(io.ren2_uops zip ren2_fire map {case (u,f) => u.zero_waiting && f})
  one_waiting_count  := one_waiting_count  + PopCount(io.ren2_uops zip ren2_fire map {case (u,f) => u.one_waiting  && f})
  two_waiting_count  := two_waiting_count  + PopCount(io.ren2_uops zip ren2_fire map {case (u,f) => u.two_waiting  && f})

  dontTouch(zero_waiting_count)
  dontTouch(one_waiting_count)
  dontTouch(two_waiting_count)
}
