//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Rename BusyTable
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util._

/**
 * IO bundle to interact with the busy table.
 * Internally bypasses newly busy registers (.write) to the read ports (.read)
 *
 * @param plWidth pipeline width
 * @param numPregs number of physical registers
 * @param numReadPorts number of read ports to the regfile
 * @param numWbPorts number of writeback ports to the regfile
 */
class BusyTableIo(
  val plWidth: Int,
  val numPregs: Int,
  val numReadPorts: Int,
  val numWbPorts: Int)
  (implicit p: Parameters) extends BoomBundle
{
  private val pregSz = log2Ceil(numPregs)

  // reading out the busy bits
  val p_rs           = Input(Vec(numReadPorts, UInt(pregSz.W)))
  val p_rs_busy      = Output(Vec(numReadPorts, Bool()))

  def prs(i:Int, w:Int):UInt      = p_rs     (w+i*plWidth)
  def prs_busy(i:Int, w:Int):Bool = p_rs_busy(w+i*plWidth)

  // marking new registers as busy
  val allocated_pdst = Flipped(Vec(plWidth, new ValidIO(UInt(pregSz.W))))

  // marking registers being written back as unbusy
  val unbusy_pdst    = Flipped(Vec(numWbPorts, new ValidIO(UInt(pregSz.W))))

  val debug = new Bundle { val busytable= Output(Bits(numPregs.W)) }
}

/**
 * Register P0 is always NOT_BUSY, and cannot be set to BUSY
 * Note: I do NOT bypass from newly busied registers to the read ports.
 * That bypass check should be done elsewhere (this is to get it off the
 * critical path).
 *
 * @param plWidth pipeline width
 * @param numPregs number of physical registers
 * @param numReadPorts number of read ports to the regfile
 * @param numWbPorts number of writeback ports to the regfile
 */
class BusyTableHelper(
  plWidth: Int,
  numPregs: Int,
  numReadPorts: Int,
  numWbPorts: Int)
  (implicit p: Parameters) extends BoomModule
{
  val io = IO(new BusyTableIo(plWidth, numPregs, numReadPorts, numWbPorts))

  def BUSY     = true.B
  def NOT_BUSY = false.B

  //TODO BUG chisel3
  val table_bsy = RegInit(VecInit(Seq.fill(numPregs){false.B}))

  for (wb_idx <- 0 until numWbPorts) {
    when (io.unbusy_pdst(wb_idx).valid) {
      table_bsy(io.unbusy_pdst(wb_idx).bits) := NOT_BUSY
    }
  }

  for (w <- 0 until plWidth) {
    when (io.allocated_pdst(w).valid && io.allocated_pdst(w).bits =/= 0.U) {
      table_bsy(io.allocated_pdst(w).bits) := BUSY
    }
  }

  // handle bypassing a clearing of the busy-bit
  for (ridx <- 0 until numReadPorts) {
    val just_cleared = io.unbusy_pdst.map(p => p.valid && (p.bits === io.p_rs(ridx))).reduce(_|_)
    // note: no bypassing of the newly busied (that is done outside this module)
    io.p_rs_busy(ridx) := (table_bsy(io.p_rs(ridx)) && !just_cleared)
  }

  io.debug.busytable := table_bsy.asUInt
}

/**
 * Bundle indicating what physical register is busy
 */
class BusyTableOutput extends Bundle
{
  val prs1_busy = Bool()
  val prs2_busy = Bool()
  val prs3_busy = Bool()
}

/**
 * Busy table indicating which physical registers are currently busy
 *
 * @param plWidth pipeline width (dispatch group size)
 * @param rtype type of register the free list is operating on
 * @param numPregs number of physical registers
 * @param numReadPorts number of read ports to the regfile
 * @param numWbPorts number of writeback ports to the regfile
 */
class BusyTable(
  plWidth: Int,
  rtype: BigInt,
  numPregs: Int,
  numReadPorts: Int,
  numWbPorts: Int)
  (implicit p: Parameters) extends BoomModule
{
  private val pregSz = log2Ceil(numPregs)

  val io = IO(new Bundle {
    // Inputs
    val ren_will_fire         = Input(Vec(plWidth, Bool()))
    val ren_uops              = Input(Vec(plWidth, new MicroOp()))

    val map_table             = Input(Vec(plWidth, new MapTableOutput(pregSz)))

    val wb_valids             = Input(Vec(numWbPorts, Bool()))
    val wb_pdsts              = Input(Vec(numWbPorts, UInt(pregSz.W)))

    // Outputs
    val values                = Output(Vec(plWidth, new BusyTableOutput()))

    val debug                 = new Bundle { val busytable= Output(Bits(numPregs.W)) }
  })

  val busy_table = Module(new BusyTableHelper(
    plWidth = plWidth,
    numPregs = numPregs,
    numReadPorts = numReadPorts,
    numWbPorts = numWbPorts))

  // figure out if we need to bypass a newly allocated physical register from a previous instruction in this cycle.
  val prs1_was_bypassed = WireInit(VecInit(Seq.fill(plWidth) {false.B}))
  val prs2_was_bypassed = WireInit(VecInit(Seq.fill(plWidth) {false.B}))
  val prs3_was_bypassed = WireInit(VecInit(Seq.fill(plWidth) {false.B}))
  for {
    w <- 0 until plWidth
    xx <- w-1 to 0 by -1
  }{
    when (io.ren_uops(w).lrs1_rtype === rtype.U && io.ren_will_fire(xx) && io.ren_uops(xx).ldst_val &&
          io.ren_uops(xx).dst_rtype === rtype.U && (io.ren_uops(w).lrs1 === io.ren_uops(xx).ldst))
      { prs1_was_bypassed(w) := true.B }
    when (io.ren_uops(w).lrs2_rtype === rtype.U && io.ren_will_fire(xx) && io.ren_uops(xx).ldst_val &&
          io.ren_uops(xx).dst_rtype === rtype.U && (io.ren_uops(w).lrs2 === io.ren_uops(xx).ldst))
      { prs2_was_bypassed(w) := true.B }

    if (rtype == RT_FLT.litValue) {
       when (io.ren_uops(w).frs3_en && io.ren_will_fire(xx) && io.ren_uops(xx).ldst_val &&
             io.ren_uops(xx).dst_rtype === rtype.U && (io.ren_uops(w).lrs3 === io.ren_uops(xx).ldst))
         { prs3_was_bypassed(w) := true.B }
    }
  }

  for (w <- 0 until plWidth) {
    // Reading the Busy Bits
    // for critical path reasons, we speculatively read out the busy-bits assuming no dependencies between uops
    // then verify if the uop actually uses a register and if it depends on a newly unfreed register
    busy_table.io.prs(0,w) := io.map_table(w).prs1
    busy_table.io.prs(1,w) := io.map_table(w).prs2

    io.values(w).prs1_busy := io.ren_uops(w).lrs1_rtype === rtype.U &&
                              (busy_table.io.prs_busy(0,w) || prs1_was_bypassed(w))
    io.values(w).prs2_busy := io.ren_uops(w).lrs2_rtype === rtype.U &&
                              (busy_table.io.prs_busy(1,w) || prs2_was_bypassed(w))

    if (rtype == RT_FLT.litValue) {
      busy_table.io.prs(2,w) := io.map_table(w).prs3
      io.values(w).prs3_busy := (io.ren_uops(w).frs3_en) && (busy_table.io.prs_busy(2,w) || prs3_was_bypassed(w))
    } else {
      io.values(w).prs3_busy := false.B
    }

    // Updating the Table (new busy register)
    busy_table.io.allocated_pdst(w).valid := io.ren_will_fire(w) &&
                                             io.ren_uops(w).ldst_val &&
                                             io.ren_uops(w).dst_rtype === rtype.U
    busy_table.io.allocated_pdst(w).bits  := io.ren_uops(w).pdst
  }

  // Clear Busy-bit
  for (i <- 0 until numWbPorts) {
    busy_table.io.unbusy_pdst(i).valid := io.wb_valids(i)
    busy_table.io.unbusy_pdst(i).bits  := io.wb_pdsts(i)
  }

  // scalastyle:on
  io.debug := busy_table.io.debug
}
