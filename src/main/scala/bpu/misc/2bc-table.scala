//******************************************************************************
// Copyright (c) 2016 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Two-bit Counter Table
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Provide a two-bit counter table for use by branch predictors. Should only be
// updated during Commit.
//
//   P | H
//   1 | 1 strongly taken
//   1 | 0 weakly taken
//   0 | 1 weakly not-taken
//   0 | 0 strongly not-taken
//
// To reduce port access requirements, the two-bit state machine is not actually
// a counter - a misprediction when in the weak state pushes the counter into
// the strong state for the correct prediction.
//
//   Example: (00 -> 01 -> 11) for a string of taken branches.
//
// P-bits
//   Read: every cycle for a prediction.
//   Write: on a branch misprediction (with the h-bit value).
//
// H-bits
//   Read: on a branch misprediction.
//   Write: on a branch resolution (with the direction of the branch).
//
// TODO:
//   - Don't read the p-table SRAM if stalled (need extra state to store data
//     while stalled)..

package boom.bpu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters}

import boom.util.{SeqMem1rwTransformable}
import boom.common.{BoomBundle, BoomModule}

class UpdateEntry(val idxSz: Int)(implicit p: Parameters) extends BoomBundle
{
  val index            = UInt(idxSz.W)
  val executed         = Vec(fetchWidth, Bool())
  val takens           = Vec(fetchWidth, Bool())
  // Was there a misprediction? If yes, we need to read the h-tables.
  val was_mispredicted = Bool()
  // Are we initializing this entry? If yes, we need to write directly to both P and H-tables.
  // If takens(i), then we initialize entry to Weak-Taken. Otherwise, Weak-NotTaken.
  val do_initialize    = Bool()
}

class BrTableUpdate(val idxSz: Int)(implicit p: Parameters) extends BoomBundle
{
  val index      = UInt(idxSz.W)
  val executed   = UInt(fetchWidth.W) // which words in the fetch packet does the update correspond to?
  val new_value  = UInt(fetchWidth.W)
}

/**
 * Read p-table every cycle for a prediction.
 * Write p-table only if a misprediction occurs.
 * The p-table requires 1read/1write port.
 */
abstract class PTable(numEntries: Int)(implicit p: Parameters) extends BoomModule
{
  val idxSz = log2Ceil(numEntries)
  val io = IO(new Bundle {
    val s1_r_idx = Input(UInt(idxSz.W))
    val s2_r_out = Output(UInt(fetchWidth.W))
    val stall    = Input(Bool())
    val update   = Flipped(Decoupled(new BrTableUpdate(idxSz)))
  })

  val ridx = Wire(UInt())
  val last_idx = RegNext(ridx)
  ridx := Mux(io.stall, last_idx, io.s1_r_idx)
}

/**
 * This version uses 1 read and 1 write port.
 */
class PTableDualPorted(numEntries: Int)(implicit p: Parameters) extends PTable(numEntries)
{
  val p_table = SyncReadMem(numEntries, Vec(fetchWidth, Bool()))

  io.update.ready := true.B

  when (io.update.valid) {
    val waddr = io.update.bits.index
    val wdata = VecInit(io.update.bits.new_value.toBools)
    val wmask = io.update.bits.executed.toBools
    p_table.write(waddr, wdata, wmask)
  }

  io.s2_r_out := p_table.read(this.ridx, !io.stall).asUInt
}

/**
 * Read p-table every cycle for a prediction.
 * Write p-table only if a misprediction occurs.
 * The p-table requires 1read/1write port.
 * This version banks the table to get by with a single 1rw port.
 */
class PTableBanked(numEntries: Int)(implicit p: Parameters) extends PTable(numEntries)
{
  val p_table_0 = Module(new SeqMem1rwTransformable(numEntries/2, fetchWidth))
  val p_table_1 = Module(new SeqMem1rwTransformable(numEntries/2, fetchWidth))

  private def getBank (idx: UInt): UInt = idx(0)
  private def getRowIdx (idx: UInt): UInt = idx >> 1.U

  val widx = io.update.bits.index
  val rbank = getBank(ridx)
  val wbank = getBank(widx)
  io.update.ready := rbank =/= wbank

  val ren_0   = rbank === 0.U
  val ren_1   = rbank === 1.U
  val wdata   = VecInit(io.update.bits.new_value.toBools)
  val wmask = io.update.bits.executed.toBools

  // ** use resizable SyncReadMems ** //
  p_table_0.io.wen   := !ren_0 && wbank === 0.U && io.update.valid
  p_table_0.io.waddr := getRowIdx(widx)
  p_table_0.io.wmask := VecInit(wmask).asUInt
  p_table_0.io.wdata := wdata.asUInt
  p_table_1.io.wen   := !ren_1 && wbank === 1.U && io.update.valid
  p_table_1.io.waddr := getRowIdx(widx)
  p_table_1.io.wmask := VecInit(wmask).asUInt
  p_table_1.io.wdata := wdata.asUInt

  p_table_0.io.ren   := ren_0
  p_table_0.io.raddr := getRowIdx(ridx)
  p_table_1.io.ren   := ren_1
  p_table_1.io.raddr := getRowIdx(ridx)
  val rout_0 = p_table_0.io.rout
  val rout_1 = p_table_1.io.rout

  val s2_ren = RegEnable(ren_0, !io.stall)
  io.s2_r_out := Mux(s2_ren, rout_0, rout_1)
}

/**
 * Write h-table for every branch resolution (we can buffer these up).
 * Read h-table immediately to update the p-table (only if a mispredict occurred).
 * Track the number of entries in the P-table (needed to couple our I/Os).
 * We may (or may not) share one h-bit across two p-bits.
 */
class HTable(numPEntries: Int, shareHBit: Boolean)(implicit p: Parameters) extends BoomModule
{
  private val ptableIdxSz = log2Ceil(numPEntries)
  private val numHEntries = if (shareHBit) numPEntries/2 else numPEntries
  val io = IO(new Bundle {
    // Update the h-table.
    val update   = Flipped(Valid(new UpdateEntry(ptableIdxSz)))
    // Enqueue an update to the p-table.
    val pwq_enq  = Decoupled(new BrTableUpdate(ptableIdxSz))
  })

  val h_table = Module(new SeqMem1rwTransformable(numHEntries, fetchWidth))
  val hwq = Module(new Queue(new UpdateEntry(ptableIdxSz), entries=4))

  hwq.io.enq <> io.update

  val h_ren = io.update.valid && io.update.bits.was_mispredicted && !io.update.bits.do_initialize
  hwq.io.deq.ready := !h_ren

  h_table.io.wen   := !h_ren && hwq.io.deq.valid
  h_table.io.waddr := hwq.io.deq.bits.index
  h_table.io.wmask := hwq.io.deq.bits.executed.asUInt
  h_table.io.wdata := VecInit(hwq.io.deq.bits.takens.map(t =>
                       Mux(hwq.io.deq.bits.do_initialize, !t, t))).asUInt

  val h_raddr = io.update.bits.index
  h_table.io.ren   := h_ren
  h_table.io.raddr := h_raddr
  val h_rout = h_table.io.rout

  io.pwq_enq.valid          := RegNext(h_ren || io.update.bits.do_initialize)
  io.pwq_enq.bits.index     := RegNext(h_raddr)
  io.pwq_enq.bits.executed  := RegNext(io.update.bits.executed.asUInt)
  io.pwq_enq.bits.new_value := Mux(RegNext(io.update.bits.do_initialize),
                                   RegNext(io.update.bits.takens.asUInt),
                                   h_rout)
}

class TwobcCounterTable(
  numEntries: Int,
  dualported: Boolean = false,
  shareHBit: Boolean = true // share 1 h-bit across 2 p-bits.
  )(implicit p: Parameters) extends BoomModule
{
  private val idxSz = log2Ceil(numEntries)
  private val numHEntries = if (shareHBit) numEntries/2 else numEntries

  val io = IO(new Bundle {
    // send read addr on cycle 0, get data out on cycle 2.
    val s1_r_idx = Input(UInt(idxSz.W))
    val s2_r_out = Output(UInt(fetchWidth.W))
    val stall    = Input(Bool())

    val update   = Flipped(Valid(new UpdateEntry(idxSz)))
  })

  //------------------------------------------------------------
  // prediction bits
  // hysteresis bits

  val p_table = if (dualported) Module(new PTableDualPorted(numEntries))
                else            Module(new PTableBanked(numEntries))
  val h_table = Module(new HTable(numPEntries = numEntries, shareHBit = shareHBit))

  //------------------------------------------------------------
  // write queue from h-table to p-table.

  val pwqEntries = if (dualported) 2 else 6
  val pwq = Module(new Queue(new BrTableUpdate(idxSz), entries=pwqEntries))

  //------------------------------------------------------------
  // p-table
  // Read table every cycle for a prediction.
  // Write table only if a misprediction occurs.

  p_table.io.s1_r_idx <> io.s1_r_idx
  io.s2_r_out <> p_table.io.s2_r_out
  p_table.io.update <> pwq.io.deq
  p_table.io.stall := io.stall

  //------------------------------------------------------------
  // h-table
  // Write table for every branch resolution (we can buffer these up).
  // Read table immediately to update the p-table (only if a mispredict occurred).

  h_table.io.update <> io.update
  pwq.io.enq <> h_table.io.pwq_enq

  //------------------------------------------------------------

  override def toString: String =
    ("  Building (" +
    (numEntries * fetchWidth * 2/8/1024) + " kB) 2-bit counter table for (" +
    fetchWidth + "-wide fetch) and " +
    numEntries + " p-table entries " +
    (if (dualported) "[1read/1write]" else "[1rw]" +
    ", " + numHEntries + " h-table entries."))
}

