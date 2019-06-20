//******************************************************************************
// Copyright (c) 2016 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TAGE Table (used by the TAGE branch predictor)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.bpu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.util.{Str}

import boom.common._
import boom.util.{BoomCoreStringPrefix}

/**
 * IO bundle to connect the TAGE table to the TAGE predictor top
 *
 * @param idxSz log2Ceil of the number of entries in the table
 * @param tagSz size of the tag for each entry
 * @param cntrSz size of the prediction counter in bits
 * @param ubitSz size of the usefulness counter in bits
 */
class TageTableIo(
  val idxSz: Int,
  val tagSz: Int,
  val cntrSz: Int,
  val ubitSz: Int)(implicit p: Parameters)
  extends BoomBundle
{
  // bp1 - request a prediction (provide the index and tag).
  val bp1_req = Flipped(Valid(new TageTableReq(idxSz, tagSz)))

  // bp2 - send prediction to bpd pipeline.
  val bp2_resp = Valid(new TageTableResp(tagSz, cntrSz, ubitSz))

  // Write to this table. Can either be an Allocate, Update, or Degrade operation.
  // (Update and Degrade are not mutually exclusive).
  val write = Flipped(Valid(new TageTableWrite(idxSz, tagSz, cntrSz, ubitSz)))
  def WriteEntry(
    idx: UInt, old: TageTableEntry, allocate: Bool, update: Bool, degrade: Bool, mispredict: Bool, taken: Bool) = {
    this.write.valid := true.B
    this.write.bits.index := idx
    this.write.bits.old := old
    this.write.bits.allocate := allocate
    this.write.bits.update := update
    this.write.bits.degrade := degrade
    this.write.bits.mispredict := mispredict
    this.write.bits.taken := taken
  }

  def InitializeIo = {
    this.write.valid := false.B
    this.write.bits.index := 0.U
    this.write.bits.old := (0.U).asTypeOf(new TageTableEntry(tagSz, cntrSz, ubitSz))
    this.write.bits.allocate := false.B
    this.write.bits.update := false.B
    this.write.bits.degrade := false.B
    this.write.bits.mispredict := false.B
    this.write.bits.taken := false.B
  }

  val do_reset = Input(Bool())
}

/**
 * Data send to the TAGE predictor table
 *
 * @param idxSz log2Ceil of the number entries in the table
 * @param tagSz size of the tag per entry
 */
class TageTableReq(val idxSz: Int, val tagSz: Int) extends Bundle
{
  val index = UInt(idxSz.W)
  val tag = UInt(tagSz.W)
}

/**
 * Data sent from the TAGE predictor table
 *
 * @param tagSz size of the tag per entry
 * @param cntrSz size of the prediction counter in bits
 * @param ubitSz size of the usefulness counter in bits
 */
class TageTableResp(val tagSz: Int, val cntrSz: Int, val ubitSz: Int)(implicit p: Parameters) extends BoomBundle
{
  val tag  = UInt(tagSz.W)
  val cntr = UInt(cntrSz.W)
  val cidx = UInt(log2Ceil(fetchWidth).W)
  val ubit = UInt(ubitSz.W)

  def predictsTaken = cntr(cntrSz-1)
}

/**
 * Single entry in the TAGE table
 *
 * @param tagSz size of the tag per entry
 * @param cntrSz size of the prediction counter in bits
 * @param ubitSz size of the usefulness counter in bits
 */
class TageTableEntry(val tagSz: Int, val cntrSz: Int, val ubitSz: Int)(implicit p: Parameters) extends BoomBundle
{
  val tag  = UInt(tagSz.W)                 // Tag.
  val cntr = UInt(cntrSz.W)                // Prediction counter.
  val cidx = UInt(log2Ceil(fetchWidth).W)  // Control-flow instruction index.
  val ubit = UInt(ubitSz.W)                // Usefulness counter.
}

/**
 * IO bundle to write into the TAGE table
 *
 * @param idxSz log2Ceil of the number entries in the table
 * @param tagSz size of the tag per entry
 * @param cntrSz size of the prediction counter in bits
 * @param ubitSz size of the usefulness counter in bits
 */
class TageTableWrite(val idxSz: Int, val tagSz: Int, val cntrSz: Int, val ubitSz: Int)(implicit p: Parameters)
  extends BoomBundle
{
  val index = UInt(idxSz.W)
  val old = new TageTableEntry(tagSz, cntrSz, ubitSz)

  // What kind of write are we going to perform?
  val allocate = Bool()
  val update   = Bool()
  val degrade  = Bool()
  // What was the outcome of the branch?
  val mispredict = Bool()
  val taken = Bool()
}

/**
 * Construct a TAGE table
 *
 * Note: In Chisel3, all Bundle elements in a Vec() must be homogenous (i.e., when
 * using a Vec() of TageTableIOs, the sub-fields within the TageTableIOs must
 * have the exact same widths (no heterogenous types/widths). Therefore, we must
 * track the max_* size of the parameters, and then within the TageTable we must
 * mask off extra bits as needed.
 *
 * @param numEntries number of entries in a singular TAGE table
 * @param tagSz size of the tag per entry
 * @param maxNumEntries max number of entries in all tables
 * @param maxHistoryLength max history length in all tables
 * @param cntrSz size of the prediction counter in bits
 * @param ubitSz size of the usefulness counter in bits
 * @param id id of the table
 * @param historyLength history length that this table uses
 */
class TageTable(
  numEntries: Int,
  tagSz: Int,
  maxNumEntries: Int,
  maxHistoryLength: Int,
  cntrSz: Int,
  ubitSz: Int,
  id: Int,
  historyLength: Int
  )(implicit p: Parameters) extends BoomModule
{
  val idxSz = log2Ceil(numEntries)

  val io = IO(new TageTableIo(idxSz, tagSz, cntrSz, ubitSz))

  private val CNTR_MAX = ((1 << cntrSz) - 1).U
  private val CNTR_WEAK_TAKEN = (1 << (cntrSz-1)).U
  private val CNTR_WEAK_NOTTAKEN = CNTR_WEAK_TAKEN - 1.U
  private val UBIT_MAX = ((1 << ubitSz) - 1).U
  private val UBIT_INIT_VALUE = 0.U

  //------------------------------------------------------------
  // Utility functions

  // Take in the old counter value. If allocating the counter, set to weak.
  // Otherwise, move the counter in the direction of the branch.
  private def generateCounter(old: UInt, taken: Bool, allocate: Bool): UInt = {
    val next = Wire(UInt(cntrSz.W))
    next :=
      Mux(taken && allocate        , CNTR_WEAK_TAKEN,
      Mux(!taken && allocate       , CNTR_WEAK_NOTTAKEN,
      Mux(taken && old =/= CNTR_MAX, old + 1.U,
      Mux(!taken && old =/= 0.U    , old - 1.U,
                                     old))))
    next
  }

  // Decide on the new u-bit value.
  private def generateUBit(old: UInt, allocate: Bool, update: Bool, degrade: Bool, mispredicted: Bool): UInt = {
    val next = Wire(UInt(ubitSz.W))

    // If mispredicted or degraded for sz==1, set to 0.
    require (ubitSz == 1)
    next :=
      Mux(allocate, UBIT_INIT_VALUE,
      Mux(update && !mispredicted, 1.U,
          0.U))

    assert (PopCount(VecInit(allocate, update, degrade)) > 0.U,
            "[TageTable[" + id + "]] ubit not told to do something.")

    next
  }

  //------------------------------------------------------------
  // reset/initialization

  val s_reset :: s_wait :: s_clear :: s_idle :: Nil = Enum(4)
  val fsm_state = RegInit(s_reset)
  val nResetLagCycles = 64
  val nBanks = 1
  val (lag_counter, lag_done) = Counter(fsm_state === s_wait, nResetLagCycles)
  val (clear_row_addr, clear_done) = Counter(fsm_state === s_clear, numEntries/nBanks)

  switch (fsm_state) {
    is (s_reset) { fsm_state := s_wait }
    is (s_wait)  { when (lag_done) { fsm_state := s_clear } }
    is (s_clear) { when (clear_done) { fsm_state := s_idle } }
    is (s_idle)  { when (io.do_reset) { fsm_state := s_clear } }
  }

  //------------------------------------------------------------
  // State

  // TODO add banking
  val ram = SyncReadMem(numEntries, new TageTableEntry(tagSz, cntrSz, ubitSz))
  ram.suggestName("TageTableDataArray")

  //------------------------------------------------------------
  // Get Prediction

  val s1_valid = io.bp1_req.valid
  val s1_idx = io.bp1_req.bits.index
  val s1_tag = io.bp1_req.bits.tag

  val s2_out = ram.read(s1_idx, s1_valid)

  val s2_tag_hit = s2_out.tag === RegEnable(s1_tag, s1_valid)

  io.bp2_resp.valid     := s2_tag_hit && RegNext(fsm_state === s_idle, false.B)
  io.bp2_resp.bits.tag  := s2_out.tag
  io.bp2_resp.bits.cntr := s2_out.cntr
  io.bp2_resp.bits.cidx := s2_out.cidx
  io.bp2_resp.bits.ubit := s2_out.ubit

  //------------------------------------------------------------
  // Update (Commit)

  when (io.write.valid || fsm_state === s_clear)
  {
    val widx = WireInit(io.write.bits.index)

    // Allocate, Update, and Degrade. Effects how we compute next counter, u-bit values.
    val allocate   = io.write.bits.allocate
    val update     = io.write.bits.update
    val degrade    = io.write.bits.degrade
    val taken      = io.write.bits.taken
    val mispredict = io.write.bits.mispredict

    val wentry = Wire(new TageTableEntry(tagSz, cntrSz, ubitSz))

    when (fsm_state === s_clear) {
      widx := clear_row_addr
      wentry.tag := 0.U
      wentry.cntr := 0.U
      wentry.cidx := 0.U
      wentry.ubit := 0.U
    } .otherwise {
      widx := io.write.bits.index
      wentry := io.write.bits.old
      wentry.cntr := generateCounter(io.write.bits.old.cntr, taken, allocate)
      wentry.ubit := generateUBit(io.write.bits.old.ubit, allocate, update, degrade, mispredict)
    }

    ram.write(widx, wentry)

    assert (!(allocate && !mispredict), "[TageTable] an allocated entry should be mispredicted.")
    assert (!(allocate && (update || degrade)), "[TageTable] competing updates.")
    assert (widx < numEntries.U, "[TageTable] out of bounds write index.")
  }

  override def toString: String = BoomCoreStringPrefix(
    "TageTable[" + id + "] - " +
    numEntries + " entries, " +
    historyLength + " bits of history, " +
    tagSz + "-bit tags, " +
    cntrSz + "-bit counters")
}
