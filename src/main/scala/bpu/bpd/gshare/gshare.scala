//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// GShare Branch Predictor
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Notes:
//    - Implements gshare in a a 1r1w SRAM (need to bank to get to 1rw).
//    - Does not rectangularize the memory.
//    - Folds history if it is too long for the number of sets available.

package boom.bpu

import chisel3._
import chisel3.util._
import chisel3.core.withReset

import freechips.rocketchip.config.{Parameters, Field}

import boom.common._
import boom.util.{ElasticReg, Fold, BoomCoreStringPrefix}

/**
 * GShare configuration parameters used in configurations
 *
 * @param enabled using GShare?
 * @param historyLength length of the GHR
 * @param numSets number of entries in the BHT
 */
case class GShareParameters(
  enabled: Boolean = true,
  historyLength: Int = 12,
  numSets: Int = 4096 // 2^(default historyLength) = 2 ^ 12b
)

/**
 * Trait to inherit parameters from the configuration
 */
trait HasGShareParameters extends HasBoomCoreParameters
{
  val gsParams = boomParams.gshare.get
  // prevent us from building something too big (and taking millions of cycles to initialize!)
  val nSets = gsParams.numSets

  val idxSz = log2Ceil(nSets)
  val rowSz = fetchWidth*2
}

/**
 * Counter table entry.
 */
class GShareEntry(val fetchWidth: Int) extends Bundle {
  val cfi_idx = UInt(log2Ceil(fetchWidth).W)
  val cntr = UInt(2.W)
}

/**
 * Set of data values passed around the GShare predictor and used to update the
 * predictor state at commit
 *
 * @param fetchWidth fetch width of the processor
 * @param idxSz log2 of the number of sets/entries in the BHT
 */
class GShareResp(val fetchWidth: Int, val idxSz: Int) extends Bundle
{
  val debugIdx = UInt(idxSz.W) // Can recompute index during update (but let's check for errors).
  val rowdata = UInt((fetchWidth*2).W) // Store to prevent a re-read during an update.

  def isTaken(cfi_idx: UInt) = {
    val cntr = getCounterValue(cfi_idx)
    val taken = cntr(1)
    taken
  }

  def getCounterValue(cfi_idx: UInt) = {
    val cntr = (rowdata >> (cfi_idx << 1.U)) & 0x3.U
    cntr
  }
}

/**
 * Companion object to GShareBrPredictor to get the the size of the
 * branch predictor response
 */
object GShareBrPredictor
{
  def GetRespInfoSize(hlen: Int)(implicit p: Parameters): Int = {
    val boomParams: BoomCoreParams = p(freechips.rocketchip.tile.TileKey).core.asInstanceOf[BoomCoreParams]
    val dummy = new GShareResp(fetchWidth = boomParams.fetchWidth, idxSz = hlen)
    dummy.getWidth
  }
}

/**
 * Class to create a GShare predictor
 *
 * @param historyLength length of GHR in bits
 */
class GShareBrPredictor(
   historyLength: Int = 12,
   bankBytes: Int
   )(implicit p: Parameters)
   extends BoomBrPredictor(historyLength)
   with HasGShareParameters
{
  require (log2Ceil(nSets) == idxSz)

  private def Hash (addr: UInt, hist: UInt) = {
    // fold history if too big for our table
    val folded_history = Fold (hist, idxSz, historyLength)
    ((addr >> (log2Ceil(bankBytes).U)) ^ folded_history)(idxSz-1,0)
  }

  // for initializing the counter table, this is the value to reset the row to.
  private def initRowValue (): UInt = {
    val row = Wire(UInt(rowSz.W))
    row := Fill(fetchWidth, 2.U)
    row
  }

  // Given old counter value, provide the new counter value.
  private def updateCounter(cntr: UInt, taken: Bool): UInt = {
    val next = Wire(UInt(2.W))
    next := Mux(cntr(1) ^ cntr(0), Fill(2, cntr(0)), cntr ^ 1.U)
    next
  }

  // Get a fetchWidth length bit-vector of taken/not-takens.
  private def getTakensFromRow(row: UInt): UInt = {
    val takens = WireInit(VecInit(Seq.fill(fetchWidth){false.B}))
    for (i <- 0 until fetchWidth) {
      // assumes 2-bits per branch.
      takens(i) := row(2*i+1)
    }
    takens.asUInt
  }

  // Pick out the old counter value from a full row and increment it.
  // Return the new row.
  private def updateCounterInRow(old_row: UInt, cfi_idx: UInt, taken: Bool): UInt = {
    val row = Wire(UInt(rowSz.W))
    val shamt = cfi_idx << 1.U
    val mask = Wire(UInt(rowSz.W))
    mask := ~(0x3.U << shamt)
    val old_cntr = (old_row >> shamt) & 0x3.U
    val new_cntr = updateCounter(old_cntr, taken)
    row := (old_row & mask) | (new_cntr << shamt)
    row
  }

  //------------------------------------------------------------
  // reset/initialization

  val s_reset :: s_wait :: s_clear :: s_idle :: Nil = Enum(4)
  val fsm_state = RegInit(s_reset)
  val nResetLagCycles = 128
  val nBanks = 1
  val (lag_counter, lag_done) = Counter(fsm_state === s_wait, nResetLagCycles)
  val (clear_row_addr, clear_done) = Counter(fsm_state === s_clear, nSets/nBanks)

  switch (fsm_state) {
    is (s_reset) { fsm_state := s_wait }
    is (s_wait)  { when (lag_done) { fsm_state := s_clear } }
    is (s_clear) { when (clear_done) { fsm_state := s_idle } }
    is (s_idle)  { when (io.do_reset) { fsm_state := s_clear } }
  }

  //------------------------------------------------------------
  // Predictor state.

  val counter_table = SyncReadMem(nSets, UInt(rowSz.W))

  //------------------------------------------------------------
  // Perform hash in F1.

  val s1_ridx = Hash(this.r_f1_fetchpc, this.r_f1_history)

  //------------------------------------------------------------
  // Get prediction in F2 (and store into an ElasticRegister).

  // return data to superclass (via f2_resp bundle).
  val s2_out = counter_table.read(s1_ridx, this.f1_valid)

  val q_s3_resp = withReset(reset.asBool || io.fe_clear || io.f4_redirect)
    {Module(new ElasticReg(new GShareResp(fetchWidth, idxSz)))}

  q_s3_resp.io.enq.valid := io.f2_valid
  q_s3_resp.io.enq.bits.rowdata  := s2_out
  q_s3_resp.io.enq.bits.debugIdx := RegNext(s1_ridx)
  assert (q_s3_resp.io.enq.ready === !io.f2_stall)

  //------------------------------------------------------------
  // Give out prediction in F3.

  io.resp.valid := fsm_state === s_idle
  io.resp.bits.takens := getTakensFromRow(q_s3_resp.io.deq.bits.rowdata)
  io.resp.bits.info := q_s3_resp.io.deq.bits.asUInt

  q_s3_resp.io.deq.ready := io.resp.ready

  //------------------------------------------------------------
  // Update counter table.

  val com_info = (io.commit.bits.info).asTypeOf(new GShareResp(fetchWidth, idxSz))
  val com_idx = Hash(io.commit.bits.fetch_pc, io.commit.bits.history)(idxSz-1,0)

  // For now, only update the counter table upon a mispredict.
  // This is because we are not tracking individual not-taken branches,
  // and cannot accurately update an entire row at once.
  val wen = io.commit.valid && io.commit.bits.mispredict || (fsm_state === s_clear)
  when (wen) {
    val new_row = updateCounterInRow(
      com_info.rowdata,
      io.commit.bits.miss_cfi_idx,
      io.commit.bits.taken)

    val waddr = Mux(fsm_state === s_clear, clear_row_addr, com_idx)
    val wdata = Mux(fsm_state === s_clear, initRowValue(), new_row)

    counter_table.write(waddr, wdata)
  }

  // First commit will have garbage so ignore it.
  val enable_assert = RegInit(false.B); when (io.commit.valid) { enable_assert := true.B }
  when (enable_assert && io.commit.valid) {
    assert (com_idx === com_info.debugIdx, "[gshare] disagreement on update indices.")
  }

  override def toString: String = BoomCoreStringPrefix(
    "==GShare BPU==",
    "(" + (nSets * fetchWidth * 2/8/1024) +
    " kB) GShare Predictor, with " + historyLength + " bits of history for (" +
    fetchWidth + "-wide fetch) and " + nSets + " entries")
}
