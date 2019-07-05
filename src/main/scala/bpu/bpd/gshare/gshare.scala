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
  val counter = UInt(2.W)

  def isTaken = counter(1)
  def isWeak  = counter(1) ^ counter(0)

  def getTakens: UInt = {
    UIntToOH(cfi_idx) & Fill(fetchWidth, isTaken)
  }

  private def updateCounter(taken: Bool): UInt = {
    Mux(taken,
      Mux(counter === 3.U, counter, counter + 1.U),
      Mux(counter === 0.U, counter, counter - 1.U))
  }

  def getUpdated(cfi_idx: UInt, taken: Bool): GShareEntry = {
    val new_cfi_idx = Wire(UInt(log2Ceil(fetchWidth).W))
    val new_counter = Wire(UInt(2.W))
    new_cfi_idx := this.cfi_idx

    when (cfi_idx === this.cfi_idx) {
      new_counter := updateCounter(taken)
    } .otherwise {
      when (isWeak) {
        new_counter := Mux(taken, 2.U, 1.U)
        new_cfi_idx := cfi_idx
      } .otherwise {
        new_counter := Cat(counter(1), !counter(0))
      }
    }

    val new_entry = Wire(new GShareEntry(fetchWidth))
    new_entry.cfi_idx := new_cfi_idx
    new_entry.counter := new_counter

    new_entry
  }
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
  val entry = new GShareEntry(fetchWidth) // Store to prevent a re-read during an update.

  def isTaken(cfi_idx: UInt) = {
    val counter = entry.counter
    val taken = counter(1)
    taken
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

  private def Hash(addr: UInt, hist: UInt) = {
    // fold history if too big for our table
    val folded_history = Fold (hist, idxSz, historyLength)
    val pc = addr >> log2Ceil(coreInstBytes).U
    val n = idxSz
    val k = log2Ceil(fetchWidth)
    val hashed_pc = ((pc >> k.U) ^ pc(k-1,0))(n-1,0)
    hashed_pc ^ folded_history
  }

  // for initializing the counter table, this is the value to reset the row to.
  private def initEntryValue(): GShareEntry = {
    val entry = Wire(new GShareEntry(fetchWidth))
    entry.counter := 0.U
    entry.cfi_idx := 0.U
    entry
  }

  //------------------------------------------------------------
  // reset/initialization

  val s_reset :: s_wait :: s_clear :: s_idle :: Nil = Enum(4)
  val fsm_state = RegInit(s_reset)
  val nResetLagCycles = 128
  val nBanks = 1
  val (lag_counter, lag_done) = Counter(fsm_state === s_wait, nResetLagCycles)
  val (clear_entry_addr, clear_done) = Counter(fsm_state === s_clear, nSets/nBanks)

  switch (fsm_state) {
    is (s_reset) { fsm_state := s_wait }
    is (s_wait)  { when (lag_done) { fsm_state := s_clear } }
    is (s_clear) { when (clear_done) { fsm_state := s_idle } }
    is (s_idle)  { when (io.do_reset) { fsm_state := s_clear } }
  }

  //------------------------------------------------------------
  // Predictor state.

  val counter_table = SyncReadMem(nSets, new GShareEntry(fetchWidth))

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
  q_s3_resp.io.enq.bits.entry := s2_out
  q_s3_resp.io.enq.bits.debugIdx := RegNext(s1_ridx)
  assert (q_s3_resp.io.enq.ready === !io.f2_stall)

  //------------------------------------------------------------
  // Give out prediction in F3.

  io.resp.valid := fsm_state === s_idle
  io.resp.bits.takens := q_s3_resp.io.deq.bits.entry.getTakens
  io.resp.bits.info := q_s3_resp.io.deq.bits.asUInt

  q_s3_resp.io.deq.ready := io.resp.ready

  //------------------------------------------------------------
  // Update counter table.

  val com_info = (io.commit.bits.info).asTypeOf(new GShareResp(fetchWidth, idxSz))
  val com_idx = Hash(io.commit.bits.fetch_pc, io.commit.bits.history)(idxSz-1,0)

  val wen = io.commit.valid || (fsm_state === s_clear)
  when (wen) {
    val new_entry = com_info.entry.getUpdated(io.commit.bits.miss_cfi_idx, io.commit.bits.taken)

    val waddr = Mux(fsm_state === s_clear, clear_entry_addr, com_idx)
    val wdata = Mux(fsm_state === s_clear, initEntryValue(), new_entry)

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
