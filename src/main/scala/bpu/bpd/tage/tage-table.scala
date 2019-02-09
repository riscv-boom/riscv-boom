//******************************************************************************
// Copyright (c) 2016 - 2018, The Regents of the University of California (Regents).
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

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import boom.common._

/**
 * IO bundle to connect the TAGE table to the TAGE predictor top
 *
 * @param fetch_width # of instructions fetched
 * @param index_sz ...
 * @param tag_sz ...
 * @param cntr_sz ...
 * @param ubit_sz ...
 */
class TageTableIo(
   val fetch_width: Int,
   val index_sz: Int,
   val tag_sz: Int,
   val cntr_sz: Int,
   val ubit_sz: Int)
   extends Bundle
{
   // bp1 - request a prediction (provide the index and tag).
   val bp1_req = Flipped(Valid(new TageTableReq(index_sz, tag_sz)))

   // bp2 - send prediction to bpd pipeline.
   val bp2_resp = Valid(new TageTableResp(fetch_width, tag_sz, cntr_sz, ubit_sz))

   // Write to this table. Can either be an Allocate, Update, or Degrade operation.
   // (Update and Degrade are not mutually exclusive).
   val write = Flipped(Valid(new TageTableWrite(fetch_width, index_sz, tag_sz, cntr_sz, ubit_sz)))
   def WriteEntry(
      idx: UInt, old: TageTableEntry, allocate: Bool, update: Bool, degrade: Bool, mispredict: Bool, taken: Bool) =
   {
      this.write.valid := true.B
      this.write.bits.index := idx
      this.write.bits.old := old
      this.write.bits.allocate := allocate
      this.write.bits.update := update
      this.write.bits.degrade := degrade
      this.write.bits.mispredict := mispredict
      this.write.bits.taken := taken
   }

   def InitializeIo =
   {
      this.write.valid := false.B
      this.write.bits.index := 0.U
      this.write.bits.old := (0.U).asTypeOf(new TageTableEntry(fetch_width, tag_sz, cntr_sz, ubit_sz))
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
 * @param index_sz ...
 * @param tag_sz ...
 */
class TageTableReq(val index_sz: Int, val tag_sz: Int) extends Bundle
{
   val index = UInt(index_sz.W)
   val tag = UInt(tag_sz.W)
}

/**
 * Data sent from the TAGE predictor table
 *
 * @param fetch_width # of instructions fetched
 * @param tag_sz ...
 * @param cntr_sz ...
 * @param ubit_sz ...
 */
class TageTableResp(val fetch_width: Int, val tag_sz: Int, val cntr_sz: Int, val ubit_sz: Int) extends Bundle
{
   val tag  = UInt(tag_sz.W)
   val cntr = UInt(cntr_sz.W)
   val cidx = UInt(log2Ceil(fetch_width).W)
   val ubit = UInt(ubit_sz.W)

   def predictsTaken = cntr(cntr_sz-1)
}

/**
 * Single entry in the TAGE table
 *
 * @param fetch_width # of instructions fetched
 * @param tag_sz ...
 * @param cntr_sz ...
 * @param ubit_sz ...
 */
class TageTableEntry(val fetch_width: Int, val tag_sz: Int, val cntr_sz: Int, val ubit_sz: Int) extends Bundle
{
   val tag  = UInt(tag_sz.W)                 // Tag.
   val cntr = UInt(cntr_sz.W)                // Prediction counter.
   val cidx = UInt(log2Ceil(fetch_width).W)  // Control-flow instruction index.
   val ubit = UInt(ubit_sz.W)                // Usefulness counter.
}

/**
 * IO bundle to write into the TAGE table
 *
 * @param fetch_width # of instructions fetched
 * @param index_sz ...
 * @param tag_sz ...
 * @param cntr_sz ...
 * @param ubit_sz ...
 */
class TageTableWrite(val fetch_width: Int, val index_sz: Int, val tag_sz: Int, val cntr_sz: Int, val ubit_sz: Int)
  extends Bundle
{
   val index = UInt(index_sz.W)
   val old = new TageTableEntry(fetch_width, tag_sz, cntr_sz, ubit_sz)

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
 * @param fetch_width # of instructions fetched
 * @param num_entries ...
 * @param tag_sz ...
 * @param max_num_entries ...
 * @param max_history_length ...
 * @param cntr_sz ...
 * @param ubit_sz ...
 * @param id ...
 * @param history_length ...
 */
class TageTable(
   fetch_width: Int,
   num_entries: Int,
   tag_sz: Int,
   max_num_entries: Int,
   max_history_length: Int,
   cntr_sz: Int,
   ubit_sz: Int,
   id: Int,
   history_length: Int
   )(implicit p: Parameters) extends BoomModule()(p)
{
   val index_sz = log2Ceil(num_entries)

   val io = IO(new TageTableIo(fetch_width, index_sz, tag_sz, cntr_sz, ubit_sz))

   private val CNTR_MAX = ((1 << cntr_sz) - 1).U
   private val CNTR_WEAK_TAKEN = (1 << (cntr_sz-1)).U
   private val CNTR_WEAK_NOTTAKEN = CNTR_WEAK_TAKEN - 1.U
   private val UBIT_MAX = ((1 << ubit_sz) - 1).U
   private val UBIT_INIT_VALUE = 0.U

   //------------------------------------------------------------
   // Utility functions

   // Take in the old counter value. If allocating the counter, set to weak.
   // Otherwise, move the counter in the direction of the branch.
   private def generateCounter(old: UInt, taken: Bool, allocate: Bool): UInt =
   {
      val next = Wire(UInt(cntr_sz.W))
      next :=
         Mux(taken && allocate        , CNTR_WEAK_TAKEN,
         Mux(!taken && allocate       , CNTR_WEAK_NOTTAKEN,
         Mux(taken && old =/= CNTR_MAX, old + 1.U,
         Mux(!taken && old =/= 0.U    , old - 1.U,
                                        old))))
      next
   }

   // Decide on the new u-bit value.
   private def generateUBit(old: UInt, allocate: Bool, update: Bool, degrade: Bool, mispredicted: Bool): UInt =
   {
      val next = Wire(UInt(ubit_sz.W))

      // If mispredicted or degraded for sz==1, set to 0.
      require (ubit_sz == 1)
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
   val (clear_row_addr, clear_done) = Counter(fsm_state === s_clear, num_entries/nBanks)

   switch (fsm_state)
   {
      is (s_reset) { fsm_state := s_wait }
      is (s_wait)  { when (lag_done) { fsm_state := s_clear } }
      is (s_clear) { when (clear_done) { fsm_state := s_idle } }
      is (s_idle)  { when (io.do_reset) { fsm_state := s_clear } }
   }

   //------------------------------------------------------------
   // State

   // TODO add banking
   val ram = SyncReadMem(num_entries, new TageTableEntry(fetch_width, tag_sz, cntr_sz, ubit_sz))
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

      val wentry = Wire(new TageTableEntry(fetch_width, tag_sz, cntr_sz, ubit_sz))

      when (fsm_state === s_clear)
      {
         widx := clear_row_addr
         wentry.tag := 0.U
         wentry.cntr := 0.U
         wentry.cidx := 0.U
         wentry.ubit := 0.U
      }
      .otherwise
      {
         widx := io.write.bits.index
         wentry := io.write.bits.old
         wentry.cntr := generateCounter(io.write.bits.old.cntr, taken, allocate)
         wentry.ubit := generateUBit(io.write.bits.old.ubit, allocate, update, degrade, mispredict)
      }

      ram.write(widx, wentry)

      assert (!(allocate && !mispredict), "[TageTable] an allocated entry should be mispredicted.")
      assert (!(allocate && (update || degrade)), "[TageTable] competing updates.")
      assert (widx < num_entries.U, "[TageTable] out of bounds write index.")
   }

   override def toString: String =
      "   TageTable[" + id + "] - " +
      num_entries + " entries, " +
      history_length + " bits of history, " +
      tag_sz + "-bit tags, " +
      cntr_sz + "-bit counters"
}
