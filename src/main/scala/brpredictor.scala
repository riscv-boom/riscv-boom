//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Branch Predictor (abstract class)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2015 Oct 12

// provide an abstract class for branch predictors. Also provides support by
// maintaining the global history. However, sub-classes may want to implement
// their own, optimized implementation of global history (e.g. if history is
// very long!).
//
// Notes:
//    - JALR snapshots ghistory since JALRs can mispredict and must be able to
//    reset ghistory.
//    - JALR is added to ghistory (always taken) since it simplifies the logic
//    regarding resetting ghistory.
//
// Issues:
//    - double counting in ghistory with fetch packets that contain multiple
//    branches and the earlier branch mispredicts, causing a re-fetch of the
//    later part of the fetch packet. Subsequent executions of that code, when
//    predicted correctly, won't double count ghistory, potentially increasing
//    mispredictions.

package boom

import Chisel._
import cde.{Parameters, Field}

import util.Str


// This is the response packet from the branch predictor. The predictor is
// expecting to receive it back when it needs to perform an update.
class BpdResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val takens = UInt(width = FETCH_WIDTH)

   // Roughly speaking, track the outcome of the last N branches.
   // The purpose of these is for resetting the global history on a branch
   // mispredict -- they are NOT the history used to index the branch predictor,
   // as speculative updates to the global history will have occurred between
   // the branch predictor is indexed and when the branch makes its own
   // prediction and update to the history.
   val history = if (!ENABLE_VLHR) Some(UInt(width = GLOBAL_HISTORY_LENGTH)) else None
   // Only track user-mode history.
   val history_u = if (!ENABLE_VLHR) Some(UInt(width = GLOBAL_HISTORY_LENGTH)) else None
   // For very long histories, implement as a circular buffer and only snapshot the tail pointer.
   val history_ptr = UInt(width = log2Up(VLHR_LENGTH))

   // The info field stores the response information from the branch predictor.
   // The response is stored (conceptually) in the ROB and is returned to the
   // predictor during the Commit stage to aid in updating the predictor. Each
   // predictor (and its configuration) changes the amount of information it
   // needs to store, and so we need to ask the predictor (in parameters.scala)
   // how many bits of info it requires
   val info = UInt(width = BPD_INFO_SIZE)
}

// BP2 stage needs to speculatively update the history register with what the
// processor decided to do (takes BTB's effect into account).
// Also used for updating the commit-copy during commit.
class GHistUpdate extends Bundle
{
   val taken = Bool()
}

// update comes from the branch-unit with the actual outcome
// it needs to do three things:
//    - 1) correct the history if the processor mispredicted
//    - 2) correct the p-table if it mispredicted (the processor may have been correct though)
//    - 3) strengthen the h-table (on all branch resolutions)
class BpdUpdate(implicit p: Parameters) extends BoomBundle()(p)
{
   // valid: a branch or jump was resolved in the branch unit
   // is_br: a branch was resolved
   // the fetch pc (points to start of the fetch packet)
   // which word in the fetch packet does the update correspond to?
   // processor mispredicted: must reset history
   // history: what was the history our branch saw?
   // bpd_pred_val: did the bpd make a prediction? (i.e., a tag hit)
   // bpd mispredicted:  must correct the bpd predictor
   // taken: was the branch taken?
   // new_pc_same_packet: is the new target PC after a misprediction found
   // within the same fetch packet as the mispredicting branch? If yes, then
   // we have to be careful which "history" we show the new PC.
   val pc = UInt(width = vaddrBits)
   val br_pc = UInt(width = log2Up(FETCH_WIDTH)+log2Ceil(coreInstBytes))
   val brob_idx   = UInt(width = BROB_ADDR_SZ)
   val mispredict = Bool()
   val history     = if (!ENABLE_VLHR) Some(UInt(width = GLOBAL_HISTORY_LENGTH)) else None
   val history_u   = if (!ENABLE_VLHR) Some(UInt(width = GLOBAL_HISTORY_LENGTH)) else None
   val history_ptr = UInt(width = log2Up(VLHR_LENGTH))
   val bpd_predict_val = Bool()
   val bpd_mispredict = Bool()
   val taken = Bool()
   val is_br = Bool()
   val new_pc_same_packet = Bool()

   // give the bpd back the information it sent out when it made a prediction.
   // this information may include things like CSR snapshots.
   val info = UInt(width = BPD_INFO_SIZE)
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------

abstract class BrPredictor(fetch_width: Int, val history_length: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new BoomBundle()(p)
   {
      // the PC to predict
      val req_pc = UInt(INPUT, width = vaddrBits)
      // our prediction. Assert "valid==true" if we want our prediction to be honored.
      // For a tagged predictor, valid==true means we had a tag hit and trust our prediction.
      // For an un-tagged predictor, valid==true should probably only be if a branch is predicted taken.
      // This has an effect on whether to override the BTB's prediction.
      val resp = Decoupled(new BpdResp)
      // speculatively update the global history (once we know we're predicting a branch)
      val hist_update_spec = Valid(new GHistUpdate).flip
      // branch resolution comes from the branch-unit, during the Execute stage.
      val br_resolution = Valid(new BpdUpdate).flip
      val brob = new BrobBackendIo(fetch_width)
      // Pipeline flush - reset history as appropriate.
      // Arrives same cycle as redirecting the front-end -- otherwise, the ghistory would be wrong if it came later!
      val flush = Bool(INPUT)
      // privilege-level (allow predictor to change behavior in different privilege modes).
      val status_prv = UInt(INPUT, width = rocket.PRV.SZ)
   }

   // the (speculative) global history wire (used for accessing the branch predictor state).
   val ghistory = Wire(Bits(width = history_length))

   // the commit update bundle (we update predictors).
   val commit = Wire(Valid(new BrobEntry(fetch_width)))

   // we must delay flush signal by 1 cycle to match the delay of the commit signal from the BROB.
   val r_flush = RegNext(io.flush)

   // The (speculative) global history register. Needs to be massaged before usably by the bpd.
   // Tracks history through all privilege levels.
   private val r_ghistory = new HistoryRegister(history_length)
   // Tracks history only when in user-mode.
   private val r_ghistory_u = new HistoryRegister(history_length)
   // TODO temporary assertion coverage to TAGE
   val r_ghistory_commit_copy = r_ghistory.commit_copy

   // Track VERY long histories with a specialized history implementation.
   // TODO abstract this away so nobody knows which they are using.
   val r_vlh = new VeryLongHistoryRegister(history_length, VLHR_LENGTH)

   val in_usermode = io.status_prv === UInt(rocket.PRV.U)
   val disable_bpd = in_usermode && Bool(ENABLE_BPD_UMODE_ONLY)

   val ghistory_all =
      r_ghistory.value(
         io.hist_update_spec.valid,
         io.hist_update_spec.bits,
         io.br_resolution.valid,
         io.br_resolution.bits,
         r_flush,
         umode_only = false)

   val ghistory_uonly =
      r_ghistory_u.value(
         io.hist_update_spec.valid,
         io.hist_update_spec.bits,
         io.br_resolution.valid,
         io.br_resolution.bits,
         r_flush,
         umode_only = true)

   val vlh_head =
      r_vlh.getSnapshot(
         io.br_resolution.valid,
         io.br_resolution.bits,
         r_flush)

   val r_vlh_commit_copy = r_vlh.commit_copy
   val vlh_commit = Reverse(r_vlh_commit_copy)
   val vlh_commit_head = r_vlh.commit_ptr
   val vlh_raw = r_vlh.raw_value
   val vlh_raw_spec_head = r_vlh.raw_spec_head


   assert (r_ghistory_commit_copy === vlh_commit,
      "[brpredictor] mistmatch between short history and very long history implementations.")


   if (ENABLE_BPD_USHISTORY && !ENABLE_BPD_UMODE_ONLY)
   {
      ghistory := Mux(in_usermode, ghistory_uonly, ghistory_all)
   }
   else if (ENABLE_BPD_UMODE_ONLY)
   {
      ghistory := ghistory_uonly
   }
   else
   {
      ghistory := ghistory_all
   }


   r_ghistory.update(
      io.hist_update_spec.valid,
      io.hist_update_spec.bits,
      io.br_resolution.valid,
      io.br_resolution.bits,
      r_flush,
      disable = Bool(false),
      umode_only = false)

   r_ghistory_u.update(
      io.hist_update_spec.valid,
      io.hist_update_spec.bits,
      io.br_resolution.valid,
      io.br_resolution.bits,
      r_flush,
      disable = !in_usermode,
      umode_only = true)

   r_vlh.update(
      io.hist_update_spec.valid,
      io.hist_update_spec.bits,
      io.br_resolution.valid,
      io.br_resolution.bits,
      r_flush,
      disable = Bool(false))


   io.resp.bits.history match
   {
      case Some(resp_history: UInt) => resp_history := ghistory
      case _ => require (ENABLE_VLHR)
   }
   io.resp.bits.history_u match
   {
      case Some(resp_history_u: UInt) => resp_history_u := ghistory_uonly
      case _ => require (ENABLE_VLHR)
   }

   io.resp.bits.history_ptr := vlh_head


   // -----------------------------------------------

   val brob = Module(new BranchReorderBuffer(fetch_width, NUM_BROB_ENTRIES))
   io.brob <> brob.io.backend
   commit := brob.io.commit_entry

   // TODO add this back in (perhaps just need to initialize mispredicted array?)
//   assert ((~commit.bits.executed.toBits & commit.bits.mispredicted.toBits) === Bits(0),
//      "[BrPredictor] the BROB is marking a misprediction for something that didn't execute.")

   // This shouldn't happen, unless a branch instruction was also marked to flush after it commits.
   // But we don't want to bypass the ghistory to make this "just work", so let's outlaw it.
   assert (!(commit.valid && r_flush), "[brpredictor] commit and flush are colliding.")

   when (commit.valid)
   {
      r_ghistory.commit(commit.bits.ctrl.taken.reduce(_|_))
      r_vlh.commit(commit.bits.ctrl.taken.reduce(_|_))
   }
   when (commit.valid && in_usermode)
   {
      r_ghistory_u.commit(commit.bits.ctrl.taken.reduce(_|_))
   }

}


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

class HistoryRegister(length: Int)
{
   // the (speculative) global history register. Needs to be massaged before usably by the bpd.
   private val r_history = Reg(init = Bits(0, width = length))
   // we need to maintain a copy of the commit history, in-case we need to
   // reset it on a pipeline flush/replay.
   private val r_commit_history = Reg(init = Bits(0, width = length))

   def commit_copy(): UInt = r_commit_history

   def getHistory(resolution: BpdUpdate): UInt =
   {
      resolution.history match
      {
         case Some(history: UInt) => history
         case _ => UInt(0) // enable vlhr
      }
   }

   def getHistoryU(resolution: BpdUpdate): UInt =
   {
      resolution.history_u match
      {
         case Some(history_u: UInt) => history_u
         case _ => UInt(0) // enable vlhr
      }
   }

   def value(
      hist_update_spec_valid: Bool,
      hist_update_spec_bits: GHistUpdate,
      br_resolution_valid: Bool,
      br_resolution_bits: BpdUpdate,
      flush: Bool,
      umode_only: Boolean
      ): UInt =
   {
      // Bypass some history modifications before it can be used by the predictor
      // hash functions. For example, "massage" the history for the scenario where
      // the mispredicted branch is not taken AND we're having to refetch the rest
      // of the fetch_packet.
      val res_history = if (umode_only) getHistoryU(br_resolution_bits) else getHistory(br_resolution_bits)
      val fixed_history = Cat(res_history, br_resolution_bits.taken)
      val ret_value =
         Mux(flush,
            r_commit_history,
         Mux(br_resolution_valid &&
               br_resolution_bits.mispredict &&
               br_resolution_bits.new_pc_same_packet,
            res_history,
         Mux(br_resolution_valid &&
               br_resolution_bits.bpd_mispredict,
            fixed_history,
            r_history)))

      ret_value(length-1,0)
   }

   def update(
      hist_update_spec_valid: Bool,
      hist_update_spec_bits: GHistUpdate,
      br_resolution_valid: Bool,
      br_resolution_bits: BpdUpdate,
      flush: Bool,
      disable: Bool,
      umode_only: Boolean
      ): Unit =
   {
      val res_history = if (umode_only) getHistoryU(br_resolution_bits) else getHistory(br_resolution_bits)
      val fixed_history = Cat(res_history, br_resolution_bits.taken)
      when (disable)
      {
         r_history := r_history
      }
      .elsewhen (flush)
      {
         r_history := r_commit_history
      }
      // TODO do we need to handle same_packet mispredictions?
      .elsewhen (br_resolution_valid && br_resolution_bits.mispredict)
      {
         r_history := fixed_history
      }
      .elsewhen (hist_update_spec_valid)
      {
         r_history := Cat(r_history, hist_update_spec_bits.taken)
      }
   }

   def commit(taken: Bool): Unit =
   {
      r_commit_history := Cat(r_commit_history, taken)
   }

}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// for very long histories, it is more efficient to implement as a circular buffer,
// and to snapshot the head pointer.
class VeryLongHistoryRegister(hlen: Int, vlhr_len: Int)
{
   // we need to provide extra bits for speculating past the commit-head of the buffer.
   private val plen = vlhr_len
   private val hist_buffer = Reg(init = UInt(0, plen))
   // the speculative head point to the next empty spot (head-1 is the newest bit).
   private val spec_head = Reg(init = UInt(0, width = log2Up(plen)))
   private val com_head = Reg(init = UInt(0, width = log2Up(plen)))

   // the tail points to the last (oldest) bit in the history.
   private val com_tail =
      Mux(com_head >= UInt(hlen),
         com_head - UInt(hlen),
         UInt(plen) - (UInt(hlen) - com_head))


   // logical_idx is relative to the logical history, not the physical buffer.
   def getSpecBit(logical_idx: Int): Bool =
   {
      // the "+1" is because the com_head is pointing beyond the valid hist_buffer to the next, open entry.
      val idx = WrapSub(spec_head, logical_idx+1, plen)
      hist_buffer(idx)
   }

   // logical_idx is relative to the logical history, not the physical buffer.
   def getCommitBit(logical_idx: Int): Bool =
   {
      // the "+1" is because the com_head is pointing beyond the valid hist_buffer to the next, open entry.
      val idx = WrapSub(com_head, logical_idx+1, plen)
      hist_buffer(idx)
   }

   def raw_value(): UInt = hist_buffer
   def raw_spec_head(): UInt = spec_head

   def getSnapshot(
      br_resolution_valid: Bool,
      br_resolution_bits: BpdUpdate,
      flush: Bool
      ): UInt =
   {
      val retval = Wire(init=spec_head)
      when (flush)
      {
         retval := com_head
      }
      .elsewhen (br_resolution_valid && br_resolution_bits.mispredict)
      {
         retval := WrapInc(br_resolution_bits.history_ptr, plen)
      }
      .otherwise
      {
         retval := spec_head
      }

      retval
   }

   def commit_copy(): UInt =
   {
      (Cat(hist_buffer, hist_buffer) >> com_tail)(hlen-1,0)
   }

   def commit_ptr(): UInt = com_head

   def update(
      hist_update_spec_valid: Bool,
      hist_update_spec_bits: GHistUpdate,
      br_resolution_valid: Bool,
      br_resolution_bits: BpdUpdate,
      flush: Bool,
      disable: Bool
      ): Unit =
   {
      when (disable)
      {
         ; // nop
      }
      .elsewhen (flush)
      {
         spec_head := com_head
      }
      .elsewhen (br_resolution_valid && br_resolution_bits.mispredict)
      {
         val snapshot_ptr = br_resolution_bits.history_ptr
         assert (snapshot_ptr <= UInt(plen), "[brpredictor] VLHR: snapshot is out-of-bounds.")
         hist_buffer := hist_buffer.bitSet(snapshot_ptr, br_resolution_bits.taken)
         spec_head := WrapInc(snapshot_ptr, plen)
      }
      .elsewhen (hist_update_spec_valid)
      {
         hist_buffer := hist_buffer.bitSet(spec_head, hist_update_spec_bits.taken)
         spec_head := WrapInc(spec_head, plen)
      }
   }

   def commit(taken: Bool): Unit =
   {
      assert(com_head =/= spec_head, "[brpredictor] VLHR: commit head is moving ahead of the spec head.")
      val debug_com_bit = (hist_buffer >> com_head) & UInt(1,1)
      assert (debug_com_bit  === taken, "[brpredictor] VLHR: commit bit doesn't match speculative bit.")
      com_head := WrapInc(com_head, plen)
   }
}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// Act as a "null" branch predictor (it makes no predictions).
// However, we need to instantiate a branch predictor, as it contains the Branch
// ROB which tracks all of the inflight prediction state and performs the
// updates at commit as necessary.
class NullBrPredictor(
   fetch_width: Int,
   history_length: Int = 12
   )(implicit p: Parameters) extends BrPredictor(fetch_width, history_length)(p)
{
   println ("\tBuilding (0 kB) Null Predictor (never predict).")
   io.resp.valid := Bool(false)
}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// Provide a branch predictor that generates random predictions. Good for testing!

case object RandomBpdKey extends Field[RandomBpdParameters]
case class RandomBpdParameters(enabled: Boolean = false)

object RandomBrPredictor
{
   def GetRespInfoSize(p: Parameters): Int =
   {
      // Should be zero (no RespInfo needed for Random predictor), but avoid 0-width wires.
      1
   }
}

class RandomBrPredictor(
   fetch_width: Int
   )(implicit p: Parameters) extends BrPredictor(fetch_width, history_length = 1)(p)
{
   println ("\tBuilding Random Branch Predictor.")
   private val rand_val = Reg(init = Bool(false))
   rand_val := ~rand_val
   private var lfsr= LFSR16(Bool(true))
   def rand(width: Int) = {
        lfsr = lfsr(lfsr.getWidth-1,1)
        val mod = (1 << width) - 1
          util.Random(mod, lfsr)
   }

   io.resp.valid := rand_val
   io.resp.bits.takens := rand(fetch_width)
}


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// The BranchReorderBuffer holds all inflight branchs for the purposes of
// bypassing inflight prediction updates to the predictor. It also holds
// expensive predictor state needed for updating the predictor at commit time.

// NOTE: JALRs also go into the BROB. Their effect is ignored for the purposes
// of updating the branch predictor, as it is the job of the BTB and not the
// BPD to predict JALRs.  However, as JALRs can mispredict and kill inflight
// instructions, it GREATLY simplifies the BROB misprediction handling logic to
// include JALR in the BROB entries (as the brob_tail must be reset on a
// misprediction).  This is further exacerbated by superscalar issues
// <-,br,jalr,br>, in which how the brob_tail is reset depends on whether or
// not a valid branch is before the jalr instruction (and what if the br has
// already been committed?). Yuck. But it does waste BROB entries to include
// JALRs.

class BrobBackendIo(fetch_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val allocate = Decoupled(new BrobEntry(fetch_width)).flip // Decode/Dispatch stage, allocate new entry
   val allocate_brob_tail = UInt(OUTPUT, width = BROB_ADDR_SZ) // tell Decode which entry gets allocated

   val deallocate = Valid(new BrobDeallocateIdx).flip // Commmit stage, from the ROB

   val bpd_update = Valid(new BpdUpdate()).flip // provide br resolution information
   val flush = Bool(INPUT) // wipe the ROB
}

class BrobDeallocateIdx(implicit p: Parameters) extends BoomBundle()(p)
{
   val brob_idx = UInt(width = BROB_ADDR_SZ)
}

// Each "entry" corresponds to a single fetch packet.
// Each fetch packet may contain up to W branches, where W is the fetch_width.
// this only holds the MetaData, which requires combinational/highly-ported access.
// The meat of the BrobEntry is the BpdResp information, and is stored elsewhere.
// We must also allocate entries for JALRs in here since they can single-cycle roll-back the BROB state.
class BrobEntryMetaData(fetch_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val executed     = Vec(fetch_width, Bool()) // Mark that a branch (but not JALR) executed (and should update predictor).
   val taken        = Vec(fetch_width, Bool())
   val mispredicted = Vec(fetch_width, Bool()) // Did bpd mispredict the br? (aka should we update predictor).
                                               // Only set for branches, not jumps.
   val brob_idx     = UInt(width = BROB_ADDR_SZ)

   val debug_executed = Bool() // Did a BR or JALR get executed? Verify we're not deallocating an empty entry.
   val debug_rob_idx = UInt(width = ROB_ADDR_SZ)

   override def cloneType: this.type = new BrobEntryMetaData(fetch_width).asInstanceOf[this.type]
}

class BrobEntry(fetch_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val ctrl = new BrobEntryMetaData(fetch_width)
   val info = new BpdResp
   override def cloneType: this.type = new BrobEntry(fetch_width).asInstanceOf[this.type]
}

// TODO: for large branch snapshots, this maps very poorly to technology (very low-depth, very wide memories).
// A future solution is to spend multiple cycles reading and writing out the branch snapshots while maintaining
// a 1 snapshot/cycle throughput.
class BranchReorderBuffer(fetch_width: Int, num_entries: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new BoomBundle()(p)
   {
      // connection to BOOM's ROB/backend/etc.
      val backend = new BrobBackendIo(fetch_width)

      // update predictor at commit
      val commit_entry = Valid(new BrobEntry(fetch_width))

      // forward predictions
      // TODO enable bypassing of information. See if there's a "match", and then forward the outcome.
      //val pred_req = Valid(new // from fetch, requesting if a prediction matches an inflight entry.
      //val pred_resp = Valid(new // from fetch, return a prediction
   }

   println ("\tBROB (w=" + fetch_width + ") Size (" + num_entries + ") entries of " +
      Wire(new BpdResp).toBits.getWidth + " bits (" +
      Wire(new BrobEntryMetaData(fetch_width)).toBits.getWidth + " of meta-bits).")

   // each entry corresponds to a fetch-packet
   // ROB shouldn't send "deallocate signal" until the entire packet has finished committing.
   // for synthesis quality, break apart ctrl (highly ported) from info, which is stored until commit.
   val entries_ctrl = Reg(Vec(num_entries, new BrobEntryMetaData(fetch_width)))
   val entries_info = SeqMem(num_entries, new BpdResp)

   val head_ptr = Reg(init = UInt(0, log2Up(num_entries)))
   val tail_ptr = Reg(init = UInt(0, log2Up(num_entries)))

   val r_bpd_update = RegNext(io.backend.bpd_update)
   val r_deallocate = RegNext(io.backend.deallocate)

   private def GetIdx(addr: UInt) =
      if (fetch_width == 1) UInt(0)
      else (addr >> UInt(log2Ceil(coreInstBytes))) & Fill(log2Ceil(fetch_width), UInt(1))

   // -----------------------------------------------
   when (io.backend.allocate.valid)
   {
      entries_ctrl(tail_ptr) := io.backend.allocate.bits.ctrl
      entries_info.write(tail_ptr, io.backend.allocate.bits.info)
      tail_ptr := WrapInc(tail_ptr, num_entries)

      assert (tail_ptr === io.backend.allocate.bits.ctrl.brob_idx,
         "[BROB] allocating the wrong entry.")
   }
   when (r_deallocate.valid)
   {
      head_ptr := WrapInc(head_ptr, num_entries)

      assert (entries_ctrl(head_ptr).debug_executed === Bool(true),
         "[BROB] Committing an entry with no executed branches or jalrs.")
      assert (head_ptr === r_deallocate.bits.brob_idx ,
         "[BROB] Committing wrong entry.")
   }

   when (r_bpd_update.valid)
   {
      val idx = GetIdx(r_bpd_update.bits.br_pc)
      entries_ctrl(r_bpd_update.bits.brob_idx).executed(idx) := r_bpd_update.bits.is_br
      entries_ctrl(r_bpd_update.bits.brob_idx).taken(idx) := r_bpd_update.bits.taken
      entries_ctrl(r_bpd_update.bits.brob_idx).debug_executed := Bool(true)
      // update the predictor on either mispredicts or tag misses
      entries_ctrl(r_bpd_update.bits.brob_idx).mispredicted(idx) :=
         r_bpd_update.bits.is_br &&
         (r_bpd_update.bits.bpd_mispredict || !r_bpd_update.bits.bpd_predict_val)

      when (r_bpd_update.bits.mispredict)
      {
         // clear the executed bits behind this instruction
         // (as they are on the misspeculated path)
         for (w <- 0 until fetch_width)
         {
            when (UInt(w) > idx)
            {
               entries_ctrl(r_bpd_update.bits.brob_idx).executed(w) := Bool(false)
               entries_ctrl(r_bpd_update.bits.brob_idx).mispredicted(w) := Bool(false)
            }
         }

         tail_ptr := WrapInc(r_bpd_update.bits.brob_idx, num_entries)
      }
      require (coreInstBytes == 4)
   }

   // backend flushes and branch mispredictions can occur on the same cycle,
   // but because we're registering the misprediction (r_bpd_update), we
   // need to spend one cycle flushing to catch this scenario.
   // We must also delay the flush by one cycle to handle the delayed commit
   // signals being sent to the branch predictor (delayed so we can read out the
   // memories).
   when (RegNext(io.backend.flush))
   {
      head_ptr := UInt(0)
      tail_ptr := UInt(0)
      assert (!io.backend.bpd_update.valid, "[BROB] Collision of flush and BPD update.")
   }

   // -----------------------------------------------
   // outputs

   // entries_info is a sequential memory, so buffer the rest of the bundle to match
   io.commit_entry.valid     := r_deallocate.valid
   io.commit_entry.bits.ctrl := entries_ctrl(head_ptr)
   // Since this is SeqRead, we need to set up the address on the previous cycle before
   // it has been incrementd, if it is going to be incremented.
   val next_head_ptr =
      Mux(r_deallocate.valid,
         WrapInc(head_ptr, num_entries),
         head_ptr)
   io.commit_entry.bits.info := entries_info.read(next_head_ptr, io.backend.deallocate.valid)

   // TODO allow filling the entire BROB ROB, instead of wasting an entry
   val full = (head_ptr === WrapInc(tail_ptr, num_entries))
   io.backend.allocate.ready := !full

   assert (!(full && io.backend.allocate.valid), "Trying to allocate entry while full.")

   io.backend.allocate_brob_tail := tail_ptr

   // -----------------------------------------------

   if (DEBUG_PRINTF)
   {
      for (i <- 0 until num_entries)
      {
         printf (" brob[%d] (%x) T=%x m=%x r=%d "
            , UInt(i, log2Up(num_entries))
            , entries_ctrl(i).executed.toBits
            , entries_ctrl(i).taken.toBits
            , entries_ctrl(i).mispredicted.toBits
            , entries_ctrl(i).debug_rob_idx
//            , entries_info(i).history_ptr
//            , entries_info(i).info
            )
         printf("%c\n",
            Mux(head_ptr === UInt(i) && tail_ptr === UInt(i), Str("B"),
            Mux(head_ptr === UInt(i),                         Str("H"),
            Mux(tail_ptr === UInt(i),                         Str("T"),
                                                              Str(" "))))
            )
      }
   }
}
