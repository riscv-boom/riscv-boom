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
import freechips.rocketchip.config.{Parameters, Field}

import freechips.rocketchip.util.Str
import freechips.rocketchip.rocket.RocketCoreParams


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
   val history = UInt(width = GLOBAL_HISTORY_LENGTH)

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
//   val brob_idx   = UInt(width = BROB_ADDR_SZ) TODO switch to ftq_idx?
   val mispredict = Bool()
   val history     = UInt(width = GLOBAL_HISTORY_LENGTH)
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

// Return the desired branch predictor based on the provided parameters.
object BrPredictor
{
   def apply(tileParams: freechips.rocketchip.tile.TileParams, boomParams: BoomCoreParams)(implicit p: Parameters): BrPredictor =
   {
      val boomParams: BoomCoreParams = p(freechips.rocketchip.tile.TileKey).core.asInstanceOf[BoomCoreParams]
      val fetch_width = boomParams.fetchWidth
      val enableCondBrPredictor = boomParams.enableBranchPredictor

      var br_predictor: BrPredictor = null

      if (enableCondBrPredictor && boomParams.tage.isDefined && boomParams.tage.get.enabled)
      {
         br_predictor = Module(new TageBrPredictor(
            fetch_width = fetch_width,
            num_tables = boomParams.tage.get.num_tables,
            table_sizes = boomParams.tage.get.table_sizes,
            history_lengths = boomParams.tage.get.history_lengths,
            tag_sizes = boomParams.tage.get.tag_sizes,
            ubit_sz = boomParams.tage.get.ubit_sz))
      }
      else if (enableCondBrPredictor && boomParams.gskew.isDefined && boomParams.gskew.get.enabled)
      {
         br_predictor = Module(new GSkewBrPredictor(
            fetch_width = fetch_width,
            history_length = boomParams.gskew.get.history_length,
            dualported = boomParams.gskew.get.dualported,
            enable_meta = boomParams.gskew.get.enable_meta))
      }
      else if (enableCondBrPredictor && boomParams.gshare.isDefined && boomParams.gshare.get.enabled)
      {
         br_predictor = Module(new GShareBrPredictor(
            fetch_width = fetch_width,
            history_length = boomParams.gshare.get.history_length,
            dualported = boomParams.gshare.get.dualported))
      }
      else if (enableCondBrPredictor && p(SimpleGShareKey).enabled)
      {
         br_predictor = Module(new SimpleGShareBrPredictor(
            fetch_width = fetch_width,
            history_length = p(SimpleGShareKey).history_length))
      }
      else if (enableCondBrPredictor && p(RandomBpdKey).enabled)
      {
         br_predictor = Module(new RandomBrPredictor(
            fetch_width = fetch_width))
      }
      else
      {
         br_predictor = Module(new NullBrPredictor(
            fetch_width = fetch_width,
            history_length = 1))
      }

      br_predictor
   }
}


abstract class BrPredictor(
   fetch_width: Int,
   val history_length: Int
   )(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new BoomBundle()(p)
   {
      // the PC to predict
      val req_pc = UInt(INPUT, width = vaddrBits)
      // our prediction. Assert "valid==true" if we want our prediction to be honored.
      // For a tagged predictor, valid==true means we had a tag hit and trust our prediction.
      // For an un-tagged predictor, valid==true should probably only be if a branch is predicted taken.
      // This has an effect on whether to override the BTB's prediction.
      val resp = Decoupled(new BpdResp)
      // bpd resp comes in F2, but speculative history update isn't until F3, so to avoid missing older branches in our
      // snapshots, we need to wait and pull the history from F3 for snapshotting.
      val f3_resp_history = UInt(width = GLOBAL_HISTORY_LENGTH)


      // speculatively update the global history (once we know we're predicting a branch)
      val hist_update_spec = Valid(new GHistUpdate).flip
      // branch resolution comes from the branch-unit, during the Execute stage.
      val exe_bpd_update = Valid(new BpdUpdate).flip // Update/correct the BPD during Branch Resolution (Exe).
      // Pipeline flush - reset history as appropriate.
      // Arrives same cycle as redirecting the front-end -- otherwise, the ghistory would be wrong if it came later!
      val flush = Bool(INPUT)
      // privilege-level (allow predictor to change behavior in different privilege modes).
      val status_prv = UInt(INPUT, width = freechips.rocketchip.rocket.PRV.SZ)
   })

   // the (speculative) global history wire (used for accessing the branch predictor state).
   val ghistory = Wire(Bits(width = history_length))

   // the commit update bundle (we update predictors).
   val commit = Wire(Valid(new BrobEntry(fetch_width)))

   // we must delay flush signal by 1 cycle to match the delay of the commit signal from the BROB.
   val r_flush = RegNext(io.flush)

   // The (speculative) global history register. Needs to be massaged before usably by the bpd.
   // Tracks history through all privilege levels.
   private val r_ghistory = new HistoryRegister(history_length)
   val r_ghistory_commit_copy = r_ghistory.commit_copy

   val in_usermode = io.status_prv === UInt(freechips.rocketchip.rocket.PRV.U)
   val disable_bpd = !in_usermode && ENABLE_BPD_UMODE_ONLY.B

   val ghistory_all =
      r_ghistory.value(
         io.hist_update_spec.valid,
         io.hist_update_spec.bits,
         io.exe_bpd_update.valid,
         io.exe_bpd_update.bits,
         r_flush,
         umode_only = false)

   ghistory := ghistory_all

   val r_ghistory_raw = r_ghistory.raw_value

   r_ghistory.update(
      io.hist_update_spec.valid,
      io.hist_update_spec.bits,
      io.exe_bpd_update.valid,
      io.exe_bpd_update.bits,
      r_flush,
      disable = Bool(false),
      umode_only = false)

   io.f3_resp_history := ghistory


   // -----------------------------------------------

// TODO XXX need to update predictor from FTQ
//   val brob = Module(new BranchReorderBuffer(fetch_width, NUM_BROB_ENTRIES))
//   io.brob <> brob.io.backend
//   commit := brob.io.commit_entry

   // This shouldn't happen, unless a branch instruction was also marked to flush after it commits.
   // But we don't want to bypass the ghistory to make this "just work", so let's outlaw it.
   assert (!(commit.valid && r_flush), "[brpredictor] commit and flush are colliding.")

   when (commit.valid)
   {
      r_ghistory.commit(commit.bits.ctrl.taken.reduce(_|_))
   }
}


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

class HistoryRegister(length: Int)
{
   // the (speculative) global history register. Needs to be massaged before usably by the bpd.
   private val r_history = Reg(init = UInt(0, width = length))
   // we need to maintain a copy of the commit history, in-case we need to
   // reset it on a pipeline flush/replay.
   private val r_commit_history = Reg(init = UInt(0, width = length))

   def commit_copy(): UInt = r_commit_history

   def getHistory(resolution: BpdUpdate): UInt =
   {
      resolution.history
   }

   def raw_value(): UInt = r_history

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
      val res_history = getHistory(br_resolution_bits)
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
      val res_history = getHistory(br_resolution_bits)
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
   io.resp.valid := false.B
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
   private val rand_val = Reg(init = false.B)
   rand_val := ~rand_val
   private var lfsr= LFSR16(true.B)
   def rand(width: Int) = {
        lfsr = lfsr(lfsr.getWidth-1,1)
        val mod = (1 << width) - 1
          freechips.rocketchip.util.Random(mod, lfsr)
   }

   io.resp.valid := rand_val
   io.resp.bits.takens := rand(fetch_width)
}




//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// TODO DELETE ALL OF THIS LATER


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


