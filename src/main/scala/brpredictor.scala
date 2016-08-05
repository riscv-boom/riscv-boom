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

package boom

import Chisel._
import cde.{Parameters, Field}

import rocket.Str


// This is the response packet from the branch predictor. The predictor is
// expecting to receive it back when it needs to perform an update.
class BpdResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val takens = UInt(width = FETCH_WIDTH)
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
   val brob_idx   = UInt(width = BROB_ADDR_SZ)
   val mispredict = Bool()
   val history = Bits(width = GLOBAL_HISTORY_LENGTH)
   val bpd_predict_val = Bool()
   val bpd_mispredict = Bool()
   val taken = Bool()
   val is_br = Bool()
   val new_pc_same_packet = Bool()
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
      // pipeline flush - reset history as appropriate
      val flush = Bool(INPUT)
   }

   // the (speculative) global history wire (used for accessing the branch predictor state).
   val ghistory = Wire(Bits(width = history_length))

   // the commit update bundle (we update predictors).
   val commit = Wire(Valid(new BrobEntry(fetch_width)))

   // the (speculative) global history register. Needs to be massaged before usably by the bpd.
   private val r_ghistory = Reg(init = Bits(0, width = history_length))

   // we need to maintain a copy of the commit history, in-case we need to
   // reset it on a pipeline flush/replay.
   private val r_ghistory_commit_copy = Reg(init = Bits(0, width = history_length))


   // Bypass some history modifications before it can be used by the predictor
   // hash functions. For example, "massage" the history for the scenario where
   // the mispredicted branch is not taken AND we're having to refetch the rest
   // of the fetch_packet.
   private val fixed_history = Cat(io.br_resolution.bits.history, io.br_resolution.bits.taken)
   ghistory :=
      Mux(io.flush,
         r_ghistory_commit_copy,
      Mux(io.br_resolution.valid &&
            io.br_resolution.bits.bpd_mispredict &&
            io.br_resolution.bits.new_pc_same_packet,
         io.br_resolution.bits.history,
      Mux(io.br_resolution.valid &&
            io.br_resolution.bits.bpd_mispredict,
         fixed_history,
         r_ghistory)))

   when (io.flush)
   {
      r_ghistory := r_ghistory_commit_copy
   }
   when (io.br_resolution.valid && io.br_resolution.bits.mispredict)
   {
      r_ghistory := fixed_history
   }
   .elsewhen (io.hist_update_spec.valid)
   {
      r_ghistory := Cat(r_ghistory, io.hist_update_spec.bits.taken)
   }

   // -----------------------------------------------

   val brob = Module(new BranchReorderBuffer(fetch_width, NUM_BROB_ENTRIES))
   io.brob <> brob.io.backend
   commit := brob.io.commit_entry

   // TODO XXX add this back in (perhaps just need to initialize mispredicted array?)
//   assert ((~commit.bits.executed.toBits & commit.bits.mispredicted.toBits) === Bits(0),
//      "[BrPredictor] the BROB is marking a misprediction for something that didn't execute.")

   when (commit.valid)
   {
      r_ghistory_commit_copy := Cat(r_ghistory_commit_copy, commit.bits.ctrl.taken.reduce(_|_))
   }

   if (DEBUG_PRINTF)
   {
      printf(" predictor: ghist: 0x%x, r_ghist: 0x%x commit: 0x%x\n"
         , ghistory
         , r_ghistory
         , r_ghistory_commit_copy
         )
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
   io.resp.valid := Bool(false)
   io.resp.bits := new BpdResp().fromBits(Bits(0))
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
          rocket.Random(mod, lfsr)
   }

   io.resp.valid := rand_val
   io.resp.bits := new BpdResp().fromBits(Bits(0))
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
class BrobEntryMetaData(fetch_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val executed     = Vec(fetch_width, Bool()) // mark that a branch executed (and should update the predictor).
   val taken        = Vec(fetch_width, Bool())
   val mispredicted = Vec(fetch_width, Bool()) // did bpd mispredict the br? (aka should we update predictor).
                                                     // Only set for branches, not jumps.
   val brob_idx     = UInt(width = BROB_ADDR_SZ)

   val debug_executed = Bool() // did a br or jalr get executed? verify we're not deallocating an empty entry.
   val debug_rob_idx = UInt(width = ROB_ADDR_SZ)

   override def cloneType: this.type = new BrobEntryMetaData(fetch_width).asInstanceOf[this.type]
}

class BrobEntry(fetch_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val ctrl = new BrobEntryMetaData(fetch_width)
   val info = new BpdResp
   override def cloneType: this.type = new BrobEntry(fetch_width).asInstanceOf[this.type]
}

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

   println ("\tBROB (w=" + fetch_width + ") Size (" + num_entries + ") entries")

   // each entry corresponds to a fetch-packet
   // ROB shouldn't send "deallocate signal" until the entire packet has finished committing.
   // for synthesis quality, break apart ctrl (highly ported) from info, which is stored until commit.
   val entries_ctrl = Reg(Vec(num_entries, new BrobEntryMetaData(fetch_width)))
   val entries_info = SeqMem(num_entries, new BpdResp)

   val head_ptr = Reg(init = UInt(0, log2Up(num_entries)))
   val tail_ptr = Reg(init = UInt(0, log2Up(num_entries)))

   val r_bpd_update = RegNext(io.backend.bpd_update)

   private def GetIdx(addr: UInt) =
      if (fetch_width == 1) UInt(0)
      else (addr >> UInt(log2Ceil(coreInstBytes))) & Fill(log2Ceil(fetch_width), UInt(1))
//      else (addr >> UInt(log2Ceil(coreInstBytes))) & SInt(-1, log2Ceil(fetch_width))

   // -----------------------------------------------
   when (io.backend.allocate.valid)
   {
      entries_ctrl(tail_ptr) := io.backend.allocate.bits.ctrl
      entries_info.write(tail_ptr, io.backend.allocate.bits.info)
      tail_ptr := WrapInc(tail_ptr, num_entries)

      assert (tail_ptr === io.backend.allocate.bits.ctrl.brob_idx,
         "[BROB] allocating the wrong entry.")
   }
   when (io.backend.deallocate.valid)
   {
      head_ptr := WrapInc(head_ptr, num_entries)

      assert (entries_ctrl(head_ptr).debug_executed === Bool(true),
         "[BROB] Committing an entry with no executed branches or jalrs.")
      assert (head_ptr === io.backend.deallocate.bits.brob_idx ,
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

   when (io.backend.flush)
   {
      head_ptr := UInt(0)
      tail_ptr := UInt(0)
   }

   // -----------------------------------------------
   // outputs

   // entries_info is a sequential memory, so buffer the rest of the bundle to match
   io.commit_entry.valid     := RegNext(io.backend.deallocate.valid)
   io.commit_entry.bits.ctrl := RegNext(entries_ctrl(head_ptr))
   io.commit_entry.bits.info := entries_info.read(head_ptr, io.backend.deallocate.valid)

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
            )
         printf("%c\n"
         // chisel3 lacks %s support
            ,  Mux(head_ptr === UInt(i) && tail_ptr === UInt(i), Str("B"),
               Mux(head_ptr === UInt(i),                         Str("H"),
               Mux(tail_ptr === UInt(i),                         Str("T"),
                                                                 Str(" "))))
//            ,  Mux(head_ptr === UInt(i) && tail_ptr === UInt(i), Str("<-HEAD TL"),
//               Mux(head_ptr === UInt(i),                         Str("<-HEAD   "),
//               Mux(tail_ptr === UInt(i),                         Str("<-     TL"),
//                                                                 Str(" "))))
            )
      }
   }
}
