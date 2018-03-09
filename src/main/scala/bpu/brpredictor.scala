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

// provide an abstract class for branch predictors. Provides support by
// maintaining the global history.
//
// Notes:
//    - TODO discuss timings of the expected sub-class.
//

package boom

import Chisel._
import chisel3.core.withReset
import freechips.rocketchip.config.{Parameters, Field}

import freechips.rocketchip.util.{Str}
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
   // how many bits of info it requires.
   val info = UInt(width = BPD_INFO_SIZE)
}

// Update comes from the FTQ after Commit.
class BpdUpdate(implicit p: Parameters) extends BoomBundle()(p)
{
   // valid: an FTQ entry is being committed/deallocated.
   // is_br: a branch was resolved
   // the fetch pc (points to start of the fetch packet)
   // which word in the fetch packet does the update correspond to?
   // history: what was the history our branch saw?
   // cfi_idx: what is the index of the control-flow-instruction? (low-order PC bits).
   // taken: was the branch taken?
   val fetch_pc = UInt(width = vaddrBits)
   val history     = UInt(width = GLOBAL_HISTORY_LENGTH)
   val mispredict = Bool()
   val miss_cfi_idx = UInt(width=log2Up(fetchWidth).W) // if mispredict, this is the cfi_idx of miss.
   val taken = Bool()

   // Give the bpd back the information it sent out when it made a prediction.
   val info = UInt(width = BPD_INFO_SIZE)
}

class RestoreHistory(implicit p: Parameters) extends BoomBundle()(p)
{
   val history = UInt(width = GLOBAL_HISTORY_LENGTH)
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------

abstract class BrPredictor(
   fetch_width: Int,
   val history_length: Int
   )(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new BoomBundle()(p)
   {
      // The PC to predict on.
      // req.valid is false if stalling (aka, we won't read and use results).
      // req.bits.addr is available on cycle S0.
      // resp is expected on cycle S2.
      val req = Valid(new PCReq).flip

      // Update from the FTQ during commit.
      val commit = Valid(new BpdUpdate).flip

      // Not ready to dequeue F2 buffer (I$ did not provide a valid response in F2).
      val f2_stall = Bool(INPUT)

      // F2 stage is valid (the I$ resp is valid so the F3 buffer can accept an entry).
      val f2_valid = Bool(INPUT)

      // our prediction. Assert "valid==true" if we want our prediction to be honored.
      // For a tagged predictor, valid==true means we had a tag hit and trust our prediction.
      // For an un-tagged predictor, valid==true should probably only be if a branch is predicted taken.
      // This has an effect on whether to override the BTB's prediction.
      // Provided in the beginning of the F3 stage.
      val resp = Decoupled(new BpdResp)

      // A BIM table is shared with the BTB and accessed in F1. Let's use this as our base predictor, if desired.
      val f2_bim_resp = Valid(new BimResp).flip

      // Restore history on a F2 redirection (and clear out appropriate state).
      // Don't provide history since we have it ourselves.
      val f2_redirect = Bool(INPUT)

      // Restore history on a F4 redirection (and clear out appropriate state).
      // Don't provide history since we have it ourselves.
      val f4_redirect = Bool(INPUT)

      // Restore history on a branch mispredict or pipeline flush.
      val ftq_restore = Valid(new RestoreHistory).flip

      // The I$ failed to succeed in S2 -- replay F2 (place into F0).
      val f2_replay = Bool(INPUT)

      // Clear/flush inflight state in the entire front-end.
      val fe_clear = Bool(INPUT)

      // TODO provide a way to reset/clear the predictor state.
      val do_reset = Bool(INPUT)

      // privilege-level (allow predictor to change behavior in different privilege modes).
      val status_prv = UInt(INPUT, width = freechips.rocketchip.rocket.PRV.SZ)
   })

   val r_f1_fetchpc = RegEnable(io.req.bits.addr, io.req.valid)

   // The global history register  that will be hashed with the fetch-pc to compute tags and indices for our branch predictors.
   val f0_history   = Wire(UInt(width=history_length.W))
   val next_f1_history = Wire(UInt(width=history_length.W))
   val r_f1_history = Reg(init=0.asUInt(width=history_length.W))
   val r_f2_history = Reg(init=0.asUInt(width=history_length.W))
   val r_f4_history = Reg(init=0.asUInt(width=history_length.W))

   // Let base-class predictor set these wires, then we can handle the queuing of the bundle.
   val f2_resp = Wire(Valid(new BpdResp))

   // match the other ERegs in the FrontEnd.
   val q_f3_resp = withReset(reset || io.fe_clear || io.f4_redirect) { Module(new ElasticReg(Valid(new BpdResp))) }

   require (history_length == GLOBAL_HISTORY_LENGTH)

   //************************************************
   // Branch Prediction (F0 Stage)

   private def UpdateHistoryHash(old: UInt, addr: UInt): UInt =
   {
      val ret = Wire(UInt(width=history_length))
      val shamt = 2
      val fold = (addr >> log2Ceil(coreInstBytes) ^ addr >> 5 ^ addr >> 17)(shamt-1,0)
      val h0 = fold
      ret := Cat(old(history_length-1, shamt), old(shamt-1,0), fold)
      ret
   }


   // as predictions come in (or pipelines are flushed/replayed), we need to correct the history.
   f0_history :=
      Mux(io.ftq_restore.valid,
         io.ftq_restore.bits.history,
      Mux(io.f2_redirect,
         r_f2_history,
      Mux(io.f4_redirect,
         r_f4_history,
         r_f1_history)))

    next_f1_history := UpdateHistoryHash(f0_history, io.req.bits.addr)


   //************************************************
   // Branch Prediction (F1 Stage)

   val f1_valid = RegNext(io.req.valid) && !(io.fe_clear || io.f2_redirect || io.f4_redirect)

   r_f1_history :=
      Mux(io.f2_replay,
         r_f2_history,
      Mux(io.req.valid, // forward progress of pipeline
         next_f1_history,
         r_f1_history))

   //************************************************
   // Branch Prediction (F2 Stage)

   when (!io.f2_replay)
   {
      r_f2_history := r_f1_history
   }


   f2_resp.bits.history := r_f2_history


   q_f3_resp.io.enq.valid := io.f2_valid
   q_f3_resp.io.enq.bits  := f2_resp

   assert (q_f3_resp.io.enq.ready === !io.f2_stall)


   //************************************************
   // Branch Prediction (F3 Stage)

   io.resp.valid := q_f3_resp.io.deq.valid && q_f3_resp.io.deq.bits.valid
   io.resp.bits := q_f3_resp.io.deq.bits.bits
   q_f3_resp.io.deq.ready := io.resp.ready

   //************************************************
   // Branch Prediction (F4 Stage)

   when (io.resp.ready)
   {
      r_f4_history := q_f3_resp.io.deq.bits.bits.history
   }

}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// Return the desired branch predictor based on the provided parameters.
object BrPredictor
{
   def apply(tileParams: freechips.rocketchip.tile.TileParams, boomParams: BoomCoreParams)
      (implicit p: Parameters): BrPredictor =
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
            cntr_sz = boomParams.tage.get.cntr_sz,
            ubit_sz = boomParams.tage.get.ubit_sz))
      }
      else if (enableCondBrPredictor && boomParams.gshare.isDefined && boomParams.gshare.get.enabled)
      {
         br_predictor = Module(new GShareBrPredictor(
            fetch_width = fetch_width,
            history_length = boomParams.gshare.get.history_length))
      }
      else if (enableCondBrPredictor && boomParams.bpdBaseOnly.isDefined && boomParams.bpdBaseOnly.get.enabled)
      {
         br_predictor = Module(new BaseOnlyBrPredictor(
            fetch_width = fetch_width))
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


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// Act as a "null" branch predictor (it makes no predictions).
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

