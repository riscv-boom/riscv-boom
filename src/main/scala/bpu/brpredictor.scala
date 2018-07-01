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
//    - The sub-class of BrPredictor must flop its own output and set io.resp
//       itself for F3.
//

package boom.bpu

import chisel3._
import chisel3.util.{Valid, Decoupled, RegEnable, log2Ceil, Cat, LFSR16}
import chisel3.experimental.withReset
import freechips.rocketchip.config.{Parameters, Field}

import freechips.rocketchip.util.{Str}
import freechips.rocketchip.rocket.RocketCoreParams
import boom.common._
import boom.exu._
import boom.exu.BranchUnitResp
import boom.util.ElasticReg


// This is the response packet from the branch predictor. The predictor is
// expecting to receive it back when it needs to perform an update.
class BpdResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val takens = UInt(rvcFetchWidth.W)

   // Roughly speaking, track the outcome of the last N branches.
   // The purpose of these is for resetting the global history on a branch
   // mispredict -- they are NOT the history used to index the branch predictor,
   // as speculative updates to the global history will have occurred between
   // the branch predictor is indexed and when the branch makes its own
   // prediction and update to the history.
   val history = UInt(GLOBAL_HISTORY_LENGTH.W)

   // The info field stores the response information from the branch predictor.
   // The response is stored (conceptually) in the ROB and is returned to the
   // predictor during the Commit stage to aid in updating the predictor. Each
   // predictor (and its configuration) changes the amount of information it
   // needs to store, and so we need to ask the predictor (in parameters.scala)
   // how many bits of info it requires.
   val info = UInt(BPD_INFO_SIZE.W)
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
   val fetch_pc = UInt(vaddrBits.W)
   val history     = UInt(GLOBAL_HISTORY_LENGTH.W)
   val mispredict = Bool()
   val miss_cfi_idx = UInt(log2Ceil(rvcFetchWidth).W) // if mispredict, this is the cfi_idx of miss.
   val taken = Bool()

   // Give the bpd back the information it sent out when it made a prediction.
   val info = UInt(BPD_INFO_SIZE.W)
}

class RestoreHistory(implicit p: Parameters) extends BoomBundle()(p)
{
   val history = UInt(GLOBAL_HISTORY_LENGTH.W)
   val taken = Bool()
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------

abstract class BrPredictor(val history_length: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new BoomBundle()(p)
   {
      val capture = Input(Bool())
      val s3_valid = Input(Bool())

      // The PC to predict on.
      // req.valid is false if stalling (aka, we won't read and use results).
      // req.bits.addr is available on cycle S0.
      // resp is expected on cycle S2.
      val req = Flipped(Valid(new PCReq))

      // Update from the FTQ during commit.
      val commit = Flipped(Valid(new BpdUpdate))

      // Not ready to dequeue F2 buffer (I$ did not provide a valid response in F2).
      val f2_stall = Input(Bool())

      // F2 stage is valid (the I$ resp is valid so the F3 buffer can accept an entry).
      val f2_valid = Input(Bool())

      // our prediction. Assert "valid==true" if we want our prediction to be honored.
      // For a tagged predictor, valid==true means we had a tag hit and trust our prediction.
      // For an un-tagged predictor, valid==true should probably only be if a branch is predicted taken.
      // This has an effect on whether to override the BTB's prediction.
      // Provided in the beginning of the F3 stage.
      val resp = Decoupled(new BpdResp)

      // A BIM table is shared with the BTB and accessed in F1. Let's use this as our base predictor, if desired.
      val f2_bim_resp = Flipped(Valid(new BimResp))

      val f3_is_br = Input(Vec(rvcFetchWidth, Bool()))

      // Restore history on a F2 redirection (and clear out appropriate state).
      // Don't provide history since we have it ourselves.
      val f2_redirect = Input(Bool())

      // Restore history on a F4 redirection (and clear out appropriate state).
      // Don't provide history since we have it ourselves.
      // Taken tells us if the redirect is to take a new path or to fix up a
      // previous prediction and "not take" the branch.
      val f4_redirect = Input(Bool())
      val f4_taken = Input(Bool())

      // Restore history on a branch mispredict or pipeline flush.
      val ftq_restore = Flipped(Valid(new RestoreHistory))

      // The I$ failed to succeed in S2 -- replay F2 (place into F0).
      val f2_replay = Input(Bool())

      // Clear/flush inflight state in the entire front-end.
      val fe_clear = Input(Bool())

      // TODO provide a way to reset/clear the predictor state.
      val do_reset = Input(Bool())

      // privilege-level (allow predictor to change behavior in different privilege modes).
      val status_prv = Input(UInt(freechips.rocketchip.rocket.PRV.SZ.W))
   })

   val r_f1_fetchpc = RegEnable(io.req.bits.addr, io.req.valid)

   // The global history register  that will be hashed with the fetch-pc to compute tags and indices for our branch predictors.
   val f0_history   = WireInit(0.U(history_length.W))
   val new_history  = WireInit(0.U(history_length.W))
   val r_f1_history = RegInit(0.U(history_length.W))
   val r_f2_history = RegInit(0.U(history_length.W))
   val r_f4_history = RegInit(0.U(history_length.W))


   // match the other ERegs in the FrontEnd.
   val q_f3_history = withReset(reset.toBool || io.fe_clear || io.f4_redirect)
      { Module(new ElasticReg(UInt(history_length.W))) }

   require (history_length == GLOBAL_HISTORY_LENGTH)

   //************************************************
   // Branch Prediction (F0 Stage)

   private def UpdateHistoryHash(old: UInt, addr: UInt): UInt =
   {
      val ret = WireInit(0.U(history_length.W))

      //ret := ((addr >> 4.U) & 0xf.U) | (old << 4.U) -- for debugging
      val pc = addr >> log2Ceil(minCoreInstBytes)
      val foldpc = (pc >> 17) ^ pc
      val shamt = 2
      val sz0 = 6
      if (history_length < (sz0*2+1))
      {
         (old << 1.U) | (foldpc(5) ^ foldpc(6))
      }
      else
      {
         val o0 = old(sz0-1,0)
         val o1 = old(2*sz0-1,sz0)

         val h0 = foldpc(sz0-1,0)
         val h1 = o0
         val h2 = (o1 ^ (o1 >> (sz0/2).U))(sz0/2-1,0)
         val min = h0.getWidth + h1.getWidth
         ret := Cat(old(history_length-1, min), h2, h1, h0)
      }
      ret
   }


   // As predictions come in (or pipelines are flushed/replayed), we need to correct the history.
   f0_history :=
      Mux(io.ftq_restore.valid,
         io.ftq_restore.bits.history,
      Mux(io.f4_redirect,
         r_f4_history,
         r_f1_history)) // valid for f2_redirect

   // Hash target into history.
   new_history := UpdateHistoryHash(f0_history, io.req.bits.addr)


   //************************************************
   // Branch Prediction (F1 Stage)

   val f1_valid = RegNext(io.req.valid) && !(io.fe_clear || io.f2_redirect || io.f4_redirect)

   // Do we hash in a new history?
   val use_new_hash =
      (io.ftq_restore.valid && io.ftq_restore.bits.taken) ||
      (io.f4_redirect && io.f4_taken) ||
      io.f2_redirect

   r_f1_history :=
      Mux(io.f2_replay,
         r_f2_history,
      Mux(io.ftq_restore.valid && !io.ftq_restore.bits.taken,
         io.ftq_restore.bits.history,
      Mux(io.f4_redirect && !io.f4_taken,
         r_f4_history,
      Mux(use_new_hash,
         new_history,
         r_f1_history))))


   //assert (!io.f2_redirect || r_f2_history === r_f1_history,
   //   "[bpd] if a F2 redirect occurs, F2-hist should equal F1-hist.")


   //************************************************
   // Branch Prediction (F2 Stage)

   when (!io.f2_replay)
   {
      r_f2_history := r_f1_history
   }


   q_f3_history.io.enq.valid := io.f2_valid
   q_f3_history.io.enq.bits  := r_f2_history

   //assert (q_f3_history.io.enq.ready === !io.f2_stall)
   val take_history_reg = Wire(Bool())
   val f2f3_history_reg = RegEnable(r_f2_history, take_history_reg)
   take_history_reg := io.capture
   when (io.s3_valid) {
      q_f3_history.io.enq.bits := f2f3_history_reg   
   }


   //************************************************
   // Branch Prediction (F3 Stage)

   io.resp.bits.history := q_f3_history.io.deq.bits
   q_f3_history.io.deq.ready := io.resp.ready


   //************************************************
   // Branch Prediction (F4 Stage)

   when (io.resp.ready)
   {
      r_f4_history := q_f3_history.io.deq.bits
   }


   //************************************************
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
      val enableCondBrPredictor = boomParams.enableBranchPredictor

      var br_predictor: BrPredictor = null

      if (enableCondBrPredictor && boomParams.bpdBaseOnly.isDefined && boomParams.bpdBaseOnly.get.enabled)
      {
         br_predictor = Module(new BaseOnlyBrPredictor)
      }
      else if (enableCondBrPredictor && boomParams.gshare.isDefined && boomParams.gshare.get.enabled)
      {
         br_predictor = Module(new GShareBrPredictor(
            history_length = boomParams.gshare.get.history_length))
      }
      else if (enableCondBrPredictor && boomParams.tage.isDefined && boomParams.tage.get.enabled)
      {
         br_predictor = Module(new TageBrPredictor(
            num_tables = boomParams.tage.get.num_tables,
            table_sizes = boomParams.tage.get.table_sizes,
            history_lengths = boomParams.tage.get.history_lengths,
            tag_sizes = boomParams.tage.get.tag_sizes,
            cntr_sz = boomParams.tage.get.cntr_sz,
            ubit_sz = boomParams.tage.get.ubit_sz))
      }
      else if (enableCondBrPredictor && p(RandomBpdKey).enabled)
      {
         br_predictor = Module(new RandomBrPredictor)
      }
      else
      {
         br_predictor = Module(new NullBrPredictor(
            history_length = 1))
      }

      br_predictor
   }
}


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// Act as a "null" branch predictor (it makes no predictions).
class NullBrPredictor(
   history_length: Int = 12
   )(implicit p: Parameters) extends BrPredictor(history_length)(p)
{
   override def toString: String = "  Building (0 kB) Null Predictor (never predict)."
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
   )(implicit p: Parameters) extends BrPredictor(history_length = 1)(p)
{
   override def toString: String = "  Building Random Branch Predictor."
   private val rand_val = RegInit(false.B)
   rand_val := ~rand_val
   private var lfsr= LFSR16(true.B)
   def rand(width: Int) = {
        lfsr = lfsr(lfsr.getWidth-1,1)
        val mod = (1 << width) - 1
          freechips.rocketchip.util.Random(mod, lfsr)
   }

   io.resp.valid := rand_val
   io.resp.bits.takens := rand(rvcFetchWidth)
}

