//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Branch Predictor (abstract class)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// provide an abstract class for branch predictors. Provides support by
// maintaining the global history.
//
// Notes:
//    - The sub-class of BrPredictor must flop its own output and set io.resp
//       itself for F3.

package boom.bpu

import chisel3._
import chisel3.util._
import chisel3.core.withReset

import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.util.{Str}
import freechips.rocketchip.rocket.{RocketCoreParams}

import boom.common._
import boom.exu._
import boom.exu.{BranchUnitResp}
import boom.util.{ElasticReg}

/**
 * This is the response packet from the branch predictor. The predictor is
 * expecting to receive it back when it needs to perform an update.
 */
class BpdResp(implicit p: Parameters) extends BoomBundle
{
  val takens = UInt(fetchWidth.W)

  // Roughly speaking, track the outcome of the last N branches.
  // The purpose of these is for resetting the global history on a branch
  // mispredict -- they are NOT the history used to index the branch predictor,
  // as speculative updates to the global history will have occurred between
  // the branch predictor is indexed and when the branch makes its own
  // prediction and update to the history.
  val history = UInt(globalHistoryLength.W)

  // The info field stores the response information from the branch predictor.
  // The response is stored (conceptually) in the ROB and is returned to the
  // predictor during the Commit stage to aid in updating the predictor. Each
  // predictor (and its configuration) changes the amount of information it
  // needs to store, and so we need to ask the predictor (in parameters.scala)
  // how many bits of info it requires.
  val info = UInt(bpdInfoSize.W)
}

/**
 * Update comes from the FTQ after Commit.
 */
class BpdUpdate(implicit p: Parameters) extends BoomBundle
{
  // valid: an FTQ entry is being committed/deallocated.
  // is_br: a branch was resolved
  // the fetch pc (points to start of the fetch packet)
  // which word in the fetch packet does the update correspond to?
  // history: what was the history our branch saw?
  // cfi_idx: what is the index of the control-flow-instruction? (low-order PC bits).
  // taken: was the branch taken?
  val fetch_pc     = UInt(vaddrBits.W)
  val history      = UInt(globalHistoryLength.W)
  val mispredict   = Bool()
  val miss_cfi_idx = UInt(log2Ceil(fetchWidth).W) // if mispredict, this is the cfi_idx of miss.
  val taken        = Bool()

  // Give the bpd back the information it sent out when it made a prediction.
  val info = UInt(bpdInfoSize.W)
}

/**
 * History to restore when there is a branch mispredict or pipeline flush
 */
class RestoreHistory(implicit p: Parameters) extends BoomBundle
{
  val history = UInt(globalHistoryLength.W)
  val taken = Bool()
}

/**
 * Abstract top level branch predictor class. Exposes the necessary signals for different
 * branch predictor types to be instantiated into BOOM.
 *
 * @param historyLength length of the GHR
 */
abstract class BoomBrPredictor(
   val historyLength: Int)(implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle {
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

    val f3_is_br = Input(Vec(fetchWidth, Bool()))

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

  /**
   * Hash input history with an address
   */
  private def UpdateHistoryHash(old: UInt, addr: UInt): UInt = {
    val ret = Wire(UInt(historyLength.W))
    ret := DontCare

    //ret := ((addr >> 4.U) & 0xf.U) | (old << 4.U) -- for debugging
    val pc = addr >> log2Ceil(coreInstBytes)
    val foldpc = (pc >> 17) ^ pc
    val shamt = 2
    val sz0 = 6
    if (historyLength < (sz0*2+1)) {
      (old << 1.U) | (foldpc(5) ^ foldpc(6))
    } else {
      val o0 = old(sz0-1,0)
      val o1 = old(2*sz0-1,sz0)

      val h0 = foldpc(sz0-1,0)
      val h1 = o0
      val h2 = (o1 ^ (o1 >> (sz0/2).U))(sz0/2-1,0)
      val min = h0.getWidth + h1.getWidth
      ret := Cat(old(historyLength-1, min), h2, h1, h0)
      ret
    }
  }

  val r_f1_fetchpc = RegEnable(io.req.bits.addr, io.req.valid)

  // The global history register  that will be hashed with the fetch-pc to
  // compute tags and indices for our branch predictors.
  val f0_history   = Wire(UInt(historyLength.W))
  val new_history  = Wire(UInt(historyLength.W))
  val r_f1_history = RegInit(0.U(historyLength.W))
  val r_f2_history = RegInit(0.U(historyLength.W))
  val r_f4_history = RegInit(0.U(historyLength.W))

  // match the other ERegs in the FrontEnd.
  val q_f3_history = withReset(reset.asBool || io.fe_clear || io.f4_redirect)
     { Module(new ElasticReg(UInt(historyLength.W))) }

  require (historyLength == globalHistoryLength)

  //************************************************
  // Branch Prediction (F0 Stage)

  // As predictions come in (or pipelines are flushed/replayed), we need to correct the history.
  f0_history := Mux(io.ftq_restore.valid,
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

  r_f1_history := Mux(io.f2_replay,
                      r_f2_history,
                      Mux(io.ftq_restore.valid && !io.ftq_restore.bits.taken,
                          io.ftq_restore.bits.history,
                          Mux(io.f4_redirect && !io.f4_taken,
                              r_f4_history,
                              Mux(use_new_hash,
                                  new_history,
                                  r_f1_history))))

  assert (!io.f2_redirect || r_f2_history === r_f1_history,
     "[bpd] if a F2 redirect occurs, F2-hist should equal F1-hist.")

  //************************************************
  // Branch Prediction (F2 Stage)

  when (!io.f2_replay) {
     r_f2_history := r_f1_history
  }

  q_f3_history.io.enq.valid := io.f2_valid
  q_f3_history.io.enq.bits  := r_f2_history

  assert (q_f3_history.io.enq.ready === !io.f2_stall)

  //************************************************
  // Branch Prediction (F3 Stage)

  io.resp.bits.history := q_f3_history.io.deq.bits
  q_f3_history.io.deq.ready := io.resp.ready

  //************************************************
  // Branch Prediction (F4 Stage)

  when (io.resp.ready) {
     r_f4_history := q_f3_history.io.deq.bits
  }
}

/**
 * Factory object to create a branch predictor
 */
object BoomBrPredictor
{
  /**
   * Create a specific type of branch predictor based on the parameters specified
   *
   * @param boomParams general boom core parameters that determine the BPU type
   * @return a BoomBrPredictor instance determined by the input parameters
   */
  def apply(boomParams: BoomCoreParams, bankBytes: Int)(implicit p: Parameters): BoomBrPredictor = {
    val boomParams: BoomCoreParams = p(freechips.rocketchip.tile.TileKey).core.asInstanceOf[BoomCoreParams]

    val enableCondBrPredictor = boomParams.enableBranchPredictor

    var br_predictor: BoomBrPredictor = null

    val useBaseOnly = boomParams.bpdBaseOnly.isDefined && boomParams.bpdBaseOnly.get.enabled
    val useGshare   = boomParams.gshare.isDefined && boomParams.gshare.get.enabled
    val useTage     = boomParams.tage.isDefined && boomParams.tage.get.enabled
    val useRandom   = boomParams.bpdRandom.isDefined && boomParams.bpdRandom.get.enabled

    // a BPU must be defined with the enable flag
    require(!enableCondBrPredictor || (Seq(useBaseOnly, useGshare, useTage, useRandom).count(_ == true) == 1))

    // select BPU based on parameters specified
    if (enableCondBrPredictor) {
      if (useBaseOnly) {
        br_predictor = Module(new BaseOnlyBrPredictor())
      } else if (useGshare) {
        br_predictor = Module(new GShareBrPredictor(
          historyLength = boomParams.gshare.get.historyLength,
          bankBytes = bankBytes))
      } else if (useTage) {
        br_predictor = Module(new TageBrPredictor(
          numTables = boomParams.tage.get.numTables,
          tableSizes = boomParams.tage.get.tableSizes,
          historyLengths = boomParams.tage.get.historyLengths,
          tagSizes = boomParams.tage.get.tagSizes,
          cntrSz = boomParams.tage.get.cntrSz,
          ubitSz = boomParams.tage.get.ubitSz))
      } else if (useRandom) {
         br_predictor = Module(new RandomBrPredictor())
      }
    } else {
      br_predictor = Module(new NullBrPredictor(
        historyLength = 1))
    }

    br_predictor
  }
}
