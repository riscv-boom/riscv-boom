//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Branch Checker Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// The branch checker performs the following tasks:
//    - Verify BTB predicted the type and target of instructions correctly.
//    - Catch any JALs and redirect the frontend.
//    - Look at BPD's full prediction and decide to use it if no BTB hit.
// This is purely combinational logic.
// If an error is found, redirect the front-end to refetch and correct the
// misprediction.
// NOTE: Incoming signals may be garbage (if f2_valid not true); consumer will
// have to handle that scenario.

package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.bpu._
import boom.common._

/**
 * Combinational logic to verify that the BoomBTB predicted correctly. This chooses
 * between the BrPredictor or the BoomBTB. Also catch JALs that have not been
 * predicted by the BTB.
 *
 * @param fetch_width # of instructions fetched
 */
class BranchChecker(fetch_width: Int)(implicit p: Parameters) extends BoomModule()(p)
   with HasL1ICacheBankedParameters
{
   val io = IO(new Bundle
   {
      val valid        = Input(Bool())                   // are the inputs valid?
      val inst_mask    = Input(Vec(fetch_width, Bool())) // valid instruction mask from I$

      val is_br        = Input(Vec(fetch_width, Bool())) // is it a type of instruction?
      val is_jal       = Input(Vec(fetch_width, Bool()))
      val is_jalr      = Input(Vec(fetch_width, Bool()))
      val is_call      = Input(Vec(fetch_width, Bool()))
      val is_ret       = Input(Vec(fetch_width, Bool()))
      val is_rvc       = Input(Vec(fetch_width, Bool()))

      val br_targs     = Input(Vec(fetch_width, UInt(vaddrBitsExtended.W))) // calculated targets from branch decode
      val jal_targs    = Input(Vec(fetch_width, UInt(vaddrBitsExtended.W)))

      val fetch_pc     = Input(UInt(vaddrBitsExtended.W))
      val aligned_pc   = Input(UInt(vaddrBitsExtended.W))

      val btb_resp     = Flipped(Valid(new BoomBTBResp)) // responses from the btb/bpd
      val bpd_resp     = Flipped(Valid(new BpdResp))

      val btb_update   = Valid(new BoomBTBUpdate) // send a correctional update to the btb/ras
      val ras_update   = Valid(new RasUpdate)

      val resp         = Valid(new PCReq) // redirection of the frontend based on results
      val resp_cfi_idx = Output(UInt(log2Ceil(fetchWidth).W)) // where is cfi we are predicting?
   })

   // Did the BTB mispredict the cfi type?
   val wrong_cfi = WireInit(false.B)
   // Did the BTB mispredict the cfi target?
   val wrong_target = WireInit(false.B)

   val btb_cfi_idx = io.btb_resp.bits.cfi_idx
   val btb_target = io.btb_resp.bits.target
   val btb_cfi_type = io.btb_resp.bits.cfi_type
   val bpd_predicted_taken = io.bpd_resp.valid && io.bpd_resp.bits.takens(io.btb_resp.bits.cfi_idx)

   // determine based on the btb resp if the btb gave a wrong target or wrong cfi type
   when (io.btb_resp.valid)
   {
      when (btb_cfi_type === CfiType.BRANCH && (io.btb_resp.bits.taken || bpd_predicted_taken))
      {
         wrong_cfi := !io.is_br(btb_cfi_idx)
         wrong_target := io.br_targs(btb_cfi_idx) =/= btb_target
      }
      .elsewhen (btb_cfi_type === CfiType.JAL)
      {
         wrong_cfi := !io.is_jal(btb_cfi_idx)
         wrong_target := io.jal_targs(btb_cfi_idx) =/= btb_target
      }
      .elsewhen (btb_cfi_type === CfiType.JALR)
      {
         wrong_cfi := !io.is_jalr(btb_cfi_idx)
      }
      .otherwise
      {
         wrong_cfi := btb_cfi_type === CfiType.NONE && io.btb_resp.bits.taken
         when (io.valid)
         {
            assert (btb_cfi_type =/= CfiType.NONE, "[br-checker] BTB predicted on a non-cfi type")
         }
      }
   }

   // get the PC + X to fetch from
   val nextline_pc = nextFetchStart(io.aligned_pc)

   // did the btb actually predict?
   val btb_hit  = io.btb_resp.valid
   // was the btb wrong (technically this also includes the bpd since the btb is used to get the target)
   val btb_was_wrong = io.btb_resp.valid && (wrong_cfi || wrong_target || !io.inst_mask(btb_cfi_idx))

   // check to see if btb predicted instruction after a jal (when it should've predicted the jal)
   //   if so then adjust for the jal
   val jal_idx  = PriorityEncoder(io.is_jal.asUInt)
   val jal_wins = io.is_jal.reduce(_|_) &&
                  (!btb_hit || btb_was_wrong || (jal_idx < btb_cfi_idx) || !io.btb_resp.bits.taken)

   //-------------------------------------------------------------
   // Perform redirection

   // Redirect if:
   //    - JAL comes before BTB's cfi_idx
   //       * kill everything behind JAL -- including BTB's predinfo
   //          - BTB should have predicted the JAL
   //    - BTB was wrong
   //       * if JAL, take JAL (if valid instructions available)
   //       * if !JAL, request nextline (set all masks to valid)
   //    - No JAL, BTB correct
   //       * do nothing

   io.resp.valid := jal_wins || btb_was_wrong
   io.resp.bits.addr := Mux(jal_wins, io.jal_targs(jal_idx), nextline_pc)
   // mask out instructions after predicted cfi
   io.resp_cfi_idx := Mux(jal_wins, jal_idx, (fetchWidth-1).U)


   //-------------------------------------------------------------
   // Perform updates

   // update the BTB for jumps it missed
   // TODO XXX also allow us to clear bad BTB entries when btb is wrong
   io.btb_update.valid         := jal_wins
   io.btb_update.bits.pc       := io.fetch_pc
   io.btb_update.bits.target   := io.jal_targs(jal_idx)
   io.btb_update.bits.taken    := true.B
   io.btb_update.bits.cfi_idx  := jal_idx
   io.btb_update.bits.bpd_type := Mux(io.is_call(jal_idx), BpredType.CALL, BpredType.JUMP) // can use RA or not
   io.btb_update.bits.cfi_type := CfiType.JAL

   // for critical path reasons, remove dependence on bpu_request to ras_update
   val jal_may_win = io.is_jal.reduce(_|_) && (!btb_hit || btb_was_wrong || jal_idx < btb_cfi_idx)
   io.ras_update.valid            := jal_may_win && io.is_call(jal_idx)
   io.ras_update.bits.is_call     := true.B
   io.ras_update.bits.is_ret      := false.B
   io.ras_update.bits.return_addr := (io.aligned_pc
                                     + (jal_idx << log2Ceil(fetchBytes))
                                     + Mux(io.is_rvc(jal_idx), 2.U, 4.U))
}
