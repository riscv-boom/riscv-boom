//******************************************************************************
// Copyright (c) 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Branch Checker Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


package boom.ifu

import Chisel._
import freechips.rocketchip.config.Parameters
import boom.bpu._
import boom.common._


// The branch checker performs the following tasks:
//    - Verify BTB predicted the type and target of instructions correctly.
//    - Catch any JALs and redirect the frontend.
//    - Look at BPD's full prediction and decide to use it if no BTB hit.
// This is purely combinational logic.
// If an error is found, redirect the front-end to refetch and correct the
// misprediction.
// NOTE: Incoming signals may be garbage (if f2_valid not true); consumer will
// have to handle that scenario.
class BranchChecker(fetch_width: Int)(implicit p: Parameters) extends BoomModule()(p)
   with HasL1ICacheBankedParameters
{
   val io = IO(new Bundle
   {
      val req           = Valid(new PCReq)

      val valid         = Bool(INPUT)                      // are the inputs valid?
      val inst_mask     = Vec(fetch_width, Bool()).asInput // valid instruction mask from I$
      val is_br         = Vec(fetch_width, Bool()).asInput
      val is_jal        = Vec(fetch_width, Bool()).asInput
      val is_jr         = Vec(fetch_width, Bool()).asInput
      val is_call       = Vec(fetch_width, Bool()).asInput

      val br_targs      = Vec(fetch_width, UInt(width=vaddrBitsExtended)).asInput
      val jal_targs     = Vec(fetch_width, UInt(width=vaddrBitsExtended)).asInput

      val fetch_pc      = UInt(INPUT, width=vaddrBitsExtended)
      val aligned_pc    = UInt(INPUT, width=vaddrBitsExtended)

      val btb_resp      = Valid(new BoomBTBResp).flip
      val bpd_resp      = Valid(new BpdResp).flip

      val btb_update    = Valid(new BoomBTBUpdate)
      val ras_update    = Valid(new RasUpdate)

      val req_cfi_idx   = UInt(OUTPUT, width = log2Up(fetchWidth)) // where is cfi we are predicting?
   })

   // Did the BTB mispredict the cfi type?
   // Did the BTB mispredict the cfi target?
   // Did the BTB predict a masked-off instruction?
   val wrong_cfi = Wire(init = false.B)
   val wrong_target = Wire(init = false.B)

   val btb_idx = io.btb_resp.bits.cfi_idx
   val btb_target = io.btb_resp.bits.target // TODO uncomment .sextTo(vaddrBitsExtended)
   val bpd_predicted_taken = io.bpd_resp.valid && io.bpd_resp.bits.takens(io.btb_resp.bits.cfi_idx)

   when (io.btb_resp.valid)
   {
      when (io.btb_resp.bits.cfi_type === CfiType.branch && (io.btb_resp.bits.taken || bpd_predicted_taken))
      {
         wrong_cfi := !io.is_br(btb_idx)
         wrong_target := io.br_targs(btb_idx) =/= btb_target
      }
      .elsewhen (io.btb_resp.bits.cfi_type === CfiType.jal)
      {
         wrong_cfi := !io.is_jal(btb_idx)
         wrong_target := io.jal_targs(btb_idx) =/= btb_target
      }
      .elsewhen (io.btb_resp.bits.cfi_type === CfiType.jalr)
      {
         wrong_cfi := !io.is_jr(btb_idx)
      }
      .otherwise
      {
         wrong_cfi := io.btb_resp.bits.cfi_type === CfiType.none && io.btb_resp.bits.taken
         when (io.valid)
         {
            assert (io.btb_resp.bits.cfi_type =/= CfiType.none, "[fetch] predicted on a non-cfi type.")
         }
      }
   }

   val nextline_pc = nextFetchStart(io.aligned_pc)

   val btb_was_wrong = io.btb_resp.valid && (wrong_cfi || wrong_target || !io.inst_mask(btb_idx))

   val jal_idx = PriorityEncoder(io.is_jal.asUInt)
   val btb_hit  = io.btb_resp.valid
   val jal_wins = io.is_jal.reduce(_|_) &&
      (!btb_hit ||
      btb_was_wrong ||
      (jal_idx < btb_idx) ||
      !io.btb_resp.bits.taken)

   //-------------------------------------------------------------
   // Perform redirection

   // Redirect if:
   //    - JAL comes before BTB's cfi_idx
   //       * kill everything behind JAL -- including BTB's predinfo
   //    - BTB was wrong
   //       * if JAL, take JAL (if valid instructions available)
   //       * if !JAL, request nextline (set all masks to valid).
   //    - No JAL, BTB correct
   //       * do nothing

   io.req.valid := jal_wins || btb_was_wrong
   io.req.bits.addr := Mux(jal_wins, io.jal_targs(jal_idx), nextline_pc)
   // Help mask out instructions after predicted cfi.
   io.req_cfi_idx := Mux(jal_wins, jal_idx, UInt(fetchWidth-1))


   //-------------------------------------------------------------
   // Perform updates

   // update the BTB for jumps it missed.
   // TODO XXX also allow us to clear bad BTB entries when btb is wrong.
   io.btb_update.valid := jal_wins
   io.btb_update.bits.pc := io.fetch_pc
   io.btb_update.bits.target := io.jal_targs(jal_idx)
   io.btb_update.bits.taken := true.B
   io.btb_update.bits.cfi_pc := jal_idx << log2Up(coreInstBytes)
   io.btb_update.bits.bpd_type := Mux(io.is_call(jal_idx), BpredType.call, BpredType.jump)
   io.btb_update.bits.cfi_type := CfiType.jal

   // for critical path reasons, remove dependence on bpu_request to ras_update.
   val jal_may_win = io.is_jal.reduce(_|_) && (!btb_hit || btb_was_wrong || jal_idx < btb_idx)
   io.ras_update.valid := jal_may_win && io.is_call(jal_idx)
   io.ras_update.bits.is_call := true.B
   io.ras_update.bits.is_ret := false.B
   io.ras_update.bits.return_addr := io.aligned_pc + (jal_idx << 2.U) + 4.U
}

