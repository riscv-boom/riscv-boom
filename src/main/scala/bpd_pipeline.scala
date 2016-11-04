//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Branch Prediction Pipeline
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2014 Apr 23
//
// Access branch predictor and redirect the pipeline as necessary. Also in
// charge of JALs (direction and target are known).
//
// These stages are effectively in parallel with instruction fetch and decode.
// BHT look-up (bp1) is in parallel with I$ access, and Branch Decode (bp2)
// occurs before fetch buffer insertion.
//
// Currently, I ignore JALRs (either the BTB took care of it or it'll get
// mispredicted and kill everything behind it anyways).

package boom

import Chisel._
import cde.Parameters

import util.Str

class RedirectRequest(fetch_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val target  = UInt(width = vaddrBits+1)
   val br_pc   = UInt(width = vaddrBits+1) // PC of the instruction changing control flow (to update the BTB with jumps)
   val idx     = UInt(width = log2Up(fetch_width)) // idx of br in fetch bundle (to mask out the appropriate fetch
                                                   // instructions)
   val is_jump = Bool() // (only valid if redirect request is valid)
   val is_cfi  = Bool() // Is redirect due to a control-flow instruction?
   val is_taken= Bool() // (true if redirect is to "take" a branch,
                        //  false if it's to request PC+4 for a mispred
  override def cloneType = new RedirectRequest(fetch_width)(p).asInstanceOf[this.type]
}

// this information is shared across the entire fetch packet, and stored in the
// branch snapshots. Since it's not unique to an instruction, it could be
// compressed further. It can be de-allocated once the branch is resolved in
// Execute.
class BranchPredictionResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val btb_resp_valid = Bool()
   val btb_resp       = new rocket.BTBResp

   val bpd_resp       = new BpdResp

   // used to tell front-end how to mask off instructions
   val mask           = Bits(width = fetchWidth)
   val br_seen        = Bool() // was a branch seen in this fetch packet?
}

// give this to each instruction/uop and pass this down the pipeline to the branch-unit
class BranchPrediction(implicit p: Parameters) extends BoomBundle()(p)
{
   val bpd_predict_val  = Bool() // did the bpd predict this instruction? (ie, tag hit in the BPD)
   val bpd_predict_taken= Bool() // did the bpd predict taken for this instruction?
   val btb_hit          = Bool() // this instruction was the br/jmp predicted by the BTB
   val btb_predicted    = Bool() // Does the BTB get credit for the prediction? (FU checks)

   val is_br_or_jalr    = Bool() // is this instruction a branch or jalr?
                                 // (need to allocate brob entry).

   def wasBTB = btb_predicted
}

class BranchPredictionStage(fetch_width: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new BoomBundle()(p)
   {
      val req        = Decoupled(new RedirectRequest(fetch_width))
      val imem_resp  = Decoupled(new rocket.FrontendResp).flip
      val btb_resp   = Valid(new rocket.BTBResp).flip
      val npc        = UInt(INPUT, width = vaddrBitsExtended)
      val ras_update = Valid(new rocket.RASUpdate)

      val pred_resp  = new BranchPredictionResp().asOutput
      val predictions= Vec(fetch_width, new BranchPrediction().asOutput)
      val br_unit    = new BranchUnitResp().asInput

      val brob       = new BrobBackendIo(fetch_width)
      val flush      = Bool(INPUT)
      val status_prv = UInt(INPUT, width = rocket.PRV.SZ)
   }

   //-------------------------------------------------------------
   // Branch Prediction (BP1 Stage)

   val bp2_br_seen = Wire(Bool())  // did we see a branch to make a prediction?
                                   // (and not overridden by an earlier jal)
   val bp2_br_taken = Wire(Bool()) // was there a taken branch in the bp2 stage
                                   // we use this to update the bpd's history register speculatively

   var br_predictor: BrPredictor = null
   if (ENABLE_BRANCH_PREDICTOR && p(TageKey).enabled)
   {
      br_predictor = Module(new TageBrPredictor(fetch_width = fetch_width,
                                                num_tables = p(TageKey).num_tables,
                                                table_sizes = p(TageKey).table_sizes,
                                                history_lengths = p(TageKey).history_lengths,
                                                tag_sizes = p(TageKey).tag_sizes
                                                ))
   }
   else if (ENABLE_BRANCH_PREDICTOR && p(GSkewKey).enabled)
   {
      br_predictor = Module(new GSkewBrPredictor(fetch_width = fetch_width,
                                                  history_length = p(GSkewKey).history_length,
                                                  dualported = p(GSkewKey).dualported,
                                                  enable_meta = p(GSkewKey).enable_meta))
   }
   else if (ENABLE_BRANCH_PREDICTOR && p(GShareKey).enabled)
   {
      br_predictor = Module(new GShareBrPredictor(fetch_width = fetch_width,
                                                  history_length = p(GShareKey).history_length,
                                                  dualported = p(GShareKey).dualported))
   }
   else if (ENABLE_BRANCH_PREDICTOR && p(SimpleGShareKey).enabled)
   {
      br_predictor = Module(new SimpleGShareBrPredictor(fetch_width = fetch_width,
                                                  history_length = p(SimpleGShareKey).history_length))
   }
   else if (ENABLE_BRANCH_PREDICTOR && p(RandomBpdKey).enabled)
   {
      br_predictor = Module(new RandomBrPredictor(fetch_width = fetch_width))
   }
   else
   {
      br_predictor = Module(new NullBrPredictor(fetch_width = fetch_width,
                                                history_length = GLOBAL_HISTORY_LENGTH))
   }

   br_predictor.io.req_pc := io.npc
   br_predictor.io.br_resolution <> io.br_unit.bpd_update
   // TODO BUG XXX i suspect this is completely and utterly broken. what about <bne,jr,bne> or  <bne,j,bne>. What
   // about <csr, bne>/<b,csr,b>? does unique/pipeline replaysincrement ghistory when they shouldn't?
   br_predictor.io.hist_update_spec.valid := bp2_br_seen && io.req.ready
   br_predictor.io.hist_update_spec.bits.taken := bp2_br_taken
   br_predictor.io.resp.ready := io.req.ready

   io.brob <> br_predictor.io.brob
   br_predictor.io.flush := io.flush
   br_predictor.io.status_prv := io.status_prv

//   val bpd_valid = br_predictor.io.resp.valid && (io.status_prv === UInt(rocket.PRV.U) || !Bool(ENABLE_BPD_UMODE_ONLY))
   val bpd_valid = br_predictor.io.resp.valid
   val bpd_bits = br_predictor.io.resp.bits


   //-------------------------------------------------------------
   // Branch Decode (BP2 Stage)
   //
   // 1) Which branch to take?
   // 2) Is there a jal earlier to take?
   // 3) Does the BTB override our prediction?
   //    - 3b) if no, verify BTB is correct?
   // 4) Update RAS

   // round off to nearest fetch boundary
   val aligned_pc = ~(~io.imem_resp.bits.pc | (UInt(fetch_width*coreInstBytes-1)))

   val is_br     = Wire(Vec(fetch_width, Bool()))
   val is_jal    = Wire(Vec(fetch_width, Bool()))
   val is_jr     = Wire(Vec(fetch_width, Bool()))
   val br_targs  = Wire(Vec(fetch_width, UInt(width=vaddrBits+1)))
   val jal_targs = Wire(Vec(fetch_width, UInt(width=vaddrBits+1)))

   for (i <- 0 until fetch_width)
   {
      val inst = io.imem_resp.bits.data(i*coreInstBits+coreInstBits-1,i*coreInstBits)
      val bpd_decoder = Module(new BranchDecode)
      bpd_decoder.io.inst := inst

      is_br(i)  := bpd_decoder.io.is_br   && io.imem_resp.bits.mask(i)
      is_jal(i) := bpd_decoder.io.is_jal  && io.imem_resp.bits.mask(i)
      is_jr(i)  := bpd_decoder.io.is_jalr && io.imem_resp.bits.mask(i)

      val pc = aligned_pc + UInt(i << 2)
      br_targs(i)  := ComputeBranchTarget(pc, inst, xLen, coreInstBytes)
      jal_targs(i) := ComputeJALTarget(pc, inst, xLen, coreInstBytes)
   }


   //-------------------------------------------------------------
   // Output (make an actual prediction/redirect request)

   // There are many predictions vying for priority.
   // The following are equal priority - whosever has
   // the instruction earliest in program-order wins:
   //    - JALs
   //    - BTB (JALs/JRs)
   //    - BPD (branches)
   //
   // At a lower priority is the BTB predicting branches:
   //    - BTB (branches)
   //
   // If the BPD defers (bpd_valid == false), then the BTB's
   // branch prediction stand.

   val bpd_predictions  = is_br.toBits & bpd_bits.takens
   val bpd_br_taken     = bpd_predictions.orR && bpd_valid
   val bpd_br_idx       = PriorityEncoder(bpd_predictions)
   val bpd_jal_val      = is_jal.reduce(_|_)
   val bpd_jal_idx      = PriorityEncoder(is_jal.toBits)
   val bpd_br_beats_jal = bpd_br_taken && (!bpd_jal_val || (bpd_br_idx < bpd_jal_idx))
   val bpd_req_idx      = Mux(bpd_br_beats_jal, bpd_br_idx, bpd_jal_idx)
   val bpd_req_target   = Mux(bpd_br_beats_jal, br_targs(bpd_br_idx), jal_targs(bpd_jal_idx))



   // For the particular instruction the BTB predicted, does the BPD agree with the direction?
   // (this is only valid if the BTB is predicting a branch.)
   val bpd_agrees_with_btb = !(IsIdxAMatch(io.btb_resp.bits.bridx, bpd_predictions) ^
                             io.btb_resp.bits.taken)

   // bpd will make a redirection request (either for a br or jal)
   // for "taking" a branch or JAL.
   val bpd_br_fire = Wire(init = Bool(false))
   val bpd_jal_fire = Wire(init = Bool(false))
   // If the BTB predicted taken, and the BPD disagrees and believes no branch
   // is taken, we must instead redirect the FrontEnd to fetch the "next packet"
   // in program order (PC+4-ish, if you will).
   val bpd_nextline_fire = Wire(init = Bool(false))
   val nextline_pc = aligned_pc + UInt(fetch_width*coreInstBytes)
   // BTB provides a suggested "valid instruction" mask, based on its prediction.
   // Should we override its mask?
   val override_btb = Wire(init = Bool(false))

   // Is the predicted instruction a control-flow instruction?
   // Used to invalidate BTB entries due to it predicting on
   // a non-branch/non-jump instruction.
   val is_cfi = Wire(init = Bool(true))

   // does the index match on a true bit in the mask?
   def IsIdxAMatch(idx: UInt, mask: UInt) : Bool =
   {
      mask(idx)
   }
   val btb_predicted_br          = IsIdxAMatch(io.btb_resp.bits.bridx, is_br.toBits)
   val btb_predicted_br_taken    = btb_predicted_br && io.btb_resp.bits.taken
   val btb_predicted_br_nottaken = btb_predicted_br && !io.btb_resp.bits.taken
   val btb_predicted_jump        = IsIdxAMatch(io.btb_resp.bits.bridx, is_jal.toBits | is_jr.toBits)
   val btb_predicted_jal         = IsIdxAMatch(io.btb_resp.bits.bridx, is_jal.toBits)

   val btb_predicted_wrong_jal_target = btb_predicted_jal && io.btb_resp.bits.target =/= jal_targs(bpd_jal_idx)

   when (io.btb_resp.valid)
   {
      // BTB made a prediction -
      // make a redirect request if:
      //    - if the BPD (br) or JAL comes earlier than the BTB's redirection
      //    - if both the BTB and the BPD predicted a branch, the BPD wins
      //       * involves refetching the latter half of the packet if we "undo"
      //          the BTB's taken branch.

      when (btb_predicted_jump) // both JAL and JRs
      {
         val btb_nt = !io.btb_resp.bits.taken
         bpd_br_fire  := bpd_br_beats_jal && bpd_br_taken && (bpd_br_idx < io.btb_resp.bits.bridx)
         bpd_jal_fire := !bpd_br_beats_jal && bpd_jal_val &&
                           ((bpd_jal_idx < io.btb_resp.bits.bridx) || btb_nt || btb_predicted_wrong_jal_target)

         when (io.imem_resp.valid)
         {
            when (!io.btb_resp.bits.taken)
            {
               val btb_predicted_jr = IsIdxAMatch(io.btb_resp.bits.bridx, is_jr.toBits)
               // if JR but predicted not TAKEN, do nothing and let BrUnit fix mispredicton.
               assert (bpd_br_fire ||
                  (bpd_jal_fire && (bpd_jal_idx <= io.btb_resp.bits.bridx)) ||
                  btb_predicted_jr,
                  "[bpd_pipeline] BTB predicted a jump but didn't take it, and we are failing to correct it.")
            }
         }
      }
      .elsewhen (btb_predicted_br_taken)
      {
         // overrule the BTB if
         //    1. either a jump or branch occurs earlier
         //          (bpd_jal_fire, bpd_br_fire)
         //    2. OR the BPD predicts the BTB's branch as not taken...
         //       a. and no other branch taken (bpd_nextline_fire)
         //       b. a later branch as taken (bpd_br_fire)
         //       c. a later jump is present (bpd_jal_fire)

         // does the bpd predict the branch is taken too? (assuming bpd_valid)
         val bpd_agrees_with_btb = bpd_predictions(io.btb_resp.bits.bridx)

         bpd_jal_fire := !bpd_br_beats_jal && bpd_jal_val &&
                           ((bpd_jal_idx < io.btb_resp.bits.bridx) ||
                           (bpd_valid && !bpd_agrees_with_btb))
         bpd_br_fire  := bpd_br_beats_jal && bpd_br_taken &&
                           (bpd_br_idx < io.btb_resp.bits.bridx ||  // earlier than BTB's branch
                           (bpd_valid && !bpd_agrees_with_btb))     // taken later
//                         (bpd_valid && !bpd_agrees_with_btb && io.btb_resp.bits.bridx =/= bpd_br_idx)) // taken later
                                                                                                    // than BTB's branch

         bpd_nextline_fire := bpd_valid && !bpd_predictions.orR && !bpd_jal_val
         override_btb := bpd_valid && !bpd_agrees_with_btb

         when (bpd_nextline_fire)
         {
            assert (override_btb, "[bpd_pipeline] redirecting the PC without overriding the BTB.")
         }
         when (override_btb)
         {
            assert (bpd_nextline_fire || bpd_jal_fire || bpd_br_fire,
               "[bpd_pipeline] overriding the BTB without redirecting the PC.")
         }
      }
      .elsewhen (btb_predicted_br_nottaken)
      {
         // completely overrule the BTB if it predicted not-taken, but the BPD is predicted taken
         bpd_br_fire  := bpd_br_beats_jal && bpd_br_taken
         bpd_jal_fire := !bpd_br_beats_jal && bpd_jal_val
      }
      .otherwise
      {
         when (io.imem_resp.valid)
         {
            // Thanks to uncacheable regions (also, the BTB is never invalidated),
            // The BTB may actually predict things that aren't branches!
            // But we must undo these "mispredictions"!
            //printf ("[bpd_pipeline] BTB resp is valid, but didn't detect what it predicted.")
            override_btb := Bool(true)
            is_cfi := Bool(false)

            // but is there a branch or jump we need to handle? Or just fetch the nextline?
            bpd_br_fire  := bpd_br_beats_jal && bpd_br_taken
            bpd_jal_fire := !bpd_br_beats_jal && bpd_jal_val
            bpd_nextline_fire := !bpd_br_fire && !bpd_jal_fire
         }
      }
   }
   .otherwise
   {
      // BTB made no prediction - let the BPD do what it wants
      bpd_br_fire  := bpd_br_beats_jal
      bpd_jal_fire := bpd_jal_val && !bpd_br_fire
   }

   assert (PopCount(Vec(bpd_br_fire, bpd_jal_fire, bpd_nextline_fire)) <= UInt(1),
      "[bpd_pipeline] mutually-exclusive signals firing")


   io.req.valid        := io.imem_resp.valid &&
                          (bpd_br_fire || bpd_jal_fire || bpd_nextline_fire) &&
                          !io.imem_resp.bits.xcpt_if
   io.req.bits.target  := Mux(bpd_nextline_fire, nextline_pc, bpd_req_target)
   io.req.bits.idx     := Mux(bpd_nextline_fire, UInt(fetch_width-1), bpd_req_idx)
   io.req.bits.br_pc   := aligned_pc + (io.req.bits.idx << UInt(2))
   io.req.bits.is_jump := !bpd_br_beats_jal
   io.req.bits.is_cfi  := is_cfi
   io.req.bits.is_taken:= bpd_br_fire || bpd_jal_fire

   io.pred_resp.btb_resp_valid   := io.btb_resp.valid
   io.pred_resp.btb_resp         := io.btb_resp.bits
   io.pred_resp.bpd_resp         := bpd_bits
   io.pred_resp.br_seen          := bp2_br_seen

   private def KillMask(m_enable: Bool, m_idx: UInt, m_width: Int): UInt =
   {
      val mask = Wire(Bits(width = m_width))
      mask := Fill(m_width, m_enable) & (Fill(m_width, UInt(1)) << UInt(1) << m_idx)
      mask
   }
   // mask out instructions after predicted branch
   val bpd_kill_mask = KillMask(io.req.valid,
                               io.req.bits.idx,
                               fetchWidth)
   // mask out instructions after first jr (doesn't matter if predicted correctly or not!)
   val jr_kill_mask = KillMask(is_jr.reduce(_|_),
                               PriorityEncoder(is_jr.toBits),
                               fetchWidth)
   // if we accept the BTB's prediction, mask out instructions after its prediction
   val btb_kill_mask = KillMask(io.btb_resp.valid && io.btb_resp.bits.taken &&
                              !(bpd_br_fire || bpd_jal_fire || bpd_nextline_fire),
                               PriorityEncoder(io.btb_resp.bits.bridx),
                               fetchWidth)

   val btb_mask = Mux(override_btb || !io.btb_resp.valid,
                     Fill(fetchWidth, UInt(1,1)),
                     io.btb_resp.bits.mask)
   io.pred_resp.mask := ~bpd_kill_mask & ~jr_kill_mask & btb_mask


   for (w <- 0 until FETCH_WIDTH)
   {
      io.predictions(w).is_br_or_jalr := is_br(w) || is_jr(w)
      io.predictions(w).bpd_predict_taken := bpd_predictions(w) && bpd_valid && !bpd_nextline_fire
      io.predictions(w).btb_predicted := io.btb_resp.valid &&
                                          !(bpd_nextline_fire || bpd_br_fire || bpd_jal_fire)
      io.predictions(w).btb_hit := Mux(io.btb_resp.bits.bridx === UInt(w),
                                          io.btb_resp.valid, Bool(false))
      io.predictions(w).bpd_predict_val   := bpd_valid
   }

   bp2_br_seen := io.imem_resp.valid &&
                  !io.imem_resp.bits.xcpt_if &&
                  is_br.reduce(_|_) &&
                  (!bpd_jal_val || (PriorityEncoder(is_br.toBits) < PriorityEncoder(is_jal.toBits)))
   bp2_br_taken := bpd_br_fire || (io.btb_resp.valid && btb_predicted_br_taken && !bpd_nextline_fire && !override_btb)

   //-------------------------------------------------------------
   // Look for CALL and RETURN for RAS shenanigans.
   // TODO flush_take_pc should probably be given to the branch unit, instead of resetting it here?
   // NOTE: what about branch taken earlier?

   val jumps    = is_jal.toBits | is_jr.toBits
   val jmp_idx  = PriorityEncoder(jumps)
   val jmp_inst = (io.imem_resp.bits.data >> (jmp_idx*UInt(coreInstBits)))(coreInstBits-1,0)
   val is_call  = IsCall(jmp_inst)
   val is_ret   = IsReturn(jmp_inst)
   io.ras_update.valid           := io.imem_resp.valid &&
                                         !io.imem_resp.bits.xcpt_if &&
                                         jumps.orR &&
                                         !bpd_br_beats_jal &&
                                         io.req.ready &&
                                         !io.flush
   io.ras_update.bits.isCall     := is_call
   io.ras_update.bits.isReturn   := is_ret
   io.ras_update.bits.returnAddr := aligned_pc + (jmp_idx << UInt(2)) + UInt(4)
   io.ras_update.bits.prediction := io.btb_resp

   //-------------------------------------------------------------
   // printfs

   if (DEBUG_PRINTF)
   {
      printf("bp2_aligned_pc: 0x%x BHT:(%c 0x%x, %d) p:%x (%d) b:%x j:%x (%d) %c %c\n"
         , aligned_pc, Mux(io.req.valid, Str("T"), Str("-")), io.req.bits.target, io.req.bits.idx
         , bpd_predictions.toBits, bpd_br_idx, is_br.toBits, is_jal.toBits, bpd_jal_idx
         , Mux(bpd_br_beats_jal, Str("B"), Str("J")), Mux(bpd_nextline_fire, Str("N"), Str("-"))
         )
   }

   //-------------------------------------------------------------
   // asserts

   when (io.imem_resp.valid && io.btb_resp.valid && io.btb_resp.bits.taken && !io.imem_resp.bits.xcpt_if)
   {
      val idx = io.btb_resp.bits.bridx
      val targ = Mux(is_br(idx), br_targs(idx), jal_targs(idx))
      when (!is_jr(idx) && !(bpd_nextline_fire))
      {
//         assert (io.btb_resp.bits.target === targ(vaddrBits-1,0),
//            "[bpd_pipeline] BTB is jumping to an invalid target.")
         when (io.btb_resp.bits.target =/= targ(vaddrBits-1,0))
         {
            // TODO remove this... BTBs can now just predict total garbage
            //printf("[bpd_pipeline] BTB is jumping to an invalid target.\n")
         }
      }
   }

   if (!p(EnableBTB))
   {
      assert (!(io.btb_resp.valid), "[bpd_pipeline] BTB predicted, but it's been disabled.")
   }

}
