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

//class RedirectRequest(fetch_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
//{
//   val target  = UInt(width = vaddrBits+1)
//   val br_pc   = UInt(width = vaddrBits+1) // PC of the instruction changing control flow (to update the BTB with jumps)
//   val idx     = UInt(width = log2Up(fetch_width)) // idx of br in fetch bundle (to mask out the appropriate fetch
//                                                   // instructions)
//   val is_jump = Bool() // (only valid if redirect request is valid)
//   val is_cfi  = Bool() // Is redirect due to a control-flow instruction?
//   val is_taken= Bool() // (true if redirect is to "take" a branch,
//                        //  false if it's to request PC+4 for a mispred
//  override def cloneType = new RedirectRequest(fetch_width)(p).asInstanceOf[this.type]
//}

// this information is shared across the entire fetch packet, and stored in the
// branch snapshots. Since it's not unique to an instruction, it could be
// compressed further. It can be de-allocated once the branch is resolved in
// Execute.
//class BranchPredictionResp(implicit p: Parameters) extends BoomBundle()(p)
//{
//   val btb_resp_valid = Bool()
//   val btb_resp       = new rocket.BTBResp
//
//   val bpd_resp       = new BpdResp
//
//   // used to tell front-end how to mask off instructions
//   val mask           = Bits(width = fetchWidth)
//   val br_seen        = Bool() // was a branch seen in this fetch packet?
//}

//class BranchPrediction(implicit p: Parameters) extends BoomBundle()(p)
// Give this to each instruction/uop and pass this down the pipeline to the branch-unit
// This covers the per-instruction info on all cfi-related predictions.
class BranchPredInfo(implicit p: Parameters) extends BoomBundle()(p)
{
//   val bpd_predict_val  = Bool() // did the bpd predict this instruction? (ie, tag hit in the BPD)
//   val bpd_predict_taken= Bool() // did the bpd predict taken for this instruction?
   val btb_hit          = Bool() // this instruction was the br/jmp predicted by the BTB
   val btb_taken        = Bool() // this instruction was the br/jmp predicted by the BTB and was taken
//   val btb_predicted    = Bool() // Does the BTB get credit for the prediction? (FU checks)
//
//   val is_br_or_jalr    = Bool() // is this instruction a branch or jalr?
//                                 // (need to allocate brob entry).
//
//   def wasBTB = btb_predicted
}

class BranchPredictionStage(fetch_width: Int)(implicit p: cde.Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
{
   val io = new BoomBundle()(p)
   {
      // Fetch0
      // TODO restructure as req/valid bundle?
      val npc        = UInt(INPUT, width = vaddrBitsExtended)
//      val imem              = new rocket.FrontendIO
      val ext_btb_req = Valid(new PCReq).flip
      val fetch_stalled = Bool(INPUT)
      val f0_btb     = Valid(new BTBsaResp)
      val f2_bpu_info = Valid(new BTBsaResp)
//      val f2_bpu_info = Valid(new BranchPredInfo)

      // Other
      val br_unit    = new BranchUnitResp().asInput
//      val brob       = new BrobBackendIo(fetch_width)
      val flush      = Bool(INPUT) // pipeline flush
      val redirect   = Bool(INPUT)
//      val status_prv = UInt(INPUT, width = rocket.PRV.SZ)
      val status_debug = Bool(INPUT)
   }

   //************************************************
   // construct all of the modules

   val btb = Module(new BTBsa())
   btb.io.status_debug := io.status_debug


   //************************************************
   // Branch Prediction (BP0 Stage)

//   btb.io.req.valid := Bool(true) && !io.fetch_stalled // XXX && !stall
//   btb.io.req.valid := io.ext_btb_req.valid //(this is (!icmiss and !stall)
//   btb.io.req.bits.addr := io.npc
   btb.io.req := io.ext_btb_req

//   when (io.ext_btb_req.valid) // this breaks when un-stalling and getting redirected on same cycle
//   {
//      assert (RegNext(io.npc) === io.ext_btb_req.bits.addr, "[btb stuff]")
//   }

   //************************************************
   // Branch Prediction (BP1 Stage)

   io.f0_btb <> btb.io.resp

   //************************************************
   // Branch Prediction (BP2 Stage)

   val f2_btb = Reg(Valid(new BTBsaResp))

   when (!io.fetch_stalled)
   {
      f2_btb := btb.io.resp
   }
   io.f2_bpu_info := f2_btb

   //************************************************
   // Update the BTB

   btb.io.btb_update := io.br_unit.btb_update

   if (enableBTB)
   {
      btb.io.btb_update.valid :=
         (io.br_unit.btb_update.valid || false.B) && !io.flush
//       (io.bp2_take_pc && io.bp2_is_taken && !if_stalled && !br_unit.take_pc)) &&
   }
   else
   {
      btb.io.btb_update.valid := false.B
   }

   // update the BTB
   // If a branch is mispredicted and taken, update the BTB.
   // (if branch unit mispredicts, instructions in decode are no longer valid)
   //io.imem.btb_update.bits.pc         := Mux(br_unit.btb_update_valid, br_unit.btb_update.pc, io.imem.resp.bits.pc)
   //io.imem.btb_update.bits.br_pc      := Mux(br_unit.btb_update_valid, br_unit.btb_update.br_pc, io.bp2_pc_of_br_inst)
   //io.imem.btb_update.bits.target     := Mux(br_unit.btb_update_valid, br_unit.btb_update.target,
   //                                                                    (io.bp2_pred_target.toSInt &
   //                                                                     SInt(-coreInstBytes)).toUInt)
   //io.imem.btb_update.bits.prediction := Mux(br_unit.btb_update_valid, br_unit.btb_update.prediction,
   //                                                                    io.imem.resp.bits.btb)
   //io.imem.btb_update.bits.taken      := Mux(br_unit.btb_update_valid, br_unit.btb_update.taken,
   //                                                                    io.bp2_take_pc && io.bp2_is_taken && !if_stalled)
   //io.imem.btb_update.bits.isJump     := Mux(br_unit.btb_update_valid, br_unit.btb_update.isJump, io.bp2_is_jump)
   //io.imem.btb_update.bits.isReturn   := Mux(br_unit.btb_update_valid, br_unit.btb_update.isReturn, Bool(false))
   //io.imem.btb_update.bits.isValid    := Mux(br_unit.btb_update_valid, Bool(true), io.bp2_is_cfi)


   //************************************************
   // Handle redirects/flushes

   when (io.flush || reset.toBool)
   {
      f2_btb.valid := false.B
      btb.io.btb_update.valid := false.B
   }

   when (io.redirect)
   {
      f2_btb.valid := false.B
   }


   //************************************************
   // printfs

   if (DEBUG_PRINTF)
   {
      printf("btb, f0_npc=%c 0x%x, req_pc 0x%x, f1=%c targ=0x%x\n"
         , Mux(btb.io.req.valid, Str("V"), Str("-"))
         , io.npc
         , io.ext_btb_req.bits.addr
         , Mux(btb.io.resp.valid, Str("V"), Str("-"))
         , btb.io.resp.bits.target
         )
//      printf("bp2_aligned_pc: 0x%x BHT:(%c 0x%x, %d) p:%x (%d) b:%x j:%x (%d) %c %c\n"
//         , aligned_pc, Mux(imemreq_valid, Str("T"), Str("-")), imemreq.target, imemreq.idx
//         , bpd_predictions.toBits, bpd_br_idx, is_br.toBits, is_jal.toBits, bpd_jal_idx
//         , Mux(bpd_br_beats_jal, Str("B"), Str("J")), Mux(bpd_nextline_fire, Str("N"), Str("-"))
//         )
   }

   //************************************************
   // asserts

   if (!enableBTB)
   {
      assert (!(io.f0_btb.valid), "[bpd_pipeline] BTB predicted, but it's been disabled.")
   }

}
