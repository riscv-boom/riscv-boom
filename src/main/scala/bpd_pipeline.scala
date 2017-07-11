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
// Access BTB and BPD to feed predictions to the Fetch Unit.
//
// These stages are in parallel with instruction fetch.
//    * F0 - Select next PC.
//    * F1 - Access I$ and BTB RAMs. Perform BPD hashing.
//    * F2 - Access BPD RAMs. Begin decoding instruction bits and computing targets from I$.

package boom

import Chisel._
import config.Parameters

import util.Str

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
// TODO rename to make clear this is "BPD->RenameSnapshot->BRU". Should NOT go into Issue Windows.
class BranchPredInfo(implicit p: Parameters) extends BoomBundle()(p)
{
//   val bpd_predict_val  = Bool() // did the bpd predict this instruction? (ie, tag hit in the BPD)
//   val bpd_predict_taken= Bool() // did the bpd predict taken for this instruction?
   val btb_hit          = Bool() // this instruction was the br/jmp predicted by the BTB
   val btb_taken        = Bool() // this instruction was the br/jmp predicted by the BTB and was taken
//   val btb_predicted    = Bool() // Does the BTB get credit for the prediction? (FU checks)
//
//   val is_br_or_jalr    = Bool() // is this instruction a branch or jalr?
                                   // (need to allocate brob entry).

   val bim_resp         = new BimResp

//   def wasBTB = btb_predicted
}

class BranchPredictionStage(fetch_width: Int)(implicit p: Parameters) extends BoomModule()(p)
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
   // Update the BTB/BIM

   btb.io.btb_update := io.br_unit.btb_update
   btb.io.bim_update := io.br_unit.bim_update

   // TODO do we also update the BTB during the F3/branch-checker stage if BPD says we should take it?
   // TODO when do we update the bim?


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
   }

   //************************************************
   // asserts

   if (!enableBTB)
   {
      assert (!(io.f0_btb.valid), "[bpd_pipeline] BTB predicted, but it's been disabled.")
   }

}
