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
import cde.Parameters

import util.Str
import util.UIntToAugmentedUInt

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

// Sent to the Fetch Unit to redirect the pipeline as needed.
// The Fetch Unit must also sanity check the request.
class BpuRequest(implicit p: Parameters) extends BoomBundle()(p)
{
//   btb_hit
//   taken
//   cfi_type
   val target  = UInt(width = vaddrBitsExtended)
   val cfi_idx = UInt(width = log2Up(fetchWidth)) // where is cfi we are predicting?
   val mask    = UInt(width = fetchWidth) // mask of valid instructions.

   // Push this info down the pipeline for save keeping.
//   val predinfo = new BranchPredInfo
}

//class BranchPrediction(implicit p: Parameters) extends BoomBundle()(p)
// Give this to each instruction/uop and pass this down the pipeline to the branch-unit
// This covers the per-instruction info on all cfi-related predictions.
// TODO rename to make clear this is "BPD->RenameSnapshot->BRU". Should NOT go into Issue Windows.
class BranchPredInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val bpd_predicted     = Bool() // did the bpd predict this instruction? (ie, tag hit in the BPD)
   val bpd_taken         = Bool() // did the bpd predict taken for this instruction?
   val btb_hit           = Bool() // this instruction was the br/jmp predicted by the BTB
   val btb_taken         = Bool() // this instruction was the br/jmp predicted by the BTB and was taken
   val btb_predicted     = Bool() // Does the BTB get credit for the prediction? (FU checks)
//
//   val is_br_or_jalr    = Bool() // is this instruction a branch or jalr?
                                   // (need to allocate brob entry).
   val bim_resp         = new BimResp

   val bpd_resp         = new BpdResp // TODO XXX this can be very expensive -- don't give to every instruction? And break into separate toBRU/Exe and toCom versions.

   def wasBTB = btb_predicted
}

class BranchPredictionStage(fetch_width: Int)(implicit p: cde.Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
{
   val io = new BoomBundle()(p)
   {
      // Fetch0
      val ext_btb_req   = Valid(new PCReq).flip
      val icmiss        = Bool(INPUT)
      val fetch_stalled = Bool(INPUT)
      val f0_btb        = Valid(new BTBsaResp)
      val f2_bpu_request= Valid(new BpuRequest)

      val f2_btb_resp   = Valid(new BTBsaResp)
      val f2_bpd_resp   = Valid(new BpdResp)
      val f2_btb_update = Valid(new BTBsaUpdate).flip
      val f2_ras_update = Valid(new RasUpdate).flip

      // Other
      val br_unit       = new BranchUnitResp().asInput
      val brob       = new BrobBackendIo(fetch_width)
      val flush         = Bool(INPUT) // pipeline flush
      val redirect      = Bool(INPUT)
      val status_prv    = UInt(INPUT, width = rocket.PRV.SZ)
      val status_debug  = Bool(INPUT)
   }

   //************************************************
   // construct all of the modules

   val btb = Module(new BTBsa())
   val bpd = BrPredictor(tileParams, boomParams)

   btb.io.status_debug := io.status_debug
   bpd.io.status_prv := io.status_prv


   //************************************************
   // Branch Prediction (BP0 Stage)

   btb.io.req := io.ext_btb_req
   btb.io.icmiss := io.icmiss
   bpd.io.req_pc := io.ext_btb_req.bits.addr


   //************************************************
   // Branch Prediction (BP1 Stage)

   io.f0_btb <> btb.io.resp


   //************************************************
   // Branch Prediction (BP2 Stage)

   val f2_btb = Reg(Valid(new BTBsaResp))
   val f2_pc  = Reg(UInt())
   val f2_aligned_pc = ~(~f2_pc | (UInt(fetch_width*coreInstBytes-1)))
   val f2_nextline_pc = Wire(UInt(width=vaddrBits))
   f2_nextline_pc := f2_aligned_pc + UInt(fetch_width*coreInstBytes)

   // request in BTB can advance
   when (io.ext_btb_req.valid)
   {
      f2_btb := btb.io.resp
      f2_pc := btb.io.resp.bits.fetch_pc
   }

   io.f2_btb_resp := f2_btb
   io.f2_bpd_resp := bpd.io.resp

   // does the BPD predict a taken branch?
   private def bitRead(bits: UInt, offset: UInt): Bool = (bits >> offset)(0)

   val bpd_valid = bpd.io.resp.valid
   val bpd_bits = bpd.io.resp.bits

   val bpd_predict_taken = bitRead(bpd_bits.takens, f2_btb.bits.cfi_idx)
   val bpd_disagrees_with_btb =
      f2_btb.valid && bpd_valid && (bpd_predict_taken ^ f2_btb.bits.taken) && f2_btb.bits.cfi_type === CfiType.branch

   io.f2_bpu_request.valid := bpd_disagrees_with_btb
   io.f2_bpu_request.bits.target :=
      Mux(bpd_predict_taken,
         f2_btb.bits.target.sextTo(vaddrBitsExtended),
         f2_nextline_pc.sextTo(vaddrBitsExtended))
   io.f2_bpu_request.bits.mask := Cat((UInt(1) << ~Mux(bpd_predict_taken, ~f2_btb.bits.cfi_idx, UInt(0)))-UInt(1), UInt(1))


   val f2_br_seen = f2_btb.valid && f2_btb.bits.cfi_type === CfiType.branch
   val f2_jr_seen = f2_btb.valid && f2_btb.bits.cfi_type === CfiType.jalr
   bpd.io.hist_update_spec.valid := (f2_br_seen || f2_jr_seen) && !io.fetch_stalled
   bpd.io.hist_update_spec.bits.taken := f2_jr_seen || Mux(bpd_valid, bpd_predict_taken, f2_btb.bits.taken)
   bpd.io.resp.ready := !io.fetch_stalled

   //************************************************
   // Update the RAS

   // update RAS based on BTB's prediction information (or the branch-check correction).
   val jmp_idx = f2_btb.bits.cfi_idx

   btb.io.ras_update := io.f2_ras_update
   btb.io.ras_update.valid := (f2_btb.valid || io.f2_ras_update.valid) && !io.fetch_stalled
   when (f2_btb.valid)
   {
      btb.io.ras_update.bits.is_call      := f2_btb.bits.bpd_type === BpredType.call
      btb.io.ras_update.bits.is_ret       := f2_btb.bits.bpd_type === BpredType.ret
      btb.io.ras_update.bits.return_addr  := f2_aligned_pc + (jmp_idx << 2.U) + 4.U
   }


   //************************************************
   // Update the BTB/BIM

   // TODO XXX: allow branch-checker to kill bad entries.
   // TODO XXX update the BTB during the F3/branch-checker stage if BPD says we should take it.
   btb.io.btb_update :=
      Mux(io.br_unit.btb_update.valid,
         io.br_unit.btb_update,
         io.f2_btb_update)

   // TODO XXX allow F2/BPD redirects to correct the BIM.
   btb.io.bim_update := io.br_unit.bim_update


   //************************************************
   // Update the BPD

   bpd.io.br_resolution := io.br_unit.bpd_update

   bpd.io.flush := io.flush
   io.brob <> bpd.io.brob


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
      printf("btb, f0_npc=%c req_pc 0x%x, f1=%c targ=0x%x\n"
         , Mux(btb.io.req.valid, Str("V"), Str("-"))
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
