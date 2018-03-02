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
// 2017
//
// Access BTB and BPD to feed predictions to the Fetch Unit.
//
// Stages (these are in parallel with instruction fetch):
//    * F0 - Select next PC.
//    * F1 - Access I$ and BTB RAMs. Perform BPD hashing.
//    * F2 - Access BPD RAMs. Begin decoding instruction bits and computing targets from I$.

package boom

import Chisel._
import chisel3.core.withReset
import freechips.rocketchip.config.Parameters

import freechips.rocketchip.util.{Str, UIntToAugmentedUInt, ShiftQueue}

// this information is shared across the entire fetch packet, and stored in the
// branch snapshots. Since it's not unique to an instruction, it could be
// compressed further.
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
   val target  = UInt(width = vaddrBitsExtended)
   val cfi_idx = UInt(width = log2Up(fetchWidth)) // where is cfi we are predicting?
   val mask    = UInt(width = fetchWidth) // mask of valid instructions.
}

//class BranchPrediction(implicit p: Parameters) extends BoomBundle()(p)
// Give this to each instruction/uop and pass this down the pipeline to the branch-unit
// This covers the per-instruction info on all cfi-related predictions.
// TODO rename to make clear this is "BPD->RenameSnapshot->BRU". Should NOT go into Issue Windows.
class BranchPredInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val btb_blame         = Bool() // Does the BTB get credit for the prediction? (during BRU check).
   val btb_hit           = Bool() // this instruction was the br/jmp predicted by the BTB
   val btb_taken         = Bool() // this instruction was the br/jmp predicted by the BTB and was taken

   val bpd_blame         = Bool() // Does the BPD get credit for this prediction? (during BRU check).
   val bpd_hit           = Bool() // did the bpd predict this instruction? (ie, tag hit in the BPD)
   val bpd_taken         = Bool() // did the bpd predict taken for this instruction?

   val bim_resp         = new BimResp
   val bpd_resp         = new BpdResp // TODO XXX this can be very expensive -- don't give to every instruction? And break into separate toBRU/Exe and toCom versions.
}

class BranchPredictionStage(fetch_width: Int)(implicit p: Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
{
   val io = IO(new BoomBundle()(p)
   {
      // Fetch0
      val btb_req       = Valid(new freechips.rocketchip.rocket.BTBReq).flip
      val fqenq_valid   = Bool(INPUT)
      val debug_fqenq_pc= UInt(INPUT, width = vaddrBitsExtended) // For debug -- make sure I$ and BTB are synchronised.
      val debug_fqenq_ready = Bool(INPUT)

      // Fetch1

      // Fetch2
      val f2_btb_resp   = Valid(new BTBsaResp)
      val f2_stall      = Bool(INPUT) // f3 is not ready -- back-pressure the f2 stage.
      val f2_replay     = Bool(INPUT) // I$ is replaying S2 PC into S0 again (S2 backed up or failed).
      val f2_redirect   = Bool(INPUT) // I$ is being redirected from F2.

      // Fetch3
      val f3_bpd_resp   = Valid(new BpdResp)
      val f3_btb_update = Valid(new BTBsaUpdate).flip
      val f3_ras_update = Valid(new RasUpdate).flip
      val f3_stall      = Bool(INPUT) // f4 is not ready -- back-pressure the f3 stage.
      val f3_enq_valid  = Bool(INPUT) // f2 stage may proceed into the f3 stage.

      // Fetch4
      val f4_redirect   = Bool(INPUT) // I$ is being redirected from F4.

      // Commit
      val bim_update    = Valid(new BimUpdate).flip

      // Other
      val br_unit       = new BranchUnitResp().asInput
      val fe_clear      = Bool(INPUT) // The FrontEnd needs to be cleared (due to redirect or flush).
      val ftq_restore   = Valid(new RestoreHistory).flip
      val brob          = new BrobBackendIo(fetch_width)
      val flush         = Bool(INPUT) // pipeline flush from ROB TODO CODEREVIEW (redudant with fe_clear?)
      val redirect      = Bool(INPUT)
      val status_prv    = UInt(INPUT, width = freechips.rocketchip.rocket.PRV.SZ)
      val status_debug  = Bool(INPUT)
   })

   //************************************************
   // construct all of the modules

   val btb = Module(new BTBsa())
   val bpd = BrPredictor(tileParams, boomParams)

   btb.io.status_debug := io.status_debug
   bpd.io.status_prv := io.status_prv


   //************************************************
   // Branch Prediction (BP0 Stage)

   btb.io.req := io.btb_req
   //btb.io.icmiss := false.B //XXX TODO switch out to s2_replay?


   //************************************************
   // Branch Prediction (BP1 Stage)

   bpd.io.req := io.btb_req
   bpd.io.f2_replay := io.f2_replay


   //************************************************
   // Branch Prediction (BP2 Stage)

   // Enqueue predictions into a buffer to match timings (and back-pressures) of
   // the ICache.

   val btb_queue = withReset(reset || io.flush || io.redirect)
      { Module(new ShiftQueue(Valid(new BTBsaResp), 5, flow = true)) }

   btb_queue.io.deq.ready := !io.f2_stall

   btb_queue.io.enq.valid := io.fqenq_valid
   btb_queue.io.enq.bits := btb.io.resp

   assert (btb_queue.io.enq.ready === io.debug_fqenq_ready, "[bpd-pipeline] mismatch between BTB-Q and I$ readies.")

   when (io.fqenq_valid)
   {
      assert (btb.io.resp.bits.fetch_pc(15,0) === io.debug_fqenq_pc(15,0),
         "[bpd-pipeline] mismatch between BTB and I$.")
   }

   // CODEREVIEW: this gets wonky --- we need to override payload's valid if queue's valid not true.
   val f2_btb = Wire(init=btb_queue.io.deq.bits); f2_btb.valid := btb_queue.io.deq.valid && btb_queue.io.deq.bits.valid
   val f2_pc = f2_btb.bits.fetch_pc
   val f2_aligned_pc = ~(~f2_pc | (UInt(fetch_width*coreInstBytes-1)))
   val f2_nextline_pc = Wire(UInt(width=vaddrBits))
   f2_nextline_pc := f2_aligned_pc + UInt(fetch_width*coreInstBytes)

   io.f2_btb_resp := btb_queue.io.deq.bits; io.f2_btb_resp.valid := btb_queue.io.deq.valid && btb_queue.io.deq.bits.valid


   bpd.io.fqenq_valid:= io.fqenq_valid
   bpd.io.f2_bim_resp := io.f2_btb_resp.bits.bim_resp

   //************************************************
   // Branch Prediction (BP3 Stage)

   bpd.io.resp.ready := !io.f3_stall

   // does the BPD predict a taken branch?
   //private def bitRead(bits: UInt, offset: UInt): Bool = (bits >> offset)(0)

   val bpd_valid = bpd.io.resp.valid
   val bpd_bits = bpd.io.resp.bits

   io.f3_bpd_resp.valid := bpd.io.resp.valid
   io.f3_bpd_resp.bits := bpd.io.resp.bits






   //************************************************
   // Update the RAS

   // update RAS based on BTB's prediction information (or the branch-check correction).
   val jmp_idx = f2_btb.bits.cfi_idx

   btb.io.ras_update := io.f3_ras_update
   btb.io.ras_update.valid := false.B // TODO XXX renable RAS (f2_btb.valid || io.f3_ras_update.valid) && !io.fetch_stalled
   when (f2_btb.valid)
   {
      btb.io.ras_update.bits.is_call      := BpredType.isCall(f2_btb.bits.bpd_type)
      btb.io.ras_update.bits.is_ret       := BpredType.isReturn(f2_btb.bits.bpd_type)
      btb.io.ras_update.bits.return_addr  := f2_aligned_pc + (jmp_idx << 2.U) + 4.U
   }


   //************************************************
   // Update the BTB/BIM

   btb.io.btb_update :=
      Mux(io.br_unit.btb_update.valid,
         io.br_unit.btb_update,
         io.f3_btb_update)

   btb.io.bim_update := io.bim_update


   //************************************************
   // Update the BPD

   bpd.io.ftq_restore <> io.ftq_restore
//   bpd.io.flush := io.flush
//   bpd.io.redirect := io.redirect
   bpd.io.f2_redirect := io.f2_redirect
   bpd.io.f4_redirect := io.f4_redirect
   bpd.io.fe_clear := io.fe_clear
//   bpd.io.f3_clear := io.f3_clear
   bpd.io.f3enq_valid := io.f3_enq_valid
   bpd.io.f2_stall := io.f2_stall
   // TODO hook up update logic
//   bpd.io.commit


   //************************************************
   // Handle redirects/flushes

   btb.io.flush := io.flush || reset.toBool || io.redirect

   when (io.flush || reset.toBool)
   {
      btb.io.btb_update.valid := false.B
   }


   //************************************************
   // printfs

   if (DEBUG_PRINTF)
   {
      printf("btb, f0_npc=%c req_pc 0x%x, f1=%c targ=0x%x\n"
         , Mux(btb.io.req.valid, Str("V"), Str("-"))
         , io.btb_req.bits.addr
         , Mux(btb.io.resp.valid, Str("V"), Str("-"))
         , btb.io.resp.bits.target
         )
   }

   //************************************************
   // asserts

   if (!enableBTB)
   {
      assert (!(io.f2_btb_resp.valid), "[bpd_pipeline] BTB predicted, but it's been disabled.")
   }

}
