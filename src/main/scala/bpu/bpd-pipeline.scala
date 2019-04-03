//******************************************************************************
// Copyright (c) 2017 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Branch Prediction Pipeline
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Access BTB and BPD to feed predictions to the FetchControlUnit.
//
// Stages (these are in parallel with instruction fetch):
//    * F0 - Send in next PC into the BTB and the BPD.
//           Hash the PC with the older history of the BPD.
//    * F1 - Access the BTB RAMs. Do 1st stage of BPD.
//    * F2 - Get resp from BTB (send BIM results to BPD). 2nd stage of BPD.
//    * F3 - Get resp from BPD.
//    * F4 - Other logic

package boom.bpu

import chisel3._
import chisel3.util._
import chisel3.core.withReset

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.{Str, UIntToAugmentedUInt}

import boom.common._
import boom.exu.BranchUnitResp
import boom.util.{PrintUtil}

/**
 * Give this to each instruction/uop and pass this down the pipeline to the branch unit
 * This covers the per-instruction info on all cfi-related predictions.
 */
class BranchPredInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val btb_blame = Bool() // Does the BTB get credit for the prediction? (during BRU check).
   val btb_hit   = Bool() // this instruction was the br/jmp predicted by the BTB
   val btb_taken = Bool() // this instruction was the br/jmp predicted by the BTB and was taken

   val bpd_blame = Bool() // Does the BPD get credit for this prediction? (during BRU check).
   val bpd_hit   = Bool() // did the bpd predict this instruction? (ie, tag hit in the BPD)
   val bpd_taken = Bool() // did the bpd predict taken for this instruction?

   val bim_resp  = new BimResp
   val bpd_resp  = new BpdResp
}

/**
 * Wraps the BoomBTB and BrPredictor into a pipeline that is parallel with the Fetch pipeline.
 *
 * @param fetch_width # of instructions fetched
 */
class BranchPredictionStage(fetch_width: Int)(implicit p: Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
{
   val io = IO(new BoomBundle()(p)
   {
      // Fetch0
      val s0_req            = Flipped(Valid(new freechips.rocketchip.rocket.BTBReq))
      val debug_imemresp_pc = Input(UInt(vaddrBitsExtended.W)) // For debug -- make sure I$ and BTB are synchronised.

      // Fetch1

      // Fetch2
      val f2_valid      = Input(Bool()) // f2 stage may proceed into the f3 stage.
      val f2_btb_resp   = Valid(new BoomBTBResp)
      val f2_stall      = Input(Bool()) // f3 is not ready -- back-pressure the f2 stage.
      val f2_replay     = Input(Bool()) // I$ is replaying S2 PC into S0 again (S2 backed up or failed).
      val f2_redirect   = Input(Bool()) // I$ is being redirected from F2.

      // Fetch3
      val f3_is_br      = Input(Vec(fetch_width, Bool())) // mask of branches from I$
      val f3_bpd_resp   = Valid(new BpdResp)
      val f3_btb_update = Flipped(Valid(new BoomBTBUpdate))
      val f3_ras_update = Flipped(Valid(new RasUpdate))
      val f3_stall      = Input(Bool()) // f4 is not ready -- back-pressure the f3 stage.

      // Fetch4
      val f4_redirect   = Input(Bool()) // I$ is being redirected from F4.
      val f4_taken      = Input(Bool()) // I$ is being redirected from F4 (and it is to take a CFI).

      // Commit
      val bim_update    = Flipped(Valid(new BimUpdate))
      val bpd_update    = Flipped(Valid(new BpdUpdate))

      // Other
      val br_unit_resp  = Input(new BranchUnitResp())
      val fe_clear      = Input(Bool()) // The FrontEnd needs to be cleared (due to redirect or flush).
      val ftq_restore   = Flipped(Valid(new RestoreHistory))
      val flush         = Input(Bool()) // pipeline flush from ROB TODO CODEREVIEW (redudant with fe_clear?)
      val redirect      = Input(Bool())
      val status_prv    = Input(UInt(freechips.rocketchip.rocket.PRV.SZ.W))
      val status_debug  = Input(Bool())
   })

   //************************************************
   // construct all of the modules

   val btb = BoomBTB(boomParams)
   val bpd = BoomBrPredictor(boomParams)

   btb.io.status_debug := io.status_debug
   bpd.io.status_prv := io.status_prv
   bpd.io.do_reset := false.B // TODO

   //************************************************
   // Branch Prediction (F0 Stage)

   btb.io.req := io.s0_req
   bpd.io.req := io.s0_req

   //************************************************
   // Branch Prediction (F1 Stage)

   //************************************************
   // Branch Prediction (F2 Stage)

   // BTB's response isn't valid if there's no instruction from I$ to match against.
   io.f2_btb_resp.valid := btb.io.resp.valid && io.f2_valid
   io.f2_btb_resp.bits := btb.io.resp.bits

   bpd.io.f2_bim_resp := btb.io.resp.bits.bim_resp
   bpd.io.f2_replay := io.f2_replay

   //************************************************
   // Branch Prediction (F3 Stage)

   bpd.io.resp.ready := !io.f3_stall
   io.f3_bpd_resp.valid := bpd.io.resp.valid
   io.f3_bpd_resp.bits := bpd.io.resp.bits

   //************************************************
   // Update the RAS
   // TODO XXX  reenable RAS

   // update RAS based on BTB's prediction information (or the branch-check correction).
//   val jmp_idx = f2_btb.bits.cfi_idx

   btb.io.ras_update := io.f3_ras_update
   btb.io.ras_update.valid := false.B // (f2_btb.valid || io.f3_ras_update.valid) &&
                                      // !io.fetch_stalled
//   when (f2_btb.valid)
//   {
//      btb.io.ras_update.bits.is_call      := BpredType.isCall(f2_btb.bits.bpd_type)
//      btb.io.ras_update.bits.is_ret       := BpredType.isReturn(f2_btb.bits.bpd_type)
//      btb.io.ras_update.bits.return_addr  := f2_aligned_pc + (jmp_idx << 2.U) + 4.U
//   }

   //************************************************
   // Update the BTB/BIM

   // br unit has higher pri than a f3 update
   btb.io.btb_update := Mux(io.br_unit_resp.btb_update.valid,
                            io.br_unit_resp.btb_update,
                            io.f3_btb_update)

   btb.io.bim_update := io.bim_update

   //************************************************
   // Update the BPD

   bpd.io.f2_valid := io.f2_valid
   bpd.io.f2_stall := io.f2_stall
   bpd.io.f2_redirect := io.f2_redirect
   bpd.io.f3_is_br := io.f3_is_br
   bpd.io.f4_redirect := io.f4_redirect
   bpd.io.f4_taken := io.f4_taken
   bpd.io.fe_clear := io.fe_clear
   bpd.io.ftq_restore := io.ftq_restore
   bpd.io.commit := io.bpd_update

   //************************************************
   // Handle redirects/flushes

   btb.io.flush := io.flush || reset.toBool || io.redirect || io.f2_replay

   when (io.flush || reset.toBool)
   {
      btb.io.btb_update.valid := false.B
   }

   //************************************************
   // printfs

   if (DEBUG_PRINTF)
   {
      printf("BPD Pipeline:\n")
      printf("    BTB: F0NPC:(V:%c PC:0x%x) F2RESP:(V:%c TRG:0x%x)\n",
             PrintUtil.ConvertChar(btb.io.req.valid, 'V'),
             io.s0_req.bits.addr,
             PrintUtil.ConvertChar(btb.io.resp.valid, 'V'),
             btb.io.resp.bits.target)
   }

   if (BPU_PRINTF)
   {
     printf("BPD Pipeline:\n")
     printf("    Fetch0: BTB: Req:(V:%c PC:0x%x)\n",
            PrintUtil.ConvertChar(btb.io.req.valid, 'V'),
            btb.io.req.bits.addr)
     printf("    Fetch2: BTB: Resp:(V:%c T:%c ReqPC:0x%x TARG:0x%x)\n",
            PrintUtil.ConvertChar(btb.io.resp.valid, 'V'),
            PrintUtil.ConvertChar(btb.io.resp.bits.taken, 'T'),
            btb.io.resp.bits.fetch_pc,
            btb.io.resp.bits.target)

     val cfiTypeStrings = PrintUtil.CfiTypeChars(btb.io.btb_update.bits.cfi_type)
     val bpdTypeStrings = PrintUtil.BpdTypeChars(btb.io.btb_update.bits.bpd_type)
     printf("    Update: BTB: V:%c From:%c%c PC:0x%x TARG:0x%x T:%c BpdType:%c%c%c%c CfiType:%c%c%c%c\n",
            PrintUtil.ConvertChar(btb.io.btb_update.valid, 'V'),
            PrintUtil.ConvertChar(io.br_unit_resp.btb_update.valid, 'B', 'F'),
            PrintUtil.ConvertChar(io.br_unit_resp.btb_update.valid, 'R', '3'),
            btb.io.btb_update.bits.pc,
            btb.io.btb_update.bits.target,
            PrintUtil.ConvertChar(btb.io.btb_update.bits.taken, 'T'),
            bpdTypeStrings(0),
            bpdTypeStrings(1),
            bpdTypeStrings(2),
            bpdTypeStrings(3),
            cfiTypeStrings(0),
            cfiTypeStrings(1),
            cfiTypeStrings(2),
            cfiTypeStrings(3))
     printf("            RAS: V:%c Call?:%c Ret?:%c RetAddr:0x%x\n",
            PrintUtil.ConvertChar(btb.io.ras_update.valid, 'V'),
            PrintUtil.ConvertChar(btb.io.ras_update.bits.is_call, 'C'),
            PrintUtil.ConvertChar(btb.io.ras_update.bits.is_ret, 'R'),
            btb.io.ras_update.bits.return_addr)
     printf("            BIM: V:%c EntryIdx:%d Cntr:%d Mispred:%c T:%c\n",
            PrintUtil.ConvertChar(btb.io.bim_update.valid, 'V'),
            btb.io.bim_update.bits.entry_idx,
            btb.io.bim_update.bits.cntr_value,
            PrintUtil.ConvertChar(btb.io.bim_update.bits.mispredicted, 'M'),
            PrintUtil.ConvertChar(btb.io.bim_update.bits.taken, 'T'))
     printf("            BPD: V:%c PC:0x%x Hist:0x%x Mispred:%c MissCfiIdx:%x T:%c +PriorInfo\n",
            PrintUtil.ConvertChar(bpd.io.commit.valid, 'V'),
            bpd.io.commit.bits.fetch_pc,
            bpd.io.commit.bits.history,
            PrintUtil.ConvertChar(bpd.io.commit.bits.mispredict, 'M'),
            bpd.io.commit.bits.miss_cfi_idx,
            PrintUtil.ConvertChar(bpd.io.commit.bits.taken, 'T'))
   }

   //************************************************
   // asserts

   when (io.f2_btb_resp.valid)
   {
      assert (io.f2_valid, "[bpd-pipeline] BTB has a valid request but imem.resp is invalid.")
   }

   // forward progress into F3 will be made assuming the BTB is giving valid resp
   when (io.f2_valid && btb.io.resp.valid)
   {
      assert (btb.io.resp.bits.fetch_pc(15,0) === io.debug_imemresp_pc(15,0),
         "[bpd-pipeline] Mismatch between BTB and I$ fetch PCs")
   }

   if (!enableBTB)
   {
      assert (!(io.f2_btb_resp.valid), "[bpd_pipeline] BTB predicted, but it's been disabled.")
   }

   override def toString: String = btb.toString + "\n\n" + bpd.toString
}
