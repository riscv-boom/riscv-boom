//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Fetch Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// f0 = next PC select
// f1 = icache SRAM access
// f2 = icache response/pre-decode
// f3 = redirect

package boom

import Chisel._
import cde.Parameters

import util.Str

class FetchBundle(implicit p: Parameters) extends BoomBundle()(p)
{
   val pc          = UInt(width = vaddrBits+1)
   val insts       = Vec(FETCH_WIDTH, Bits(width = 32))
   val mask        = Bits(width = FETCH_WIDTH) // mark which words are valid instructions
   val xcpt_if     = Bool()
   val replay_if   = Bool()

//   val pred_resp   = new BranchPredictionResp
//   val predictions = Vec(FETCH_WIDTH, new BranchPrediction)
   val bpu_info    = Vec(FETCH_WIDTH, new BranchPredInfo)

   val debug_events = Vec(FETCH_WIDTH, new DebugStageEvents)

  override def cloneType: this.type = new FetchBundle().asInstanceOf[this.type]
}


class FetchUnit(fetch_width: Int)(implicit p: Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
{
   val io = new BoomBundle()(p)
   {
      val imem              = new rocket.FrontendIO
      val f0_btb            = Valid(new BTBsaResp).flip
      val f2_bpu_info       = Valid(new BTBsaResp).flip

      val br_unit           = new BranchUnitResp().asInput

      val tsc_reg           = UInt(INPUT, xLen)

      val clear_fetchbuffer = Bool(INPUT)

      val flush_take_pc     = Bool(INPUT)
      val flush_pc          = UInt(INPUT, vaddrBits+1)

      val resp              = new DecoupledIO(new FetchBundle)
      val stalled           = Bool(OUTPUT) // CODE REVIEW
   }

   val bchecker = Module (new BranchChecker(fetchWidth))
   val FetchBuffer = Module(new Queue(gen=new FetchBundle,
                                entries=fetchBufferSz,
                                pipe=false,
                                flow=enableFetchBufferFlowThrough,
                                _reset=(io.clear_fetchbuffer || reset.toBool)))

   val br_unit = io.br_unit
   val fseq_reg = Reg(init = UInt(0, xLen))
   val f0_redirect_pc = Wire(UInt(width = vaddrBits+1))

   val f2_valid = Wire(Bool())
   val f2_req = Wire(Valid(new PCReq()))
   val f2_fetch_bundle = Wire(new FetchBundle)

   val f3_valid = Reg(init=Bool(false))
   val f3_req = Reg(Valid(new PCReq()))
   val f3_fetch_bundle = Reg(new FetchBundle())

   //-------------------------------------------------------------
   // **** Helper Functions ****
   //-------------------------------------------------------------

   private def KillMask(m_enable: Bool, m_idx: UInt, m_width: Int): UInt =
   {
      val mask = Wire(Bits(width = m_width))
      mask := Fill(m_width, m_enable) & (Fill(m_width, UInt(1)) << UInt(1) << m_idx)
      mask
   }

   //-------------------------------------------------------------
   // **** NextPC Select (F0) ****
   //-------------------------------------------------------------

   val if_stalled = !(FetchBuffer.io.enq.ready) // if FetchBuffer backs up, we have to stall the front-end
   io.stalled := if_stalled


   val f0_redirect_val =
      br_unit.take_pc ||
      io.flush_take_pc ||
      (f3_req.valid && !if_stalled) // TODO this seems way too low-level to get backpressure signal correct

   io.imem.req.valid   := f0_redirect_val // tell front-end we had an unexpected change in the stream
   io.imem.req.bits.pc := f0_redirect_pc
   io.imem.req.bits.speculative := !(io.flush_take_pc)
   io.imem.resp.ready  := !(if_stalled)

   f0_redirect_pc := 
      Mux(io.flush_take_pc, io.flush_pc,
      Mux(br_unit.take_pc,  br_unit.target(vaddrBits,0),
         f3_req.bits.addr))

   io.imem.ext_btb.resp := io.f0_btb

   //-------------------------------------------------------------
   // **** ICache Access (F1) ****
   //-------------------------------------------------------------

   // twiddle thumbs

   //-------------------------------------------------------------
   // **** ICache Response/Pre-decode (F2) ****
   //-------------------------------------------------------------

   f2_valid := io.imem.resp.valid && !io.clear_fetchbuffer && !f3_req.valid
   f2_fetch_bundle.pc := io.imem.resp.bits.pc
   f2_fetch_bundle.mask := io.imem.resp.bits.mask
   f2_fetch_bundle.xcpt_if := io.imem.resp.bits.xcpt_if
   f2_fetch_bundle.replay_if := io.imem.resp.bits.replay


   for (w <- 0 until fetch_width)
   {
      f2_fetch_bundle.bpu_info(w).btb_hit   := Bool(false)
      f2_fetch_bundle.bpu_info(w).btb_taken := Bool(false)

      when (UInt(w) === io.f2_bpu_info.bits.cfi_idx && io.f2_bpu_info.valid)
      {
         f2_fetch_bundle.bpu_info(w).btb_hit   := Bool(true)
         f2_fetch_bundle.bpu_info(w).btb_taken := io.f2_bpu_info.bits.taken
      }
   }


   // round off to nearest fetch boundary
   val aligned_pc = ~(~io.imem.resp.bits.pc | (UInt(fetch_width*coreInstBytes-1)))
   val is_br     = Wire(Vec(fetch_width, Bool()))
   val is_jal    = Wire(Vec(fetch_width, Bool()))
   val is_jr     = Wire(Vec(fetch_width, Bool()))
   val br_targs  = Wire(Vec(fetch_width, UInt(width=vaddrBitsExtended)))
   val jal_targs = Wire(Vec(fetch_width, UInt(width=vaddrBitsExtended)))

   for (i <- 0 until fetch_width)
   {
      val bpd_decoder = Module(new BranchDecode)

      val inst = io.imem.resp.bits.data(i*coreInstBits+coreInstBits-1,i*coreInstBits)
      bpd_decoder.io.inst := inst
      f2_fetch_bundle.insts(i) := inst

      val pc = aligned_pc + UInt(i << 2)
      is_br(i) := io.imem.resp.valid && bpd_decoder.io.is_br && io.imem.resp.bits.mask(i)
      is_jal(i) := io.imem.resp.valid && bpd_decoder.io.is_jal  && io.imem.resp.bits.mask(i)
      is_jr(i) := io.imem.resp.valid && bpd_decoder.io.is_jalr  && io.imem.resp.bits.mask(i)
      br_targs(i) := ComputeBranchTarget(pc, inst, xLen, coreInstBytes)
      jal_targs(i) := ComputeJALTarget(pc, inst, xLen, coreInstBytes)

      if (i == 0) {
         f2_fetch_bundle.debug_events(i).fetch_seq := fseq_reg
      } else {
         f2_fetch_bundle.debug_events(i).fetch_seq := fseq_reg +
            PopCount(f2_fetch_bundle.mask.toBits()(i-1,0))
      }
   }


   // catch any BTB mispredictions (and fix-up missed JALs)
   bchecker.io.is_br  := is_br
   bchecker.io.is_jal := is_jal
   bchecker.io.is_jr  := is_jr
   bchecker.io.br_targs := br_targs
   bchecker.io.jal_targs := jal_targs
   bchecker.io.nextline_pc := aligned_pc + UInt(fetch_width*coreInstBytes)
   bchecker.io.bpu_info := io.f2_bpu_info
   // TODO, fix up f2_bpu_info


   f2_req.valid := bchecker.io.req.valid && !f0_redirect_val && f2_valid
   f2_req.bits  := bchecker.io.req.bits

   // mask out instructions after predicted branch
   val btb_kill_mask = KillMask(f2_req.valid, bchecker.io.cfi_idx, fetchWidth)
   val f2_kill_mask = KillMask(f2_req.valid, bchecker.io.cfi_idx, fetchWidth)

   val btb_mask = Mux(f2_req.valid || !io.f2_bpu_info.valid,
                  Fill(fetchWidth, UInt(1,1)),
                  io.f2_bpu_info.bits.mask)
   f2_fetch_bundle.mask := io.imem.resp.bits.mask & ~f2_kill_mask & btb_mask

   //-------------------------------------------------------------
   // **** F3 ****
   //-------------------------------------------------------------

   when (io.clear_fetchbuffer)
   {
      f3_valid := Bool(false)
      f3_req.valid := Bool(false)
   }
   .elsewhen (!if_stalled)
   {
      f3_valid := f2_valid
      f3_fetch_bundle := f2_fetch_bundle
      f3_req := f2_req
//      f3_req := Mux(bchecker.io.req.valid, bchecker.io.req, f2_req)
   }


   //-------------------------------------------------------------
   // **** FetchBuffer Enqueue ****
   //-------------------------------------------------------------

   // Fetch Buffer
   FetchBuffer.io.enq.valid := f3_valid
   FetchBuffer.io.enq.bits  := f3_fetch_bundle


//   fetch_bundle.mask := io.imem.resp.bits.mask //& io.bp2_pred_resp.mask
//   fetch_bundle.pred_resp := io.bp2_pred_resp
//   fetch_bundle.predictions := io.bp2_predictions

   // We do not use the imem's BTB.
   io.imem.btb_update.valid := Bool(false)

   //-------------------------------------------------------------
   // **** Frontend Response ****
   //-------------------------------------------------------------

   io.resp <> FetchBuffer.io.deq


   //-------------------------------------------------------------
   // **** Pipeview Support ****
   //-------------------------------------------------------------

   if (O3PIPEVIEW_PRINTF)
   {
      when (FetchBuffer.io.enq.fire())
      {
         fseq_reg := fseq_reg + PopCount(FetchBuffer.io.enq.bits.mask)
         val bundle = FetchBuffer.io.enq.bits
         for (i <- 0 until fetch_width)
         {
            when (bundle.mask(i))
            {
               // TODO for now, manually set the fetch_tsc to point to when the fetch
               // started. This doesn't properly account for i-cache and i-tlb misses. :(
               // Also not factoring in NPC.
               printf("%d; O3PipeView:fetch:%d:0x%x:0:%d:DASM(%x)\n",
                  bundle.debug_events(i).fetch_seq,
                  io.tsc_reg - UInt(1*O3_CYCLE_TIME),
                  (bundle.pc.toSInt & SInt(-(fetch_width*coreInstBytes))).toUInt + UInt(i << 2),
                  bundle.debug_events(i).fetch_seq,
                  bundle.insts(i))
            }
         }
      }

   }


   //-------------------------------------------------------------
   // **** Assertions ****
   //-------------------------------------------------------------

   assert (!(io.imem.resp.bits.btb.valid), "[bpd_pipeline] BTB predicted, but it's been disabled.")

   //-------------------------------------------------------------
   // **** Printfs ****
   //-------------------------------------------------------------

   if (DEBUG_PRINTF)
   {
      // Fetch Stage 1
      printf("BrPred1:    (IF1_PC= 0x%x Predict:n/a) ------ PC: [%c%c-%c for br_id:(n/a), %c %c next: 0x%x ifst:%d]\n"
         , io.imem.npc
         , Mux(br_unit.brinfo.valid, Str("V"), Str("-"))
         , Mux(br_unit.brinfo.taken, Str("T"), Str("-"))
         , Mux(br_unit.brinfo.mispredict, Str("M"), Str(" "))
         , Mux(f0_redirect_val, Str("T"), Str(" "))
         , Mux(io.flush_take_pc, Str("F"),
           Mux(br_unit.take_pc, Str("B"), Str(" ")))
         , f0_redirect_pc
         , if_stalled)

      // Fetch Stage 2
      printf(" Fetch2 : (%c) 0x%x I$ Response <<-- IF2_PC (mask:0x%x) "
         , Mux(io.imem.resp.valid, Str("V"), Str("-"))
         , io.imem.resp.bits.pc
         , io.imem.resp.bits.mask
         )

      if (fetch_width == 1)
      {
         printf("DASM(%x) "
            , io.imem.resp.bits.data(coreInstBits-1,0)
            )
      }
      else if (fetch_width >= 2)
      {
         printf("DASM(%x)DASM(%x) "
            , io.imem.resp.bits.data(coreInstBits-1,0)
            , io.imem.resp.bits.data(2*coreInstBits-1, coreInstBits)
            )
      }

      printf("----BrPred2:(%c,%c,%d) [btbtarg: 0x%x]\n"
         , Mux(io.imem.resp.bits.btb.valid, Str("H"), Str("-"))
         , Mux(io.imem.resp.bits.btb.bits.taken, Str("T"), Str("-"))
         , io.imem.resp.bits.btb.bits.bridx
         , io.imem.resp.bits.btb.bits.target(19,0)
         )

      // Fetch Stage 3
      printf(" Fetch3 : (%c) 0x%x jkilmsk:0x%x ->(0x%x)\n"
         , Mux(FetchBuffer.io.enq.valid, Str("V"), Str("-"))
         , FetchBuffer.io.enq.bits.pc
         , UInt(0) //io.bp2_pred_resp.mask
         , FetchBuffer.io.enq.bits.mask
         )
   }
}


// Verify BTB predicted the type and target of instructions correctly.
// Also catch any JALs and redirect the frontend.
// Combinational logic.
// If an error is found, redirect the front-end to refetch and correct the misprediction.
// Incoming signals may be garbage (if f2_valid not true); consumer will have to handle that scenario.
class BranchChecker(fetch_width: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new Bundle
   {
      val req        = Valid(new PCReq)

      val is_br      = Vec(fetch_width, Bool()).asInput
      val is_jal     = Vec(fetch_width, Bool()).asInput
      val is_jr      = Vec(fetch_width, Bool()).asInput

      val br_targs  = Vec(fetch_width, UInt(width=vaddrBitsExtended)).asInput
      val jal_targs = Vec(fetch_width, UInt(width=vaddrBitsExtended)).asInput
      val nextline_pc = UInt(INPUT, width=vaddrBitsExtended)

      val bpu_info   = Valid(new BTBsaResp).flip
      // TODO only handle if taken and taken_targ != correct_targ


      // pass in F2's thoughts on JALs... or compute it ourselves?
      // pass in PCnextline, in case no-JALs and we want to redirect after a bad-taken

//      val kill_mask = UInt(OUTPUT, width = fetch_width)
      val cfi_idx     = UInt(OUTPUT, width = log2Up(fetchWidth)) // where is cfi we are predicting?

   }

   // If
   // Step #1: did the BTB mispredict the cfi type?
   // Step #2: did the BTB mispredict the cfi target?
   val wrong_cfi = Wire(init = false.B)
   val wrong_target = Wire(init = false.B)

   val btb_idx = io.bpu_info.bits.cfi_idx
   val btb_target = io.bpu_info.bits.target
   when (io.bpu_info.valid)
   {
      when (io.bpu_info.bits.cfi_type === CFIType.branch && io.bpu_info.bits.taken)
      {
         wrong_cfi := !io.is_br(btb_idx)
         wrong_target := io.br_targs(btb_idx) =/= btb_target
      }
      .elsewhen (io.bpu_info.bits.cfi_type === CFIType.jal)
      {
         wrong_cfi := !io.is_jal(btb_idx)
         wrong_target := io.jal_targs(btb_idx) =/= btb_target
      }
      .elsewhen (io.bpu_info.bits.cfi_type === CFIType.jalr)
      {
         wrong_cfi := !io.is_jr(btb_idx)
      }
   }

   val btb_was_wrong = io.bpu_info.valid && (wrong_cfi || wrong_target)

   // Redirect if:
   //    - JAL comes before BT's cfi_idx
   //       * kill everything behind JAL -- including BTB's predinfo
   //    - BTB was wrong
   //       * if JAL, take JAL (if valid instructions available)
   //       * if !JAL, request nextline (set all masks to valid).
   //    - No JAL, BTB correct
   //       * do nothing

   val jal_idx = PriorityEncoder(io.is_jal.toBits)
   val btb_hit  = io.bpu_info.valid
   val jal_wins = io.is_jal.reduce(_|_) && (!btb_hit || btb_was_wrong || (jal_idx < btb_idx))

   io.req.valid := jal_wins || btb_was_wrong
   io.req.bits.addr := Mux(jal_wins, io.jal_targs(jal_idx), io.nextline_pc)
   // Help mask out instructions after predicted cfi.
   io.cfi_idx := Mux(jal_wins, jal_idx, UInt(fetchWidth-1))
}

