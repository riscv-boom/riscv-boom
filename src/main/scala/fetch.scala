//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Fetch Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Stages:
//    * F0 -- next PC select
//    * F1 -- icache SRAM access
//    * F2 -- icache response/pre-decode/branch-checking/verification
//    * F3 -- redirect
//
// NOTES:
//    Things like the branch-check rely on register retiming (since the
//    branch-check happens in F2, but doesn't redirect the pipeline until F3.

package boom

import Chisel._
import freechips.rocketchip.config.Parameters

import freechips.rocketchip.util.Str
import freechips.rocketchip.util.UIntToAugmentedUInt

class FetchBundle(implicit p: Parameters) extends BoomBundle()(p)
{
   val pc            = UInt(width = vaddrBitsExtended)
   val insts         = Vec(FETCH_WIDTH, Bits(width = 32))
   val mask          = Bits(width = FETCH_WIDTH) // mark which words are valid instructions
   val xcpt_pf_if    = Bool() // I-TLB miss (instruction fetch fault).
   val xcpt_ae_if    = Bool() // Access exception.
   val replay_if     = Bool() // the I$ demands we replay the instruction fetch.
   val xcpt_ma_if_oh = UInt(width = FETCH_WIDTH)
                            // A cfi branched to a misaligned address --
                            // one-hit encoding (1:1 with insts).

   val bpu_info    = Vec(FETCH_WIDTH, new BranchPredInfo)

   val debug_events = Vec(FETCH_WIDTH, new DebugStageEvents)

  override def cloneType: this.type = new FetchBundle().asInstanceOf[this.type]
}


class FetchUnit(fetch_width: Int)(implicit p: Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
{
   val io = IO(new BoomBundle()(p)
   {
      val imem              = new freechips.rocketchip.rocket.FrontendIO
      val f2_bpu_request    = Valid(new BpuRequest).flip

      val f2_btb_resp       = Valid(new BTBsaResp).flip
      val f2_bpd_resp       = Valid(new BpdResp).flip
      val f2_ras_update     = Valid(new RasUpdate)
      val f3_bpd_resp       = Valid(new BpdResp).flip
      val f3_btb_update     = Valid(new BTBsaUpdate)
      val f3_hist_update    = Valid(new GHistUpdate)
      val f3_bim_update     = Valid(new BimUpdate)

      val br_unit           = new BranchUnitResp().asInput

      val tsc_reg           = UInt(INPUT, xLen)

      val clear_fetchbuffer = Bool(INPUT)

      val flush_take_pc     = Bool(INPUT)
      val flush_pc          = UInt(INPUT, vaddrBits+1)

      // sfence needs to steal the TLB CAM part.
      val sfence_take_pc    = Bool(INPUT)
      val sfence_addr       = UInt(INPUT, vaddrBits+1)

      val resp              = new DecoupledIO(new FetchBundle)
      val stalled           = Bool(OUTPUT) // CODE REVIEW
   })

   val bchecker = Module (new BranchChecker(fetchWidth))
   val FetchBuffer = Module(new Queue(gen=new FetchBundle,
                                entries=fetchBufferSz,
                                pipe=false,
                                flow=enableFetchBufferFlowThrough,
                                _reset=(io.clear_fetchbuffer || reset.toBool)))

   val br_unit = io.br_unit
   val fseq_reg = Reg(init = UInt(0, xLen))
   val f0_redirect_pc = Wire(UInt(width = vaddrBitsExtended))

   val f2_valid = Wire(Bool())
   val f2_req = Wire(Valid(new PCReq()))
   val f2_fetch_bundle = Wire(new FetchBundle)

   val f3_valid = Reg(init=false.B)
   val f3_req = Reg(Valid(new PCReq()))
   val f3_fetch_bundle = Reg(new FetchBundle())
   val f3_taken = Reg(Bool())
   val f3_btb_hit = Reg(Bool())
   val f3_br_seen = Reg(init=false.B)
   val f3_jr_seen = Reg(init=false.B)

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
      io.sfence_take_pc ||
      (io.f2_btb_resp.valid && io.f2_btb_resp.bits.taken) ||
      (io.f2_bpu_request.valid && !if_stalled && io.imem.resp.valid) ||
      (f3_req.valid && !if_stalled) // TODO this seems way too low-level to get backpressure signal correct

   io.imem.req.valid   := f0_redirect_val // tell front-end we had an unexpected change in the stream
   io.imem.req.bits.pc := f0_redirect_pc
   io.imem.req.bits.speculative := !(io.flush_take_pc)
   io.imem.resp.ready  := !(if_stalled)

   f0_redirect_pc :=
      Mux(io.sfence_take_pc,
         io.sfence_addr,
      Mux(io.flush_take_pc,
         io.flush_pc,
      Mux(br_unit.take_pc,
         br_unit.target(vaddrBits,0),
      Mux(io.f2_btb_resp.valid && io.f2_btb_resp.bits.taken,
         io.f2_btb_resp.bits.target,
      Mux(f3_req.valid || !enableBpdF2Redirect.B,
         f3_req.bits.addr,
         io.f2_bpu_request.bits.target)))))

   //-------------------------------------------------------------
   // **** ICache Access (F1) ****
   //-------------------------------------------------------------

   // twiddle thumbs

   //-------------------------------------------------------------
   // **** ICache Response/Pre-decode (F2) ****
   //-------------------------------------------------------------

   // round off to nearest fetch boundary
   val f2_aligned_pc = ~(~io.imem.resp.bits.pc | (UInt(fetch_width*coreInstBytes-1)))

   val is_br     = Wire(Vec(fetch_width, Bool()))
   val is_jal    = Wire(Vec(fetch_width, Bool()))
   val is_jr     = Wire(Vec(fetch_width, Bool()))
   val is_call   = Wire(Vec(fetch_width, Bool()))
   val br_targs  = Wire(Vec(fetch_width, UInt(width=vaddrBitsExtended)))
   val jal_targs = Wire(Vec(fetch_width, UInt(width=vaddrBitsExtended)))
   // catch misaligned jumps -- let backend handle misaligned
   // branches though since only taken branches are exceptions.
   val jal_targs_ma = Wire(Vec(fetch_width, Bool()))

   for (i <- 0 until fetch_width)
   {
      val bpd_decoder = Module(new BranchDecode)

      val inst = io.imem.resp.bits.data(i*coreInstBits+coreInstBits-1,i*coreInstBits)
      bpd_decoder.io.inst := inst
      f2_fetch_bundle.insts(i) := inst

      val pc = f2_aligned_pc + UInt(i << 2)
      is_br(i) := io.imem.resp.valid && bpd_decoder.io.is_br && io.imem.resp.bits.mask(i)
      is_jal(i) := io.imem.resp.valid && bpd_decoder.io.is_jal  && io.imem.resp.bits.mask(i)
      is_jr(i) := io.imem.resp.valid && bpd_decoder.io.is_jalr  && io.imem.resp.bits.mask(i)
      is_call(i) := io.imem.resp.valid && IsCall(inst) && io.imem.resp.bits.mask(i)
      br_targs(i) := ComputeBranchTarget(pc, inst, xLen)
      jal_targs(i) := ComputeJALTarget(pc, inst, xLen)
      jal_targs_ma(i) := jal_targs(i)(1) && is_jal(i)
   }

   val f2_br_seen = io.imem.resp.valid &&
                  !io.imem.resp.bits.xcpt.pf.inst &&
                  is_br.reduce(_|_) &&
                  (!is_jal.reduce(_|_) || (PriorityEncoder(is_br.asUInt) < PriorityEncoder(is_jal.asUInt))) &&
                  (!is_jr.reduce(_|_) || (PriorityEncoder(is_br.asUInt) < PriorityEncoder(is_jr.asUInt)))
   val f2_jr_seen = io.imem.resp.valid &&
                  !io.imem.resp.bits.xcpt.pf.inst &&
                  is_jr.reduce(_|_) &&
                  (!is_jal.reduce(_|_) || (PriorityEncoder(is_jr.asUInt) < PriorityEncoder(is_jal.asUInt)))


   // Does the BPD have a prediction to make (in the case of a BTB miss?)
   // Calculate in F2 but don't redirect until F3.
   val f2_bpd_predictions = is_br.asUInt & io.f2_bpd_resp.bits.takens
   val f2_bpd_br_taken = f2_bpd_predictions.orR
   val f2_bpd_br_idx = PriorityEncoder(f2_bpd_predictions)
   val f2_bpd_target = br_targs(f2_bpd_br_idx)
   // check for jumps -- if we decide to override a taken BTB and choose "nextline" we don't want to miss the JAL.
   val f2_has_jal = is_jal.reduce(_|_)
   val f2_jal_idx = PriorityEncoder(is_jal.asUInt)
   val f2_jal_target = jal_targs(f2_jal_idx)
   val f2_bpd_btb_update_valid = Wire(init=false.B) // does the BPD's choice cause a BTB update?
   val f2_bpd_may_redirect_taken = Wire(init=false.B) // request towards a taken branch target
   val f2_bpd_may_redirect_next = Wire(init=false.B) // override taken prediction and fetch the next line (or take JAL)
   val f2_bpd_may_redirect = f2_bpd_may_redirect_taken || f2_bpd_may_redirect_next
   val f2_bpd_redirect_cfiidx =
      Mux(f2_bpd_may_redirect_taken,
         f2_bpd_br_idx,
      Mux(f2_has_jal,
         f2_jal_idx,
         (fetch_width-1).U))
   val f2_bpd_redirect_target =
      Mux(f2_bpd_may_redirect_taken,
         f2_bpd_target,
      Mux(f2_has_jal,
         f2_jal_target,
         f2_aligned_pc + (fetch_width*coreInstBytes).U))

   if (enableBpdF2Redirect)
   {
      // Listen to BPD only if BTB miss -- otherwise BPD in F2 has handled the BTB hit scenario.
      f2_bpd_may_redirect_taken := io.f2_bpd_resp.valid && f2_bpd_br_taken && !io.f2_btb_resp.valid && enableBpdF3Redirect.B
   }
   else if (!enableBpdF2Redirect && enableBpdF3Redirect)
   {
      when (io.f2_btb_resp.valid)
      {
         // btb made a prediction
         // Make a redirect request if:
         //    - the BPD (br) comes earlier than the BTB's redirection.
         //    - If both the BTB and the BPD predicted a branch, the BPD wins (if disagree).
         //       * involves refetching the next cacheline and undoing the current packet's mask if we "undo" the BT's
         //       taken branch.

         val btb_idx = io.f2_btb_resp.bits.cfi_idx

         when (BpredType.isAlwaysTaken(io.f2_btb_resp.bits.bpd_type))
         {
            f2_bpd_may_redirect_taken := io.f2_bpd_resp.valid && f2_bpd_br_taken && f2_bpd_br_idx < btb_idx

            assert (io.f2_btb_resp.bits.taken)
         }
         .elsewhen (io.f2_btb_resp.bits.taken)
         {
            // does the bpd predict the branch is taken too? (assuming bpd_valid)
            val bpd_agrees_with_btb = f2_bpd_predictions(btb_idx)
            f2_bpd_may_redirect_taken := io.f2_bpd_resp.valid && f2_bpd_br_taken &&
               (f2_bpd_br_idx < btb_idx || !bpd_agrees_with_btb)
               // XXX in this scenario, ignore the btb mask and go with the bpd mask
            f2_bpd_may_redirect_next := io.f2_bpd_resp.valid && !f2_bpd_br_taken

            assert (BpredType.isBranch(io.f2_btb_resp.bits.bpd_type))
         }
         .elsewhen (!io.f2_btb_resp.bits.taken)
         {
            f2_bpd_may_redirect_taken := io.f2_bpd_resp.valid && f2_bpd_br_taken
         }
      }
      .otherwise
      {
         // BTB made no prediction - let the BPD do what it wants
         f2_bpd_may_redirect_taken := io.f2_bpd_resp.valid && f2_bpd_br_taken
         // add branch to the BTB if we think it will be taken
         f2_bpd_btb_update_valid := f2_bpd_may_redirect_taken
      }

      assert (PopCount(Vec(f2_bpd_may_redirect_taken, f2_bpd_may_redirect_next)) <= UInt(1),
         "[bpd_pipeline] mutually-exclusive signals firing")
   }

   // catch any BTB mispredictions (and fix-up missed JALs)
   bchecker.io.is_valid  := Vec(io.imem.resp.bits.mask.toBools)
   bchecker.io.imem_resp_valid := io.imem.resp.valid
   bchecker.io.is_br  := is_br
   bchecker.io.is_jal := is_jal
   bchecker.io.is_jr  := is_jr
   bchecker.io.is_call  := is_call
   bchecker.io.br_targs := br_targs
   bchecker.io.jal_targs := jal_targs
   bchecker.io.fetch_pc := io.imem.resp.bits.pc
   bchecker.io.aligned_pc := f2_aligned_pc
   bchecker.io.btb_resp := io.f2_btb_resp
   bchecker.io.bpd_resp := io.f2_bpd_resp
   bchecker.io.f2_bpu_request := io.f2_bpu_request

   // who wins? bchecker or bpd? or jal?
   val jal_overrides_bpd = f2_has_jal && f2_jal_idx < f2_bpd_redirect_cfiidx && f2_bpd_may_redirect_taken
   val f2_bpd_overrides_bcheck =
      f2_bpd_may_redirect &&
      !jal_overrides_bpd &&
      (!bchecker.io.req.valid || (f2_bpd_redirect_cfiidx < bchecker.io.req_cfi_idx))
   f2_req.valid := (bchecker.io.req.valid || (f2_bpd_may_redirect && !jal_overrides_bpd)) && f2_valid && !(f0_redirect_val && !io.f2_bpu_request.valid)
   f2_req.bits.addr := Mux(f2_bpd_overrides_bcheck, f2_bpd_redirect_target, bchecker.io.req.bits.addr)

   val f2_btb_update_bits = Wire(new BTBsaUpdate)
   io.f3_btb_update.valid := RegNext(bchecker.io.btb_update.valid || f2_bpd_btb_update_valid)
   io.f3_btb_update.bits := RegNext(f2_btb_update_bits)
   f2_btb_update_bits := bchecker.io.btb_update.bits
   when (f2_bpd_overrides_bcheck)
   {
      f2_btb_update_bits.target := f2_bpd_target
      f2_btb_update_bits.cfi_pc := f2_bpd_br_idx << log2Up(coreInstBytes)
      f2_btb_update_bits.bpd_type := BpredType.branch
      f2_btb_update_bits.cfi_type := CfiType.branch
   }


   io.f2_ras_update := bchecker.io.ras_update

   // mask out instructions after predicted branch
   val f2_kill_mask = KillMask(
      f2_req.valid,
      Mux(f2_bpd_overrides_bcheck, f2_bpd_redirect_cfiidx, bchecker.io.req_cfi_idx),
      fetchWidth)



   val btb_mask = Mux(io.f2_btb_resp.valid && !io.f2_bpu_request.valid && !f2_req.valid,
                  io.f2_btb_resp.bits.mask,
                  Fill(fetchWidth, UInt(1,1)))
   val bpd_mask = Mux(io.f2_bpu_request.valid && !f2_req.valid,
                  io.f2_bpu_request.bits.mask,
                  Fill(fetchWidth, UInt(1,1)))
   f2_fetch_bundle.mask := io.imem.resp.bits.mask & ~f2_kill_mask & btb_mask & bpd_mask


   val f2_taken = Wire(init=false.B) // was a branch taken in the F2 stage?
   when (f2_req.valid)
   {
      // f2_bpd only requests taken redirections on btb misses.
      // f2_req via bchecker only ever requests nextline_pc or jump targets (which we don't track in ghistory).
      f2_taken := Mux(f2_bpd_overrides_bcheck, (f2_bpd_may_redirect_taken && !jal_overrides_bpd), false.B)
   }
   .elsewhen (io.f2_bpu_request.valid)
   {
      f2_taken := io.f2_bpd_resp.bits.takens(io.f2_btb_resp.bits.cfi_idx)
   }
   .elsewhen (io.f2_btb_resp.valid)
   {
      f2_taken := io.f2_btb_resp.bits.taken
   }

   f2_valid := io.imem.resp.valid && !io.clear_fetchbuffer && !f3_req.valid
   f2_fetch_bundle.pc := io.imem.resp.bits.pc
   f2_fetch_bundle.xcpt_pf_if := io.imem.resp.bits.xcpt.pf.inst
   f2_fetch_bundle.xcpt_ae_if := io.imem.resp.bits.xcpt.ae.inst
   f2_fetch_bundle.replay_if := io.imem.resp.bits.replay
   f2_fetch_bundle.xcpt_ma_if_oh := jal_targs_ma.asUInt

   assert (!(io.imem.resp.valid && io.imem.resp.bits.xcpt.ae.inst)) // TODO what is ae?

   for (w <- 0 until fetch_width)
   {
      f2_fetch_bundle.bpu_info(w).btb_blame     := false.B
      f2_fetch_bundle.bpu_info(w).btb_hit       := io.f2_btb_resp.valid
      f2_fetch_bundle.bpu_info(w).btb_taken     := false.B

      f2_fetch_bundle.bpu_info(w).bpd_blame     := false.B
      f2_fetch_bundle.bpu_info(w).bpd_hit       := io.f2_bpd_resp.valid
      f2_fetch_bundle.bpu_info(w).bpd_taken     := io.f2_bpd_resp.bits.takens(w.U)
      f2_fetch_bundle.bpu_info(w).bim_resp      := io.f2_btb_resp.bits.bim_resp
      f2_fetch_bundle.bpu_info(w).bpd_resp      := io.f2_bpd_resp.bits

      when (w.U === f2_bpd_br_idx && f2_bpd_overrides_bcheck)
      {
         f2_fetch_bundle.bpu_info(w).bpd_blame := true.B
      }
      .elsewhen (w.U === io.f2_btb_resp.bits.cfi_idx && io.f2_bpu_request.valid && !f2_req.valid)
      {
         f2_fetch_bundle.bpu_info(w).bpd_blame := true.B
      }
      .elsewhen (w.U === io.f2_btb_resp.bits.cfi_idx && io.f2_btb_resp.valid && !f2_req.valid)
      {
         f2_fetch_bundle.bpu_info(w).btb_blame := true.B
      }

      when (w.U === io.f2_btb_resp.bits.cfi_idx && io.f2_btb_resp.valid)
      {
         f2_fetch_bundle.bpu_info(w).btb_taken := io.f2_btb_resp.bits.taken
      }
   }


   //-------------------------------------------------------------
   // **** F3 ****
   //-------------------------------------------------------------

   // Rely on register-retiming to move a lot of the work in the F2 stage to here.

   when (io.clear_fetchbuffer)
   {
      f3_valid := false.B
      f3_req.valid := false.B
      f3_br_seen := false.B
      f3_jr_seen := false.B
   }
   .elsewhen (!if_stalled)
   {
      f3_valid := f2_valid
      f3_fetch_bundle := f2_fetch_bundle
      f3_req := f2_req
      f3_taken := f2_taken
      f3_btb_hit := io.f2_btb_resp.valid
      f3_br_seen := f2_br_seen
      f3_jr_seen := f2_jr_seen

      for (w <- 0 until fetch_width)
      {
         if (!ENABLE_VLHR) {
            f3_fetch_bundle.bpu_info(w).bpd_resp.history.get := io.f3_bpd_resp.bits.history.get
         }
         f3_fetch_bundle.bpu_info(w).bpd_resp.history_ptr := io.f3_bpd_resp.bits.history_ptr
      }
   }

   io.f3_hist_update.valid := f3_valid && (f3_br_seen || f3_jr_seen) && !if_stalled
   io.f3_hist_update.bits.taken := f3_jr_seen || f3_taken

   io.f3_bim_update.valid := f3_valid && f3_req.valid && f3_btb_hit
   io.f3_bim_update.bits.taken :=  f3_taken
   // HACK: all instructions in the bundle get the same bim_resp, so just read the first.
   io.f3_bim_update.bits.bim_resp := f3_fetch_bundle.bpu_info(0).bim_resp


   //-------------------------------------------------------------
   // **** FetchBuffer Enqueue ****
   //-------------------------------------------------------------

   // Fetch Buffer
   FetchBuffer.io.enq.valid := f3_valid
   FetchBuffer.io.enq.bits  := f3_fetch_bundle


   for (i <- 0 until fetch_width)
   {
      if (i == 0) {
         FetchBuffer.io.enq.bits.debug_events(i).fetch_seq := fseq_reg
      } else {
         FetchBuffer.io.enq.bits.debug_events(i).fetch_seq := fseq_reg +
            PopCount(f3_fetch_bundle.mask.asUInt()(i-1,0))
      }
   }

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
                  io.tsc_reg - UInt(2*O3_CYCLE_TIME),
                  (bundle.pc.asSInt & SInt(-(fetch_width*coreInstBytes))).asUInt + UInt(i << 2),
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

   // check if enqueue'd PC is a target of the previous valid enqueue'd PC.

   // clear checking if misprediction/flush/etc.
   val last_valid = Reg(init=false.B)
   val last_pc = Reg(UInt(width=vaddrBitsExtended))
   val last_target = Reg(UInt(width=vaddrBitsExtended))
   val last_nextlinepc = Reg(UInt(width=vaddrBitsExtended))
   val last_cfi_type = Reg(UInt(width=CfiType.SZ))

   val cfi_idx = (fetch_width-1).U - PriorityEncoder(Reverse(f3_fetch_bundle.mask))
   val fetch_pc = f3_fetch_bundle.pc
   val curr_aligned_pc = ~(~fetch_pc | (UInt(fetch_width*coreInstBytes-1)))
   val cfi_pc = curr_aligned_pc  + (cfi_idx << 2.U)

   when (FetchBuffer.io.enq.fire() &&
      !f3_fetch_bundle.replay_if &&
      !f3_fetch_bundle.xcpt_pf_if &&
      !f3_fetch_bundle.xcpt_ae_if)
   {
      assert (f3_fetch_bundle.mask =/= 0.U)
      val curr_inst = if (fetchWidth == 0) f3_fetch_bundle.insts(0) else f3_fetch_bundle.insts(cfi_idx)
      last_valid := true.B
      last_pc := cfi_pc
      last_nextlinepc := curr_aligned_pc + UInt(fetch_width*coreInstBytes)

      val cfi_type = GetCfiType(curr_inst)
      last_cfi_type := cfi_type
      last_target := Mux(cfi_type === CfiType.jal,
         ComputeJALTarget(cfi_pc, curr_inst, xLen),
         ComputeBranchTarget(cfi_pc, curr_inst, xLen))

      when (last_valid)
      {
         // check for error
         when (last_cfi_type === CfiType.none)
         {
            assert (fetch_pc ===  last_nextlinepc,
               "[fetch] A non-cfi instruction is followed by the wrong instruction.")
         }
         .elsewhen (last_cfi_type === CfiType.jal)
         {
            // ignore misaligned fetches -- we should have marked the instruction as excepting,
            // but when it makes a misaligned fetch request the I$ gives us back an aligned PC.
            val f_pc = fetch_pc(vaddrBitsExtended-1, log2Up(coreInstBytes))
            val targ = last_target(vaddrBitsExtended-1, log2Up(coreInstBytes))
            when (f_pc =/= targ) {
               printf("about to abort: [fetch] JAL is followed by the wrong instruction.")
               printf("fetch_pc: 0x%x, last_target: 0x%x, last_nextlinepc: 0x%x\n",
                  fetch_pc, last_target, last_nextlinepc)
            }
            assert (f_pc === targ, "[fetch] JAL is followed by the wrong instruction.")
         }
         .elsewhen (last_cfi_type === CfiType.branch)
         {
            // again, ignore misaligned fetches -- an exception should be caught.
            val f_pc = fetch_pc(vaddrBitsExtended-1, log2Up(coreInstBytes))
            val targ = last_target(vaddrBitsExtended-1, log2Up(coreInstBytes))
            assert (fetch_pc === last_nextlinepc || f_pc === targ,
               "[fetch] branch is followed by the wrong instruction.")
         }
         .otherwise
         {
            // we can't verify JALR instruction stream integrity --  /throws hands up.
            assert (last_cfi_type === CfiType.jalr, "[fetch] Should be a JALR if none of the others were valid.")
         }
      }
   }

   when (io.clear_fetchbuffer || 
      (FetchBuffer.io.enq.fire() && 
         (f3_fetch_bundle.replay_if || f3_fetch_bundle.xcpt_pf_if || f3_fetch_bundle.xcpt_ae_if)))
   {
      last_valid := false.B
   }

   when (FetchBuffer.io.enq.fire() && 
      !f3_fetch_bundle.replay_if && 
      !f3_fetch_bundle.xcpt_pf_if &&
      !f3_fetch_bundle.xcpt_ae_if)
   {
      // check that, if there is a jal, the last valid instruction is not after him.
      // <beq, jal, bne, ...>, either the beq or jal may be the last instruction, but because
      // the jal dominates everything after it, nothing valid can be after it.
      val f3_is_jal = Vec(f3_fetch_bundle.insts map (GetCfiType(_) === CfiType.jal)).asUInt & f3_fetch_bundle.mask
      val f3_jal_idx = PriorityEncoder(f3_is_jal)
      val has_jal = f3_is_jal.orR

      assert (!(has_jal && f3_jal_idx < cfi_idx), "[fetch] JAL was not taken.")
   }


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
// Also look at BPD's full prediction and decide to use it if no BTB hit.
// Combinational logic.
// If an error is found, redirect the front-end to refetch and correct the misprediction.
// Incoming signals may be garbage (if f2_valid not true); consumer will have to handle that scenario.
class BranchChecker(fetch_width: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new Bundle
   {
      val req           = Valid(new PCReq)

      val is_valid      = Vec(fetch_width, Bool()).asInput // valid instruction mask from I$
      val is_br         = Vec(fetch_width, Bool()).asInput
      val is_jal        = Vec(fetch_width, Bool()).asInput
      val is_jr         = Vec(fetch_width, Bool()).asInput
      val is_call       = Vec(fetch_width, Bool()).asInput
      val imem_resp_valid= Bool(INPUT)

      val br_targs      = Vec(fetch_width, UInt(width=vaddrBitsExtended)).asInput
      val jal_targs     = Vec(fetch_width, UInt(width=vaddrBitsExtended)).asInput
      val fetch_pc      = UInt(INPUT, width=vaddrBitsExtended)
      val aligned_pc    = UInt(INPUT, width=vaddrBitsExtended)

      val btb_resp      = Valid(new BTBsaResp).flip
      val bpd_resp      = Valid(new BpdResp).flip
      val f2_bpu_request= Valid(new BpuRequest).flip

      val btb_update    = Valid(new BTBsaUpdate)
      val ras_update    = Valid(new RasUpdate)

      val req_cfi_idx   = UInt(OUTPUT, width = log2Up(fetchWidth)) // where is cfi we are predicting?
   })

   // Did the BTB mispredict the cfi type?
   // Did the BTB mispredict the cfi target?
   // Did the BTB predict a masked-off instruction?
   val wrong_cfi = Wire(init = false.B)
   val wrong_target = Wire(init = false.B)

   val btb_idx = io.btb_resp.bits.cfi_idx
   val btb_target = io.btb_resp.bits.target.sextTo(vaddrBitsExtended)
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
         assert (!io.btb_resp.bits.cfi_type === CfiType.none, "[fetch] predicted on a non-cfi type.")
      }
   }
   .otherwise
   {
      assert (!io.f2_bpu_request.valid, "[fetch] F2 redirect despite no BTB hit.")
   }

   val nextline_pc = io.aligned_pc + UInt(fetch_width*coreInstBytes)

   when (io.f2_bpu_request.valid && io.imem_resp_valid)
   {
      // f2 stage (BPD) decided to disagree with the BTB.
      assert (io.btb_resp.valid, "[fetch] f2 redirect but no btb hit.")
      assert (io.btb_resp.bits.taken || bpd_predicted_taken, "[fetch] redirecting means one of these should be true.")
      assert (!bpd_predicted_taken || btb_target === io.f2_bpu_request.bits.target,
         "[fetch] redirecting target disagrees with BTB -- and it should have come from the BTB.")
      assert (bpd_predicted_taken || nextline_pc === io.f2_bpu_request.bits.target,
         "[fetch] BPD is redirecting PC to not-taken, but seems confused on what PC we're actually on!")
   }

   val btb_was_wrong = io.btb_resp.valid && (wrong_cfi || wrong_target || !io.is_valid(btb_idx))

   val f2_bpu_req_valid = io.f2_bpu_request.valid && enableBpdF2Redirect.B

   val jal_idx = PriorityEncoder(io.is_jal.asUInt)
   val btb_hit  = io.btb_resp.valid
   val jal_wins = io.is_jal.reduce(_|_) &&
      (!btb_hit ||
      btb_was_wrong ||
      (jal_idx < btb_idx) ||
      (!f2_bpu_req_valid && !io.btb_resp.bits.taken) ||
      (f2_bpu_req_valid && !bpd_predicted_taken))

   //-------------------------------------------------------------
   // Perform redirection & updates

   // Redirect if:
   //    - JAL comes before BT's cfi_idx
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

