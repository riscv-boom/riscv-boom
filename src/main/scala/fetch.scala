//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Fetch Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

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

   val pred_resp   = new BranchPredictionResp
   val predictions = Vec(FETCH_WIDTH, new BranchPrediction)

   val debug_events = Vec(FETCH_WIDTH, new DebugStageEvents)

  override def cloneType: this.type = new FetchBundle().asInstanceOf[this.type]
}


class FetchUnit(fetch_width: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new BoomBundle()(p)
   {
      val imem              = new rocket.FrontendIO
      val br_unit           = new BranchUnitResp().asInput

      val tsc_reg           = UInt(INPUT, xLen)

      val bp2_take_pc       = Bool(INPUT)
      val bp2_is_taken      = Bool(INPUT)
      val bp2_br_seen       = Bool(INPUT)
      val bp2_is_jump       = Bool(INPUT)
      val bp2_is_cfi        = Bool(INPUT)
      val bp2_pred_resp     = new BranchPredictionResp().asInput
      val bp2_predictions   = Vec(fetch_width, new BranchPrediction()).asInput
      val bp2_pc_of_br_inst = UInt(INPUT, vaddrBits+1)
      val bp2_pred_target   = UInt(INPUT, vaddrBits+1)

      val kill              = Bool(INPUT)
      val com_exception     = Bool(INPUT)
      val flush_take_pc     = Bool(INPUT)
      val flush_pc          = UInt(INPUT, vaddrBits+1)
      val csr_take_pc       = Bool(INPUT)
      val csr_evec          = UInt(INPUT, vaddrBits+1)

      val fetchbuffer_kill  = Bool(OUTPUT)
      val stalled           = Bool(OUTPUT)
      val resp              = new DecoupledIO(new FetchBundle)
   }

   val fseq_reg = Reg(init = UInt(0, xLen))
   val if_pc_next = Wire(UInt(width = vaddrBits+1))

   val br_unit = io.br_unit


   val fetch_bundle = Wire(new FetchBundle())

   val FetchBuffer = Module(new Queue(gen=new FetchBundle,
                                entries=FETCH_BUFFER_SZ,
                                pipe=false,
                                flow=p(EnableFetchBufferFlowThrough),
                                _reset=(io.fetchbuffer_kill || reset.toBool)))

   if (O3PIPEVIEW_PRINTF)
   {
      when (FetchBuffer.io.enq.fire())
      {
         fseq_reg := fseq_reg + PopCount(FetchBuffer.io.enq.bits.mask)
         for (i <- 0 until fetch_width)
         {
            when (fetch_bundle.mask(i))
            {
               // TODO for now, manually set the fetch_tsc to point to when the fetch
               // started. This doesn't properly account for i-cache and i-tlb misses. :(
               printf("%d; O3PipeView:fetch:%d:0x%x:0:%d:DASM(%x)\n",
                  fetch_bundle.debug_events(i).fetch_seq,
                  io.tsc_reg - UInt(2*O3_CYCLE_TIME),
                  (fetch_bundle.pc.toSInt & SInt(-(fetch_width*coreInstBytes))).toUInt + UInt(i << 2),
                  fetch_bundle.debug_events(i).fetch_seq,
                  fetch_bundle.insts(i))
            }
         }
      }
   }

   val if_stalled = !(FetchBuffer.io.enq.ready) // if FetchBuffer backs up, we have to stall the front-end

   val take_pc = br_unit.take_pc ||
                 io.flush_take_pc ||
                 io.csr_take_pc ||
                 (io.bp2_take_pc && !if_stalled) // TODO this seems way too low-level to get backpressure signal correct

   io.imem.req.valid   := take_pc // tell front-end we had an unexpected change in the stream
   io.imem.req.bits.pc := if_pc_next
   io.imem.req.bits.speculative := !(io.csr_take_pc || io.flush_take_pc)
   io.imem.resp.ready  := !(if_stalled) // TODO perf BUG || take_pc?

   if_pc_next := Mux(io.com_exception || io.csr_take_pc, io.csr_evec,
                 Mux(io.flush_take_pc                  , io.flush_pc,
                 Mux(br_unit.take_pc                   , br_unit.target(vaddrBits,0),
                                                         io.bp2_pred_target))) // bp2_take_pc

   // Fetch Buffer
   FetchBuffer.io.enq.valid := io.imem.resp.valid && !io.fetchbuffer_kill
   FetchBuffer.io.enq.bits  := fetch_bundle
   io.fetchbuffer_kill      := io.kill || io.com_exception || io.csr_take_pc

   fetch_bundle.pc := io.imem.resp.bits.pc
   fetch_bundle.xcpt_if := io.imem.resp.bits.xcpt_if
   fetch_bundle.replay_if := io.imem.resp.bits.replay

   for (i <- 0 until fetch_width)
   {
      fetch_bundle.insts(i) := io.imem.resp.bits.data(i*coreInstBits+coreInstBits-1,i*coreInstBits)

      if (i == 0)
         fetch_bundle.debug_events(i).fetch_seq := fseq_reg
      else
         fetch_bundle.debug_events(i).fetch_seq := fseq_reg +
            PopCount(fetch_bundle.mask.toBits()(i-1,0))
   }

   if (p(EnableBTB))
   {
      io.imem.btb_update.valid := (br_unit.btb_update_valid ||
                                    (io.bp2_take_pc && io.bp2_is_taken && !if_stalled && !br_unit.take_pc)) &&
                                  !io.flush_take_pc &&
                                  !io.csr_take_pc
   }
   else
   {
      io.imem.btb_update.valid := Bool(false)
   }

   // update the BTB
   // If a branch is mispredicted and taken, update the BTB.
   // (if branch unit mispredicts, instructions in decode are no longer valid)
   io.imem.btb_update.bits.pc         := Mux(br_unit.btb_update_valid, br_unit.btb_update.pc, io.imem.resp.bits.pc)
   io.imem.btb_update.bits.br_pc      := Mux(br_unit.btb_update_valid, br_unit.btb_update.br_pc, io.bp2_pc_of_br_inst)
   io.imem.btb_update.bits.target     := Mux(br_unit.btb_update_valid, br_unit.btb_update.target,
                                                                       (io.bp2_pred_target.toSInt &
                                                                        SInt(-coreInstBytes)).toUInt)
   io.imem.btb_update.bits.prediction := Mux(br_unit.btb_update_valid, br_unit.btb_update.prediction,
                                                                       io.imem.resp.bits.btb)
   io.imem.btb_update.bits.taken      := Mux(br_unit.btb_update_valid, br_unit.btb_update.taken,
                                                                       io.bp2_take_pc && io.bp2_is_taken && !if_stalled)
   io.imem.btb_update.bits.isJump     := Mux(br_unit.btb_update_valid, br_unit.btb_update.isJump, io.bp2_is_jump)
   io.imem.btb_update.bits.isReturn   := Mux(br_unit.btb_update_valid, br_unit.btb_update.isReturn, Bool(false))
   io.imem.btb_update.bits.isValid    := Mux(br_unit.btb_update_valid, Bool(true), io.bp2_is_cfi)

   // Update the BHT in the BP2 stage.
   // Also update the BHT in the Exe stage IF and only if the branch is a misprediction.
   // TODO move this into the bpd_pipeline
   val bp2_bht_update = Wire(Valid(new rocket.BHTUpdate()).asOutput)
   bp2_bht_update.valid           := io.imem.resp.valid && io.bp2_br_seen && !if_stalled && !br_unit.take_pc
   bp2_bht_update.bits.prediction := io.imem.resp.bits.btb
   bp2_bht_update.bits.pc         := io.imem.resp.bits.pc
   bp2_bht_update.bits.taken      := Mux(io.bp2_take_pc,
                                       io.bp2_is_taken,
                                       io.imem.resp.bits.btb.valid && io.imem.resp.bits.btb.bits.taken)
   bp2_bht_update.bits.mispredict := io.bp2_take_pc

   io.imem.bht_update := Mux(br_unit.brinfo.valid &&
                             br_unit.brinfo.mispredict &&
                             RegNext(br_unit.bht_update.valid),
                           RegNext(br_unit.bht_update),
                           bp2_bht_update)


   // bp2 stage
   fetch_bundle.mask := io.imem.resp.bits.mask & io.bp2_pred_resp.mask
   fetch_bundle.pred_resp := io.bp2_pred_resp
   fetch_bundle.predictions := io.bp2_predictions

   // output
   io.stalled := if_stalled
   io.resp <> FetchBuffer.io.deq


   //-------------------------------------------------------------
   if (DEBUG_PRINTF)
   {
      // Fetch Stage 1
      printf("BrPred1:    (IF1_PC= 0x%x Predict:n/a) ------ PC: [%c%c%c-%c for br_id:(n/a), %c %c next: 0x%x ifst:%d]\n"
         , io.imem.npc
         , Mux(br_unit.brinfo.valid, Str("V"), Str("-"))
         , Mux(br_unit.brinfo.taken, Str("T"), Str("-"))
         , Mux(br_unit.debug_btb_pred, Str("B"), Str("_"))
         , Mux(br_unit.brinfo.mispredict, Str("M"), Str(" "))
         // chisel3 lacks %s support
         //, Mux(br_unit.brinfo.mispredict, Str(b_mgt + "MISPREDICT" + end), Str(grn + "          " + end))
         //, bpd_stage.io.req.bits.idx
         , Mux(take_pc, Str("T"), Str(" "))
         //, Mux(take_pc, Str("TAKE_PC"), Str(" "))
         , Mux(io.flush_take_pc, Str("F"),
           Mux(br_unit.take_pc, Str("B"),
           Mux(io.bp2_take_pc && !if_stalled, Str("P"),
           Mux(io.bp2_take_pc, Str("J"),
                              Str(" ")))))
//         , Mux(io.flush_take_pc, Str("FLSH"),
//           Mux(br_unit.take_pc, Str("BRU "),
//           Mux(io.bp2_take_pc && !if_stalled, Str("BP2"),
//           Mux(io.bp2_take_pc, Str("J-s"),
//                              Str(" ")))))
         , if_pc_next
         , if_stalled)

      // Fetch Stage 2
//      printf("I$ Response: (%s) IF2_PC= 0x%x (mask:0x%x) \u001b[1;35m TODO need Str in Chisel3\u001b[0m  ----BrPred2:(%s,%s,%d) [btbtarg: 0x%x] jkilmsk:0x%x ->(0x%x)\n"
//      printf("I$ Response: (%s) IF2_PC= 0x%x (mask:0x%x) [1;35m TODO need Str in Chisel3[0m  ----BrPred2:(%s,%s,%d) [btbtarg: 0x%x] jkilmsk:0x%x ->(0x%x)\n"

      printf("I$ Response: (%c) IF2_PC= 0x%x (mask:0x%x) "
//         , Mux(io.imem.resp.valid && !io.kill, Str(mgt + "v" + end), Str(grn + "-" + end))
         , Mux(io.imem.resp.valid && !io.kill, Str("v"), Str("-"))
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

      printf("----BrPred2:(%c,%c,%d) [btbtarg: 0x%x] jkilmsk:0x%x ->(0x%x)\n"
         , Mux(io.imem.resp.bits.btb.valid, Str("H"), Str("-"))
         , Mux(io.imem.resp.bits.btb.bits.taken, Str("T"), Str("-"))
         , io.imem.resp.bits.btb.bits.bridx
         , io.imem.resp.bits.btb.bits.target(19,0)
         , io.bp2_pred_resp.mask
         , fetch_bundle.mask
         )
   }
}
