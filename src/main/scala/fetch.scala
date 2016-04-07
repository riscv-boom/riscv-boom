package boom

import Chisel._
import Node._
import cde.Parameters

import rocket.Instructions._
import rocket.Str
import uncore.HtifIO


class BoomFetch(
    io:                 BoomIO,
    br_unit:            BranchUnitResp,
    reset:              Bool,
    flush_pipeline:     Bool,
    fseq_reg:           UInt,
    tsc_reg:            UInt,
    flush_take_pc:      Bool,
    flush_pc:           UInt,
    csr_take_pc:        Bool,
    csr_evec:           UInt,
    bp2_take_pc:        Bool,
    bp2_is_jump:        Bool,
    bp2_is_taken:       Bool,
    bp2_pred_target:    UInt,
    bp2_br_seen:        Bool,
    bp2_pc_of_br_inst:  UInt,
    com_exception:      Bool,
    if_pc_next:         UInt,
    com_valids:         Vec[Bool],
    com_uops:           Vec[MicroOp],
    FetchBuffer:        Queue[FetchBundle]
  )(implicit val p: Parameters) extends HasBoomCoreParameters
{

   val kill_frontend = br_unit.brinfo.mispredict || flush_pipeline

   val fetchbuffer_kill = Wire(Bool())
   private val fetch_bundle = Wire(new FetchBundle())

   val bundle = fetch_bundle


   if (O3PIPEVIEW_PRINTF)
   {
      when (FetchBuffer.io.enq.fire())
      {
         fseq_reg := fseq_reg + PopCount(FetchBuffer.io.enq.bits.mask)
         for (i <- 0 until FETCH_WIDTH)
         {
            when (fetch_bundle.mask(i))
            {
               // TODO for now, manually set the fetch_tsc to point to when the fetch
               // started. This doesn't properly account for i-cache and i-tlb misses. :(
               printf("%d; O3PipeView:fetch:%d:0x%x:0:%d:DASM(%x)\n",
                  fetch_bundle.debug_events(i).fetch_seq,
                  tsc_reg - UInt(2*O3_CYCLE_TIME),
                  (fetch_bundle.pc & SInt(-(FETCH_WIDTH*coreInstBytes))) + UInt(i << 2),
                  fetch_bundle.debug_events(i).fetch_seq,
                  fetch_bundle.insts(i))
            }
         }
      }
   }

   val if_stalled = Wire(Bool()) // if FetchBuffer backs up, we have to stall the front-end
   if_stalled := !(FetchBuffer.io.enq.ready)

   val take_pc = br_unit.take_pc ||
                 flush_take_pc ||
                 csr_take_pc ||
                 (bp2_take_pc && !if_stalled) // TODO this seems way too low-level, to get this backpressure signal correct

   io.imem.req.valid   := take_pc // tell front-end we had an unexpected change in the stream
   io.imem.req.bits.pc := if_pc_next
   io.imem.resp.ready  := !(if_stalled) // TODO perf BUG || take_pc?

   if_pc_next :=  Mux(com_exception || csr_take_pc, csr_evec,
                  Mux(flush_take_pc               , flush_pc,
                  Mux(br_unit.take_pc             , br_unit.target(vaddrBits,0),
                                                    bp2_pred_target))) // bp2_take_pc

   // Fetch Buffer
   FetchBuffer.io.enq.valid := io.imem.resp.valid && !fetchbuffer_kill
   FetchBuffer.io.enq.bits  := fetch_bundle
   fetchbuffer_kill         := br_unit.brinfo.mispredict || com_exception || flush_pipeline || csr_take_pc

   fetch_bundle.pc := io.imem.resp.bits.pc
   fetch_bundle.xcpt_if := io.imem.resp.bits.xcpt_if

   for (i <- 0 until FETCH_WIDTH)
   {
      fetch_bundle.insts(i) := io.imem.resp.bits.data(i)

      if (i == 0)
         fetch_bundle.debug_events(i).fetch_seq := fseq_reg
      else
         fetch_bundle.debug_events(i).fetch_seq := fseq_reg +
            PopCount(fetch_bundle.mask.toBits()(i-1,0))
   }

   if (p(EnableBTB))
   {
      io.imem.btb_update.valid := (br_unit.btb_update_valid ||
                                    (bp2_take_pc && bp2_is_taken && !if_stalled && !br_unit.take_pc)) &&
                                  !flush_take_pc &&
                                  !csr_take_pc
   }
   else
   {
      io.imem.btb_update.valid := Bool(false)
   }

   // update the BTB
   // If a branch is mispredicted and taken, update the BTB.
   // (if branch unit mispredicts, instructions in decode are no longer valid)
   io.imem.btb_update.bits.pc         := Mux(br_unit.btb_update_valid, br_unit.btb_update.pc, io.imem.resp.bits.pc)
   io.imem.btb_update.bits.br_pc      := Mux(br_unit.btb_update_valid, br_unit.btb_update.br_pc, bp2_pc_of_br_inst)
   io.imem.btb_update.bits.target     := Mux(br_unit.btb_update_valid, br_unit.btb_update.target,
                                                                       bp2_pred_target & SInt(-coreInstBytes))
   io.imem.btb_update.bits.prediction := Mux(br_unit.btb_update_valid, br_unit.btb_update.prediction, io.imem.btb_resp)
   io.imem.btb_update.bits.taken      := Mux(br_unit.btb_update_valid, br_unit.btb_update.taken,
                                                                       bp2_take_pc && bp2_is_taken && !if_stalled)
   io.imem.btb_update.bits.isJump     := Mux(br_unit.btb_update_valid, br_unit.btb_update.isJump, bp2_is_jump)
   io.imem.btb_update.bits.isReturn   := Mux(br_unit.btb_update_valid, br_unit.btb_update.isReturn, Bool(false))

   // Update the BHT in the BP2 stage.
   // Also update the BHT in the Exe stage IF and only if the branch is a misprediction.
   // TODO move this into the bpd_pipeline
   val bp2_bht_update = Wire(Valid(new rocket.BHTUpdate()).asOutput)
   bp2_bht_update.valid           := io.imem.resp.valid && bp2_br_seen && !if_stalled && !br_unit.take_pc
   bp2_bht_update.bits.prediction := io.imem.btb_resp
   bp2_bht_update.bits.pc         := io.imem.resp.bits.pc
   bp2_bht_update.bits.taken      := Mux(bp2_take_pc,
                                       bp2_is_taken,
                                       io.imem.btb_resp.valid && io.imem.btb_resp.bits.taken)
   bp2_bht_update.bits.mispredict := bp2_take_pc

   io.imem.bht_update := Mux(br_unit.brinfo.valid &&
                             br_unit.brinfo.mispredict &&
                             RegNext(br_unit.bht_update.valid),
                           RegNext(br_unit.bht_update),
                           bp2_bht_update)

   io.imem.invalidate := Range(0,DECODE_WIDTH).map{i => com_valids(i) && com_uops(i).is_fencei}.reduce(_|_)
 }
