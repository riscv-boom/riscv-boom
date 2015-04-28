//**************************************************************************
// RISCV Processor Branch Prediction Pipeline
//--------------------------------------------------------------------------
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
       
package BOOM
{

import Chisel._
import Node._

import rocket.Str


class BranchPrediction (fetch_width: Int) extends BOOMCoreBundle 
{
   val target = UInt(width = vaddrBits+1) 
   val br_pc  = UInt(width = vaddrBits+1) // PC of the instruction changing control flow
   val idx    = UInt(width = log2Up(fetch_width)) // idx of br in fetch bundle
  override def clone = new BranchPrediction(fetch_width).asInstanceOf[this.type]
}

class BranchPredictionStage (fetch_width: Int) extends Module with BOOMCoreParameters
{
   val io = new BOOMCoreBundle
   {
      val imem       = new rocket.CPUFrontendIO
      val prediction = Decoupled(new BranchPrediction(fetch_width))
      val ras_update = Valid(new rocket.RASUpdate)
      val br_unit    = new BranchUnitResp().asInput
      val kill       = Bool(INPUT) // e.g., pipeline flush
   }

   //-------------------------------------------------------------
   // Branch Prediction (BP1 Stage)

   val r_bht_predictions = Vec.fill(fetch_width) {Reg(Bool())}

   var br_predictor: BrPredictor = null 
                                                  
   if (ENABLE_BRANCH_PREDICTOR)
   {
      br_predictor = Module(new TwobcBrPredictor(num_entries = 1024))
      br_predictor.io.curr_pc := io.imem.resp.bits.pc

      br_predictor.io.update <> io.br_unit.bpd_update
      r_bht_predictions.map(_ := br_predictor.io.resp.taken)
      
   }
   else
   {
      r_bht_predictions.map(_ := Bool(false))
   }

   //-------------------------------------------------------------
   // Branch Decode (BP2 Stage)
   //
   // 1) Which branch to take?
   // 2) Is there a jal earlier to take?
   // 3) Does the BTB override our prediction?
   //    - 3b) if no, verify BTB is correct?
   // 4) Update RAS
  
   // round off to nearest fetch boundary
   val aligned_pc = io.imem.resp.bits.pc & SInt(-(fetch_width*coreInstBytes))
    
   val is_br     = Vec.fill(fetch_width) {Bool()}
   val is_jal    = Vec.fill(fetch_width) {Bool()}
   val is_jr     = Vec.fill(fetch_width) {Bool()}
   val br_targs  = Vec.fill(fetch_width) {UInt(width=vaddrBits+1)}
   val jal_targs = Vec.fill(fetch_width) {UInt(width=vaddrBits+1)}
 
   for (i <- 0 until fetch_width)
   {
      val inst = io.imem.resp.bits.data(i)
      val bpd_decoder = Module(new BranchDecode)
      bpd_decoder.io.inst := inst

      is_br(i)  := bpd_decoder.io.is_br   && io.imem.resp.bits.mask(i)
      is_jal(i) := bpd_decoder.io.is_jal  && io.imem.resp.bits.mask(i) 
      is_jr(i)  := bpd_decoder.io.is_jalr && io.imem.resp.bits.mask(i) 

      val pc = aligned_pc + UInt(i << 2)
      br_targs(i)  := ComputeBranchTarget(pc, inst, xLen, coreInstBytes)
      jal_targs(i) := ComputeJALTarget(pc, inst, xLen, coreInstBytes)
   }


   //-------------------------------------------------------------
   // Output
    
   val predictions = is_br.toBits & r_bht_predictions.toBits
   val br_val  = predictions.orR
   val br_idx  = PriorityEncoder(predictions)
   val jal_val = is_jal.reduce(_|_)
   val jal_idx = PriorityEncoder(is_jal.toBits)
   val br_wins = br_val && (!jal_val || (br_idx < jal_idx))
   val btb_overrides = io.imem.btb_resp.valid && 
                       io.imem.btb_resp.bits.taken && 
                       (io.imem.btb_resp.bits.bridx <= io.prediction.bits.idx)

   io.prediction.valid       := io.imem.resp.valid && (br_val || jal_val) && !btb_overrides
   io.prediction.bits.target := Mux(br_wins, br_targs(br_idx), jal_targs(jal_idx))
   io.prediction.bits.idx    := Mux(br_wins, br_idx, jal_idx)
   io.prediction.bits.br_pc  := aligned_pc + (io.prediction.bits.idx << UInt(2))

   //-------------------------------------------------------------
   // Look for CALL and RETURN for RAS shenanigans.
   // TODO flush_take_pc should probably be given to the branch unit, instead of resetting it here?
   // NOTE: what about branch taken earlier?

   val jumps    = is_jal.toBits | is_jr.toBits
   val jmp_idx  = PriorityEncoder(jumps)
   val jmp_inst = io.imem.resp.bits.data(jmp_idx)
   val is_call  = IsCall(jmp_inst)
   //val is_ret   = IsReturn(jmp_inst)
   io.imem.ras_update.valid           := io.imem.resp.valid &&
                                         jumps.orR && 
                                         !br_wins &&
                                         !io.prediction.ready &&
                                         !io.kill
   io.imem.ras_update.bits.isCall     := is_call
   io.imem.ras_update.bits.isReturn   := !is_call
   io.imem.ras_update.bits.returnAddr := aligned_pc + (jmp_idx << UInt(2)) + UInt(4)
   io.imem.ras_update.bits.prediction := io.imem.btb_resp
 
   //-------------------------------------------------------------

   if (DEBUG_PRINTF)
   {
      printf("bp2_aligned_pc: 0x%x BHT:(%s 0x%x, %d) p:%x (%d) b:%x j:%x (%d) %s %s\n" 
         , aligned_pc, Mux(io.prediction.valid, Str("TAKE"), Str(" -- ")), io.prediction.bits.target, io.prediction.bits.idx
         , predictions.toBits, br_idx, is_br.toBits, is_jal.toBits, jal_idx, Mux(br_wins, Str("BR"), Str("JA")), Mux(btb_overrides, Str("BO"), Str("--"))
         )
   }
 
   //-------------------------------------------------------------
   // does the BP2 stage get to change the pc? Or does the BTB's actions win?
   // The BTB wins if it predicts UNLESS BP2 redirects a jump that's earlier than the BTB's prediction.
//   val override_btb = bp2_val &&
//                      bp2_jal_val &&
//                      io.imem.btb_resp.valid
//                      (bp2_pred_idx < io.imem.btb_resp.bits.bridx)

//   bp2_take_pc := bp2_wants_to_take_pc &&
//                  (!(io.imem.btb_resp.valid && io.imem.btb_resp.bits.taken) || bp2_bht_overrides_btb)
       
   //-------------------------------------------------------------
   // TODO assert to catch if a JAL gets by (like behind a JALR) that doesn't redirect
   // (bne, jal) -> verify the JAL is predicted taken if the bne isn't
    

   // TODO assert here?
   // It's the job of the BHT to verify that if the BTB predicts on a JAL, it got it right.
   // It must also check that the BTB didn't miss the JAL and predict on a later branch

//   // did the BTB predict JAL *AND* was it the first JAL in the fetch packet
//   val btb_predicted_our_jal = io.imem.btb_resp.valid &&
//                               io.imem.btb_resp.bits.taken &&
//                               bp2_jal_val &&
//                               (bp2_jmp_idx === io.imem.btb_resp.bits.bridx)
//
          


}

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------


}
