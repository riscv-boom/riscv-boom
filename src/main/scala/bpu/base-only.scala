//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV BaseOnly Branch Predictor
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// This predictor does nothing but make predictions using the base predictor "BIM".
// The BIM is accessed externally and also used to drive the BTB predictions.
// In essense, this predictor uses no history.
//
// This predictor is useful for testing the BIM in isolation, and for demonstrating
// how to build your own predictor that leverages the BIM as a base predictor.


package boom

import Chisel._
import chisel3.core.withReset
import freechips.rocketchip.config.{Parameters, Field}


case class BaseOnlyParameters(
   enabled: Boolean = true
   )

class BaseOnlyResp() extends Bundle
{
   val not_used = Bool()
}

object BaseOnlyBrPredictor
{
   def GetRespInfoSize(p: Parameters, hlen: Int): Int =
   {
      val dummy = new BaseOnlyResp()
      dummy.getWidth
   }
}

class BaseOnlyBrPredictor(
   fetch_width: Int
   )(implicit p: Parameters)
   extends BrPredictor(fetch_width, 8)(p)
{

   println ("\tBuilding no predictor (just using BIM as a base predictor).")


   //------------------------------------------------------------
   // Predictor state.
  
   // We need to buffer up our responses in case of back-pressure.
//   val q_resp_info = withReset(reset || io.flush) { Module(new ElasticReg(Valid(new BpdResp))) }
  

   //------------------------------------------------------------
   // Get prediction.
                
//   val stall := !q_resp_info.io.enq.ready // unused 

   f2_resp.valid := io.f2_bim_resp.valid
   f2_resp.bits.takens := io.f2_bim_resp.bits.getTakens
   f2_resp.bits.info := 0.U

//   q_resp_info.io.enq.valid := io.fqenq_valid
//   q_resp_info.io.enq.bits.valid := io.f2_bim_resp.valid
//   q_resp_info.io.enq.bits.bits.takens := io.f2_bim_resp.bits.getTakens
//   q_resp_info.io.enq.bits.bits.info := 0.U

//   io.resp.valid := q_resp_info.io.deq.valid && q_resp_info.io.deq.bits.valid

//   io.resp.bits.takens := q_resp_info.io.deq.bits.bits.takens
//   io.resp.bits.info := q_resp_info.io.deq.bits.bits.info

//   q_resp_info.io.deq.ready := io.resp.ready


   //------------------------------------------------------------
   // Update counter table.

   // Nothing to update, as the BIM is handled externally.


}

