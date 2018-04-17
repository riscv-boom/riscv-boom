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


package boom.bpu

import Chisel._
import chisel3.core.withReset
import freechips.rocketchip.config.{Parameters, Field}
import boom.util.ElasticReg


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
   // Predictor state (none: use BIM).

   //------------------------------------------------------------
   // Get prediction in F2, buffer, and provide prediction in F3.

   val q_s3_resp = withReset(reset || io.fe_clear || io.f4_redirect)
      {Module(new ElasticReg(Valid(new BimResp)))}

   q_s3_resp.io.enq.valid := io.f2_valid
   q_s3_resp.io.enq.bits := io.f2_bim_resp

   io.resp.valid := q_s3_resp.io.deq.valid && q_s3_resp.io.deq.bits.valid
   io.resp.bits.takens := q_s3_resp.io.deq.bits.bits.getTakens
   io.resp.bits.info := 0.U

   //------------------------------------------------------------
   // Update counter table.

   // Nothing to update, as the BIM is handled externally.


}

