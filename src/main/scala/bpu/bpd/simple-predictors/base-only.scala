//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

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

import chisel3._
import chisel3.util._
import chisel3.core.withReset

import freechips.rocketchip.config.{Parameters, Field}

import boom.util.ElasticReg

/**
 * BaseOnly predictor configuration parameters used in configs
 *
 * @param enabled using BaseOnly predictor?
 */
case class BaseOnlyParameters(
   enabled: Boolean = true
)

/**
 * Dummy response for commit (nothing to commit since you are just using the BIM
 */
class BaseOnlyResp() extends Bundle
{
   // Used to avoid 0 width wires
   val not_used = Bool()
}

/**
 * Companion object to BaseOnlyBrPredictor to get the the size of the 
 * BPD resp.
 */
object BaseOnlyBrPredictor
{
   def GetRespInfoSize(p: Parameters, hlen: Int): Int =
   {
      val dummy = new BaseOnlyResp()
      dummy.getWidth
   }
}

/**
 * Class to create a BaseOnlyBr Predictor
 *
 * @param fetch_width # of instructions fetched
 */
class BaseOnlyBrPredictor(
   fetch_width: Int
   )(implicit p: Parameters)
   extends BrPredictor(fetch_width, 8)(p)
{
   //------------------------------------------------------------
   // Predictor state (none: use BIM).

   //------------------------------------------------------------
   // Get prediction in F2, buffer, and provide prediction in F3.

   val q_s3_resp = withReset(reset.toBool || io.fe_clear || io.f4_redirect)
      {Module(new ElasticReg(Valid(new BimResp)))}

   q_s3_resp.io.enq.valid := io.f2_valid
   q_s3_resp.io.enq.bits := io.f2_bim_resp
   q_s3_resp.io.deq.ready := DontCare

   io.resp.valid := q_s3_resp.io.deq.valid && q_s3_resp.io.deq.bits.valid
   io.resp.bits.takens := q_s3_resp.io.deq.bits.bits.getTakens
   io.resp.bits.info := 0.U

   //------------------------------------------------------------
   // Update counter table.

   // Nothing to update, as the BIM is handled externally.

   override def toString: String = "  Building no predictor (just using BIM as a base predictor)."
}
