//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// BaseOnly Branch Predictor
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

import boom.util.{ElasticReg, BoomCoreStringPrefix}

/**
 * BaseOnly predictor configuration parameters used in configurations
 *
 * @param enabled using BaseOnly predictor?
 */
case class BaseOnlyParameters(
  enabled: Boolean = true
)

/**
 * Dummy response for commit (nothing to commit since you are just using the
 * bi-modal table
 */
class BaseOnlyResp() extends Bundle
{
  // Used to avoid 0 width wires
  val not_used = Bool()
}

/**
 * Companion object to BaseOnlyBrPredictor to get the the size of the
 * branch predictor response
 */
object BaseOnlyBrPredictor
{
  def GetRespInfoSize(): Int = {
    val dummy = new BaseOnlyResp()
    dummy.getWidth
  }
}

/**
 * Class to create a BaseOnlyBr predictor that only uses
 * the BoomBTB's bi-modal table
 */
class BaseOnlyBrPredictor(
   )(implicit p: Parameters)
   extends BoomBrPredictor(8)
{
  //------------------------------------------------------------
  // Predictor state (none: use BIM).

  //------------------------------------------------------------
  // Get prediction in F2, buffer, and provide prediction in F3.

  val q_s3_resp = withReset(reset.asBool || io.fe_clear || io.f4_redirect)
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

  override def toString: String = BoomCoreStringPrefix(
    "==Base Only BPU==",
    "Building no predictor (just using BIM as a base predictor)")
}
