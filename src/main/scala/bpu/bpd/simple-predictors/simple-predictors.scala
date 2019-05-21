//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Simple Predictor Classes
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.bpu

import chisel3._
import chisel3.util._
import chisel3.core.withReset

import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.util.{Str}
import freechips.rocketchip.rocket.RocketCoreParams

import boom.common._
import boom.exu._
import boom.exu.BranchUnitResp
import boom.util.ElasticReg

/**
 * A null branch predictor that makes no predictions
 *
 * @param historyLength length of the BHR in bits
 */
class NullBrPredictor(
  historyLength: Int = 12
  )(implicit p: Parameters) extends BoomBrPredictor(historyLength)
{
  override def toString: String = "   [Core " + hartId + "] ==Null BPU==" +
    "\n   [Core " + hartId + "] Building (0 kB) Null Predictor (never predict)."
  io.resp.valid := false.B
}

/**
 * Random predictor configuration parameters used in configurations
 *
 * @param enabled using Random predictor?
 */
case class RandomBpdParameters(
  enabled: Boolean = true
)

/**
 * Companion object to RandomBrPredictor to get the the size of the
 * branch predictor response
 */
object RandomBrPredictor
{
  def GetRespInfoSize(): Int = {
    // Should be zero (no RespInfo needed for Random predictor), but avoid 0-width wires.
    1
  }
}

/**
 * Class to create a Random predictor that generates random predictions. Good for testing!
 */
class RandomBrPredictor(
  )(implicit p: Parameters) extends BoomBrPredictor(historyLength = 1)
{
  override def toString: String = "   [Core " + hartId + "] ==Random BPU==" +
    "\n   [Core " + hartId + "] Building Random Branch Predictor."
  private val rand_val = RegInit(false.B)
  rand_val := ~rand_val
  private var lfsr= LFSR16(true.B)
  def rand(width: Int) = {
    lfsr = lfsr(lfsr.getWidth-1,1)
    val mod = (1 << width) - 1
    freechips.rocketchip.util.Random(mod, lfsr)
  }

  io.resp.valid := rand_val
  io.resp.bits.takens := rand(fetchWidth)
}
