//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Jerry Zhao
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// The RoCC shim unit. Similar to the LSU, in that we need to allocate entries
// for instruction bits at decode, and send commands strictly in order.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.RoCCCoreIO
import boom.common._

/**
 * IO bundle representing the different signals to interact with the RoCC
 *
  */
class RoCCShimIO(implicit p: Parameters) extends BoomBundle()(p)
{
   // Decode Stage
   val dec_rocc_vals    = Input(Vec(decodeWidth, Bool()))
   val dec_uops         = Input(Vec(decodeWidth, new MicroOp))

   val req              = Flipped(new DecoupledIO(new FuncUnitReq(xLen)))
   val resp             = new DecoupledIO(new FuncUnitResp(xLen))
   val brinfo           = Input(new BrResolutionInfo())

   val rocc             = Flipped(new RoCCCoreIO)
}

class RoCCShim(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new RoCCShimIO)
}

