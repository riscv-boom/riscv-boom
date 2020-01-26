//******************************************************************************
// Copyright (c) 2015 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Ring Architecture Writeback Crossbar
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util._

class RingWriteback(numWritingUnits)(implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle{
    val exu_resps  = Input(Vec(numWritingUnits, Valid(new ExeUnitResp(xLen))))
    val writebacks = Output(Vec(coreWidth, Valid(new ExeUnitResp(xLen))))
  })


}
