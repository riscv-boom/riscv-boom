//******************************************************************************
// Copyright (c) 2015 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Column Arbiter
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util._

class ColumnArbiter(implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle {
    val uops = Input(Vec(coreWidth, Valid(new MicroOp)))
    val gnts = Output(Vec(coreWidth, UInt(coreWidth.W)))
  })

  for (w <- 0 until coreWidth) {
    io.gnts(w) := Mux(io.uops(w).valid, 1.U(coreWidth.W) << w.U, 0.U)
  }
}
