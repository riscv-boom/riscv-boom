//******************************************************************************
// Copyright (c) 2015 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Ring Scheduler
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import FUConstants._
import boom.common._

class RingScheduler(numEntries: Int)(implicit p: Parameters) {
  val io = IO(new BoomBundle{
    val dis_uops = Flipped(Vec(coreWidth, DecoupledIO(new MicroOp)))
    val iss_uops = Output(Vec(coreWidth, Valid(new MicroOp)))

    val wakeups  = Input(Vec(coreWidth, Valid(UInt(pregSz.W))))
    val ld_miss  = Input(Bool()) // TODO use this

    val div_rdy  = Input(Bool()) // TODO do fu_types instead? Does it make a difference in synth?

    val brinfo   = Input(new BrResolutionInfo)
    val flush    = Input(Bool())
  })


}
