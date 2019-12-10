//******************************************************************************
// Copyright (c) 2019 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Dromajo Cosimulation BlackBox
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Takes commit signals from the core and runs the Dromajo cosimulation tool
// for functional verification.

package boom.util

import chisel3._
import chisel3.util._
import chisel3.core.{IntParam, StringParam}

/**
 * Connect to the Dromajo Cosimulation Tool
 */
class DromajoCosimBlackBox(
  commitWidth: Int,
  xLen: Int,
  bootromFile: String,
  resetVector: String,
  mmioStart: String,
  mmioEnd: String,
  plicBase: String,
  plicSize: String,
  clintBase: String,
  clintSize: String,
  memSize: String)
  extends BlackBox(Map(
    "COMMIT_WIDTH" -> IntParam(commitWidth),
    "XLEN" -> IntParam(xLen),
    "BOOTROM_FILE" -> StringParam(bootromFile),
    "RESET_VECTOR" -> StringParam(resetVector),
    "MMIO_START" -> StringParam(mmioStart),
    "MMIO_END" -> StringParam(mmioEnd),
    "PLIC_BASE" -> StringParam(plicBase),
    "PLIC_SIZE" -> StringParam(plicSize),
    "CLINT_BASE" -> StringParam(clintBase),
    "CLINT_SIZE" -> StringParam(clintSize),
    "MEM_SIZE" -> StringParam(memSize)
  ))
  with HasBlackBoxResource
{
  val instBits = 32
  val maxHartIdBits = 32
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())

    val valid   = Input(UInt(         (commitWidth).W))
    val hartid  = Input(UInt(       (maxHartIdBits).W))
    val pc      = Input(UInt(    (xLen*commitWidth).W))
    val inst    = Input(UInt((instBits*commitWidth).W))
    val wdata   = Input(UInt(    (xLen*commitWidth).W))
    val mstatus = Input(UInt(    (xLen*commitWidth).W))
    val check   = Input(UInt(         (commitWidth).W))

    val int_xcpt = Input(      Bool())
    val cause    = Input(UInt(xLen.W))
  })

  addResource("/vsrc/dromajo_boom.v")
  addResource("/csrc/dromajo_boom.cc")
  addResource("/csrc/dromajo_wrapper.cc")
  addResource("/csrc/dromajo_wrapper.h")
}
