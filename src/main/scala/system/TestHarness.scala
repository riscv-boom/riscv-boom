//******************************************************************************
// Copyright (c) 2017 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

package boom.system

import chisel3._

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy.{LazyModule}
import freechips.rocketchip.devices.debug.{Debug}

/**
 * A test harness to wrap around the system being simulated
 */
class TestHarness(implicit p: Parameters) extends Module
{
  val io = IO(new Bundle {
    val success = Output(Bool())
  })

  println("\n\nBuilding TestHarness for an ExampleBoomAndRocketSystem.\n")

  val dut = Module(LazyModule(new ExampleBoomAndRocketSystem).module)
  dut.reset := reset.asBool | dut.debug.ndreset

  dut.dontTouchPorts()
  dut.tieOffInterrupts()
  dut.connectSimAXIMem()
  dut.connectSimAXIMMIO()
  dut.l2_frontend_bus_axi4.foreach( q => q := DontCare ) // Overridden in next line
  dut.l2_frontend_bus_axi4.foreach(_.tieoff)

  Debug.connectDebug(dut.debug, clock, reset.asBool, io.success)
}
