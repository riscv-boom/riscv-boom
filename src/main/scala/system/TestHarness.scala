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
 * Test harness using debug test module (dtm) to bringup the core
 */
class TestHarness(implicit p: Parameters) extends Module
{
  val io = IO(new Bundle {
    val success = Output(Bool())
  })

  println("\n\nBuilding TestHarness for an BoomRocketSystem with DTM bringup.\n")

  val dut = Module(LazyModule(new BoomRocketSystem).module)

  dut.reset := reset.asBool | dut.debug.ndreset
  dut.dontTouchPorts()
  dut.tieOffInterrupts()
  dut.connectSimAXIMem()
  dut.connectSimAXIMMIO()
  dut.l2_frontend_bus_axi4.foreach( q => q := DontCare ) // Overridden in next line
  dut.l2_frontend_bus_axi4.foreach(_.tieoff)

  Debug.connectDebug(dut.debug, clock, reset.asBool, io.success)
}

/**
 * Test harness using test serial interface (tsi) to bringup the core
 */
class TestHarnessWithTSI(implicit p: Parameters) extends Module
{
  val io = IO(new Bundle {
    val success = Output(Bool())
  })

  // force Chisel to rename module
  override def desiredName = "TestHarness"

  println("\n\nBuilding TestHarness for an BoomRocketSystemWithTSI with TSI bringup.\n")

  val dut = Module(LazyModule(new BoomRocketSystemWithTSI).module)

  dut.reset := reset.asBool | dut.debug.ndreset
  dut.dontTouchPorts()
  dut.tieOffInterrupts()
  dut.connectSimAXIMem()
  dut.connectSimAXIMMIO()
  dut.l2_frontend_bus_axi4.foreach(axi => {
    axi.tieoff()
    experimental.DataMirror.directionOf(axi.ar.ready) match {
      case core.ActualDirection.Input =>
        axi.r.bits := DontCare
        axi.b.bits := DontCare
      case core.ActualDirection.Output =>
        axi.aw.bits := DontCare
        axi.ar.bits := DontCare
        axi.w.bits := DontCare
    }
  })
  dut.debug := DontCare

  io.success := dut.connectSimSerial()
}
