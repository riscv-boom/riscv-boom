// See LICENSE.SiFive for license details.

package boom.system

//TODO: Figure out how to switch to Chisel3

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.devices.debug.Debug


class TestHarness()(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val success = Output(Bool())
  })
  println("\n\nBuilding TestHarness for an ExampleBoomSystem.\n")

  val dut = Module(LazyModule(new ExampleBoomSystem).module)
  dut.reset := reset.toBool | dut.debug.ndreset

  dut.dontTouchPorts()
  dut.tieOffInterrupts()
  dut.connectSimAXIMem()
  dut.connectSimAXIMMIO()
  dut.l2_frontend_bus_axi4.foreach( q => q := DontCare ) // Overridden in next line
  dut.l2_frontend_bus_axi4.foreach(_.tieoff)

  Debug.connectDebug(dut.debug, clock, reset.toBool, io.success)
}
