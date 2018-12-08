// See LICENSE.SiFive for license details.

package boom.system

//TODO: Figure out how to switch to Chisel3

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.devices.debug.Debug


class TestHarness()(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val success = Bool(OUTPUT)
  })
  println("\n\nBuilding TestHarness for an ExampleBoomSystem.\n")

  val dut = Module(LazyModule(new ExampleBoomSystem).module)
  dut.reset := reset | dut.debug.ndreset

  dut.dontTouchPorts()
  dut.tieOffInterrupts()
  dut.connectSimAXIMem()
  dut.connectSimAXIMMIO()
  dut.l2_frontend_bus_axi4.foreach(_.tieoff)

  Debug.connectDebug(dut.debug, clock, reset, io.success)
}
