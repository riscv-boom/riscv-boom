// See LICENSE.SiFive for license details.

package boom.system

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule

class TestHarness()(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }
  println("\n\nBuilding TestHarness for an ExampleBoomSystem.\n")

  val dut = Module(LazyModule(new ExampleBoomSystem).module)
  dut.reset := reset | dut.debug.ndreset

  dut.dontTouchPorts()
  dut.tieOffInterrupts()
  dut.connectSimAXIMem()
  dut.connectSimAXIMMIO()
  dut.l2_frontend_bus_axi4.foreach(_.tieoff)
  dut.connectDebug(clock, reset, io.success)
}
