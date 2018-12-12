//******************************************************************************
// Copyright (c) 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

// See LICENSE.SiFive for license details.

package boom.system

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.DontTouch

/** Example Top with periphery devices and ports, and a Boom subsystem */
class ExampleBoomSystem(implicit p: Parameters) extends BoomSubsystem
    with HasAsyncExtInterrupts
    with CanHaveMisalignedMasterAXI4MemPort
    with CanHaveMasterAXI4MMIOPort
    with CanHaveSlaveAXI4Port
    with HasPeripheryBootROM {
  override lazy val module = new ExampleBoomSystemModule(this)

  // Error device used for testing and to NACK invalid front port transactions
  val error = LazyModule(new TLError(p(ErrorDeviceKey), sbus.beatBytes))
  // always buffer the error device because no one cares about its latency
  sbus.coupleTo("slave_named_error"){ error.node := TLBuffer() := _ }
}

class ExampleBoomSystemModule[+L <: ExampleBoomSystem](_outer: L) extends BoomSubsystemModule(_outer)
    with HasRTCModuleImp
    with HasExtInterruptsModuleImp
    with CanHaveMisalignedMasterAXI4MemPortModuleImp
    with CanHaveMasterAXI4MMIOPortModuleImp
    with CanHaveSlaveAXI4PortModuleImp
    with HasPeripheryBootROMModuleImp
    with DontTouch
