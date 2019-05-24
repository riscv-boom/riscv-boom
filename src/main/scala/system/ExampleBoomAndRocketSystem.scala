//******************************************************************************
// Copyright (c) 2019 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Abraham Gonzalez
//------------------------------------------------------------------------------

package boom.system

import chisel3._

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.{DontTouch}

/**
 * Example top with periphery devices and ports, and a BOOM + Rocket subsystem
 */
class ExampleBoomAndRocketSystem(implicit p: Parameters) extends BoomAndRocketSubsystem
  with HasAsyncExtInterrupts
  with CanHaveMasterAXI4MemPort
  with CanHaveMasterAXI4MMIOPort
  with CanHaveSlaveAXI4Port
  with HasPeripheryBootROM
{
  override lazy val module = new ExampleBoomAndRocketSystemModule(this)

  // The sbus masters the cbus; here we convert TL-UH -> TL-UL
  sbus.crossToBus(cbus, NoCrossing)

  // The cbus masters the pbus; which might be clocked slower
  cbus.crossToBus(pbus, SynchronousCrossing())

  // The fbus masters the sbus; both are TL-UH or TL-C
  FlipRendering { implicit p =>
    sbus.crossFromBus(fbus, SynchronousCrossing())
  }

  // The sbus masters the mbus; here we convert TL-C -> TL-UH
  private val BankedL2Params(nBanks, coherenceManager) = p(BankedL2Key)
  private val (in, out, halt) = coherenceManager(this)
  if (nBanks != 0) {
    sbus.coupleTo("coherence_manager") { in :*= _ }
    mbus.coupleFrom("coherence_manager") { _ :=* BankBinder(mbus.blockBytes * (nBanks-1)) :*= out }
  }
}

/**
 * Example top module with periphery devices and ports, and a BOOM + Rocket subsystem
 */
class ExampleBoomAndRocketSystemModule[+L <: ExampleBoomAndRocketSystem](_outer: L) extends BoomAndRocketSubsystemModuleImp(_outer)
  with HasRTCModuleImp
  with HasExtInterruptsModuleImp
  with CanHaveMasterAXI4MemPortModuleImp
  with CanHaveMasterAXI4MMIOPortModuleImp
  with CanHaveSlaveAXI4PortModuleImp
  with HasPeripheryBootROMModuleImp
  with DontTouch
