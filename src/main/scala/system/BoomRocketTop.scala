//******************************************************************************
// Copyright (c) 2019 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Abraham Gonzalez
//------------------------------------------------------------------------------

package boom.system

import chisel3._

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.util.{DontTouch}

import testchipip.{HasPeripherySerial, HasPeripherySerialModuleImp, HasNoDebug, HasNoDebugModuleImp}

// -----------------------------------------------------------------------
// Base system that uses testchipip's serial interface to bringup the core
//   - The alternative is to instantiate `BoomRocketSystem` directly if
//     you just want to use Rocket Chip's DTM
// -----------------------------------------------------------------------

class BoomRocketTop(implicit p: Parameters) extends BoomRocketSystem
  with HasNoDebug
  with HasPeripherySerial {
  override lazy val module = new BoomRocketTopModule(this)
}

class BoomRocketTopModule[+L <: BoomRocketTop](l: L) extends BoomRocketSystemModule(l)
  with HasNoDebugModuleImp
  with HasPeripherySerialModuleImp
  with DontTouch
