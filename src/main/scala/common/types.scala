//******************************************************************************
// Copyright (c) 2018 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.common

import chisel3._

import freechips.rocketchip.config.Parameters

/**
 * BOOM module that is used to add parameters to the module
 */
abstract class BoomModule(implicit p: Parameters) extends freechips.rocketchip.tile.CoreModule
  with HasBoomCoreParameters

abstract class BoomMultiIOModule(implicit val p: Parameters) extends MultiIOModule
  with HasBoomCoreParameters
  with freechips.rocketchip.tile.HasCoreParameters

/**
 * BOOM bundle used to add parameters to the object/class/trait/etc
 */
class BoomBundle(implicit val p: Parameters) extends freechips.rocketchip.util.ParameterizedBundle
  with HasBoomCoreParameters
