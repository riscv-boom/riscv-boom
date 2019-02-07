//******************************************************************************
// Copyright (c) 2018 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

package boom.common

import chisel3._

import freechips.rocketchip.config.Parameters

abstract class BoomModule(implicit p: Parameters) extends freechips.rocketchip.tile.CoreModule()(p)
  with HasBoomCoreParameters

class BoomBundle(implicit val p: Parameters) extends freechips.rocketchip.util.ParameterizedBundle()(p)
  with HasBoomCoreParameters

