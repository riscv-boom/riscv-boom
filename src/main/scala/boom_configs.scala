//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.coreplex._
import freechips.rocketchip.system._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import boom._

// scalastyle:off

//class BOOMConfig extends Config(new DefaultBoomConfig ++ new WithNBigCores(1) ++ new WithoutTLMonitors ++ new BaseConfig)
// TODO re-enable WithTLMonitors once rocket-chip #1017 resolved.
class BOOMConfig extends Config(new DefaultBoomConfig ++ new WithNBigCores(1) ++ new BaseConfig)
class SmallBoomConfig extends Config(new WithSmallBooms ++ new DefaultBoomConfig ++ new WithNBigCores(1) ++ new BaseConfig)
class MediumBoomConfig extends Config(new WithMediumBooms ++ new DefaultBoomConfig ++ new WithNBigCores(1) ++ new BaseConfig)
class MegaBoomConfig extends Config(new WithMegaBooms ++ new DefaultBoomConfig ++ new WithNBigCores(1) ++ new WithoutTLMonitors ++ new BaseConfig)

// scalastyle:on
