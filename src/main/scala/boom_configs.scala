//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom

import Chisel._
import uncore.tilelink._
import uncore.coherence._
import uncore.agents._
import uncore.devices._
import uncore.converters._
import coreplex._
import rocketchip._
import tile._
import rocket._
import boom._
import config.{Parameters, Config}

// scalastyle:off

class BOOMConfig extends Config(new DefaultBoomConfig ++ new WithNBigCores(1) ++ new BaseConfig)
class SmallBoomConfig extends Config(new WithSmallBooms ++ new DefaultBoomConfig ++ new WithNBigCores(1) ++ new BaseConfig)
class MediumBoomConfig extends Config(new WithMediumBooms ++ new DefaultBoomConfig ++ new WithNBigCores(1) ++ new BaseConfig)
class MegaBoomConfig extends Config(new WithMegaBooms ++ new DefaultBoomConfig ++ new WithNBigCores(1) ++ new BaseConfig)

// scalastyle:on
