//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio, Abraham Gonzalez, Ben Korpan, Jerry Zhao
//------------------------------------------------------------------------------

package boom.common

import chisel3._

import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.system._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

// ---------------------
// BOOM Configs
// ---------------------

/**
 * Note: For all these configs, the mix-ins are applied from
 * "bottom" to "top". This means that the "lower" mix-ins set the
 * default values of the parameters, and the "higher" mix-ins
 * overwrite the defaults to implement a new configuration.
 *
 * This order is specified in the GeneratorApp class (aka foldRight)
 *
 * Ex.
 * class SmallBoomConfig extends Config(
 *    new WithRVC ++ <-- Applied 6th
 *    new WithSmallBooms ++ <-- Applied 5th
 *    new BaseBoomConfig ++ <-- Applied 4th
 *    new WithNBoomCores(1) ++ <-- Applied 3rd
 *    new WithoutTLMonitors ++ <-- Applied 2nd
 *    new freechips.rocketchip.system.BaseConfig) <-- Applied 1st
 */

// scalastyle:off

// Main configs. SmallBoomConfig and MediumBoomConfig are best-maintained
//   MediumBoomConfig is typically described in documentation
//   All RV64IMAFDC
class SmallBoomConfig extends Config(
  new WithRVC ++
  new WithSmallBooms ++
  new BaseBoomConfig ++
  new WithNBoomCores(1) ++
  new freechips.rocketchip.system.BaseConfig)

class MediumBoomConfig extends Config(
  new WithRVC ++
  new WithMediumBooms ++
  new BaseBoomConfig ++
  new WithNBoomCores(1) ++
  new freechips.rocketchip.system.BaseConfig)

class LargeBoomConfig extends Config(
  new WithRVC ++
  new WithLargeBooms ++
  new BaseBoomConfig ++
  new WithNBoomCores(1) ++
  new freechips.rocketchip.system.BaseConfig)

class MegaBoomConfig extends Config(
  new WithRVC ++
  new WithMegaBooms ++
  new BaseBoomConfig ++
  new WithNBoomCores(1) ++
  new freechips.rocketchip.system.BaseConfig)

// Assorted configs

// RV64IMAC
class SmallIntBoomConfig extends Config(
  new WithoutBoomFPU ++
  new SmallBoomConfig)

class SmallDualBoomConfig extends Config(
  new WithRVC ++
  new WithSmallBooms ++
  new BaseBoomConfig ++
  new WithNBoomCores(2) ++
  new freechips.rocketchip.system.BaseConfig)

class TracedSmallBoomConfig extends Config(
  new WithTrace ++
  new SmallBoomConfig)

//RV32IMAC TODO: Support FP
class SmallRV32UnifiedBoomConfig extends Config(
  new WithoutBoomFPU ++
  new WithUnifiedMemIntIQs ++
  new SmallBoomConfig)

// --------------------------
// BOOM + Rocket Configs
// ----
// Heterogeneous Tile Configs
// --------------------------

class SmallBoomAndRocketConfig extends Config(
  // final param setup
  new WithRenumberHarts ++
  // boom param setup
  new WithRVC ++
  new WithSmallBooms ++
  new BaseBoomConfig ++
  // create boom tile
  new WithNBoomCores(1) ++
  // create rocket tile
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new freechips.rocketchip.system.BaseConfig)

class MediumBoomAndRocketConfig extends Config(
  // final param setup
  new WithRenumberHarts ++
  // boom param setup
  new WithRVC ++
  new WithMediumBooms ++
  new BaseBoomConfig ++
  // create boom tile
  new WithNBoomCores(1) ++
  // create rocket tile
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new freechips.rocketchip.system.BaseConfig)

class DualMediumBoomAndDualRocketConfig extends Config(
  // final param setup
  new WithRenumberHarts ++
  // boom param setup (applies to all boom cores)
  new WithRVC ++
  new WithMediumBooms ++
  new BaseBoomConfig ++
  // create boom tiles
  new WithNBoomCores(2) ++
  // create rocket tiles
  new freechips.rocketchip.subsystem.WithNBigCores(2) ++
  new freechips.rocketchip.system.BaseConfig)
