//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

package boom.system

import chisel3._
import chisel3.util.{log2Up}

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

import boom.common._

// ------------------
// BOOM Mixin Configs
// ------------------

/**
 * Create multiple copies of a BOOM tile (and thus a core).
 * Override with the default mixins to control all params of the tiles.
 * Default adds small BOOMs.
 *
 * @param n amount of tiles to duplicate
 */
class WithNBoomCores(n: Int) extends Config(
  new WithSmallBooms ++
  new BaseBoomConfig ++
  new Config((site, here, up) => {
    case BoomTilesKey => {
      List.tabulate(n)(i => BoomTileParams(hartId = i))
    }
  })
)

/**
 * This sets the ECC for the L1 instruction cache.
 *
 * @param tecc ...
 * @param decc ...
 */
class WithL1IECC(tecc: String, decc: String) extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { r =>
    r.copy(icache = r.icache.map(_.copy(tagECC = Some(tecc), dataECC = Some(decc)))) }
})

/**
 * This sets the ECC for the L1 data cache.
 *
 * @param tecc ...
 * @param decc ...
 */
class WithL1DECC(tecc: String, decc: String) extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { r =>
    r.copy(dcache = r.dcache.map(_.copy(tagECC = Some(tecc), dataECC = Some(decc)))) }
})

/**
 * Class to renumber BOOM + Rocket harts so that there are no overlapped harts
 * This mixin assumes Rocket tiles are numbered before BOOM tiles
 * Also makes support for multiple harts depend on Rocket + BOOM
 * Note: Must come after all harts are assigned for it to apply
 */
class WithRenumberHarts(rocketFirst: Boolean = false) extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site).zipWithIndex map { case (r, i) =>
    r.copy(hartId = i + (if(rocketFirst) 0 else up(BoomTilesKey, site).length))
  }
  case BoomTilesKey => up(BoomTilesKey, site).zipWithIndex map { case (b, i) =>
    b.copy(hartId = i + (if(rocketFirst) up(RocketTilesKey, site).length else 0))
  }
  case MaxHartIdBits => log2Up(up(BoomTilesKey, site).size + up(RocketTilesKey, site).size)
})

/**
 * Add a synchronous clock crossing to the tile boundary
 */
class WithSynchronousBoomTiles extends Config((site, here, up) => {
  case BoomCrossingKey => up(BoomCrossingKey, site) map { b =>
    b.copy(crossingType = SynchronousCrossing())
  }
})

/**
 * Add an asynchronous clock crossing to the tile boundary
 */
class WithAsynchronousBoomTiles(depth: Int, sync: Int) extends Config((site, here, up) => {
  case BoomCrossingKey => up(BoomCrossingKey, site) map { b =>
    b.copy(crossingType = AsynchronousCrossing(depth, sync))
  }
})

/**
 * Add a rational clock crossing to the tile boundary (used when the clocks are related by a fraction).
 */
class WithRationalBoomTiles extends Config((site, here, up) => {
  case BoomCrossingKey => up(BoomCrossingKey, site) map { b =>
    b.copy(crossingType = RationalCrossing())
  }
})
