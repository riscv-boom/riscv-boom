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
 *
 * @param n amount of tiles to duplicate
 */
class WithNBoomCores(n: Int) extends Config((site, here, up) => {
  case BoomTilesKey => {
    val boomTileParams = BoomTileParams(
      core   = BoomCoreParams(mulDiv = Some(MulDivParams(
        mulUnroll = 8,
        mulEarlyOut = true,
        divEarlyOut = true))),
      dcache = Some(DCacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nMSHRs = 0,
        blockBytes = site(CacheBlockBytes))),
      icache = Some(ICacheParams(
        rowBits = site(SystemBusKey).beatBits,
        blockBytes = site(CacheBlockBytes))))
    List.tabulate(n)(i => boomTileParams.copy(hartId = i))
  }
})

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
class WithRenumberHarts extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site).zipWithIndex map { case (r, i) =>
    r.copy(hartId = i)
  }
  case BoomTilesKey => up(BoomTilesKey, site).zipWithIndex map { case (b, i) =>
    b.copy(hartId = i + up(RocketTilesKey, site).length)
  }
  case MaxHartIdBits => log2Up(up(BoomTilesKey, site).size + up(RocketTilesKey, site).size)
})
