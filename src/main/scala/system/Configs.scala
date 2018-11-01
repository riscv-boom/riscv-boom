//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom.system

import Chisel._
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

// scalastyle:off

class BoomConfig extends Config(new DefaultBoomConfig ++ new WithNBoomCores(1) ++ new WithoutTLMonitors ++ new freechips.rocketchip.system.BaseConfig)
class SmallBoomConfig extends Config(new WithSmallBooms ++ new DefaultBoomConfig ++ new WithNBoomCores(1) ++ new WithoutTLMonitors ++ new freechips.rocketchip.system.BaseConfig)
class MediumBoomConfig extends Config(new WithMediumBooms ++ new DefaultBoomConfig ++ new WithNBoomCores(1) ++ new WithoutTLMonitors ++ new freechips.rocketchip.system.BaseConfig)
class MegaBoomConfig extends Config(new WithMegaBooms ++ new DefaultBoomConfig ++ new WithNBoomCores(1) ++ new WithoutTLMonitors ++ new freechips.rocketchip.system.BaseConfig)

class MegaBoomECCConfig extends Config(new WithL1IECC("parity", "parity") ++ new WithL1DECC("identity", "parity") ++ new WithMegaBooms ++ new DefaultBoomConfig ++ new WithNBoomCores(1) ++ new WithoutTLMonitors ++ new freechips.rocketchip.system.BaseConfig)


class jtagSmallBoomConfig extends Config(new WithSmallBooms ++ new DefaultBoomConfig ++ new WithNBoomCores(1) ++ new WithoutTLMonitors ++ new freechips.rocketchip.system.BaseConfig ++ new WithJtagDTM)
class jtagMediumBoomConfig extends Config(new WithMediumBooms ++ new DefaultBoomConfig ++ new WithNBoomCores(1) ++ new WithoutTLMonitors ++ new freechips.rocketchip.system.BaseConfig ++ new WithJtagDTM)
class jtagMegaBoomConfig extends Config(new WithMegaBooms ++ new DefaultBoomConfig ++ new WithNBoomCores(1) ++ new WithoutTLMonitors ++ new freechips.rocketchip.system.BaseConfig ++ new WithJtagDTM)

class SmallIntBoomConfig extends Config(new WithoutBoomFPU ++ new WithSmallBooms ++ new DefaultBoomConfig ++ new WithNBoomCores(1) ++ new WithoutTLMonitors ++ new freechips.rocketchip.system.BaseConfig)
// scalastyle:on
 
class SmallDualBoomConfig extends Config(new WithTrace ++ new WithSmallBooms ++ new DefaultBoomConfig ++ new WithNBoomCores(2) ++ new WithoutTLMonitors ++ new freechips.rocketchip.system.BaseConfig)


class TracedSmallBoomConfig extends Config(new WithTrace ++ new WithSmallBooms ++ new DefaultBoomConfig ++ new WithNBoomCores(1) ++ new WithoutTLMonitors ++ new freechips.rocketchip.system.BaseConfig)


// Allow for some number N BOOM cores.
class WithNBoomCores(n: Int) extends Config((site, here, up) => {
  case BoomTilesKey => {
    // "big" is vestigial -- we could also add a corresponding "little" vector too for hetereogenous setups.
    val big = BoomTileParams(
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
    List.tabulate(n)(i => big.copy(hartId = i))
  }
})

// This sets the ECC for the L1 instruction cache.
class WithL1IECC(tecc: String, decc: String) extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { r =>
    r.copy(icache = r.icache.map(_.copy(tagECC = Some(tecc), dataECC = Some(decc)))) }
})

// This sets the ECC for the L1 data cache.
class WithL1DECC(tecc: String, decc: String) extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { r =>
    r.copy(dcache = r.dcache.map(_.copy(tagECC = Some(tecc), dataECC = Some(decc)))) }
})

