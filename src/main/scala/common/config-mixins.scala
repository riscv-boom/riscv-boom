//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.common

import chisel3._
import chisel3.util.{log2Up}

import freechips.rocketchip.config.{Parameters, Config, Field}
import freechips.rocketchip.subsystem.{SystemBusKey, RocketTilesKey, RocketCrossingParams}
import freechips.rocketchip.devices.tilelink.{BootROMParams}
import freechips.rocketchip.diplomacy.{SynchronousCrossing, AsynchronousCrossing, RationalCrossing}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

import boom.ifu._
import boom.exu._
import boom.lsu._

case object BoomTilesKey extends Field[Seq[BoomTileParams]](Nil)
case object BoomCrossingKey extends Field[Seq[RocketCrossingParams]](List(RocketCrossingParams()))

// ---------------------
// BOOM Config Fragments
// ---------------------

/**
 * Enables RV32 version of the core
 */
class WithBoomRV32 extends Config((site, here, up) => {
  case XLen => 32
  case BoomTilesKey => up(BoomTilesKey, site) map { b =>
    b.copy(core = b.core.copy(
      fpu = b.core.fpu.map(_.copy(fLen = 32)),
      mulDiv = Some(MulDivParams(mulUnroll = 8))))
  }
})

/**
 * Disable support for C-extension (RVC)
 */
class WithoutBoomRVC extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b =>
    b.copy(core = b.core.copy(
      fetchWidth = b.core.fetchWidth / 2,
      useCompressed = false))
   }
})

/**
 * Remove FPU
 */
class WithoutBoomFPU extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b =>
    b.copy(core = b.core.copy(
      issueParams = b.core.issueParams.filter(_.iqType != IQT_FP.litValue),
      fpu = None))
   }
})

/**
 * Remove Fetch Monitor (should not be synthesized (although it can be))
 */
class WithoutFetchMonitor extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b =>
    b.copy(core = b.core.copy(
      useFetchMonitor = false
    ))
  }
})

/**
 * Customize the amount of perf. counters (HPMs) for the core
 */
class WithNPerfCounters(n: Int) extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(core = b.core.copy(
    nPerfCounters = n
  ))}
})

/**
 * Enable tracing
 */
class WithTrace extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(trace = true) }
})

/**
 * Create multiple copies of a BOOM tile (and thus a core).
 * Override with the default fragments to control all params of the tiles.
 * Default adds small BOOMs.
 *
 * @param n amount of tiles to duplicate
 */
class WithNBoomCores(n: Int) extends Config(
  new WithSmallBooms ++
  new Config((site, here, up) => {
    case BoomTilesKey => {
      List.tabulate(n)(i => BoomTileParams(hartId = i))
    }
  })
)

/**
 * Class to renumber BOOM + Rocket harts so that there are no overlapped harts
 * This fragment assumes Rocket tiles are numbered before BOOM tiles
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

/**
 * N-wide Ring-BOOM.
 */
class WithRingBooms(n: Int, f: Int) extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(
    core = b.core.copy(
      fetchWidth = f,
      useCompressed = true,
      decodeWidth = n,
      numRobEntries = 25*n,
      issueParams = Seq(
        IssueParams(issueWidth=n, numEntries=8*n, iqType=IQT_INT.litValue, dispatchWidth=2),
        IssueParams(issueWidth=1, numEntries=4*n, iqType=IQT_FP.litValue , dispatchWidth=n)),
      memWidth = 1,
      numDCacheBanks = 1,
      numIntPhysRegisters = 32 + 17*n,
      numFpPhysRegisters = 32 + 12*n,
      numLdqEntries = 4*n,
      numStqEntries = 4*n,
      maxBrCount = 2*n,
      numFetchBufferEntries = 8*n,
      ftq = FtqParameters(nEntries=50*n/f),
      fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))),
    dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBytes*8,
                               nSets=64, nWays=2*n, nMSHRs=2, nRPQ=4, nTLBEntries=16)),
    icache = Some(ICacheParams(fetchBytes = 2*f, rowBits = site(SystemBusKey).beatBytes*8, nSets=64, nWays=2*n, prefetch=true))
  )}
  case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 2*f)
  case XLen => 64
  case MaxHartIdBits => log2Up(site(BoomTilesKey).size)
})

/**
 * 1-wide BOOM.
 */
class WithSmallBooms extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(
    core = b.core.copy(
      fetchWidth = 4,
      useCompressed = true,
      decodeWidth = 1,
      numRobEntries = 32,
      issueParams = Seq(
        IssueParams(issueWidth=1, numEntries=8, iqType=IQT_MEM.litValue, dispatchWidth=1),
        IssueParams(issueWidth=1, numEntries=8, iqType=IQT_INT.litValue, dispatchWidth=1),
        IssueParams(issueWidth=1, numEntries=8, iqType=IQT_FP.litValue , dispatchWidth=1)),
      numIntPhysRegisters = 52,
      numFpPhysRegisters = 48,
      numLdqEntries = 8,
      numStqEntries = 8,
      maxBrCount = 4,
      numFetchBufferEntries = 8,
      ftq = FtqParameters(nEntries=16),
      nPerfCounters = 2,
      fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))),
    dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBits,
                               nSets=64, nWays=4, nMSHRs=2, nTLBEntries=8)),
    icache = Some(ICacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=4, fetchBytes=2*4))
  )}
  case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 8)
  case XLen => 64
  case MaxHartIdBits => log2Up(site(BoomTilesKey).size)
})

/**
 * 2-wide BOOM. Try to match the Cortex-A9.
 */
class WithMediumBooms extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(
    core = b.core.copy(
      fetchWidth = 4,
      useCompressed = true,
      decodeWidth = 2,
      numRobEntries = 64,
      issueParams = Seq(
        IssueParams(issueWidth=1, numEntries=16, iqType=IQT_MEM.litValue, dispatchWidth=2),
        IssueParams(issueWidth=2, numEntries=16, iqType=IQT_INT.litValue, dispatchWidth=2),
        IssueParams(issueWidth=1, numEntries=16, iqType=IQT_FP.litValue , dispatchWidth=2)),
      numIntPhysRegisters = 80,
      numFpPhysRegisters = 64,
      numLdqEntries = 16,
      numStqEntries = 16,
      maxBrCount = 8,
      numFetchBufferEntries = 16,
      ftq = FtqParameters(nEntries=32),
      nPerfCounters = 6,
      fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))),
    dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBits,
                                 nSets=64, nWays=4, nMSHRs=2, nTLBEntries=8)),
    icache = Some(ICacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=4, fetchBytes=2*4))
    )}
  case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 8)
  case XLen => 64
  case MaxHartIdBits => log2Up(site(BoomTilesKey).size)

})

// DOC include start: LargeBoomConfig
/**
 * 3-wide BOOM. Try to match the Cortex-A15.
 */
class WithLargeBooms extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(
    core = b.core.copy(
      fetchWidth = 8,
      useCompressed = true,
      decodeWidth = 3,
      numRobEntries = 96,
      issueParams = Seq(
        IssueParams(issueWidth=1, numEntries=24, iqType=IQT_MEM.litValue, dispatchWidth=3),
        IssueParams(issueWidth=2, numEntries=24, iqType=IQT_INT.litValue, dispatchWidth=3),
        IssueParams(issueWidth=1, numEntries=24, iqType=IQT_FP.litValue , dispatchWidth=3)),
      numIntPhysRegisters = 100,
      numFpPhysRegisters = 96,
      numLdqEntries = 24,
      numStqEntries = 24,
      maxBrCount = 12,
      numFetchBufferEntries = 24,
      ftq = FtqParameters(nEntries=32),
      fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))),
    dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBytes*8,
                               nSets=64, nWays=8, nMSHRs=4, nTLBEntries=16)),
    icache = Some(ICacheParams(fetchBytes = 4*4, rowBits = site(SystemBusKey).beatBytes*8, nSets=64, nWays=8))
  )}
  case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 16)
  case XLen => 64
  case MaxHartIdBits => log2Up(site(BoomTilesKey).size)
})
// DOC include end: LargeBoomConfig

/**
 * 4-wide BOOM.
 */
class WithMegaBooms extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(
    core = b.core.copy(
      fetchWidth = 8,
      useCompressed = true,
      decodeWidth = 4,
      numRobEntries = 128,
      issueParams = Seq(
        IssueParams(issueWidth=2, numEntries=32, iqType=IQT_MEM.litValue, dispatchWidth=4),
        IssueParams(issueWidth=3, numEntries=32, iqType=IQT_INT.litValue, dispatchWidth=4),
        IssueParams(issueWidth=2, numEntries=32, iqType=IQT_FP.litValue , dispatchWidth=4)),
      numIntPhysRegisters = 128,
      numFpPhysRegisters = 128,
      numLdqEntries = 32,
      numStqEntries = 32,
      maxBrCount = 16,
      numFetchBufferEntries = 32,
      enablePrefetching=true,
      numDCacheBanks=2,
      ftq = FtqParameters(nEntries=32),
      fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))),
    dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBytes*8,
                               nSets=64, nWays=8, nMSHRs=8, nTLBEntries=32)),
    icache = Some(ICacheParams(fetchBytes = 4*4, rowBits = site(SystemBusKey).beatBytes*8, nSets=64, nWays=8, prefetch=true))
  )}
  case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 16)
  case XLen => 64
  case MaxHartIdBits => log2Up(site(BoomTilesKey).size)
})
