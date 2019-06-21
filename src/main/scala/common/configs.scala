//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

package boom.common

import chisel3._
import chisel3.util.{log2Up}

import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.subsystem.{SystemBusKey}
import freechips.rocketchip.devices.tilelink.{BootROMParams}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

import boom.ifu._
import boom.bpu._
import boom.exu._
import boom.lsu._
import boom.system.{BoomTilesKey}

/**
 * Baseline BOOM configuration.
 */
class BaseBoomConfig extends Config((site, here, up) => {
  // Top-Level
  case XLen => 64

  // Specify things which are typically common between core configs.
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(
    core = b.core.copy(
      fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true)))
  )}

  // Make sure there are enough hart bits to support multiple cores
  case MaxHartIdBits => log2Up(site(BoomTilesKey).size)
})

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
 * Combines the Memory and Integer Issue Queues. Similar to BOOM v1.
 */
class WithUnifiedMemIntIQs extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b =>
    b.copy(core = b.core.copy(
      issueParams = b.core.issueParams.filter(_.iqType != IQT_MEM.litValue)
                      .map(iq => if (iq.iqType == IQT_INT.litValue && iq.issueWidth < b.core.decodeWidth)
                                   iq.copy(issueWidth=b.core.decodeWidth) else iq)
    ))
  }
})

/**
  * Adds a boot ROM.
  */
class WithBootROM extends Config((site, here, up) => {
  case BootROMParams => BootROMParams(contentFileName = s"./bootrom/bootrom.rv${site(XLen)}.img")
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
 * Enable RVC
 */
class WithRVC extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(
    core = b.core.copy(
      fetchWidth = b.core.fetchWidth * 2,
      useCompressed = true))}
})

/**
 * 1-wide BOOM.
 */
class WithSmallBooms extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(
    core = b.core.copy(
      fetchWidth = 2,
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
      bpdBaseOnly = None,
      ftq = FtqParameters(nEntries=8),
      gshare = Some(GShareParameters(historyLength=11, numSets=2048)),
      tage = None,
      bpdRandom = None,
      nPerfCounters = 2),
    dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBits,
                               nSets=64, nWays=4, nMSHRs=2, nTLBEntries=8)),
    icache = Some(ICacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=4, fetchBytes=2*4))
    )}
  case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 8)
})

/**
 * 2-wide BOOM. Try to match the Cortex-A9.
 */
class WithMediumBooms extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(
    core = b.core.copy(
      fetchWidth = 2,
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
      ftq = FtqParameters(nEntries=32),
      btb = BoomBTBParameters(btbsa=true, densebtb=false, nSets=64, nWays=2,
                              nRAS=8, tagSz=20, bypassCalls=false, rasCheckForEmpty=false),
      bpdBaseOnly = None,
      gshare = Some(GShareParameters(historyLength=23, numSets=4096)),
      tage = None,
      bpdRandom = None,
      nPerfCounters = 6,
      fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))),
    dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBits,
                                 nSets=64, nWays=4, nMSHRs=2, nTLBEntries=8)),
    icache = Some(ICacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=4, fetchBytes=2*4))
    )}
  case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 8)
})

/**
 * 3-wide BOOM. Try to match the Cortex-A15.
 */
class WithLargeBooms extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(
    core = b.core.copy(
      fetchWidth = 4,
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
      ftq = FtqParameters(nEntries=32),
      btb = BoomBTBParameters(btbsa=true, densebtb=false, nSets=512, nWays=4, nRAS=16, tagSz=20),
      bpdBaseOnly = None,
      gshare = Some(GShareParameters(historyLength=23, numSets=4096)),
      tage = None,
      bpdRandom = None),
    dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBytes*8,
                               nSets=64, nWays=8, nMSHRs=4, nTLBEntries=16)),
    icache = Some(ICacheParams(fetchBytes = 4*4, rowBits = site(SystemBusKey).beatBytes*8, nSets=64, nWays=8))
    )}
  case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 16)
})

/**
 * 4-wide BOOM.
 */
class WithMegaBooms extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(
    core = b.core.copy(
      fetchWidth = 4,
      decodeWidth = 4,
      numRobEntries = 128,
      issueParams = Seq(
        IssueParams(issueWidth=1, numEntries=32, iqType=IQT_MEM.litValue, dispatchWidth=4),
        IssueParams(issueWidth=3, numEntries=32, iqType=IQT_INT.litValue, dispatchWidth=4),
        IssueParams(issueWidth=2, numEntries=32, iqType=IQT_FP.litValue , dispatchWidth=4)),
      numIntPhysRegisters = 128,
      numFpPhysRegisters = 128,
      numLdqEntries = 32,
      numStqEntries = 32,
      maxBrCount = 16,
      ftq = FtqParameters(nEntries=32),
      btb = BoomBTBParameters(btbsa=true, densebtb=false, nSets=512, nWays=4, nRAS=16, tagSz=20),
      bpdBaseOnly = None,
      gshare = Some(GShareParameters(historyLength=23, numSets=4096)),
      tage = None,
      bpdRandom = None),
    dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBytes*8,
                               nSets=64, nWays=8, nMSHRs=8, nTLBEntries=32)),
    icache = Some(ICacheParams(fetchBytes = 4*4, rowBits = site(SystemBusKey).beatBytes*8, nSets=64, nWays=8))
    )}
  case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 16)
})

