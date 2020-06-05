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

class WithBoomCommitLogPrintf extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b =>
    b.copy(core = b.core.copy(enableCommitLogPrintf = true))
  }
})

class WithBoomBranchPrintf extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b =>
    b.copy(core = b.core.copy(enableBranchPrintf = true))
  }
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
 * Create multiple copies of a BOOM tile (and thus a core).
 * Override with the default fragments to control all params of the tiles.
 * Default adds small BOOMs.
 *
 * @param n amount of tiles to duplicate
 */
class WithNBoomCores(n: Int) extends Config(
  new WithSmallBooms ++
  new WithTAGELBPD ++ // Default to TAGE-L BPD
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
 * 1-wide BOOM.
 */
class WithSmallBooms extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(
    core = b.core.copy(
      fetchWidth = 4,
      decodeWidth = 1,
      numRobEntries = 32,
      issueParams = Seq(
        IssueParams(issueWidth=2, numEntries=8, iqType=IQT_MEM.litValue, dispatchWidth=1),
        IssueParams(issueWidth=1, numEntries=8, iqType=IQT_INT.litValue, dispatchWidth=1),
        IssueParams(issueWidth=1, numEntries=8, iqType=IQT_FP.litValue , dispatchWidth=1)),
      numIntPhysRegisters = 52,
      numFpPhysRegisters = 48,
      numLdqEntries = 8,
      numStqEntries = 8,
      maxBrCount = 8,
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
      decodeWidth = 2,
      numRobEntries = 64,
      issueParams = Seq(
        IssueParams(issueWidth=2, numEntries=12, iqType=IQT_MEM.litValue, dispatchWidth=2),
        IssueParams(issueWidth=2, numEntries=20, iqType=IQT_INT.litValue, dispatchWidth=2),
        IssueParams(issueWidth=1, numEntries=16, iqType=IQT_FP.litValue , dispatchWidth=2)),
      numIntPhysRegisters = 80,
      numFpPhysRegisters = 64,
      numLdqEntries = 16,
      numStqEntries = 16,
      maxBrCount = 12,
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
      decodeWidth = 3,
      numRobEntries = 96,
      issueParams = Seq(
        IssueParams(issueWidth=2, numEntries=16, iqType=IQT_MEM.litValue, dispatchWidth=3),
        IssueParams(issueWidth=3, numEntries=32, iqType=IQT_INT.litValue, dispatchWidth=3),
        IssueParams(issueWidth=1, numEntries=24, iqType=IQT_FP.litValue , dispatchWidth=3)),
      numIntPhysRegisters = 100,
      numFpPhysRegisters = 96,
      numLdqEntries = 24,
      numStqEntries = 24,
      maxBrCount = 16,
      enableSFBOpt = true,
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
      decodeWidth = 4,
      numRobEntries = 128,
      issueParams = Seq(
        IssueParams(issueWidth=3, numEntries=24, iqType=IQT_MEM.litValue, dispatchWidth=4),
        IssueParams(issueWidth=4, numEntries=40, iqType=IQT_INT.litValue, dispatchWidth=4),
        IssueParams(issueWidth=2, numEntries=32, iqType=IQT_FP.litValue , dispatchWidth=4)),
      lsuWidth = 2,
      numIntPhysRegisters = 128,
      numFpPhysRegisters = 128,
      numLdqEntries = 32,
      numStqEntries = 32,
      maxBrCount = 20,
      numFetchBufferEntries = 32,
      enablePrefetching=true,
      enableSFBOpt=true,
      numDCacheBanks=4,
      ftq = FtqParameters(nEntries=40),
      fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))),
    dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBytes*8,
                               nSets=64, nWays=8, nMSHRs=8, nTLBEntries=32)),
    icache = Some(ICacheParams(fetchBytes = 4*4, rowBits = site(SystemBusKey).beatBytes*8, nSets=64, nWays=8, prefetch=true))
  )}
  case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 16)
  case XLen => 64
  case MaxHartIdBits => log2Up(site(BoomTilesKey).size)
})


/**
 * 5-wide BOOM.
 */
class WithGigaBooms extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => b.copy(
    core = b.core.copy(
      fetchWidth = 8,
      decodeWidth = 5,
      numRobEntries = 130,
      issueParams = Seq(
        IssueParams(issueWidth=2, numEntries=24, iqType=IQT_MEM.litValue, dispatchWidth=5),
        IssueParams(issueWidth=5, numEntries=40, iqType=IQT_INT.litValue, dispatchWidth=5),
        IssueParams(issueWidth=2, numEntries=32, iqType=IQT_FP.litValue , dispatchWidth=5)),
      lsuWidth = 2,
      numIntPhysRegisters = 128,
      numFpPhysRegisters = 128,
      numLdqEntries = 32,
      numStqEntries = 32,
      maxBrCount = 20,
      numFetchBufferEntries = 35,
      enableSFBOpt=true,
      enablePrefetching=true,
      numDCacheBanks=1, // Duplicate the DCache. For Science
      ftq = FtqParameters(nEntries=40),
      fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))),
    dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBytes*8,
                               nSets=64, nWays=8, nMSHRs=8, nTLBEntries=32)),
    icache = Some(ICacheParams(fetchBytes = 4*4, rowBits = site(SystemBusKey).beatBytes*8, nSets=64, nWays=8, prefetch=true))
  )}
  case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 16)
  case XLen => 64
  case MaxHartIdBits => log2Up(site(BoomTilesKey).size)
})


/**
  * BOOM Configs for CS152 lab
  */
class WithCS152BaselineBooms extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => {
    val coreWidth = 1                     // CS152: Change me (1 to 4)
    val lsuWidth = 1                      // CS152: Change me (1 or 2)
    b.copy(
      core = b.core.copy(
        fetchWidth = 4,                   // CS152: Change me (4 or 8)
        numRobEntries = 4,                // CS152: Change me (2+)
        numIntPhysRegisters = 33,         // CS152: Change me (33+)
        numLdqEntries = 8,                // CS152: Change me (2+)
        numStqEntries = 8,                // CS152: Change me (2+)
        maxBrCount = 8,                   // CS152: Change me (2+)
        enableBranchPrediction = false,   // CS152: Change me
        numRasEntries = 0,                // CS152: Change me

        // DO NOT CHANGE BELOW
        enableBranchPrintf = true,
        decodeWidth = coreWidth,
        numFetchBufferEntries = coreWidth * 8,
        numDCacheBanks = lsuWidth,
        lsuWidth = lsuWidth,
        issueParams = Seq(
          IssueParams(issueWidth=2,         numEntries=8,  iqType=IQT_MEM.litValue, dispatchWidth=coreWidth),
          IssueParams(issueWidth=coreWidth, numEntries=32, iqType=IQT_INT.litValue, dispatchWidth=coreWidth),
          IssueParams(issueWidth=1,         numEntries=4,  iqType=IQT_FP.litValue , dispatchWidth=coreWidth))
        // DO NOT CHANGE ABOVE
      ),
      dcache = Some(DCacheParams(
        rowBits=site(SystemBusKey).beatBytes*8,
        nSets=64, // CS152: Change me (must be pow2, 2-64)
        nWays=4,  // CS152: Change me (1-8)
        nMSHRs=2  // CS152: Change me (1+)
      ))
    )
  }}
})


class WithCS152DefaultBooms extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b => {
    val coreWidth = 3                    // CS152: Change me (1 to 4)
    val lsuWidth = 1                     // CS152: Change me (1 or 2)
    val nIssueSlots = 32                 // CS152: Change me (2+)
    b.copy(
      core = b.core.copy(
        fetchWidth = 4,                  // CS152: Change me (4 or 8)
        numRobEntries = 96,              // CS152: Change me (2+)
        numIntPhysRegisters = 96,        // CS152: Change me (33+)
        numLdqEntries = 16,              // CS152: Change me (2+)
        numStqEntries = 16,              // CS152: Change me (2+)
        maxBrCount = 12,                 // CS152: Change me (2+)
        enableBranchPrediction = true,   // CS152: Change me
        numRasEntries = 16,              // CS152: Change me

        // DO NOT CHANGE BELOW
        enableBranchPrintf = true,
        decodeWidth = coreWidth,
        numFetchBufferEntries = coreWidth * 8,
        numDCacheBanks = lsuWidth,
        lsuWidth = lsuWidth,
        issueParams = Seq(
          IssueParams(issueWidth=2,         numEntries=nIssueSlots, iqType=IQT_MEM.litValue, dispatchWidth=coreWidth),
          IssueParams(issueWidth=coreWidth, numEntries=nIssueSlots, iqType=IQT_INT.litValue, dispatchWidth=coreWidth),
          IssueParams(issueWidth=1,         numEntries=nIssueSlots, iqType=IQT_FP.litValue , dispatchWidth=coreWidth))
        // DO NOT CHANGE ABOVE
      ),
      dcache = Some(DCacheParams(
        rowBits=site(SystemBusKey).beatBytes*8,
        nSets=64, // CS152: Change me (must be pow2, 2-64)
        nWays=4,  // CS152: Change me (1-8)
        nMSHRs=2  // CS152: Change me (1+)
      ))
    )
  }}
})



/**
  *  Branch prediction configs below
  */

class WithTAGELBPD extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b =>
    b.copy(core = b.core.copy(
      bpdMaxMetaLength = 120,
      globalHistoryLength = 64,
      localHistoryLength = 1,
      localHistoryNSets = 0,
      branchPredictor = ((resp_in: BranchPredictionBankResponse, p: Parameters) => {
        val loop = Module(new LoopBranchPredictorBank()(p))
        val tage = Module(new TageBranchPredictorBank()(p))
        val btb = Module(new BTBBranchPredictorBank()(p))
        val bim = Module(new BIMBranchPredictorBank()(p))
        val ubtb = Module(new FAMicroBTBBranchPredictorBank()(p))
        val preds = Seq(loop, tage, btb, ubtb, bim)
        preds.map(_.io := DontCare)

        ubtb.io.resp_in(0)  := resp_in
        bim.io.resp_in(0)   := ubtb.io.resp
        btb.io.resp_in(0)   := bim.io.resp
        tage.io.resp_in(0)  := btb.io.resp
        loop.io.resp_in(0)  := tage.io.resp

        (preds, loop.io.resp)
      })
    ))
  }
})

class WithBoom2BPD extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b =>
    b.copy(core = b.core.copy(
      bpdMaxMetaLength = 45,
      globalHistoryLength = 16,
      localHistoryLength = 1,
      localHistoryNSets = 0,
      branchPredictor = ((resp_in: BranchPredictionBankResponse, p: Parameters) => {
        // gshare is just variant of TAGE with 1 table
        val gshare = Module(new TageBranchPredictorBank(
          BoomTageParams(tableInfo = Seq((256, 16, 7)))
        )(p))
        val btb = Module(new BTBBranchPredictorBank()(p))
        val bim = Module(new BIMBranchPredictorBank()(p))
        val preds = Seq(bim, btb, gshare)
        preds.map(_.io := DontCare)

        bim.io.resp_in(0)  := resp_in
        btb.io.resp_in(0)  := bim.io.resp
        gshare.io.resp_in(0) := btb.io.resp
        (preds, gshare.io.resp)
      })
    ))
  }
})

class WithAlpha21264BPD extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b =>
    b.copy(core = b.core.copy(
      bpdMaxMetaLength = 64,
      globalHistoryLength = 32,
      localHistoryLength = 32,
      localHistoryNSets = 128,
      branchPredictor = ((resp_in: BranchPredictionBankResponse, p: Parameters) => {
        val btb = Module(new BTBBranchPredictorBank()(p))
        val gbim = Module(new HBIMBranchPredictorBank()(p))
        val lbim = Module(new HBIMBranchPredictorBank(BoomHBIMParams(useLocal=true))(p))
        val tourney = Module(new TourneyBranchPredictorBank()(p))
        val preds = Seq(lbim, btb, gbim, tourney)
        preds.map(_.io := DontCare)

        gbim.io.resp_in(0) := resp_in
        lbim.io.resp_in(0) := resp_in
        tourney.io.resp_in(0) := gbim.io.resp
        tourney.io.resp_in(1) := lbim.io.resp
        btb.io.resp_in(0)  := tourney.io.resp

        (preds, btb.io.resp)
      })
    ))
  }
})


class WithSWBPD extends Config((site, here, up) => {
  case BoomTilesKey => up(BoomTilesKey, site) map { b =>
    b.copy(core = b.core.copy(
      bpdMaxMetaLength = 1,
      globalHistoryLength = 32,
      localHistoryLength = 1,
      localHistoryNSets = 0,
      branchPredictor = ((resp_in: BranchPredictionBankResponse, p: Parameters) => {
        val sw = Module(new SwBranchPredictorBank()(p))

        sw.io.resp_in(0) := resp_in

        (Seq(sw), sw.io.resp)
      })
    ))
  }
})
