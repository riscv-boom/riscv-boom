//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom
import Chisel._
import config.{Parameters, Config}
import coreplex._
import tile._
import rocket._


// Try to be a reasonable BOOM design point.
class DefaultBoomConfig extends Config((site, here, up) => {

   // Top-Level
   case BuildCore => (p: Parameters, e: uncore.tilelink2.TLEdgeOut) => new BoomCore()(p, e)
   case XLen => 64

   // Rocket/Core Parameters
   case RocketTilesKey => up(RocketTilesKey, site) map { r => r.copy(
      core = r.core.copy(
         fWidth = 2,
         useCompressed = false,
         nPerfCounters = 29,
         nPerfEvents = 50,
         perfIncWidth = 3, // driven by issue ports, as set in BoomCoreParams.issueParams
         fpu = Some(tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))),
      btb = Some(BTBParams(nEntries = 0, updatesOutOfOrder = true)),
      dcache = Some(DCacheParams(rowBits = site(L1toL2Config).beatBytes*8, nSets=64, nWays=8, nMSHRs=4, nTLBEntries=8)),
      icache = Some(ICacheParams(rowBits = site(L1toL2Config).beatBytes*8, nSets=64, nWays=8))
      )}

   // BOOM-specific uarch Parameters
   case BoomKey => BoomCoreParams(
      numRobEntries = 80,
      issueParams = Seq(
         IssueParams(issueWidth=1, numEntries=20, iqType=IQT_MEM.litValue),
         IssueParams(issueWidth=2, numEntries=20, iqType=IQT_INT.litValue),
         IssueParams(issueWidth=1, numEntries=20, iqType=IQT_FP.litValue)),
      numIntPhysRegisters = 100,
      numFpPhysRegisters = 64,
      numLsuEntries = 16,
      maxBrCount = 8,
      btb = BTBsaParameters(nSets=64, nWays=4, nRAS=8, tagSz=20),
      enableBranchPredictor = true,
      gshare = Some(GShareParameters(enabled = true, history_length=15))
   )
   // Widen L1toL2 bandwidth.
   case L1toL2Config => up(L1toL2Config, site).copy(
      beatBytes = site(XLen)/4)
  }
)


class WithNPerfCounters(n: Int) extends Config((site, here, up) => {
   case RocketTilesKey => up(RocketTilesKey, site) map { r => r.copy(core = r.core.copy(
      nPerfCounters = n
   ))}
})

// Small BOOM! Try to be fast to compile, easier to debug.
class WithSmallBooms extends Config((site, here, up) => {
   case RocketTilesKey => up(RocketTilesKey, site) map { r =>r.copy(core = r.core.copy(
      fWidth = 1,
      nPerfCounters = 2,
      perfIncWidth = 2 // driven by issue ports, as set in BoomCoreParams.issueParams
      ))}
   case BoomKey => up(BoomKey, site).copy(
      numRobEntries = 24,
      issueParams = Seq(
         IssueParams(issueWidth=1, numEntries=4, iqType=IQT_MEM.litValue),
         IssueParams(issueWidth=1, numEntries=4, iqType=IQT_INT.litValue),
         IssueParams(issueWidth=1, numEntries=4, iqType=IQT_FP.litValue)),
      numIntPhysRegisters = 56,
      numFpPhysRegisters = 48,
      numLsuEntries = 4,
      maxBrCount = 4,
      gshare = Some(GShareParameters(enabled = true, history_length=12))
      )
})


// try to match the Cortex-A9
class WithMediumBooms extends Config((site, here, up) => {
   case RocketTilesKey => up(RocketTilesKey, site) map { r =>r.copy(
      core = r.core.copy(
         fWidth = 2,
         //nPerfCounters = 6,
         perfIncWidth = 3, // driven by issue ports, as set in BoomCoreParams.issueParams
         fpu = Some(tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))),
      dcache = Some(DCacheParams(rowBits = site(L1toL2Config).beatBytes*8, nSets=64, nWays=4, nMSHRs=2, nTLBEntries=8)),
      icache = Some(ICacheParams(rowBits = site(L1toL2Config).beatBytes*8, nSets=64, nWays=4))
      )}
   case BoomKey => up(BoomKey, site).copy(
      numRobEntries = 48,
      issueParams = Seq(
         IssueParams(issueWidth=1, numEntries=20, iqType=IQT_MEM.litValue),
         IssueParams(issueWidth=2, numEntries=16, iqType=IQT_INT.litValue),
         IssueParams(issueWidth=1, numEntries=10, iqType=IQT_FP.litValue)),
      numIntPhysRegisters = 70,
      numFpPhysRegisters = 64,
      numLsuEntries = 16,
      maxBrCount = 8,
      regreadLatency = 1,
      renameLatency = 2,
      enableBpdF2Redirect = false,
      btb = BTBsaParameters(nSets=64, nWays=2, nRAS=8, tagSz=20, bypassCalls=false, rasCheckForEmpty=false),
      gshare = Some(GShareParameters(enabled=true, history_length=13))
      )
})


// try to match the Cortex-A15
class WithMegaBooms extends Config((site, here, up) => {
   case RocketTilesKey => up(RocketTilesKey, site) map { r => r.copy(core = r.core.copy(
      fWidth = 4,
      perfIncWidth = 3 // driven by issue ports, as set in BoomCoreParams.issueParams
      ))}
   case BoomKey => up(BoomKey, site).copy(
      numRobEntries = 128,
      issueParams = Seq(
         IssueParams(issueWidth=1, numEntries=20, iqType=IQT_MEM.litValue),
         IssueParams(issueWidth=2, numEntries=20, iqType=IQT_INT.litValue),
         IssueParams(issueWidth=1, numEntries=20, iqType=IQT_FP.litValue)), // TODO make this 2-wide issue
      numIntPhysRegisters = 128,
      numFpPhysRegisters = 80,
      numLsuEntries = 32,
      gshare = Some(GShareParameters(enabled=true, history_length=15))
      // tage is unsupported in boomv2 for now.
      //tage = Some(TageParameters())
      )
   // Widen L1toL2 bandwidth so we can increase icache rowBytes size for 4-wide fetch.
   // a beatsize of 32 bytes causes issues (see https://github.com/ucb-bar/riscv-boom/issues/30)).
   //case L1toL2Config => up(L1toL2Config, site).copy(
   //   beatBytes = site(XLen)/2)

})
