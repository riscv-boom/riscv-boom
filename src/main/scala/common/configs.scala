//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom
import Chisel._
import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.subsystem.{SystemBusKey}
import freechips.rocketchip.devices.tilelink.{BootROMParams}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

import boom.system.BoomTilesKey


// Try to be a reasonable BOOM design point.
class DefaultBoomConfig extends Config((site, here, up) => {

   // Top-Level
   case XLen => 64

   // Use this boot ROM for SimDTM.
   case BootROMParams => BootROMParams(contentFileName = "./rocket-chip/bootrom/bootrom.img")

   // Core Parameters
   case BoomTilesKey => up(BoomTilesKey, site) map { r => r.copy(
      core = r.core.copy(
         fetchWidth = 2,
         decodeWidth = 2,
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
         bpd_base_only = Some(BaseOnlyParameters(enabled=true)),
//         gshare = Some(GShareParameters(enabled=true, history_length=15)),
         nPerfCounters = 29,
         fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))),
      btb = Some(BTBParams(nEntries = 0, updatesOutOfOrder = true)),
      dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=8, nMSHRs=4, nTLBEntries=16)),
      icache = Some(ICacheParams(fetchBytes = 2*4, rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=8))
      )}
})


class WithNPerfCounters(n: Int) extends Config((site, here, up) => {
   case BoomTilesKey => up(BoomTilesKey, site) map { r => r.copy(core = r.core.copy(
      nPerfCounters = n
   ))}
})


// Small BOOM! Try to be fast to compile and easier to debug.
class WithSmallBooms extends Config((site, here, up) => {
   case BoomTilesKey => up(BoomTilesKey, site) map { r =>r.copy(
      core = r.core.copy(
         fetchWidth = 2,
         decodeWidth = 2,
         numRobEntries = 16,
         issueParams = Seq(
            IssueParams(issueWidth=1, numEntries=4, iqType=IQT_MEM.litValue),
            IssueParams(issueWidth=1, numEntries=4, iqType=IQT_INT.litValue),
            IssueParams(issueWidth=1, numEntries=4, iqType=IQT_FP.litValue)),
         numIntPhysRegisters = 56,
         numFpPhysRegisters = 48,
         numLsuEntries = 4,
         maxBrCount = 4,
//         gshare = Some(GShareParameters(enabled = true, history_length=12)),
         bpd_base_only = Some(BaseOnlyParameters(enabled=true)),
         nPerfCounters = 2),
      icache = Some(r.icache.get.copy(fetchBytes=2*4))
      )}
})


// Try to match the Cortex-A9.
class WithMediumBooms extends Config((site, here, up) => {
   case BoomTilesKey => up(BoomTilesKey, site) map { r =>r.copy(
      core = r.core.copy(
         fetchWidth = 2,
         decodeWidth = 2,
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
         btb = BTBsaParameters(nSets=64, nWays=2, nRAS=8, tagSz=20, bypassCalls=false, rasCheckForEmpty=false),
//         gshare = Some(GShareParameters(enabled=true, history_length=14)),
         bpd_base_only = Some(BaseOnlyParameters(enabled=true)),
         nPerfCounters = 6,
         fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))),
      dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=4, nMSHRs=2, nTLBEntries=8)),
      icache = Some(ICacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=4, fetchBytes=2*4))
      )}
})


// Try to match the Cortex-A15. Don't expect good QoR (yet).
class WithMegaBooms extends Config((site, here, up) => {
   case BoomTilesKey => up(BoomTilesKey, site) map { r =>r.copy(
      core = r.core.copy(
         fetchWidth = 4,
         decodeWidth = 4,
         numRobEntries = 128,
         issueParams = Seq(
            IssueParams(issueWidth=1, numEntries=20, iqType=IQT_MEM.litValue),
            IssueParams(issueWidth=2, numEntries=20, iqType=IQT_INT.litValue),
            IssueParams(issueWidth=1, numEntries=20, iqType=IQT_FP.litValue)), // TODO make this 2-wide issue
         numIntPhysRegisters = 128,
         numFpPhysRegisters = 64,
         numLsuEntries = 32,
         maxBrCount = 16,
         btb = BTBsaParameters(nSets=128, nWays=4, nRAS=16, tagSz=20),
//         gshare = Some(GShareParameters(enabled=true, history_length=15))),
         bpd_base_only = Some(BaseOnlyParameters(enabled=true))),
         // tage is unsupported in boomv2 for now.
         //tage = Some(TageParameters())
      dcache = Some(DCacheParams(rowBits = site(SystemBusKey).beatBytes*8, nSets=64, nWays=16, nMSHRs=4, nTLBEntries=8)),
      icache = Some(ICacheParams(fetchBytes = 4*4, rowBits = site(SystemBusKey).beatBytes*8, nSets=64, nWays=8))
      )}
   // Set TL network to 128bits wide
   case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 16)
})

