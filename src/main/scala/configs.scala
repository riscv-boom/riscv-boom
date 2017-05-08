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


class DefaultBoomConfig extends Config((site, here, up) => {

   // Top-Level
   case BuildCore => (p: Parameters, e: uncore.tilelink2.TLEdgeOut) => new BoomCore()(p, e)
   case XLen => 64

   // Rocket/Core Parameters
   case RocketTilesKey => up(RocketTilesKey, site) map { r => r.copy(
      core = r.core.copy(
         fWidth = 2,
         useCompressed = false,
         nPerfCounters = 4,
         nPerfEvents = 48,
         perfIncWidth = 3, // driven by issue ports, as set in BoomCoreParams.issueParams
         fpu = Some(tile.FPUParams(sfmaLatency=3, dfmaLatency=3, divSqrt=true))),
      btb = Some(BTBParams(nEntries = 40, nRAS = 4, updatesOutOfOrder = true))
   )}

   // BOOM-specific uarch Parameters
   case BoomKey => BoomCoreParams(
      issueWidth = 3,
      numRobEntries = 48,
      numIssueSlotEntries = 20,
      numPhysRegisters = 110,
      numLsuEntries = 16,
      maxBrCount = 8,
      enableBranchPredictor = true,
      gshare = Some(GShareParameters(enabled = true, history_length=11))
   )
  }
)


class WithNPerfCounters(n: Int) extends Config((site, here, up) => {
   case RocketTilesKey => up(RocketTilesKey, site) map { r => r.copy(core = r.core.copy(
      nPerfCounters = n
   ))}
})

// Small BOOM!
class WithSmallBooms extends Config((site, here, up) => {
   case RocketTilesKey => up(RocketTilesKey, site) map { r =>r.copy(core = r.core.copy(
      fWidth = 1,
      nPerfCounters = 2,
      perfIncWidth = 2 // driven by issue ports, as set in BoomCoreParams.issueParams
      ))}
   case BoomKey => up(BoomKey, site).copy(
      issueWidth = 1,
      numRobEntries = 24,
      numIssueSlotEntries = 10,
      numLsuEntries = 4,
      numPhysRegisters = 100,
      maxBrCount = 4,
      gshare = Some(GShareParameters(enabled = true, history_length=11))
      )
})


// try to match the Cortex-A9
class WithMediumBooms extends Config((site, here, up) => {
   case RocketTilesKey => up(RocketTilesKey, site) map { r =>r.copy(core = r.core.copy(
      fWidth = 2,
      perfIncWidth = 3 // driven by issue ports, as set in BoomCoreParams.issueParams
      ))}
   case BoomKey => up(BoomKey, site).copy(
      issueWidth = 3,
      numRobEntries = 48,
      numIssueSlotEntries = 20,
      numLsuEntries = 16,
      numPhysRegisters = 110,
      gshare = Some(GShareParameters(enabled = true, history_length=14))
      )
})


//// try to match the Cortex-A15
class WithMegaBooms extends Config((site, here, up) => {
   case RocketTilesKey => up(RocketTilesKey, site) map { r => r.copy(core = r.core.copy(
      fWidth = 4,
      perfIncWidth = 3 // driven by issue ports, as set in BoomCoreParams.issueParams
      ))}
   case BoomKey => up(BoomKey, site).copy(
      issueWidth = 4,
      numRobEntries = 128,
      numIssueSlotEntries = 28,
      numLsuEntries = 32,
      numPhysRegisters = 128,
      gshare = Some(GShareParameters(enabled = true, history_length=11))
      )
   // Widen L1toL2 bandwidth so we can increase icache rowBytes size for 4-wide fetch.
   case L1toL2Config => up(L1toL2Config, site).copy(
      beatBytes = site(XLen)/4)

})
