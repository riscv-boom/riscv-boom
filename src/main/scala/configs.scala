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
   case RocketTilesKey => up(RocketTilesKey, site) map { r => r.copy(core = r.core.copy(
      fWidth = 2,
      useCompressed = false,
      nPerfCounters = 4,
      nPerfEvents = 31,
      fpu = None //Some(tile.FPUParams(sfmaLatency=3, dfmaLatency=3))
   ))}

   // BOOM-specific uarch Parameters
   case BoomKey => BoomCoreParams(
      numRobEntries = 48,
      issueWidths = Seq(1, 2, 0), // Mem, Int, FP
      numIssueSlotEntries = Seq(16, 16, 16), // Mem, Int, FP
      numIntPhysRegisters = 80,
      numFpPhysRegisters = 56,
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
      nPerfCounters = 1
      ))}
   case BoomKey => up(BoomKey, site).copy(
      numRobEntries = 24,
      issueWidths = Seq(1, 1, 0), // Mem, Int, FP
      numIssueSlotEntries = Seq(4, 4, 4), // Mem, Int, FP
      numIntPhysRegisters = 56,
      numFpPhysRegisters = 48,
      numLsuEntries = 4,
      maxBrCount = 4,
      gshare = Some(GShareParameters(enabled = true, history_length=11))
      )
})


// try to match the Cortex-A9
class WithMediumBooms extends Config((site, here, up) => {
   case RocketTilesKey => up(RocketTilesKey, site) map { r =>r.copy(core = r.core.copy(
      fWidth = 2))}
   case BoomKey => up(BoomKey, site).copy(
      numRobEntries = 48,
      issueWidths = Seq(1, 2, 0), // Mem, Int, FP
      numIssueSlotEntries = Seq(16, 16, 16), // Mem, Int, FP
      numIntPhysRegisters = 80,
      numFpPhysRegisters = 56,
      numLsuEntries = 16,
      gshare = Some(GShareParameters(enabled = true, history_length=11))
      )
})


//// try to match the Cortex-A15
class WithMegaBooms extends Config((site, here, up) => {
   case RocketTilesKey => up(RocketTilesKey, site) map { r => r.copy(core = r.core.copy(
      fWidth = 4))}
   case BoomKey => up(BoomKey, site).copy(
      numRobEntries = 128,
      issueWidths = Seq(1, 2, 0), // Mem, Int, FP
      numIssueSlotEntries = Seq(20, 16, 20), // Mem, Int, FP
      numIntPhysRegisters = 128,
      numFpPhysRegisters = 64,
      numLsuEntries = 32,
      gshare = Some(GShareParameters(enabled = true, history_length=11))
      )
   // Widen L1toL2 bandwidth so we can increase icache rowBytes size for 4-wide fetch.
   case L1toL2Config => up(L1toL2Config, site).copy(
      beatBytes = site(XLen)/4)

})
