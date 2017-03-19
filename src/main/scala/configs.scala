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

         // Front-end
//         case EnableBTB => true // for now, only gates off updates to BTB
//         case EnableBTBContainsBranches => true // don't send branches to BTB (but let jumps be predicted)
//         case BtbKey => BtbParameters(nEntries = 64, nRAS = 8, updatesOutOfOrder = true)
//         case FetchBufferSz => 4

class DefaultBoomConfig extends Config((site, here, up) => {

   // Top-Level
   case BuildCore => (p: Parameters, e: uncore.tilelink2.TLEdgeOut) => new BoomCore()(p, e)
   case XLen => 64

   // Rocket/Core Parameters
   case RocketTilesKey => up(RocketTilesKey, site) map { r =>
      r.copy(core = r.core.copy(
         fWidth = 2,
         useCompressed = false,
         nPerfCounters = 4,
         nPerfEvents = 31,
         fpu = Some(tile.FPUParams(sfmaLatency=3, dfmaLatency=3))
      ))}

   // BOOM-specific uarch Parameters
   case BoomKey => BoomCoreParams(
      issueWidth = 3,
      numRobEntries = 48,
      numIssueSlotEntries = 20,
      numPhysRegisters = 110,
      numLsuEntries = 16,
      maxBrCount = 8,
      enableBranchPredictor = true
   )
   // TODO put these keys into the BoomParams
   case TageKey => TageParameters(enabled = true)
   case GShareKey => GShareParameters(enabled = false)
   case GSkewKey => GSkewParameters(enabled = false)
  }
)

//class WithNPerfCounters(n: Int) extends Config(
//  knobValues = {case "PERF_COUNTERS" => n; case _ => throw new CDEMatchError })
//
//class WithSmallBOOMs extends Config(
//   knobValues = {
//      case "FETCH_WIDTH" => 1
//      case "ISSUE_WIDTH" => 1
//      case "ROB_ENTRIES" => 24
//      case "ISSUE_ENTRIES" => 10
//      case "LSU_ENTRIES" => 4
//      case "PHYS_REGISTERS" => 100
//      case "MAX_BR_COUNT" => 4
//      case "PERF_COUNTERS" => 1
//   }
//)
//
//// try to match the Cortex-A9
//class WithMediumBOOMs extends Config(
//   knobValues = {
//      case "FETCH_WIDTH" => 2
//      case "ISSUE_WIDTH" => 3
//      case "ROB_ENTRIES" => 48
//      case "ISSUE_ENTRIES" => 20
//      case "LSU_ENTRIES" => 16
//      case "PHYS_REGISTERS" => 110
//      case "MAX_BR_COUNT" => 8
//      case "L1D_MSHRS" => 4
//      case "L1D_WAYS" => 8
//      case "L1D_SETS" => 64
//      case "L1I_WAYS" => 8
//      case "L1I_SETS" => 64
//   }
//)
//
//// try to match the Cortex-A15
//class WithMegaBOOMs extends Config(
//   knobValues = {
//      case "FETCH_WIDTH" => 4
//      case "ISSUE_WIDTH" => 4
//      case "ROB_ENTRIES" => 128
//      case "ISSUE_ENTRIES" => 28
//      case "LSU_ENTRIES" => 32
//      case "PHYS_REGISTERS" => 128
//      case "MAX_BR_COUNT" => 8
//      case "L1D_MSHRS" => 4
//      case "L1D_WAYS" => 8
//      case "L1D_SETS" => 64
//      case "L1I_WAYS" => 8
//      case "L1I_SETS" => 64
//   }
//)
