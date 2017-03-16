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
//   case BuildCore => (p: Parameters) => new BoomCore()(p)
   case BoomKey => BoomCoreParams()
   case XLen => 64

   case RocketTilesKey => up(RocketTilesKey, site) map { r =>
      r.copy(core = r.core.copy(
         useCompressed = false,
         nPerfCounters = 4,
         nPerfEvents = 31
      ))}


//   case CoreInstBits => 32
//   case UseCompressed => false
//         case FPUKey => Some(FPUConfig(
//            divSqrt = true,
//            sfmaLatency = 3,
//            dfmaLatency = 3))
//
//         // Uarch Performance Counters
//         case NPerfEvents => 31
//         case NPerfCounters => Knob("PERF_COUNTERS")
//
//         // Superscalar Widths
//         case FetchWidth => Knob("FETCH_WIDTH")
//         case IssueWidth => Knob("ISSUE_WIDTH")
//         case DecodeWidth => here(FetchWidth)
//         case DispatchWidth => here(DecodeWidth)
//         case RetireWidth => here(DecodeWidth)
//
//         // Data Structure Sizes
//         case NumRobEntries => Knob("ROB_ENTRIES")
//         case NumIssueSlotEntries => Knob("ISSUE_ENTRIES")
//         case NumLsuEntries=> Knob("LSU_ENTRIES")
//         case NumPhysRegisters => Knob("PHYS_REGISTERS")
//         case MaxBrCount => Knob("MAX_BR_COUNT")
//
//         // Front-end
//         case EnableBTB => true // for now, only gates off updates to BTB
//         case EnableBTBContainsBranches => true // don't send branches to BTB (but let jumps be predicted)
//         case BtbKey => BtbParameters(nEntries = 64, nRAS = 8, updatesOutOfOrder = true)
//         case FetchBufferSz => 4
//
//         // Branch Predictor (enable one of the following:)
//         case EnableBranchPredictor => true
         case TageKey => TageParameters()
//            enabled = false,
//            // 25 kB
//            num_tables      = 12,
//            table_sizes     = Seq(1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,512),
//            history_lengths = Seq(5, 11, 17, 27, 43, 67,105,165,258,407,637,1000),
//            tag_sizes       = Seq(8,  9,  9, 10, 10, 11, 11, 12, 12, 13, 13,  14))
//            // 16 kB
//            //num_tables = 4,
//            //table_sizes = Seq(2048,2048,2048,2048),
//            //history_lengths = Seq(5,15,44,130),
//            //tag_sizes = Seq(10,10,10,12))
//            // 8 kB
//            //num_tables = 4,
//            //table_sizes = Seq(1024,1024,1024,1024),
//            //history_lengths = Seq(3,7,19,63),
//            //tag_sizes = Seq(10,10,10,12))
         case GShareKey => GShareParameters(enabled = true)
         case GSkewKey => GSkewParameters(enabled = false)
//         // Only predict (and update) when in user-mode.
//         case EnableBpdUModeOnly => false
//         // Add a user+privileged global history register, separate from a user-only history regiser.
//         case EnableBpdUSModeHistory => false
//
//         // **** Pipelining ****
//
//         case EnableFetchBufferFlowThrough => false
//
//         // Immediately register the branch resolution information
//         // and wait for the next cycle to redirect the front-end.
//         case EnableBrResolutionRegister => true
//
//         // Track the committed rename state; allows for single-cycle resets.
//         case EnableCommitMapTable => false
//
//
//         // **** Extra Knobs and Features ****
//         case EnableAgePriorityIssue => Knob("AGE_PRIORITY_ISSUE")
//         case EnablePrefetching => false
//
//      }
//   },
//   knobValues = {
//      case "NTiles" => 1
//      case "FETCH_WIDTH" => 2
//      case "ISSUE_WIDTH" => 3
//      case "ROB_ENTRIES" => 48
//      case "ISSUE_ENTRIES" => 20
//      case "LSU_ENTRIES" => 16
//      case "PHYS_REGISTERS" => 110
//      case "MAX_BR_COUNT" => 8
//      case "AGE_PRIORITY_ISSUE" => true
//      case "L1D_MSHRS" => 2
//      case "L1D_WAYS" => 8
//      case "L1D_SETS" => 64
//      case "L1I_WAYS" => 8
//      case "L1I_SETS" => 64
//      case "PERF_COUNTERS" => 29
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
