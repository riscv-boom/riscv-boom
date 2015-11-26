//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom
import Chisel._
import rocket._

class DefaultBOOMConfig extends ChiselConfig (
   topDefinitions = {
      (pname,site,here) => pname match {

         // Top-Level
         case CoreName => "BOOM"
         case XLen => 64
         case FDivSqrt => false
         case NPTWPorts => 2
         case CoreInstBits => 32

         // Superscalar Widths
         case FetchWidth => Knob("FETCH_WIDTH")
         case IssueWidth => Knob("ISSUE_WIDTH")
         case DecodeWidth => here(FetchWidth)
         case DispatchWidth => here(DecodeWidth)
         case RetireWidth => here(DecodeWidth)

         // Data Structure Sizes
         case NumRobEntries => Knob("ROB_ENTRIES")
         case NumIssueSlotEntries => Knob("ISSUE_ENTRIES")
         case NumLsuEntries=> Knob("LSU_ENTRIES")
         case NumPhysRegisters => Knob("PHYS_REGISTERS")
         case MaxBrCount => Knob("MAX_BR_COUNT")

         // Front-end
         case EnableBTB => true // only gates off updates :(
         case EnableBTBContainsBranches => false // don't send branches to BTB
         case NBTBEntries => if(site(CoreName) == "BOOM") 64 else 62
         case NRAS => 8
         case FetchBufferSz => 4

         // Branch Predictor
         case EnableBranchPredictor => true
         case BranchPredictorSizeInKB => Knob("BPD_SIZE_IN_KB")

         // Pipelining
         case EnableFetchBufferFlowThrough => true
         case EnableCommitMapTable => false        // track the committed rename state; allows
                                                   // for single-cycle resets.

         // Extra Knobs and Features
         case EnableAgePriorityIssue => Knob("AGE_PRIORITY_ISSUE")
         case EnableUarchCounters => true
         case EnablePrefetching => false

      }
   },
   knobValues = {
      case "NTiles" => 1
      case "FETCH_WIDTH" => 2
      case "ISSUE_WIDTH" => 3
      case "ROB_ENTRIES" => 48
      case "ISSUE_ENTRIES" => 20
      case "LSU_ENTRIES" => 16
      case "PHYS_REGISTERS" => 110
      case "MAX_BR_COUNT" => 8
      case "AGE_PRIORITY_ISSUE" => true
      case "BPD_SIZE_IN_KB" => 4
      case "L1D_MSHRS" => 4
      case "L1D_WAYS" => 8
      case "L1D_SETS" => 64
      case "L1I_WAYS" => 8
      case "L1I_SETS" => 64
  }
)

class WithNoBoomCounters extends ChiselConfig (
  (pname,site,here) => pname match {
    case EnableUarchCounters => false
  }
)

class WithSmallBOOMs extends ChiselConfig(
   knobValues = {
      case "FETCH_WIDTH" => 1
      case "ISSUE_WIDTH" => 1
      case "ROB_ENTRIES" => 24
      case "ISSUE_ENTRIES" => 12
      case "LSU_ENTRIES" => 8
      case "PHYS_REGISTERS" => 100
      case "MAX_BR_COUNT" => 4
   }
)

// try to match the Cortex-A9
class WithMediumBOOMs extends ChiselConfig(
   knobValues = {
      case "FETCH_WIDTH" => 2
      case "ISSUE_WIDTH" => 3
      case "ROB_ENTRIES" => 48
      case "ISSUE_ENTRIES" => 20
      case "LSU_ENTRIES" => 16
      case "PHYS_REGISTERS" => 110
      case "MAX_BR_COUNT" => 8
      case "BPD_SIZE_IN_KB" => 4
      case "L1D_MSHRS" => 4
      case "L1D_WAYS" => 8
      case "L1D_SETS" => 64
      case "L1I_WAYS" => 8
      case "L1I_SETS" => 64
   }
)

// try to match the Cortex-A15
class WithMegaBOOMs extends ChiselConfig(
   knobValues = {
      case "FETCH_WIDTH" => 4
      case "ISSUE_WIDTH" => 4
      case "ROB_ENTRIES" => 128
      case "ISSUE_ENTRIES" => 28
      case "LSU_ENTRIES" => 32
      case "PHYS_REGISTERS" => 128
      case "MAX_BR_COUNT" => 8
   }
)
