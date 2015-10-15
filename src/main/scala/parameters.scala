//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
package boom
{

import Chisel._
import Node._
import rocket._

case object DecodeWidth extends Field[Int]
case object DispatchWidth extends Field[Int]
case object IssueWidth extends Field[Int]
case object NumRobEntries extends Field[Int]
case object NumIssueSlotEntries extends Field[Int]
case object NumLsuEntries extends Field[Int]
case object NumPhysRegisters extends Field[Int]
case object MaxBrCount extends Field[Int]
case object FetchBufferSz extends Field[Int]
case object EnableFetchBufferFlowThrough extends Field[Boolean]
case object EnableBTB extends Field[Boolean]
case object EnableBTBContainsBranches extends Field[Boolean]
case object EnableBranchPredictor extends Field[Boolean]
case object BranchPredictorSizeInKB extends Field[Int]
case object EnableAgePriorityIssue extends Field[Boolean]
case object EnableUarchCounters extends Field[Boolean]
case object EnablePrefetching extends Field[Boolean]
case object EnableCommitMapTable extends Field[Boolean]

trait HasBoomCoreParameters extends rocket.HasCoreParameters
{
   require(xLen == 64)

   //************************************
   // Superscalar Widths
   val FETCH_WIDTH      = p(FetchWidth)       // number of insts we can fetch
   val DECODE_WIDTH     = p(DecodeWidth)
   val DISPATCH_WIDTH   = p(DispatchWidth) // number of insts put into the IssueWindow
   val ISSUE_WIDTH      = p(IssueWidth)
   val COMMIT_WIDTH     = p(RetireWidth)

   require (DECODE_WIDTH == COMMIT_WIDTH)
   require (isPow2(FETCH_WIDTH))
   require (DECODE_WIDTH <= FETCH_WIDTH)

   //************************************
   // Data Structure Sizes
   val NUM_ROB_ENTRIES  = p(NumRobEntries)     // number of ROB entries (e.g., 32 entries for R10k)
   val NUM_LSU_ENTRIES  = p(NumLsuEntries)     // number of LD/ST entries
   val MAX_BR_COUNT     = p(MaxBrCount)        // number of branches we can speculate simultaneously
   val PHYS_REG_COUNT   = p(NumPhysRegisters)  // size of the unified, physical register file
   val FETCH_BUFFER_SZ  = p(FetchBufferSz)     // number of instructions that stored between fetch&decode

   //************************************
   // Pipelining

   val IMUL_STAGES = 3 // TODO move this to the parameter configuration file

   //************************************
   // Load/Store Unit

   //************************************
   // Branch Prediction
   val BPD_SIZE_IN_KB = p(BranchPredictorSizeInKB)
   val BPD_NUM_ENTRIES = BPD_SIZE_IN_KB*1024*8/FETCH_WIDTH/2 // computation for GShare
   val ENABLE_BRANCH_PREDICTOR = p(EnableBranchPredictor)
   val GHIST_LENGTH = log2Up(BPD_NUM_ENTRIES)

   //************************************
   // Extra Knobs and Features
   val ENABLE_BRANCH_PREDICTOR = params(EnableBranchPredictor)
   val ENABLE_REGFILE_BYPASSING  = true  // bypass regfile write ports to read ports
   val MAX_WAKEUP_DELAY = 3              // unused

   //************************************
   // Implicitly calculated constants
   val NUM_ROB_ROWS      = NUM_ROB_ENTRIES/DECODE_WIDTH
   val ROB_ADDR_SZ       = log2Up(NUM_ROB_ENTRIES)
   // the f-registers are mapped into the space above the x-registers
   val LOGICAL_REG_COUNT = if (usingFPU) 32 else 64 
   val LREG_SZ           = log2Up(LOGICAL_REG_COUNT)
   val PREG_SZ           = log2Up(PHYS_REG_COUNT)
   val MEM_ADDR_SZ       = log2Up(NUM_LSU_ENTRIES)
   val MAX_ST_COUNT      = (1 << MEM_ADDR_SZ)
   val MAX_LD_COUNT      = (1 << MEM_ADDR_SZ)
   val BR_TAG_SZ         = log2Up(MAX_BR_COUNT)
   val NUM_BROB_ENTRIES  = NUM_ROB_ROWS //TODO explore smaller BROBs
   val BROB_ADDR_SZ      = log2Up(NUM_BROB_ENTRIES)

   require (PHYS_REG_COUNT >= (LOGICAL_REG_COUNT + DECODE_WIDTH))
   require (MAX_BR_COUNT >=2)
   require (NUM_ROB_ROWS % 2 == 0)
   require (NUM_ROB_ENTRIES % DECODE_WIDTH == 0)
   require (isPow2(NUM_LSU_ENTRIES))
   require ((NUM_LSU_ENTRIES-1) > DECODE_WIDTH)

   //************************************
   // Non-BOOM parameters

//   val fastMulDiv = params(FastMulDiv)
   val corePAddrBits = p(uncore.PAddrBits)
   val corePgIdxBits = p(uncore.PgIdxBits)
}


}
