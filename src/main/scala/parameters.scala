//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
package boom
{

import Chisel._
import rocket._
import tile._
import config.{Parameters, Field}

case object BoomKey extends Field[BoomCoreParams]

case class BoomCoreParams(
   useVM: Boolean = true,
   useUser: Boolean = true,
   useDebug: Boolean = true,
   useAtomics: Boolean = true,
   useCompressed: Boolean = false,
   nBreakpoints: Int = 1,
   nPerfCounters: Int = 4,
   nPerfEvents: Int = 31,
   nCustomMRWCSRs: Int = 0,
   mtvecInit: Option[BigInt] = Some(BigInt(0)),
   mtvecWritable: Boolean = true,
   fastLoadWord: Boolean = true,
   fastLoadByte: Boolean = false,
   fastJAL: Boolean = false,
   mulDiv: Option[MulDivParams] = Some(MulDivParams()),
   fpu: Option[FPUParams] = Some(FPUParams()),
//   decodeWidth: Int = 1,
   dispatchWidth: Int = 1,
   issueWidth: Int = 1,
   numRobEntries: Int = 32,
   numIssueSlotEntries: Int = 12,
   numLsuEntries: Int = 8,
   numPhysRegisters: Int = 110,
   maxBrCount: Int = 4,
   fetchBufferSz: Int = 4,
//   enableBTB: Boolean = true,
   enableBTBContainsBranches: Boolean = true,
   enableBranchPredictor: Boolean = true,
   enableBpdUModeOnly: Boolean = false,
   enableBpdUSModeHistory: Boolean = false,
   enableAgePriorityIssue: Boolean = true,
   enablePrefetching: Boolean = false,
   enableFetchBufferFlowThrough: Boolean = false,
   enableBrResolutionRegister: Boolean = true,
   enableCommitMapTable: Boolean = false
) extends CoreParams {
//) extends RocketCoreParams {
   val fetchWidth: Int = 2 // TODO XXX this is hardcoded -- how should I get this parameterized?
   val decodeWidth: Int = fetchWidth
   val retireWidth: Int = fetchWidth
   val instBits: Int = if (useCompressed) 16 else 32

   require (useCompressed == false)
   require (instBits == 32) 
}


trait HasBoomCoreParameters extends tile.HasCoreParameters
{
//   val boomParams: BoomCoreParams = tileParams.core.asInstanceOf[BoomCoreParams]
   val boomParams: BoomCoreParams = p(BoomKey)
   require(xLen == 64)

   val nPerfCounters = boomParams.nPerfCounters
   val nPerfEvents = boomParams.nPerfEvents
   //************************************
   // Superscalar Widths
   val FETCH_WIDTH      = boomParams.fetchWidth       // number of insts we can fetch
   val DECODE_WIDTH     = boomParams.decodeWidth
   val DISPATCH_WIDTH   = boomParams.dispatchWidth // number of insts put into the IssueWindow
   val ISSUE_WIDTH      = boomParams.issueWidth
   val COMMIT_WIDTH     = boomParams.retireWidth

   require (DECODE_WIDTH == COMMIT_WIDTH)
   require (isPow2(FETCH_WIDTH))
   require (DECODE_WIDTH <= FETCH_WIDTH)

   //************************************
   // Data Structure Sizes
   val NUM_ROB_ENTRIES  = boomParams.numRobEntries     // number of ROB entries (e.g., 32 entries for R10k)
   val NUM_LSU_ENTRIES  = boomParams.numLsuEntries     // number of LD/ST entries
   val MAX_BR_COUNT     = boomParams.maxBrCount        // number of branches we can speculate simultaneously
   val PHYS_REG_COUNT   = boomParams.numPhysRegisters  // size of the unified, physical register file
   val FETCH_BUFFER_SZ  = boomParams.fetchBufferSz     // number of instructions that stored between fetch&decode


   val enableFetchBufferFlowThrough = boomParams.enableFetchBufferFlowThrough 

   //************************************
   // Functional Units
   val usingFDivSqrt = boomParams.fpu.get.divSqrt

   val mulDivParams = boomParams.mulDiv.getOrElse(MulDivParams())

   //************************************
   // Pipelining

   val IMUL_STAGES = boomParams.fpu.get.dfmaLatency
   val dfmaLatency = boomParams.fpu.get.dfmaLatency
   
   val enableBrResolutionRegister = boomParams.enableBrResolutionRegister
    
   //************************************
   // Issue Window
   
   val numIssueSlotEntries = boomParams.numIssueSlotEntries
   val enableAgePriorityIssue = boomParams.enableAgePriorityIssue 
    
   //************************************
   // Load/Store Unit
   val dcacheParams: DCacheParams = tileParams.dcache.get
   val nTLBEntries = dcacheParams.nTLBEntries

   //************************************
   // Branch Prediction

   val enableBTB = tileParams.btb.isDefined
   val btbParams: rocket.BTBParams = tileParams.btb.get

   val enableBTBContainsBranches = boomParams.enableBTBContainsBranches 

   val ENABLE_BRANCH_PREDICTOR = boomParams.enableBranchPredictor
   val ENABLE_BPD_UMODE_ONLY = boomParams.enableBpdUModeOnly
   val ENABLE_BPD_USHISTORY = boomParams.enableBpdUSModeHistory
   // What is the maximum length of global history tracked?
   var GLOBAL_HISTORY_LENGTH = 0
   // What is the physical length of the VeryLongHistoryRegister? This must be
   // able to handle the GHIST_LENGTH as well as being able hold all speculative
   // updates well beyond the GHIST_LENGTH (i.e., +ROB_SZ and other buffering).
   var VLHR_LENGTH = 0
   var BPD_INFO_SIZE = 0
   var ENABLE_VLHR = false

   if (p(TageKey).enabled)
   {
      GLOBAL_HISTORY_LENGTH = p(TageKey).history_lengths.max
      BPD_INFO_SIZE = TageBrPredictor.GetRespInfoSize(p, fetchWidth)
      ENABLE_VLHR = true
   }
   else if (p(GSkewKey).enabled)
   {
      GLOBAL_HISTORY_LENGTH = p(GSkewKey).history_length
      BPD_INFO_SIZE = GSkewBrPredictor.GetRespInfoSize(p, fetchWidth)
   }
   else if (p(GShareKey).enabled)
   {
      GLOBAL_HISTORY_LENGTH = p(GShareKey).history_length
      BPD_INFO_SIZE = GShareBrPredictor.GetRespInfoSize(p)
   }
   else if (p(SimpleGShareKey).enabled)
   {
      GLOBAL_HISTORY_LENGTH = p(SimpleGShareKey).history_length
      BPD_INFO_SIZE = SimpleGShareBrPredictor.GetRespInfoSize(p)
   }
   else if (p(RandomBpdKey).enabled)
   {
      GLOBAL_HISTORY_LENGTH = 1
      BPD_INFO_SIZE = RandomBrPredictor.GetRespInfoSize(p)
   }
   else
   {
      require(!ENABLE_BRANCH_PREDICTOR) // set branch predictor in configs.scala
      BPD_INFO_SIZE = 1
      GLOBAL_HISTORY_LENGTH = 1
   }
   VLHR_LENGTH = GLOBAL_HISTORY_LENGTH+2*NUM_ROB_ENTRIES


   //************************************
   // Extra Knobs and Features
   val ENABLE_REGFILE_BYPASSING  = true  // bypass regfile write ports to read ports
   val MAX_WAKEUP_DELAY = 3              // unused
   val ENABLE_COMMIT_MAP_TABLE = boomParams.enableCommitMapTable

   //************************************
   // Implicitly calculated constants
   val NUM_ROB_ROWS      = NUM_ROB_ENTRIES/DECODE_WIDTH
   val ROB_ADDR_SZ       = log2Up(NUM_ROB_ENTRIES)
   // the f-registers are mapped into the space above the x-registers
   val LOGICAL_REG_COUNT = if (usingFPU) 64 else 32
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

   val corePAddrBits = paddrBits
   val corePgIdxBits = pgIdxBits
}


}
