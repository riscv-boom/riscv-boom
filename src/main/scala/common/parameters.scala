//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

package boom.common

import chisel3._
import chisel3.util._

import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.config.{Parameters, Field}

import boom.ifu._
import boom.bpu._
import boom.exu._
import boom.lsu._

/**
 * Default BOOM core parameters
 */
case class BoomCoreParams(
   fetchWidth: Int = 1,
   decodeWidth: Int = 1,
   numRobEntries: Int = 64,
   issueParams: Seq[IssueParams] = Seq(
         IssueParams(issueWidth=1, numEntries=16, iqType=IQT_MEM.litValue),
         IssueParams(issueWidth=2, numEntries=16, iqType=IQT_INT.litValue),
         IssueParams(issueWidth=1, numEntries=16, iqType=IQT_FP.litValue)),
   numLdqEntries: Int = 16,
   numStqEntries: Int = 16,
   numIntPhysRegisters: Int = 96,
   numFpPhysRegisters: Int = 64,
   enableCustomRf: Boolean = false,
   enableCustomRfModel: Boolean = true,
   maxBrCount: Int = 4,
   fetchBufferSz: Int = 8,
   enableAgePriorityIssue: Boolean = true,
   enablePrefetching: Boolean = false,
   enableBrResolutionRegister: Boolean = true,
   enableCommitMapTable: Boolean = false,
   enableBTBContainsBranches: Boolean = true,
   enableBranchPredictor: Boolean = true,
   enableBpdUModeOnly: Boolean = false,
   enableBpdUSModeHistory: Boolean = false,
   useAtomicsOnlyForIO: Boolean = false,
   ftq: FtqParameters = FtqParameters(),
   btb: BoomBTBParameters = BoomBTBParameters(),
   bim: BimParameters = BimParameters(),
   tage: Option[TageParameters] = None,
   gshare: Option[GShareParameters] = None,
   bpdBaseOnly: Option[BaseOnlyParameters] = None,
   bpdRandom: Option[RandomBpdParameters] = None,
   intToFpLatency: Int = 2,
   imulLatency: Int = 3,
   fetchLatency: Int = 3,
   renameLatency: Int = 2,
   regreadLatency: Int = 1,
   nPerfCounters: Int = 0,
   /* more stuff */

   useFetchMonitor: Boolean = true,
   bootFreqHz: BigInt = 0,
   fpu: Option[FPUParams] = Some(FPUParams()),
   usingFPU: Boolean = true,
   haveBasicCounters: Boolean = true,
   misaWritable: Boolean = true,
   mtvecInit: Option[BigInt] = Some(BigInt(0)),
   mtvecWritable: Boolean = true,
   mulDiv: Option[freechips.rocketchip.rocket.MulDivParams] = Some(MulDivParams()),
   nBreakpoints: Int = 1,
   nL2TLBEntries: Int = 512,
   nLocalInterrupts: Int = 0,
   tileControlAddr: Option[BigInt] = None,
   useAtomics: Boolean = true,
   useDebug: Boolean = true,
   useUser: Boolean = true,
   useVM: Boolean = true,
   useCompressed: Boolean = false
) extends freechips.rocketchip.tile.CoreParams
{
   val retireWidth: Int = decodeWidth
   val haveFSDirty = false
   val pmpGranularity: Int = 4
   val instBits: Int = if (useCompressed) 16 else 32
   val lrscCycles: Int = 80 // worst case is 14 mispredicted branches + slop

   val jumpInFrontend: Boolean = false // unused in boom
   val nPMPs: Int = 8
}

/**
 * Mixin trait to add BOOM parameters to expand other traits/objects/etc
 */
trait HasBoomCoreParameters extends freechips.rocketchip.tile.HasCoreParameters
{
   val boomParams: BoomCoreParams = tileParams.core.asInstanceOf[BoomCoreParams]

   //************************************
   // Superscalar Widths

   // fetchWidth provided by CoreParams class.
   // decodeWidth provided by CoreParams class.
   // retireWidth provided by BoomCoreParams class.
   val DISPATCH_WIDTH   = decodeWidth                // number of insts put into the IssueWindow
   val COMMIT_WIDTH     = boomParams.retireWidth

   require (decodeWidth == COMMIT_WIDTH)
   require (DISPATCH_WIDTH == COMMIT_WIDTH)
   require (isPow2(fetchWidth))
   require (decodeWidth <= fetchWidth)

   //************************************
   // Data Structure Sizes
   val NUM_ROB_ENTRIES = boomParams.numRobEntries       // number of ROB entries (e.g., 32 entries for R10k)
   val NUM_LDQ_ENTRIES = boomParams.numLdqEntries       // number of LAQ entries
   val NUM_STQ_ENTRIES = boomParams.numStqEntries       // number of SAQ/SDQ entries
   val MAX_BR_COUNT    = boomParams.maxBrCount          // number of branches we can speculate simultaneously
   val ftqSz           = NUM_ROB_ENTRIES / fetchWidth   // number of FTQ entries should match (or slightly exceed) ROB entries
   val fetchBufferSz   = boomParams.fetchBufferSz       // number of instructions that stored between fetch&decode

   val numIntPhysRegs  = boomParams.numIntPhysRegisters // size of the integer physical register file
   val numFpPhysRegs   = boomParams.numFpPhysRegisters  // size of the floating point physical register file

   //************************************
   // Functional Units
   val usingFDivSqrt = boomParams.fpu.isDefined && boomParams.fpu.get.divSqrt

   val mulDivParams = boomParams.mulDiv.getOrElse(MulDivParams())
   // TODO: Allow RV32IF
   require(!(xLen == 32 && usingFPU), "RV32 does not support fp")

   //************************************
   // Pipelining

   val imulLatency = boomParams.imulLatency
   val dfmaLatency = if (boomParams.fpu.isDefined) boomParams.fpu.get.dfmaLatency else 3
   val sfmaLatency = if (boomParams.fpu.isDefined) boomParams.fpu.get.sfmaLatency else 3
   // All FPU ops padded out to same delay for writeport scheduling.
   require (sfmaLatency == dfmaLatency)

   val intToFpLatency = boomParams.intToFpLatency

   val fetchLatency = boomParams.fetchLatency // how many cycles does fetch occupy?
   require (fetchLatency == 3) // do not currently support changing this
   val renameLatency = boomParams.renameLatency // how many cycles does rename occupy?
   val regreadLatency = boomParams.regreadLatency // how many cycles does rrd occupy?
   require (regreadLatency == 1) // Any color you like, so long as its black.

   val enableBrResolutionRegister = boomParams.enableBrResolutionRegister

   //************************************
   // Issue Units

   val issueParams: Seq[IssueParams] = boomParams.issueParams
   val enableAgePriorityIssue = boomParams.enableAgePriorityIssue
   val usingUnifiedMemIntIQs = issueParams.count(_.iqType == IQT_MEM.litValue) == 0

   // currently, only support one of each.
   require (issueParams.count(_.iqType == IQT_FP.litValue) == 1 || !usingFPU)
   require (issueParams.count(_.iqType == IQT_MEM.litValue) == 1 || usingUnifiedMemIntIQs)
   require (issueParams.count(_.iqType == IQT_INT.litValue) == 1)

   //************************************
   // Load/Store Unit
   val dcacheParams: DCacheParams = tileParams.dcache.get
   val icacheParams: ICacheParams = tileParams.icache.get
   val icBlockBytes = icacheParams.blockBytes

   require(icacheParams.nSets <= 64, "Handling aliases in the ICache is buggy.")

   //************************************
   // Branch Prediction

   val enableBTB = true
   val enableBTBContainsBranches = boomParams.enableBTBContainsBranches

   val ENABLE_BRANCH_PREDICTOR = boomParams.enableBranchPredictor

   val ENABLE_BPD_UMODE_ONLY = boomParams.enableBpdUModeOnly
   val ENABLE_BPD_USHISTORY = boomParams.enableBpdUSModeHistory
   // What is the maximum length of global history tracked?
   var GLOBAL_HISTORY_LENGTH = 0
   // What is the physical length of the VeryLongHistoryRegister? This must be
   // able to handle the GHIST_LENGTH as well as being able hold all speculative
   // updates well beyond the GHIST_LENGTH (i.e., +ROB_SZ and other buffering).
   var BPD_INFO_SIZE = 0

   val tageBpuParams = boomParams.tage
   val gshareBpuParams = boomParams.gshare
   val baseOnlyBpuParams = boomParams.bpdBaseOnly
   val randomBpuParams = boomParams.bpdRandom

   if (!ENABLE_BRANCH_PREDICTOR)
   {
      BPD_INFO_SIZE = 1
      GLOBAL_HISTORY_LENGTH = 1
   }
   else if (baseOnlyBpuParams.isDefined && baseOnlyBpuParams.get.enabled)
   {
      GLOBAL_HISTORY_LENGTH = 8
      BPD_INFO_SIZE = BaseOnlyBrPredictor.GetRespInfoSize(p, GLOBAL_HISTORY_LENGTH)
   }
   else if (gshareBpuParams.isDefined && gshareBpuParams.get.enabled)
   {
      GLOBAL_HISTORY_LENGTH = gshareBpuParams.get.history_length
      BPD_INFO_SIZE = GShareBrPredictor.GetRespInfoSize(fetchWidth, GLOBAL_HISTORY_LENGTH)
   }
   else if (tageBpuParams.isDefined && tageBpuParams.get.enabled)
   {
      GLOBAL_HISTORY_LENGTH = tageBpuParams.get.history_lengths.max
      BPD_INFO_SIZE = TageBrPredictor.GetRespInfoSize(p, fetchWidth)
   }
   else if (randomBpuParams.isDefined && randomBpuParams.get.enabled)
   {
      GLOBAL_HISTORY_LENGTH = 1
      BPD_INFO_SIZE = RandomBrPredictor.GetRespInfoSize(p)
   }

   //************************************
   // Extra Knobs and Features
   val ENABLE_COMMIT_MAP_TABLE = boomParams.enableCommitMapTable

   //************************************
   // Implicitly calculated constants
   val NUM_ROB_ROWS      = NUM_ROB_ENTRIES/decodeWidth
   val ROB_ADDR_SZ       = log2Ceil(NUM_ROB_ENTRIES)
   // the f-registers are mapped into the space above the x-registers
   val LOGICAL_REG_COUNT = if (usingFPU) 64 else 32
   val LREG_SZ           = log2Ceil(LOGICAL_REG_COUNT)
   val IPREG_SZ          = log2Ceil(numIntPhysRegs)
   val FPREG_SZ          = log2Ceil(numFpPhysRegs)
   val PREG_SZ           = IPREG_SZ max FPREG_SZ
   val LDQ_ADDR_SZ       = log2Ceil(NUM_LDQ_ENTRIES)
   val STQ_ADDR_SZ       = log2Ceil(NUM_STQ_ENTRIES)
   val LSU_ADDR_SZ       = LDQ_ADDR_SZ max STQ_ADDR_SZ
   val BR_TAG_SZ         = log2Ceil(MAX_BR_COUNT)
   val NUM_BROB_ENTRIES  = NUM_ROB_ROWS //TODO explore smaller BROBs
   val BROB_ADDR_SZ      = log2Ceil(NUM_BROB_ENTRIES)

   require (numIntPhysRegs >= (32 + decodeWidth))
   require (numFpPhysRegs >= (32 + decodeWidth))
   require (MAX_BR_COUNT >=2)
   require (NUM_ROB_ROWS % 2 == 0)
   require (NUM_ROB_ENTRIES % decodeWidth == 0)
   require ((NUM_LDQ_ENTRIES-1) > decodeWidth)
   require ((NUM_STQ_ENTRIES-1) > decodeWidth)

   //************************************
   // Custom Logic
   val enableCustomRf      = boomParams.enableCustomRf
   val enableCustomRfModel = boomParams.enableCustomRfModel

   //************************************
   // Other Non/Should-not-be sythesizable modules
   val useFetchMonitor = boomParams.useFetchMonitor

   //************************************
   // Non-BOOM parameters

   val corePAddrBits = paddrBits
   val corePgIdxBits = pgIdxBits
}
