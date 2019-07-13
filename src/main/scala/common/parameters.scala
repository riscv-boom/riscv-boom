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
import freechips.rocketchip.util._
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
    IssueParams(issueWidth=1, numEntries=16, iqType=IQT_MEM.litValue, dispatchWidth=1),
    IssueParams(issueWidth=2, numEntries=16, iqType=IQT_INT.litValue, dispatchWidth=1),
    IssueParams(issueWidth=1, numEntries=16, iqType=IQT_FP.litValue , dispatchWidth=1)),
  numLdqEntries: Int = 16,
  numStqEntries: Int = 16,
  numIntPhysRegisters: Int = 96,
  numFpPhysRegisters: Int = 64,
  enableCustomRf: Boolean = false,
  enableCustomRfModel: Boolean = true,
  maxBrCount: Int = 4,
  numFetchBufferEntries: Int = 16,
  useNewFetchBuffer: Boolean = true,
  enableAgePriorityIssue: Boolean = true,
  enablePrefetching: Boolean = false,
  enableBrResolutionRegister: Boolean = true,
  enableCommitMapTable: Boolean = false,
  enableFastPNR: Boolean = false,
  enableFastWakeupsToRename: Boolean = true,
  enableBTBContainsBranches: Boolean = true,
  enableBranchPredictor: Boolean = true,
  enableBTB: Boolean = true,
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
  fetchLatency: Int = 4,
  renameLatency: Int = 2,
  nPerfCounters: Int = 0,
  numRXQEntries: Int = 4,
  numRCQEntries: Int = 8,
  /* more stuff */

  useFetchMonitor: Boolean = true,
  bootFreqHz: BigInt = 0,
  fpu: Option[FPUParams] = Some(FPUParams()),
  usingFPU: Boolean = true,
  haveBasicCounters: Boolean = true,
  misaWritable: Boolean = true,
  mtvecInit: Option[BigInt] = Some(BigInt(0)),
  mtvecWritable: Boolean = true,
  haveCFlush: Boolean = false,
  mulDiv: Option[freechips.rocketchip.rocket.MulDivParams] = Some(MulDivParams(divEarlyOut=true)),
  nBreakpoints: Int = 1,
  nL2TLBEntries: Int = 512,
  nLocalInterrupts: Int = 0,
  useAtomics: Boolean = true,
  useDebug: Boolean = true,
  useUser: Boolean = true,
  useVM: Boolean = true,
  useCompressed: Boolean = false,
  useSCIE: Boolean = false,
  useRVE: Boolean = false,
  useBPWatch: Boolean = false,
  clockGate: Boolean = false
) extends freechips.rocketchip.tile.CoreParams
{
  val haveFSDirty = false
  val pmpGranularity: Int = 4
  val instBits: Int = if (useCompressed) 16 else 32
  val lrscCycles: Int = 80 // worst case is 14 mispredicted branches + slop
  val retireWidth = decodeWidth
  val jumpInFrontend: Boolean = false // unused in boom
  val nPMPs: Int = 0 // TODO Fix this!!!!!

  override def customCSRs(implicit p: Parameters) = new BoomCustomCSRs
}

/**
  * Defines custom BOOM CSRs
  */
class BoomCustomCSRs(implicit p: Parameters) extends freechips.rocketchip.tile.CustomCSRs
  with HasBoomCoreParameters {
  override def chickenCSR = {
    val params = tileParams.core.asInstanceOf[BoomCoreParams]
    val mask = BigInt(
      tileParams.dcache.get.clockGate.toInt << 0 |
      params.clockGate.toInt << 1 |
      params.clockGate.toInt << 2 |
      1 << 3 // Disable OOO when this bit is high
    )
    val init = BigInt(
      tileParams.dcache.get.clockGate.toInt << 0 |
      params.clockGate.toInt << 1 |
      params.clockGate.toInt << 2 |
      0 << 3 // Enable OOO at init
    )
    Some(CustomCSR(chickenCSRId, mask, Some(init)))
  }
  def disableOOO = getOrElse(chickenCSR, _.value(3), true.B)
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

  // coreWidth is width of decode, width of integer rename, width of ROB, and commit width
  val coreWidth = decodeWidth

  require (isPow2(fetchWidth))
  require (coreWidth <= fetchWidth)

  //************************************
  // Data Structure Sizes
  val numRobEntries = boomParams.numRobEntries       // number of ROB entries (e.g., 32 entries for R10k)
  val numRxqEntries = boomParams.numRXQEntries       // number of RoCC execute queue entries. Keep small since this holds operands and instruction bits
  val numRcqEntries = boomParams.numRCQEntries       // number of RoCC commit queue entries. This can be large since it just keeps a pdst
  val numLdqEntries = boomParams.numLdqEntries       // number of LAQ entries
  val numStqEntries = boomParams.numStqEntries       // number of SAQ/SDQ entries
  val NUM_LDQ_ENTRIES = numLdqEntries // TODO Remove these after
  val NUM_STQ_ENTRIES = numStqEntries // completion of lsu refactor.
  val maxBrCount    = boomParams.maxBrCount          // number of branches we can speculate simultaneously
  val ftqSz         = boomParams.ftq.nEntries        // number of FTQ entries
  val numFetchBufferEntries = boomParams.numFetchBufferEntries // number of instructions that stored between fetch&decode
  val useNewFetchBuffer = boomParams.useNewFetchBuffer

  val numIntPhysRegs= boomParams.numIntPhysRegisters // size of the integer physical register file
  val numFpPhysRegs = boomParams.numFpPhysRegisters  // size of the floating point physical register file

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
  require (Seq(3, 4).contains(fetchLatency)) // 3 and 4 cycle fetch supported
  val renameLatency = boomParams.renameLatency // how many cycles does rename occupy?

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

  issueParams.map(x => require(x.dispatchWidth <= coreWidth && x.dispatchWidth > 0))

  //************************************
  // Load/Store Unit
  val dcacheParams: DCacheParams = tileParams.dcache.get
  val icacheParams: ICacheParams = tileParams.icache.get
  val icBlockBytes = icacheParams.blockBytes

  require(icacheParams.nSets <= 64, "Handling aliases in the ICache is buggy.")

  //************************************
  // Branch Prediction

  val enableBTB = boomParams.enableBTB
  val enableBTBContainsBranches = boomParams.enableBTBContainsBranches

  val enableBranchPredictor = boomParams.enableBranchPredictor

  val enableBpdUmodeOnly = boomParams.enableBpdUModeOnly
  val enableBpdUshistory = boomParams.enableBpdUSModeHistory
  // What is the maximum length of global history tracked?
  var globalHistoryLength = 0
  // What is the physical length of the VeryLongHistoryRegister? This must be
  // able to handle the GHIST_LENGTH as well as being able hold all speculative
  // updates well beyond the GHIST_LENGTH (i.e., +ROB_SZ and other buffering).
  var bpdInfoSize = 0

  val tageBpuParams = boomParams.tage
  val gshareBpuParams = boomParams.gshare
  val baseOnlyBpuParams = boomParams.bpdBaseOnly
  val randomBpuParams = boomParams.bpdRandom

  if (!enableBranchPredictor) {
    bpdInfoSize = 1
    globalHistoryLength = 1
  } else if (baseOnlyBpuParams.isDefined && baseOnlyBpuParams.get.enabled) {
    globalHistoryLength = 8
    bpdInfoSize = BaseOnlyBrPredictor.GetRespInfoSize()
  } else if (gshareBpuParams.isDefined && gshareBpuParams.get.enabled) {
    globalHistoryLength = gshareBpuParams.get.historyLength
    bpdInfoSize = GShareBrPredictor.GetRespInfoSize(globalHistoryLength)
  } else if (tageBpuParams.isDefined && tageBpuParams.get.enabled) {
    globalHistoryLength = tageBpuParams.get.historyLengths.max
    bpdInfoSize = TageBrPredictor.GetRespInfoSize(p)
  } else if (randomBpuParams.isDefined && randomBpuParams.get.enabled) {
    globalHistoryLength = 1
    bpdInfoSize = RandomBrPredictor.GetRespInfoSize()
  }

  //************************************
  // Extra Knobs and Features
  val enableCommitMapTable = boomParams.enableCommitMapTable
  require(!enableCommitMapTable) // TODO Fix the commit map table.
  val enableFastPNR = boomParams.enableFastPNR
  val enableFastWakeupsToRename = boomParams.enableFastWakeupsToRename

  //************************************
  // Implicitly calculated constants
  val numRobRows      = numRobEntries/coreWidth
  val robAddrSz       = log2Ceil(numRobRows) + log2Ceil(coreWidth)
  // the f-registers are mapped into the space above the x-registers
  val logicalRegCount = if (usingFPU) 64 else 32
  val lregSz          = log2Ceil(logicalRegCount)
  val ipregSz         = log2Ceil(numIntPhysRegs)
  val fpregSz         = log2Ceil(numFpPhysRegs)
  val maxPregSz       = ipregSz max fpregSz
  val ldqAddrSz       = log2Ceil(numLdqEntries)
  val stqAddrSz       = log2Ceil(numStqEntries)
  val lsuAddrSz       = ldqAddrSz max stqAddrSz
  val brTagSz         = log2Ceil(maxBrCount)

  require (numIntPhysRegs >= (32 + coreWidth))
  require (numFpPhysRegs >= (32 + coreWidth))
  require (maxBrCount >=2)
  require (numRobEntries % coreWidth == 0)
  require ((numLdqEntries-1) > coreWidth)
  require ((numStqEntries-1) > coreWidth)

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
