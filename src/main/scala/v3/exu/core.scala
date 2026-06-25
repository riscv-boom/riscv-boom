//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISC-V Processor Core
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// BOOM has the following (conceptual) stages:
//   if0 - Instruction Fetch 0 (next-pc select)
//   if1 - Instruction Fetch 1 (I$ access)
//   if2 - Instruction Fetch 2 (instruction return)
//   if3 - Instruction Fetch 3 (enqueue to fetch buffer)
//   if4 - Instruction Fetch 4 (redirect from bpd)
//   dec - Decode
//   ren - Rename1
//   dis - Rename2/Dispatch
//   iss - Issue
//   rrd - Register Read
//   exe - Execute
//   mem - Memory
//   sxt - Sign-extend
//   wb  - Writeback
//   com - Commit

package boom.v3.exu

import java.nio.file.{Paths}

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.tile.{TraceBundle}
import freechips.rocketchip.rocket.{Causes, PRV, TracedInstruction}
import freechips.rocketchip.util.{Str, UIntIsOneOf, CoreMonitorBundle}
import freechips.rocketchip.devices.tilelink.{PLICConsts, CLINTConsts}

import boom.v3.common._
import boom.v3.ifu.{GlobalHistory, HasBoomFrontendParameters}
import boom.v3.exu.FUConstants._
import boom.v3.util._

/**
 * Top level core object that connects the Frontend to the rest of the pipeline.
 */
class BoomCore()(implicit p: Parameters) extends BoomModule
  with HasBoomFrontendParameters // TODO: Don't add this trait
{
  val io = IO(new Bundle {
    val hartid = Input(UInt(hartIdLen.W))
    val interrupts = Input(new freechips.rocketchip.rocket.CoreInterrupts(false))
    val ifu = new boom.v3.ifu.BoomFrontendIO
    val ptw = Flipped(new freechips.rocketchip.rocket.DatapathPTWIO())
    val rocc = Flipped(new freechips.rocketchip.tile.RoCCCoreIO())
    val lsu = Flipped(new boom.v3.lsu.LSUCoreIO)
    val ptw_tlb = new freechips.rocketchip.rocket.TLBPTWIO()
    val trace = Output(new TraceBundle)
    val fcsr_rm = UInt(freechips.rocketchip.tile.FPConstants.RM_SZ.W)
    val tma_counters = if (boomParams.enableTMACounters)
      Some(Output(Vec(BoomPerfCounterConsts.NUM_COUNTERS, UInt(xLen.W)))) else None
  })

  io.ptw_tlb := DontCare
  io.ptw := DontCare
  io.ifu := DontCare

  //**********************************
  // construct all of the modules

  // Only holds integer-registerfile execution units.
  val exe_units = new boom.v3.exu.ExecutionUnits(fpu=false)
  val jmp_unit_idx = exe_units.jmp_unit_idx
  val jmp_unit = exe_units(jmp_unit_idx)

  // Meanwhile, the FP pipeline holds the FP issue window, FP regfile, and FP arithmetic units.
  var fp_pipeline: FpPipeline = null
  if (usingFPU) fp_pipeline = Module(new FpPipeline)

  // ********************************************************
  // Clear fp_pipeline before use
  if (usingFPU) {
    fp_pipeline.io.ll_wports := DontCare
    fp_pipeline.io.wb_valids := DontCare
    fp_pipeline.io.wb_pdsts  := DontCare
  }

  val numIrfWritePorts        = exe_units.numIrfWritePorts + memWidth
  val numLlIrfWritePorts      = exe_units.numLlIrfWritePorts
  val numIrfReadPorts         = exe_units.numIrfReadPorts

  val numFastWakeupPorts      = exe_units.count(_.bypassable)
  val numAlwaysBypassable     = exe_units.count(_.alwaysBypassable)

  val numIntIssueWakeupPorts  = numIrfWritePorts + numFastWakeupPorts - numAlwaysBypassable // + memWidth for ll_wb
  val numIntRenameWakeupPorts = numIntIssueWakeupPorts
  val numFpWakeupPorts        = if (usingFPU) fp_pipeline.io.wakeups.length else 0

  val decode_units     = for (w <- 0 until decodeWidth) yield { val d = Module(new DecodeUnit); d }
  val dec_brmask_logic = Module(new BranchMaskGenerationLogic(coreWidth))
  val rename_stage     = Module(new RenameStage(coreWidth, numIntPhysRegs, numIntRenameWakeupPorts, false))
  val fp_rename_stage  = if (usingFPU) Module(new RenameStage(coreWidth, numFpPhysRegs, numFpWakeupPorts, true)) else null
  val pred_rename_stage = Module(new PredRenameStage(coreWidth, ftqSz, 1))
  val rename_stages    = if (usingFPU) Seq(rename_stage, fp_rename_stage, pred_rename_stage) else Seq(rename_stage, pred_rename_stage)

  val mem_iss_unit     = Module(new IssueUnitCollapsing(memIssueParam, numIntIssueWakeupPorts))
  mem_iss_unit.suggestName("mem_issue_unit")
  val int_iss_unit     = Module(new IssueUnitCollapsing(intIssueParam, numIntIssueWakeupPorts))
  int_iss_unit.suggestName("int_issue_unit")

  val issue_units      = Seq(mem_iss_unit, int_iss_unit)
  val dispatcher       = Module(new BasicDispatcher)

  val iregfile         = Module(new RegisterFileSynthesizable(
                             numIntPhysRegs,
                             numIrfReadPorts,
                             numIrfWritePorts,
                             xLen,
                             Seq.fill(memWidth) {true} ++ exe_units.bypassable_write_port_mask)) // bypassable ll_wb
  val pregfile         = Module(new RegisterFileSynthesizable(
                            ftqSz,
                            exe_units.numIrfReaders,
                            1,
                            1,
                            Seq(true))) // The jmp unit is always bypassable
  pregfile.io := DontCare // Only use the IO if enableSFBOpt

  // wb arbiter for the 0th ll writeback
  // TODO: should this be a multi-arb?
  val ll_wbarb         = Module(new Arbiter(new ExeUnitResp(xLen), 1 +
                                                                   (if (usingFPU) 1 else 0) +
                                                                   (if (usingRoCC) 1 else 0)))
  val iregister_read   = Module(new RegisterRead(
                           issue_units.map(_.issueWidth).sum,
                           exe_units.withFilter(_.readsIrf).map(_.supportedFuncUnits).toSeq,
                           numIrfReadPorts,
                           exe_units.withFilter(_.readsIrf).map(x => 2).toSeq,
                           exe_units.numTotalBypassPorts,
                           jmp_unit.numBypassStages,
                           xLen))
  val rob              = Module(new Rob(
                           numIrfWritePorts + numFpWakeupPorts, // +memWidth for ll writebacks
                           numFpWakeupPorts))
  // Used to wakeup registers in rename and issue. ROB needs to listen to something else.
  val int_iss_wakeups  = Wire(Vec(numIntIssueWakeupPorts, Valid(new ExeUnitResp(xLen))))
  val int_ren_wakeups  = Wire(Vec(numIntRenameWakeupPorts, Valid(new ExeUnitResp(xLen))))
  val pred_wakeup  = Wire(Valid(new ExeUnitResp(1)))

  require (exe_units.length == issue_units.map(_.issueWidth).sum)

  //***********************************
  // Pipeline State Registers and Wires

  // Decode/Rename1 Stage
  val dec_valids = Wire(Vec(coreWidth, Bool()))  // are the decoded instruction valid? It may be held up though.
  val dec_uops   = Wire(Vec(coreWidth, new MicroOp()))
  val dec_fire   = Wire(Vec(coreWidth, Bool()))  // can the instruction fire beyond decode?
                                                    // (can still be stopped in ren or dis)
  val dec_ready  = Wire(Bool())
  val dec_xcpts  = Wire(Vec(coreWidth, Bool()))
  val ren_stalls = Wire(Vec(coreWidth, Bool()))
  val branch_mask_full = Wire(Vec(coreWidth, Bool()))
  val dec_finished_mask = RegInit(0.U(coreWidth.W))
  val dec_unfinished_frontend_fault = RegInit(false.B)

  // Rename2/Dispatch stage
  val dis_valids = Wire(Vec(coreWidth, Bool()))
  val dis_uops   = Wire(Vec(coreWidth, new MicroOp))
  val dis_fire   = Wire(Vec(coreWidth, Bool()))
  val dis_ready  = Wire(Bool())

  // Issue Stage/Register Read
  val iss_valids = Wire(Vec(exe_units.numIrfReaders, Bool()))
  val iss_uops   = Wire(Vec(exe_units.numIrfReaders, new MicroOp()))
  val bypasses   = Wire(Vec(exe_units.numTotalBypassPorts, Valid(new ExeUnitResp(xLen))))
  val pred_bypasses = Wire(Vec(jmp_unit.numBypassStages, Valid(new ExeUnitResp(1))))
  require(jmp_unit.bypassable)

  // --------------------------------------
  // Dealing with branch resolutions

  // The individual branch resolutions from each ALU
  val brinfos = Reg(Vec(coreWidth, new BrResolutionInfo()))

  // "Merged" branch update info from all ALUs
  // brmask contains masks for rapidly clearing mispredicted instructions
  // brindices contains indices to reset pointers for allocated structures
  //           brindices is delayed a cycle
  val brupdate  = Wire(new BrUpdateInfo)
  val b1    = Wire(new BrUpdateMasks)
  val b2    = Reg(new BrResolutionInfo)

  brupdate.b1 := b1
  brupdate.b2 := b2

  for ((b, a) <- brinfos zip exe_units.alu_units) {
    b := a.io.brinfo
    b.valid := a.io.brinfo.valid && !rob.io.flush.valid
  }
  b1.resolve_mask := brinfos.map(x => x.valid << x.uop.br_tag).reduce(_|_)
  b1.mispredict_mask := brinfos.map(x => (x.valid && x.mispredict) << x.uop.br_tag).reduce(_|_)

  // Find the oldest mispredict and use it to update indices
  var mispredict_val = false.B
  var oldest_mispredict = brinfos(0)
  for (b <- brinfos) {
    val use_this_mispredict = !mispredict_val ||
    b.valid && b.mispredict && IsOlder(b.uop.rob_idx, oldest_mispredict.uop.rob_idx, rob.io.rob_head_idx)

    mispredict_val = mispredict_val || (b.valid && b.mispredict)
    oldest_mispredict = Mux(use_this_mispredict, b, oldest_mispredict)
  }

  b2.mispredict  := mispredict_val
  b2.cfi_type    := oldest_mispredict.cfi_type
  b2.taken       := oldest_mispredict.taken
  b2.pc_sel      := oldest_mispredict.pc_sel
  b2.uop         := UpdateBrMask(brupdate, oldest_mispredict.uop)
  b2.jalr_target := RegNext(jmp_unit.io.brinfo.jalr_target)
  b2.target_offset := oldest_mispredict.target_offset

  val oldest_mispredict_ftq_idx = oldest_mispredict.uop.ftq_idx


  assert (!((brupdate.b1.mispredict_mask =/= 0.U || brupdate.b2.mispredict)
    && rob.io.commit.rollback), "Can't have a mispredict during rollback.")

  io.ifu.brupdate := brupdate

  for (eu <- exe_units) {
    eu.io.brupdate := brupdate
  }

  if (usingFPU) {
    fp_pipeline.io.brupdate := brupdate
  }

  // Load/Store Unit & ExeUnits
  val mem_units = exe_units.memory_units
  val mem_resps = mem_units.map(_.io.ll_iresp)
  for (i <- 0 until memWidth) {
    mem_units(i).io.lsu_io <> io.lsu.exe(i)
  }

  //-------------------------------------------------------------
  // Uarch Hardware Performance Events (HPEs)

  // Helper: is the pipeline in a recovery/flush state where no useful work happens?
  // Note: io.ifu.redirect_flush is intentionally excluded — it stays high for hundreds
  // of cycles after a mispredict (via b1.mispredict_mask and flush_frontend). Post-redirect
  // frontend stalls (icache refill, pipeline refill) should be classified as frontend_bound,
  // not lumped into bad_speculation.
  val tma_in_recovery = rob.io.commit.rollback ||
                        brupdate.b2.mispredict ||
                        rob.io.flush.valid ||
                        RegNext(rob.io.flush.valid) ||
                        RegNext(RegNext(rob.io.flush.valid))

  // Detect branch misprediction vs other machine clears for TMA L2
  val tma_is_branch_mispredict_recovery = brupdate.b2.mispredict
  val tma_is_machine_clear = tma_in_recovery && !brupdate.b2.mispredict

  // Detect fetch buffer delivering valid uops to decode
  val tma_fetch_valid = io.ifu.fetchpacket.valid

  // Detect memory-related backend stalls (any slot blocked by LSU)
  val tma_memory_stall = (0 until coreWidth).map(w =>
    dis_valids(w) && (
      (io.lsu.ldq_full(w) && dis_uops(w).uses_ldq) ||
      (io.lsu.stq_full(w) && dis_uops(w).uses_stq)
    )).reduce(_||_)

  // Instruction mix signals at retire
  // Note: ctrl.is_load/is_sta/is_std are set during register-read (after ROB enqueue),
  // so they are NOT valid at commit time. Use uses_ldq/uses_stq instead.
  val tma_retired_loads = PopCount(VecInit((0 until coreWidth) map { w =>
    rob.io.commit.arch_valids(w) && rob.io.commit.uops(w).uses_ldq }))
  val tma_retired_stores = PopCount(VecInit((0 until coreWidth) map { w =>
    rob.io.commit.arch_valids(w) && rob.io.commit.uops(w).uses_stq }))
  val tma_retired_branches = PopCount(VecInit((0 until coreWidth) map { w =>
    rob.io.commit.arch_valids(w) && rob.io.commit.uops(w).is_br }))
  val tma_retired_jals = PopCount(VecInit((0 until coreWidth) map { w =>
    rob.io.commit.arch_valids(w) && rob.io.commit.uops(w).is_jal }))
  val tma_retired_jalrs = PopCount(VecInit((0 until coreWidth) map { w =>
    rob.io.commit.arch_valids(w) && rob.io.commit.uops(w).is_jalr }))
  val tma_retired_fp = PopCount(VecInit((0 until coreWidth) map { w =>
    rob.io.commit.arch_valids(w) && rob.io.commit.uops(w).fp_val }))
  val tma_retired_amo = PopCount(VecInit((0 until coreWidth) map { w =>
    rob.io.commit.arch_valids(w) && rob.io.commit.uops(w).is_amo }))
  val tma_retired_system = PopCount(VecInit((0 until coreWidth) map { w =>
    rob.io.commit.arch_valids(w) && rob.io.commit.uops(w).is_unique }))

  // Stall reason signals
  val tma_rob_full       = !rob.io.ready
  val tma_ldq_full       = io.lsu.ldq_full.reduce(_||_)
  val tma_stq_full       = io.lsu.stq_full.reduce(_||_)
  val tma_int_iq_full    = !int_iss_unit.io.dis_uops(0).ready
  val tma_mem_iq_full    = !mem_iss_unit.io.dis_uops(0).ready
  val tma_branch_mask_full_any = branch_mask_full.reduce(_||_)
  val tma_ren_stall_any  = ren_stalls.reduce(_||_)

  // Branch predictor source tracking at retire
  val tma_br_correct_bpd = PopCount(VecInit((0 until coreWidth) map { w =>
    rob.io.commit.arch_valids(w) && rob.io.commit.uops(w).is_br &&
    !rob.io.commit.uops(w).taken && // not mispredicted (if taken was set correctly at commit)
    (rob.io.commit.uops(w).debug_fsrc === BSRC_2) }))
  val tma_br_correct_btb = PopCount(VecInit((0 until coreWidth) map { w =>
    rob.io.commit.arch_valids(w) && rob.io.commit.uops(w).is_br &&
    (rob.io.commit.uops(w).debug_fsrc === BSRC_1) }))

  val perfEvents = new freechips.rocketchip.rocket.EventSets(Seq(
    // EventSet 0: Exceptions and basic events
    new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
      ("exception",                         () => rob.io.com_xcpt.valid),
      ("flush",                             () => rob.io.flush.valid),
      ("branch resolved",                   () => b2.valid),
      ("nop",                               () => false.B))),

    // EventSet 1: Branch prediction events
    new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
      ("branch misprediction",              () => b2.mispredict),
      ("control-flow target misprediction", () => b2.mispredict && b2.cfi_type === CFI_JALR),
      ("branch mispredict from BPD",        () => b2.mispredict && b2.uop.debug_fsrc === BSRC_2),
      ("branch mispredict from BTB",        () => b2.mispredict && b2.uop.debug_fsrc === BSRC_1))),

    // EventSet 2: Cache and TLB events
    new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
      ("I$ miss",     () => io.ifu.perf.acquire),
      ("D$ miss",     () => io.lsu.perf.acquire),
      ("D$ release",  () => io.lsu.perf.release),
      ("ITLB miss",   () => io.ifu.perf.tlbMiss),
      ("DTLB miss",   () => io.lsu.perf.tlbMiss),
      ("L2 TLB miss", () => io.ptw.perf.l2miss))),

    // EventSet 3: Instruction mix at retire
    new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
      ("retired loads",    () => tma_retired_loads > 0.U),
      ("retired stores",   () => tma_retired_stores > 0.U),
      ("retired branches", () => tma_retired_branches > 0.U),
      ("retired jals",     () => tma_retired_jals > 0.U),
      ("retired jalrs",    () => tma_retired_jalrs > 0.U),
      ("retired fp",       () => tma_retired_fp > 0.U),
      ("retired amo",      () => tma_retired_amo > 0.U),
      ("retired system",   () => tma_retired_system > 0.U))),

    // EventSet 4: Backend stall reasons (cycle-level)
    new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
      ("ROB full",              () => tma_rob_full),
      ("LDQ full",              () => tma_ldq_full),
      ("STQ full",              () => tma_stq_full),
      ("int IQ full",           () => tma_int_iq_full),
      ("mem IQ full",           () => tma_mem_iq_full),
      ("branch mask full",      () => tma_branch_mask_full_any),
      ("rename stall",          () => tma_ren_stall_any),
      ("pipeline flush",        () => rob.io.flush.valid))),

    // EventSet 5: Frontend events (cycle-level)
    new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
      ("fetch buffer empty",    () => !tma_fetch_valid && !tma_in_recovery),
      ("rollback cycles",       () => rob.io.commit.rollback),
      ("recovery cycles",       () => tma_in_recovery),
      ("dispatch ready",        () => dis_ready)))))

  val csr = Module(new freechips.rocketchip.rocket.CSRFile(perfEvents, boomParams.customCSRs.decls))
  csr.io.inst foreach { c => c := DontCare }
  csr.io.rocc_interrupt := io.rocc.interrupt
  csr.io.mhtinst_read_pseudo := false.B

  val custom_csrs = Wire(new BoomCustomCSRs)
  custom_csrs.csrs.foreach { c => c.stall := false.B; c.set := false.B; c.sdata := DontCare }

  (custom_csrs.csrs zip csr.io.customCSRs).map { case (lhs, rhs) => lhs <> rhs }

  csr.io.counters foreach { c => c.inc := RegNext(perfEvents.evaluate(c.eventSel)) }

  //****************************************
  // Time Stamp Counter & Retired Instruction Counter
  // (only used for printf and vcd dumps - the actual counters are in the CSRFile)
  val debug_tsc_reg = RegInit(0.U(xLen.W))
  val debug_irt_reg = RegInit(0.U(xLen.W))
  val debug_brs     = Reg(Vec(4, UInt(xLen.W)))
  val debug_jals    = Reg(Vec(4, UInt(xLen.W)))
  val debug_jalrs   = Reg(Vec(4, UInt(xLen.W)))

  for (j <- 0 until 4) {
    debug_brs(j) := debug_brs(j) + PopCount(VecInit((0 until coreWidth) map {i =>
      rob.io.commit.arch_valids(i) &&
      (rob.io.commit.uops(i).debug_fsrc === j.U) &&
      rob.io.commit.uops(i).is_br
    }))
    debug_jals(j) := debug_jals(j) + PopCount(VecInit((0 until coreWidth) map {i =>
      rob.io.commit.arch_valids(i) &&
      (rob.io.commit.uops(i).debug_fsrc === j.U) &&
      rob.io.commit.uops(i).is_jal
    }))
    debug_jalrs(j) := debug_jalrs(j) + PopCount(VecInit((0 until coreWidth) map {i =>
      rob.io.commit.arch_valids(i) &&
      (rob.io.commit.uops(i).debug_fsrc === j.U) &&
      rob.io.commit.uops(i).is_jalr
    }))
  }

  dontTouch(debug_brs)
  dontTouch(debug_jals)
  dontTouch(debug_jalrs)

  debug_tsc_reg := debug_tsc_reg + 1.U
  debug_irt_reg := debug_irt_reg + PopCount(rob.io.commit.arch_valids.asUInt)
  dontTouch(debug_tsc_reg)
  dontTouch(debug_irt_reg)

  //****************************************
  // Print-out information about the machine

  val issStr =
    if (enableAgePriorityIssue) " (Age-based Priority)"
    else " (Unordered Priority)"

  // val btbStr =
  //   if (enableBTB) ("" + boomParams.btb.nSets * boomParams.btb.nWays + " entries (" + boomParams.btb.nSets + " x " + boomParams.btb.nWays + " ways)")
  //   else 0
  val btbStr = ""

  val fpPipelineStr =
    if (usingFPU) fp_pipeline.toString
    else ""

  override def toString: String =
    (BoomCoreStringPrefix("====Overall Core Params====") + "\n"
    + exe_units.toString + "\n"
    + fpPipelineStr + "\n"
    + rob.toString + "\n"
    + BoomCoreStringPrefix(
        "===Other Core Params===",
        "Fetch Width           : " + fetchWidth,
        "Decode Width          : " + coreWidth,
        "Issue Width           : " + issueParams.map(_.issueWidth).sum,
        "ROB Size              : " + numRobEntries,
        "Issue Window Size     : " + issueParams.map(_.numEntries) + issStr,
        "Load/Store Unit Size  : " + numLdqEntries + "/" + numStqEntries,
        "Num Int Phys Registers: " + numIntPhysRegs,
        "Num FP  Phys Registers: " + numFpPhysRegs,
        "Max Branch Count      : " + maxBrCount)
    + iregfile.toString + "\n"
    + BoomCoreStringPrefix(
        "Num Slow Wakeup Ports : " + numIrfWritePorts,
        "Num Fast Wakeup Ports : " + exe_units.count(_.bypassable),
        "Num Bypass Ports      : " + exe_units.numTotalBypassPorts) + "\n"
    + BoomCoreStringPrefix(
        "DCache Ways           : " + dcacheParams.nWays,
        "DCache Sets           : " + dcacheParams.nSets,
        "DCache nMSHRs         : " + dcacheParams.nMSHRs,
        "ICache Ways           : " + icacheParams.nWays,
        "ICache Sets           : " + icacheParams.nSets,
        "D-TLB Ways            : " + dcacheParams.nTLBWays,
        "I-TLB Ways            : " + icacheParams.nTLBWays,
        "Paddr Bits            : " + paddrBits,
        "Vaddr Bits            : " + vaddrBits) + "\n"
    + BoomCoreStringPrefix(
        "Using FPU Unit?       : " + usingFPU.toString,
        "Using FDivSqrt?       : " + usingFDivSqrt.toString,
        "Using VM?             : " + usingVM.toString) + "\n")

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Fetch Stage/Frontend ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  io.ifu.redirect_val         := false.B
  io.ifu.redirect_flush       := false.B

  // Breakpoint info
  io.ifu.status  := csr.io.status
  io.ifu.bp      := csr.io.bp
  io.ifu.mcontext := csr.io.mcontext
  io.ifu.scontext := csr.io.scontext

  io.ifu.flush_icache := (0 until coreWidth).map { i =>
    (rob.io.commit.arch_valids(i) && rob.io.commit.uops(i).is_fencei) ||
    (RegNext(dec_valids(i) && dec_uops(i).is_jalr && csr.io.status.debug))
  }.reduce(_||_)

  // TODO FIX THIS HACK
  // The below code works because of two quirks with the flush mechanism
  //  1 ) All flush_on_commit instructions are also is_unique,
  //      In the future, this constraint will be relaxed.
  //  2 ) We send out flush signals one cycle after the commit signal. We need to
  //      mux between one/two cycle delay for the following cases:
  //       ERETs are reported to the CSR two cycles before we send the flush
  //       Exceptions are reported to the CSR on the cycle we send the flush
  // This discrepency should be resolved elsewhere.
  when (RegNext(rob.io.flush.valid)) {
    io.ifu.redirect_val   := true.B
    io.ifu.redirect_flush := true.B
    val flush_typ = RegNext(rob.io.flush.bits.flush_typ)
    // Clear the global history when we flush the ROB (exceptions, AMOs, unique instructions, etc.)
    val new_ghist = WireInit((0.U).asTypeOf(new GlobalHistory))
    new_ghist.current_saw_branch_not_taken := true.B
    new_ghist.ras_idx := io.ifu.get_pc(0).entry.ras_idx
    io.ifu.redirect_ghist := new_ghist
    when (FlushTypes.useCsrEvec(flush_typ)) {
      io.ifu.redirect_pc  := Mux(flush_typ === FlushTypes.eret,
                                 RegNext(RegNext(csr.io.evec)),
                                 csr.io.evec)
    } .otherwise {
      val flush_pc = (AlignPCToBoundary(io.ifu.get_pc(0).pc, icBlockBytes)
                      + RegNext(rob.io.flush.bits.pc_lob)
                      - Mux(RegNext(rob.io.flush.bits.edge_inst), 2.U, 0.U))
      val flush_pc_next = flush_pc + Mux(RegNext(rob.io.flush.bits.is_rvc), 2.U, 4.U)
      io.ifu.redirect_pc := Mux(FlushTypes.useSamePC(flush_typ),
                                flush_pc, flush_pc_next)

    }
    io.ifu.redirect_ftq_idx := RegNext(rob.io.flush.bits.ftq_idx)
  } .elsewhen (brupdate.b2.mispredict && !RegNext(rob.io.flush.valid)) {
    val block_pc = AlignPCToBoundary(io.ifu.get_pc(1).pc, icBlockBytes)
    val uop_maybe_pc = block_pc | brupdate.b2.uop.pc_lob
    val npc = uop_maybe_pc + Mux(brupdate.b2.uop.is_rvc || brupdate.b2.uop.edge_inst, 2.U, 4.U)
    val jal_br_target = Wire(UInt(vaddrBitsExtended.W))
    jal_br_target := (uop_maybe_pc.asSInt + brupdate.b2.target_offset +
      (Fill(vaddrBitsExtended-1, brupdate.b2.uop.edge_inst) << 1).asSInt).asUInt
    val bj_addr = Mux(brupdate.b2.cfi_type === CFI_JALR, brupdate.b2.jalr_target, jal_br_target)
    val mispredict_target = Mux(brupdate.b2.pc_sel === PC_PLUS4, npc, bj_addr)
    io.ifu.redirect_val     := true.B
    io.ifu.redirect_pc      := mispredict_target
    io.ifu.redirect_flush   := true.B
    io.ifu.redirect_ftq_idx := brupdate.b2.uop.ftq_idx
    val use_same_ghist = (brupdate.b2.cfi_type === CFI_BR &&
                          !brupdate.b2.taken &&
                          bankAlign(block_pc) === bankAlign(npc))
    val ftq_entry = io.ifu.get_pc(1).entry
    val cfi_idx = (brupdate.b2.uop.pc_lob ^
      Mux(ftq_entry.start_bank === 1.U, 1.U << log2Ceil(bankBytes), 0.U))(log2Ceil(fetchWidth), 1)
    val ftq_ghist = io.ifu.get_pc(1).ghist
    val next_ghist = ftq_ghist.update(
      ftq_entry.br_mask.asUInt,
      brupdate.b2.taken,
      brupdate.b2.cfi_type === CFI_BR,
      cfi_idx,
      true.B,
      io.ifu.get_pc(1).pc,
      ftq_entry.cfi_is_call && ftq_entry.cfi_idx.bits === cfi_idx,
      ftq_entry.cfi_is_ret  && ftq_entry.cfi_idx.bits === cfi_idx)


    io.ifu.redirect_ghist   := Mux(
      use_same_ghist,
      ftq_ghist,
      next_ghist)
    io.ifu.redirect_ghist.current_saw_branch_not_taken := use_same_ghist
  } .elsewhen (rob.io.flush_frontend || brupdate.b1.mispredict_mask =/= 0.U) {
    io.ifu.redirect_flush   := true.B
  }

  // Tell the FTQ it can deallocate entries by passing youngest ftq_idx.
  val youngest_com_idx = (coreWidth-1).U - PriorityEncoder(rob.io.commit.valids.reverse)
  io.ifu.commit.valid := rob.io.commit.valids.reduce(_|_) || rob.io.com_xcpt.valid
  io.ifu.commit.bits  := Mux(rob.io.com_xcpt.valid,
                             rob.io.com_xcpt.bits.ftq_idx,
                             rob.io.commit.uops(youngest_com_idx).ftq_idx)

  assert(!(rob.io.commit.valids.reduce(_|_) && rob.io.com_xcpt.valid),
    "ROB can't commit and except in same cycle!")

  for (i <- 0 until memWidth) {
    when (RegNext(io.lsu.exe(i).req.bits.sfence.valid)) {
      io.ifu.sfence := RegNext(io.lsu.exe(i).req.bits.sfence)
    }
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Branch Prediction ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Decode Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // track mask of finished instructions in the bundle
  // use this to mask out insts coming from FetchBuffer that have been finished
  // for example, back pressure may cause us to only issue some instructions from FetchBuffer
  // but on the next cycle, we only want to retry a subset (dec_finished_mask declared earlier for TMA)

  //-------------------------------------------------------------
  // Pull out instructions and send to the Decoders

  io.ifu.fetchpacket.ready := dec_ready
  val dec_fbundle = io.ifu.fetchpacket.bits

  //-------------------------------------------------------------
  // Decoders

  for (w <- 0 until coreWidth) {
    dec_valids(w)                      := io.ifu.fetchpacket.valid && dec_fbundle.uops(w).valid &&
                                          !dec_finished_mask(w)
    decode_units(w).io.enq.uop         := dec_fbundle.uops(w).bits
    decode_units(w).io.status          := csr.io.status
    decode_units(w).io.csr_decode      <> csr.io.decode(w)
    decode_units(w).io.interrupt       := csr.io.interrupt
    decode_units(w).io.interrupt_cause := csr.io.interrupt_cause

    dec_uops(w) := decode_units(w).io.deq.uop
  }

  //-------------------------------------------------------------
  // FTQ GetPC Port Arbitration

  val jmp_pc_req  = Wire(Decoupled(UInt(log2Ceil(ftqSz).W)))
  val xcpt_pc_req = Wire(Decoupled(UInt(log2Ceil(ftqSz).W)))
  val flush_pc_req = Wire(Decoupled(UInt(log2Ceil(ftqSz).W)))

  val ftq_arb = Module(new Arbiter(UInt(log2Ceil(ftqSz).W), 3))

  // Order by the oldest. Flushes come from the oldest instructions in pipe
  // Decoding exceptions come from youngest
  ftq_arb.io.in(0) <> flush_pc_req
  ftq_arb.io.in(1) <> jmp_pc_req
  ftq_arb.io.in(2) <> xcpt_pc_req

  // Hookup FTQ
  io.ifu.get_pc(0).ftq_idx := ftq_arb.io.out.bits
  ftq_arb.io.out.ready  := true.B

  // Branch Unit Requests (for JALs) (Should delay issue of JALs if this not ready)
  jmp_pc_req.valid := RegNext(iss_valids(jmp_unit_idx) && iss_uops(jmp_unit_idx).fu_code === FU_JMP)
  jmp_pc_req.bits  := RegNext(iss_uops(jmp_unit_idx).ftq_idx)

  jmp_unit.io.get_ftq_pc := DontCare
  jmp_unit.io.get_ftq_pc.pc               := io.ifu.get_pc(0).pc
  jmp_unit.io.get_ftq_pc.entry            := io.ifu.get_pc(0).entry
  jmp_unit.io.get_ftq_pc.next_val         := io.ifu.get_pc(0).next_val
  jmp_unit.io.get_ftq_pc.next_pc          := io.ifu.get_pc(0).next_pc


  // Frontend Exception Requests
  val xcpt_idx = PriorityEncoder(dec_xcpts)
  xcpt_pc_req.valid    := dec_xcpts.reduce(_||_)
  xcpt_pc_req.bits     := dec_uops(xcpt_idx).ftq_idx
  //rob.io.xcpt_fetch_pc := RegEnable(io.ifu.get_pc.fetch_pc, dis_ready)
  rob.io.xcpt_fetch_pc := io.ifu.get_pc(0).pc

  flush_pc_req.valid   := rob.io.flush.valid
  flush_pc_req.bits    := rob.io.flush.bits.ftq_idx

  // Mispredict requests (to get the correct target)
  io.ifu.get_pc(1).ftq_idx := oldest_mispredict_ftq_idx


  //-------------------------------------------------------------
  // Decode/Rename1 pipeline logic

  dec_xcpts := dec_uops zip dec_valids map {case (u,v) => u.exception && v}
  val dec_xcpt_stall = dec_xcpts.reduce(_||_) && !xcpt_pc_req.ready
  // stall fetch/dcode because we ran out of branch tags (Wire declared earlier for TMA)

  val dec_hazards = (0 until coreWidth).map(w =>
                      dec_valids(w) &&
                      (  !dis_ready
                      || rob.io.commit.rollback
                      || dec_xcpt_stall
                      || branch_mask_full(w)
                      || brupdate.b1.mispredict_mask =/= 0.U
                      || brupdate.b2.mispredict
                      || io.ifu.redirect_flush))

  val dec_stalls = dec_hazards.scanLeft(false.B) ((s,h) => s || h).takeRight(coreWidth)
  dec_fire := (0 until coreWidth).map(w => dec_valids(w) && !dec_stalls(w))

  // all decoders are empty and ready for new instructions
  dec_ready := dec_fire.last

  when (dec_ready || io.ifu.redirect_flush) {
    dec_finished_mask := 0.U
    dec_unfinished_frontend_fault := false.B
  } .otherwise {
    dec_finished_mask := dec_fire.asUInt | dec_finished_mask
    dec_unfinished_frontend_fault := !dec_stalls.last
  }

  //-------------------------------------------------------------
  // Branch Mask Logic

  dec_brmask_logic.io.brupdate := brupdate
  dec_brmask_logic.io.flush_pipeline := RegNext(rob.io.flush.valid)

  for (w <- 0 until coreWidth) {
    dec_brmask_logic.io.is_branch(w) := !dec_finished_mask(w) && dec_uops(w).allocate_brtag
    dec_brmask_logic.io.will_fire(w) :=  dec_fire(w) &&
                                         dec_uops(w).allocate_brtag // ren, dis can back pressure us
    dec_uops(w).br_tag  := dec_brmask_logic.io.br_tag(w)
    dec_uops(w).br_mask := dec_brmask_logic.io.br_mask(w)
  }

  branch_mask_full := dec_brmask_logic.io.is_full

  //****************************************
  // TMA (Top-Down Microarchitectural Analysis) Counters
  // Always-on 64-bit counters for slot-based TMA accounting.
  // Gated by enableTMACounters parameter.
  // Placed here because it needs dec_stalls, dec_finished_mask, branch_mask_full etc.

  if (boomParams.enableTMACounters) {
    //------------------------------------------------------
    // TMA Level 1: classify each pipeline slot each cycle
    // Frontend/backend bound are classified at decode time (where the stall is observable).
    // Retiring is counted at commit time (arch_valids) to match instret.
    // Bad speculation is derived as the remainder to preserve the tiling invariant.
    val tma_slot_frontend_bound = Wire(Vec(coreWidth, Bool()))
    val tma_slot_backend_bound  = Wire(Vec(coreWidth, Bool()))
    val tma_slot_bad_spec = Wire(Vec(coreWidth, Bool()))

    for (w <- 0 until coreWidth) {

      // If we're recovering, then it's not frontend or backend bound
      when (tma_in_recovery) {
        tma_slot_frontend_bound(w) := false.B
        tma_slot_backend_bound(w)  := false.B
        tma_slot_bad_spec(w) := true.B
      }

      // When there is no uop in decode slot AND it's not because of a
      // partial packet the last cycle, then that slot is frontend bound
      .elsewhen ( !dec_valids(w) && !dec_finished_mask(w) ) {
        tma_slot_frontend_bound(w) := true.B
        tma_slot_backend_bound(w) := false.B
        tma_slot_bad_spec(w) := false.B
      }
      // When a slot is blocked by a dec_finished_mask from a previous cycle,
      // classify based on why the row didn't complete:
      //   - dec_unfinished_frontend_fault = true  -> frontend couldn't fill the row
      //   - dec_unfinished_frontend_fault = false -> backend backpressure stalled decode
      .elsewhen ( dec_finished_mask(w) && dec_unfinished_frontend_fault ) {
        tma_slot_frontend_bound(w) := true.B
        tma_slot_backend_bound(w) := false.B
        tma_slot_bad_spec(w) := false.B
      }
      .elsewhen ( dec_finished_mask(w) && !dec_unfinished_frontend_fault ) {
        tma_slot_frontend_bound(w) := false.B
        tma_slot_backend_bound(w) := true.B
        tma_slot_bad_spec(w) := false.B
      }
      // When there is a uop in decode, but it cannot move on because of
      // a stall in decode, then that slot is backend bound
      .elsewhen ( dec_valids(w) && dec_stalls(w) ) {
        tma_slot_frontend_bound(w) := false.B
        tma_slot_backend_bound(w) := true.B
        tma_slot_bad_spec(w) := false.B
      }
      .otherwise {
        tma_slot_frontend_bound(w) := false.B
        tma_slot_backend_bound(w) := false.B
        tma_slot_bad_spec(w) := false.B
      }
    }



    // TMA L1 counters
    // Retiring is counted at commit (arch_valids), not decode, to match instret.
    // Bad speculation is derived at read time as the aggregate remainder:
    //   bad_spec = coreWidth * cycles - retiring - frontend_bound - backend_bound
    // This avoids per-cycle unsigned underflow from the commit/decode temporal mismatch,
    // and correctly absorbs wrong-path slots that were classified as frontend/backend at decode.
    val tma_ctr_retiring       = RegInit(0.U(xLen.W))
    val tma_ctr_frontend_bound = RegInit(0.U(xLen.W))
    val tma_ctr_backend_bound  = RegInit(0.U(xLen.W))
    val tma_ctr_bad_spec       = RegInit(0.U(xLen.W))

    tma_ctr_retiring       := tma_ctr_retiring       + PopCount(rob.io.commit.arch_valids.asUInt)
    tma_ctr_frontend_bound := tma_ctr_frontend_bound + PopCount(tma_slot_frontend_bound.asUInt)
    tma_ctr_backend_bound  := tma_ctr_backend_bound  + PopCount(tma_slot_backend_bound.asUInt)
    tma_ctr_bad_spec       := tma_ctr_bad_spec       + PopCount(tma_slot_bad_spec.asUInt) + rob.io.tma_killed_by_branch_count + rob.io.tma_killed_by_rollback_count + rename_stage.io.tma_kill_machine_clear + rename_stage.io.tma_kill_branch_mispredict

    dontTouch(tma_ctr_retiring)
    dontTouch(tma_ctr_frontend_bound)
    dontTouch(tma_ctr_backend_bound)
    dontTouch(tma_ctr_bad_spec)

    // TMA Level 2 counters
    val tma_ctr_fetch_latency   = RegInit(0.U(xLen.W))
    val tma_ctr_fetch_bandwidth = RegInit(0.U(xLen.W))

    val frontend_slots_this_cycle = PopCount(tma_slot_frontend_bound.asUInt)
    when (!tma_in_recovery && !tma_fetch_valid) {
      tma_ctr_fetch_latency := tma_ctr_fetch_latency + frontend_slots_this_cycle
    } .elsewhen (!tma_in_recovery && tma_fetch_valid && frontend_slots_this_cycle > 0.U) {
      tma_ctr_fetch_bandwidth := tma_ctr_fetch_bandwidth + frontend_slots_this_cycle
    }

    dontTouch(tma_ctr_fetch_latency)
    dontTouch(tma_ctr_fetch_bandwidth)

    val tma_ctr_branch_mispredict = RegInit(0.U(xLen.W))
    val tma_ctr_machine_clears   = RegInit(0.U(xLen.W))

    tma_ctr_branch_mispredict := tma_ctr_branch_mispredict + rob.io.tma_killed_by_branch_count + rename_stage.io.tma_kill_branch_mispredict + Mux(tma_is_branch_mispredict_recovery, coreWidth.U, 0.U)
    tma_ctr_machine_clears := tma_ctr_machine_clears + rob.io.tma_killed_by_rollback_count + rename_stage.io.tma_kill_machine_clear + Mux(tma_is_machine_clear, coreWidth.U, 0.U)

    // when (tma_in_recovery) {
    //   when (tma_is_branch_mispredict_recovery) {
    //     tma_ctr_branch_mispredict := tma_ctr_branch_mispredict + coreWidth.U
    //   } .elsewhen (tma_is_machine_clear) {
    //     tma_ctr_machine_clears := tma_ctr_machine_clears + coreWidth.U
    //   }
    // }

    dontTouch(tma_ctr_branch_mispredict)
    dontTouch(tma_ctr_machine_clears)

    val tma_ctr_memory_bound = RegInit(0.U(xLen.W))
    val tma_ctr_core_bound   = RegInit(0.U(xLen.W))

    // Per-slot signal: this slot's instruction is valid and stalled specifically because the IQ is full
    val dis_iq_full_stall = (0 until coreWidth).map(w =>
      dis_valids(w) && (!dispatcher.io.ren_uops(w).ready || rob.io.full || ren_stalls(w)))

    // Per-slot: IQ-full stall while a demand dcache refill is in flight (memory-bound IQ pressure)
    val dis_iq_full_dcache_miss = (0 until coreWidth).map(w =>
      (dis_iq_full_stall(w)) && io.lsu.refill_in_flight)

    val refill_blocking_decode = dis_iq_full_dcache_miss.reduce(_||_)

    // Use PopCount to correctly count across all slots (`:=` in a for loop
    // would only keep the last slot's increment due to last-connect semantics)
    val mem_bound_slots = PopCount(VecInit((0 until coreWidth).map { w =>
      tma_slot_backend_bound(w) && dis_valids(w) && (
        (io.lsu.ldq_full(w) && dis_uops(w).uses_ldq) ||
        (io.lsu.stq_full(w) && dis_uops(w).uses_stq) ||
        tma_mem_iq_full ||
        refill_blocking_decode) 
        // refill_blocking_decode is a heuristic for memory bound 
        // where the IQs are full (blocking any dispatch) and there
        // is an outstanding refill request in dcache
    }))
    tma_ctr_memory_bound := tma_ctr_memory_bound + mem_bound_slots
    tma_ctr_core_bound   := tma_ctr_core_bound + (PopCount(tma_slot_backend_bound.asUInt) - mem_bound_slots)

    dontTouch(tma_ctr_memory_bound)
    dontTouch(tma_ctr_core_bound)

    // Instruction mix at retire
    val tma_ctr_retired_loads    = RegInit(0.U(xLen.W))
    val tma_ctr_retired_stores   = RegInit(0.U(xLen.W))
    val tma_ctr_retired_branches = RegInit(0.U(xLen.W))
    val tma_ctr_retired_jals     = RegInit(0.U(xLen.W))
    val tma_ctr_retired_jalrs    = RegInit(0.U(xLen.W))
    val tma_ctr_retired_fp       = RegInit(0.U(xLen.W))
    val tma_ctr_retired_amo      = RegInit(0.U(xLen.W))
    val tma_ctr_retired_system   = RegInit(0.U(xLen.W))

    tma_ctr_retired_loads    := tma_ctr_retired_loads    + tma_retired_loads
    tma_ctr_retired_stores   := tma_ctr_retired_stores   + tma_retired_stores
    tma_ctr_retired_branches := tma_ctr_retired_branches + tma_retired_branches
    tma_ctr_retired_jals     := tma_ctr_retired_jals     + tma_retired_jals
    tma_ctr_retired_jalrs    := tma_ctr_retired_jalrs    + tma_retired_jalrs
    tma_ctr_retired_fp       := tma_ctr_retired_fp       + tma_retired_fp
    tma_ctr_retired_amo      := tma_ctr_retired_amo      + tma_retired_amo
    tma_ctr_retired_system   := tma_ctr_retired_system   + tma_retired_system

    dontTouch(tma_ctr_retired_loads)
    dontTouch(tma_ctr_retired_stores)
    dontTouch(tma_ctr_retired_branches)
    dontTouch(tma_ctr_retired_jals)
    dontTouch(tma_ctr_retired_jalrs)
    dontTouch(tma_ctr_retired_fp)
    dontTouch(tma_ctr_retired_amo)
    dontTouch(tma_ctr_retired_system)

    // Stall reason counters
    val tma_ctr_rob_full          = RegInit(0.U(xLen.W))
    val tma_ctr_ldq_full          = RegInit(0.U(xLen.W))
    val tma_ctr_stq_full          = RegInit(0.U(xLen.W))
    val tma_ctr_int_iq_full       = RegInit(0.U(xLen.W))
    val tma_ctr_mem_iq_full       = RegInit(0.U(xLen.W))
    val tma_ctr_branch_mask_full  = RegInit(0.U(xLen.W))
    val tma_ctr_rename_stall      = RegInit(0.U(xLen.W))
    val tma_ctr_flush_cycles      = RegInit(0.U(xLen.W))
    val tma_ctr_rollback_cycles   = RegInit(0.U(xLen.W))

    tma_ctr_rob_full         := tma_ctr_rob_full         + tma_rob_full
    tma_ctr_ldq_full         := tma_ctr_ldq_full         + tma_ldq_full
    tma_ctr_stq_full         := tma_ctr_stq_full         + tma_stq_full
    tma_ctr_int_iq_full      := tma_ctr_int_iq_full      + tma_int_iq_full
    tma_ctr_mem_iq_full      := tma_ctr_mem_iq_full      + tma_mem_iq_full
    tma_ctr_branch_mask_full := tma_ctr_branch_mask_full + tma_branch_mask_full_any
    tma_ctr_rename_stall     := tma_ctr_rename_stall     + tma_ren_stall_any
    tma_ctr_flush_cycles     := tma_ctr_flush_cycles     + rob.io.flush.valid
    tma_ctr_rollback_cycles  := tma_ctr_rollback_cycles  + rob.io.commit.rollback

    dontTouch(tma_ctr_rob_full)
    dontTouch(tma_ctr_ldq_full)
    dontTouch(tma_ctr_stq_full)
    dontTouch(tma_ctr_int_iq_full)
    dontTouch(tma_ctr_mem_iq_full)
    dontTouch(tma_ctr_branch_mask_full)
    dontTouch(tma_ctr_rename_stall)
    dontTouch(tma_ctr_flush_cycles)
    dontTouch(tma_ctr_rollback_cycles)

    // Cache/TLB event counters
    val tma_ctr_icache_miss    = RegInit(0.U(xLen.W))
    val tma_ctr_icache_lookups = RegInit(0.U(xLen.W)) // Resolved I-cache lookup outcomes (io.resp.valid || s2_miss); miss-rate denominator
    val tma_ctr_dcache_miss  = RegInit(0.U(xLen.W))
    val tma_ctr_dcache_rel   = RegInit(0.U(xLen.W))
    val tma_ctr_itlb_miss    = RegInit(0.U(xLen.W))
    val tma_ctr_dtlb_miss    = RegInit(0.U(xLen.W))
    val tma_ctr_l2tlb_miss   = RegInit(0.U(xLen.W))

    tma_ctr_icache_miss    := tma_ctr_icache_miss + io.ifu.perf.acquire
    tma_ctr_icache_lookups := tma_ctr_icache_lookups + io.ifu.perf.lookups
    tma_ctr_dcache_miss := tma_ctr_dcache_miss + io.lsu.perf.acquire
    tma_ctr_dcache_rel  := tma_ctr_dcache_rel  + io.lsu.perf.release
    tma_ctr_itlb_miss   := tma_ctr_itlb_miss   + io.ifu.perf.tlbMiss
    tma_ctr_dtlb_miss   := tma_ctr_dtlb_miss   + io.lsu.perf.tlbMiss
    tma_ctr_l2tlb_miss  := tma_ctr_l2tlb_miss  + io.ptw.perf.l2miss

    dontTouch(tma_ctr_icache_miss)
    dontTouch(tma_ctr_icache_lookups)
    dontTouch(tma_ctr_dcache_miss)
    dontTouch(tma_ctr_dcache_rel)
    dontTouch(tma_ctr_itlb_miss)
    dontTouch(tma_ctr_dtlb_miss)
    dontTouch(tma_ctr_l2tlb_miss)

    // Branch prediction source counters
    val tma_ctr_br_mispredict      = RegInit(0.U(xLen.W))
    val tma_ctr_br_resolve         = RegInit(0.U(xLen.W))
    val tma_ctr_jalr_mispredict    = RegInit(0.U(xLen.W))
    val tma_ctr_br_mispred_bpd     = RegInit(0.U(xLen.W))
    val tma_ctr_br_mispred_btb     = RegInit(0.U(xLen.W))

    // b2.valid is never set in BOOM v3 — b2 only captures mispredict info.
    // Use brinfos (per-ALU branch resolution outputs) for resolve counting.
    tma_ctr_br_mispredict   := tma_ctr_br_mispredict   + b2.mispredict
    tma_ctr_br_resolve      := tma_ctr_br_resolve      + PopCount(brinfos.map(_.valid))
    tma_ctr_jalr_mispredict := tma_ctr_jalr_mispredict + (b2.mispredict && b2.cfi_type === CFI_JALR)
    tma_ctr_br_mispred_bpd  := tma_ctr_br_mispred_bpd + (b2.mispredict && b2.uop.debug_fsrc === BSRC_2)
    tma_ctr_br_mispred_btb  := tma_ctr_br_mispred_btb + (b2.mispredict && b2.uop.debug_fsrc === BSRC_1)

    dontTouch(tma_ctr_br_mispredict)
    dontTouch(tma_ctr_br_resolve)
    dontTouch(tma_ctr_jalr_mispredict)
    dontTouch(tma_ctr_br_mispred_bpd)
    dontTouch(tma_ctr_br_mispred_btb)

    // --- Pipeline & Execution counters ---
    val tma_ctr_dispatch_slots_valid = RegInit(0.U(xLen.W))
    val tma_ctr_issued_int           = RegInit(0.U(xLen.W))
    val tma_ctr_issued_mem           = RegInit(0.U(xLen.W))
    val tma_ctr_issued_mul           = RegInit(0.U(xLen.W))
    val tma_ctr_issued_div           = RegInit(0.U(xLen.W))
    val tma_ctr_flush_xcpt           = RegInit(0.U(xLen.W))
    val tma_ctr_flush_eret           = RegInit(0.U(xLen.W))
    val tma_ctr_flush_refetch        = RegInit(0.U(xLen.W))
    val tma_ctr_flush_next           = RegInit(0.U(xLen.W))
    val tma_ctr_dis_stall            = RegInit(0.U(xLen.W))

    tma_ctr_dispatch_slots_valid := tma_ctr_dispatch_slots_valid + PopCount(dis_valids)
    tma_ctr_issued_int  := tma_ctr_issued_int  + PopCount(int_iss_unit.io.iss_valids)
    tma_ctr_issued_mem  := tma_ctr_issued_mem  + PopCount(mem_iss_unit.io.iss_valids)
    tma_ctr_issued_mul  := tma_ctr_issued_mul  + PopCount(VecInit((0 until exe_units.numIrfReaders).map(i => iss_valids(i) && iss_uops(i).fu_code_is(FU_MUL))))
    tma_ctr_issued_div  := tma_ctr_issued_div  + PopCount(VecInit((0 until exe_units.numIrfReaders).map(i => iss_valids(i) && iss_uops(i).fu_code_is(FU_DIV))))
    tma_ctr_flush_xcpt    := tma_ctr_flush_xcpt    + (rob.io.flush.valid && rob.io.flush.bits.flush_typ === FlushTypes.xcpt)
    tma_ctr_flush_eret    := tma_ctr_flush_eret    + (rob.io.flush.valid && rob.io.flush.bits.flush_typ === FlushTypes.eret)
    tma_ctr_flush_refetch := tma_ctr_flush_refetch + (rob.io.flush.valid && rob.io.flush.bits.flush_typ === FlushTypes.refetch)
    tma_ctr_flush_next    := tma_ctr_flush_next    + (rob.io.flush.valid && rob.io.flush.bits.flush_typ === FlushTypes.next)
    tma_ctr_dis_stall     := tma_ctr_dis_stall     + !dis_ready

    dontTouch(tma_ctr_dispatch_slots_valid)
    dontTouch(tma_ctr_issued_int)
    dontTouch(tma_ctr_issued_mem)
    dontTouch(tma_ctr_issued_mul)
    dontTouch(tma_ctr_issued_div)
    dontTouch(tma_ctr_flush_xcpt)
    dontTouch(tma_ctr_flush_eret)
    dontTouch(tma_ctr_flush_refetch)
    dontTouch(tma_ctr_flush_next)
    dontTouch(tma_ctr_dis_stall)

    // --- Branch Prediction counters ---
    val tma_ctr_br_cond_mispredict     = RegInit(0.U(xLen.W))
    val tma_ctr_br_indirect_mispredict = RegInit(0.U(xLen.W))
    val tma_ctr_br_ret_mispredict      = RegInit(0.U(xLen.W))
    val tma_ctr_br_no_prediction       = RegInit(0.U(xLen.W))

    val tma_ftq1_is_ret = io.ifu.get_pc(1).entry.cfi_is_ret
    tma_ctr_br_cond_mispredict     := tma_ctr_br_cond_mispredict     + (b2.mispredict && b2.cfi_type === CFI_BR)
    tma_ctr_br_indirect_mispredict := tma_ctr_br_indirect_mispredict + (b2.mispredict && b2.cfi_type === CFI_JALR && !tma_ftq1_is_ret)
    tma_ctr_br_ret_mispredict      := tma_ctr_br_ret_mispredict      + (b2.mispredict && b2.cfi_type === CFI_JALR && tma_ftq1_is_ret)
    tma_ctr_br_no_prediction       := tma_ctr_br_no_prediction       + PopCount(VecInit(brinfos.map(bi => bi.valid && bi.uop.debug_fsrc === BSRC_C)))

    dontTouch(tma_ctr_br_cond_mispredict)
    dontTouch(tma_ctr_br_indirect_mispredict)
    dontTouch(tma_ctr_br_ret_mispredict)
    dontTouch(tma_ctr_br_no_prediction)

    // --- Fetch & Decode counters ---
    val tma_ctr_fetch_bubble_raw      = RegInit(0.U(xLen.W))
    val tma_ctr_fetch_slots_delivered = RegInit(0.U(xLen.W))
    val tma_ctr_decode_backend_stall  = RegInit(0.U(xLen.W))
    val tma_ctr_int_iq_empty          = RegInit(0.U(xLen.W))
    val tma_ctr_mem_iq_empty          = RegInit(0.U(xLen.W))
    val tma_ctr_sfb_opt_events        = RegInit(0.U(xLen.W))

    tma_ctr_fetch_bubble_raw      := tma_ctr_fetch_bubble_raw      + !io.ifu.fetchpacket.valid
    tma_ctr_fetch_slots_delivered := tma_ctr_fetch_slots_delivered + Mux(io.ifu.fetchpacket.valid,
      PopCount(VecInit((0 until coreWidth).map(w => dec_fbundle.uops(w).valid))), 0.U)
    tma_ctr_decode_backend_stall  := tma_ctr_decode_backend_stall  + (io.ifu.fetchpacket.valid && !dis_ready)
    tma_ctr_int_iq_empty          := tma_ctr_int_iq_empty          + int_iss_unit.io.event_empty
    tma_ctr_mem_iq_empty          := tma_ctr_mem_iq_empty          + mem_iss_unit.io.event_empty
    tma_ctr_sfb_opt_events        := tma_ctr_sfb_opt_events        + PopCount(VecInit((0 until exe_units.numIrfReaders).map(i => iss_valids(i) && iss_uops(i).is_sfb_br)))

    dontTouch(tma_ctr_fetch_bubble_raw)
    dontTouch(tma_ctr_fetch_slots_delivered)
    dontTouch(tma_ctr_decode_backend_stall)
    dontTouch(tma_ctr_int_iq_empty)
    dontTouch(tma_ctr_mem_iq_empty)
    dontTouch(tma_ctr_sfb_opt_events)

    // --- Memory ordering counters (60-67) ---
    val tma_ctr_stld_fwd_stall_cycles       = RegInit(0.U(xLen.W))
    val tma_ctr_stld_fwd_success            = RegInit(0.U(xLen.W))
    val tma_ctr_stld_fwd_wakeup_retries     = RegInit(0.U(xLen.W))
    val tma_ctr_stld_block_load_wakeup      = RegInit(0.U(xLen.W))
    val tma_ctr_mem_order_failures          = RegInit(0.U(xLen.W))
    val tma_ctr_load_ordering_failures      = RegInit(0.U(xLen.W))
    val tma_ctr_load_spec_mispredict        = RegInit(0.U(xLen.W))
    val tma_ctr_load_nack_retries           = RegInit(0.U(xLen.W))

    if (boomParams.enableMemOrderCounters) {
      tma_ctr_stld_fwd_stall_cycles   := tma_ctr_stld_fwd_stall_cycles + io.lsu.perf.stldForwardStall
      tma_ctr_stld_fwd_success        := tma_ctr_stld_fwd_success + io.lsu.perf.stldForwardSuccess
      tma_ctr_stld_fwd_wakeup_retries := tma_ctr_stld_fwd_wakeup_retries + io.lsu.perf.stldForwardWakeupRetry
      tma_ctr_stld_block_load_wakeup  := tma_ctr_stld_block_load_wakeup + io.lsu.perf.stldBlockLoadWakeup
      tma_ctr_mem_order_failures      := tma_ctr_mem_order_failures +
        (io.lsu.lxcpt.valid && io.lsu.lxcpt.bits.cause === MINI_EXCEPTION_MEM_ORDERING)
      tma_ctr_load_ordering_failures  := tma_ctr_load_ordering_failures + io.lsu.perf.loadOrderingFailure
      tma_ctr_load_spec_mispredict    := tma_ctr_load_spec_mispredict + io.lsu.ld_miss
      tma_ctr_load_nack_retries       := tma_ctr_load_nack_retries + io.lsu.perf.loadNackRetry
    }

    dontTouch(tma_ctr_stld_fwd_stall_cycles)
    dontTouch(tma_ctr_stld_fwd_success)
    dontTouch(tma_ctr_stld_fwd_wakeup_retries)
    dontTouch(tma_ctr_stld_block_load_wakeup)
    dontTouch(tma_ctr_mem_order_failures)
    dontTouch(tma_ctr_load_ordering_failures)
    dontTouch(tma_ctr_load_spec_mispredict)
    dontTouch(tma_ctr_load_nack_retries)

    // --- Data dependency counters (68-74) ---
    val tma_ctr_dep_stall_cycles         = RegInit(0.U(xLen.W))
    val tma_ctr_operand_wait_slot_cycles = RegInit(0.U(xLen.W))
    val tma_ctr_iq_dispatched_ready      = RegInit(0.U(xLen.W))
    val tma_ctr_iq_dispatched_not_ready  = RegInit(0.U(xLen.W))
    val tma_ctr_issued_with_poison       = RegInit(0.U(xLen.W))
    val tma_ctr_ldspec_squash_grants     = RegInit(0.U(xLen.W))
    val tma_ctr_spec_ld_wakeup_events    = RegInit(0.U(xLen.W))

    if (boomParams.enableDataDepCounters) {
      val any_dep_stall = issue_units.map(_.io.perf_dep.dep_stall).reduce(_||_)
      tma_ctr_dep_stall_cycles := tma_ctr_dep_stall_cycles + any_dep_stall

      val total_not_ready = issue_units.map(_.io.perf_dep.not_ready_slots).reduce(_ +& _)
      tma_ctr_operand_wait_slot_cycles := tma_ctr_operand_wait_slot_cycles + total_not_ready

      val total_disp_ready = issue_units.map(_.io.perf_dep.iq_dispatched_ready).reduce(_ +& _)
      val total_disp_not_ready = issue_units.map(_.io.perf_dep.iq_dispatched_not_ready).reduce(_ +& _)
      tma_ctr_iq_dispatched_ready     := tma_ctr_iq_dispatched_ready     + total_disp_ready
      tma_ctr_iq_dispatched_not_ready := tma_ctr_iq_dispatched_not_ready + total_disp_not_ready

      val total_poison = issue_units.map(_.io.perf_dep.issued_with_poison).reduce(_ +& _)
      tma_ctr_issued_with_poison := tma_ctr_issued_with_poison + total_poison

      val total_squash = issue_units.map(_.io.perf_dep.squash_grants).reduce(_ +& _)
      tma_ctr_ldspec_squash_grants := tma_ctr_ldspec_squash_grants + total_squash

      tma_ctr_spec_ld_wakeup_events := tma_ctr_spec_ld_wakeup_events +
        PopCount(io.lsu.spec_ld_wakeup.map(_.valid))
    }

    dontTouch(tma_ctr_dep_stall_cycles)
    dontTouch(tma_ctr_operand_wait_slot_cycles)
    dontTouch(tma_ctr_iq_dispatched_ready)
    dontTouch(tma_ctr_iq_dispatched_not_ready)
    dontTouch(tma_ctr_issued_with_poison)
    dontTouch(tma_ctr_ldspec_squash_grants)
    dontTouch(tma_ctr_spec_ld_wakeup_events)

    // OOO engine counters (92-98)
    // Physical register exhaustion: subset decomposition of rename_stall.
    // int_preg_stall + fp_preg_stall <= rename_stall (predicate stalls not decomposed).
    val tma_ctr_int_preg_stall    = RegInit(0.U(xLen.W))
    val tma_ctr_fp_preg_stall     = RegInit(0.U(xLen.W))
    // Retirement width distribution: cycles with exactly N instructions retired.
    // retire_width_0 + ... + retire_width_4 == cycles (for coreWidth <= 4).
    val tma_ctr_retire_width_0    = RegInit(0.U(xLen.W))
    val tma_ctr_retire_width_1    = RegInit(0.U(xLen.W))
    val tma_ctr_retire_width_2    = RegInit(0.U(xLen.W))
    val tma_ctr_retire_width_3    = RegInit(0.U(xLen.W))
    val tma_ctr_retire_width_4    = RegInit(0.U(xLen.W))

    if (boomParams.enableOOOEngineCounters) {
      // INT freelist exhaustion: any slot's INT rename can't allocate
      val int_ren_stall = rename_stage.io.ren_stalls.reduce(_||_)
      tma_ctr_int_preg_stall := tma_ctr_int_preg_stall + int_ren_stall

      // FP freelist exhaustion: any slot's FP rename can't allocate
      if (usingFPU) {
        val fp_ren_stall = fp_rename_stage.io.ren_stalls.reduce(_||_)
        tma_ctr_fp_preg_stall := tma_ctr_fp_preg_stall + fp_ren_stall
      }

      // Retirement width distribution
      val retire_count = PopCount(rob.io.commit.arch_valids.asUInt)
      tma_ctr_retire_width_0 := tma_ctr_retire_width_0 + (retire_count === 0.U)
      tma_ctr_retire_width_1 := tma_ctr_retire_width_1 + (retire_count === 1.U)
      if (coreWidth >= 2) {
        tma_ctr_retire_width_2 := tma_ctr_retire_width_2 + (retire_count === 2.U)
      }
      if (coreWidth >= 3) {
        tma_ctr_retire_width_3 := tma_ctr_retire_width_3 + (retire_count === 3.U)
      }
      if (coreWidth >= 4) {
        tma_ctr_retire_width_4 := tma_ctr_retire_width_4 + (retire_count === 4.U)
      }
    }

    dontTouch(tma_ctr_int_preg_stall)
    dontTouch(tma_ctr_fp_preg_stall)
    dontTouch(tma_ctr_retire_width_0)
    dontTouch(tma_ctr_retire_width_1)
    dontTouch(tma_ctr_retire_width_2)
    dontTouch(tma_ctr_retire_width_3)
    dontTouch(tma_ctr_retire_width_4)

    // --- L3 TMA Counters (100-108) ---
    // Intel-inspired BOOM-native observability counters.
    // These are raw occupancy / throughput counters, NOT parent-gated decomposition counters.
    // The existing TMA L1/L2 parents are slot-based; these L3 counters are cycle-based.
    // Do not subtract these from slot-based parents without acknowledging the unit mismatch.

    // Memory Bound L3: cycles with at least one demand L1D MSHR in its refill path active.
    // Uses refill_in_flight which covers the full demand miss handling pipeline
    // (from MSHR allocation through refill, drain, and metadata write).
    // This is broader than pure "waiting for refill data" but narrower than "any MSHR allocated"
    // because it excludes prefetch-only MSHRs.
    val tma_ctr_l1d_miss_pending = RegInit(0.U(xLen.W))
    tma_ctr_l1d_miss_pending := tma_ctr_l1d_miss_pending + io.lsu.refill_in_flight
    dontTouch(tma_ctr_l1d_miss_pending)

    // Core Bound L3: cycles with any divider (INT or FP) busy.
    // div_busy = !div.io.req.ready || (req.valid && fu_code_is(FU_DIV)), i.e. divider cannot accept new work.
    val tma_ctr_divider_active = RegInit(0.U(xLen.W))
    val any_int_div_busy = exe_units.anyDivBusy
    val any_fp_div_busy  = if (usingFPU) fp_pipeline.io.perf_fdiv_busy else false.B
    tma_ctr_divider_active := tma_ctr_divider_active + (any_int_div_busy || any_fp_div_busy)
    dontTouch(tma_ctr_divider_active)

    // Core Bound L3: issue port utilization (INT + FP).
    // These count cycles by issue-port activity threshold, NOT execution completion.
    // "issued" = uop leaves issue queue into register-read stage.
    // Invariant: no_issue + issued_c1 == cycles (every cycle is either 0 or >= 1 issued).
    // Monotonicity: issued_c3 <= issued_c2 <= issued_c1 <= cycles.
    // On narrower BOOM configs (e.g. 2-wide), issued_c3 may be rarely active.
    val int_issue_count = PopCount(VecInit((0 until exe_units.numIrfReaders).map(i => iss_valids(i))))
    val fp_issue_count  = if (usingFPU) PopCount(fp_pipeline.io.perf_iss_valids) else 0.U
    val total_issue_count = int_issue_count +& fp_issue_count

    val tma_ctr_no_issue  = RegInit(0.U(xLen.W))
    val tma_ctr_issued_c1 = RegInit(0.U(xLen.W))
    val tma_ctr_issued_c2 = RegInit(0.U(xLen.W))
    val tma_ctr_issued_c3 = RegInit(0.U(xLen.W))
    tma_ctr_no_issue  := tma_ctr_no_issue  + (total_issue_count === 0.U)
    tma_ctr_issued_c1 := tma_ctr_issued_c1 + (total_issue_count >= 1.U)
    tma_ctr_issued_c2 := tma_ctr_issued_c2 + (total_issue_count >= 2.U)
    tma_ctr_issued_c3 := tma_ctr_issued_c3 + (total_issue_count >= 3.U)
    dontTouch(tma_ctr_no_issue)
    dontTouch(tma_ctr_issued_c1)
    dontTouch(tma_ctr_issued_c2)
    dontTouch(tma_ctr_issued_c3)

    // Fetch Latency L3: I-cache stall cycles.
    // Counts cycles where frontend s2 stage has a valid request but I-cache hasn't responded
    // and it's not a TLB miss (mutually exclusive with itlb_stall in s2).
    val tma_ctr_icache_stall = RegInit(0.U(xLen.W))
    tma_ctr_icache_stall := tma_ctr_icache_stall + io.ifu.perf.icacheStall
    dontTouch(tma_ctr_icache_stall)

    // Fetch Latency L3: I-TLB stall cycles.
    // Counts cycles where frontend s2 stage has a valid request and ITLB miss is active.
    // Guarded by s2_valid to avoid counting stale register state.
    val tma_ctr_itlb_stall = RegInit(0.U(xLen.W))
    tma_ctr_itlb_stall := tma_ctr_itlb_stall + io.ifu.perf.itlbStall
    dontTouch(tma_ctr_itlb_stall)

    // Fetch Latency L3: branch mispredict recovery cycles.
    // Counts cycles where the frontend has not yet delivered a valid fetch packet
    // after a branch mispredict (b2.mispredict). The FSM arms on b2.mispredict and
    // clears on the first cycle where fetchpacket.valid is true. Only cycles where
    // fetchpacket.valid is false while the FSM is armed are counted.
    // Intentionally scoped to branch mispredicts only, not general frontend resteers
    // (machine clears, RAS corrections, BTB corrections are excluded).
    // If a new b2.mispredict fires while already armed, the FSM stays armed (when-priority).
    val tma_ctr_branch_mispredict_recovery = RegInit(0.U(xLen.W))
    val in_branch_mispredict_recovery = RegInit(false.B)
    when (brupdate.b2.mispredict) {
      in_branch_mispredict_recovery := true.B
    } .elsewhen (io.ifu.fetchpacket.valid) {
      in_branch_mispredict_recovery := false.B
    }
    tma_ctr_branch_mispredict_recovery := tma_ctr_branch_mispredict_recovery +
      (in_branch_mispredict_recovery && !io.ifu.fetchpacket.valid)
    dontTouch(tma_ctr_branch_mispredict_recovery)

    // Populate MMIO counter output vector
    io.tma_counters.get := VecInit(Seq(
      debug_tsc_reg,                // 0: cycles
      debug_irt_reg,                // 1: instret
      tma_ctr_retiring,             // 2: tma_retiring
      tma_ctr_bad_spec,             // 3: tma_bad_speculation (derived)
      tma_ctr_frontend_bound,       // 4: tma_frontend_bound
      tma_ctr_backend_bound,        // 5: tma_backend_bound
      tma_ctr_fetch_latency,        // 6: tma_fetch_latency
      tma_ctr_fetch_bandwidth,      // 7: tma_fetch_bandwidth
      tma_ctr_branch_mispredict,    // 8: tma_branch_mispredict
      tma_ctr_machine_clears,       // 9: tma_machine_clears
      tma_ctr_memory_bound,         // 10: tma_memory_bound
      tma_ctr_core_bound,           // 11: tma_core_bound
      tma_ctr_retired_loads,        // 12: retired_loads
      tma_ctr_retired_stores,       // 13: retired_stores
      tma_ctr_retired_branches,     // 14: retired_branches
      tma_ctr_retired_jals,         // 15: retired_jals
      tma_ctr_retired_jalrs,        // 16: retired_jalrs
      tma_ctr_retired_fp,           // 17: retired_fp
      tma_ctr_retired_amo,          // 18: retired_amo
      tma_ctr_retired_system,       // 19: retired_system
      tma_ctr_rob_full,             // 20: rob_full_cycles
      tma_ctr_ldq_full,             // 21: ldq_full_cycles
      tma_ctr_stq_full,             // 22: stq_full_cycles
      tma_ctr_int_iq_full,          // 23: int_iq_full_cycles
      tma_ctr_mem_iq_full,          // 24: mem_iq_full_cycles
      tma_ctr_branch_mask_full,     // 25: branch_mask_full_cycles
      tma_ctr_rename_stall,         // 26: rename_stall_cycles
      tma_ctr_flush_cycles,         // 27: flush_cycles
      tma_ctr_rollback_cycles,      // 28: rollback_cycles
      tma_ctr_icache_miss,          // 29: icache_miss
      tma_ctr_dcache_miss,          // 30: dcache_miss
      tma_ctr_dcache_rel,           // 31: dcache_release
      tma_ctr_itlb_miss,            // 32: itlb_miss
      tma_ctr_dtlb_miss,            // 33: dtlb_miss
      tma_ctr_l2tlb_miss,           // 34: l2tlb_miss
      tma_ctr_br_mispredict,        // 35: br_mispredict
      tma_ctr_br_resolve,           // 36: br_resolve
      tma_ctr_jalr_mispredict,      // 37: jalr_mispredict
      tma_ctr_br_mispred_bpd,       // 38: br_mispredict_bpd
      tma_ctr_br_mispred_btb,       // 39: br_mispredict_btb
      // New core counters (40-59)
      tma_ctr_dispatch_slots_valid, // 40: dispatch_slots_valid
      tma_ctr_issued_int,           // 41: issued_int_total
      tma_ctr_issued_mem,           // 42: issued_mem_total
      tma_ctr_issued_mul,           // 43: issued_mul_total
      tma_ctr_issued_div,           // 44: issued_div_total
      tma_ctr_flush_xcpt,           // 45: flush_xcpt_events
      tma_ctr_flush_eret,           // 46: flush_eret_events
      tma_ctr_flush_refetch,        // 47: flush_refetch_events
      tma_ctr_flush_next,           // 48: flush_next_events
      tma_ctr_dis_stall,            // 49: dis_stall_cycles
      tma_ctr_br_cond_mispredict,   // 50: br_cond_mispredict
      tma_ctr_br_indirect_mispredict, // 51: br_indirect_mispredict
      tma_ctr_br_ret_mispredict,    // 52: br_ret_mispredict
      tma_ctr_br_no_prediction,     // 53: br_no_prediction
      tma_ctr_fetch_bubble_raw,     // 54: fetch_bubble_raw
      tma_ctr_fetch_slots_delivered,// 55: fetch_slots_delivered
      tma_ctr_decode_backend_stall, // 56: decode_backend_stall
      tma_ctr_int_iq_empty,         // 57: int_iq_empty_cycles
      tma_ctr_mem_iq_empty,         // 58: mem_iq_empty_cycles
      tma_ctr_sfb_opt_events        // 59: sfb_opt_events
    ) ++ Seq(
      // Memory ordering counters (60-67)
      tma_ctr_stld_fwd_stall_cycles,   // 60
      tma_ctr_stld_fwd_success,        // 61
      tma_ctr_stld_fwd_wakeup_retries, // 62
      tma_ctr_stld_block_load_wakeup,  // 63
      tma_ctr_mem_order_failures,      // 64
      tma_ctr_load_ordering_failures,  // 65
      tma_ctr_load_spec_mispredict,    // 66
      tma_ctr_load_nack_retries        // 67
    ) ++ Seq(
      // Data dependency counters (68-74)
      tma_ctr_dep_stall_cycles,         // 68
      tma_ctr_operand_wait_slot_cycles, // 69
      tma_ctr_iq_dispatched_ready,      // 70
      tma_ctr_iq_dispatched_not_ready,  // 71
      tma_ctr_issued_with_poison,       // 72
      tma_ctr_ldspec_squash_grants,     // 73
      tma_ctr_spec_ld_wakeup_events     // 74
    ) ++ Seq.fill(BoomPerfCounterConsts.L2_INLINE_NUM_COUNTERS)(0.U(xLen.W)) ++ Seq(
      // OOO engine counters (92-98)
      tma_ctr_int_preg_stall,           // 92: int_preg_stall_cycles
      tma_ctr_fp_preg_stall,            // 93: fp_preg_stall_cycles
      tma_ctr_retire_width_0,           // 94: retire_width_0_cycles
      tma_ctr_retire_width_1,           // 95: retire_width_1_cycles
      tma_ctr_retire_width_2,           // 96: retire_width_2_cycles
      tma_ctr_retire_width_3,           // 97: retire_width_3_cycles
      tma_ctr_retire_width_4            // 98: retire_width_4_cycles
    ) ++ Seq(
      // Fetch/decode counters (99)
      tma_ctr_icache_lookups            // 99: icache_lookups (io.resp.valid || s2_miss; miss-rate denominator)
    ) ++ Seq(
      // L3 TMA counters (100-108): Intel-inspired BOOM-native observability counters
      tma_ctr_l1d_miss_pending,         // 100: l1d_miss_pending (cycles with demand L1D refill path active)
      tma_ctr_divider_active,           // 101: divider_active (cycles with any INT/FP divider busy)
      tma_ctr_no_issue,                 // 102: no_issue (cycles with zero uops issued)
      tma_ctr_issued_c1,                // 103: issued_c1 (cycles with >= 1 uop issued)
      tma_ctr_issued_c2,                // 104: issued_c2 (cycles with >= 2 uops issued)
      tma_ctr_issued_c3,                // 105: issued_c3 (cycles with >= 3 uops issued)
      tma_ctr_icache_stall,             // 106: icache_stall (cycles frontend stalled on I-cache miss)
      tma_ctr_itlb_stall,              // 107: itlb_stall (cycles frontend stalled on ITLB miss)
      tma_ctr_branch_mispredict_recovery // 108: branch_mispredict_recovery (mispredict-to-first-fetch cycles)
    ) ++ Seq(
      // L2 extra counter (appended to avoid shifting existing counter indices)
      0.U(xLen.W)                        // 109: l2_demand_miss_pending (overridden in tile.scala; cycles with demand Acquire outstanding below L2)
    )
    )
  } // end enableTMACounters

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Register Rename Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Inputs
  for (rename <- rename_stages) {
    rename.io.kill := io.ifu.redirect_flush
    rename.io.brupdate := brupdate

    rename.io.debug_rob_empty := rob.io.empty

    rename.io.dec_fire := dec_fire
    rename.io.dec_uops := dec_uops

    rename.io.dis_fire := dis_fire
    rename.io.dis_ready := dis_ready

    rename.io.com_valids := rob.io.commit.valids
    rename.io.com_uops := rob.io.commit.uops
    rename.io.rbk_valids := rob.io.commit.rbk_valids
    rename.io.rollback := rob.io.commit.rollback
  }


  // Outputs
  dis_uops := rename_stage.io.ren2_uops
  dis_valids := rename_stage.io.ren2_mask
  ren_stalls := rename_stage.io.ren_stalls


  /**
   * TODO This is a bit nasty, but it's currently necessary to
   * split the INT/FP rename pipelines into separate instantiations.
   * Won't have to do this anymore with a properly decoupled FP pipeline.
   */
  for (w <- 0 until coreWidth) {
    val i_uop   = rename_stage.io.ren2_uops(w)
    val f_uop   = if (usingFPU) fp_rename_stage.io.ren2_uops(w) else NullMicroOp
    val p_uop   = if (enableSFBOpt) pred_rename_stage.io.ren2_uops(w) else NullMicroOp
    val f_stall = if (usingFPU) fp_rename_stage.io.ren_stalls(w) else false.B
    val p_stall = if (enableSFBOpt) pred_rename_stage.io.ren_stalls(w) else false.B

    // lrs1 can "pass through" to prs1. Used solely to index the csr file.
    dis_uops(w).prs1 := Mux(dis_uops(w).lrs1_rtype === RT_FLT, f_uop.prs1,
                        Mux(dis_uops(w).lrs1_rtype === RT_FIX, i_uop.prs1, dis_uops(w).lrs1))
    dis_uops(w).prs2 := Mux(dis_uops(w).lrs2_rtype === RT_FLT, f_uop.prs2, i_uop.prs2)
    dis_uops(w).prs3 := f_uop.prs3
    dis_uops(w).ppred := p_uop.ppred
    dis_uops(w).pdst := Mux(dis_uops(w).dst_rtype  === RT_FLT, f_uop.pdst,
                        Mux(dis_uops(w).dst_rtype  === RT_FIX, i_uop.pdst,
                                                               p_uop.pdst))
    dis_uops(w).stale_pdst := Mux(dis_uops(w).dst_rtype === RT_FLT, f_uop.stale_pdst, i_uop.stale_pdst)

    dis_uops(w).prs1_busy := i_uop.prs1_busy && (dis_uops(w).lrs1_rtype === RT_FIX) ||
                             f_uop.prs1_busy && (dis_uops(w).lrs1_rtype === RT_FLT)
    dis_uops(w).prs2_busy := i_uop.prs2_busy && (dis_uops(w).lrs2_rtype === RT_FIX) ||
                             f_uop.prs2_busy && (dis_uops(w).lrs2_rtype === RT_FLT)
    dis_uops(w).prs3_busy := f_uop.prs3_busy && dis_uops(w).frs3_en
    dis_uops(w).ppred_busy := p_uop.ppred_busy && dis_uops(w).is_sfb_shadow

    ren_stalls(w) := rename_stage.io.ren_stalls(w) || f_stall || p_stall
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Dispatch Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  //-------------------------------------------------------------
  // Rename2/Dispatch pipeline logic

  val dis_prior_slot_valid = dis_valids.scanLeft(false.B) ((s,v) => s || v)
  val dis_prior_slot_unique = (dis_uops zip dis_valids).scanLeft(false.B) {case (s,(u,v)) => s || v && u.is_unique}
  val wait_for_empty_pipeline = (0 until coreWidth).map(w => (dis_uops(w).is_unique || custom_csrs.disableOOO) &&
                                  (!rob.io.empty || !io.lsu.fencei_rdy || dis_prior_slot_valid(w)))
  val rocc_shim_busy = if (usingRoCC) !exe_units.rocc_unit.io.rocc.rxq_empty else false.B
  val wait_for_rocc = (0 until coreWidth).map(w =>
                        (dis_uops(w).is_fence || dis_uops(w).is_fencei) && (io.rocc.busy || rocc_shim_busy))
  val rxq_full = if (usingRoCC) exe_units.rocc_unit.io.rocc.rxq_full else false.B
  val block_rocc = (dis_uops zip dis_valids).map{case (u,v) => v && u.uopc === uopROCC}.scanLeft(rxq_full)(_||_)
  val dis_rocc_alloc_stall = (dis_uops.map(_.uopc === uopROCC) zip block_rocc) map {case (p,r) =>
                               if (usingRoCC) p && r else false.B}

  val dis_hazards = (0 until coreWidth).map(w =>
                      dis_valids(w) &&
                      (  !rob.io.ready
                      || ren_stalls(w)
                      || io.lsu.ldq_full(w) && dis_uops(w).uses_ldq
                      || io.lsu.stq_full(w) && dis_uops(w).uses_stq
                      || !dispatcher.io.ren_uops(w).ready
                      || wait_for_empty_pipeline(w)
                      || wait_for_rocc(w)
                      || dis_prior_slot_unique(w)
                      || dis_rocc_alloc_stall(w)
                      || brupdate.b1.mispredict_mask =/= 0.U
                      || brupdate.b2.mispredict
                      || io.ifu.redirect_flush))




  io.lsu.fence_dmem := (dis_valids zip wait_for_empty_pipeline).map {case (v,w) => v && w} .reduce(_||_)

  val dis_stalls = dis_hazards.scanLeft(false.B) ((s,h) => s || h).takeRight(coreWidth)
  dis_fire := dis_valids zip dis_stalls map {case (v,s) => v && !s}
  dis_ready := !dis_stalls.last

  //-------------------------------------------------------------
  // LDQ/STQ Allocation Logic

  for (w <- 0 until coreWidth) {
    // Dispatching instructions request load/store queue entries when they can proceed.
    dis_uops(w).ldq_idx := io.lsu.dis_ldq_idx(w)
    dis_uops(w).stq_idx := io.lsu.dis_stq_idx(w)
  }

  //-------------------------------------------------------------
  // Rob Allocation Logic

  rob.io.enq_valids := dis_fire
  rob.io.enq_uops   := dis_uops
  rob.io.enq_partial_stall := dis_stalls.last // TODO come up with better ROB compacting scheme.
  rob.io.debug_tsc := debug_tsc_reg
  rob.io.csr_stall := csr.io.csr_stall

  // Minor hack: ecall and breaks need to increment the FTQ deq ptr earlier than commit, since
  // they write their PC into the CSR the cycle before they commit.
  // Since these are also unique, increment the FTQ ptr when they are dispatched
  when (RegNext(dis_fire.reduce(_||_) && dis_uops(PriorityEncoder(dis_fire)).is_sys_pc2epc)) {
    io.ifu.commit.valid := true.B
    io.ifu.commit.bits  := RegNext(dis_uops(PriorityEncoder(dis_valids)).ftq_idx)
  }

  for (w <- 0 until coreWidth) {
    // note: this assumes uops haven't been shifted - there's a 1:1 match between PC's LSBs and "w" here
    // (thus the LSB of the rob_idx gives part of the PC)
    if (coreWidth == 1) {
      dis_uops(w).rob_idx := rob.io.rob_tail_idx
    } else {
      dis_uops(w).rob_idx := Cat(rob.io.rob_tail_idx >> log2Ceil(coreWidth).U,
                               w.U(log2Ceil(coreWidth).W))
    }
  }

  //-------------------------------------------------------------
  // RoCC allocation logic
  if (usingRoCC) {
    for (w <- 0 until coreWidth) {
      // We guarantee only decoding 1 RoCC instruction per cycle
      dis_uops(w).rxq_idx := exe_units.rocc_unit.io.rocc.rxq_idx(w)
    }
  }

  //-------------------------------------------------------------
  // Dispatch to issue queues

  // Get uops from rename2
  for (w <- 0 until coreWidth) {
    dispatcher.io.ren_uops(w).valid := dis_fire(w)
    dispatcher.io.ren_uops(w).bits  := dis_uops(w)
  }

  var iu_idx = 0
  // Send dispatched uops to correct issue queues
  // Backpressure through dispatcher if necessary
  for (i <- 0 until issueParams.size) {
    if (issueParams(i).iqType == IQT_FP.litValue) {
       fp_pipeline.io.dis_uops <> dispatcher.io.dis_uops(i)
    } else {
       issue_units(iu_idx).io.dis_uops <> dispatcher.io.dis_uops(i)
       iu_idx += 1
    }
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Issue Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  require (issue_units.map(_.issueWidth).sum == exe_units.length)

  var iss_wu_idx = 1
  var ren_wu_idx = 1
  // The 0th wakeup port goes to the ll_wbarb
  int_iss_wakeups(0).valid := ll_wbarb.io.out.fire && ll_wbarb.io.out.bits.uop.dst_rtype === RT_FIX
  int_iss_wakeups(0).bits  := ll_wbarb.io.out.bits

  int_ren_wakeups(0).valid := ll_wbarb.io.out.fire && ll_wbarb.io.out.bits.uop.dst_rtype === RT_FIX
  int_ren_wakeups(0).bits  := ll_wbarb.io.out.bits

  for (i <- 1 until memWidth) {
    int_iss_wakeups(i).valid := mem_resps(i).valid && mem_resps(i).bits.uop.dst_rtype === RT_FIX
    int_iss_wakeups(i).bits  := mem_resps(i).bits

    int_ren_wakeups(i).valid := mem_resps(i).valid && mem_resps(i).bits.uop.dst_rtype === RT_FIX
    int_ren_wakeups(i).bits  := mem_resps(i).bits
    iss_wu_idx += 1
    ren_wu_idx += 1
  }

  // loop through each issue-port (exe_units are statically connected to an issue-port)
  for (i <- 0 until exe_units.length) {
    if (exe_units(i).writesIrf) {
      val fast_wakeup = Wire(Valid(new ExeUnitResp(xLen)))
      val slow_wakeup = Wire(Valid(new ExeUnitResp(xLen)))
      fast_wakeup := DontCare
      slow_wakeup := DontCare

      val resp = exe_units(i).io.iresp
      assert(!(resp.valid && resp.bits.uop.rf_wen && resp.bits.uop.dst_rtype =/= RT_FIX))

      // Fast Wakeup (uses just-issued uops that have known latencies)
      fast_wakeup.bits.uop := iss_uops(i)
      fast_wakeup.valid    := iss_valids(i) &&
                              iss_uops(i).bypassable &&
                              iss_uops(i).dst_rtype === RT_FIX &&
                              iss_uops(i).ldst_val &&
                              !(io.lsu.ld_miss && (iss_uops(i).iw_p1_poisoned || iss_uops(i).iw_p2_poisoned))

      // Slow Wakeup (uses write-port to register file)
      slow_wakeup.bits.uop := resp.bits.uop
      slow_wakeup.valid    := resp.valid &&
                                resp.bits.uop.rf_wen &&
                                !resp.bits.uop.bypassable &&
                                resp.bits.uop.dst_rtype === RT_FIX

      if (exe_units(i).bypassable) {
        int_iss_wakeups(iss_wu_idx) := fast_wakeup
        iss_wu_idx += 1
      }
      if (!exe_units(i).alwaysBypassable) {
        int_iss_wakeups(iss_wu_idx) := slow_wakeup
        iss_wu_idx += 1
      }

      if (exe_units(i).bypassable) {
        int_ren_wakeups(ren_wu_idx) := fast_wakeup
        ren_wu_idx += 1
      }
      if (!exe_units(i).alwaysBypassable) {
        int_ren_wakeups(ren_wu_idx) := slow_wakeup
        ren_wu_idx += 1
      }
    }
  }
  require (iss_wu_idx == numIntIssueWakeupPorts)
  require (ren_wu_idx == numIntRenameWakeupPorts)
  require (iss_wu_idx == ren_wu_idx)

  // jmp unit performs fast wakeup of the predicate bits
  require (jmp_unit.bypassable)
  pred_wakeup.valid := (iss_valids(jmp_unit_idx) &&
                        iss_uops(jmp_unit_idx).is_sfb_br &&
                        !(io.lsu.ld_miss && (iss_uops(jmp_unit_idx).iw_p1_poisoned || iss_uops(jmp_unit_idx).iw_p2_poisoned))
  )
  pred_wakeup.bits.uop := iss_uops(jmp_unit_idx)
  pred_wakeup.bits.fflags := DontCare
  pred_wakeup.bits.data := DontCare
  pred_wakeup.bits.predicated := DontCare

  // Perform load-hit speculative wakeup through a special port (performs a poison wake-up).
  issue_units map { iu =>
     iu.io.spec_ld_wakeup := io.lsu.spec_ld_wakeup
  }


  // Connect the predicate wakeup port
  issue_units map { iu =>
    iu.io.pred_wakeup_port.valid := false.B
    iu.io.pred_wakeup_port.bits := DontCare
  }
  if (enableSFBOpt) {
    int_iss_unit.io.pred_wakeup_port.valid := pred_wakeup.valid
    int_iss_unit.io.pred_wakeup_port.bits := pred_wakeup.bits.uop.pdst
  }


  // ----------------------------------------------------------------
  // Connect the wakeup ports to the busy tables in the rename stages

  for ((renport, intport) <- rename_stage.io.wakeups zip int_ren_wakeups) {
    renport <> intport
  }
  if (usingFPU) {
    for ((renport, fpport) <- fp_rename_stage.io.wakeups zip fp_pipeline.io.wakeups) {
       renport <> fpport
    }
  }
  if (enableSFBOpt) {
    pred_rename_stage.io.wakeups(0) := pred_wakeup
  } else {
    pred_rename_stage.io.wakeups := DontCare
  }

  // If we issue loads back-to-back endlessly (probably because we are executing some tight loop)
  // the store buffer will never drain, breaking the memory-model forward-progress guarantee
  // If we see a large number of loads saturate the LSU, pause for a cycle to let a store drain
  val loads_saturating = (mem_iss_unit.io.iss_valids(0) && mem_iss_unit.io.iss_uops(0).uses_ldq)
  val saturating_loads_counter = RegInit(0.U(5.W))
  when (loads_saturating) { saturating_loads_counter := saturating_loads_counter + 1.U }
  .otherwise { saturating_loads_counter := 0.U }
  val pause_mem = RegNext(loads_saturating) && saturating_loads_counter === ~(0.U(5.W))

  var iss_idx = 0
  var int_iss_cnt = 0
  var mem_iss_cnt = 0
  for (w <- 0 until exe_units.length) {
    var fu_types = exe_units(w).io.fu_types
    val exe_unit = exe_units(w)
    if (exe_unit.readsIrf) {
      if (exe_unit.supportedFuncUnits.muld) {
        // Supress just-issued divides from issuing back-to-back, since it's an iterative divider.
        // But it takes a cycle to get to the Exe stage, so it can't tell us it is busy yet.
        val idiv_issued = iss_valids(iss_idx) && iss_uops(iss_idx).fu_code_is(FU_DIV)
        fu_types = fu_types & RegNext(~Mux(idiv_issued, FU_DIV, 0.U))
      }

      if (exe_unit.hasMem) {
        iss_valids(iss_idx) := mem_iss_unit.io.iss_valids(mem_iss_cnt)
        iss_uops(iss_idx)   := mem_iss_unit.io.iss_uops(mem_iss_cnt)
        mem_iss_unit.io.fu_types(mem_iss_cnt) := Mux(pause_mem, 0.U, fu_types)
        mem_iss_cnt += 1
      } else {
        iss_valids(iss_idx) := int_iss_unit.io.iss_valids(int_iss_cnt)
        iss_uops(iss_idx)   := int_iss_unit.io.iss_uops(int_iss_cnt)
        int_iss_unit.io.fu_types(int_iss_cnt) := fu_types
        int_iss_cnt += 1
      }
      iss_idx += 1
    }
  }
  require(iss_idx == exe_units.numIrfReaders)

  issue_units.map(_.io.tsc_reg := debug_tsc_reg)
  issue_units.map(_.io.brupdate := brupdate)
  issue_units.map(_.io.flush_pipeline := RegNext(rob.io.flush.valid))

  // Load-hit Misspeculations
  require (mem_iss_unit.issueWidth <= 2)
  issue_units.map(_.io.ld_miss := io.lsu.ld_miss)

  mem_units.map(u => u.io.com_exception := RegNext(rob.io.flush.valid))

  // Wakeup (Issue & Writeback)
  for {
    iu <- issue_units
    (issport, wakeup) <- iu.io.wakeup_ports zip int_iss_wakeups
  }{
    issport.valid := wakeup.valid
    issport.bits.pdst := wakeup.bits.uop.pdst
    issport.bits.poisoned := wakeup.bits.uop.iw_p1_poisoned || wakeup.bits.uop.iw_p2_poisoned

    require (iu.io.wakeup_ports.length == int_iss_wakeups.length)
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Register Read Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Register Read <- Issue (rrd <- iss)
  iregister_read.io.rf_read_ports <> iregfile.io.read_ports
  iregister_read.io.prf_read_ports := DontCare
  if (enableSFBOpt) {
    iregister_read.io.prf_read_ports <> pregfile.io.read_ports
  }

  for (w <- 0 until exe_units.numIrfReaders) {
    iregister_read.io.iss_valids(w) :=
      iss_valids(w) && !(io.lsu.ld_miss && (iss_uops(w).iw_p1_poisoned || iss_uops(w).iw_p2_poisoned))
  }
  iregister_read.io.iss_uops := iss_uops
  iregister_read.io.iss_uops map { u => u.iw_p1_poisoned := false.B; u.iw_p2_poisoned := false.B }

  iregister_read.io.brupdate := brupdate
  iregister_read.io.kill   := RegNext(rob.io.flush.valid)

  iregister_read.io.bypass := bypasses
  iregister_read.io.pred_bypass := pred_bypasses

  //-------------------------------------------------------------
  // Privileged Co-processor 0 Register File
  // Note: Normally this would be bad in that I'm writing state before
  // committing, so to get this to work I stall the entire pipeline for
  // CSR instructions so I never speculate these instructions.

  val csr_exe_unit = exe_units.csr_unit

  // for critical path reasons, we aren't zero'ing this out if resp is not valid
  val csr_rw_cmd = csr_exe_unit.io.iresp.bits.uop.ctrl.csr_cmd
  val wb_wdata = csr_exe_unit.io.iresp.bits.data

  csr.io.rw.addr        := csr_exe_unit.io.iresp.bits.uop.csr_addr
  csr.io.rw.cmd         := freechips.rocketchip.rocket.CSR.maskCmd(csr_exe_unit.io.iresp.valid, csr_rw_cmd)
  csr.io.rw.wdata       := wb_wdata

  rob.io.csr_replay.valid := csr_exe_unit.io.iresp.valid && csr.io.rw_stall
  rob.io.csr_replay.bits.uop := csr_exe_unit.io.iresp.bits.uop
  rob.io.csr_replay.bits.cause := MINI_EXCEPTION_CSR_REPLAY
  rob.io.csr_replay.bits.badvaddr := DontCare

  // Extra I/O
  // Delay retire/exception 1 cycle
  csr.io.retire    := RegNext(PopCount(rob.io.commit.arch_valids.asUInt))
  csr.io.exception := RegNext(rob.io.com_xcpt.valid)
  // csr.io.pc used for setting EPC during exception or CSR.io.trace.

  csr.io.pc        := (boom.v3.util.AlignPCToBoundary(io.ifu.get_pc(0).com_pc, icBlockBytes)
                     + RegNext(rob.io.com_xcpt.bits.pc_lob)
                     - Mux(RegNext(rob.io.com_xcpt.bits.edge_inst), 2.U, 0.U))
  // Cause not valid for for CALL or BREAKPOINTs (CSRFile will override it).
  csr.io.cause     := RegNext(rob.io.com_xcpt.bits.cause)
  csr.io.ungated_clock := clock

  val tval_valid = csr.io.exception &&
    csr.io.cause.isOneOf(
      //Causes.illegal_instruction.U, we currently only write 0x0 for illegal instructions
      Causes.breakpoint.U,
      Causes.misaligned_load.U,
      Causes.misaligned_store.U,
      Causes.load_access.U,
      Causes.store_access.U,
      Causes.fetch_access.U,
      Causes.load_page_fault.U,
      Causes.store_page_fault.U,
      Causes.fetch_page_fault.U)

  csr.io.tval := Mux(tval_valid,
    RegNext(encodeVirtualAddress(rob.io.com_xcpt.bits.badvaddr, rob.io.com_xcpt.bits.badvaddr)), 0.U)

  // TODO move this function to some central location (since this is used elsewhere).
  def encodeVirtualAddress(a0: UInt, ea: UInt) =
    if (vaddrBitsExtended == vaddrBits) {
      ea
    } else {
      // Efficient means to compress 64-bit VA into vaddrBits+1 bits.
      // (VA is bad if VA(vaddrBits) != VA(vaddrBits-1)).
      val a = a0.asSInt >> vaddrBits
      val msb = Mux(a === 0.S || a === -1.S, ea(vaddrBits), !ea(vaddrBits-1))
      Cat(msb, ea(vaddrBits-1,0))
    }

  // reading requires serializing the entire pipeline
  csr.io.fcsr_flags.valid := rob.io.commit.fflags.valid
  csr.io.fcsr_flags.bits  := rob.io.commit.fflags.bits
  csr.io.set_fs_dirty.get := rob.io.commit.fflags.valid

  exe_units.withFilter(_.hasFcsr).map(_.io.fcsr_rm := csr.io.fcsr_rm)
  io.fcsr_rm := csr.io.fcsr_rm

  if (usingFPU) {
    fp_pipeline.io.fcsr_rm := csr.io.fcsr_rm
  }

  csr.io.hartid := io.hartid
  csr.io.interrupts := io.interrupts

  // we do not support the H-extension
  csr.io.htval := DontCare
  csr.io.gva := DontCare

// TODO can we add this back in, but handle reset properly and save us
//      the mux above on csr.io.rw.cmd?
//   assert (!(csr_rw_cmd =/= rocket.CSR.N && !exe_units(0).io.resp(0).valid),
//   "CSRFile is being written to spuriously.")

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Execute Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  iss_idx = 0
  var bypass_idx = 0
  for (w <- 0 until exe_units.length) {
    val exe_unit = exe_units(w)
    if (exe_unit.readsIrf) {
      exe_unit.io.req <> iregister_read.io.exe_reqs(iss_idx)

      if (exe_unit.bypassable) {
        for (i <- 0 until exe_unit.numBypassStages) {
          bypasses(bypass_idx) := exe_unit.io.bypass(i)
          bypass_idx += 1
        }
      }
      iss_idx += 1
    }
  }
  require (bypass_idx == exe_units.numTotalBypassPorts)
  for (i <- 0 until jmp_unit.numBypassStages) {
    pred_bypasses(i) := jmp_unit.io.bypass(i)
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Load/Store Unit ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // enqueue basic load/store info in Decode
  for (w <- 0 until coreWidth) {
    io.lsu.dis_uops(w).valid := dis_fire(w)
    io.lsu.dis_uops(w).bits  := dis_uops(w)
  }

  // tell LSU about committing loads and stores to clear entries
  io.lsu.commit                  := rob.io.commit

  // tell LSU that it should fire a load that waits for the rob to clear
  io.lsu.commit_load_at_rob_head := rob.io.com_load_is_at_rob_head

  //com_xcpt.valid comes too early, will fight against a branch that resolves same cycle as an exception
  io.lsu.exception := RegNext(rob.io.flush.valid)

  // Handle Branch Mispeculations
  io.lsu.brupdate := brupdate
  io.lsu.rob_head_idx := rob.io.rob_head_idx
  io.lsu.rob_pnr_idx  := rob.io.rob_pnr_idx

  io.lsu.tsc_reg := debug_tsc_reg


  if (usingFPU) {
    io.lsu.fp_stdata <> fp_pipeline.io.to_sdq
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Writeback Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  var w_cnt = 1
  iregfile.io.write_ports(0) := WritePort(ll_wbarb.io.out, ipregSz, xLen, RT_FIX)
  ll_wbarb.io.in(0) <> mem_resps(0)
  assert (ll_wbarb.io.in(0).ready) // never backpressure the memory unit.
  for (i <- 1 until memWidth) {
    iregfile.io.write_ports(w_cnt) := WritePort(mem_resps(i), ipregSz, xLen, RT_FIX)
    w_cnt += 1
  }

  for (i <- 0 until exe_units.length) {
    if (exe_units(i).writesIrf) {
      val wbresp = exe_units(i).io.iresp
      val wbpdst = wbresp.bits.uop.pdst
      val wbdata = wbresp.bits.data

      def wbIsValid(rtype: UInt) =
        wbresp.valid && wbresp.bits.uop.rf_wen && wbresp.bits.uop.dst_rtype === rtype
      val wbReadsCSR = wbresp.bits.uop.ctrl.csr_cmd =/= freechips.rocketchip.rocket.CSR.N

      iregfile.io.write_ports(w_cnt).valid     := wbIsValid(RT_FIX)
      iregfile.io.write_ports(w_cnt).bits.addr := wbpdst
      wbresp.ready := true.B
      if (exe_units(i).hasCSR) {
        iregfile.io.write_ports(w_cnt).bits.data := Mux(wbReadsCSR, csr.io.rw.rdata, wbdata)
      } else {
        iregfile.io.write_ports(w_cnt).bits.data := wbdata
      }

      assert (!wbIsValid(RT_FLT), "[fppipeline] An FP writeback is being attempted to the Int Regfile.")

      assert (!(wbresp.valid &&
        !wbresp.bits.uop.rf_wen &&
        wbresp.bits.uop.dst_rtype === RT_FIX),
        "[fppipeline] An Int writeback is being attempted with rf_wen disabled.")

      assert (!(wbresp.valid &&
        wbresp.bits.uop.rf_wen &&
        wbresp.bits.uop.dst_rtype =/= RT_FIX),
        "[fppipeline] writeback being attempted to Int RF with dst != Int type exe_units("+i+").iresp")
      w_cnt += 1
    }
  }
  require(w_cnt == iregfile.io.write_ports.length)

  if (enableSFBOpt) {
    pregfile.io.write_ports(0).valid     := jmp_unit.io.iresp.valid && jmp_unit.io.iresp.bits.uop.is_sfb_br
    pregfile.io.write_ports(0).bits.addr := jmp_unit.io.iresp.bits.uop.pdst
    pregfile.io.write_ports(0).bits.data := jmp_unit.io.iresp.bits.data
  }

  if (usingFPU) {
    // Connect IFPU
    fp_pipeline.io.from_int  <> exe_units.ifpu_unit.io.ll_fresp
    // Connect FPIU
    ll_wbarb.io.in(1)        <> fp_pipeline.io.to_int
    // Connect FLDs
    fp_pipeline.io.ll_wports <> exe_units.memory_units.map(_.io.ll_fresp).toSeq
  }
  if (usingRoCC) {
    require(usingFPU)
    ll_wbarb.io.in(2)       <> exe_units.rocc_unit.io.ll_iresp
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Commit Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Writeback
  // ---------
  // First connect the ll_wport
  val ll_uop = ll_wbarb.io.out.bits.uop
  rob.io.wb_resps(0).valid  := ll_wbarb.io.out.valid && !(ll_uop.uses_stq && !ll_uop.is_amo)
  rob.io.wb_resps(0).bits   <> ll_wbarb.io.out.bits
  rob.io.debug_wb_valids(0) := ll_wbarb.io.out.valid && ll_uop.dst_rtype =/= RT_X
  rob.io.debug_wb_wdata(0)  := ll_wbarb.io.out.bits.data
  var cnt = 1
  for (i <- 1 until memWidth) {
    val mem_uop = mem_resps(i).bits.uop
    rob.io.wb_resps(cnt).valid := mem_resps(i).valid && !(mem_uop.uses_stq && !mem_uop.is_amo)
    rob.io.wb_resps(cnt).bits  := mem_resps(i).bits
    rob.io.debug_wb_valids(cnt) := mem_resps(i).valid && mem_uop.dst_rtype =/= RT_X
    rob.io.debug_wb_wdata(cnt)  := mem_resps(i).bits.data
    cnt += 1
  }
  var f_cnt = 0 // rob fflags port index
  for (eu <- exe_units) {
    if (eu.writesIrf)
    {
      val resp   = eu.io.iresp
      val wb_uop = resp.bits.uop
      val data   = resp.bits.data

      rob.io.wb_resps(cnt).valid := resp.valid && !(wb_uop.uses_stq && !wb_uop.is_amo)
      rob.io.wb_resps(cnt).bits  <> resp.bits
      rob.io.debug_wb_valids(cnt) := resp.valid && wb_uop.rf_wen && wb_uop.dst_rtype === RT_FIX
      if (eu.hasFFlags) {
        rob.io.fflags(f_cnt) <> resp.bits.fflags
        f_cnt += 1
      }
      if (eu.hasCSR) {
        rob.io.debug_wb_wdata(cnt) := Mux(wb_uop.ctrl.csr_cmd =/= freechips.rocketchip.rocket.CSR.N,
          csr.io.rw.rdata,
          data)
      } else {
        rob.io.debug_wb_wdata(cnt) := data
      }
      cnt += 1
    }
  }

  require(cnt == numIrfWritePorts)
  if (usingFPU) {
    for ((wdata, wakeup) <- fp_pipeline.io.debug_wb_wdata zip fp_pipeline.io.wakeups) {
      rob.io.wb_resps(cnt) <> wakeup
      rob.io.fflags(f_cnt) <> wakeup.bits.fflags
      rob.io.debug_wb_valids(cnt) := wakeup.valid
      rob.io.debug_wb_wdata(cnt) := wdata
      cnt += 1
      f_cnt += 1

      assert (!(wakeup.valid && wakeup.bits.uop.dst_rtype =/= RT_FLT),
        "[core] FP wakeup does not write back to a FP register.")

      assert (!(wakeup.valid && !wakeup.bits.uop.fp_val),
        "[core] FP wakeup does not involve an FP instruction.")
    }
  }

  require (cnt == rob.numWakeupPorts)
  require (f_cnt == rob.numFpuPorts)

  // branch resolution
  rob.io.brupdate <> brupdate

  exe_units.map(u => u.io.status := csr.io.status)
  if (usingFPU)
    fp_pipeline.io.status := csr.io.status

  // Connect breakpoint info to memaddrcalcunit
  for (i <- 0 until memWidth) {
    mem_units(i).io.status   := csr.io.status
    mem_units(i).io.bp       := csr.io.bp
    mem_units(i).io.mcontext := csr.io.mcontext
    mem_units(i).io.scontext := csr.io.scontext
  }

  // LSU <> ROB
  rob.io.lsu_clr_bsy    := io.lsu.clr_bsy
  rob.io.lsu_clr_unsafe := io.lsu.clr_unsafe
  rob.io.lxcpt          <> io.lsu.lxcpt

  assert (!(csr.io.singleStep), "[core] single-step is unsupported.")


  //-------------------------------------------------------------
  // **** Flush Pipeline ****
  //-------------------------------------------------------------
  // flush on exceptions, miniexeptions, and after some special instructions

  if (usingFPU) {
    fp_pipeline.io.flush_pipeline := RegNext(rob.io.flush.valid)
  }

  for (w <- 0 until exe_units.length) {
    exe_units(w).io.req.bits.kill := RegNext(rob.io.flush.valid)
  }

  assert (!(rob.io.com_xcpt.valid && !rob.io.flush.valid),
    "[core] exception occurred, but pipeline flush signal not set!")

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Outputs to the External World ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // detect pipeline freezes and throw error
  val idle_cycles = freechips.rocketchip.util.WideCounter(32)
  when (rob.io.commit.valids.asUInt.orR ||
        csr.io.csr_stall ||
        io.rocc.busy ||
        reset.asBool) {
    idle_cycles := 0.U
  }
  assert (!(idle_cycles.value(13)), "Pipeline has hung.")

  if (usingFPU) {
    fp_pipeline.io.debug_tsc_reg := debug_tsc_reg
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Handle Cycle-by-Cycle Printouts ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------


  if (COMMIT_LOG_PRINTF) {
    var new_commit_cnt = 0.U

    for (w <- 0 until coreWidth) {
      val priv = RegNext(csr.io.status.prv) // erets change the privilege. Get the old one

      // To allow for diffs against spike :/
      def printf_inst(uop: MicroOp) = {
        when (uop.is_rvc) {
          printf("(0x%x)", uop.debug_inst(15,0))
          if (COMMIT_LOG_HUMAN_READABLE) {
            printf(" DASM(%x)", uop.debug_inst(15,0))
          }
        } .otherwise {
          printf("(0x%x)", uop.debug_inst)
          if (COMMIT_LOG_HUMAN_READABLE) {
            printf(" DASM(%x)", uop.debug_inst)
          }
        }
      }

      when (rob.io.commit.arch_valids(w)) {
        if (COMMIT_LOG_HUMAN_READABLE) {
          printf("C%d: ", debug_tsc_reg)
        }
        printf("%d 0x%x ",
          priv,
          Sext(rob.io.commit.uops(w).debug_pc(vaddrBits-1,0), xLen))
        printf_inst(rob.io.commit.uops(w))
        when (rob.io.commit.uops(w).dst_rtype === RT_FIX && rob.io.commit.uops(w).ldst =/= 0.U) {
          printf(" x%d 0x%x\n",
            rob.io.commit.uops(w).ldst,
            rob.io.commit.debug_wdata(w))
        } .elsewhen (rob.io.commit.uops(w).dst_rtype === RT_FLT) {
          printf(" f%d 0x%x\n",
            rob.io.commit.uops(w).ldst,
            rob.io.commit.debug_wdata(w))
        } .otherwise {
          printf("\n")
        }
      }
    }
  } else if (BRANCH_PRINTF) {
    val debug_ghist = RegInit(0.U(globalHistoryLength.W))
    when (rob.io.flush.valid && FlushTypes.useCsrEvec(rob.io.flush.bits.flush_typ)) {
      debug_ghist := 0.U
    }

    var new_ghist = debug_ghist

    for (w <- 0 until coreWidth) {
      when (rob.io.commit.arch_valids(w) &&
        (rob.io.commit.uops(w).is_br || rob.io.commit.uops(w).is_jal || rob.io.commit.uops(w).is_jalr)) {
        // for (i <- 0 until globalHistoryLength) {
        //   printf("%x", new_ghist(globalHistoryLength-i-1))
        // }
        // printf("\n")
        printf("%x %x %x %x %x %x\n",
          rob.io.commit.uops(w).debug_fsrc, rob.io.commit.uops(w).taken,
          rob.io.commit.uops(w).is_br, rob.io.commit.uops(w).is_jal,
          rob.io.commit.uops(w).is_jalr, Sext(rob.io.commit.uops(w).debug_pc(vaddrBits-1,0), xLen))

      }
      new_ghist = Mux(rob.io.commit.arch_valids(w) && rob.io.commit.uops(w).is_br,
        Mux(rob.io.commit.uops(w).taken, new_ghist << 1 | 1.U(1.W), new_ghist << 1),
        new_ghist)
    }
    debug_ghist := new_ghist
  }

  // TODO: Does anyone want this debugging functionality?
  val coreMonitorBundle = Wire(new CoreMonitorBundle(xLen, fLen))
  coreMonitorBundle := DontCare
  coreMonitorBundle.clock  := clock
  coreMonitorBundle.reset  := reset


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Page Table Walker

  io.ptw.ptbr       := csr.io.ptbr
  io.ptw.status     := csr.io.status
  io.ptw.pmp        := csr.io.pmp
  io.ptw.sfence     := io.ifu.sfence

  //-------------------------------------------------------------
  //-------------------------------------------------------------

  io.rocc := DontCare
  io.rocc.exception := csr.io.exception && csr.io.status.xs.orR
  io.rocc.csrs <> csr.io.roccCSRs
  if (usingRoCC) {
    exe_units.rocc_unit.io.rocc.rocc         <> io.rocc
    exe_units.rocc_unit.io.rocc.dis_uops     := dis_uops
    exe_units.rocc_unit.io.rocc.rob_head_idx := rob.io.rob_head_idx
    exe_units.rocc_unit.io.rocc.rob_pnr_idx  := rob.io.rob_pnr_idx
    exe_units.rocc_unit.io.com_exception     := rob.io.flush.valid
    exe_units.rocc_unit.io.status            := csr.io.status

    for (w <- 0 until coreWidth) {
      exe_units.rocc_unit.io.rocc.dis_rocc_vals(w) := (
        dis_fire(w) &&
        dis_uops(w).uopc === uopROCC &&
        !dis_uops(w).exception
      )
    }
  }

  io.trace := DontCare
  io.trace.time := csr.io.time
  io.trace.insns map (t => t.valid := false.B)
  io.trace.custom.get.asInstanceOf[BoomTraceBundle].rob_empty := rob.io.empty

  if (trace) {
    for (w <- 0 until coreWidth) {
      // Delay the trace so we have a cycle to pull PCs out of the FTQ
      io.trace.insns(w).valid      := RegNext(rob.io.commit.arch_valids(w))

      // Recalculate the PC
      io.ifu.debug_ftq_idx(w) := rob.io.commit.uops(w).ftq_idx
      val iaddr = (AlignPCToBoundary(io.ifu.debug_fetch_pc(w), icBlockBytes)
                   + RegNext(rob.io.commit.uops(w).pc_lob)
                   - Mux(RegNext(rob.io.commit.uops(w).edge_inst), 2.U, 0.U))(vaddrBits-1,0)
      io.trace.insns(w).iaddr      := Sext(iaddr, xLen)

      def getInst(uop: MicroOp, inst: UInt): UInt = {
        Mux(uop.is_rvc, Cat(0.U(16.W), inst(15,0)), inst)
      }

      def getWdata(uop: MicroOp, wdata: UInt): UInt = {
        Mux((uop.dst_rtype === RT_FIX && uop.ldst =/= 0.U) || (uop.dst_rtype === RT_FLT), wdata, 0.U(xLen.W))
      }

      // use debug_insts instead of uop.debug_inst to use the rob's debug_inst_mem
      // note: rob.debug_insts comes 1 cycle later
      io.trace.insns(w).insn       := getInst(RegNext(rob.io.commit.uops(w)), rob.io.commit.debug_insts(w))
      io.trace.insns(w).wdata.map { _ := RegNext(getWdata(rob.io.commit.uops(w), rob.io.commit.debug_wdata(w))) }

      // Comment out this assert because it blows up FPGA synth-asserts
      // This tests correctedness of the debug_inst mem
      // when (RegNext(rob.io.commit.valids(w))) {
      //   assert(rob.io.commit.debug_insts(w) === RegNext(rob.io.commit.uops(w).debug_inst))
      // }
      // This tests correctedness of recovering pcs through ftq debug ports
      // when (RegNext(rob.io.commit.valids(w))) {
      //   assert(Sext(io.trace.insns(w).iaddr, xLen) ===
      //     RegNext(Sext(rob.io.commit.uops(w).debug_pc(vaddrBits-1,0), xLen)))
      // }

      // These csr signals do not exactly match up with the ROB commit signals.
      io.trace.insns(w).priv       := RegNext(Cat(RegNext(csr.io.status.debug), csr.io.status.prv))
      // Can determine if it is an interrupt or not based on the MSB of the cause
      io.trace.insns(w).exception  := RegNext(rob.io.com_xcpt.valid && !rob.io.com_xcpt.bits.cause(xLen - 1)) && (w == 0).B
      io.trace.insns(w).interrupt  := RegNext(rob.io.com_xcpt.valid && rob.io.com_xcpt.bits.cause(xLen - 1)) && (w == 0).B
      io.trace.insns(w).cause      := RegNext(rob.io.com_xcpt.bits.cause)
      io.trace.insns(w).tval       := RegNext(csr.io.tval)
    }
    dontTouch(io.trace)
  } else {
    io.ifu.debug_ftq_idx := DontCare
  }
}
