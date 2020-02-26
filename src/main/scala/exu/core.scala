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

package boom.exu

import java.nio.file.{Paths}

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.rocket.{Causes, PRV}
import freechips.rocketchip.util.{Str, UIntIsOneOf, CoreMonitorBundle}
import freechips.rocketchip.devices.tilelink.{PLICConsts, CLINTConsts}

import boom.common._
import boom.ifu.{GlobalHistory, HasBoomFrontendParameters}
import boom.exu.FUConstants._
import boom.common.BoomTilesKey
import boom.util._

/**
 * Top level core object that connects the Frontend to the rest of the pipeline.
 */
class BoomCore(implicit p: Parameters) extends BoomModule
  with HasBoomFrontendParameters // TODO: Don't add this trait
{
  val io = new freechips.rocketchip.tile.CoreBundle
    with freechips.rocketchip.tile.HasExternallyDrivenTileConstants
  {
    val interrupts = Input(new freechips.rocketchip.tile.CoreInterrupts())
    val ifu = new boom.ifu.BoomFrontendIO
    val ptw = Flipped(new freechips.rocketchip.rocket.DatapathPTWIO())
    val rocc = Flipped(new freechips.rocketchip.tile.RoCCCoreIO())
    val lsu = Flipped(new boom.lsu.LSUCoreIO)
    val ptw_tlb = new freechips.rocketchip.rocket.TLBPTWIO()
    val trace = Output(Vec(coreParams.retireWidth,
      new freechips.rocketchip.rocket.TracedInstruction))
    val fcsr_rm = UInt(freechips.rocketchip.tile.FPConstants.RM_SZ.W)
  }
  //**********************************
  // construct all of the modules

  // Only holds integer-registerfile execution units.
  val exe_units = Module(new boom.exu.RingExecutionUnits)

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

  val numFpWakeupPorts   = if (usingFPU) fp_pipeline.io.wakeups.length else 0

  val decode_units     = for (w <- 0 until decodeWidth) yield { val d = Module(new DecodeUnit); d }
  val dec_brmask_logic = Module(new BranchMaskGenerationLogic(coreWidth))
  val rename_stage     = Module(new RingRename)
  val fp_rename_stage  = if (usingFPU) Module(new RenameStage(coreWidth, numFpPhysRegs, numFpWakeupPorts, true))
                         else null
  val rename_stages    = if (usingFPU) Seq(rename_stage, fp_rename_stage) else Seq(rename_stage)

  val scheduler        = Module(new RingScheduler(intIssueParam.numEntries, intIssueParam.dispatchWidth))

  val dispatcher       = Module(new BasicDispatcher)

  val iregfile         = Module(new BankedRegisterFileSynthesizable(numIntPhysRegs, xLen))

  val iregister_read   = Module(new RingRegisterRead)
  val rob              = Module(new Rob(
                           coreWidth * 2 + numFpWakeupPorts,
                           numFpWakeupPorts))

  val wakeups          = Wire(Vec(coreWidth*2, Valid(UInt(ipregSz.W)))) // 'Slow' wakeups
  wakeups := DontCare

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

  // Rename2/Dispatch stage
  val dis_valids = Wire(Vec(coreWidth, Bool()))
  val dis_uops   = Wire(Vec(coreWidth, new MicroOp))
  val dis_fire   = Wire(Vec(coreWidth, Bool()))
  val dis_ready  = Wire(Bool())

  // Issue Stage/Register Read
  val iss_valids = Wire(Vec(coreWidth, Bool()))
  val iss_uops   = Wire(Vec(coreWidth, new MicroOp()))

  // --------------------------------------
  // Dealing with branch resolutions

  // The individual branch resolutions from each ALU
  val brinfos = Reg(Vec(coreWidth+1, new BrResolutionInfo))

  // "Merged" branch update info from all ALUs
  // brmask contains masks for rapidly clearing mispredicted instructions
  // brindices contains indices to reset pointers for allocated structures
  //           brindices is delayed a cycle
  val brupdate  = Wire(new BrUpdateInfo)
  val b1        = Wire(new BrUpdateMasks)
  val b2        = Reg(new BrResolutionInfo)

  brupdate.b1 := b1
  brupdate.b2 := b2

  for (w <- 0 until coreWidth) {
    brinfos(w)       := exe_units.io.brinfos(w)
    brinfos(w).valid := exe_units.io.brinfos(w).valid && !rob.io.flush.valid
  }
  brinfos(coreWidth)       := exe_units.io.jmp_brinfo
  brinfos(coreWidth).valid := exe_units.io.jmp_brinfo.valid && !rob.io.flush.valid

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
  b2.jalr_target := RegNext(exe_units.io.jmp_brinfo.jalr_target)
  b2.target_offset := oldest_mispredict.target_offset

  val oldest_mispredict_ftq_idx = oldest_mispredict.uop.ftq_idx


  assert (!((brupdate.b1.mispredict_mask =/= 0.U || brupdate.b2.mispredict)
    && rob.io.commit.rollback), "Can't have a mispredict during rollback.")

  io.ifu.brupdate := brupdate

  if (usingFPU) {
    fp_pipeline.io.brupdate := brupdate
  }

  // Load/Store Unit <-> ExeUnits
  for (w <- 0 until memWidth) {
    exe_units.io.lsu_io(w) <> io.lsu.exe(w)
  }

  //-------------------------------------------------------------
  // Uarch Hardware Performance Events (HPEs)

  val perfEvents = new freechips.rocketchip.rocket.EventSets(Seq(
    new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
      ("exception", () => rob.io.com_xcpt.valid),
      ("nop",       () => false.B),
      ("nop",       () => false.B),
      ("nop",       () => false.B))),

    new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
//      ("I$ blocked",                        () => icache_blocked),
      ("nop",                               () => false.B),
      // ("branch misprediction",              () => br_unit.brinfo.mispredict),
      // ("control-flow target misprediction", () => br_unit.brinfo.mispredict &&
      //                                             br_unit.brinfo.cfi_type === CFI_JALR),
      ("flush",                             () => rob.io.flush.valid)
      //("branch resolved",                   () => br_unit.brinfo.valid)
    )),

    new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
//      ("I$ miss",     () => io.ifu.perf.acquire),
//      ("D$ miss",     () => io.dmem.perf.acquire),
//      ("D$ release",  () => io.dmem.perf.release),
//      ("ITLB miss",   () => io.ifu.perf.tlbMiss),
//      ("DTLB miss",   () => io.dmem.perf.tlbMiss),
      ("L2 TLB miss", () => io.ptw.perf.l2miss)))))
  val csr = Module(new freechips.rocketchip.rocket.CSRFile(perfEvents, boomParams.customCSRs.decls))
  csr.io.inst foreach { c => c := DontCare }
  csr.io.rocc_interrupt := io.rocc.interrupt

  val custom_csrs = Wire(new BoomCustomCSRs)
  (custom_csrs.csrs zip csr.io.customCSRs).map { case (lhs, rhs) => lhs := rhs }

  //val icache_blocked = !(io.ifu.fetchpacket.valid || RegNext(io.ifu.fetchpacket.valid))
  val icache_blocked = false.B
  csr.io.counters foreach { c => c.inc := RegNext(perfEvents.evaluate(c.eventSel)) }

  //****************************************
  // Time Stamp Counter & Retired Instruction Counter
  // (only used for printf and vcd dumps - the actual counters are in the CSRFile)
  val debug_tsc_reg = RegInit(0.U(xLen.W))
  val debug_irt_reg = RegInit(0.U(xLen.W))
  val debug_cfis = RegInit(0.U(xLen.W))
  val debug_f3_mispredicts = RegInit(0.U(xLen.W)) // How many cfis were missed by the l3 predictor
  val debug_f2_mispredicts = RegInit(0.U(xLen.W)) // How many cfis were missed by the l2 predictor
  val debug_f1_mispredicts = RegInit(0.U(xLen.W)) // How many cfis were missed by the l1 predictor
  dontTouch(debug_f3_mispredicts)
  dontTouch(debug_f2_mispredicts)
  dontTouch(debug_f1_mispredicts)
  dontTouch(debug_cfis)
  debug_cfis := debug_cfis + PopCount(VecInit((0 until coreWidth) map {i =>
    rob.io.commit.valids(i) &&
    (rob.io.commit.uops(i).is_br || rob.io.commit.uops(i).is_jal || rob.io.commit.uops(i).is_jalr)
  }))
  debug_f3_mispredicts := debug_f3_mispredicts + PopCount(VecInit((0 until coreWidth) map { i =>
    rob.io.commit.valids(i) &&
    (rob.io.commit.uops(i).is_br || rob.io.commit.uops(i).is_jal || rob.io.commit.uops(i).is_jalr) &&
    rob.io.commit.uops(i).debug_fsrc > BSRC_3
  }))
  debug_f2_mispredicts := debug_f2_mispredicts + PopCount(VecInit((0 until coreWidth) map { i =>
    rob.io.commit.valids(i) &&
    (rob.io.commit.uops(i).is_br || rob.io.commit.uops(i).is_jal || rob.io.commit.uops(i).is_jalr) &&
    rob.io.commit.uops(i).debug_fsrc > BSRC_2
  }))
  debug_f1_mispredicts := debug_f1_mispredicts + PopCount(VecInit((0 until coreWidth) map { i =>
    rob.io.commit.valids(i) &&
    (rob.io.commit.uops(i).is_br || rob.io.commit.uops(i).is_jal || rob.io.commit.uops(i).is_jalr) &&
    rob.io.commit.uops(i).debug_fsrc > BSRC_1
  }))

  debug_tsc_reg := debug_tsc_reg + 1.U
  debug_irt_reg := debug_irt_reg + PopCount(rob.io.commit.valids.asUInt)
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
        "Num Slow Wakeup Ports : " + coreWidth,
        "Num Fast Wakeup Ports : " + coreWidth,
        "Num Bypass Ports      : " + coreWidth) + "\n"
    + BoomCoreStringPrefix(
        "DCache Ways           : " + dcacheParams.nWays,
        "DCache Sets           : " + dcacheParams.nSets,
        "DCache nMSHRs         : " + dcacheParams.nMSHRs,
        "ICache Ways           : " + icacheParams.nWays,
        "ICache Sets           : " + icacheParams.nSets,
        "D-TLB Entries         : " + dcacheParams.nTLBEntries,
        "I-TLB Entries         : " + icacheParams.nTLBEntries,
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

  io.ifu.flush_icache := (0 until coreWidth).map { i =>
    rob.io.commit.valids(i) && rob.io.commit.uops(i).is_fencei }.reduce(_||_)

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
    io.ifu.redirect_ghist := io.ifu.get_pc(0).entry.ghist
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
    val next_ghist = ftq_entry.ghist.update(
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
      ftq_entry.ghist,
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
  // but on the next cycle, we only want to retry a subset
  val dec_finished_mask = RegInit(0.U(coreWidth.W))

  //-------------------------------------------------------------
  // Pull out instructions and send to the Decoders

  io.ifu.fetchpacket.ready := dec_ready
  val dec_fbundle = io.ifu.fetchpacket.bits

  //-------------------------------------------------------------
  // Decoders

  // stall fetch/dcode because we ran out of branch tags
  val branch_mask_full = Wire(Vec(coreWidth, Bool()))

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

  // Jump Unit Requests
  val iss_jmp = iss_uops zip iss_valids map { case (u,v) => u.fu_code === FU_JMP && v }
  jmp_pc_req.valid                 := RegNext(iss_jmp.reduce(_||_))
  jmp_pc_req.bits                  := RegNext(Mux1H(iss_jmp, iss_uops.map(_.ftq_idx)))
  exe_units.io.get_ftq_pc.pc       := io.ifu.get_pc(0).pc
  exe_units.io.get_ftq_pc.entry    := io.ifu.get_pc(0).entry
  exe_units.io.get_ftq_pc.com_pc   := DontCare
  exe_units.io.get_ftq_pc.next_val := io.ifu.get_pc(0).next_val
  exe_units.io.get_ftq_pc.next_pc  := io.ifu.get_pc(0).next_pc

  // Frontend Exception Requests
  val xcpt_idx = PriorityEncoder(dec_xcpts)
  xcpt_pc_req.valid    := dec_xcpts.reduce(_||_)
  xcpt_pc_req.bits     := dec_uops(xcpt_idx).ftq_idx
  rob.io.xcpt_fetch_pc := io.ifu.get_pc(0).pc

  flush_pc_req.valid   := rob.io.flush.valid
  flush_pc_req.bits    := rob.io.flush.bits.ftq_idx

  // Mispredict requests (to get the correct target)
  io.ifu.get_pc(1).ftq_idx := oldest_mispredict_ftq_idx


  //-------------------------------------------------------------
  // Decode/Rename1 pipeline logic

  dec_xcpts := dec_uops zip dec_valids map {case (u,v) => u.exception && v}
  val dec_xcpt_stall = dec_xcpts.reduce(_||_) && !xcpt_pc_req.ready

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
  } .otherwise {
    dec_finished_mask := dec_fire.asUInt | dec_finished_mask
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

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Register Rename Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Inputs
  rename_stage.io.kill := io.ifu.redirect_flush
  rename_stage.io.brupdate := brupdate

  rename_stage.io.debug_rob_empty := rob.io.empty

  rename_stage.io.dec_fire := dec_fire
  rename_stage.io.dec_uops := dec_uops

  rename_stage.io.dis_fire := dis_fire
  rename_stage.io.dis_ready := dis_ready

  rename_stage.io.com_valids := rob.io.commit.valids
  rename_stage.io.com_uops := rob.io.commit.uops
  rename_stage.io.rbk_valids := rob.io.commit.rbk_valids
  rename_stage.io.rollback := rob.io.commit.rollback

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
    val f_stall = if (usingFPU) fp_rename_stage.io.ren_stalls(w) else false.B

    // lrs1 can "pass through" to prs1. Used solely to index the csr file.
    dis_uops(w).prs1 := Mux(dis_uops(w).lrs1_rtype === RT_FLT, f_uop.prs1,
                        Mux(dis_uops(w).lrs1_rtype === RT_FIX, i_uop.prs1, dis_uops(w).lrs1))
    dis_uops(w).prs2 := Mux(dis_uops(w).lrs2_rtype === RT_FLT, f_uop.prs2, i_uop.prs2)
    dis_uops(w).prs3 := f_uop.prs3
    dis_uops(w).pdst := Mux(dis_uops(w).dst_rtype  === RT_FLT, f_uop.pdst, i_uop.pdst)
    dis_uops(w).stale_pdst := Mux(dis_uops(w).dst_rtype === RT_FLT, f_uop.stale_pdst, i_uop.stale_pdst)

    dis_uops(w).prs1_busy := i_uop.prs1_busy && (dis_uops(w).lrs1_rtype === RT_FIX) ||
                             f_uop.prs1_busy && (dis_uops(w).lrs1_rtype === RT_FLT)
    dis_uops(w).prs2_busy := i_uop.prs2_busy && (dis_uops(w).lrs2_rtype === RT_FIX) ||
                             f_uop.prs2_busy && (dis_uops(w).lrs2_rtype === RT_FLT)
    dis_uops(w).prs3_busy := f_uop.prs3_busy && dis_uops(w).frs3_en

    ren_stalls(w) := rename_stage.io.ren_stalls(w) || f_stall
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
  val rocc_shim_busy = if (usingRoCC) !exe_units.io.rocc.rxq_empty else false.B
  val wait_for_rocc = (0 until coreWidth).map(w =>
                        (dis_uops(w).is_fence || dis_uops(w).is_fencei) && (io.rocc.busy || rocc_shim_busy))
  val rxq_full = if (usingRoCC) exe_units.io.rocc.rxq_full else false.B
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
  rob.io.enq_partial_stall := dis_stalls.last
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
      dis_uops(w).rxq_idx := exe_units.io.rocc.rxq_idx(w)
    }
  }

  //-------------------------------------------------------------
  // Dispatch to issue queues

  // Get uops from rename2
  for (w <- 0 until coreWidth) {
    dispatcher.io.ren_uops(w).valid := dis_fire(w)
    dispatcher.io.ren_uops(w).bits  := dis_uops(w)
  }

  scheduler.io.dis_uops <> dispatcher.io.dis_uops(issueParams.indexWhere(_.iqType == IQT_INT.litValue))
  if (usingFPU) fp_pipeline.io.dis_uops <> dispatcher.io.dis_uops(issueParams.indexWhere(_.iqType == IQT_FP.litValue))

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Issue Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Generate slow wakeup signals from writeback ports.
  // TODO don't need this anymore with the separate long-latency ports
  for (w <- 0 until coreWidth) {
    val wbresp = exe_units.io.exe_resps(w)
    wakeups(w).bits  := wbresp.bits.uop.pdst
    wakeups(w).valid := (  wbresp.valid
                        && wbresp.bits.uop.rf_wen
                        && wbresp.bits.uop.dst_rtype === RT_FIX )
  }

  // Generate slow wakeup signals from long-latency writeback ports.
  for (w <- 0 until coreWidth) {
    val wbresp = exe_units.io.ll_resps(w)
    wakeups(w+coreWidth).bits  := wbresp.bits.uop.pdst
    wakeups(w+coreWidth).valid := (  wbresp.valid
                                  && wbresp.bits.uop.rf_wen
                                  && wbresp.bits.uop.dst_rtype === RT_FIX )
  }

  for ((renport, intport) <- rename_stage.io.wakeups zip wakeups) {
    renport <> intport
  }
  if (usingFPU) {
    for ((renport, fpport) <- fp_rename_stage.io.wakeups zip fp_pipeline.io.wakeups) {
      renport <> fpport
    }
  }

  var idiv_issued = false.B

  for (w <- 0 until coreWidth) {
    iss_valids(w) := scheduler.io.iss_uops(w).valid
    iss_uops(w)   := scheduler.io.iss_uops(w).bits

    idiv_issued = idiv_issued || (iss_valids(w) && iss_uops(w).fu_code_is(FU_DIV))
  }

  // Supress just-issued divides from issuing back-to-back, since it's an iterative divider.
  // But it takes a cycle to get to the Exe stage, so it can't tell us it is busy yet.
  scheduler.io.fu_avail := exe_units.io.fu_avail & ~(Fill(FUC_SZ, RegNext(idiv_issued)) & FU_DIV)

  // Send slow wakeups to scheduler
  scheduler.io.wakeups := wakeups

  // Load-hit Misspeculations
  scheduler.io.ld_miss := io.lsu.ld_miss

  scheduler.io.brupdate := brupdate
  scheduler.io.kill     := RegNext(rob.io.flush.valid)

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Register Read Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Register Read <- Issue (rrd <- iss)
  iregister_read.io.rf_read_ports <> iregfile.io.read_ports

  iregister_read.io.iss_valids := iss_valids
  iregister_read.io.iss_uops := iss_uops
  iregister_read.io.iss_uops map { u => u.iw_p1_poisoned := false.B; u.iw_p2_poisoned := false.B }

  iregister_read.io.brupdate := brupdate
  iregister_read.io.kill   := RegNext(rob.io.flush.valid)

  //-------------------------------------------------------------
  // Privileged Co-processor 0 Register File
  // Note: Normally this would be bad in that I'm writing state before
  // committing, so to get this to work I stall the entire pipeline for
  // CSR instructions so I never speculate these instructions.

  val csr_unit_resp = exe_units.io.csr_unit_resp
  csr_unit_resp.ready := DontCare

  // for critical path reasons, we aren't zero'ing this out if resp is not valid
  val csr_rw_cmd = csr_unit_resp.bits.uop.ctrl.csr_cmd
  val wb_wdata   = csr_unit_resp.bits.data

  csr.io.rw.addr        := csr_unit_resp.bits.uop.csr_addr
  csr.io.rw.cmd         := freechips.rocketchip.rocket.CSR.maskCmd(csr_unit_resp.valid, csr_rw_cmd)
  csr.io.rw.wdata       := wb_wdata

  // Extra I/O
  // Delay retire/exception 1 cycle
  csr.io.retire    := RegNext(PopCount(rob.io.commit.valids.asUInt))
  csr.io.exception := RegNext(rob.io.com_xcpt.valid)
  // csr.io.pc used for setting EPC during exception or CSR.io.trace.

  csr.io.pc        := (boom.util.AlignPCToBoundary(io.ifu.get_pc(0).com_pc, icBlockBytes)
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

  io.fcsr_rm := csr.io.fcsr_rm

  if (usingFPU) {
    exe_units.io.fcsr_rm   := csr.io.fcsr_rm
    fp_pipeline.io.fcsr_rm := csr.io.fcsr_rm
  }

  csr.io.hartid := io.hartid
  csr.io.interrupts := io.interrupts

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Execute Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  exe_units.io.exe_reqs <> iregister_read.io.exe_reqs

  exe_units.io.brupdate := brupdate
  exe_units.io.kill     := RegNext(rob.io.flush.valid)

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

  // Hook the writebacks up to the regfile
  for (w <- 0 until coreWidth) {
    val wbresp = exe_units.io.exe_resps(w)
    val wbpdst = wbresp.bits.uop.pdst_spec
    val wbdata = wbresp.bits.data

    def wbIsValid(rtype: UInt) =
      wbresp.valid && wbresp.bits.uop.rf_wen && wbresp.bits.uop.dst_rtype === rtype
    val wbReadsCSR = wbresp.bits.uop.ctrl.csr_cmd =/= freechips.rocketchip.rocket.CSR.N

    iregfile.io.write_ports(w).valid     := wbIsValid(RT_FIX)
    iregfile.io.write_ports(w).bits.addr := wbpdst
    iregfile.io.write_ports(w).bits.data := Mux(wbReadsCSR, csr.io.rw.rdata, wbdata)

    assert (!wbIsValid(RT_FLT), "[fppipeline] An FP writeback is being attempted to the Int Regfile.")

    assert (!(wbresp.valid &&
      !wbresp.bits.uop.rf_wen &&
      wbresp.bits.uop.dst_rtype === RT_FIX),
      "[fppipeline] An Int writeback is being attempted with rf_wen disabled.")

    assert (!(wbresp.valid &&
      wbresp.bits.uop.rf_wen &&
      wbresp.bits.uop.dst_rtype =/= RT_FIX),
      "[fppipeline] writeback being attempted to Int RF with dst != Int type exe_units("+w+").iresp")
  }

  // Hook the LL writebacks up to the regfile
  for (w <- 0 until coreWidth) {
    val wbresp = exe_units.io.ll_resps(w)
    val wbpdst = wbresp.bits.uop.pdst_spec
    val wbdata = wbresp.bits.data

    def wbIsValid(rtype: UInt) =
      wbresp.valid && wbresp.bits.uop.rf_wen && wbresp.bits.uop.dst_rtype === rtype

    iregfile.io.ll_write_ports(w).valid     := wbIsValid(RT_FIX)
    iregfile.io.ll_write_ports(w).bits.addr := wbpdst
    iregfile.io.ll_write_ports(w).bits.data := wbdata

    assert (!wbIsValid(RT_FLT), "[fppipeline] An FP writeback is being attempted to the Int Regfile.")

    assert (!(wbresp.valid &&
      !wbresp.bits.uop.rf_wen &&
      wbresp.bits.uop.dst_rtype === RT_FIX),
      "[fppipeline] An Int writeback is being attempted with rf_wen disabled.")

    assert (!(wbresp.valid &&
      wbresp.bits.uop.rf_wen &&
      wbresp.bits.uop.dst_rtype =/= RT_FIX),
      "[fppipeline] writeback being attempted to Int RF with dst != Int type exe_units("+w+").iresp")
  }

  if (usingFPU) {
    // Connect IFPU
    fp_pipeline.io.from_int  <> exe_units.io.to_fpu
    // Connect FPIU
    exe_units.io.from_fpu    <> fp_pipeline.io.to_int
    // Connect FLDs
    fp_pipeline.io.ll_wports <> exe_units.io.ll_fresps
  } else {
    exe_units.io.from_fpu.bits  := DontCare
    exe_units.io.from_fpu.valid := DontCare
    exe_units.io.to_fpu.ready   := DontCare

    for (ll_fresp <- exe_units.io.ll_fresps) {
      ll_fresp.ready := DontCare
    }
  }


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Commit Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Writeback
  // ---------

  var cnt = 0

  // Column resps
  for (w <- 0 until coreWidth) {
    val resp   = exe_units.io.exe_resps(w)
    val wb_uop = resp.bits.uop
    val data   = resp.bits.data

    rob.io.wb_resps(cnt).valid := resp.valid && !(wb_uop.uses_stq && !wb_uop.is_amo)
    rob.io.wb_resps(cnt).bits  := resp.bits

    rob.io.debug_wb_valids(cnt) := resp.valid && wb_uop.rf_wen && wb_uop.dst_rtype === RT_FIX
    rob.io.debug_wb_wdata(cnt)  := Mux(wb_uop.ctrl.csr_cmd =/= freechips.rocketchip.rocket.CSR.N,
    csr.io.rw.rdata,
    data)

    cnt += 1
  }

  // Long-latency resps
  for (w <- 0 until coreWidth) {
    val resp   = exe_units.io.ll_resps(w)
    val wb_uop = resp.bits.uop
    val data   = resp.bits.data

    rob.io.wb_resps(cnt).valid := resp.valid && !(wb_uop.uses_stq && !wb_uop.is_amo)
    rob.io.wb_resps(cnt).bits  := resp.bits

    rob.io.debug_wb_valids(cnt) := resp.valid && wb_uop.rf_wen && wb_uop.dst_rtype === RT_FIX
    rob.io.debug_wb_wdata(cnt)  := data

    cnt += 1
  }

  var f_cnt = 0
  for (eu <- exe_units) {
    val resp = eu.io.iresp
    if (eu.hasFFlags) {
      rob.io.fflags(f_cnt) <> resp.bits.fflags
    }
  }
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

  exe_units.io.status := csr.io.status
  exe_units.io.bp     := csr.io.bp
  exe_units.io.com_exception := rob.io.flush.valid

  if (usingFPU)
    fp_pipeline.io.status := csr.io.status

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

  // TODO: Does anyone want this debugging functionality?
  val coreMonitorBundle = Wire(new CoreMonitorBundle(xLen))
  coreMonitorBundle.clock  := clock
  coreMonitorBundle.reset  := reset
  coreMonitorBundle.hartid := DontCare
  coreMonitorBundle.timer  := DontCare
  coreMonitorBundle.valid  := DontCare
  coreMonitorBundle.pc     := DontCare
  coreMonitorBundle.wrdst  := DontCare
  coreMonitorBundle.wrdata := DontCare
  coreMonitorBundle.wren   := DontCare
  coreMonitorBundle.rd0src := DontCare
  coreMonitorBundle.rd0val := DontCare
  coreMonitorBundle.rd1src := DontCare
  coreMonitorBundle.rd1val := DontCare
  coreMonitorBundle.inst   := DontCare

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

  if (p(BoomTilesKey)(0).trace) {
    for (w <- 0 until coreWidth) {
      io.trace(w).clock      := clock
      io.trace(w).reset      := reset
      io.trace(w).valid      := RegNext(rob.io.commit.valids(w))
      io.trace(w).iaddr      := RegNext(Sext(rob.io.commit.uops(w).debug_pc(vaddrBits-1,0), xLen))
      io.trace(w).insn       := rob.io.commit.debug_insts(w)

      // These csr signals do not exactly match up with the ROB commit signals.
      io.trace(w).priv       := csr.io.status.prv
      // Can determine if it is an interrupt or not based on the MSB of the cause
      io.trace(w).exception  := rob.io.com_xcpt.valid && !rob.io.com_xcpt.bits.cause(xLen - 1)
      io.trace(w).interrupt  := rob.io.com_xcpt.valid && rob.io.com_xcpt.bits.cause(xLen - 1)
      io.trace(w).cause      := rob.io.com_xcpt.bits.cause
      io.trace(w).tval       := csr.io.tval
    }
    dontTouch(io.trace)
  } else {
    io.trace := DontCare
    io.trace map (t => t.valid := false.B)
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Commit log ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  io.lsu.debug_stcom.foreach(_.idx := DontCare)

  if (COMMIT_LOG_PRINTF) {
    for (w <- 0 until coreWidth) {
      val priv = RegNext(csr.io.status.prv) // erets change the privilege. Get the old one

      // To allow for diffs against spike
      def printf_inst(uop: MicroOp) = {
        when (uop.is_rvc) {
          printf("(0x%x)", uop.debug_inst(15,0))
        } .otherwise {
          printf("(0x%x)", uop.debug_inst)
        }
      }

      io.lsu.debug_stcom(w).idx := rob.io.commit.uops(w).stq_idx

      when (rob.io.commit.valids(w)) {
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
        } .elsewhen (rob.io.commit.uops(w).uses_stq) {
          val stq_data = io.lsu.debug_stcom(w).data
          val mem_size = rob.io.commit.uops(w).mem_size

          when (mem_size === 3.U) {
            printf(" mem 0x%x 0x%x\n",
              io.lsu.debug_stcom(w).addr,
              stq_data)
          } .elsewhen (mem_size === 2.U) {
            printf(" mem 0x%x 0x%x\n",
              io.lsu.debug_stcom(w).addr,
              stq_data(31,0))
          } .elsewhen (mem_size === 1.U) {
            printf(" mem 0x%x 0x%x\n",
              io.lsu.debug_stcom(w).addr,
              stq_data(15,0))
          } .elsewhen (stq_data(7,4).orR) {
            printf(" mem 0x%x 0x%x\n",
              io.lsu.debug_stcom(w).addr,
              stq_data(7,0))
          } .otherwise {  // Drop leading zero from byte stores to match a bug in spike
            printf(" mem 0x%x 0x%x\n",
              io.lsu.debug_stcom(w).addr,
              stq_data(3,0))
          }

        } .otherwise {
          printf("\n")
        }
      }
    }
  }
}
