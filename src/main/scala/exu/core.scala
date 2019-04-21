//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
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

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.rocket.Causes
import freechips.rocketchip.util.{Str, UIntIsOneOf, CoreMonitorBundle}

import boom.common._
import boom.exu.FUConstants._
import boom.system.BoomTilesKey
import boom.util.{GetNewUopAndBrMask, Sext, WrapInc}

/**
 * IO bundle for the BOOM Core. Connects the external components such as
 * the Frontend to the core.
 */
trait HasBoomCoreIO extends freechips.rocketchip.tile.HasTileParameters
{
   implicit val p: Parameters
   val io = new freechips.rocketchip.tile.CoreBundle()(p)
      with freechips.rocketchip.tile.HasExternallyDrivenTileConstants
   {
         val interrupts = Input(new freechips.rocketchip.tile.CoreInterrupts())
         val ifu = new boom.ifu.BoomFrontendIO
         val dmem = new freechips.rocketchip.rocket.HellaCacheIO
         val ptw = Flipped(new freechips.rocketchip.rocket.DatapathPTWIO())
         val rocc = Flipped(new freechips.rocketchip.tile.RoCCCoreIO())
         val ptw_tlb = new freechips.rocketchip.rocket.TLBPTWIO()
         val trace = Output(Vec(coreParams.retireWidth,
            new freechips.rocketchip.rocket.TracedInstruction))
         val release = Flipped(Valid(new boom.lsu.ReleaseInfo))
         val fcsr_rm = UInt(freechips.rocketchip.tile.FPConstants.RM_SZ.W)
   }
}

/**
 * Top level core object that connects the Frontend to the rest of the pipeline.
 */
class BoomCore(implicit p: Parameters, edge: freechips.rocketchip.tilelink.TLEdgeOut) extends BoomModule()(p)
   with HasBoomCoreIO
{
   //**********************************
   // construct all of the modules

   // Only holds integer-registerfile execution units.
   val exe_units = new boom.exu.ExecutionUnits(fpu=false)

   // Meanwhile, the FP pipeline holds the FP issue window, FP regfile, and FP arithmetic units.
   var fp_pipeline: FpPipeline = null
   if (usingFPU) fp_pipeline = Module(new FpPipeline)

   // ********************************************************
   // Clear fp_pipeline before use
   if (usingFPU)
   {
     fp_pipeline.io.ll_wport  := DontCare
     fp_pipeline.io.wb_valids := DontCare
     fp_pipeline.io.wb_pdsts  := DontCare
   }

   val numIrfWritePorts      = exe_units.numIrfWritePorts
   val numLlIrfWritePorts   = exe_units.numLlIrfWritePorts
   val numIrfReadPorts       = exe_units.numIrfReadPorts

   val num_fast_wakeup_ports    = exe_units.count(_.bypassable)
   val num_alwaysBypassable    = exe_units.count(_.alwaysBypassable)

   val num_int_iss_wakeup_ports = numIrfWritePorts + 1 + num_fast_wakeup_ports - num_alwaysBypassable // + 1 for ll_wb
   val num_int_ren_wakeup_ports = if (enableFastWakeupsToRename) num_int_iss_wakeup_ports else numIrfWritePorts + 1
   val num_fp_wakeup_ports      = if (usingFPU) fp_pipeline.io.wakeups.length else 0

   val decode_units     = for (w <- 0 until decodeWidth) yield { val d = Module(new DecodeUnit); d }
   val dec_brmask_logic = Module(new BranchMaskGenerationLogic(coreWidth))
   val rename_stage     = Module(new RenameStage(coreWidth, num_int_ren_wakeup_ports, num_fp_wakeup_ports))
   val issue_units      = new boom.exu.IssueUnits(num_int_iss_wakeup_ports)
   val dispatcher       = Module(new BasicDispatcher)

   val iregfile         = if (enableCustomRf)
                          {
                              Module(new RegisterFileSeqCustomArray(numIntPhysRegs,
                                 numIrfReadPorts,
                                 numIrfWritePorts + 1, // + 1 for ll writebacks
                                 xLen,
                                 Seq(true) ++ exe_units.bypassable_write_port_mask)) // 0th is bypassable ll_wb
                          }
                          else
                          {
                              Module(new RegisterFileSynthesizable(numIntPhysRegs,
                                 numIrfReadPorts,
                                 numIrfWritePorts + 1, // + 1 for ll writebacks
                                 xLen,
                                 Seq(true) ++ exe_units.bypassable_write_port_mask)) // 0th is bypassable ll_wb
                          }
   val ll_wbarb         = Module(new Arbiter(new ExeUnitResp(xLen), 1 +
                                                                    (if (usingFPU) 1 else 0) +
                                                                    (if (usingRoCC) 1 else 0)))
   val iregister_read   = Module(new RegisterRead(
                                 issue_units.map(_.issueWidth).sum,
                                 exe_units.withFilter(_.readsIrf).map(_.supportedFuncUnits),
                                 numIrfReadPorts,
                                 exe_units.withFilter(_.readsIrf).map(x => 2),
                                 exe_units.numTotalBypassPorts,
                                 xLen))
   val dc_shim          = Module(new boom.lsu.DCacheShim())
   val lsu              = Module(new boom.lsu.LoadStoreUnit(coreWidth))
   val rob              = Module(new Rob(
                                 numIrfWritePorts + 1 + num_fp_wakeup_ports, // +1 for ll writebacks
                                 num_fp_wakeup_ports))
   // Used to wakeup registers in rename and issue. ROB needs to listen to something else.
   val int_iss_wakeups  = Wire(Vec(num_int_iss_wakeup_ports, Valid(new ExeUnitResp(xLen))))
   val int_ren_wakeups  = Wire(Vec(num_int_ren_wakeup_ports, Valid(new ExeUnitResp(xLen))))
   int_iss_wakeups := DontCare
   int_ren_wakeups := DontCare

   require (exe_units.length == issue_units.map(_.issueWidth).sum)

   //***********************************
   // Pipeline State Registers and Wires

   // Instruction Decode Stage
   val dec_valids     = Wire(Vec(coreWidth, Bool()))  // are the decoded instruction valid? It may be held up though.
   val dec_uops       = Wire(Vec(coreWidth, new MicroOp()))
   val dec_will_fire  = Wire(Vec(coreWidth, Bool()))  // can the instruction fire beyond decode?
                                                         // (can still be stopped in ren or dis)
   val dec_rdy        = Wire(Bool())

   // Issue Stage/Register Read
   val iss_valids     = Wire(Vec(exe_units.numIrfReaders, Bool()))
   val iss_uops       = Wire(Vec(exe_units.numIrfReaders, new MicroOp()))
   val bypasses       = Wire(new BypassData(exe_units.numTotalBypassPorts, xLen))

   // Branch Unit
   val br_unit = Wire(new BranchUnitResp())
   val brunit_idx = exe_units.br_unit_idx
   br_unit <> exe_units.br_unit_io

   for (eu <- exe_units)
   {
      eu.io.brinfo        := br_unit.brinfo
   }

   if (usingFPU)
   {
      fp_pipeline.io.brinfo := br_unit.brinfo
   }

   // Shim to DCache
   io.dmem <> dc_shim.io.dmem
   dc_shim.io.core <> exe_units.memory_unit.io.dmem

   // Load/Store Unit & ExeUnits
   exe_units.memory_unit.io.lsu_io <> lsu.io

   // TODO: Generate this in lsu
   val sxt_ldMiss = Wire(Bool())

   //-------------------------------------------------------------
   // Uarch Hardware Performance Events (HPEs)

   val perfEvents = new freechips.rocketchip.rocket.EventSets(Seq(
      new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
         ("exception", () => rob.io.com_xcpt.valid),
         ("nop",       () => false.B),
         ("nop",       () => false.B),
         ("nop",       () => false.B))),

         // Unused RocketCore HPE's
         //("load",   () => id_ctrl.mem && id_ctrl.mem_cmd === M_XRD && !id_ctrl.fp),
         //("store",  () => id_ctrl.mem && id_ctrl.mem_cmd === M_XWR && !id_ctrl.fp),
         //("amo",    () => Bool(usingAtomics) && id_ctrl.mem && (isAMO(id_ctrl.mem_cmd) ||
         //                 id_ctrl.mem_cmd.isOneOf(M_XLR, M_XSC))),
         //("system", () => =/= CSR.N))),
         //("arith",  () => id_ctrl.wxd && !(id_ctrl.jal || id_ctrl.jalr || id_ctrl.mem || id_ctrl.fp ||
         //                id_ctrl.div || id_ctrl.csr =/= CSR.N)),
         //("branch", () => id_ctrl.branch),
         //("jal",    () => id_ctrl.jal),
         //("jalr",   () => id_ctrl.jalr))
         //++ (if (!usingMulDiv) Seq() else Seq(
         //  ("mul", () => id_ctrl.div && (id_ctrl.alu_fn & ALU.FN_DIV) =/= ALU.FN_DIV),
         //  ("div", () => id_ctrl.div && (id_ctrl.alu_fn & ALU.FN_DIV) === ALU.FN_DIV)))
         //++ (if (!usingFPU) Seq() else Seq(
         //  ("fp load",     () => id_ctrl.fp && io.fpu.dec.ldst && io.fpu.dec.wen),
         //  ("fp store",    () => id_ctrl.fp && io.fpu.dec.ldst && !io.fpu.dec.wen),
         //  ("fp add",      () => id_ctrl.fp && io.fpu.dec.fma && io.fpu.dec.swap23),
         //  ("fp mul",      () => id_ctrl.fp && io.fpu.dec.fma && !io.fpu.dec.swap23 && !io.fpu.dec.ren3),
         //  ("fp mul-add",  () => id_ctrl.fp && io.fpu.dec.fma && io.fpu.dec.ren3),
         //  ("fp div/sqrt", () => id_ctrl.fp && (io.fpu.dec.div || io.fpu.dec.sqrt)),
         //  ("fp other",    () => id_ctrl.fp && !(io.fpu.dec.ldst || io.fpu.dec.fma ||
         //                        io.fpu.dec.div || io.fpu.dec.sqrt))))),

      new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
         ("I$ blocked",                        () => icache_blocked),
         ("nop",                               () => false.B),
         ("branch misprediction",              () => br_unit.brinfo.mispredict),
         ("control-flow target misprediction", () => br_unit.brinfo.mispredict &&
                                                     br_unit.brinfo.is_jr),
         ("flush",                             () => rob.io.flush.valid),
         ("branch resolved",                   () => br_unit.brinfo.valid))),

         // Unused RocketCore HPE's
         //("load-use interlock",     () => id_ex_hazard && ex_ctrl.mem || id_mem_hazard && mem_ctrl.mem ||
         //                                 id_wb_hazard && wb_ctrl.mem),
         //("long-latency interlock", () => id_sboard_hazard),
         //("csr interlock",          () => id_ex_hazard && ex_ctrl.csr =/= CSR.N ||
         //                                 id_mem_hazard && mem_ctrl.csr =/= CSR.N ||
         //                                 id_wb_hazard && wb_ctrl.csr =/= CSR.N),
         //("D$ blocked",             () => id_ctrl.mem && dcache_blocked),
         //++ (if (!usingMulDiv) Seq() else Seq(
         //  ("mul/div interlock", () => id_ex_hazard && ex_ctrl.div || id_mem_hazard &&
         //                              mem_ctrl.div || id_wb_hazard && wb_ctrl.div)))
         //++ (if (!usingFPU) Seq() else Seq(
         //  ("fp interlock", () => id_ex_hazard && ex_ctrl.fp || id_mem_hazard && mem_ctrl.fp ||
         //                         id_wb_hazard && wb_ctrl.fp || id_ctrl.fp && id_stall_fpu)))),

     new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
        ("I$ miss",     () => io.ifu.perf.acquire),
        ("D$ miss",     () => io.dmem.perf.acquire),
        ("D$ release",  () => io.dmem.perf.release),
        ("ITLB miss",   () => io.ifu.perf.tlbMiss),
        ("DTLB miss",   () => io.dmem.perf.tlbMiss),
        ("L2 TLB miss", () => io.ptw.perf.l2miss)))))

   val csr = Module(new freechips.rocketchip.rocket.CSRFile(perfEvents))
   csr.io.inst foreach { c => c := DontCare }
   csr.io.rocc_interrupt := DontCare

   // evaluate performance counters
   val icache_blocked = !(io.ifu.fetchpacket.valid || RegNext(io.ifu.fetchpacket.valid))
   csr.io.counters foreach { c => c.inc := RegNext(perfEvents.evaluate(c.eventSel)) }

   // Old BOOM Core HPE's
   //// User-level instruction count.
   //csr.io.events(2) := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && (csr.io.status.prv === UInt(freechips.rocketchip.rocket.PRV.U))})

   //csr.io.events(5)  := csr.io.status.prv === UInt(freechips.rocketchip.rocket.PRV.U)

   //// Instruction mixes.
   //csr.io.events(6)  := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal})
   //csr.io.events(7)  := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).is_jal})
   //csr.io.events(8)  := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).is_jump && !rob.io.commit.uops(w).is_jal})
   //csr.io.events(9)  := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).is_load})
   //csr.io.events(10) := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).is_store})
   //csr.io.events(11) := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).fp_val})

   //// Decode stall causes.
   //csr.io.events(12) := !rob.io.ready
   //csr.io.events(13) := lsu.io.laq_full
   //csr.io.events(14) := lsu.io.stq_full
   //csr.io.events(15) := !dis_readys.asBools.reduce(_&_) // issue queues
   //csr.io.events(16) := branch_mask_full.reduce(_|_)
   //csr.io.events(17) := rob.io.flush.valid

   //// LSU Speculation stats.
   //csr.io.events(18) := lsu.io.counters.ld_valid
   //csr.io.events(19) := lsu.io.counters.stld_order_fail
   //csr.io.events(20) := lsu.io.counters.ldld_order_fail

   //// Branch prediction stats.
   //csr.io.events(21)  := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
   //   rob.io.commit.uops(w).stat_brjmp_mispredicted})
   //csr.io.events(22) := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
   //   rob.io.commit.uops(w).stat_btb_made_pred})
   //csr.io.events(23) := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
   //   rob.io.commit.uops(w).stat_btb_mispredicted})
   //csr.io.events(24) := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
   //   rob.io.commit.uops(w).stat_bpd_made_pred})
   //csr.io.events(25) := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
   //   rob.io.commit.uops(w).stat_bpd_mispredicted})

   //// Branch prediction - no prediction made.
   //csr.io.events(26) := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
   //   !rob.io.commit.uops(w).stat_btb_made_pred && !rob.io.commit.uops(w).stat_bpd_made_pred})

   //// Branch prediction - no predition made & a mispredict occurred.
   //csr.io.events(27) := PopCount((Range(0,coreWidth)).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
   //   !rob.io.commit.uops(w).stat_btb_made_pred && !rob.io.commit.uops(w).stat_bpd_made_pred &&
   //   rob.io.commit.uops(w).stat_brjmp_mispredicted})

   //// Count user-level branches (subtract from total to get privilege branch accuracy)
   //csr.io.events(28) := br_unit.brinfo.valid && (csr.io.status.prv === UInt(freechips.rocketchip.rocket.PRV.U))
   //csr.io.events(29) := br_unit.brinfo.mispredict && (csr.io.status.prv === UInt(rocket.PRV.U))

   //// count change of privilege modes
   //csr.io.events(30) := csr.io.status.prv =/= RegNext(csr.io.status.prv)

   //csr.io.events(31) := !issue_units(0).io.dis_readys.reduce(_&_)
   //csr.io.events(32) := !issue_units(1).io.dis_readys.reduce(_&_)
   //csr.io.events(33) := !fp_pipeline.io.dis_readys.reduce(_&_)

   //assert (!(Range(0,coreWidth).map{w =>
   //   rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && rob.io.commit.uops(w).is_jal &&
   //   rob.io.commit.uops(w).stat_brjmp_mispredicted}.reduce(_|_)),
   //   "[dpath] A committed JAL was marked as having been mispredicted.")

   //// Count issued instructions (only integer currently).
   //require (log2Ceil(1+iss_valids.length) <= csr.io.events(0).getWidth) // CSR.scala sets increment width.
   //csr.io.events(34) := PopCount(iss_valids)

   //// Count not-issued slots due to empty issue windows (only integer currently).
   //val not_issued_and_empty = for {iu <- issue_units; iss_valid <- iu.io.iss_valids} yield {
   //      !iss_valid && iu.io.event_empty }
   //csr.io.events(35) := PopCount(not_issued_and_empty)

   //// Count not-issued slots due to backend hazards/unsatisified dependencies (only integer currently).
   //val not_issued_and_not_empty = for {iu <- issue_units; iss_valid <- iu.io.iss_valids} yield {
   //      !iss_valid && !iu.io.event_empty}
   //csr.io.events(36) := PopCount(not_issued_and_not_empty)

   //****************************************
   // Time Stamp Counter & Retired Instruction Counter
   // (only used for printf and vcd dumps - the actual counters are in the CSRFile)
   val debug_tsc_reg  = RegInit(0.U(xLen.W))
   val debug_irt_reg  = RegInit(0.U(xLen.W))
   debug_tsc_reg  := debug_tsc_reg + Mux(O3PIPEVIEW_PRINTF.B, O3_CYCLE_TIME.U, 1.U)
   debug_irt_reg  := debug_irt_reg + PopCount(rob.io.commit.valids.asUInt)
   dontTouch(debug_tsc_reg)
   dontTouch(debug_irt_reg)

   //****************************************
   // Print-out information about the machine

   val iss_str =
      if (enableAgePriorityIssue) " (Age-based Priority)"
      else " (Unordered Priority)"

   val exe_units_str = exe_units.toString
   val fp_pipeline_str =
      if (usingFPU) fp_pipeline.fp_string
      else ""
   val rob_str = rob.toString

   override def toString: String =
     ( exe_units_str + "\n"
     + fp_pipeline_str + "\n"
     + rob_str + "\n"
     + "\n   ==Overall Core Params=="
     + "\n   Fetch Width           : " + fetchWidth
     + "\n   Decode Width          : " + coreWidth
     + "\n   Issue Width           : " + issueParams.map(_.issueWidth).sum
     + "\n   ROB Size              : " + numRobEntries
     + "\n   Issue Window Size     : " + issueParams.map(_.numEntries) + iss_str
     + "\n   Load/Store Unit Size  : " + NUM_LDQ_ENTRIES + "/" + NUM_STQ_ENTRIES
     + "\n   Num Int Phys Registers: " + numIntPhysRegs
     + "\n   Num FP  Phys Registers: " + numFpPhysRegs
     + "\n   Max Branch Count      : " + MAX_BR_COUNT
     + "\n   BTB Size              : "
     + (if (enableBTB) ("" + boomParams.btb.nSets * boomParams.btb.nWays + " entries (" +
          boomParams.btb.nSets + " x " + boomParams.btb.nWays + " ways)") else 0)
     + "\n   RAS Size              : " + (if (enableBTB) boomParams.btb.nRAS else 0)
     + "\n   Rename  Stage Latency : " + renameLatency
     + "\n" + iregfile.toString
     + "\n   Num Slow Wakeup Ports : " + numIrfWritePorts
     + "\n   Num Fast Wakeup Ports : " + exe_units.count(_.bypassable)
     + "\n   Num Bypass Ports      : " + exe_units.numTotalBypassPorts
     + "\n" + (if (usingFPU) fp_pipeline.toString else "")
     + "\n   DCache Ways           : " + dcacheParams.nWays
     + "\n   DCache Sets           : " + dcacheParams.nSets
     + "\n   DCache nMSHRs         : " + dcacheParams.nMSHRs
     + "\n   ICache Ways           : " + icacheParams.nWays
     + "\n   ICache Sets           : " + icacheParams.nSets
     + "\n   D-TLB Entries         : " + dcacheParams.nTLBEntries
     + "\n   I-TLB Entries         : " + icacheParams.nTLBEntries
     + "\n   Paddr Bits            : " + paddrBits
     + "\n   Vaddr Bits            : " + vaddrBits
     + "\n\n   Using FPU Unit?       : " + usingFPU.toString
     + "\n   Using FDivSqrt?       : " + usingFDivSqrt.toString
     + "\n   Using VM?             : " + usingVM.toString)

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Fetch Stage/Frontend ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   io.ifu.br_unit := br_unit
   io.ifu.tsc_reg := debug_tsc_reg

   // SFence needs access to the PC to inject an address into the TLB's CAM port. The ROB
   // will have to later redirect the PC back to the regularly scheduled program.
   io.ifu.sfence_take_pc    := lsu.io.exe_resp.bits.sfence.valid
   io.ifu.sfence_addr       := lsu.io.exe_resp.bits.sfence.bits.addr

   // We must redirect the PC the cycle after playing the SFENCE game.
   io.ifu.flush_take_pc     := rob.io.flush.valid || RegNext(lsu.io.exe_resp.bits.sfence.valid)

   // TODO FIX THIS HACK
   // The below code works because of two quirks with the flush mechanism
   //  1 ) All flush_on_commit instructions are also is_unique,
   //      In the future, this constraint will be relaxed.
   //  2 ) We send out flush signals one cycle after the commit signal. We need to
   //      mux between one/two cycle delay for the following cases:
   //       ERETs are reported to the CSR two cycles before we send the flush
   //       Exceptions are reported to the CSR one cycle before we send the flush
   // This discrepency should be resolved elsewhere.
   io.ifu.flush_pc          := Mux(RegNext(RegNext(csr.io.eret)),
                                   RegNext(RegNext(csr.io.evec)),
                                   RegNext(csr.io.evec))
   io.ifu.com_ftq_idx       := rob.io.com_xcpt.bits.ftq_idx

   io.ifu.clear_fetchbuffer := br_unit.brinfo.mispredict ||
                               rob.io.flush.valid ||
                               io.ifu.sfence_take_pc

   io.ifu.flush_info.valid  := rob.io.flush.valid
   io.ifu.flush_info.bits   := rob.io.flush.bits

   // Tell the FTQ it can deallocate entries by passing youngest ftq_idx.
   val youngest_com_idx     = (coreWidth-1).U - PriorityEncoder(rob.io.commit.valids.reverse)
   io.ifu.commit.valid      := rob.io.commit.valids.reduce(_|_)
   io.ifu.commit.bits       := rob.io.commit.uops(youngest_com_idx).ftq_idx

   io.ifu.flush_icache :=
      Range(0,coreWidth).map{i => rob.io.commit.valids(i) && rob.io.commit.uops(i).is_fencei}.reduce(_|_) ||
      (br_unit.brinfo.mispredict && br_unit.brinfo.is_jr &&  csr.io.status.debug)

   // Delay sfence to match pushing the sfence.addr into the TLB's CAM port.
   io.ifu.sfence := RegNext(lsu.io.exe_resp.bits.sfence)

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Branch Prediction ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   io.ifu.flush := rob.io.flush.valid

   io.ifu.status_prv    := csr.io.status.prv
   io.ifu.status_debug  := csr.io.status.debug

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

   io.ifu.fetchpacket.ready := dec_rdy
   val dec_fbundle = io.ifu.fetchpacket.bits

   //-------------------------------------------------------------
   // Decoders

   // allow early instructions to stall later instructions
   var dec_stall_next_inst = false.B
   var dec_last_inst_was_stalled = false.B

   // send only 1 RoCC instructions at a time
   var dec_rocc_found = if (usingRoCC) exe_units.rocc_unit.io.rocc.rxq_full else false.B
   val rocc_shim_busy = if (usingRoCC) !exe_units.rocc_unit.io.rocc.rxq_empty else false.B

   // stall fetch/dcode because we ran out of branch tags
   val branch_mask_full = Wire(Vec(coreWidth, Bool()))

   for (w <- 0 until coreWidth)
   {
      dec_valids(w)                      := io.ifu.fetchpacket.valid && dec_fbundle.uops(w).valid &&
                                            !dec_finished_mask(w)
      decode_units(w).io.enq.uop         := dec_fbundle.uops(w).bits
      decode_units(w).io.status          := csr.io.status
      decode_units(w).io.csr_decode      <> csr.io.decode(w)
      decode_units(w).io.interrupt       := csr.io.interrupt
      decode_units(w).io.interrupt_cause := csr.io.interrupt_cause

      val prev_insts_in_bundle_valid = Range(0,w).map{i => dec_valids(i)}.foldLeft(false.B)(_|_)

      // stall this instruction?
      // TODO tailor this to only care if a given instruction uses a resource?
      val stall_me = (dec_valids(w) &&
                        (  !(rename_stage.io.inst_can_proceed(w))
                        || (dec_uops(w).is_unique &&
                           (!(rob.io.empty) || !lsu.io.lsu_fencei_rdy || prev_insts_in_bundle_valid))
                        || !rob.io.ready
                        || lsu.io.laq_full(w) && dec_uops(w).is_load
                        || lsu.io.stq_full(w) && dec_uops(w).is_store
                        || branch_mask_full(w)
                        || br_unit.brinfo.mispredict
                        || rob.io.flush.valid
                        || dec_stall_next_inst
                        || ((dec_uops(w).is_fence || dec_uops(w).is_fencei) && (io.rocc.busy || rocc_shim_busy))
                        || (dec_uops(w).is_fencei && !lsu.io.lsu_fencei_rdy)
                        || (dec_uops(w).uopc === uopROCC && dec_rocc_found)
                        )) ||
                     dec_last_inst_was_stalled

      // stall the next instruction following me in the decode bundle?
      dec_last_inst_was_stalled = stall_me
      dec_stall_next_inst = stall_me || (dec_valids(w) && dec_uops(w).is_unique)
      dec_rocc_found = dec_rocc_found || (dec_valids(w) && dec_uops(w).uopc === uopROCC)

      dec_will_fire(w) := dec_valids(w) && !stall_me && !io.ifu.clear_fetchbuffer
      dec_uops(w)      := decode_units(w).io.deq.uop
   }

   // all decoders are empty and ready for new instructions
   dec_rdy := !(dec_stall_next_inst)

   when (dec_rdy || io.ifu.clear_fetchbuffer)
   {
      dec_finished_mask := 0.U
   }
   .otherwise
   {
      dec_finished_mask := dec_will_fire.asUInt | dec_finished_mask
   }

   //-------------------------------------------------------------
   // Branch Mask Logic

   dec_brmask_logic.io.brinfo := br_unit.brinfo
   dec_brmask_logic.io.flush_pipeline := rob.io.flush.valid

   for (w <- 0 until coreWidth)
   {
      dec_brmask_logic.io.is_branch(w) := !dec_finished_mask(w) && dec_uops(w).allocate_brtag
      dec_brmask_logic.io.will_fire(w) :=  dec_will_fire(w) &&
                                           dec_uops(w).allocate_brtag // ren, dis can back pressure us

      dec_uops(w).br_tag  := dec_brmask_logic.io.br_tag(w)
      dec_uops(w).br_mask := dec_brmask_logic.io.br_mask(w)
   }

   branch_mask_full := dec_brmask_logic.io.is_full

   //-------------------------------------------------------------
   // LD/ST Unit Allocation Logic

   for (w <- 0 until decodeWidth)
   {
      dec_uops(w).ldq_idx := lsu.io.new_ldq_idx(w)
      dec_uops(w).stq_idx := lsu.io.new_stq_idx(w)
   }

   //-------------------------------------------------------------
   // RoCC allocation logic
   if (usingRoCC)
   {
      for (w <- 0 until coreWidth)
      {
         // We guarantee only decoding 1 RoCC instruction per cycle
         dec_uops(w).rxq_idx := exe_units.rocc_unit.io.rocc.rxq_idx
      }
   }

   //-------------------------------------------------------------
   // Rob Allocation Logic

   for (w <- 0 until coreWidth)
   {
      // note: this assumes uops haven't been shifted - there's a 1:1 match between PC's LSBs and "w" here
      // (thus the LSB of the rob_idx gives part of the PC)
      if (coreWidth == 1)
      {
         dec_uops(w).rob_idx := rob.io.rob_tail_idx
      }
      else
      {
         dec_uops(w).rob_idx := Cat(rob.io.rob_tail_idx >> log2Ceil(coreWidth).U,
                                    w.U(log2Ceil(coreWidth).W))
      }
   }

   val dec_has_br_or_jalr_in_packet =
      (dec_valids zip dec_uops map {case(v,u) => v && u.is_br_or_jmp && !u.is_jal}).reduce(_|_)

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Register Rename Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------
   rename_stage.io.kill     := io.ifu.clear_fetchbuffer // mispredict or flush
   rename_stage.io.brinfo   := br_unit.brinfo

   rename_stage.io.flush_pipeline := rob.io.flush.valid
   rename_stage.io.debug_rob_empty := rob.io.empty

   rename_stage.io.dec_will_fire := dec_will_fire
   rename_stage.io.dec_uops := dec_uops

   var iss_wu_idx = 1
   var ren_wu_idx = 1
   // The 0th wakeup port goes to the ll_wbarb
   int_iss_wakeups(0).valid := ll_wbarb.io.out.fire() && ll_wbarb.io.out.bits.uop.dst_rtype === RT_FIX
   int_iss_wakeups(0).bits  := ll_wbarb.io.out.bits
   int_ren_wakeups(0).valid := ll_wbarb.io.out.fire()
   int_ren_wakeups(0).bits  := ll_wbarb.io.out.bits

   // loop through each issue-port (exe_units are statically connected to an issue-port)
   for (i <- 0 until exe_units.length)
   {
      if (exe_units(i).writesIrf)
      {
         val fast_wakeup = Wire(Valid(new ExeUnitResp(xLen)))
         val slow_wakeup = Wire(Valid(new ExeUnitResp(xLen)))
         fast_wakeup := DontCare
         slow_wakeup := DontCare

         val resp = exe_units(i).io.iresp
         assert(!(resp.valid && resp.bits.uop.ctrl.rf_wen && resp.bits.uop.dst_rtype =/= RT_FIX))

         // Fast Wakeup (uses just-issued uops) that have known latencies.
         fast_wakeup.bits.uop := iss_uops(i)
         fast_wakeup.valid    := iss_valids(i) &&
                                   iss_uops(i).bypassable &&
                                   iss_uops(i).dst_rtype === RT_FIX &&
                                   iss_uops(i).ldst_val

         // Slow Wakeup (uses write-port to register file)
         slow_wakeup.bits.uop := resp.bits.uop
         slow_wakeup.valid    := resp.valid &&
                                   resp.bits.uop.ctrl.rf_wen &&
                                   !resp.bits.uop.bypassable &&
                                   resp.bits.uop.dst_rtype === RT_FIX

         if (exe_units(i).bypassable)
         {
            int_iss_wakeups(iss_wu_idx) := fast_wakeup
            iss_wu_idx += 1
         }
         if (!exe_units(i).alwaysBypassable)
         {
            int_iss_wakeups(iss_wu_idx) := slow_wakeup
            iss_wu_idx += 1
         }

         if (exe_units(i).bypassable && enableFastWakeupsToRename)
         {
            int_ren_wakeups(ren_wu_idx) := fast_wakeup
            ren_wu_idx += 1
         }
         if (!exe_units(i).alwaysBypassable || !enableFastWakeupsToRename)
         {
            int_ren_wakeups(ren_wu_idx) := slow_wakeup
            ren_wu_idx += 1
         }
      }
   }
   require (iss_wu_idx == num_int_iss_wakeup_ports)
   require (ren_wu_idx == num_int_ren_wakeup_ports)
   require (iss_wu_idx == ren_wu_idx || !enableFastWakeupsToRename)

   // Perform load-hit speculative wakeup through a special port (performs a poison wake-up).
   issue_units map { iu =>
      iu.io.mem_ldSpecWakeup <> lsu.io.mem_ldSpecWakeup
   }

   for ((renport, intport) <- rename_stage.io.int_wakeups zip int_ren_wakeups)
   {
      // Stop wakeup for bypassable children of spec-loads trying to issue during a ldMiss.
      renport.valid :=
         intport.valid &&
         !(sxt_ldMiss && (intport.bits.uop.iw_p1_poisoned || intport.bits.uop.iw_p2_poisoned))
      renport.bits := intport.bits
   }
   if (usingFPU)
   {
      for ((renport, fpport) <- rename_stage.io.fp_wakeups zip fp_pipeline.io.wakeups)
      {
         renport <> fpport
      }
   }

   rename_stage.io.com_valids := rob.io.commit.valids
   rename_stage.io.com_uops := rob.io.commit.uops
   rename_stage.io.com_rbk_valids := rob.io.commit.rbk_valids

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Dispatch Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // Get uops from rename 2, and backpressure ren2 if necessary
   rename_stage.io.dis_inst_can_proceed := VecInit(dispatcher.io.ren_uops.map(_.ready))
   for (w <- 0 until coreWidth)
   {
      dispatcher.io.ren_uops(w).valid := rename_stage.io.ren2_mask(w)
      dispatcher.io.ren_uops(w).bits  := GetNewUopAndBrMask(rename_stage.io.ren2_uops(w), br_unit.brinfo)
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

   // Output (Issue)

   var iss_idx = 0
   var iss_cnt = 0
   for (w <- 0 until exe_units.length)
   {
      var fu_types = exe_units(w).io.fu_types
      val exe_unit = exe_units(w)
      if (exe_unit.readsIrf)
      {
         if (exe_unit.supportedFuncUnits.muld)
         {
            // Supress just-issued divides from issuing back-to-back, since it's an iterative divider.
            // But it takes a cycle to get to the Exe stage, so it can't tell us it is busy yet.
            val idiv_issued = iss_valids(iss_idx) && iss_uops(iss_idx).fu_code_is(FU_DIV)
            fu_types = fu_types & RegNext(~Mux(idiv_issued, FU_DIV, 0.U))
         }

         if (exe_unit.hasMem)
         {
            iss_valids(iss_idx) := issue_units.mem_iq.io.iss_valids(0)
            iss_uops(iss_idx)   := issue_units.mem_iq.io.iss_uops(0)
            issue_units.mem_iq.io.fu_types(0) := fu_types
         }
         else
         {
            iss_valids(iss_idx) := issue_units.int_iq.io.iss_valids(iss_cnt)
            iss_uops(iss_idx)   := issue_units.int_iq.io.iss_uops(iss_cnt)
            issue_units.int_iq.io.fu_types(iss_cnt) := fu_types
            iss_cnt += 1
         }
         iss_idx += 1
      }
   }
   require(iss_idx == exe_units.numIrfReaders)

   issue_units.map(_.io.tsc_reg := debug_tsc_reg)
   issue_units.map(_.io.brinfo := br_unit.brinfo)
   issue_units.map(_.io.flush_pipeline := rob.io.flush.valid)

   // Load-hit Misspeculations
   require (issue_units.count(_.iqType == IQT_MEM.litValue) == 1 || usingUnifiedMemIntIQs)
   val mem_iq = issue_units.mem_iq

   require (mem_iq.issueWidth == 1)
   val iss_loadIssued =
      mem_iq.io.iss_valids(0) &&
      mem_iq.io.iss_uops(0).is_load &&
      !mem_iq.io.iss_uops(0).fp_val &&
      mem_iq.io.iss_uops(0).pdst =/= 0.U &&
      !(sxt_ldMiss && (mem_iq.io.iss_uops(0).iw_p1_poisoned || mem_iq.io.iss_uops(0).iw_p2_poisoned))
   sxt_ldMiss :=
      ((lsu.io.nack.valid && lsu.io.nack.isload) || dc_shim.io.core.load_miss) &&
      Pipe(true.B, iss_loadIssued, 4).bits
   issue_units.map(_.io.sxt_ldMiss := sxt_ldMiss)

   // Check that IF we see a speculative load-wakeup and NO load-miss, then we should
   // see a writeback to the register file!

   // Share the memory port with other long latency operations.
   val mem_unit = exe_units.memory_unit
   val mem_resp = mem_unit.io.ll_iresp
   mem_unit.io.com_exception := rob.io.flush.valid

   when (RegNext(!sxt_ldMiss) && RegNext(RegNext(lsu.io.mem_ldSpecWakeup.valid)) &&
      !(RegNext(rob.io.flush.valid || (br_unit.brinfo.valid && br_unit.brinfo.mispredict))) &&
      !(RegNext(RegNext(rob.io.flush.valid || (br_unit.brinfo.valid && br_unit.brinfo.mispredict)))))
   {
      assert (mem_resp.valid && mem_resp.bits.uop.ctrl.rf_wen && mem_resp.bits.uop.dst_rtype === RT_FIX,
         "[core] We did not see a RF writeback for a speculative load that claimed no load-miss.")
   }

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

   for (w <- 0 until exe_units.numIrfReaders)
   {
      iregister_read.io.iss_valids(w) :=
         iss_valids(w) && !(sxt_ldMiss && (iss_uops(w).iw_p1_poisoned || iss_uops(w).iw_p2_poisoned))
   }
   iregister_read.io.iss_uops := iss_uops
   iregister_read.io.iss_uops map { u => u.iw_p1_poisoned := false.B; u.iw_p2_poisoned := false.B }

   iregister_read.io.brinfo := br_unit.brinfo
   iregister_read.io.kill   := rob.io.flush.valid

   iregister_read.io.bypass := bypasses

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

   // Extra I/O
   csr.io.retire    := PopCount(rob.io.commit.valids.asUInt)
   csr.io.exception := rob.io.com_xcpt.valid
   // csr.io.pc used for setting EPC during exception or CSR.io.trace.
   csr.io.pc        := (boom.util.AlignPCToBoundary(io.ifu.com_fetch_pc, icBlockBytes)
                      + rob.io.com_xcpt.bits.pc_lob
                      - Mux(rob.io.com_xcpt.bits.edge_inst, 2.U, 0.U))
   // Cause not valid for for CALL or BREAKPOINTs (CSRFile will override it).
   csr.io.cause     := rob.io.com_xcpt.bits.cause
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
      encodeVirtualAddress(rob.io.com_xcpt.bits.badvaddr, rob.io.com_xcpt.bits.badvaddr), 0.U)

   // TODO move this function to some central location (since this is used elsewhere).
   def encodeVirtualAddress(a0: UInt, ea: UInt) = if (vaddrBitsExtended == vaddrBits)
   {
     ea
   }
   else
   {
      // Efficient means to compress 64-bit VA into vaddrBits+1 bits.
      // (VA is bad if VA(vaddrBits) != VA(vaddrBits-1)).
      val a = a0.asSInt >> vaddrBits
      val msb = Mux(a === 0.S || a === -1.S, ea(vaddrBits), !ea(vaddrBits-1))
      Cat(msb, ea(vaddrBits-1,0))
   }

   // reading requires serializing the entire pipeline
   csr.io.fcsr_flags.valid := rob.io.commit.fflags.valid
   csr.io.fcsr_flags.bits  := rob.io.commit.fflags.bits

   exe_units.withFilter(_.has_fcsr).map(_.io.fcsr_rm := csr.io.fcsr_rm)
   io.fcsr_rm := csr.io.fcsr_rm

   if (usingFPU)
   {
      fp_pipeline.io.fcsr_rm := csr.io.fcsr_rm
   }

   csr.io.hartid := io.hartid
   csr.io.interrupts := io.interrupts

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
   for (w <- 0 until exe_units.length)
   {
      val exe_unit = exe_units(w)
      if (exe_unit.readsIrf)
      {
         exe_unit.io.req <> iregister_read.io.exe_reqs(iss_idx)

         if (exe_unit.bypassable)
         {
            for (i <- 0 until exe_unit.numBypassStages)
            {
               bypasses.valid(bypass_idx) := exe_unit.io.bypass.valid(i)
               bypasses.uop(bypass_idx)   := exe_unit.io.bypass.uop(i)
               bypasses.data(bypass_idx)  := exe_unit.io.bypass.data(i)
               bypass_idx += 1
            }
         }
         iss_idx += 1
      }

   }
   require (bypass_idx == exe_units.numTotalBypassPorts)

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Load/Store Unit ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // enqueue basic load/store info in Decode
   lsu.io.dec_uops := dec_uops

   for (w <- 0 until coreWidth)
   {
      // Decoding instructions request load/store queue entries when they can proceed.
      lsu.io.dec_ld_vals(w) := dec_will_fire(w) && dec_uops(w).is_load
      lsu.io.dec_st_vals(w) := dec_will_fire(w) && dec_uops(w).is_store

      lsu.io.dec_uops(w).rob_idx := dec_uops(w).rob_idx // for debug purposes (commit logging)
   }

   lsu.io.commit_store_mask := rob.io.commit.st_mask
   lsu.io.commit_load_mask  := rob.io.commit.ld_mask
   lsu.io.commit_load_at_rob_head := rob.io.com_load_is_at_rob_head

   //com_xcpt.valid comes too early, will fight against a branch that resolves same cycle as an exception
   lsu.io.exception := rob.io.flush.valid

   // Handle Branch Mispeculations
   lsu.io.brinfo := br_unit.brinfo
   dc_shim.io.core.brinfo := br_unit.brinfo

   lsu.io.debug_tsc := debug_tsc_reg

   dc_shim.io.core.flush_pipe := rob.io.flush.valid

   lsu.io.nack <> dc_shim.io.core.nack

   lsu.io.dmem_req_ready := dc_shim.io.core.req.ready
   lsu.io.dmem_is_ordered:= dc_shim.io.core.ordered
   lsu.io.release := io.release

   if (usingFPU)
   {
      lsu.io.fp_stdata <> fp_pipeline.io.tosdq
   }

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Writeback Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   var w_cnt = 1
   // 0th goes to ll_wbarb
   iregfile.io.write_ports(0) := WritePort(ll_wbarb.io.out, IPREG_SZ, xLen)
   for (i <- 0 until exe_units.length)
   {
      if (exe_units(i).writesIrf)
      {
         val wbresp = exe_units(i).io.iresp
         val wbpdst = wbresp.bits.uop.pdst
         val wbdata = wbresp.bits.data

         def wbIsValid(rtype: UInt) =
            wbresp.valid && wbresp.bits.uop.ctrl.rf_wen && wbresp.bits.uop.dst_rtype === rtype
         val wbReadsCSR = wbresp.bits.uop.ctrl.csr_cmd =/= freechips.rocketchip.rocket.CSR.N

         iregfile.io.write_ports(w_cnt).valid     := wbIsValid(RT_FIX)
         iregfile.io.write_ports(w_cnt).bits.addr := wbpdst
         wbresp.ready := true.B
         if (exe_units(i).usesCsrWport)
         {
            iregfile.io.write_ports(w_cnt).bits.data := Mux(wbReadsCSR, csr.io.rw.rdata, wbdata)
         }
         else
         {
            iregfile.io.write_ports(w_cnt).bits.data := wbdata
         }

         assert (!wbIsValid(RT_FLT), "[fppipeline] An FP writeback is being attempted to the Int Regfile.")

         assert (!(wbresp.valid &&
            !wbresp.bits.uop.ctrl.rf_wen &&
            wbresp.bits.uop.dst_rtype === RT_FIX),
            "[fppipeline] An Int writeback is being attempted with rf_wen disabled.")

         assert (!(wbresp.valid &&
            wbresp.bits.uop.ctrl.rf_wen &&
            wbresp.bits.uop.dst_rtype =/= RT_FIX),
            "[fppipeline] writeback being attempted to Int RF with dst != Int type exe_units("+i+").iresp")
         w_cnt += 1
      }
   }
   require(w_cnt == iregfile.io.write_ports.length)
   ll_wbarb.io.in(0) <> mem_resp
   assert (ll_wbarb.io.in(0).ready) // never backpressure the memory unit.

   if (usingFPU)
   {
      // Connect IFPU
      fp_pipeline.io.fromint  <> exe_units.ifpu_unit.io.ll_fresp
      // Connect FPIU
      ll_wbarb.io.in(1)       <> fp_pipeline.io.toint
      // Connect FLDs
      fp_pipeline.io.ll_wport <> exe_units.memory_unit.io.ll_fresp
   }
   if (usingRoCC)
   {
      require(usingFPU)
      ll_wbarb.io.in(2)       <> exe_units.rocc_unit.io.ll_iresp
   }

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Commit Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // Dispatch
   rob.io.enq_valids := rename_stage.io.ren1_mask
   rob.io.enq_uops   := rename_stage.io.ren1_uops
   rob.io.enq_new_packet := dec_finished_mask === 0.U
   rob.io.enq_partial_stall := dec_last_inst_was_stalled // TODO come up with better ROB compacting scheme.
   rob.io.debug_tsc := debug_tsc_reg
   rob.io.csr_stall := csr.io.csr_stall

   assert ((dec_will_fire zip rename_stage.io.ren1_mask map {case(d,r) => d === r}).reduce(_|_),
      "[core] Assumption that dec_will_fire and ren1_mask are equal is being violated.")

   // Writeback
   // ---------
   // First connect the ll_wport
   val ll_uop = ll_wbarb.io.out.bits.uop
   rob.io.wb_resps(0).valid  := ll_wbarb.io.out.valid && !(ll_uop.is_store && !ll_uop.is_amo)
   rob.io.wb_resps(0).bits   <> ll_wbarb.io.out.bits
   rob.io.debug_wb_valids(0) := ll_wbarb.io.out.valid && ll_uop.dst_rtype =/= RT_X
   rob.io.debug_wb_wdata(0)  := ll_wbarb.io.out.bits.data
   var cnt = 1
   var f_cnt = 0 // rob fflags port index
   for (eu <- exe_units)
   {
      if (eu.writesIrf)
      {
         val resp   = eu.io.iresp
         val wb_uop = resp.bits.uop
         val data   = resp.bits.data

         rob.io.wb_resps(cnt).valid := resp.valid && !(wb_uop.is_store && !wb_uop.is_amo)
         rob.io.wb_resps(cnt).bits  <> resp.bits
         rob.io.debug_wb_valids(cnt) := resp.valid && wb_uop.ctrl.rf_wen && wb_uop.dst_rtype === RT_FIX
         if (eu.hasFFlags)
         {
            rob.io.fflags(f_cnt) <> resp.bits.fflags
            f_cnt += 1
         }
         if (eu.usesCsrWport)
         {
            rob.io.debug_wb_wdata(cnt) := Mux(wb_uop.ctrl.csr_cmd =/= freechips.rocketchip.rocket.CSR.N,
               csr.io.rw.rdata,
               data)
         }
         else
         {
            rob.io.debug_wb_wdata(cnt) := data
         }
         cnt += 1

      }
   }

   require(cnt == numIrfWritePorts + 1)
   if (usingFPU) {
      for ((wdata, wakeup) <- fp_pipeline.io.debug_wb_wdata zip fp_pipeline.io.wakeups)
      {
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
   rob.io.brinfo <> br_unit.brinfo

   // branch unit requests PCs and predictions from ROB during register read
   // (fetch PC from ROB cycle earlier than needed for critical path reasons)
   io.ifu.get_pc.ftq_idx := RegNext(iss_uops(brunit_idx).ftq_idx)
   exe_units(brunit_idx).io.get_ftq_pc.fetch_pc       := RegNext(io.ifu.get_pc.fetch_pc)
   exe_units(brunit_idx).io.get_ftq_pc.next_val       := RegNext(io.ifu.get_pc.next_val)
   exe_units(brunit_idx).io.get_ftq_pc.next_pc        := RegNext(io.ifu.get_pc.next_pc)
   exe_units(brunit_idx).io.status := csr.io.status

   // LSU <> ROB
   rob.io.lsu_clr_bsy_valid      := lsu.io.clr_bsy_valid
   rob.io.lsu_clr_bsy_rob_idx    := lsu.io.clr_bsy_rob_idx
   rob.io.lsu_clr_unsafe_valid   := lsu.io.clr_unsafe_valid
   rob.io.lsu_clr_unsafe_rob_idx := lsu.io.clr_unsafe_rob_idx
   rob.io.lxcpt <> lsu.io.xcpt

   assert (!(csr.io.singleStep), "[core] single-step is unsupported.")

   rob.io.bxcpt <> br_unit.xcpt

   //-------------------------------------------------------------
   // **** Flush Pipeline ****
   //-------------------------------------------------------------
   // flush on exceptions, miniexeptions, and after some special instructions

   if (usingFPU) {
      fp_pipeline.io.flush_pipeline := rob.io.flush.valid
   }

   for (w <- 0 until exe_units.length)
   {
      exe_units(w).io.req.bits.kill := rob.io.flush.valid
   }

   assert (!(RegNext(rob.io.com_xcpt.valid) && !rob.io.flush.valid),
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
         reset.toBool) {
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

   if (DEBUG_PRINTF)
   {
      println("\n Chisel Printout Enabled\n")

      val numFtqWhitespace = if (DEBUG_PRINTF_FTQ) (ftqSz/4)+1 else 0
      val fetchWhitespace = if (fetchWidth >= 8) 2 else 0
       var whitespace = (debugScreenheight - 25 + 3 -10 + 3 + 4 - coreWidth - (NUM_LDQ_ENTRIES max NUM_STQ_ENTRIES) -
         issueParams.map(_.numEntries).sum - issueParams.length - (numRobEntries/coreWidth) -
         numFtqWhitespace - fetchWhitespace
     )

      println("Whitespace padded: " + whitespace)

      printf("--- Cyc=%d , ----------------- Ret: %d ----------------------------------",
             debug_tsc_reg,
             debug_irt_reg & (0xffffff).U)

      for (w <- 0 until coreWidth)
      {
         if (w == 0)
         {
            printf("\n  Dec:  ([0x%x]                        ", dec_uops(w).pc(19,0))
         }
         else
         {
            printf("[0x%x]                        ", dec_uops(w).pc(19,0))
         }
      }

      for (w <- 0 until coreWidth)
      {
         printf("(%c%c) " + "DASM(%x)" + " |  ",
                Mux(io.ifu.fetchpacket.valid && dec_fbundle.uops(w).valid && !dec_finished_mask(w), Str("v"), Str("-")),
                Mux(dec_will_fire(w), Str("V"), Str("-")),
                dec_fbundle.uops(w).bits.debug_inst
                )
      }

      for (w <- 0 until coreWidth)
      {
         if (w == 0)
         {
            printf("\n  Ren:  ([0x%x]                        ", rename_stage.io.ren2_uops(w).pc(19,0))
         }
         else
         {
            printf("[0x%x]                        ", rename_stage.io.ren2_uops(w).pc(19,0))
         }
      }

      for (w <- 0 until coreWidth)
      {
         printf(" (%c) " + "DASM(%x)" + " |  ",
                Mux(rename_stage.io.ren2_mask(w), Str("V"), Str("-")),
                rename_stage.io.ren2_uops(w).debug_inst
                )
      }

      printf(") fin(%x)\n", dec_finished_mask)
      for (w <- 0 until coreWidth)
      {
         val ren_uop = dispatcher.io.ren_uops(w).bits
         printf("        [ISA:%d,%d,%d,%d] [Phs:%d(%c)%d[%c](%c)%d[%c](%c)%d[%c](%c)] ",
                ren_uop.ldst,
                ren_uop.lrs1,
                ren_uop.lrs2,
                ren_uop.lrs3,
                ren_uop.pdst,
                Mux(ren_uop.dst_rtype   === RT_FIX, Str("X"),
                  Mux(ren_uop.dst_rtype === RT_X  , Str("-"),
                  Mux(ren_uop.dst_rtype === RT_FLT, Str("f"),
                  Mux(ren_uop.dst_rtype === RT_PAS, Str("C"), Str("?"))))),
                ren_uop.pop1,
                Mux(rename_stage.io.ren2_uops(w).prs1_busy, Str("B"), Str("R")),
                Mux(ren_uop.lrs1_rtype    === RT_FIX, Str("X"),
                   Mux(ren_uop.lrs1_rtype === RT_X  , Str("-"),
                   Mux(ren_uop.lrs1_rtype === RT_FLT, Str("f"),
                   Mux(ren_uop.lrs1_rtype === RT_PAS, Str("C"), Str("?"))))),
                ren_uop.pop2,
                Mux(rename_stage.io.ren2_uops(w).prs2_busy, Str("B"), Str("R")),
                Mux(ren_uop.lrs2_rtype    === RT_FIX, Str("X"),
                   Mux(ren_uop.lrs2_rtype === RT_X  , Str("-"),
                   Mux(ren_uop.lrs2_rtype === RT_FLT, Str("f"),
                   Mux(ren_uop.lrs2_rtype === RT_PAS, Str("C"), Str("?"))))),
                ren_uop.pop3,
                Mux(rename_stage.io.ren2_uops(w).prs3_busy, Str("B"), Str("R")),
                Mux(ren_uop.frs3_en, Str("f"), Str("-"))
                )
      }

      if (DEBUG_PRINTF_ROB)
      {
         printf("\n) ctate: (%c: %c %c %c %c %c %c) BMsk:%x Mode:%c\n",
                Mux(rob.io.debug.state === 0.U, Str("R"),
                Mux(rob.io.debug.state === 1.U, Str("N"),
                Mux(rob.io.debug.state === 2.U, Str("B"),
                Mux(rob.io.debug.state === 3.U, Str("W"),
                                                    Str(" "))))),
                Mux(rob.io.ready,Str("_"), Str("!")),
                Mux(lsu.io.laq_full(0), Str("L"), Str("_")),
                Mux(lsu.io.stq_full(0), Str("S"), Str("_")),
                Mux(rob.io.flush.valid, Str("F"), Str(" ")),
                Mux(branch_mask_full.reduce(_|_), Str("B"), Str(" ")),
                Mux(dc_shim.io.core.req.ready, Str("R"), Str("B")),
                dec_brmask_logic.io.debug.branch_mask,
                Mux(csr.io.status.prv === (0x3).U, Str("M"),
                Mux(csr.io.status.prv === (0x0).U, Str("U"),
                Mux(csr.io.status.prv === (0x1).U, Str("S"),  //2 is H
                                                      Str("?"))))
                )
      }

      printf("Exct(%c%d) Commit(%x) fl: 0x%x (%d) is: 0x%x (%d)\n",
             Mux(rob.io.com_xcpt.valid, Str("E"), Str("-")),
             rob.io.com_xcpt.bits.cause,
             rob.io.commit.valids.asUInt,
             rename_stage.io.debug.ifreelist,
             PopCount(rename_stage.io.debug.ifreelist),
             rename_stage.io.debug.iisprlist,
             PopCount(rename_stage.io.debug.iisprlist)
             )

      printf("                                      fl: 0x%x (%d) is: 0x%x (%d)\n",
             rename_stage.io.debug.ffreelist,
             PopCount(rename_stage.io.debug.ffreelist),
             rename_stage.io.debug.fisprlist,
             PopCount(rename_stage.io.debug.fisprlist)
             )

      // branch unit
      printf("                          Branch Unit: %c,%c,%d  NPC=%d,0x%x\n",
             Mux(br_unit.brinfo.valid,Str("V"), Str(" ")),
             Mux(br_unit.brinfo.mispredict, Str("M"), Str(" ")),
             br_unit.brinfo.taken,
             exe_units(brunit_idx).io.get_ftq_pc.next_val,
             exe_units(brunit_idx).io.get_ftq_pc.next_pc(19,0)
             )

      // Rename Map Tables / ISA Register File
      val xpr_to_string =
              VecInit(Str(" x0"), Str(" ra"), Str(" sp"), Str(" gp"),
                   Str(" tp"), Str(" t0"), Str(" t1"), Str(" t2"),
                   Str(" s0"), Str(" s1"), Str(" a0"), Str(" a1"),
                   Str(" a2"), Str(" a3"), Str(" a4"), Str(" a5"),
                   Str(" a6"), Str(" a7"), Str(" s2"), Str(" s3"),
                   Str(" s4"), Str(" s5"), Str(" s6"), Str(" s7"),
                   Str(" s8"), Str(" s9"), Str("s10"), Str("s11"),
                   Str(" t3"), Str(" t4"), Str(" t5"), Str(" t6"))

      val fpr_to_string =
              VecInit( Str("ft0"), Str("ft1"), Str("ft2"), Str("ft3"),
                   Str("ft4"), Str("ft5"), Str("ft6"), Str("ft7"),
                   Str("fs0"), Str("fs1"), Str("fa0"), Str("fa1"),
                   Str("fa2"), Str("fa3"), Str("fa4"), Str("fa5"),
                   Str("fa6"), Str("fa7"), Str("fs2"), Str("fs3"),
                   Str("fs4"), Str("fs5"), Str("fs6"), Str("fs7"),
                   Str("fs8"), Str("fs9"), Str("fs10"), Str("fs11"),
                   Str("ft8"), Str("ft9"), Str("ft10"), Str("ft11"))

      for (x <- 0 until whitespace)
      {
         printf("|\n")
      }
   } // End DEBUG_PRINTF


   if (COMMIT_LOG_PRINTF)
   {
      var new_commit_cnt = 0.U
      for (w <- 0 until coreWidth)
      {
         val priv = csr.io.status.prv

         // To allow for diffs against spike :/
         def printf_inst(uop: MicroOp) = {
            when (uop.is_rvc)
            {
               printf("(0x%x)", uop.debug_inst(15,0))
            }
            .otherwise {
               printf("(0x%x)", uop.debug_inst)
            }
         }
         when (rob.io.commit.valids(w))
         {
            when (rob.io.commit.uops(w).dst_rtype === RT_FIX && rob.io.commit.uops(w).ldst =/= 0.U)
            {
               printf("%d 0x%x ",
                  priv, Sext(rob.io.commit.uops(w).pc(vaddrBits-1,0), xLen))
               printf_inst(rob.io.commit.uops(w))
               printf(" x%d 0x%x\n",
                  rob.io.commit.uops(w).ldst, rob.io.commit.uops(w).debug_wdata)

            }
            .elsewhen (rob.io.commit.uops(w).dst_rtype === RT_FLT)
            {
               printf("%d 0x%x ",
                  priv, Sext(rob.io.commit.uops(w).pc(vaddrBits-1,0), xLen))
               printf_inst(rob.io.commit.uops(w))
               printf(" f%d 0x%x\n",
                  rob.io.commit.uops(w).ldst, rob.io.commit.uops(w).debug_wdata)
            }
            .otherwise
            {
               printf("%d 0x%x ",
                  priv, Sext(rob.io.commit.uops(w).pc(vaddrBits-1,0), xLen))
               printf_inst(rob.io.commit.uops(w))
               printf("\n")
            }
         }
      }
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
   // Pipeview Visualization

   if (O3PIPEVIEW_PRINTF)
   {
      println("   O3Pipeview Visualization Enabled\n")

      // did we already print out the instruction sitting at the front of the fetchbuffer/decode stage?
      val dec_printed_mask = RegInit(0.U(coreWidth.W))

      for (w <- 0 until coreWidth)
      {
         when (dec_valids(w) && !dec_printed_mask(w))
         {
            printf("%d; O3PipeView:decode:%d\n", dec_uops(w).debug_events.fetch_seq, debug_tsc_reg)
         }
         // Rename begins when uop leaves fetch buffer (Dec+Ren1 are in same stage).
         when (dec_will_fire(w))
         {
            printf("%d; O3PipeView:rename: %d\n", dec_uops(w).debug_events.fetch_seq, debug_tsc_reg)
         }
         when (dispatcher.io.ren_uops(w).valid)
         {
            printf("%d; O3PipeView:dispatch: %d\n", dispatcher.io.ren_uops(w).bits.debug_events.fetch_seq, debug_tsc_reg)
         }

         when (dec_rdy || io.ifu.clear_fetchbuffer)
         {
            dec_printed_mask := 0.U
         }
         .otherwise
         {
            dec_printed_mask := dec_valids.asUInt | dec_printed_mask
         }
      }

      for (i <- 0 until coreWidth)
      {
         when (rob.io.commit.valids(i))
         {
            printf("%d; O3PipeView:retire:%d:store: 0\n",
               rob.io.commit.uops(i).debug_events.fetch_seq,
               debug_tsc_reg)
         }
      }
   }

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // Page Table Walker

   io.ptw_tlb <> lsu.io.ptw
   io.ptw.ptbr       := csr.io.ptbr
   io.ptw.status     := csr.io.status
   io.ptw.pmp        := csr.io.pmp
   io.ptw.sfence     := io.ifu.sfence

   //-------------------------------------------------------------
   //-------------------------------------------------------------

   io.rocc := DontCare
   if (usingRoCC)
   {
      exe_units.rocc_unit.io.rocc.rocc         <> io.rocc
      exe_units.rocc_unit.io.rocc.dec_uops     := dec_uops
      exe_units.rocc_unit.io.rocc.rob_head_idx := rob.io.rob_head_idx
      exe_units.rocc_unit.io.rocc.rob_pnr_idx  := rob.io.rob_pnr_idx
      exe_units.rocc_unit.io.com_exception     := rob.io.com_xcpt.valid
      exe_units.rocc_unit.io.status            := csr.io.status
      for (w <- 0 until coreWidth)
      {
         exe_units.rocc_unit.io.rocc.dec_rocc_vals(w) := (
            dec_will_fire(w) &&
            rename_stage.io.inst_can_proceed(w) &&
            !rob.io.flush.valid &&
            dec_uops(w).uopc === uopROCC)
      }
   }

   //io.trace := csr.io.trace unused
   if (p(BoomTilesKey)(0).trace)
   {
      for (w <- 0 until coreWidth)
      {
         io.trace(w).valid      := rob.io.commit.valids(w)
         io.trace(w).iaddr      := Sext(rob.io.commit.uops(w).pc(vaddrBits-1,0), xLen)
         io.trace(w).insn       := rob.io.commit.uops(w).debug_inst
         // I'm uncertain the commit signals from the ROB match these CSR exception signals
         io.trace(w).priv       := csr.io.status.prv
         io.trace(w).exception  := csr.io.exception
         io.trace(w).interrupt  := csr.io.interrupt
         io.trace(w).cause      := csr.io.cause
         io.trace(w).tval       := csr.io.tval
      }
      dontTouch(io.trace)
   }
   else
   {
      io.trace := DontCare
      io.trace map (t => t.valid := false.B)
   }
}
