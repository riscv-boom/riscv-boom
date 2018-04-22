//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISC-V Processor Core
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// BOOM has the following (conceptual) stages:
//   if0 - Instruction Fetch 0 (next-pc select)
//   if1 - Instruction Fetch 1 (I$ access)
//   if2 - Instruction Fetch 2 (instruction return)
//   if3 - Instruction Fetch 3 (enqueue to fetch buffer)
//   dec - Decode
//   ren - Rename
//   dis - Dispatch
//   iss - Issue
//   rrd - Register Read
//   exe - Execute
//   mem - Memory
//   wb  - Writeback
//   com - Commit



package boom.exu

import Chisel._
import chisel3.core.DontCare
import chisel3.experimental.dontTouch
import freechips.rocketchip.config.Parameters

import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.rocket.Causes
import freechips.rocketchip.util.Str
import boom.common._
import boom.exu.FUConstants._
import boom.util.{GetNewUopAndBrMask, Sext, WrapInc}


//-------------------------------------------------------------
//-------------------------------------------------------------

trait HasBoomCoreIO extends freechips.rocketchip.tile.HasTileParameters {
   implicit val p: Parameters
   val io = new freechips.rocketchip.tile.CoreBundle()(p)
      with freechips.rocketchip.tile.HasExternallyDrivenTileConstants {
         val interrupts = new freechips.rocketchip.tile.CoreInterrupts().asInput
         val ifu = new boom.ifu.BoomFrontendIO
         val dmem = new freechips.rocketchip.rocket.HellaCacheIO
         val ptw = new freechips.rocketchip.rocket.DatapathPTWIO().flip
         val fpu = new freechips.rocketchip.tile.FPUCoreIO().flip
         val rocc = new freechips.rocketchip.tile.RoCCCoreIO().flip
         val ptw_tlb = new freechips.rocketchip.rocket.TLBPTWIO()
         val trace = Vec(coreParams.retireWidth,
            new freechips.rocketchip.rocket.TracedInstruction).asOutput
   }
}

class BoomCore(implicit p: Parameters, edge: freechips.rocketchip.tilelink.TLEdgeOut) extends BoomModule()(p)
   with HasBoomCoreIO
   with freechips.rocketchip.tile.HasFPUParameters
{
   //**********************************
   // construct all of the modules

   // Only holds integer-registerfile execution units.
   val exe_units = new boom.exu.ExecutionUnits(fpu=false)
   // Meanwhile, the FP pipeline holds the FP issue window, FP regfile, and FP arithmetic units.
   var fp_pipeline: FpPipeline = null
   if (usingFPU) fp_pipeline = Module(new FpPipeline())

   val num_irf_write_ports = exe_units.map(_.num_rf_write_ports).sum
   val num_fast_wakeup_ports = exe_units.count(_.isBypassable)
   val num_wakeup_ports = num_irf_write_ports + num_fast_wakeup_ports
   val decode_units     = for (w <- 0 until decodeWidth) yield { val d = Module(new DecodeUnit); d }
   val dec_brmask_logic = Module(new BranchMaskGenerationLogic(decodeWidth))
   val rename_stage     = Module(new RenameStage(decodeWidth, num_wakeup_ports, fp_pipeline.io.wakeups.length))
   val issue_units      = new boom.exu.IssueUnits(num_wakeup_ports)
   val iregfile         = if (regreadLatency == 1 && enableCustomRf) {
                              Module(new RegisterFileSeqCustomArray(numIntPhysRegs,
                                 exe_units.withFilter(_.usesIRF).map(e => e.num_rf_read_ports).sum,
                                 exe_units.withFilter(_.usesIRF).map(e => e.num_rf_write_ports).sum,
                                 xLen,
                                 exe_units.bypassable_write_port_mask))
                          } else {
                              Module(new RegisterFileBehavorial(numIntPhysRegs,
                                 exe_units.withFilter(_.usesIRF).map(e => e.num_rf_read_ports).sum,
                                 exe_units.withFilter(_.usesIRF).map(e => e.num_rf_write_ports).sum,
                                 xLen,
                                 exe_units.bypassable_write_port_mask))
                          }
   val ll_wbarb         = Module(new Arbiter(new ExeUnitResp(xLen), 2))
   val iregister_read   = Module(new RegisterRead(
                                 issue_units.map(_.issue_width).sum,
                                 exe_units.withFilter(_.usesIRF).map(_.supportedFuncUnits),
                                 exe_units.withFilter(_.usesIRF).map(_.num_rf_read_ports).sum,
                                 exe_units.withFilter(_.usesIRF).map(_.num_rf_read_ports),
                                 exe_units.num_total_bypass_ports,
                                 xLen))
   val dc_shim          = Module(new boom.lsu.DCacheShim())
   val lsu              = Module(new boom.lsu.LoadStoreUnit(decodeWidth))
   val rob              = Module(new Rob(
                                 decodeWidth,
                                 NUM_ROB_ENTRIES,
                                 num_irf_write_ports + fp_pipeline.io.wakeups.length,
                                 exe_units.num_fpu_ports + fp_pipeline.io.wakeups.length))
   // Used to wakeup registers in rename and issue. ROB needs to listen to something else.
   val int_wakeups      = Wire(Vec(num_wakeup_ports, Valid(new ExeUnitResp(xLen))))

   require (exe_units.length == issue_units.map(_.issue_width).sum)

   //***********************************
   // Pipeline State Registers and Wires

   // Instruction Decode Stage
   val dec_valids     = Wire(Vec(decodeWidth, Bool()))  // are the decoded instruction valid? It may be held up though.
   val dec_uops       = Wire(Vec(decodeWidth, new MicroOp()))
   val dec_will_fire  = Wire(Vec(decodeWidth, Bool()))  // can the instruction fire beyond decode?
                                                         // (can still be stopped in ren or dis)
   val dec_rdy        = Wire(Bool())

   // Dispatch Stage
   val dis_valids     = Wire(Vec(DISPATCH_WIDTH, Bool())) // true if uop WILL enter IW
   val dis_uops       = Wire(Vec(DISPATCH_WIDTH, new MicroOp()))

   // Issue Stage/Register Read
   val iss_valids     = Wire(Vec(exe_units.length, Bool()))
   val iss_uops       = Wire(Vec(exe_units.length, new MicroOp()))
   val bypasses       = Wire(new BypassData(exe_units.num_total_bypass_ports, xLen))

   // Branch Unit
   val br_unit = Wire(new BranchUnitResp())
   val brunit_idx = exe_units.br_unit_idx
   br_unit <> exe_units.br_unit_io

   // Shim to DCache
   io.dmem <> dc_shim.io.dmem
   dc_shim.io.core <> exe_units.memory_unit.io.dmem
   dc_shim.io.core.invalidate_lr := rob.io.com_xcpt.valid

   // Load/Store Unit & ExeUnits
   exe_units.memory_unit.io.lsu_io := lsu.io





   //-------------------------------------------------------------
   // Uarch Hardware Performance Events (HPEs)


   val perfEvents = new freechips.rocketchip.rocket.EventSets(Seq(
      //new EventSet((mask, hits) => Mux(mask(0), wb_xcpt, wb_valid && pipelineIDToWB((mask & hits).orR)), Seq(
      new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
         ("exception", () => rob.io.com_xcpt.valid),
         ("nop", () => false.B), // ("load", () => id_ctrl.mem && id_ctrl.mem_cmd === M_XRD && !id_ctrl.fp),
         ("nop", () => false.B), // ("store", () => id_ctrl.mem && id_ctrl.mem_cmd === M_XWR && !id_ctrl.fp),
         ("nop", () => false.B))), // ("amo", () => Bool(usingAtomics) && id_ctrl.mem && (isAMO(id_ctrl.mem_cmd) || id_ctrl.mem_cmd.isOneOf(M_XLR, M_XSC))),
//         ("system", () => =/= CSR.N))),
//       ("arith", () => id_ctrl.wxd && !(id_ctrl.jal || id_ctrl.jalr || id_ctrl.mem || id_ctrl.fp || id_ctrl.div || id_ctrl.csr =/= CSR.N)),
//       ("branch", () => id_ctrl.branch),
//       ("jal", () => id_ctrl.jal),
//       ("jalr", () => id_ctrl.jalr))
//       ++ (if (!usingMulDiv) Seq() else Seq(
//         ("mul", () => id_ctrl.div && (id_ctrl.alu_fn & ALU.FN_DIV) =/= ALU.FN_DIV),
//         ("div", () => id_ctrl.div && (id_ctrl.alu_fn & ALU.FN_DIV) === ALU.FN_DIV)))
//       ++ (if (!usingFPU) Seq() else Seq(
//         ("fp load", () => id_ctrl.fp && io.fpu.dec.ldst && io.fpu.dec.wen),
//         ("fp store", () => id_ctrl.fp && io.fpu.dec.ldst && !io.fpu.dec.wen),
//         ("fp add", () => id_ctrl.fp && io.fpu.dec.fma && io.fpu.dec.swap23),
//         ("fp mul", () => id_ctrl.fp && io.fpu.dec.fma && !io.fpu.dec.swap23 && !io.fpu.dec.ren3),
//         ("fp mul-add", () => id_ctrl.fp && io.fpu.dec.fma && io.fpu.dec.ren3),
//         ("fp div/sqrt", () => id_ctrl.fp && (io.fpu.dec.div || io.fpu.dec.sqrt)),
//         ("fp other", () => id_ctrl.fp && !(io.fpu.dec.ldst || io.fpu.dec.fma || io.fpu.dec.div || io.fpu.dec.sqrt))))),
      new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
//       ("load-use interlock", () => id_ex_hazard && ex_ctrl.mem || id_mem_hazard && mem_ctrl.mem || id_wb_hazard && wb_ctrl.mem),
//       ("long-latency interlock", () => id_sboard_hazard),
//       ("csr interlock", () => id_ex_hazard && ex_ctrl.csr =/= CSR.N || id_mem_hazard && mem_ctrl.csr =/= CSR.N || id_wb_hazard && wb_ctrl.csr =/= CSR.N),
         ("I$ blocked", () => icache_blocked),
         ("nop", () => false.B),  //("D$ blocked", () => id_ctrl.mem && dcache_blocked),
         ("branch misprediction", () => br_unit.brinfo.mispredict),
         ("control-flow target misprediction", () =>  br_unit.brinfo.mispredict && br_unit.brinfo.is_jr),
         ("flush", () => rob.io.flush.valid),
//       ++ (if (!usingMulDiv) Seq() else Seq(
//         ("mul/div interlock", () => id_ex_hazard && ex_ctrl.div || id_mem_hazard && mem_ctrl.div || id_wb_hazard && wb_ctrl.div)))
//       ++ (if (!usingFPU) Seq() else Seq(
//         ("fp interlock", () => id_ex_hazard && ex_ctrl.fp || id_mem_hazard && mem_ctrl.fp || id_wb_hazard && wb_ctrl.fp || id_ctrl.fp && id_stall_fpu)))),
       ("branch resolved", () => br_unit.brinfo.valid))),
     new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
       ("I$ miss", () => io.ifu.perf.acquire),
       ("D$ miss", () => io.dmem.perf.acquire),
       ("D$ release", () => io.dmem.perf.release),
       ("ITLB miss", () => io.ifu.perf.tlbMiss),
       ("DTLB miss", () => io.dmem.perf.tlbMiss),
       ("L2 TLB miss", () => io.ptw.perf.l2miss)))))


   val csr = Module(new freechips.rocketchip.rocket.CSRFile(perfEvents))

   // evaluate performance counters
   val icache_blocked = !(io.ifu.fetchpacket.valid || RegNext(io.ifu.fetchpacket.valid))
   csr.io.counters foreach { c => c.inc := RegNext(perfEvents.evaluate(c.eventSel)) }




   //****************************************
   // Time Stamp Counter & Retired Instruction Counter
   // (only used for printf and vcd dumps - the actual counters are in the CSRFile)
   val debug_tsc_reg  = Reg(init = UInt(0, xLen))
   val debug_irt_reg  = Reg(init = UInt(0, xLen))
   debug_tsc_reg  := debug_tsc_reg + Mux(Bool(O3PIPEVIEW_PRINTF), UInt(O3_CYCLE_TIME), UInt(1))
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
      if (usingFPU) ""
      else fp_pipeline.fp_string
   val rob_str = rob.toString

   override def toString: String =
   ( exe_units_str + "\n"
   + fp_pipeline_str + "\n"
   + rob_str + "\n"
   + (if (usingFPU)      ("\n    FPU Unit Enabled") else  ("\n    FPU Unit Disabled"))
   + (if (usingVM)       ("\n    VM       Enabled") else  ("\n    VM       Disabled"))
   + (if (usingFDivSqrt) ("\n    FDivSqrt Enabled") else  ("\n    FDivSqrt Disabled"))
   + "\n\n   Fetch Width           : " + fetchWidth
   + "\n   Decode Width          : " + decodeWidth
   + "\n   Issue Width           : " + issueParams.map(_.issueWidth).sum
   + "\n   ROB Size              : " + NUM_ROB_ENTRIES
   + "\n   Issue Window Size     : " + issueParams.map(_.numEntries) + iss_str
   + "\n   Load/Store Unit Size  : " + NUM_LSU_ENTRIES + "/" + NUM_LSU_ENTRIES
   + "\n   Num Int Phys Registers: " + numIntPhysRegs
   + "\n   Num FP  Phys Registers: " + numFpPhysRegs
   + "\n   Max Branch Count      : " + MAX_BR_COUNT
   + "\n   BTB Size              : " +
      (if (enableBTB) ("" + boomParams.btb.nSets * boomParams.btb.nWays + " entries (" +
         boomParams.btb.nSets + " x " + boomParams.btb.nWays + " ways)") else 0)
   + "\n   RAS Size              : " + (if (enableBTB) boomParams.btb.nRAS else 0)
   + "\n   Rename  Stage Latency : " + renameLatency
   + "\n   RegRead Stage Latency : " + regreadLatency
   + "\n" + iregfile.toString
   + "\n   Num Slow Wakeup Ports : " + num_irf_write_ports
   + "\n   Num Fast Wakeup Ports : " + exe_units.count(_.isBypassable)
   + "\n   Num Bypass Ports      : " + exe_units.num_total_bypass_ports
   + "\n" + fp_pipeline.toString
   + "\n   DCache Ways           : " + dcacheParams.nWays
   + "\n   DCache Sets           : " + dcacheParams.nSets
   + "\n   ICache Ways           : " + icacheParams.nWays
   + "\n   ICache Sets           : " + icacheParams.nSets
   + "\n   D-TLB Entries         : " + dcacheParams.nTLBEntries
   + "\n   I-TLB Entries         : " + icacheParams.nTLBEntries
   + "\n   Paddr Bits            : " + paddrBits
   + "\n   Vaddr Bits            : " + vaddrBits)

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
   io.ifu.flush_pc          := RegNext(csr.io.evec)
   io.ifu.com_ftq_idx       := rob.io.com_xcpt.bits.ftq_idx

   io.ifu.clear_fetchbuffer := br_unit.brinfo.mispredict ||
                               rob.io.flush.valid ||
                               io.ifu.sfence_take_pc

   // TODO is sfence_take_pc correct? Can probably remove it.
   io.ifu.flush_info.valid  := rob.io.flush.valid || io.ifu.sfence_take_pc
   io.ifu.flush_info.bits   := rob.io.flush.bits

   // Tell the FTQ it can deallocate entries by passing youngest ftq_idx.
   val youngest_com_idx     = (COMMIT_WIDTH-1).U - PriorityEncoder(rob.io.commit.valids.reverse)
   io.ifu.commit.valid      := rob.io.commit.valids.reduce(_|_)
   io.ifu.commit.bits       := rob.io.commit.uops(youngest_com_idx).ftq_idx


   io.ifu.flush_icache :=
      Range(0,decodeWidth).map{i => rob.io.commit.valids(i) && rob.io.commit.uops(i).is_fencei}.reduce(_|_) ||
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
   val dec_finished_mask = Reg(init = UInt(0, decodeWidth))

   //-------------------------------------------------------------
   // Pull out instructions and send to the Decoders

   io.ifu.fetchpacket.ready := dec_rdy
   val dec_fbundle = io.ifu.fetchpacket.bits

   //-------------------------------------------------------------
   // Decoders

   // allow early instructions to stall later instructions
   var dec_stall_next_inst = Bool(false)
   var dec_last_inst_was_stalled = Bool(false)

   // stall fetch/dcode because we ran out of branch tags
   val branch_mask_full = Wire(Vec(decodeWidth, Bool()))

   for (w <- 0 until decodeWidth)
   {
      dec_valids(w)                      := io.ifu.fetchpacket.valid && dec_fbundle.uops(w).valid && !dec_finished_mask(w)
      decode_units(w).io.enq.uop         := dec_fbundle.uops(w)
      decode_units(w).io.status          := csr.io.status
      decode_units(w).io.csr_decode      := csr.io.decode(w)
      decode_units(w).io.interrupt       := csr.io.interrupt
      decode_units(w).io.interrupt_cause := csr.io.interrupt_cause

      val prev_insts_in_bundle_valid = Range(0,w).map{i => dec_valids(i)}.foldLeft(Bool(false))(_|_)

      // stall this instruction?
      // TODO tailor this to only care if a given instruction uses a resource?
      val stall_me = (dec_valids(w) &&
                        (  !(rename_stage.io.inst_can_proceed(w))
                        || (dec_valids(w) && dec_uops(w).is_unique &&
                           (!(rob.io.empty) || !lsu.io.lsu_fencei_rdy || prev_insts_in_bundle_valid))
                        || !rob.io.ready
                        || lsu.io.laq_full
                        || lsu.io.stq_full
                        || branch_mask_full(w)
                        || br_unit.brinfo.mispredict
                        || rob.io.flush.valid
                        || dec_stall_next_inst
                        || (dec_valids(w) && dec_uops(w).is_fencei && !lsu.io.lsu_fencei_rdy)
                        )) ||
                     dec_last_inst_was_stalled

      // stall the next instruction following me in the decode bundle?
      dec_last_inst_was_stalled = stall_me
      dec_stall_next_inst  = stall_me || (dec_valids(w) && dec_uops(w).is_unique)

      dec_will_fire(w) := dec_valids(w) && !stall_me && !io.ifu.clear_fetchbuffer
      dec_uops(w)      := decode_units(w).io.deq.uop
   }

   // all decoders are empty and ready for new instructions
   dec_rdy := !(dec_stall_next_inst)

   when (dec_rdy || io.ifu.clear_fetchbuffer)
   {
      dec_finished_mask := Bits(0)
   }
   .otherwise
   {
      dec_finished_mask := dec_will_fire.asUInt | dec_finished_mask
   }

   //-------------------------------------------------------------
   // Branch Mask Logic


   dec_brmask_logic.io.brinfo := br_unit.brinfo
   dec_brmask_logic.io.flush_pipeline := rob.io.flush.valid

   for (w <- 0 until decodeWidth)
   {
      dec_brmask_logic.io.is_branch(w) := !dec_finished_mask(w) && dec_uops(w).allocate_brtag
      dec_brmask_logic.io.will_fire(w) :=  dec_will_fire(w) && dec_uops(w).allocate_brtag // ren, dis can back pressure us

      dec_uops(w).br_tag  := dec_brmask_logic.io.br_tag(w)
      dec_uops(w).br_mask := dec_brmask_logic.io.br_mask(w)
   }

   branch_mask_full := dec_brmask_logic.io.is_full

   //-------------------------------------------------------------
   // LD/ST Unit Allocation Logic

   // TODO this is dupliciated logic with the the LSU... do we need ldq_idx/stq elsewhere?
   val new_ldq_idx = Wire(UInt())
   val new_stq_idx = Wire(UInt())

   var new_lidx = new_ldq_idx
   var new_sidx = new_stq_idx

   for (w <- 0 until decodeWidth)
   {
      dec_uops(w).ldq_idx := new_lidx
      dec_uops(w).stq_idx := new_sidx

      new_lidx = Mux(dec_will_fire(w) && dec_uops(w).is_load,  WrapInc(new_lidx, NUM_LSU_ENTRIES), new_lidx)
      new_sidx = Mux(dec_will_fire(w) && dec_uops(w).is_store, WrapInc(new_sidx, NUM_LSU_ENTRIES), new_sidx)
   }

   //-------------------------------------------------------------
   // Rob Allocation Logic

   for (w <- 0 until decodeWidth)
   {
      // note: this assumes uops haven't been shifted - there's a 1:1 match between PC's LSBs and "w" here
      // (thus the LSB of the rob_idx gives part of the PC)
      if (decodeWidth == 1)
         dec_uops(w).rob_idx := rob.io.curr_rob_tail
      else
         dec_uops(w).rob_idx := Cat(rob.io.curr_rob_tail, UInt(w, log2Up(decodeWidth)))
   }

   val dec_has_br_or_jalr_in_packet =
      (dec_valids zip dec_uops map {case(v,u) => v && u.is_br_or_jmp && !u.is_jal}).reduce(_|_)


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Register Rename Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // TODO for now, assume worst-case all instructions will dispatch towards one issue unit.
   val dis_readys = issue_units.map(_.io.dis_readys.asUInt).reduce(_&_) & fp_pipeline.io.dis_readys.asUInt
   rename_stage.io.dis_inst_can_proceed := dis_readys.toBools

   rename_stage.io.kill     := io.ifu.clear_fetchbuffer // mispredict or flush
   rename_stage.io.brinfo   := br_unit.brinfo

   rename_stage.io.flush_pipeline := rob.io.flush.valid
   rename_stage.io.debug_rob_empty := rob.io.empty

   rename_stage.io.dec_will_fire := dec_will_fire
   rename_stage.io.dec_uops := dec_uops


   var wu_idx = 0
   // loop through each issue-port (exe_units are statically connected to an issue-port)
   for (i <- 0 until exe_units.length)
   {
      if (exe_units(i).is_mem_unit)
      {
         // If Memory, it's the shared long-latency port.
         int_wakeups(wu_idx).valid := ll_wbarb.io.out.fire()
         int_wakeups(wu_idx).bits  := ll_wbarb.io.out.bits
         wu_idx += 1
      }
      else
      {
         // Fast Wakeup (uses just-issued uops) that have known latencies
         if (exe_units(i).isBypassable)
         {
            int_wakeups(wu_idx).valid := iss_valids(i) &&
                                         iss_uops(i).bypassable &&
                                         iss_uops(i).dst_rtype === RT_FIX &&
                                         iss_uops(i).ldst_val
            int_wakeups(wu_idx).bits.uop := iss_uops(i)
            wu_idx += 1
            assert (!(iss_uops(i).dst_rtype === RT_FLT && iss_uops(i).bypassable), "Bypassing FP is not supported.")
         }

         // Slow Wakeup (uses write-port to register file)
         for (j <- 0 until exe_units(i).num_rf_write_ports)
         {
            val resp = exe_units(i).io.resp(j)
            int_wakeups(wu_idx).valid := resp.valid &&
                                         resp.bits.uop.ctrl.rf_wen &&
                                        !resp.bits.uop.bypassable &&
                                         resp.bits.uop.dst_rtype === RT_FIX
            int_wakeups(wu_idx).bits := exe_units(i).io.resp(j).bits
            wu_idx += 1

         }
      }
      require (exe_units(i).usesIRF)
   }
   require (wu_idx == num_wakeup_ports)

   for ((renport, intport) <- rename_stage.io.int_wakeups zip int_wakeups)
   {
      renport <> intport
   }
   for ((renport, fpport) <- rename_stage.io.fp_wakeups zip fp_pipeline.io.wakeups)
   {
      renport <> fpport
   }

   rename_stage.io.com_valids := rob.io.commit.valids
   rename_stage.io.com_uops := rob.io.commit.uops
   rename_stage.io.com_rbk_valids := rob.io.commit.rbk_valids

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Dispatch Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   for (w <- 0 until decodeWidth)
   {
      dis_valids(w)       := rename_stage.io.ren2_mask(w)
      dis_uops(w)         := GetNewUopAndBrMask(rename_stage.io.ren2_uops(w), br_unit.brinfo)
   }


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Issue Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   require (issue_units.map(_.issue_width).sum == exe_units.length)

   // Input (Dispatch)
   for {
      iu <- issue_units
      w <- 0 until DISPATCH_WIDTH
   }{
      iu.io.dis_valids(w) := dis_valids(w) && dis_uops(w).iqtype === UInt(iu.iqType)
      iu.io.dis_uops(w) := dis_uops(w)

      when (dis_uops(w).uopc === uopSTA && dis_uops(w).lrs2_rtype === RT_FLT) {
         iu.io.dis_uops(w).lrs2_rtype := RT_X
         iu.io.dis_uops(w).prs2_busy := Bool(false)
      }
   }

   fp_pipeline.io.dis_valids <> dis_valids
   fp_pipeline.io.dis_uops <> dis_uops
   // Manually specify unused signals so they don't show up in the
   // FpPipeline's I/O field. This is only necessary if the FpPipeline
   // is the top module being synthesized (otherwise cross-module
   // optimization can remove the signals).
   for (uop <- fp_pipeline.io.dis_uops)
   {
      uop.exc_cause := DontCare
      uop.csr_addr := DontCare
      uop.debug_wdata := DontCare
      if (!DEBUG_PRINTF && !COMMIT_LOG_PRINTF) uop.pc := DontCare
      if (!DEBUG_PRINTF && !COMMIT_LOG_PRINTF) uop.inst := DontCare
      if (!O3PIPEVIEW_PRINTF) uop.debug_events.fetch_seq := DontCare
   }




   // Output (Issue)

   val ifpu_idx = exe_units.length-1 // TODO hack; need more disciplined manner to hook up ifpu
   require (exe_units(ifpu_idx).supportedFuncUnits.ifpu)

   var iss_idx = 0
   var iss_cnt = 0
   for (w <- 0 until exe_units.length)
   {
      iss_valids(w) := issue_units(iss_idx).io.iss_valids(iss_cnt)
      iss_uops(w)   := issue_units(iss_idx).io.iss_uops(iss_cnt)

      var fu_types = exe_units(w).io.fu_types

      if (w == ifpu_idx) {
         // TODO hack, need a more disciplined way to connect to an issue port
         // TODO XXX need to also apply back-pressure.
         fu_types = fu_types | FUConstants.FU_I2F
      }

      if (exe_units(w).supportedFuncUnits.muld && regreadLatency > 0)
      {
         // Supress just-issued divides from issuing back-to-back, since it's an iterative divider.
         // But it takes a cycle to get to the Exe stage, so it can't tell us it is busy yet.
         val idiv_issued = iss_valids(w) && iss_uops(w).fu_code_is(FU_DIV)
         fu_types = fu_types & RegNext(~Mux(idiv_issued, FU_DIV, Bits(0)))
      }
      require (!exe_units(w).supportedFuncUnits.fdiv)

      issue_units(iss_idx).io.fu_types(iss_cnt) := fu_types

      // TODO this is super fragile -- check the issue-units match the exe-units on instruction types.
      require ((issue_units(iss_idx).iqType == IQT_MEM.litValue) ^ !exe_units(w).is_mem_unit)
      require (issueParams(iss_idx).iqType != IQT_FP.litValue)

      iss_cnt += 1
      val iwidths = issueParams.map(_.issueWidth)
      if (iss_cnt >= iwidths(iss_idx)) {
         iss_idx += 1
         iss_cnt = 0
      }
   }


   issue_units.map(_.io.tsc_reg := debug_tsc_reg)
   issue_units.map(_.io.brinfo := br_unit.brinfo)
   issue_units.map(_.io.flush_pipeline := rob.io.flush.valid)

   // Wakeup (Issue & Writeback)

   for {
      iu <- issue_units
      (issport, wakeup) <- iu.io.wakeup_pdsts zip int_wakeups
   }{
      issport.valid := wakeup.valid
      issport.bits  := wakeup.bits.uop.pdst

      require (iu.io.wakeup_pdsts.length == int_wakeups.length)
   }


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Register Read Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // Register Read <- Issue (rrd <- iss)
   iregister_read.io.rf_read_ports <> iregfile.io.read_ports

   iregister_read.io.iss_valids <> iss_valids
   iregister_read.io.iss_uops := iss_uops

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
   val csr_rw_cmd = csr_exe_unit.io.resp(0).bits.uop.ctrl.csr_cmd
   val wb_wdata = csr_exe_unit.io.resp(0).bits.data

   csr.io.rw.addr  := csr_exe_unit.io.resp(0).bits.uop.csr_addr
   csr.io.rw.cmd   := Mux(csr_exe_unit.io.resp(0).valid, csr_rw_cmd, freechips.rocketchip.rocket.CSR.N)
   csr.io.rw.wdata :=wb_wdata

   // Extra I/O
   csr.io.retire    := PopCount(rob.io.commit.valids.asUInt)
   csr.io.exception := rob.io.com_xcpt.valid
   // csr.io.pc used for setting EPC during exception or CSR.io.trace.
   csr.io.pc        := boom.util.AlignPCToBoundary(io.ifu.com_fetch_pc, icBlockBytes) + rob.io.com_xcpt.bits.pc_lob
   csr.io.cause     := rob.io.com_xcpt.bits.cause
   csr.io.tval      := Mux(csr.io.cause === Causes.illegal_instruction.U, 0.U, rob.io.com_xcpt.bits.badvaddr)


   // reading requires serializing the entire pipeline
   csr.io.fcsr_flags.valid := rob.io.commit.fflags.valid
   csr.io.fcsr_flags.bits  := rob.io.commit.fflags.bits

   exe_units.map(_.io.fcsr_rm := csr.io.fcsr_rm)
   fp_pipeline.io.fcsr_rm := csr.io.fcsr_rm

   csr.io.hartid := io.hartid
   csr.io.interrupts := io.interrupts

// TODO can we add this back in, but handle reset properly and save us the mux above on csr.io.rw.cmd?
//   assert (!(csr_rw_cmd =/= rocket.CSR.N && !exe_units(0).io.resp(0).valid), "CSRFile is being written to spuriously.")


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Execute Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   var idx = 0
   for (w <- 0 until exe_units.length)
   {
      exe_units(w).io.req <> iregister_read.io.exe_reqs(w)
      exe_units(w).io.brinfo := br_unit.brinfo
      exe_units(w).io.com_exception := rob.io.flush.valid

      if (exe_units(w).isBypassable)
      {
         for (i <- 0 until exe_units(w).numBypassPorts)
         {
            bypasses.valid(idx) := exe_units(w).io.bypass.valid(i)
            bypasses.uop(idx)   := exe_units(w).io.bypass.uop(i)
            bypasses.data(idx)  := exe_units(w).io.bypass.data(i)
            idx = idx + 1
         }
      }

   }
   require (idx == exe_units.num_total_bypass_ports)


   // don't send IntToFP moves to integer execution units.
   when (iregister_read.io.exe_reqs(ifpu_idx).bits.uop.fu_code === FUConstants.FU_I2F) {
      exe_units(ifpu_idx).io.req.valid := Bool(false)
   }
   fp_pipeline.io.fromint := iregister_read.io.exe_reqs(ifpu_idx)
   fp_pipeline.io.fromint.valid :=
      iregister_read.io.exe_reqs(ifpu_idx).valid &&
      iregister_read.io.exe_reqs(ifpu_idx).bits.uop.fu_code === FUConstants.FU_I2F
   assert (fp_pipeline.io.fromint.ready,
      "[core] we don't support back-pressure against IFPU. Redesign if you hit this.")

   fp_pipeline.io.brinfo := br_unit.brinfo


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Load/Store Unit ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // enqueue basic load/store info in Decode
   lsu.io.dec_uops := dec_uops

   for (w <- 0 until decodeWidth)
   {
      lsu.io.dec_st_vals(w) := dec_will_fire(w) && rename_stage.io.inst_can_proceed(w) && !rob.io.flush.valid &&
                               dec_uops(w).is_store
      lsu.io.dec_ld_vals(w) := dec_will_fire(w) && rename_stage.io.inst_can_proceed(w) && !rob.io.flush.valid &&
                               dec_uops(w).is_load

      lsu.io.dec_uops(w).rob_idx := dec_uops(w).rob_idx // for debug purposes (comit logging)
   }

   lsu.io.commit_store_mask := rob.io.commit.st_mask
   lsu.io.commit_load_mask  := rob.io.commit.ld_mask
   lsu.io.commit_load_at_rob_head := rob.io.com_load_is_at_rob_head

   //com_xcpt.valid comes too early, will fight against a branch that resolves same cycle as an exception
   lsu.io.exception := rob.io.flush.valid

   // Handle Branch Mispeculations
   lsu.io.brinfo := br_unit.brinfo
   dc_shim.io.core.brinfo := br_unit.brinfo

   new_ldq_idx := lsu.io.new_ldq_idx
   new_stq_idx := lsu.io.new_stq_idx

   lsu.io.debug_tsc := debug_tsc_reg

   dc_shim.io.core.flush_pipe := rob.io.flush.valid

   lsu.io.nack <> dc_shim.io.core.nack

   lsu.io.dmem_req_ready := dc_shim.io.core.req.ready
   lsu.io.dmem_is_ordered:= dc_shim.io.core.ordered

   lsu.io.fp_stdata <> fp_pipeline.io.tosdq


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Writeback Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------


   var w_cnt = 0
   var llidx = -1 // find which rf port corresponds to the long latency memory port.
   for (i <- 0 until exe_units.length)
   {
      for (j <- 0 until exe_units(i).num_rf_write_ports)
      {
         val wbresp = exe_units(i).io.resp(j)
         val wbpdst = wbresp.bits.uop.pdst
         val wbdata = wbresp.bits.data

         def wbIsValid(rtype: UInt) =
            wbresp.valid && wbresp.bits.uop.ctrl.rf_wen && wbresp.bits.uop.dst_rtype === rtype
         val wbReadsCSR = wbresp.bits.uop.ctrl.csr_cmd =/= freechips.rocketchip.rocket.CSR.N

         if (exe_units(i).data_width > 64)
         {
				require (exe_units(i).is_mem_unit)
            assert (!(wbIsValid(RT_FIX) && exe_units(i).io.resp(j).bits.data(64).toBool),
               "the 65th bit was set on a fixed point write-back to the regfile.")
         }

         if (exe_units(i).uses_csr_wport && (j == 0))
         {
            iregfile.io.write_ports(w_cnt).valid     := wbIsValid(RT_FIX)
            iregfile.io.write_ports(w_cnt).bits.addr := wbpdst
            iregfile.io.write_ports(w_cnt).bits.data := Mux(wbReadsCSR, csr.io.rw.rdata, wbdata)
            wbresp.ready := iregfile.io.write_ports(w_cnt).ready
         }
         else if (exe_units(i).is_mem_unit)
         {
            assert (llidx == -1) // should only hit this once
            require (j == 0) // only support one port on memory unit for now.
            llidx = w_cnt

            // connect to FP pipeline's long latency writeport.
            fp_pipeline.io.ll_wport.valid     := wbIsValid(RT_FLT)
            fp_pipeline.io.ll_wport.bits.uop  := wbresp.bits.uop
            fp_pipeline.io.ll_wport.bits.data := wbdata
            fp_pipeline.io.ll_wport.bits.fflags.valid := Bool(false)
            assert (fp_pipeline.io.ll_wport.ready, "[core] LL port should always be ready.")
         }
         else
         {
            iregfile.io.write_ports(w_cnt).valid     := wbIsValid(RT_FIX)
            iregfile.io.write_ports(w_cnt).bits.addr := wbpdst
            iregfile.io.write_ports(w_cnt).bits.data := wbdata
            wbresp.ready := iregfile.io.write_ports(w_cnt).ready
         }


         if (!exe_units(i).is_mem_unit) {
            assert (!wbIsValid(RT_FLT), "[fppipeline] An FP writeback is being attempted to the Int Regfile.")
         }

         assert (!(exe_units(i).io.resp(j).valid &&
            !exe_units(i).io.resp(j).bits.uop.ctrl.rf_wen &&
            exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FIX),
            "[fppipeline] An Int writeback is being attempted with rf_wen disabled.")

         if (!exe_units(i).is_mem_unit) {
            assert (!(exe_units(i).io.resp(j).valid &&
               exe_units(i).io.resp(j).bits.uop.ctrl.rf_wen &&
               exe_units(i).io.resp(j).bits.uop.dst_rtype =/= RT_FIX),
               "[fppipeline] writeback being attempted to Int RF with dst != Int type exe_units("+i+").resp("+j+")")
         }

         w_cnt += 1
      }
   }

   // Share the memory port with other long latency operations.
   val mem_unit = exe_units.memory_unit
   require (mem_unit.num_rf_write_ports == 1)
   val mem_resp = mem_unit.io.resp(0)

   ll_wbarb.io.in(0).valid := mem_resp.valid && mem_resp.bits.uop.ctrl.rf_wen && mem_resp.bits.uop.dst_rtype === RT_FIX
   ll_wbarb.io.in(0).bits  := mem_resp.bits

   assert (ll_wbarb.io.in(0).ready) // never backpressure the memory unit.
   ll_wbarb.io.in(1) <> fp_pipeline.io.toint
   iregfile.io.write_ports(llidx) <> WritePort(ll_wbarb.io.out, IPREG_SZ, xLen)



   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Commit Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // Dispatch
   rob.io.enq_valids := rename_stage.io.ren1_mask
   rob.io.enq_uops   := rename_stage.io.ren1_uops
   rob.io.enq_new_packet := dec_finished_mask === 0.U
//   rob.io.enq_partial_stall := !dec_rdy && !dec_will_fire(decodeWidth-1)
   rob.io.enq_partial_stall := !dec_rdy // TODO come up with better ROB compacting scheme.
   rob.io.debug_tsc := debug_tsc_reg
   rob.io.csr_stall := csr.io.csr_stall

   assert ((dec_will_fire zip rename_stage.io.ren1_mask map {case(d,r) => d === r}).reduce(_|_),
      "[core] Assumption that dec_will_fire and ren1_mask are equal is being violated.")

   // Writeback
   var cnt = 0
   var f_cnt = 0 // rob fflags port index
   for (eu <- exe_units)
   {
      for ((resp, j) <- eu.io.resp.zipWithIndex)
      {
         val wb_uop = resp.bits.uop
         var data: UInt = null

         if (eu.is_mem_unit)
         {
            val ll_uop = ll_wbarb.io.out.bits.uop
            rob.io.wb_resps(cnt).valid := ll_wbarb.io.out.valid && !(ll_uop.is_store && !ll_uop.is_amo)
            rob.io.wb_resps(cnt).bits <> ll_wbarb.io.out.bits
            // for commit logging... (only integer writes come through here)
            rob.io.debug_wb_valids(cnt) := ll_wbarb.io.out.valid && ll_uop.ctrl.rf_wen && ll_uop.dst_rtype === RT_FIX
            data = ll_wbarb.io.out.bits.data
         }
         else
         {
            rob.io.wb_resps(cnt).valid := resp.valid && !(wb_uop.is_store && !wb_uop.is_amo)
            rob.io.wb_resps(cnt).bits <> resp.bits

            // for commit logging... (only integer writes come through here)
            rob.io.debug_wb_valids(cnt) := resp.valid && wb_uop.ctrl.rf_wen && wb_uop.dst_rtype === RT_FIX
            data = resp.bits.data
         }

         if (eu.hasFFlags || (eu.is_mem_unit && usingFPU))
         {
            if (eu.hasFFlags)
            {
               println ("\tConnecting ExeUnit with FFLAGS to ROB fflag port: " + f_cnt)
               rob.io.fflags(f_cnt) <> resp.bits.fflags
               f_cnt += 1
            }

            assert (!(resp.valid && resp.bits.uop.fp_val && wb_uop.dst_rtype === RT_FLT && wb_uop.uopc =/= uopLD),
               "[core] I think we never use FP now.")
            if (eu.uses_csr_wport && (j == 0))
            {
               rob.io.debug_wb_wdata(cnt) := Mux(wb_uop.ctrl.csr_cmd =/= freechips.rocketchip.rocket.CSR.N, csr.io.rw.rdata, data)
            }
            else
            {
               rob.io.debug_wb_wdata(cnt) := data
            }
         }
         else
         {
            if (eu.uses_csr_wport && (j == 0))
            {
               rob.io.debug_wb_wdata(cnt) := Mux(wb_uop.ctrl.csr_cmd =/= freechips.rocketchip.rocket.CSR.N, csr.io.rw.rdata, data)
            }
            else
            {
               rob.io.debug_wb_wdata(cnt) := data
            }
         }
         cnt += 1
      }
   }

   for (wakeup <- fp_pipeline.io.wakeups)
   {
      rob.io.wb_resps(cnt) <> wakeup
      rob.io.fflags(f_cnt) <> wakeup.bits.fflags
      rob.io.debug_wb_valids(cnt) := wakeup.valid
      rob.io.debug_wb_wdata(cnt) := ieee(wakeup.bits.data)
      cnt += 1
      f_cnt += 1

      assert (!(wakeup.valid && wakeup.bits.uop.dst_rtype =/= RT_FLT),
         "[core] FP wakeup does not write back to a FP register.")

      assert (!(wakeup.valid && !wakeup.bits.uop.fp_val),
         "[core] FP wakeup does not involve an FP instruction.")
   }
   assert (cnt == rob.num_wakeup_ports)


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
   rob.io.lsu_clr_bsy_valid   := lsu.io.lsu_clr_bsy_valid
   rob.io.lsu_clr_bsy_rob_idx := lsu.io.lsu_clr_bsy_rob_idx
   rob.io.lxcpt <> lsu.io.xcpt

   rob.io.csr_eret := csr.io.eret

   assert (!(csr.io.singleStep), "[core] single-step is unsupported.")

   rob.io.bxcpt <> br_unit.xcpt

   //-------------------------------------------------------------
   // **** Flush Pipeline ****
   //-------------------------------------------------------------
   // flush on exceptions, miniexeptions, and after some special instructions

   fp_pipeline.io.flush_pipeline := rob.io.flush.valid
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
   when (rob.io.commit.valids.asUInt.orR || csr.io.csr_stall || reset.toBool) { idle_cycles := 0.U }
   assert (!(idle_cycles.value(13)), "Pipeline has hung.")

   fp_pipeline.io.debug_tsc_reg := debug_tsc_reg

   //-------------------------------------------------------------
   // Uarch Hardware Performance Events (HPEs)

//   // User-level instruction count.
//   csr.io.events(2) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && (csr.io.status.prv === UInt(freechips.rocketchip.rocket.PRV.U))})
//
//   csr.io.events(5)  := csr.io.status.prv === UInt(freechips.rocketchip.rocket.PRV.U)
//
//   // Instruction mixes.
//   csr.io.events(6)  := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal})
//   csr.io.events(7)  := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_jal})
//   csr.io.events(8)  := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_jump && !rob.io.commit.uops(w).is_jal})
//   csr.io.events(9)  := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_load})
//   csr.io.events(10) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_store})
//   csr.io.events(11) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).fp_val})
//
//   // Decode stall causes.
//   csr.io.events(12) := !rob.io.ready
//   csr.io.events(13) := lsu.io.laq_full
//   csr.io.events(14) := lsu.io.stq_full
//   csr.io.events(15) := !dis_readys.toBools.reduce(_&_) // issue queues
//   csr.io.events(16) := branch_mask_full.reduce(_|_)
//   csr.io.events(17) := rob.io.flush.valid
//
//   // LSU Speculation stats.
//   csr.io.events(18) := lsu.io.counters.ld_valid
//   csr.io.events(19) := lsu.io.counters.stld_order_fail
//   csr.io.events(20) := lsu.io.counters.ldld_order_fail
//
//   // Branch prediction stats.
//   csr.io.events(21)  := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
//      rob.io.commit.uops(w).stat_brjmp_mispredicted})
//   csr.io.events(22) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
//      rob.io.commit.uops(w).stat_btb_made_pred})
//   csr.io.events(23) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
//      rob.io.commit.uops(w).stat_btb_mispredicted})
//   csr.io.events(24) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
//      rob.io.commit.uops(w).stat_bpd_made_pred})
//   csr.io.events(25) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
//      rob.io.commit.uops(w).stat_bpd_mispredicted})
//
//   // Branch prediction - no prediction made.
//   csr.io.events(26) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
//      !rob.io.commit.uops(w).stat_btb_made_pred && !rob.io.commit.uops(w).stat_bpd_made_pred})
//
//   // Branch prediction - no predition made & a mispredict occurred.
//   csr.io.events(27) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
//      !rob.io.commit.uops(w).stat_btb_made_pred && !rob.io.commit.uops(w).stat_bpd_made_pred &&
//      rob.io.commit.uops(w).stat_brjmp_mispredicted})
//
//
//   // Count user-level branches (subtract from total to get privilege branch accuracy)
//   csr.io.events(28) := br_unit.brinfo.valid && (csr.io.status.prv === UInt(freechips.rocketchip.rocket.PRV.U))
//freechips.rocketchip.   csr.io.events(29) := br_unit.brinfo.mispredict && (csr.io.status.prv === UInt(rocket.PRV.U))
//
//   // count change of privilege modes
//   csr.io.events(30) := csr.io.status.prv =/= RegNext(csr.io.status.prv)
//
//   csr.io.events(31) := !issue_units(0).io.dis_readys.reduce(_&_)
//   csr.io.events(32) := !issue_units(1).io.dis_readys.reduce(_&_)
//   csr.io.events(33) := !fp_pipeline.io.dis_readys.reduce(_&_)
//
//   assert (!(Range(0,COMMIT_WIDTH).map{w =>
//      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && rob.io.commit.uops(w).is_jal &&
//      rob.io.commit.uops(w).stat_brjmp_mispredicted}.reduce(_|_)),
//      "[dpath] A committed JAL was marked as having been mispredicted.")
//
//   // Count issued instructions (only integer currently).
//   require (log2Ceil(1+iss_valids.length) <= csr.io.events(0).getWidth) // CSR.scala sets increment width.
//   csr.io.events(34) := PopCount(iss_valids)
//
//   // Count not-issued slots due to empty issue windows (only integer currently).
//   val not_issued_and_empty = for {iu <- issue_units; iss_valid <- iu.io.iss_valids} yield {
//         !iss_valid && iu.io.event_empty }
//   csr.io.events(35) := PopCount(not_issued_and_empty)
//
//   // Count not-issued slots due to backend hazards/unsatisified dependencies (only integer currently).
//   val not_issued_and_not_empty = for {iu <- issue_units; iss_valid <- iu.io.iss_valids} yield {
//         !iss_valid && !iu.io.event_empty}
//   csr.io.events(36) := PopCount(not_issued_and_not_empty)


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
       var whitespace = (debugScreenheight - 25 + 3 -10 + 3 + 4 - decodeWidth - NUM_LSU_ENTRIES -
         issueParams.map(_.numEntries).sum - issueParams.length - (NUM_ROB_ENTRIES/COMMIT_WIDTH) -
         numFtqWhitespace - fetchWhitespace
     )

      println("Whitespace padded: " + whitespace)

      printf("--- Cyc=%d , ----------------- Ret: %d ----------------------------------"
         , debug_tsc_reg
         , debug_irt_reg & UInt(0xffffff))

      for (w <- 0 until decodeWidth)
      {
         if (w == 0) {
            printf("\n  Dec:  ([0x%x]                        ", dec_uops(w).pc(19,0))
         } else {
            printf("[0x%x]                        ", dec_uops(w).pc(19,0))
         }
      }

      for (w <- 0 until decodeWidth)
      {
         printf("(%c%c) " + "DASM(%x)" + " |  "
            , Mux(io.ifu.fetchpacket.valid && dec_fbundle.uops(w).valid && !dec_finished_mask(w), Str("v"), Str("-"))
            , Mux(dec_will_fire(w), Str("V"), Str("-"))
            , dec_fbundle.uops(w).inst
            )
      }

      for (w <- 0 until decodeWidth)
      {
         if (w == 0) {
            printf("\n  Ren:  ([0x%x]                        ", rename_stage.io.ren2_uops(w).pc(19,0))
         } else {
            printf("[0x%x]                        ", rename_stage.io.ren2_uops(w).pc(19,0))
         }
      }

      for (w <- 0 until decodeWidth)
      {
         printf(" (%c) " + "DASM(%x)" + " |  "
            , Mux(rename_stage.io.ren2_mask(w), Str("V"), Str("-"))
            , rename_stage.io.ren2_uops(w).inst
            )
      }

      printf(") fin(%x)\n", dec_finished_mask)
      for (w <- 0 until decodeWidth)
      {
         printf("        [ISA:%d,%d,%d,%d] [Phs:%d(%c)%d[%c](%c)%d[%c](%c)%d[%c](%c)] "
            , dis_uops(w).ldst
            , dis_uops(w).lrs1
            , dis_uops(w).lrs2
            , dis_uops(w).lrs3
            , dis_uops(w).pdst
            , Mux(dis_uops(w).dst_rtype   === RT_FIX, Str("X")
              , Mux(dis_uops(w).dst_rtype === RT_X  , Str("-")
              , Mux(dis_uops(w).dst_rtype === RT_FLT, Str("f")
              , Mux(dis_uops(w).dst_rtype === RT_PAS, Str("C"), Str("?")))))
            , dis_uops(w).pop1
            , Mux(rename_stage.io.ren2_uops(w).prs1_busy, Str("B"), Str("R"))
            , Mux(dis_uops(w).lrs1_rtype    === RT_FIX, Str("X")
               , Mux(dis_uops(w).lrs1_rtype === RT_X  , Str("-")
               , Mux(dis_uops(w).lrs1_rtype === RT_FLT, Str("f")
               , Mux(dis_uops(w).lrs1_rtype === RT_PAS, Str("C"), Str("?")))))
            , dis_uops(w).pop2
            , Mux(rename_stage.io.ren2_uops(w).prs2_busy, Str("B"), Str("R"))
            , Mux(dis_uops(w).lrs2_rtype    === RT_FIX, Str("X")
               , Mux(dis_uops(w).lrs2_rtype === RT_X  , Str("-")
               , Mux(dis_uops(w).lrs2_rtype === RT_FLT, Str("f")
               , Mux(dis_uops(w).lrs2_rtype === RT_PAS, Str("C"), Str("?")))))
            , dis_uops(w).pop3
            , Mux(rename_stage.io.ren2_uops(w).prs3_busy, Str("B"), Str("R"))
            , Mux(dis_uops(w).frs3_en, Str("f"), Str("-"))
            )
      }

      if (DEBUG_PRINTF_ROB)
      {
         printf("\n) ctate: (%c: %c %c %c %c %c %c) BMsk:%x Mode:%c\n"
         , Mux(rob.io.debug.state === UInt(0), Str("R"),
           Mux(rob.io.debug.state === UInt(1), Str("N"),
           Mux(rob.io.debug.state === UInt(2), Str("B"),
           Mux(rob.io.debug.state === UInt(3), Str("W"),
                                               Str(" ")))))
         , Mux(rob.io.ready,Str("_"), Str("!"))
         , Mux(lsu.io.laq_full, Str("L"), Str("_"))
         , Mux(lsu.io.stq_full, Str("S"), Str("_"))
         , Mux(rob.io.flush.valid, Str("F"), Str(" "))
         , Mux(branch_mask_full.reduce(_|_), Str("B"), Str(" "))
         , Mux(dc_shim.io.core.req.ready, Str("R"), Str("B"))
         , dec_brmask_logic.io.debug.branch_mask
         , Mux(csr.io.status.prv === Bits(0x3), Str("M"),
           Mux(csr.io.status.prv === Bits(0x0), Str("U"),
           Mux(csr.io.status.prv === Bits(0x1), Str("S"),  //2 is H
                                                 Str("?"))))
         )
      }

      printf("Exct(%c%d) Commit(%x) fl: 0x%x (%d) is: 0x%x (%d)\n"
         , Mux(rob.io.com_xcpt.valid, Str("E"), Str("-"))
         , rob.io.com_xcpt.bits.cause
         , rob.io.commit.valids.asUInt
         , rename_stage.io.debug.ifreelist
         , PopCount(rename_stage.io.debug.ifreelist)
         , rename_stage.io.debug.iisprlist
         , PopCount(rename_stage.io.debug.iisprlist)
         )

      printf("                                      fl: 0x%x (%d) is: 0x%x (%d)\n"
         , rename_stage.io.debug.ffreelist
         , PopCount(rename_stage.io.debug.ffreelist)
         , rename_stage.io.debug.fisprlist
         , PopCount(rename_stage.io.debug.fisprlist)
         )

      // branch unit
      printf("                          Branch Unit: %c,%c,%d  NPC=%d,0x%x\n"
         , Mux(br_unit.brinfo.valid,Str("V"), Str(" "))
         , Mux(br_unit.brinfo.mispredict, Str("M"), Str(" "))
         , br_unit.brinfo.taken
         , exe_units(brunit_idx).io.get_ftq_pc.next_val
         , exe_units(brunit_idx).io.get_ftq_pc.next_pc(19,0)
      )

      // Rename Map Tables / ISA Register File
      val xpr_to_string =
              Vec(Str(" x0"), Str(" ra"), Str(" sp"), Str(" gp"),
                   Str(" tp"), Str(" t0"), Str(" t1"), Str(" t2"),
                   Str(" s0"), Str(" s1"), Str(" a0"), Str(" a1"),
                   Str(" a2"), Str(" a3"), Str(" a4"), Str(" a5"),
                   Str(" a6"), Str(" a7"), Str(" s2"), Str(" s3"),
                   Str(" s4"), Str(" s5"), Str(" s6"), Str(" s7"),
                   Str(" s8"), Str(" s9"), Str("s10"), Str("s11"),
                   Str(" t3"), Str(" t4"), Str(" t5"), Str(" t6"))

      val fpr_to_string =
              Vec( Str("ft0"), Str("ft1"), Str("ft2"), Str("ft3"),
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
      var new_commit_cnt = UInt(0)
      for (w <- 0 until COMMIT_WIDTH)
      {
         val priv = csr.io.status.prv

         when (rob.io.commit.valids(w))
         {
            when (rob.io.commit.uops(w).dst_rtype === RT_FIX && rob.io.commit.uops(w).ldst =/= UInt(0))
            {
               printf("%d 0x%x (0x%x) x%d 0x%x\n",
                  priv, Sext(rob.io.commit.uops(w).pc(vaddrBits,0), xLen), rob.io.commit.uops(w).inst,
                  rob.io.commit.uops(w).inst(RD_MSB,RD_LSB), rob.io.commit.uops(w).debug_wdata)
            }
            .elsewhen (rob.io.commit.uops(w).dst_rtype === RT_FLT)
            {
               printf("%d 0x%x (0x%x) f%d 0x%x\n",
                  priv, Sext(rob.io.commit.uops(w).pc(vaddrBits,0), xLen), rob.io.commit.uops(w).inst,
                  rob.io.commit.uops(w).inst(RD_MSB,RD_LSB), rob.io.commit.uops(w).debug_wdata)
            }
            .otherwise
            {
               printf("%d 0x%x (0x%x)\n",
                  priv, Sext(rob.io.commit.uops(w).pc(vaddrBits,0), xLen), rob.io.commit.uops(w).inst)
            }
         }
      }
   }

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // Pipeview Visualization

   if (O3PIPEVIEW_PRINTF)
   {
      println("   O3Pipeview Visualization Enabled\n")

      // did we already print out the instruction sitting at the front of the fetchbuffer/decode stage?
      val dec_printed_mask = Reg(init = Bits(0, decodeWidth))

      for (w <- 0 until decodeWidth)
      {
         when (dec_valids(w) && !dec_printed_mask(w)) {
            printf("%d; O3PipeView:decode:%d\n", dec_uops(w).debug_events.fetch_seq, debug_tsc_reg)
         }
         // Rename begins when uop leaves fetch buffer (Dec+Ren1 are in same stage).
         when (dec_will_fire(w)) {
            printf("%d; O3PipeView:rename: %d\n", dec_uops(w).debug_events.fetch_seq, debug_tsc_reg)
         }
         when (dis_valids(w)) {
            printf("%d; O3PipeView:dispatch: %d\n", dis_uops(w).debug_events.fetch_seq, debug_tsc_reg)
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

      for (i <- 0 until COMMIT_WIDTH)
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

   // We do not support RoCC.
   io.rocc.cmd.valid := false.B
   io.rocc.cmd.bits <> DontCare
   io.rocc.resp.ready := false.B
   io.rocc.exception := false.B
   io.rocc.mem.req.bits := DontCare
   io.rocc.mem.resp.bits.replay := false.B
   io.rocc.mem.s2_xcpt.pf.st := false.B
   io.rocc.mem.s2_xcpt.pf.ld := false.B
   io.rocc.mem.resp.bits.tag := 0.U
   io.rocc.mem.resp.valid := false.B
   io.rocc.mem.replay_next := false.B
   io.rocc.mem.resp.bits.data_word_bypass := false.B
   io.rocc.mem.perf.acquire := false.B
   io.rocc.mem.resp.bits.addr := false.B
   io.rocc.mem.resp.bits.store_data := false.B
   io.rocc.mem.resp.bits.typ := false.B
   io.rocc.mem.req.ready := false.B
   io.rocc.mem.resp.bits.cmd := false.B
   io.rocc.mem.perf.tlbMiss := false.B
   io.rocc.mem.s2_xcpt.ae.st := false.B
   io.rocc.mem.s2_xcpt.ae.ld := false.B
   io.rocc.mem.ordered := false.B
   io.rocc.mem.resp.bits.data := 0.U
   io.rocc.mem.resp.bits.has_data := false.B
   io.rocc.mem.resp.bits.data_raw := false.B
   io.rocc.mem.perf.release := false.B
   io.rocc.mem.s2_nack := false.B
   io.rocc.mem.s2_xcpt.ma.st := false.B
   io.rocc.mem.s2_xcpt.ma.ld := false.B


   // Wire off other unused CoreIO signals.
   io.fpu <> DontCare
   io.fpu.valid := false.B
   io.fpu.inst := 0.U

   //io.trace := csr.io.trace unused
   io.trace <> DontCare
   io.trace map (t => t.valid := false.B)

   override val compileOptions = chisel3.core.ExplicitCompileOptions.NotStrict.copy(explicitInvalidate = true)
}

