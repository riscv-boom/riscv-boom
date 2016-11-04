//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISC-V Processor Core
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// BOOM has the following (conceptual) stages:
//   if1 - Instruction Fetch 1 (I$ access)
//   if2 - Instruction Fetch 2 (instruction return)
//   bp1 - Branch Predict      (in parallel with IF1)
//   bp2 - Branch Decode       (in parallel with IF2)
//   dec - Decode
//   ren - Rename
//   dis - Dispatch
//   iss - Issue
//   rrd - Register Read
//   exe - Execute
//   mem - Memory
//   wb  - Writeback
//   com - Commit
//
//


package boom

import Chisel._
import cde.Parameters

import rocket.Instructions._
import util.Str


abstract class BoomModule(implicit val p: Parameters) extends Module
  with HasBoomCoreParameters

class BoomBundle(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasBoomCoreParameters


class CacheCounters() extends Bundle
{
   val dc_miss = Bool()
   val ic_miss = Bool()
}


//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

class BOOMCore(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new BoomBundle()(p)
   {
      val interrupts = new rocket.TileInterrupts().asInput
      val hartid     = UInt(INPUT, xLen)
      val imem       = new rocket.FrontendIO
      val dmem       = new DCMemPortIO
      val ptw_dat    = new rocket.DatapathPTWIO().flip
      val ptw_tlb    = new rocket.TLBPTWIO()
      val rocc       = new rocket.RoCCInterface().flip
      val counters   = new CacheCounters().asInput
   }

   //**********************************
   // construct all of the modules

   val exe_units        = new boom.ExecutionUnits()
   val issue_width      = exe_units.length
   val register_width   = if (!usingFPU) xLen else 65

   val fetch_unit       = Module(new FetchUnit(FETCH_WIDTH))
   val bpd_stage        = Module(new BranchPredictionStage(FETCH_WIDTH))
   val dec_serializer   = Module(new FetchSerializerNtoM)
   val decode_units     = for (w <- 0 until DECODE_WIDTH) yield { val d = Module(new DecodeUnit); d }
   val dec_brmask_logic = Module(new BranchMaskGenerationLogic(DECODE_WIDTH))
   val rename_stage     = Module(new RenameStage(DECODE_WIDTH, exe_units.num_wakeup_ports))
   val issue_unit       = if (p(EnableAgePriorityIssue))
                              Module(new IssueUnitCollasping(
                                 p(NumIssueSlotEntries), issue_width, exe_units.num_wakeup_ports))
                          else
                              Module(new IssueUnitStatic(
                                 p(NumIssueSlotEntries), issue_width, exe_units.num_wakeup_ports))
   val regfile          = Module(new RegisterFile(PHYS_REG_COUNT,
                                 exe_units.num_rf_read_ports,
                                 exe_units.num_rf_write_ports,
                                 register_width,
                                 ENABLE_REGFILE_BYPASSING))
   val register_read    = Module(new RegisterRead(
                                 issue_width,
                                 exe_units.map(_.supportedFuncUnits),
                                 exe_units.num_rf_read_ports,
                                 exe_units.map(_.num_rf_read_ports),
                                 exe_units.num_total_bypass_ports,
                                 register_width))
   val csr              = Module(new rocket.CSRFile())

   val rob              = Module(new Rob(
                                 DECODE_WIDTH,
                                 NUM_ROB_ENTRIES,
                                 ISSUE_WIDTH,
                                 exe_units.num_slow_wakeup_ports,
                                 exe_units.num_fpu_ports))
                           // TODO the ROB writeback is off the regfile, which is a different set


   //***********************************
   // Pipeline State Registers and Wires

   // Instruction Decode Stage
   val dec_valids     = Wire(Vec(DECODE_WIDTH, Bool()))  // are the decoded instruction valid? It may be held up though.
   val dec_uops       = Wire(Vec(DECODE_WIDTH, new MicroOp()))
   val dec_will_fire  = Wire(Vec(DECODE_WIDTH, Bool()))  // can the instruction fire beyond decode?
                                                         // (can still be stopped in ren or dis)
   val dec_rdy        = Wire(Bool())

   // Dispatch Stage
   val dis_valids            = Wire(Vec(DISPATCH_WIDTH, Bool())) // true if uop WILL enter IW/ROB
   val dis_uops              = Wire(Vec(DISPATCH_WIDTH, new MicroOp()))

   // Issue Stage/Register Read
   val iss_valids            = Wire(Vec(issue_width, Bool()))
   val iss_uops              = Wire(Vec(issue_width, new MicroOp()))
   val bypasses              = Wire(new BypassData(exe_units.num_total_bypass_ports, register_width))

   // Branch Unit
   val br_unit = Wire(new BranchUnitResp())
   val brunit_idx = exe_units.br_unit_idx
   br_unit <> exe_units.br_unit_io

   // Load/Store Unit
   var lsu_io:LoadStoreUnitIO = null
   lsu_io = exe_units.memory_unit.io.lsu_io
   io.dmem <> exe_units.memory_unit.io.dmem


   //****************************************
   // Time Stamp Counter & Retired Instruction Counter
   // (only used for printf and vcd dumps - the actual counters are in the CSRFile)
   val tsc_reg  = Reg(init = UInt(0, xLen))
   val irt_reg  = Reg(init = UInt(0, xLen))
   tsc_reg  := tsc_reg + Mux(Bool(O3PIPEVIEW_PRINTF), UInt(O3_CYCLE_TIME), UInt(1))
   irt_reg  := irt_reg + PopCount(rob.io.commit.valids.toBits)
   debug(tsc_reg)
   debug(irt_reg)


   //****************************************
   // Print-out information about the machine

   if (usingFPU)         println ("\n    FPU Unit Enabled")
   else                  println ("\n    FPU Unit Disabled")
   if (usingVM)          println ("    VM       Enabled")
   else                  println ("    VM       Disabled")
   if (usingFDivSqrt)    println ("    FDivSqrt Enabled\n")
   else                  println ("    FDivSqrt Disabled\n")

   val iss_str = if (p(EnableAgePriorityIssue)) " (Age-based Priority)"
                 else " (Unordered Priority)"
   println("\n   Fetch Width          : " + FETCH_WIDTH)
   println("   Issue Width          : " + ISSUE_WIDTH)
   println("   ROB Size             : " + NUM_ROB_ENTRIES)
   println("   Issue Window Size    : " + p(NumIssueSlotEntries) + iss_str)
   println("   Load/Store Unit Size : " + p(NumLsuEntries) + "/" + p(NumLsuEntries))
   println("   Num Phys. Registers  : " + p(NumPhysRegisters))
   println("   Max Branch Count     : " + p(MaxBrCount))
   println("   BTB Size             : " + p(rocket.BtbKey).nEntries)
   println("   RAS Size             : " + p(rocket.BtbKey).nRAS)

   println("\n   Num RF Read Ports    : " + exe_units.num_rf_read_ports)
   println("   Num RF Write Ports   : " + exe_units.num_rf_write_ports + "\n")
   println("   RF Cost (R+W)*(R+2W) : " + exe_units.rf_cost + "\n")
   println("   Num Slow Wakeup Ports: " + exe_units.num_slow_wakeup_ports)
   println("   Num Fast Wakeup Ports: " + exe_units.num_fast_wakeup_ports)
   println("   Num Bypass Ports     : " + exe_units.num_total_bypass_ports)
   println("")

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Fetch Stage/Frontend ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   io.imem <> fetch_unit.io.imem
   // TODO: work-around rocket-chip issue #183, broken imem.mask for fetchWidth=1
   // TODO: work-around rocket-chip issue #184, broken imem.mask for fetchWidth=1
   if (FETCH_WIDTH == 1)
   {
      fetch_unit.io.imem.resp.bits.mask := UInt(1)
      fetch_unit.io.imem.resp.bits.btb.bits.bridx := UInt(0)
   }
   fetch_unit.io.br_unit <> br_unit
   fetch_unit.io.tsc_reg           := tsc_reg

   fetch_unit.io.bp2_take_pc       := bpd_stage.io.req.valid
   fetch_unit.io.bp2_pc_of_br_inst := bpd_stage.io.req.bits.br_pc
   fetch_unit.io.bp2_is_jump       := bpd_stage.io.req.bits.is_jump
   fetch_unit.io.bp2_is_cfi        := bpd_stage.io.req.bits.is_cfi
   fetch_unit.io.bp2_is_taken      := bpd_stage.io.req.bits.is_taken
   fetch_unit.io.bp2_br_seen       := bpd_stage.io.pred_resp.br_seen
   fetch_unit.io.bp2_pred_target   := bpd_stage.io.req.bits.target

   fetch_unit.io.clear_fetchbuffer := br_unit.brinfo.mispredict ||
                                       rob.io.flush.valid
   fetch_unit.io.flush_take_pc     := rob.io.flush.valid
   fetch_unit.io.flush_pc          := rob.io.flush.bits.pc

   io.imem.flush_icache :=
      Range(0,DECODE_WIDTH).map{i => rob.io.commit.valids(i) && rob.io.commit.uops(i).is_fencei}.reduce(_|_) ||
      (br_unit.brinfo.mispredict && br_unit.brinfo.is_jr &&  csr.io.status.debug)

   io.imem.flush_tlb := csr.io.fatc

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Branch Prediction ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // These stages are effectively in parallel with instruction fetch and
   // decode.  BHT look-up is in parallel with I$ access, and Branch Decode
   // occurs before fetch buffer insertion.

   //io.imem <> bpd_stage.io.imem
   bpd_stage.io.imem_resp <> io.imem.resp
   bpd_stage.io.btb_resp <> io.imem.resp.bits.btb
   // TODO: work-around rocket-chip issue #183, broken imem.mask for fetchWidth=1
   // TODO: work-around rocket-chip issue #184, broken imem.mask for fetchWidth=1
   if (FETCH_WIDTH == 1)
   {
      bpd_stage.io.imem_resp.bits.mask := UInt(1)
      bpd_stage.io.btb_resp.bits.bridx := UInt(0)
   }
   io.imem.resp.ready <> fetch_unit.io.imem.resp.ready

   bpd_stage.io.npc <> io.imem.npc


   io.imem.ras_update <> bpd_stage.io.ras_update
   bpd_stage.io.br_unit := br_unit
   bpd_stage.io.flush := rob.io.flush.valid
   bpd_stage.io.status_prv := csr.io.status.prv
   bpd_stage.io.req.ready := !fetch_unit.io.stalled

   fetch_unit.io.bp2_pred_resp <> bpd_stage.io.pred_resp
   fetch_unit.io.bp2_predictions <> bpd_stage.io.predictions

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Decode Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // track mask of finished instructions in the bundle
   // use this to mask out insts coming from FetchBuffer that have been finished
   // for example, back pressure may cause us to only issue some instructions from FetchBuffer
   // but on the next cycle, we only want to retry a subset
   val dec_finished_mask = Reg(init = Bits(0, DECODE_WIDTH))

   // TODO need to generalize this logic to other width disparities
   require (DECODE_WIDTH == FETCH_WIDTH)

   //-------------------------------------------------------------
   // Pull out instructions and send to the Decoders

   dec_serializer.io.enq <> fetch_unit.io.resp

   dec_serializer.io.kill := fetch_unit.io.clear_fetchbuffer
   dec_serializer.io.deq.ready := dec_rdy

   val fetched_inst_valid = dec_serializer.io.deq.valid
   val dec_fbundle        = dec_serializer.io.deq.bits

   //-------------------------------------------------------------
   // Decoders

   // allow early instructions to stall later instructions
   var dec_stall_next_inst = Bool(false)
   var dec_last_inst_was_stalled = Bool(false)

   // stall fetch/dcode because we ran out of branch tags
   val branch_mask_full = Wire(Vec(DECODE_WIDTH, Bool()))

   for (w <- 0 until DECODE_WIDTH)
   {
      dec_valids(w)                      := fetched_inst_valid && dec_fbundle.uops(w).valid && !dec_finished_mask(w)
      decode_units(w).io.enq.uop         := dec_fbundle.uops(w)
      decode_units(w).io.status          := csr.io.status
      decode_units(w).io.interrupt       := csr.io.interrupt
      decode_units(w).io.interrupt_cause := csr.io.interrupt_cause

      val prev_insts_in_bundle_valid = Range(0,w).map{i => dec_valids(i)}.foldLeft(Bool(false))(_|_)

      // stall this instruction?
      // TODO tailor this to only care if a given instruction uses a resource?
      val stall_me = (dec_valids(w) &&
                        (  !(rename_stage.io.inst_can_proceed(w))
                        || (dec_valids(w) && dec_uops(w).is_unique &&
                           (!rob.io.empty || !lsu_io.lsu_fencei_rdy || prev_insts_in_bundle_valid))
                        || !rob.io.ready
                        || lsu_io.laq_full
                        || lsu_io.stq_full
                        || branch_mask_full(w)
                        || br_unit.brinfo.mispredict
                        || rob.io.flush.valid
                        || dec_stall_next_inst
                        || !bpd_stage.io.brob.allocate.ready
                        || (dec_valids(w) && dec_uops(w).is_fencei && !lsu_io.lsu_fencei_rdy)
                        )) ||
                     dec_last_inst_was_stalled

      // stall the next instruction following me in the decode bundle?
      dec_last_inst_was_stalled = stall_me
      dec_stall_next_inst  = stall_me || (dec_valids(w) && dec_uops(w).is_unique)

      dec_will_fire(w) := dec_valids(w) && !stall_me && !fetch_unit.io.clear_fetchbuffer
      dec_uops(w)      := decode_units(w).io.deq.uop


      if (O3PIPEVIEW_PRINTF)
      {
         when (dec_will_fire(w))
         {
            // TODO handle spitting out decode for when a uop gets stalled at the front of the fetch-buffer
            val fetch_seq = dec_uops(w).debug_events.fetch_seq
            printf("%d; O3PipeView:decode:%d\n", fetch_seq, tsc_reg)
            printf("%d; O3PipeView:rename: 0\n", fetch_seq)
            printf("%d; O3PipeView:dispatch: 0\n", fetch_seq)
         }
      }
   }

   // all decoders are empty and ready for new instructions
   dec_rdy := !(dec_stall_next_inst)

   when (dec_rdy || fetch_unit.io.clear_fetchbuffer)
   {
      dec_finished_mask := Bits(0)
   }
   .otherwise
   {
      dec_finished_mask := dec_will_fire.toBits | dec_finished_mask
   }

   //-------------------------------------------------------------
   // Branch Mask Logic


   dec_brmask_logic.io.brinfo := br_unit.brinfo
   dec_brmask_logic.io.flush_pipeline := rob.io.flush.valid

   for (w <- 0 until DECODE_WIDTH)
   {
      dec_brmask_logic.io.is_branch(w) := !dec_finished_mask(w) && dec_uops(w).allocate_brtag
      dec_brmask_logic.io.will_fire(w) :=  dis_valids(w) && dec_uops(w).allocate_brtag // ren, dis can back pressure us

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

   for (w <- 0 until DECODE_WIDTH)
   {
      dec_uops(w).ldq_idx := new_lidx
      dec_uops(w).stq_idx := new_sidx

      new_lidx = Mux(dec_will_fire(w) && dec_uops(w).is_load,  WrapInc(new_lidx, NUM_LSU_ENTRIES), new_lidx)
      new_sidx = Mux(dec_will_fire(w) && dec_uops(w).is_store, WrapInc(new_sidx, NUM_LSU_ENTRIES), new_sidx)
   }


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Register Rename Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   rename_stage.io.dis_inst_can_proceed := issue_unit.io.dis_readys
   rename_stage.io.ren_pred_info := dec_fbundle.pred_resp

   rename_stage.io.kill     := fetch_unit.io.clear_fetchbuffer // mispredict or flush
   rename_stage.io.brinfo   := br_unit.brinfo
   rename_stage.io.get_pred.br_tag        := iss_uops(brunit_idx).br_tag
   exe_units(brunit_idx).io.get_pred.info := Reg(next=rename_stage.io.get_pred.info)

   rename_stage.io.flush_pipeline := rob.io.flush.valid

   for (w <- 0 until DECODE_WIDTH)
   {
      rename_stage.io.dec_mask(w) := dec_will_fire(w)
   }

   rename_stage.io.dec_uops := dec_uops

   var wu_idx = 0
   // loop through each issue-port (exe_units are statically connected to an issue-port)
   for (i <- 0 until exe_units.length)
   {
      // Fast Wakeup (uses just-issued uops) that have known latencies
      if (exe_units(i).isBypassable)
      {
         rename_stage.io.wb_valids(wu_idx) := iss_valids(i) &&
                                              (iss_uops(i).dst_rtype === RT_FIX || iss_uops(i).dst_rtype === RT_FLT) &&
                                              (iss_uops(i).bypassable)
         rename_stage.io.wb_pdsts(wu_idx)  := iss_uops(i).pdst
         wu_idx += 1
         assert (!(iss_uops(i).dst_rtype === RT_FLT && iss_uops(i).bypassable), "Bypassing FP is not supported.")
      }


      // Slow Wakeup (uses write-port to register file)
      for (j <- 0 until exe_units(i).num_rf_write_ports)
      {
         rename_stage.io.wb_valids(wu_idx) := exe_units(i).io.resp(j).valid &&
                                              exe_units(i).io.resp(j).bits.uop.ctrl.rf_wen && // TODO is rfwen redudant?
                                              !exe_units(i).io.resp(j).bits.uop.bypassable &&
                                              (exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FIX ||
                                                 exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FLT)
         rename_stage.io.wb_pdsts(wu_idx)  := exe_units(i).io.resp(j).bits.uop.pdst
         wu_idx += 1
      }

   }
   require (wu_idx == exe_units.num_wakeup_ports)

   rename_stage.io.com_valids := rob.io.commit.valids
   rename_stage.io.com_uops := rob.io.commit.uops
   rename_stage.io.com_rbk_valids := rob.io.commit.rbk_valids

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Dispatch Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // TODO get rid of, let the ROB handle this...?
   val dis_curr_rob_row_idx = Wire(UInt(width = ROB_ADDR_SZ))

   for (w <- 0 until DECODE_WIDTH)
   {
      dis_valids(w)         := rename_stage.io.ren_mask(w)
      dis_uops(w)         := rename_stage.io.ren_uops(w)
      // TODO probably don't need to do this, since we're going ot do it in the issue window?
      dis_uops(w).br_mask := GetNewBrMask(br_unit.brinfo, rename_stage.io.ren_uops(w))

      // note: this assumes uops haven't been shifted - there's a 1:1 match between PC's LSBs and "w" here
      // (thus the LSB of the rob_idx gives part of the PC)
      if (DECODE_WIDTH == 1)
         dis_uops(w).rob_idx := dis_curr_rob_row_idx
      else
         dis_uops(w).rob_idx := Cat(dis_curr_rob_row_idx, UInt(w, log2Up(DECODE_WIDTH)))

      dis_uops(w).brob_idx := bpd_stage.io.brob.allocate_brob_tail

   }

   val dis_has_unique = Range(0,DISPATCH_WIDTH).map{w =>
      dis_valids(w) && dis_uops(w).is_unique}.reduce(_|_)
   val dec_has_br_or_jalr_in_packet =
      Range(0,DECODE_WIDTH).map{w =>
         dec_valids(w) && dec_uops(w).br_prediction.is_br_or_jalr}.reduce(_|_) &&
      !dis_has_unique


   bpd_stage.io.brob.allocate.valid := dis_valids.reduce(_|_) &&
                                       dec_finished_mask === Bits(0) &&
                                       dec_has_br_or_jalr_in_packet
   bpd_stage.io.brob.allocate.bits.ctrl.executed.map{_ := Bool(false)}
   bpd_stage.io.brob.allocate.bits.ctrl.taken.map{_ := Bool(false)}
   bpd_stage.io.brob.allocate.bits.ctrl.mispredicted.map{_ := Bool(false)}
   bpd_stage.io.brob.allocate.bits.ctrl.debug_executed := Bool(false)
   bpd_stage.io.brob.allocate.bits.ctrl.debug_rob_idx := dis_uops(0).rob_idx
   bpd_stage.io.brob.allocate.bits.ctrl.brob_idx := dis_uops(0).brob_idx
   bpd_stage.io.brob.allocate.bits.info := dec_fbundle.pred_resp.bpd_resp


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Issue Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // Input (Dispatch)
   issue_unit.io.dis_valids := dis_valids
   issue_unit.io.dis_uops   := dis_uops
   issue_unit.io.tsc_reg    := tsc_reg

   // Output (Issue)

   for (w <- 0 until issue_width)
   {
      iss_valids(w) := issue_unit.io.iss_valids(w)
      iss_uops(w)   := issue_unit.io.iss_uops(w)

      issue_unit.io.fu_types(w) := exe_units(w).io.fu_types
   }


   issue_unit.io.brinfo := br_unit.brinfo
   issue_unit.io.flush_pipeline := rob.io.flush.valid

   wu_idx = 0
   for (i <- 0 until exe_units.length)
   {
      // Slow Wakeup (uses write-port to register file)
      for (j <- 0 until exe_units(i).num_rf_write_ports)
      {
         issue_unit.io.wakeup_pdsts(wu_idx).valid := exe_units(i).io.resp(j).valid &&
                                                     exe_units(i).io.resp(j).bits.uop.ctrl.rf_wen && // TODO get rid of other rtype checks
                                                     !exe_units(i).io.resp(j).bits.uop.bypassable &&
                                                     (exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FIX ||
                                                      exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FLT)
         issue_unit.io.wakeup_pdsts(wu_idx).bits  := exe_units(i).io.resp(j).bits.uop.pdst
         wu_idx += 1
      }

      // Fast Wakeup (uses just-issued uops)

      if (exe_units(i).isBypassable)
      {
         issue_unit.io.wakeup_pdsts(wu_idx).valid := iss_valids(i) &&
                                                     (iss_uops(i).dst_rtype === RT_FIX || iss_uops(i).dst_rtype === RT_FLT) &&
                                                     iss_uops(i).ldst_val &&
                                                     (iss_uops(i).bypassable)
         issue_unit.io.wakeup_pdsts(wu_idx).bits  := iss_uops(i).pdst
         wu_idx += 1
      }
   }
   require (wu_idx == exe_units.num_wakeup_ports)

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Register Read Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // Register Read <- Issue (rrd <- iss)
//   regfile.io.read_ports <> register_read.io.rf_read_ports
   register_read.io.rf_read_ports <> regfile.io.read_ports

   for (w <- 0 until issue_width)
   {
      register_read.io.iss_valids(w) := iss_valids(w)
      register_read.io.iss_uops(w) := iss_uops(w)
   }

   register_read.io.brinfo := br_unit.brinfo
   register_read.io.kill   := rob.io.flush.valid

   register_read.io.bypass := bypasses

   //-------------------------------------------------------------
   // Privileged Co-processor 0 Register File
   // Note: Normally this would be bad in that I'm writing state before
   // committing, so to get this to work I stall the entire pipeline for
   // CSR instructions so I never speculate these instructions.

   require (exe_units(0).uses_csr_wport)

   // for critical path reasons, we aren't zero'ing this out if resp is not valid
   val csr_rw_cmd = exe_units(0).io.resp(0).bits.uop.ctrl.csr_cmd
   val wb_wdata = exe_units(0).io.resp(0).bits.data

   csr.io.rw.addr  := exe_units(0).io.resp(0).bits.uop.csr_addr
   csr.io.rw.cmd   := Mux(exe_units(0).io.resp(0).valid, csr_rw_cmd, rocket.CSR.N)
   csr.io.rw.wdata :=wb_wdata

   // Extra I/O
   csr.io.retire    := PopCount(rob.io.commit.valids.toBits)
   csr.io.exception := rob.io.com_xcpt.valid && !csr.io.csr_xcpt
   csr.io.pc        := rob.io.com_xcpt.bits.pc
   csr.io.cause     := rob.io.com_xcpt.bits.cause
   csr.io.badaddr   := rob.io.com_xcpt.bits.badvaddr


   // reading requires serializing the entire pipeline
   csr.io.fcsr_flags.valid := rob.io.commit.fflags.valid
   csr.io.fcsr_flags.bits  := rob.io.commit.fflags.bits

   exe_units.map(_.io.fcsr_rm := csr.io.fcsr_rm)

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
      exe_units(w).io.req <> register_read.io.exe_reqs(w)
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


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Memory Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // enqueue basic load/store info in Decode
   lsu_io.dec_uops := dec_uops

   for (w <- 0 until DECODE_WIDTH)
   {
      lsu_io.dec_st_vals(w) := dec_will_fire(w) && rename_stage.io.inst_can_proceed(w) && !rob.io.flush.valid &&
                               dec_uops(w).is_store
      lsu_io.dec_ld_vals(w) := dec_will_fire(w) && rename_stage.io.inst_can_proceed(w) && !rob.io.flush.valid &&
                               dec_uops(w).is_load

      lsu_io.dec_uops(w).rob_idx := dis_uops(w).rob_idx // for debug purposes (comit logging)
   }

   lsu_io.commit_store_mask := rob.io.commit.st_mask
   lsu_io.commit_load_mask  := rob.io.commit.ld_mask
   lsu_io.commit_load_at_rob_head := rob.io.com_load_is_at_rob_head

   //com_xcpt.valid comes too early, will fight against a branch that resolves same cycle as an exception
   lsu_io.exception := rob.io.flush.valid

   // Handle Branch Mispeculations
   lsu_io.brinfo := br_unit.brinfo
   io.dmem.brinfo := br_unit.brinfo

   new_ldq_idx := lsu_io.new_ldq_idx
   new_stq_idx := lsu_io.new_stq_idx

   lsu_io.debug_tsc := tsc_reg

   io.dmem.flush_pipe := rob.io.flush.valid


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Writeback Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   var w_cnt = 0
   for (i <- 0 until exe_units.length)
   {
      for (j <- 0 until exe_units(i).num_rf_write_ports)
      {
         if (exe_units(i).data_width > 64)
         {
            assert (!(exe_units(i).io.resp(j).valid &&
                      exe_units(i).io.resp(j).bits.uop.ctrl.rf_wen &&
                      exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FIX &&
                      exe_units(i).io.resp(j).bits.data(64).toBool),
                      "the 65th bit was set on a fixed point write-back to the regfile.")
         }



         if (exe_units(i).uses_csr_wport && (j == 0))
         {
            regfile.io.write_ports(w_cnt).wen :=
               exe_units(i).io.resp(j).valid &&
               exe_units(i).io.resp(j).bits.uop.ctrl.rf_wen && // TODO get rid of other checks
               (exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FIX ||
                  exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FLT)
            regfile.io.write_ports(w_cnt).addr :=
               exe_units(i).io.resp(j).bits.uop.pdst
            regfile.io.write_ports(w_cnt).data :=
               Mux(exe_units(i).io.resp(j).bits.uop.ctrl.csr_cmd =/= rocket.CSR.N,
                  csr.io.rw.rdata,
                  exe_units(i).io.resp(j).bits.data)
         }
         else
         {
            regfile.io.write_ports(w_cnt).wen :=
               exe_units(i).io.resp(j).valid &&
               exe_units(i).io.resp(j).bits.uop.ctrl.rf_wen && // TODO get rid of other checks
               (exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FIX ||
                  exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FLT)
            regfile.io.write_ports(w_cnt).addr :=
               exe_units(i).io.resp(j).bits.uop.pdst
            regfile.io.write_ports(w_cnt).data :=
               exe_units(i).io.resp(j).bits.data
         }
         w_cnt += 1
      }
   }


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Commit Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // Dispatch
   rob.io.dis_uops := dis_uops
   rob.io.dis_valids := dis_valids
   rob.io.dis_has_br_or_jalr_in_packet := dec_has_br_or_jalr_in_packet
   rob.io.dis_partial_stall := !dec_rdy && !dis_valids(DECODE_WIDTH-1)
   rob.io.dis_new_packet := dec_finished_mask === Bits(0)
   rob.io.debug_tsc := tsc_reg

   dis_curr_rob_row_idx  := rob.io.curr_rob_tail

   // Writeback
   var cnt = 0
   var f_cnt = 0 // rob fflags port index
   for (w <- 0 until exe_units.length)
   {
      for (j <- 0 until exe_units(w).num_rf_write_ports)
      {
         val wb_uop = exe_units(w).io.resp(j).bits.uop
         rob.io.wb_resps(cnt).valid := exe_units(w).io.resp(j).valid && !(wb_uop.is_store && !wb_uop.is_amo)
         rob.io.wb_resps(cnt).bits <> exe_units(w).io.resp(j).bits

         // for commit logging...
         rob.io.debug_wb_valids(cnt) := exe_units(w).io.resp(j).valid &&
                                        wb_uop.ctrl.rf_wen &&
                                        (wb_uop.dst_rtype === RT_FIX || wb_uop.dst_rtype === RT_FLT)

         val data = exe_units(w).io.resp(j).bits.data
         if (exe_units(w).hasFFlags || (exe_units(w).is_mem_unit && usingFPU))
         {
            if (exe_units(w).hasFFlags)
            {
               rob.io.fflags(f_cnt) <> exe_units(w).io.resp(j).bits.fflags
               f_cnt += 1
            }
            val unrec_s = hardfloat.fNFromRecFN(8, 24, data)
            val unrec_d = hardfloat.fNFromRecFN(11, 53, data)
            val unrec_out     = Mux(wb_uop.fp_single, Cat(UInt(0,32), unrec_s), unrec_d)
            if (exe_units(w).uses_csr_wport && (j == 0))
            {
               rob.io.debug_wb_wdata(cnt) := Mux(wb_uop.ctrl.csr_cmd =/= rocket.CSR.N, csr.io.rw.rdata,
                                             Mux(wb_uop.fp_val && wb_uop.dst_rtype === RT_FLT, unrec_out,
                                                                                               data))
            }
            else
            {
               rob.io.debug_wb_wdata(cnt) := Mux(exe_units(w).io.resp(j).bits.uop.fp_val, unrec_out, data)
            }
         }
         else
         {
            if (exe_units(w).uses_csr_wport && (j == 0))
            {
               rob.io.debug_wb_wdata(cnt) := Mux(wb_uop.ctrl.csr_cmd =/= rocket.CSR.N, csr.io.rw.rdata, data)
            }
            else
            {
               rob.io.debug_wb_wdata(cnt) := data
            }
         }
         cnt += 1
      }
   }

   // branch resolution
   rob.io.brinfo <> br_unit.brinfo  // TODO XXX chisel3 question
//   rob.io.brinfo <> br_unit.brinfo.asInput

   // branch unit requests PCs and predictions from ROB during register read
   // (fetch PC from ROB cycle earlier than needed for critical path reasons)
   rob.io.get_pc.rob_idx := iss_uops(brunit_idx).rob_idx
   exe_units(brunit_idx).io.get_rob_pc.curr_pc  := Reg(next=rob.io.get_pc.curr_pc)
   exe_units(brunit_idx).io.get_rob_pc.curr_brob_idx  := Reg(next=rob.io.get_pc.curr_brob_idx)
   exe_units(brunit_idx).io.get_rob_pc.next_val := Reg(next=rob.io.get_pc.next_val)
   exe_units(brunit_idx).io.get_rob_pc.next_pc  := Reg(next=rob.io.get_pc.next_pc)
   exe_units(brunit_idx).io.status := csr.io.status

   // LSU <> ROB
   rob.io.lsu_clr_bsy_valid   := lsu_io.lsu_clr_bsy_valid
   rob.io.lsu_clr_bsy_rob_idx := lsu_io.lsu_clr_bsy_rob_idx
   rob.io.lxcpt <> lsu_io.xcpt

   rob.io.cxcpt.valid := csr.io.csr_xcpt
   rob.io.csr_eret := csr.io.eret
   rob.io.csr_evec := csr.io.evec

   rob.io.bxcpt <> br_unit.xcpt  // TODO XXX chisel3 question
//   rob.io.bxcpt <> br_unit.xcpt.asInput


   bpd_stage.io.brob.deallocate <> rob.io.brob_deallocate
   bpd_stage.io.brob.bpd_update <> br_unit.bpd_update
   bpd_stage.io.brob.flush := rob.io.flush.valid || rob.io.clear_brob

   //-------------------------------------------------------------
   // **** Flush Pipeline ****
   //-------------------------------------------------------------
   // flush on exceptions, miniexeptions, and after some special instructions

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
   val idle_cycles = util.WideCounter(32)
   when (rob.io.commit.valids.toBits.orR || reset.toBool) { idle_cycles := UInt(0) }
   assert (!(idle_cycles.value(13)), "Pipeline has hung.")


   //-------------------------------------------------------------
   // Uarch Hardware Performance Events (HPEs)

   csr.io.events.map(_ := UInt(0))

   require (nPerfEvents > 29)
   println ("   " + nPerfCounters + " HPM counters enabled (with " + nPerfEvents + " events).")

   // Execution-time branch prediction accuracy.
   csr.io.events(0) := br_unit.brinfo.valid
   csr.io.events(1) := br_unit.brinfo.mispredict

   // User-level instruction count.
   csr.io.events(2) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && (csr.io.status.prv === UInt(rocket.PRV.U))})

   // L1 cache stats.
   csr.io.events(3) := io.counters.dc_miss
   csr.io.events(4) := io.counters.ic_miss

   csr.io.events(5)  := csr.io.status.prv === UInt(rocket.PRV.U)

   // Instruction mixes.
   csr.io.events(6)  := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal})
   csr.io.events(7)  := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_jal})
   csr.io.events(8)  := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_jump && !rob.io.commit.uops(w).is_jal})
   csr.io.events(9)  := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_load})
   csr.io.events(10) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_store})
   csr.io.events(11) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).fp_val})

   // Decode stall causes.
   csr.io.events(12) := !rob.io.ready
   csr.io.events(13) := lsu_io.laq_full
   csr.io.events(14) := lsu_io.stq_full
   csr.io.events(15) := !issue_unit.io.dis_readys.reduce(_|_)
   csr.io.events(16) := branch_mask_full.reduce(_|_)
   csr.io.events(17) := rob.io.flush.valid

   // LSU Speculation stats.
   csr.io.events(18) := lsu_io.counters.ld_valid
   csr.io.events(19) := lsu_io.counters.stld_order_fail
   csr.io.events(20) := lsu_io.counters.ldld_order_fail

   // Branch prediction stats.
   csr.io.events(21)  := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
      rob.io.commit.uops(w).stat_brjmp_mispredicted})
   csr.io.events(22) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
      rob.io.commit.uops(w).stat_btb_made_pred})
   csr.io.events(23) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
      rob.io.commit.uops(w).stat_btb_mispredicted})
   csr.io.events(24) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
      rob.io.commit.uops(w).stat_bpd_made_pred})
   csr.io.events(25) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
      rob.io.commit.uops(w).stat_bpd_mispredicted})

   // Branch prediction - no prediction made.
   csr.io.events(26) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
      !rob.io.commit.uops(w).stat_btb_made_pred && !rob.io.commit.uops(w).stat_bpd_made_pred})

   // Branch prediction - no predition made & a mispredict occurred.
   csr.io.events(27) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && !rob.io.commit.uops(w).is_jal &&
      !rob.io.commit.uops(w).stat_btb_made_pred && !rob.io.commit.uops(w).stat_bpd_made_pred &&
      rob.io.commit.uops(w).stat_brjmp_mispredicted})


   // Count user-level branches (subtract from total to get privilege branch accuracy)
   csr.io.events(28) := br_unit.brinfo.valid && (csr.io.status.prv === UInt(rocket.PRV.U))
   csr.io.events(29) := br_unit.brinfo.mispredict && (csr.io.status.prv === UInt(rocket.PRV.U))

   // count change of privilege modes
   csr.io.events(30) := csr.io.status.prv =/= RegNext(csr.io.status.prv)

   assert (!(Range(0,COMMIT_WIDTH).map{w =>
      rob.io.commit.valids(w) && rob.io.commit.uops(w).is_br_or_jmp && rob.io.commit.uops(w).is_jal &&
      rob.io.commit.uops(w).stat_brjmp_mispredicted}.reduce(_|_)),
      "[dpath] A committed JAL was marked as having been mispredicted.")

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Handle Cycle-by-Cycle Printouts ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   if (DEBUG_PRINTF)
   {
      println("\n Chisel Printout Enabled\n")

      var whitespace = (63 + 1 - 3 - 12  - NUM_LSU_ENTRIES- p(NumIssueSlotEntries) - (NUM_ROB_ENTRIES/COMMIT_WIDTH)
         - NUM_BROB_ENTRIES
      )

      printf("--- Cyc=%d , ----------------- Ret: %d ----------------------------------\n  "
         , tsc_reg
         , irt_reg & UInt(0xffffff))

      for (w <- 0 until DECODE_WIDTH)
      {
         if (w == 0)
         {
            printf("Dec:  ([0x%x]                        ", rename_stage.io.ren_uops(w).pc(19,0))
         }
         else
         {
            printf("[0x%x]                        ", rename_stage.io.ren_uops(w).pc(19,0))
         }
      }

      if (DEBUG_PRINTF_ROB)
      {
//         printf(") State: (%s: %s %s %s \u001b[1;31m%s\u001b[0m %s %s) BMsk:%x Mode:%s\n"
         printf(") ctate: (%c: %c %c %c %c %c %c) BMsk:%x Mode:%c\n"
         // chisel3 lacks %s support
//         , Mux(rob.io.debug.state === UInt(0), Str("RESET"),
//           Mux(rob.io.debug.state === UInt(1), Str("NORMAL"),
//           Mux(rob.io.debug.state === UInt(2), Str("ROLLBK"),
//           Mux(rob.io.debug.state === UInt(3), Str("WAIT_E"),
         , Mux(rob.io.debug.state === UInt(0), Str("R"),
           Mux(rob.io.debug.state === UInt(1), Str("N"),
           Mux(rob.io.debug.state === UInt(2), Str("B"),
           Mux(rob.io.debug.state === UInt(3), Str("W"),
                                               Str(" ")))))
//         , Mux(rob.io.ready,Str("_"), Str("!ROB_RDY"))
         , Mux(rob.io.ready,Str("_"), Str("!"))
//         , Mux(lsu_io.laq_full, Str("LAQ_FULL"), Str("_"))
//         , Mux(lsu_io.stq_full, Str("STQ_FULL"), Str("_"))
         , Mux(lsu_io.laq_full, Str("L"), Str("_"))
         , Mux(lsu_io.stq_full, Str("S"), Str("_"))
//         , Mux(rob.io.flush.valid, Str("FLUSH_PIPELINE"), Str(" "))
         , Mux(rob.io.flush.valid, Str("F"), Str(" "))
//         , Mux(branch_mask_full.reduce(_|_), Str("BR_MSK_FULL"), Str(" "))
         , Mux(branch_mask_full.reduce(_|_), Str("B"), Str(" "))
//         , Mux(io.dmem.req.ready, Str("D$_Rdy"), Str("D$_BSY"))
         , Mux(io.dmem.req.ready, Str("R"), Str("B"))
         , dec_brmask_logic.io.debug.branch_mask
         , Mux(csr.io.status.prv === Bits(0x3), Str("M"),
           Mux(csr.io.status.prv === Bits(0x0), Str("U"),
           Mux(csr.io.status.prv === Bits(0x1), Str("S"),  //2 is H
                                                 Str("?"))))
         )
      }


      for (w <- 0 until DECODE_WIDTH)
      {
         printf("(%c%c) " + "DASM(%x)" + " |  "
            , Mux(fetched_inst_valid && dec_fbundle.uops(w).valid && !dec_finished_mask(w), Str("v"), Str("-"))
            , Mux(dec_will_fire(w), Str("V"), Str("-"))
            , dec_fbundle.uops(w).inst
            )
      }

      printf(")\n   fin(%x)", dec_finished_mask)

      for (w <- 0 until DECODE_WIDTH)
      {
         printf("  [ISA:%d,%d,%d,%d] [Phs:%d(%c)%d[%c](%c)%d[%c](%c)%d[%c](%c)] "
            , dec_uops(w).ldst
            , dec_uops(w).lrs1
            , dec_uops(w).lrs2
            , dec_uops(w).lrs3
            , dis_uops(w).pdst
            , Mux(dec_uops(w).dst_rtype   === RT_FIX, Str("X")
              , Mux(dec_uops(w).dst_rtype === RT_X  , Str("-")
              , Mux(dec_uops(w).dst_rtype === RT_FLT, Str("f")
              , Mux(dec_uops(w).dst_rtype === RT_PAS, Str("C"), Str("?")))))
            , dis_uops(w).pop1
            , Mux(rename_stage.io.ren_uops(w).prs1_busy, Str("B"), Str("R"))
            , Mux(dec_uops(w).lrs1_rtype    === RT_FIX, Str("X")
               , Mux(dec_uops(w).lrs1_rtype === RT_X  , Str("-")
               , Mux(dec_uops(w).lrs1_rtype === RT_FLT, Str("f")
               , Mux(dec_uops(w).lrs1_rtype === RT_PAS, Str("C"), Str("?")))))
            , dis_uops(w).pop2
            , Mux(rename_stage.io.ren_uops(w).prs2_busy, Str("B"), Str("R"))
            , Mux(dec_uops(w).lrs2_rtype    === RT_FIX, Str("X")
               , Mux(dec_uops(w).lrs2_rtype === RT_X  , Str("-")
               , Mux(dec_uops(w).lrs2_rtype === RT_FLT, Str("f")
               , Mux(dec_uops(w).lrs2_rtype === RT_PAS, Str("C"), Str("?")))))
            , dis_uops(w).pop3
            , Mux(rename_stage.io.ren_uops(w).prs3_busy, Str("B"), Str("R"))
            , Mux(dec_uops(w).frs3_en, Str("f"), Str("-"))
            )
      }


      printf("Exct(%c%d) Commit(%x) fl: 0x%x (%d) is: 0x%x (%d)\n"
         , Mux(rob.io.com_xcpt.valid, Str("E"), Str("-"))
         , rob.io.com_xcpt.bits.cause
         , rob.io.commit.valids.toBits
         , rename_stage.io.debug.freelist
         , PopCount(rename_stage.io.debug.freelist)
         , rename_stage.io.debug.isprlist
         , PopCount(rename_stage.io.debug.isprlist)
         )

      // branch unit
      printf("                          Branch Unit: %c,%c,%d PC=0x%x, %d Targ=0x%x NPC=%d,0x%x %d%d\n"
         , Mux(br_unit.brinfo.valid,Str("V"), Str(" "))
         , Mux(br_unit.brinfo.mispredict, Str("M"), Str(" "))
         , br_unit.brinfo.taken
         , br_unit.btb_update.br_pc(19,0)
         , br_unit.btb_update_valid
         , br_unit.btb_update.target(19,0)
         , exe_units(brunit_idx).io.get_rob_pc.next_val
         , exe_units(brunit_idx).io.get_rob_pc.next_pc(19,0)
         , br_unit.btb_update.isJump
         , br_unit.btb_update.isReturn
      )

//      printf("  Mem[%s l%d](%s:%d),%s,%s %s %s %s]\n"
//            , Mux(io.dmem.debug.memreq_val, Str("MREQ"), Str(" "))
//            , io.dmem.debug.memreq_lidx
//            , Mux(io.dmem.debug.memresp_val, Str("MRESP"), Str(" "))
//            , io.dmem.debug.cache_resp_tag
//            , Mux(io.dmem.debug.req_kill, Str("RKILL"), Str(" "))
//            , Mux(io.dmem.debug.cache_not_ready, Str("CBUSY"), Str(" "))
//            , Mux(io.dmem.debug.nack, Str("NACK"), Str(" "))
//            , Mux(io.dmem.debug.cache_nack, Str("CN"), Str(" "))
//            , Mux(lsu_io.forward_val, Str("FWD"), Str(" "))
//            //, Mux(lsu_io.debug.tlb_miss, Str("TLB-MISS"), Str("-"))
//            //, Mux(lsu_io.debug.tlb_ready, Str("TLB-RDY"), Str("-"))
//      )

      //for (i <- 0 until io.dmem.debug.ld_req_slot.size)
      //{
      //   printf("     ld_req_slot[%d]=(%s%s) - laq_idx:%d pdst: %d bm:%x\n"
      //      , UInt(i)
      //      , Mux(io.dmem.debug.ld_req_slot(i).valid, Str("V"), Str("-"))
      //      , Mux(io.dmem.debug.ld_req_slot(i).killed, Str("K"), Str("-"))
      //      , io.dmem.debug.ld_req_slot(i).uop.ldq_idx
      //      , io.dmem.debug.ld_req_slot(i).uop.pdst
      //      , io.dmem.debug.ld_req_slot(i).uop.br_mask
      //   )
      //}

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
         printf("\n")
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
      for (i <- 0 until COMMIT_WIDTH)
      {
         when (rob.io.commit.valids(i))
         {
            printf("%d; O3PipeView:retire:%d:store: 0\n",
               rob.io.commit.uops(i).debug_events.fetch_seq,
               tsc_reg)
         }
      }
   }

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // Page Table Walker

   io.ptw_tlb <> lsu_io.ptw

   io.ptw_dat.ptbr       := csr.io.ptbr
   io.ptw_dat.invalidate := csr.io.fatc
   io.ptw_dat.status     := csr.io.status
   io.dmem.invalidate_lr := rob.io.com_xcpt.valid

   //-------------------------------------------------------------
   //-------------------------------------------------------------

}

