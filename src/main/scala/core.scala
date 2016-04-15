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


package boom
{

import Chisel._
import Node._
import cde.Parameters

import rocket.Instructions._
import rocket.Str
import uncore.HtifIO


abstract class BoomModule(implicit val p: Parameters) extends Module
  with HasBoomCoreParameters

class BoomBundle(implicit val p: Parameters) extends junctions.ParameterizedBundle()(p)
  with HasBoomCoreParameters


class FetchBundle(implicit p: Parameters) extends BoomBundle()(p)
{
   val pc          = UInt(width = vaddrBits+1)
   val insts       = Vec.fill(FETCH_WIDTH) {Bits(width = 32)}
   val mask        = Bits(width = FETCH_WIDTH) // mark which words are valid instructions
   val xcpt_if     = Bool()

   val pred_resp   = new BranchPredictionResp
   val predictions = Vec.fill(FETCH_WIDTH) {new BranchPrediction}

   val debug_events = Vec.fill(FETCH_WIDTH) {new DebugStageEvents}

  override def cloneType: this.type = new FetchBundle().asInstanceOf[this.type]
}

class CacheCounters() extends Bundle
{
   val dc_miss = Bool()
   val ic_miss = Bool()
}


class BoomIO()(implicit p: Parameters) extends BoomBundle()
{
      val host     = new HtifIO
      val dmem     = new DCMemPortIO
      val imem     = new rocket.FrontendIO
      val ptw_dat  = new rocket.DatapathPTWIO().flip
      val ptw_tlb  = new rocket.TLBPTWIO()
      val rocc     = new rocket.RoCCInterface().flip
      val counters = new CacheCounters().asInput
}

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

class BOOMCore(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new BoomIO()

   //**********************************
   // construct the execution units

   val exe_units = new boom.ExecutionUnits()


   //**********************************
   // Pipeline State Registers &
   // Forward Declared Wires

   val flush_take_pc  = Wire(Bool())  // redirect PC due to a flush
   val flush_pc       = Wire(UInt())
   val flush_pipeline = Wire(Bool())  // kill entire pipeline (i.e., exception, load misspeculations)

   // Instruction Fetch State
   val if_pc_next     = Wire(UInt(width = vaddrBits+1))
   val csr_take_pc    = Wire(Bool())
   val csr_evec       = Wire(UInt())


   // Branch Predict State
   val bp2_take_pc       = Wire(Bool())
   val bp2_pred_target   = Wire(UInt(width=vaddrBits+1))
   val bp2_pc_of_br_inst = Wire(UInt(width=vaddrBits+1))
   val bp2_is_jump       = Wire(Bool())
   val bp2_is_taken      = Wire(Bool())
   val bp2_br_seen       = Wire(Bool())

   // Instruction Decode State
   val dec_valids     = Wire(Vec(DECODE_WIDTH, Bool()))  // are the decoded instruction valid? It may be held up though.
   val dec_uops       = Wire(Vec(DECODE_WIDTH, new MicroOp()))
   val dec_will_fire  = Wire(Vec(DECODE_WIDTH, Bool()))  // can the instruction fire beyond decode?
                                                         // (can still be stopped in ren or dis)
   val dec_rdy        = Wire(Bool())
   val rob_rdy        = Wire(Bool())
   val rob_empty      = Wire(Bool())
   val laq_full       = Wire(Bool())
   val stq_full       = Wire(Bool())


   // Register Rename State
   val ren_insts_can_proceed = Wire(Vec(DECODE_WIDTH, Bool()))

   // Dispatch State
   val dis_valid             = Wire(Bool()) // used to insert into ROB, IW
   val dis_insts_can_proceed = Wire(Vec(DISPATCH_WIDTH, Bool()))
   val dis_mask              = Wire(Vec(DISPATCH_WIDTH, Bool())) // true if uop WILL enter IW/ROB
   val dis_uops              = Wire(Vec(DISPATCH_WIDTH, new MicroOp()))


   // Issue State/Register Read/Execute State
   val issue_width           = exe_units.length
   val iss_valids            = Wire(Vec(issue_width, Bool()))
   val iss_uops              = Wire(Vec(issue_width, new MicroOp()))
   val register_width        = if (!usingFPU) xLen else 65
   val bypasses              = Wire(new BypassData(exe_units.num_total_bypass_ports, register_width))

   val br_unit = Wire(new BranchUnitResp())
   val brunit_idx = exe_units.br_unit_idx
   br_unit <> exe_units.br_unit_io


   // Memory State
   var lsu_io:LoadStoreUnitIO = null
   lsu_io = exe_units.memory_unit.io.lsu_io
   exe_units.memory_unit.io.dmem <> io.dmem


   // Writeback State


   // Commit Stage
   val com_valids            = Wire(Vec(DECODE_WIDTH, Bool()))
   val com_uops              = Wire(Vec(DECODE_WIDTH, new MicroOp()))
   val com_exception         = Wire(Bool()) // ROB or CSRFile is asserting an exception
   val com_exc_cause         = Wire(UInt())
   val com_exc_badvaddr      = Wire(UInt())
   val com_handling_exc      = Wire(Bool())
   val com_rbk_valids        = Wire(Vec(DECODE_WIDTH, Bool()))
   val com_fflags_val        = Wire(Bool())
   val com_fflags            = Wire(Bits())
   val lsu_misspec           = Wire(Bool())

   val csr_status            = Wire(new rocket.MStatus())
   val csr_interrupt         = Wire(Bool())
   val csr_interrupt_cause   = Wire(UInt())
   val watchdog_trigger      = Wire(Bool())


   //****************************************
   // Time Stamp Counter & Retired Instruction Counter
   // (only used for printf and vcd dumps - the actual counters are in the CSRFile)
   val tsc_reg  = Reg(init = UInt(0, xLen))
   val irt_reg  = Reg(init = UInt(0, xLen))
   val fseq_reg = Reg(init = UInt(0, xLen))
   tsc_reg  := tsc_reg + Mux(Bool(O3PIPEVIEW_PRINTF), UInt(O3_CYCLE_TIME), UInt(1))
   irt_reg  := irt_reg + PopCount(com_valids.toBits)
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

   val fetchbuffer_kill = Wire(new Bool())
   val FetchBuffer = Module(new Queue(gen=new FetchBundle,
                                entries=FETCH_BUFFER_SZ,
                                pipe=false,
                                flow=p(EnableFetchBufferFlowThrough),
                                _reset=(fetchbuffer_kill || reset.toBool)))
   val fetch = new BoomFetch(
    io                = io,
    br_unit           = br_unit,
    reset             = reset,
    flush_pipeline    = flush_pipeline,
    fseq_reg          = fseq_reg,
    tsc_reg           = tsc_reg,
    flush_take_pc     = flush_take_pc,
    flush_pc          = flush_pc,
    csr_take_pc       = csr_take_pc,
    csr_evec          = csr_evec,
    bp2_take_pc       = bp2_take_pc,
    bp2_is_jump       = bp2_is_jump,
    bp2_is_taken      = bp2_is_taken,
    bp2_pred_target   = bp2_pred_target,
    bp2_br_seen       = bp2_br_seen,
    bp2_pc_of_br_inst = bp2_pc_of_br_inst,
    com_exception     = com_exception,
    if_pc_next        = if_pc_next,
    com_valids        = com_valids,
    com_uops          = com_uops,
    FetchBuffer       = FetchBuffer)
   fetchbuffer_kill := fetch.fetchbuffer_kill
   assert (!(Reg(next=com_exception) && !flush_pipeline), "exception occurred, but pipeline flush signal not set!")


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Branch Prediction ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // These stages are effectively in parallel with instruction fetch and
   // decode.  BHT look-up is in parallel with I$ access, and Branch Decode
   // occurs before fetch buffer insertion.

   val bpd_stage = Module(new BranchPredictionStage(FETCH_WIDTH))
   bpd_stage.io.imem <> io.imem
   bpd_stage.io.ras_update <> io.imem.ras_update
   bpd_stage.io.br_unit := br_unit
   bpd_stage.io.kill := flush_take_pc
   bpd_stage.io.req.ready := !fetch.if_stalled

   bp2_take_pc := bpd_stage.io.req.valid
   bp2_is_taken := bpd_stage.io.req.bits.is_taken
   bp2_pred_target := bpd_stage.io.req.bits.target
   bp2_pc_of_br_inst := bpd_stage.io.req.bits.br_pc
   bp2_is_jump := bpd_stage.io.req.bits.is_jump
   bp2_br_seen := bpd_stage.io.pred_resp.br_seen

   fetch.bundle.mask := io.imem.resp.bits.mask & bpd_stage.io.pred_resp.mask
   fetch.bundle.pred_resp := bpd_stage.io.pred_resp
   fetch.bundle.predictions := bpd_stage.io.predictions

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

   val dec_serializer = Module(new FetchSerializerNtoM)
   dec_serializer.io.enq <> FetchBuffer.io.deq

   dec_serializer.io.kill := fetch.fetchbuffer_kill
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
      val decode_unit = Module(new DecodeUnit)
      dec_valids(w)                  := fetched_inst_valid && dec_fbundle.uops(w).valid && !dec_finished_mask(w)
      decode_unit.io.enq.uop         := dec_fbundle.uops(w)
      decode_unit.io.status          := csr_status
      decode_unit.io.interrupt       := csr_interrupt
      decode_unit.io.interrupt_cause := csr_interrupt_cause

      val prev_insts_in_bundle_valid = Range(0,w).map{i => dec_valids(i)}.foldLeft(Bool(false))(_|_)

      // stall this instruction?
      // TODO tailor this to only care if a given instruction uses a resource?
      val stall_me = (dec_valids(w) &&
                        (  !(ren_insts_can_proceed(w))
                        || (dec_valids(w) && dec_uops(w).is_unique && (!rob_empty || !lsu_io.lsu_fencei_rdy || prev_insts_in_bundle_valid))
                        || !rob_rdy
                        || laq_full
                        || stq_full
                        || branch_mask_full(w)
                        || br_unit.brinfo.mispredict
                        || flush_pipeline
                        || dec_stall_next_inst
                        || !bpd_stage.io.brob.allocate.ready
                        || (dec_valids(w) && dec_uops(w).is_fencei && !lsu_io.lsu_fencei_rdy)
                        )) ||
                     dec_last_inst_was_stalled

      // stall the next instruction following me in the decode bundle?
      dec_last_inst_was_stalled = stall_me
      dec_stall_next_inst  = stall_me || (dec_valids(w) && dec_uops(w).is_unique)

      dec_will_fire(w) := dec_valids(w) && !stall_me && !fetch.kill_frontend
      dec_uops(w)      := decode_unit.io.deq.uop


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

   when (dec_rdy || fetch.fetchbuffer_kill)
   {
      dec_finished_mask := Bits(0)
   }
   .otherwise
   {
      dec_finished_mask := dec_will_fire.toBits | dec_finished_mask
   }

   //-------------------------------------------------------------
   // Branch Mask Logic

   val dec_brmask_logic = Module(new BranchMaskGenerationLogic(DECODE_WIDTH))

   dec_brmask_logic.io.brinfo := br_unit.brinfo
   dec_brmask_logic.io.flush_pipeline := flush_pipeline

   for (w <- 0 until DECODE_WIDTH)
   {
      dec_brmask_logic.io.is_branch(w) := !dec_finished_mask(w) && dec_uops(w).allocate_brtag
      dec_brmask_logic.io.will_fire(w) :=  dis_mask(w) && dec_uops(w).allocate_brtag // ren, dis can back pressure us

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

   val rename_stage = Module(new RenameStage(DECODE_WIDTH, exe_units.num_wakeup_ports))

   rename_stage.io.dis_inst_can_proceed := dis_insts_can_proceed
   rename_stage.io.ren_pred_info := dec_fbundle.pred_resp

   rename_stage.io.kill     := fetch.kill_frontend // mispredict or flush
   rename_stage.io.brinfo   := br_unit.brinfo
   rename_stage.io.get_pred.br_tag        := iss_uops(brunit_idx).br_tag
   exe_units(brunit_idx).io.get_pred.info := Reg(next=rename_stage.io.get_pred.info)

   rename_stage.io.flush_pipeline := flush_pipeline

   for (w <- 0 until DECODE_WIDTH)
   {
      rename_stage.io.dec_mask(w) := dec_will_fire(w)
   }

   rename_stage.io.dec_uops := dec_uops
   ren_insts_can_proceed := rename_stage.io.inst_can_proceed

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


   rename_stage.io.com_valids := com_valids
   rename_stage.io.com_uops := com_uops
   rename_stage.io.com_rbk_valids := com_rbk_valids

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Dispatch Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // TODO get rid of, let the ROB handle this...?
   val dis_curr_rob_row_idx = Wire(UInt(width = ROB_ADDR_SZ))

   for (w <- 0 until DECODE_WIDTH)
   {
      dis_mask(w)         := rename_stage.io.ren_mask(w)
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
      dis_mask(w) && dis_uops(w).is_unique}.reduce(_|_)
   val dec_has_br_or_jalr_in_packet =
      Range(0,DECODE_WIDTH).map{w =>
         dec_valids(w) && dec_uops(w).br_prediction.is_br_or_jalr}.reduce(_|_) &&
      !dis_has_unique


   bpd_stage.io.brob.allocate.valid := dis_mask.reduce(_|_) &&
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

   val issue_unit = if (p(EnableAgePriorityIssue))
                        Module(new IssueUnitCollasping(p(NumIssueSlotEntries), issue_width, exe_units.num_wakeup_ports))
                    else
                        Module(new IssueUnitStatic(p(NumIssueSlotEntries), issue_width, exe_units.num_wakeup_ports))

   // Input (Dispatch)
   issue_unit.io.dis_mask  := dis_mask
   issue_unit.io.dis_uops  := dis_uops

   // Output (Issue)

   for (w <- 0 until issue_width)
   {
      iss_valids(w) := issue_unit.io.iss_valids(w)
      iss_uops(w)   := issue_unit.io.iss_uops(w)

      issue_unit.io.fu_types(w) := exe_units(w).io.fu_types
   }

   dis_insts_can_proceed := issue_unit.io.dis_readys



   issue_unit.io.brinfo := br_unit.brinfo
   issue_unit.io.flush_pipeline := flush_pipeline

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

   if (O3PIPEVIEW_PRINTF)
   {
      for (i <- 0 until ISSUE_WIDTH)
      {
         // only print stores once!
         when (iss_valids(i) && iss_uops(i).uopc =/= uopSTD)
         {
            printf("%d; O3PipeView:issue: %d\n",
               iss_uops(i).debug_events.fetch_seq,
               tsc_reg)
         }
      }
   }

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Register Read Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // Register Read <- Issue (rrd <- iss)

   val register_read = Module(new RegisterRead(issue_width,
                                               exe_units.map(_.supportedFuncUnits),
                                               exe_units.num_rf_read_ports,
                                               exe_units.map(_.num_rf_read_ports),
                                               exe_units.num_total_bypass_ports,
                                               register_width))

   for (w <- 0 until issue_width)
   {
      register_read.io.iss_valids(w) := iss_valids(w)
      register_read.io.iss_uops(w) := iss_uops(w)
   }

   register_read.io.brinfo := br_unit.brinfo
   register_read.io.kill   := flush_pipeline

   register_read.io.bypass := bypasses

   //-------------------------------------------------------------
   // Privileged Co-processor 0 Register File
   // Note: Normally this would be bad in that I'm writing state before
   // committing, so to get this to work I stall the entire pipeline for
   // CSR instructions so I never speculate these instructions.

   // if we catch a pipeline hang, let us puke out to the tohost register so we
   // can catch this in the hardware

   val csr = Module(new rocket.CSRFile()(p.alterPartial({case rocket.CoreName => "BOOM"})))


   require (exe_units(0).uses_csr_wport)

   // for critical path reasons, we aren't zero'ing this out if resp is not valid
   val csr_rw_cmd = exe_units(0).io.resp(0).bits.uop.ctrl.csr_cmd
   val wb_wdata = exe_units(0).io.resp(0).bits.data

   csr.io.host <> io.host
//   this isnt going to work, doesn't match up with getting data from csr file
   csr.io.rw.addr  := Mux(watchdog_trigger, UInt(rocket.CSRs.mtohost), exe_units(0).io.resp(0).bits.uop.csr_addr)
   csr.io.rw.cmd   := Mux(watchdog_trigger, rocket.CSR.W, csr_rw_cmd)
   csr.io.rw.wdata := Mux(watchdog_trigger, Bits(WATCHDOG_ERR_NO << 1 | 1),
                      Mux(com_exception,    com_exc_badvaddr,
                                            wb_wdata))

   assert (!(csr_rw_cmd =/= rocket.CSR.N && !exe_units(0).io.resp(0).valid), "CSRFile is being written to spuriously.")

   // Extra I/O
   csr.io.pc        := flush_pc
   csr.io.exception := com_exception && !csr.io.csr_xcpt
   csr.io.retire    := PopCount(com_valids.toBits)
   csr.io.cause     := com_exc_cause

   csr_take_pc      := csr.io.csr_xcpt || csr.io.eret
   csr_evec         := csr.io.evec
   csr_status       := csr.io.status
   csr_interrupt    := csr.io.interrupt
   csr_interrupt_cause := csr.io.interrupt_cause

   // reading requires serializing the entire pipeline
   csr.io.fcsr_flags.valid := com_fflags_val
   csr.io.fcsr_flags.bits := com_fflags

   exe_units.map(_.io.fcsr_rm := csr.io.fcsr_rm)

   // --------------------------------------
   // Register File

   val regfile = Module(new RegisterFile(PHYS_REG_COUNT
                                        , exe_units.num_rf_read_ports
                                        , exe_units.num_rf_write_ports
                                        , register_width
                                        , ENABLE_REGFILE_BYPASSING))


   // --------------------------------------
   // Read Ports

   regfile.io.read_ports <> register_read.io.rf_read_ports


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
      exe_units(w).io.com_handling_exc := com_handling_exc // TODO get rid of this?


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

   val com_st_mask = Wire(Vec(DECODE_WIDTH, Bool()))
   val com_ld_mask = Wire(Vec(DECODE_WIDTH, Bool()))

   // enqueue basic store info in Decode
   lsu_io.dec_uops := dec_uops

   for (w <- 0 until DECODE_WIDTH)
   {
      lsu_io.dec_st_vals(w) := dec_will_fire(w) && rename_stage.io.inst_can_proceed(w) && !com_exception && dec_uops(w).is_store
      lsu_io.dec_ld_vals(w) := dec_will_fire(w) && rename_stage.io.inst_can_proceed(w) && !com_exception && dec_uops(w).is_load

      lsu_io.dec_uops(w).rob_idx := dis_uops(w).rob_idx // for debug purposes (comit logging)
   }

   lsu_io.commit_store_mask := com_st_mask
   lsu_io.commit_load_mask  := com_ld_mask

   lsu_io.exception         := flush_pipeline || lsu_misspec //com_exception comes too early, will fight against a branch that resolves same cycle as an exception

   // Handle Branch Mispeculations
   lsu_io.brinfo      := br_unit.brinfo
   io.dmem.brinfo     := br_unit.brinfo


   laq_full    := lsu_io.laq_full
   stq_full    := lsu_io.stq_full
   new_ldq_idx := lsu_io.new_ldq_idx
   new_stq_idx := lsu_io.new_stq_idx

   lsu_io.debug_tsc := tsc_reg

   io.dmem.flush_pipe := flush_pipeline


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Writeback Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------


   var cnt = 0
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
            regfile.io.write_ports(cnt).wen  := exe_units(i).io.resp(j).valid &&
                                                exe_units(i).io.resp(j).bits.uop.ctrl.rf_wen && // TODO get rid of other checks
                                                (exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FIX || exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FLT)
            regfile.io.write_ports(cnt).addr := exe_units(i).io.resp(j).bits.uop.pdst
            regfile.io.write_ports(cnt).data := Mux(exe_units(i).io.resp(j).bits.uop.ctrl.csr_cmd =/= rocket.CSR.N, csr.io.rw.rdata,
                                                                                          exe_units(i).io.resp(j).bits.data)
         }
         else
         {
            regfile.io.write_ports(cnt).wen  := exe_units(i).io.resp(j).valid &&
                                                exe_units(i).io.resp(j).bits.uop.ctrl.rf_wen && // TODO get rid of other checks
                                                (exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FIX || exe_units(i).io.resp(j).bits.uop.dst_rtype === RT_FLT)
            regfile.io.write_ports(cnt).addr := exe_units(i).io.resp(j).bits.uop.pdst
            regfile.io.write_ports(cnt).data := exe_units(i).io.resp(j).bits.data
         }
         cnt += 1
      }
   }


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Commit Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // TODO bug, this can return too many fflag ports,e.g., the FPU is shared with the mem unit and thus has two wb ports
   val num_fpu_ports = exe_units.withFilter(_.hasFFlags).map(_.num_rf_write_ports).foldLeft(0)(_+_)
   val rob  = Module(new Rob(DECODE_WIDTH, NUM_ROB_ENTRIES, ISSUE_WIDTH, exe_units.num_slow_wakeup_ports, num_fpu_ports)) // TODO the ROB writeback is off the regfile, which is a different set

      // Dispatch
      rob_rdy := rob.io.ready

      rob.io.dis_uops := dis_uops
      rob.io.dis_mask := dis_mask
      rob.io.dis_has_br_or_jalr_in_packet := dec_has_br_or_jalr_in_packet
      rob.io.dis_partial_stall := !dec_rdy && !dis_mask(DECODE_WIDTH-1)
      rob.io.dis_new_packet := dec_finished_mask === Bits(0)
      rob.io.debug_tsc := tsc_reg

      dis_curr_rob_row_idx  := rob.io.curr_rob_tail

      rob_empty := rob.io.empty

      // Writeback
      cnt = 0
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
      rob.io.brinfo <> br_unit.brinfo.asInput()

      // branch unit requests PCs and predictions from ROB during register read
      // (fetch PC from ROB cycle earlier than needed for critical path reasons)
      rob.io.get_pc.rob_idx := iss_uops(brunit_idx).rob_idx
      exe_units(brunit_idx).io.get_rob_pc.curr_pc  := Reg(next=rob.io.get_pc.curr_pc)
      exe_units(brunit_idx).io.get_rob_pc.curr_brob_idx  := Reg(next=rob.io.get_pc.curr_brob_idx)
      exe_units(brunit_idx).io.get_rob_pc.next_val := Reg(next=rob.io.get_pc.next_val)
      exe_units(brunit_idx).io.get_rob_pc.next_pc  := Reg(next=rob.io.get_pc.next_pc)

      // LSU <> ROB
      lsu_misspec := rob.io.lsu_misspec
      rob.io.lsu_clr_bsy_valid   := lsu_io.lsu_clr_bsy_valid
      rob.io.lsu_clr_bsy_rob_idx := lsu_io.lsu_clr_bsy_rob_idx
      rob.io.lxcpt <> lsu_io.xcpt

      rob.io.cxcpt.valid := csr.io.csr_xcpt

      rob.io.bxcpt <> br_unit.xcpt.asInput()


      // Commit (ROB outputs)
      com_valids       := rob.io.com_valids
      com_uops         := rob.io.com_uops
      com_fflags_val   := rob.io.com_fflags_val
      com_fflags       := rob.io.com_fflags

      com_st_mask      := rob.io.com_st_mask
      com_ld_mask      := rob.io.com_ld_mask

      com_exception    := rob.io.com_exception // on for only a single cycle (to CSRFile)
      com_exc_cause    := rob.io.com_exc_cause
      com_exc_badvaddr := rob.io.com_badvaddr
      com_handling_exc := rob.io.com_handling_exc // on for duration of roll-back
      com_rbk_valids   := rob.io.com_rbk_valids

      bpd_stage.io.brob.deallocate <> rob.io.brob_deallocate
      bpd_stage.io.brob.bpd_update <> br_unit.bpd_update
      bpd_stage.io.brob.flush := flush_pipeline || rob.io.flush_brob

   //-------------------------------------------------------------
   // **** Flush Pipeline ****
   //-------------------------------------------------------------
   // flush on exceptions, miniexeptions, and after some special instructions

   flush_take_pc  := rob.io.flush_take_pc
   flush_pipeline := rob.io.flush_pipeline
   flush_pc       := rob.io.flush_pc

   for (w <- 0 until exe_units.length)
   {
      exe_units(w).io.req.bits.kill := flush_pipeline
   }

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Outputs to the External World ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   for (w <- 0 until DECODE_WIDTH)
   {
      debug(com_uops(w).inst)
      debug(com_valids(w))
   }
   debug(br_unit.brinfo.valid)
   debug(br_unit.brinfo.mispredict)

   // detect pipeline freezes and throw error
   val idle_cycles = rocket.WideCounter(32)
   when (com_valids.toBits.orR || reset.toBool) { idle_cycles := UInt(0) }
   watchdog_trigger := Reg(next=idle_cycles.value(30))
   assert (!(idle_cycles.value(13)), "Pipeline has hung.")


   //-------------------------------------------------------------
   // UARCH Counters

   val laq_full_count = Reg(init = UInt(0, xLen))
   when (laq_full) { laq_full_count := laq_full_count + UInt(1) }
   debug(laq_full_count)

   val stq_full_count = Reg(init = UInt(0, xLen))
   when (stq_full) { stq_full_count := stq_full_count + UInt(1) }
   debug(stq_full_count)

   val stalls = Reg(init = UInt(0, xLen))
   when (!dec_rdy) { stalls := stalls + UInt(1) }
   debug(stalls)

   val lsu_misspec_count = Reg(init = UInt(0, xLen))
   when (lsu_misspec) { lsu_misspec_count := lsu_misspec_count + UInt(1) }
   debug(lsu_misspec_count)

   // these take up a significant amount of area, so don't enable them lightly
   if (p(EnableUarchCounters))
   {
      println("\n   UArch Counters Enabled\n")
      csr.io.uarch_counters(0)  := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
         com_valids(w) && com_uops(w).is_br_or_jmp && !com_uops(w).is_jal})
      csr.io.uarch_counters(1)  := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
         com_valids(w) && com_uops(w).is_br_or_jmp && !com_uops(w).is_jal && com_uops(w).stat_brjmp_mispredicted})
      csr.io.uarch_counters(2)  := !rob_rdy
      csr.io.uarch_counters(3)  := laq_full
      csr.io.uarch_counters(4)  := !issue_unit.io.dis_readys.reduce(_|_)
      csr.io.uarch_counters(5)  := io.counters.dc_miss
      csr.io.uarch_counters(6)  := branch_mask_full.reduce(_|_)
      csr.io.uarch_counters(7)  := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
         com_valids(w) && (com_uops(w).is_store || com_uops(w).is_load)})
      csr.io.uarch_counters(8)  := lsu_io.counters.ld_valid
      csr.io.uarch_counters(9)  := lsu_io.counters.ld_order_fail

      csr.io.uarch_counters(10) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
         com_valids(w) && com_uops(w).is_br_or_jmp && !com_uops(w).is_jal && com_uops(w).stat_btb_made_pred})
      csr.io.uarch_counters(11) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
         com_valids(w) && com_uops(w).is_br_or_jmp && !com_uops(w).is_jal && com_uops(w).stat_btb_mispredicted})
      csr.io.uarch_counters(12) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
         com_valids(w) && com_uops(w).is_br_or_jmp && !com_uops(w).is_jal && com_uops(w).stat_bpd_made_pred})
      csr.io.uarch_counters(13) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
         com_valids(w) && com_uops(w).is_br_or_jmp && !com_uops(w).is_jal && com_uops(w).stat_bpd_mispredicted})

      // 14, no prediction made
      csr.io.uarch_counters(14) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
         com_valids(w) && com_uops(w).is_br_or_jmp && !com_uops(w).is_jal &&
         !com_uops(w).stat_btb_made_pred && !com_uops(w).stat_bpd_made_pred})
      // 15, no predition made - and a mispredict occurred
      csr.io.uarch_counters(15) := PopCount((Range(0,COMMIT_WIDTH)).map{w =>
         com_valids(w) && com_uops(w).is_br_or_jmp && !com_uops(w).is_jal &&
         !com_uops(w).stat_btb_made_pred && !com_uops(w).stat_bpd_made_pred &&
         com_uops(w).stat_brjmp_mispredicted})
   }
   else
   {
      println("\n   UArch Counters Disabled\n")
      csr.io.uarch_counters.foreach(_ := Bool(false))
   }

   assert (!(Range(0,COMMIT_WIDTH).map{w =>
      com_valids(w) && com_uops(w).is_br_or_jmp && com_uops(w).is_jal &&
      com_uops(w).stat_brjmp_mispredicted}.reduce(_|_)),
      "[dpath] A committed JAL was marked as having been mispredicted.")

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Handle Cycle-by-Cycle Printouts ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   if (DEBUG_PRINTF)
   {
      println("\n Chisel Printout Enabled\n")

      var whitespace = (63 - 3 - 12 - NUM_LSU_ENTRIES- p(NumIssueSlotEntries) - (NUM_ROB_ENTRIES/COMMIT_WIDTH)
         - io.dmem.debug.ld_req_slot.size - NUM_BROB_ENTRIES)

      def InstsStr(insts: Bits, width: Int) =
      {
         var string = sprintf("")
         for (w <- 0 until width)
            string = sprintf("%s(DASM(%x))", string, insts(((w+1)*32)-1,w*32))
         string
      }

      // Front-end
      printf("--- Cyc=%d , ----------------- Ret: %d ----------------------------------\n  BrPred1:        (IF1_PC= n/a- Predict:n/a) ------ PC: [%s%s%s-%s for br_id: %d, %s %s next: 0x%x ifst:%d]\nI$ Response: (%s) IF2_PC= 0x%x (mask:0x%x) \u001b[1;35m%s\u001b[0m  ----BrPred2:(%s,%s,%d) [btbtarg: 0x%x] jkilmsk:0x%x ->(0x%x)\n"
         , tsc_reg
         , irt_reg & UInt(0xffffff)
      // Fetch Stage 1
         , Mux(br_unit.brinfo.valid, Str("V"), Str("-"))
         , Mux(br_unit.brinfo.taken, Str("T"), Str("-"))
         , Mux(br_unit.debug_btb_pred, Str("B"), Str("_"))
         , Mux(br_unit.brinfo.mispredict, Str(b_mgt + "MISPREDICT" + end), Str(grn + "          " + end))
         , bpd_stage.io.req.bits.idx
         , Mux(fetch.take_pc, Str("TAKE_PC"), Str(" "))
         , Mux(flush_take_pc, Str("FLSH"),
           Mux(br_unit.take_pc, Str("BRU "),
           Mux(bp2_take_pc && !fetch.if_stalled, Str("BP2"),
           Mux(bp2_take_pc, Str("J-s"),
                              Str(" ")))))
         , if_pc_next
         , fetch.if_stalled
      // Fetch Stage 2
         , Mux(io.imem.resp.valid && !fetch.fetchbuffer_kill, Str(mgt + "v" + end), Str(grn + "-" + end))
         , io.imem.resp.bits.pc
         , io.imem.resp.bits.mask
         , InstsStr(io.imem.resp.bits.data.toBits, FETCH_WIDTH)
         , Mux(io.imem.btb_resp.valid, Str("H"), Str("-"))
         , Mux(io.imem.btb_resp.bits.taken, Str("T"), Str("-"))
         , io.imem.btb_resp.bits.bridx
         , io.imem.btb_resp.bits.target(19,0)
         , bpd_stage.io.pred_resp.mask
         , fetch.bundle.mask
         )

      // Back-end
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
         printf(") State: (%s: %s %s %s \u001b[1;31m%s\u001b[0m %s %s) BMsk:%x Mode:%s %s\n"
         , Mux(rob.io.debug.state === UInt(0), Str("RESET"),
           Mux(rob.io.debug.state === UInt(1), Str("NORMAL"),
           Mux(rob.io.debug.state === UInt(2), Str("ROLLBK"),
           Mux(rob.io.debug.state === UInt(3), Str("WAIT_E"),
                                               Str(" ")))))
         , Mux(rob_rdy,Str("_"), Str("!ROB_RDY"))
         , Mux(laq_full, Str("LAQ_FULL"), Str("_"))
         , Mux(stq_full, Str("STQ_FULL"), Str("_"))
         , Mux(flush_pipeline, Str("FLUSH_PIPELINE"), Str(" "))
         , Mux(branch_mask_full.reduce(_|_), Str("BR_MSK_FULL"), Str(" "))
         , Mux(io.dmem.req.ready, Str("D$_Rdy"), Str("D$_BSY"))
         , dec_brmask_logic.io.debug.branch_mask
         , Mux(csr.io.status.prv === Bits(0x3), Str("M"),
           Mux(csr.io.status.prv === Bits(0x0), Str("U"),
           Mux(csr.io.status.prv === Bits(0x1), Str("S"),  //2 is H
                                                 Str("?"))))
         , Mux(csr.io.status.ie, Str("EI"), Str("-"))
         )
      }


      for (w <- 0 until DECODE_WIDTH)
      {
         printf("(%s%s) " + red + "DASM(%x)" + end + " |  "
            , Mux(fetched_inst_valid && dec_fbundle.uops(w).valid && !dec_finished_mask(w), Str("v"), Str("-"))
            , Mux(dec_will_fire(w), Str("V"), Str("-"))
            , dec_fbundle.uops(w).inst
            )
      }

      printf(")\n   fin(%x)", dec_finished_mask)

      for (w <- 0 until DECODE_WIDTH)
      {
         printf("  [ISA:%d,%d,%d,%d] [Phs:%d(%s)%d[%s](%s)%d[%s](%s)%d[%s](%s)] "
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



      printf("Exct(%s%d) Commit(%x) fl: 0x%x (%d) is: 0x%x (%d)\n"
         , Mux(com_exception, Str("E"), Str("-"))
         , com_exc_cause
         , com_valids.toBits
         , rename_stage.io.debug.freelist
         , PopCount(rename_stage.io.debug.freelist)
         , rename_stage.io.debug.isprlist
         , PopCount(rename_stage.io.debug.isprlist)
         )

      // branch unit
      printf("                          Branch Unit: %s,%s,%d PC=0x%x, %d Targ=0x%x NPC=%d,0x%x %d%d\n"
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

      printf("  Mem[%s l%d](%s:%d),%s,%s %s %s %s]\n"
            , Mux(io.dmem.debug.memreq_val, Str("MREQ"), Str(" "))
            , io.dmem.debug.memreq_lidx
            , Mux(io.dmem.debug.memresp_val, Str("MRESP"), Str(" "))
            , io.dmem.debug.cache_resp_tag
            , Mux(io.dmem.debug.req_kill, Str("RKILL"), Str(" "))
            , Mux(io.dmem.debug.cache_not_ready, Str("CBUSY"), Str(" "))
            , Mux(io.dmem.debug.nack, Str("NACK"), Str(" "))
            , Mux(io.dmem.debug.cache_nack, Str("CN"), Str(" "))
            , Mux(lsu_io.forward_val, Str("FWD"), Str(" "))
            //, Mux(lsu_io.debug.tlb_miss, Str("TLB-MISS"), Str("-"))
            //, Mux(lsu_io.debug.tlb_ready, Str("TLB-RDY"), Str("-"))
      )

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

         when (com_valids(w))
         {
            when (com_uops(w).dst_rtype === RT_FIX && com_uops(w).ldst =/= UInt(0))
            {
               printf("%d 0x%x (0x%x) x%d 0x%x\n", priv, Sext(com_uops(w).pc(vaddrBits,0), xLen), com_uops(w).inst, com_uops(w).inst(RD_MSB,RD_LSB), com_uops(w).debug_wdata)
            }
            .elsewhen (com_uops(w).dst_rtype === RT_FLT)
            {
               printf("%d 0x%x (0x%x) f%d 0x%x\n", priv, Sext(com_uops(w).pc(vaddrBits,0), xLen), com_uops(w).inst, com_uops(w).inst(RD_MSB,RD_LSB), com_uops(w).debug_wdata)
            }
            .otherwise
            {
               printf("%d 0x%x (0x%x)\n", priv, Sext(com_uops(w).pc(vaddrBits,0), xLen), com_uops(w).inst)
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
         when (com_valids(i))
         {
            printf("%d; O3PipeView:retire:%d:store: 0\n",
               com_uops(i).debug_events.fetch_seq,
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
   io.dmem.invalidate_lr := com_exception

   //-------------------------------------------------------------
   //-------------------------------------------------------------


}

}
