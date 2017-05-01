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
import config.Parameters

import rocket.Instructions._
import util.Str


abstract class BoomModule(implicit p: Parameters) extends tile.CoreModule()(p)
  with HasBoomCoreParameters

class BoomBundle(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasBoomCoreParameters

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------


class BoomCore(implicit p: Parameters, edge: uncore.tilelink2.TLEdgeOut) extends BoomModule()(p)
   with tile.HasCoreIO
{
   //**********************************
   // construct all of the modules

   // Only holds integer-registerfile execution units.
   val exe_units        = new boom.ExecutionUnits(fpu=false)
   // Meanwhile, the FP pipeline holds the FP issue window, FP regfile, and FP arithmetic units.
   var fp_pipeline: FpPipeline = null
   if (usingFPU) {
      fp_pipeline       = Module(new FpPipeline())
   }

   val num_irf_write_ports = exe_units.map(_.num_rf_write_ports).sum
   val num_fast_wakeup_ports = exe_units.count(_.isBypassable)
   val num_wakeup_ports = num_irf_write_ports + num_fast_wakeup_ports
   val fetch_unit       = Module(new FetchUnit(FETCH_WIDTH))
   val bpd_stage        = Module(new BranchPredictionStage(FETCH_WIDTH))
   val dec_serializer   = Module(new FetchSerializerNtoM)
   val decode_units     = for (w <- 0 until DECODE_WIDTH) yield { val d = Module(new DecodeUnit); d }
   val dec_brmask_logic = Module(new BranchMaskGenerationLogic(DECODE_WIDTH))
   val rename_stage     = Module(new RenameStage(DECODE_WIDTH, num_wakeup_ports, fp_pipeline.io.wakeups.length))
   val issue_units      = new boom.IssueUnits(num_wakeup_ports)
   val iregfile         = Module(new RegisterFile(numIntPhysRegs,
                                 exe_units.withFilter(_.usesIRF).map(e => e.num_rf_read_ports).sum,
                                 exe_units.withFilter(_.usesIRF).map(e => e.num_rf_write_ports).sum,
                                 xLen,
                                 ENABLE_REGFILE_BYPASSING))
//   val ll_wbarb         = Module(new Arbiter(new RegisterFileWritePort(IPREG_SZ, xLen), 2))
   val ll_wbarb         = Module(new Arbiter(new ExeUnitResp(xLen), 2))
   val iregister_read   = Module(new RegisterRead(
                                 issue_units.map(_.issue_width).sum,
                                 exe_units.withFilter(_.usesIRF).map(_.supportedFuncUnits),
                                 exe_units.withFilter(_.usesIRF).map(_.num_rf_read_ports).sum,
                                 exe_units.withFilter(_.usesIRF).map(_.num_rf_read_ports),
                                 exe_units.num_total_bypass_ports,
                                 xLen))
   val csr              = Module(new rocket.CSRFile())
   val dc_shim          = Module(new DCacheShim())
   val lsu              = Module(new LoadStoreUnit(DECODE_WIDTH))
   val rob              = Module(new Rob(
                                 DECODE_WIDTH,
                                 NUM_ROB_ENTRIES,
                                 num_irf_write_ports + fp_pipeline.io.wakeups.length,
                                 exe_units.num_fpu_ports + fp_pipeline.io.wakeups.length))
   // Used to wakeup registers in rename and issue. ROB needs to listen to something else.
   val int_wakeups      = Wire(Vec(num_wakeup_ports, Valid(new ExeUnitResp(xLen))))

   require (exe_units.length == issue_units.map(_.issue_width).sum)

   //***********************************
   // Pipeline State Registers and Wires

   // Instruction Decode Stage
   val dec_valids     = Wire(Vec(DECODE_WIDTH, Bool()))  // are the decoded instruction valid? It may be held up though.
   val dec_uops       = Wire(Vec(DECODE_WIDTH, new MicroOp()))
   val dec_will_fire  = Wire(Vec(DECODE_WIDTH, Bool()))  // can the instruction fire beyond decode?
                                                         // (can still be stopped in ren or dis)
   val dec_rdy        = Wire(Bool())

   // Dispatch Stage
   val dis_valids     = Wire(Vec(DISPATCH_WIDTH, Bool())) // true if uop WILL enter IW/ROB
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


   //****************************************
   // Time Stamp Counter & Retired Instruction Counter
   // (only used for printf and vcd dumps - the actual counters are in the CSRFile)
   val debug_tsc_reg  = Reg(init = UInt(0, xLen))
   val debug_irt_reg  = Reg(init = UInt(0, xLen))
   debug_tsc_reg  := debug_tsc_reg + Mux(Bool(O3PIPEVIEW_PRINTF), UInt(O3_CYCLE_TIME), UInt(1))
   debug_irt_reg  := debug_irt_reg + PopCount(rob.io.commit.valids.toBits)
   debug(debug_tsc_reg)
   debug(debug_irt_reg)


   //****************************************
   // Print-out information about the machine

   if (usingFPU)         println ("\n    FPU Unit Enabled")
   else                  println ("\n    FPU Unit Disabled")
   if (usingVM)          println ("    VM       Enabled")
   else                  println ("    VM       Disabled")
   if (usingFDivSqrt)    println ("    FDivSqrt Enabled\n")
   else                  println ("    FDivSqrt Disabled\n")

   val iss_str = if (enableAgePriorityIssue) " (Age-based Priority)"
                 else " (Unordered Priority)"
   println("\n   Fetch Width           : " + FETCH_WIDTH)
   println("   Issue Width           : " + issueParams.map(_.issueWidth).sum)
   println("   ROB Size              : " + NUM_ROB_ENTRIES)
   println("   Issue Window Size     : " + issueParams.map(_.numEntries) + iss_str)
   println("   Load/Store Unit Size  : " + NUM_LSU_ENTRIES + "/" + NUM_LSU_ENTRIES)
   println("   Num Int Phys Registers: " + numIntPhysRegs)
   println("   Num FP  Phys Registers: " + numFpPhysRegs)
   println("   Max Branch Count      : " + MAX_BR_COUNT)
   println("   BTB Size              : " + btbParams.nEntries)
   println("   RAS Size              : " + btbParams.nRAS)

   print(iregfile)
   println("\n   Num Slow Wakeup Ports : " + num_irf_write_ports)
   println("   Num Fast Wakeup Ports : " + exe_units.count(_.isBypassable))
   println("   Num Bypass Ports      : " + exe_units.num_total_bypass_ports)

   print(fp_pipeline)

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
   fetch_unit.io.tsc_reg           := debug_tsc_reg

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
   bpd_stage.io.flush := rob.io.flush.valid //|| rob.io.clear_brob
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
                           (!rob.io.empty || !lsu.io.lsu_fencei_rdy || prev_insts_in_bundle_valid))
                        || !rob.io.ready
                        || lsu.io.laq_full
                        || lsu.io.stq_full
                        || branch_mask_full(w)
                        || br_unit.brinfo.mispredict
                        || rob.io.flush.valid
                        || dec_stall_next_inst
                        || !bpd_stage.io.brob.allocate.ready
                        || (dec_valids(w) && dec_uops(w).is_fencei && !lsu.io.lsu_fencei_rdy)
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
            printf("%d; O3PipeView:decode:%d\n", fetch_seq, debug_tsc_reg)
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

   // TODO for now, assume worst-case all instructions will dispatch towards one issue unit.
   val dis_readys = issue_units.map(_.io.dis_readys.toBits).reduce(_&_) & fp_pipeline.io.dis_readys.toBits
   rename_stage.io.dis_inst_can_proceed := dis_readys.toBools
   rename_stage.io.ren_pred_info := dec_fbundle.pred_resp

   rename_stage.io.kill     := fetch_unit.io.clear_fetchbuffer // mispredict or flush
   rename_stage.io.brinfo   := br_unit.brinfo
   rename_stage.io.get_pred.br_tag        := iss_uops(brunit_idx).br_tag
   exe_units(brunit_idx).io.get_pred.info := Reg(next=rename_stage.io.get_pred.info)

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

   // TODO get rid of, let the ROB handle this...?
   val dis_curr_rob_row_idx = Wire(UInt(width = ROB_ADDR_SZ))

   for (w <- 0 until DECODE_WIDTH)
   {
      dis_valids(w)       := rename_stage.io.ren_mask(w)
      dis_uops(w)         := rename_stage.io.ren_uops(w)
      // TODO probably don't need to do this, since we're going to do it in the issue window?
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

   // Output (Issue)

   val ifpu_idx = exe_units.length-1 // TODO hack; need more disciplined manner to hook up ifpu
   require (exe_units(ifpu_idx).supportedFuncUnits.ifpu)

   var iss_idx = 0
   var iss_cnt = 0
   for (w <- 0 until exe_units.length)
   {
      iss_valids(w) := issue_units(iss_idx).io.iss_valids(iss_cnt)
      iss_uops(w)   := issue_units(iss_idx).io.iss_uops(iss_cnt)
      issue_units(iss_idx).io.fu_types(iss_cnt) := exe_units(w).io.fu_types

      if (w == ifpu_idx) {
         // TODO hack, need a more disciplined way to connect to an issue port
         // TODO XXX need to also apply back-pressure.
         issue_units(iss_idx).io.fu_types(iss_cnt) := exe_units(w).io.fu_types | FUConstants.FU_I2F
      }

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

   for (iu <- issue_units)
   {
      for ((issport, wakeup) <- iu.io.wakeup_pdsts zip int_wakeups)
      {
         issport.valid := wakeup.valid
         issport.bits  := wakeup.bits.uop.pdst
      }
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
   csr.io.rw.cmd   := Mux(csr_exe_unit.io.resp(0).valid, csr_rw_cmd, rocket.CSR.N)
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

   fp_pipeline.io.brinfo := br_unit.brinfo


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Load/Store Unit ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // enqueue basic load/store info in Decode
   lsu.io.dec_uops := dec_uops

   for (w <- 0 until DECODE_WIDTH)
   {
      lsu.io.dec_st_vals(w) := dec_will_fire(w) && rename_stage.io.inst_can_proceed(w) && !rob.io.flush.valid &&
                               dec_uops(w).is_store
      lsu.io.dec_ld_vals(w) := dec_will_fire(w) && rename_stage.io.inst_can_proceed(w) && !rob.io.flush.valid &&
                               dec_uops(w).is_load

      lsu.io.dec_uops(w).rob_idx := dis_uops(w).rob_idx // for debug purposes (comit logging)
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

   // TODO refactor lsu/mem connection
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
         val wbReadsCSR = wbresp.bits.uop.ctrl.csr_cmd =/= rocket.CSR.N

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
   rob.io.dis_uops := dis_uops
   rob.io.dis_valids := dis_valids
   rob.io.dis_has_br_or_jalr_in_packet := dec_has_br_or_jalr_in_packet
   rob.io.dis_partial_stall := !dec_rdy && !dis_valids(DECODE_WIDTH-1)
   rob.io.dis_new_packet := dec_finished_mask === Bits(0)
   rob.io.debug_tsc := debug_tsc_reg

   dis_curr_rob_row_idx  := rob.io.curr_rob_tail

   // Writeback
   var cnt = 0
   var f_cnt = 0 // rob fflags port index
   for (eu <- exe_units)
   {
      for ((resp, j) <- eu.io.resp.zipWithIndex)
      {
         val wb_uop = resp.bits.uop

         if (eu.is_mem_unit)
         {
            val ll_uop = ll_wbarb.io.out.bits.uop
            rob.io.wb_resps(cnt).valid := ll_wbarb.io.out.valid && !(ll_uop.is_store && !ll_uop.is_amo)
            rob.io.wb_resps(cnt).bits <> ll_wbarb.io.out.bits
         }
         else
         {
            rob.io.wb_resps(cnt).valid := resp.valid && !(wb_uop.is_store && !wb_uop.is_amo)
            rob.io.wb_resps(cnt).bits <> resp.bits
         }

         // for commit logging...
         rob.io.debug_wb_valids(cnt) := resp.valid &&
                                        wb_uop.ctrl.rf_wen &&
                                        (wb_uop.dst_rtype === RT_FIX || wb_uop.dst_rtype === RT_FLT)

         val data = resp.bits.data
         if (eu.hasFFlags || (eu.is_mem_unit && usingFPU))
         {
            if (eu.hasFFlags)
            {
               rob.io.fflags(f_cnt) <> resp.bits.fflags
               f_cnt += 1
            }
            val unrec_s = hardfloat.fNFromRecFN(8, 24, data)
            val unrec_d = hardfloat.fNFromRecFN(11, 53, data)
            val unrec_out     = Mux(wb_uop.fp_single, Cat(UInt(0,32), unrec_s), unrec_d)
            if (eu.uses_csr_wport && (j == 0))
            {
               rob.io.debug_wb_wdata(cnt) := Mux(wb_uop.ctrl.csr_cmd =/= rocket.CSR.N, csr.io.rw.rdata,
                                             Mux(wb_uop.fp_val && wb_uop.dst_rtype === RT_FLT, unrec_out,
                                                                                               data))
            }
            else
            {
               rob.io.debug_wb_wdata(cnt) := Mux(resp.bits.uop.fp_val, unrec_out, data)
            }
         }
         else
         {
            if (eu.uses_csr_wport && (j == 0))
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

   for (wakeup <- fp_pipeline.io.wakeups)
   {
      rob.io.wb_resps(cnt) <> wakeup
      rob.io.fflags(f_cnt) <> wakeup.bits.fflags
      cnt += 1
      f_cnt += 1
   }
//   rob.io.wb_resps(cnt).valid := ll_wbarb.io.out.fire()
//   rob.io.wb_resps(cnt).bits  := ll_wbarb.io.out.bits
//   cnt += 1
   assert (cnt == rob.num_wakeup_ports)


   // branch resolution
   rob.io.brinfo <> br_unit.brinfo

   // branch unit requests PCs and predictions from ROB during register read
   // (fetch PC from ROB cycle earlier than needed for critical path reasons)
   rob.io.get_pc.rob_idx := iss_uops(brunit_idx).rob_idx
   exe_units(brunit_idx).io.get_rob_pc.curr_pc  := Reg(next=rob.io.get_pc.curr_pc)
   exe_units(brunit_idx).io.get_rob_pc.curr_brob_idx  := Reg(next=rob.io.get_pc.curr_brob_idx)
   exe_units(brunit_idx).io.get_rob_pc.next_val := Reg(next=rob.io.get_pc.next_val)
   exe_units(brunit_idx).io.get_rob_pc.next_pc  := Reg(next=rob.io.get_pc.next_pc)
   exe_units(brunit_idx).io.status := csr.io.status

   // LSU <> ROB
   rob.io.lsu_clr_bsy_valid   := lsu.io.lsu_clr_bsy_valid
   rob.io.lsu_clr_bsy_rob_idx := lsu.io.lsu_clr_bsy_rob_idx
   rob.io.lxcpt <> lsu.io.xcpt

   rob.io.cxcpt.valid := csr.io.csr_xcpt
   rob.io.csr_eret := csr.io.eret
   rob.io.csr_evec := csr.io.evec

   rob.io.bxcpt <> br_unit.xcpt

   bpd_stage.io.brob.deallocate <> rob.io.brob_deallocate
   bpd_stage.io.brob.bpd_update <> br_unit.bpd_update
   bpd_stage.io.brob.flush := rob.io.flush.valid || rob.io.clear_brob

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
   val idle_cycles = util.WideCounter(32)
   when (rob.io.commit.valids.toBits.orR || reset.toBool) { idle_cycles := UInt(0) }
   assert (!(idle_cycles.value(13)), "Pipeline has hung.")

   fp_pipeline.io.debug_tsc_reg := debug_tsc_reg

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
   // TODO add back in cache-miss counters.
//   csr.io.events(3) := io.dmem.acquire // D$ miss
//   csr.io.events(4) := io.imem.acquire // I$ miss

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
   csr.io.events(13) := lsu.io.laq_full
   csr.io.events(14) := lsu.io.stq_full
//   csr.io.events(15) := !issue_unit.io.dis_readys.reduce(_|_) TODO
   csr.io.events(16) := branch_mask_full.reduce(_|_)
   csr.io.events(17) := rob.io.flush.valid

   // LSU Speculation stats.
   csr.io.events(18) := lsu.io.counters.ld_valid
   csr.io.events(19) := lsu.io.counters.stld_order_fail
   csr.io.events(20) := lsu.io.counters.ldld_order_fail

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

      val numBrobWhitespace = if (DEBUG_PRINTF_BROB) NUM_BROB_ENTRIES else 0
//      val screenheight = 104-8
      val screenheight = 84 -7
       var whitespace = (screenheight - 10 + 4 - NUM_LSU_ENTRIES -
         issueParams.map(_.numEntries).sum - issueParams.length - (NUM_ROB_ENTRIES/COMMIT_WIDTH) - numBrobWhitespace
     )

      println("Whitespace padded: " + whitespace)

      printf("--- Cyc=%d , ----------------- Ret: %d ----------------------------------\n  "
         , debug_tsc_reg
         , debug_irt_reg & UInt(0xffffff))

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
         printf(") ctate: (%c: %c %c %c %c %c %c) BMsk:%x Mode:%c\n"
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
         , rename_stage.io.debug.ifreelist
         , PopCount(rename_stage.io.debug.ifreelist)
         , rename_stage.io.debug.iisprlist
         , PopCount(rename_stage.io.debug.iisprlist)
         )

      printf("                                    fl: 0x%x (%d) is: 0x%x (%d)\n"
         , rename_stage.io.debug.ffreelist
         , PopCount(rename_stage.io.debug.ffreelist)
         , rename_stage.io.debug.fisprlist
         , PopCount(rename_stage.io.debug.fisprlist)
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
   io.ptw.invalidate := csr.io.fatc
   io.ptw.status     := csr.io.status

   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // we do not support RoCC (yet)
   io.rocc.cmd.valid := Bool(false)
   io.rocc.resp.ready := Bool(false)
}

