//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Datapath: Rename Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2012 Feb 14


package boom

import Chisel._
import config.Parameters


class RenameStageIO(
   pl_width: Int,
   num_int_pregs: Int,
   num_fp_pregs: Int,
   num_int_wb_ports: Int,
   num_fp_wb_ports: Int)
   (implicit p: Parameters) extends BoomBundle()(p)
{
   private val int_preg_sz = log2Up(num_int_pregs)
   private val fp_preg_sz = log2Up(num_fp_pregs)

   val ren_mask  = Vec(pl_width, Bool().asOutput) // mask of valid instructions
   val inst_can_proceed = Vec(pl_width, Bool()).asOutput

   val kill      = Bool(INPUT)

   val dec_mask  = Vec(pl_width, Bool()).asInput

   val dec_uops  = Vec(pl_width, new MicroOp()).asInput
   val ren_uops  = Vec(pl_width, new MicroOp().asOutput)

   val ren_pred_info = new BranchPredictionResp().asInput

   // branch resolution (execute)
   val brinfo    = new BrResolutionInfo().asInput
   val get_pred  = new GetPredictionInfo().flip

   val dis_inst_can_proceed = Vec(DISPATCH_WIDTH, Bool()).asInput

   // issue stage (fast wakeup)
   val int_wb_valids = Vec(num_int_wb_ports, Bool()).asInput
   val int_wb_pdsts  = Vec(num_int_wb_ports, UInt(width=int_preg_sz)).asInput
   val fp_wb_valids = Vec(num_fp_wb_ports, Bool()).asInput
   val fp_wb_pdsts  = Vec(num_fp_wb_ports, UInt(width=fp_reg_sz)).asInput

   // commit stage
   val com_valids = Vec(pl_width, Bool()).asInput
   val com_uops   = Vec(pl_width, new MicroOp()).asInput
   val com_rbk_valids = Vec(pl_width, Bool()).asInput

   val flush_pipeline = Bool(INPUT) // TODO only used for SCR (single-cycle reset)

   val debug_rob_empty = Bool(INPUT)
   val debug = new DebugRenameStageIO(num_int_pregs).asOutput
}


class DebugRenameStageIO(num_pregs: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val ifreelist = Bits(width=num_pregs)
   val iisprlist = Bits(width=num_pregs)
   val ibusytable = UInt(width=num_pregs)
   override def cloneType: this.type = new DebugRenameStageIO(num_pregs).asInstanceOf[this.type]
}


class RenameStage(
   pl_width: Int,
   num_int_wb_ports: Int,
   num_fp_wb_ports: Int)
(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new RenameStageIO(pl_width, numIntPhysRegs, numFpPhysRegs, num_int_wb_ports, num_fp_wb_ports)

   // integer registers
   val imaptable = Module(new RenameMapTable(
      pl_width,
      RT_FIX.litValue,
      32,
      numIntPhysRegs))
   val ifreelist = Module(new RenameFreeList(
      pl_width,
      RT_FIX.litValue,
      numIntPhysRegs))
   val ibusytable = Module(new BusyTable(
      pl_width,
      RT_FIX.litValue,
      num_pregs = numIntPhysRegs,
      num_read_ports = pl_width*2,
      num_wb_ports = num_int_wb_ports))

   // floating point registers
   val fmaptable = Module(new RenameMapTable(
      pl_width,
      RT_FLT.litValue,
      32,
      numFpPhysRegs))
   val ffreelist = Module(new RenameFreeList(
      pl_width,
      RT_FLT.litValue,
      numFpPhysRegs))
   val fbusytable = Module(new BusyTable(
      pl_width,
      RT_FLT.litValue,
      num_pregs = numFpPhysRegs,
      num_read_ports = pl_width*3,
      num_wb_ports = num_fp_wb_ports))

   //-------------------------------------------------------------

   val ren_br_vals = Wire(Vec(pl_width, Bool()))
   for (w <- 0 until pl_width)
   {
      io.ren_mask(w)         := io.dec_mask(w) && io.inst_can_proceed(w) && !io.kill
      io.ren_uops(w)         := io.dec_uops(w)
      io.ren_uops(w).br_mask := GetNewBrMask(io.brinfo, io.dec_uops(w))
      ren_br_vals(w)         := io.dec_mask(w) && io.dec_uops(w).allocate_brtag
   }

   //-------------------------------------------------------------
   // Rename Table

   imaptable.io.brinfo := io.brinfo
   imaptable.io.kill := io.kill
   imaptable.io.ren_mask := io.ren_mask
   imaptable.io.ren_uops := io.ren_uops
   imaptable.io.ren_br_vals := ren_br_vals
   imaptable.io.com_valids := io.com_valids
   imaptable.io.com_uops := io.com_uops
   imaptable.io.com_rbk_valids := io.com_rbk_valids
   imaptable.io.flush_pipeline := io.flush_pipeline
   imaptable.io.inst_can_proceed := io.inst_can_proceed
   imaptable.io.freelist_can_allocate := ifreelist.io.can_allocate

   for (w <- 0 until pl_width)
   {
      // TODO XXX choose between FP, INT
      io.ren_uops(w).pop1 := imaptable.io.values(w).prs1
      io.ren_uops(w).pop2 := imaptable.io.values(w).prs2
      io.ren_uops(w).pop3 := imaptable.io.values(w).prs3
      io.ren_uops(w).stale_pdst := imaptable.io.values(w).stale_pdst
   }

   //-------------------------------------------------------------
   // Free List

   ifreelist.io.brinfo := io.brinfo
   ifreelist.io.kill := io.kill
   ifreelist.io.ren_mask := io.ren_mask
   ifreelist.io.ren_uops := io.ren_uops
   ifreelist.io.ren_br_vals := ren_br_vals
   ifreelist.io.inst_can_proceed := io.inst_can_proceed
   ifreelist.io.com_valids := io.com_valids
   ifreelist.io.com_uops := io.com_uops
   ifreelist.io.com_rbk_valids := io.com_rbk_valids
   ifreelist.io.flush_pipeline := io.flush_pipeline
   ifreelist.io.debug_rob_empty := io.debug_rob_empty

   for (w <- 0 until pl_width)
   {
      io.ren_uops(w).pdst := ifreelist.io.req_pregs(w) // TODO BUG XXX choose between FP, Int
   }

   //-------------------------------------------------------------
   // Busy Table

   ibusytable.io.ren_mask := io.ren_mask
   ibusytable.io.ren_uops := io.ren_uops  // expects pdst to be set up.
   ibusytable.io.freelist_can_allocate := ifreelist.io.can_allocate
   ibusytable.io.map_table := imaptable.io.values
   ibusytable.io.wb_valids := io.int_wb_valids
   ibusytable.io.wb_pdsts := io.int_wb_pdsts


   for (w <- 0 until pl_width)
   {
      // TODO XXX choose between FP, INT
      io.ren_uops(w).prs1_busy := ibusytable.io.values(w).prs1_busy
      io.ren_uops(w).prs2_busy := ibusytable.io.values(w).prs2_busy
      io.ren_uops(w).prs3_busy := ibusytable.io.values(w).prs3_busy
   }

   //-------------------------------------------------------------
   // Branch Predictor Snapshots

   // Each branch prediction must snapshot the predictor (history state, etc.).
   // On a mispredict, the snapshot must be used to reset the predictor.
   // TODO use Mem(), but it chokes on the undefines in VCS
   val prediction_copies = Reg(Vec(MAX_BR_COUNT, new BranchPredictionResp))

   for (w <- 0 until pl_width)
   {
      when(ren_br_vals(w)) {
         prediction_copies(io.ren_uops(w).br_tag) := io.ren_pred_info
      }
   }

   io.get_pred.info := prediction_copies(io.get_pred.br_tag)

   val temp = Wire(new BranchPredictionResp)
   println("\t\tPrediction Snapshots: " + temp.toBits.getWidth + "-bits, " + MAX_BR_COUNT + " entries")

   //-------------------------------------------------------------
   // Outputs
   for (w <- 0 until pl_width)
   {
      // TODO REFACTOR, make == rt_x?
      // TODO BUG XXX canallocate int vs fp
      io.inst_can_proceed(w) :=
         (ifreelist.io.can_allocate(w) || (io.ren_uops(w).dst_rtype =/= RT_FIX && io.ren_uops(w).dst_rtype =/= RT_FLT)) &&
         io.dis_inst_can_proceed(w)
   }

   //-------------------------------------------------------------
   // Debug signals

   io.debug.ifreelist  := ifreelist.io.debug.freelist
   io.debug.iisprlist  := ifreelist.io.debug.isprlist
   io.debug.ibusytable := ibusytable.io.debug.busytable
}

