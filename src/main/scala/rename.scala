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
   val fp_wakeups = Vec(num_fp_wb_ports, Valid(new ExeUnitResp(fLen+1))).flip

   // commit stage
   val com_valids = Vec(pl_width, Bool()).asInput
   val com_uops   = Vec(pl_width, new MicroOp()).asInput
   val com_rbk_valids = Vec(pl_width, Bool()).asInput

   val flush_pipeline = Bool(INPUT) // only used for SCR (single-cycle reset)

   val debug_rob_empty = Bool(INPUT)
   val debug = new DebugRenameStageIO(num_int_pregs, num_fp_pregs).asOutput
}


class DebugRenameStageIO(int_num_pregs: Int, fp_num_pregs: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val ifreelist = Bits(width=int_num_pregs)
   val iisprlist = Bits(width=int_num_pregs)
   val ibusytable = UInt(width=int_num_pregs)
   val ffreelist = Bits(width=fp_num_pregs)
   val fisprlist = Bits(width=fp_num_pregs)
   val fbusytable = UInt(width=fp_num_pregs)
   override def cloneType: this.type = new DebugRenameStageIO(int_num_pregs, fp_num_pregs).asInstanceOf[this.type]
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
      io.ren_uops(w).br_mask := GetNewBrMask(io.brinfo, io.dec_uops(w)) // TODO consolidate into getNewBranchMask
      ren_br_vals(w)         := io.dec_mask(w) && io.dec_uops(w).allocate_brtag
   }
 
   //-------------------------------------------------------------
   // Free List
 
   for (list <- Seq(ifreelist, ffreelist))
   {
      list.io.brinfo := io.brinfo
      list.io.kill := io.kill
      list.io.ren_mask := io.ren_mask
      list.io.ren_uops := io.ren_uops
      list.io.ren_br_vals := ren_br_vals
      list.io.inst_can_proceed := io.inst_can_proceed
      list.io.com_valids := io.com_valids
      list.io.com_uops := io.com_uops
      list.io.com_rbk_valids := io.com_rbk_valids
      list.io.flush_pipeline := io.flush_pipeline
      list.io.debug_rob_empty := io.debug_rob_empty
   }

   for ((uop, w) <- io.ren_uops.zipWithIndex)
   {
      val i_preg = ifreelist.io.req_pregs(w)
      val f_preg = ffreelist.io.req_pregs(w)
      uop.pdst := Mux(uop.dst_rtype === RT_FLT, f_preg, i_preg)
   }
 
   //-------------------------------------------------------------
   // Rename Table

   for (table <- Seq(imaptable, fmaptable))
   {
      table.io.brinfo := io.brinfo
      table.io.kill := io.kill
      table.io.ren_mask := io.ren_mask
      table.io.ren_uops := io.ren_uops // expects pdst to be set up
      table.io.ren_br_vals := ren_br_vals
      table.io.com_valids := io.com_valids
      table.io.com_uops := io.com_uops
      table.io.com_rbk_valids := io.com_rbk_valids
      table.io.flush_pipeline := io.flush_pipeline
      table.io.inst_can_proceed := io.inst_can_proceed
   }
   imaptable.io.freelist_can_allocate := ifreelist.io.can_allocate
   fmaptable.io.freelist_can_allocate := ffreelist.io.can_allocate

   for ((uop, w) <- io.ren_uops.zipWithIndex)
   {
      val imap = imaptable.io.values(w)
      val fmap = fmaptable.io.values(w)

      uop.pop1       := Mux(uop.lrs1_rtype === RT_FLT, fmap.prs1, imap.prs1)
      uop.pop2       := Mux(uop.lrs2_rtype === RT_FLT, fmap.prs2, imap.prs2)
      uop.pop3       := fmaptable.io.values(w).prs3 // only FP has 3rd operand
      uop.stale_pdst := Mux(uop.dst_rtype === RT_FLT,  fmap.stale_pdst, imap.stale_pdst)
   }

   //-------------------------------------------------------------
   // pipeline registers
   // TODO add def function for RegOrWire

   //-------------------------------------------------------------
   // Busy Table

   ibusytable.io.ren_mask := io.ren_mask
   ibusytable.io.ren_uops := io.ren_uops  // expects pdst to be set up.
   ibusytable.io.freelist_can_allocate := ifreelist.io.can_allocate
   ibusytable.io.map_table := imaptable.io.values
   ibusytable.io.wb_valids := io.int_wb_valids
   ibusytable.io.wb_pdsts := io.int_wb_pdsts

   fbusytable.io.ren_mask := io.ren_mask
   fbusytable.io.ren_uops := io.ren_uops  // expects pdst to be set up.
   fbusytable.io.freelist_can_allocate := ffreelist.io.can_allocate
   fbusytable.io.map_table := fmaptable.io.values
   fbusytable.io.wb_valids := io.fp_wakeups.map(_.valid)
   fbusytable.io.wb_pdsts := io.fp_wakeups.map(_.bits.uop.pdst)
   
   assert (!(io.fp_wakeups.map(x => x.valid && x.bits.uop.dst_rtype =/= RT_FLT).reduce(_|_)),
      "[rename] fp wakeup is not waking up a FP register.")

   for ((uop, w) <- io.ren_uops.zipWithIndex)
   {
      val ibusy = ibusytable.io.values(w)
      val fbusy = fbusytable.io.values(w)
      uop.prs1_busy := Mux(uop.lrs1_rtype === RT_FLT, fbusy.prs1_busy, ibusy.prs1_busy)
      uop.prs2_busy := Mux(uop.lrs2_rtype === RT_FLT, fbusy.prs2_busy, ibusy.prs2_busy)
      uop.prs3_busy := fbusy.prs3_busy
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
      io.inst_can_proceed(w) :=
         io.dis_inst_can_proceed(w) &&
         ((io.ren_uops(w).dst_rtype =/= RT_FIX && io.ren_uops(w).dst_rtype =/= RT_FLT) ||
         (ifreelist.io.can_allocate(w) && io.ren_uops(w).dst_rtype === RT_FIX) ||
         (ffreelist.io.can_allocate(w) && io.ren_uops(w).dst_rtype === RT_FLT))
   }

   //-------------------------------------------------------------
   // Debug signals

   io.debug.ifreelist  := ifreelist.io.debug.freelist
   io.debug.iisprlist  := ifreelist.io.debug.isprlist
   io.debug.ibusytable := ibusytable.io.debug.busytable
   io.debug.ffreelist  := ffreelist.io.debug.freelist
   io.debug.fisprlist  := ffreelist.io.debug.isprlist
   io.debug.fbusytable := fbusytable.io.debug.busytable
}

