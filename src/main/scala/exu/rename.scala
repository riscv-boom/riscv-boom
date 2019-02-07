//******************************************************************************
// Copyright (c) 2012 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// RISCV Processor Datapath: Rename Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Supports 1-cycle and 2-cycle latencies. (aka, passthrough versus registers between ren1 and ren2).
//    - ren1: read the map tables and allocate a new physical register from the freelist.
//    - ren2: read the busy table for the physical operands.
//
// Ren1 data is provided as an output to be fed directly into the ROB.


package boom.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import boom.common._
import boom.util._


class RenameStageIO(
   val pl_width: Int,
   val num_int_pregs: Int,
   val num_fp_pregs: Int,
   val num_int_wb_ports: Int,
   val num_fp_wb_ports: Int)
   (implicit p: Parameters) extends BoomBundle()(p)
{
   private val int_preg_sz = log2Ceil(num_int_pregs)
   private val fp_preg_sz = log2Ceil(num_fp_pregs)

   val inst_can_proceed = Output(Vec(pl_width, Bool()))

   val kill      = Input(Bool())

   val dec_will_fire = Input(Vec(pl_width, Bool())) // will commit state updates
   val dec_uops  = Input(Vec(pl_width, new MicroOp()))

   // physical specifiers now available (but not the busy/ready status of the operands).
   val ren1_mask = Vec(pl_width, Output(Bool())) // mask of valid instructions
   val ren1_uops = Vec(pl_width, Output(new MicroOp()))

   // physical specifiers available AND busy/ready status available.
   val ren2_mask  = Vec(pl_width, Output(Bool())) // mask of valid instructions
   val ren2_uops  = Vec(pl_width, Output(new MicroOp()))

   // branch resolution (execute)
   val brinfo    = Input(new BrResolutionInfo())

   val dis_inst_can_proceed = Input(Vec(DISPATCH_WIDTH, Bool()))

   // issue stage (fast wakeup)
   val int_wakeups = Flipped(Vec(num_int_wb_ports, Valid(new ExeUnitResp(xLen))))
   val fp_wakeups = Flipped(Vec(num_fp_wb_ports, Valid(new ExeUnitResp(fLen+1))))

   // commit stage
   val com_valids = Input(Vec(pl_width, Bool()))
   val com_uops   = Input(Vec(pl_width, new MicroOp()))
   val com_rbk_valids = Input(Vec(pl_width, Bool()))

   val flush_pipeline = Input(Bool()) // only used for SCR (single-cycle reset)

   val debug_rob_empty = Input(Bool())
   val debug = Output(new DebugRenameStageIO(num_int_pregs, num_fp_pregs))
}


class DebugRenameStageIO(val int_num_pregs: Int, val fp_num_pregs: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val ifreelist =  Bits(int_num_pregs.W)
   val iisprlist =  Bits(int_num_pregs.W)
   val ibusytable = UInt(int_num_pregs.W)
   val ffreelist =  Bits(fp_num_pregs.W)
   val fisprlist =  Bits(fp_num_pregs.W)
   val fbusytable = UInt(fp_num_pregs.W)
}


class RenameStage(
   pl_width: Int,
   num_int_wb_ports: Int,
   num_fp_wb_ports: Int)
(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new RenameStageIO(pl_width, numIntPhysRegs, numFpPhysRegs, num_int_wb_ports, num_fp_wb_ports))

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
   var fmaptable: RenameMapTable = null
   var ffreelist: RenameFreeList = null
   var fbusytable: BusyTable = null

   if (usingFPU) {
      fmaptable = Module(new RenameMapTable(
         pl_width,
         RT_FLT.litValue,
         32,
         numFpPhysRegs))
      ffreelist = Module(new RenameFreeList(
         pl_width,
         RT_FLT.litValue,
         numFpPhysRegs))
      fbusytable = Module(new BusyTable(
         pl_width,
         RT_FLT.litValue,
         num_pregs = numFpPhysRegs,
         num_read_ports = pl_width*3,
         num_wb_ports = num_fp_wb_ports))
   }

   //-------------------------------------------------------------
   // Pipeline State & Wires

   val ren1_br_vals   = Wire(Vec(pl_width, Bool()))
   val ren1_will_fire = Wire(Vec(pl_width, Bool()))
   val ren1_uops      = Wire(Vec(pl_width, new MicroOp()))

   val ren2_valids    = Wire(Vec(pl_width, Bool()))
   val ren2_uops      = Wire(Vec(pl_width, new MicroOp()))

   for (w <- 0 until pl_width)
   {
      // TODO silly, we've already verified this beforehand on the inst_can_proceed
      ren1_will_fire(w) := io.dec_will_fire(w) && io.inst_can_proceed(w) && !io.kill
      ren1_uops(w)      := GetNewUopAndBrMask(io.dec_uops(w), io.brinfo)
      ren1_br_vals(w)   := io.dec_will_fire(w) && io.dec_uops(w).allocate_brtag
   }

   //-------------------------------------------------------------
   // Free List

   var freelists = Seq(ifreelist)
   if (usingFPU) freelists ++= Seq(ffreelist)
   for (list <- freelists)
   {
      list.io.brinfo := io.brinfo
      list.io.kill := io.kill
      list.io.ren_will_fire := ren1_will_fire
      list.io.ren_uops := ren1_uops
      list.io.ren_br_vals := ren1_br_vals
      list.io.com_valids := io.com_valids
      list.io.com_uops := io.com_uops
      list.io.com_rbk_valids := io.com_rbk_valids
      list.io.flush_pipeline := io.flush_pipeline
      list.io.debug_rob_empty := io.debug_rob_empty
   }

   for ((uop, w) <- ren1_uops.zipWithIndex)
   {
      val i_preg = ifreelist.io.req_pregs(w)
      val f_preg = if (usingFPU) ffreelist.io.req_pregs(w) else 0.U
      uop.pdst := Mux(uop.dst_rtype === RT_FLT, f_preg, i_preg)
   }

   //-------------------------------------------------------------
   // Rename Table

   var maptables = Seq(imaptable)
   if (usingFPU) maptables ++= Seq(fmaptable)
   for (table <- maptables)
   {
      table.io.brinfo := io.brinfo
      table.io.kill := io.kill
      table.io.ren_will_fire := ren1_will_fire
      table.io.ren_uops := ren1_uops // expects pdst to be set up
      table.io.ren_br_vals := ren1_br_vals
      table.io.com_valids := io.com_valids
      table.io.com_uops := io.com_uops
      table.io.com_rbk_valids := io.com_rbk_valids
      table.io.flush_pipeline := io.flush_pipeline
      table.io.debug_inst_can_proceed := io.inst_can_proceed
   }
   imaptable.io.debug_freelist_can_allocate := ifreelist.io.can_allocate
   if (usingFPU) {
      fmaptable.io.debug_freelist_can_allocate := ffreelist.io.can_allocate
   }

   for ((uop, w) <- ren1_uops.zipWithIndex)
   {
      val imap = imaptable.io.values(w)
      val fmap = if (usingFPU) fmaptable.io.values(w) else Wire(new MapTableOutput(1))
      if (!usingFPU) fmap := DontCare

      uop.pop1       := Mux(uop.lrs1_rtype === RT_FLT, fmap.prs1, imap.prs1)
      uop.pop2       := Mux(uop.lrs2_rtype === RT_FLT, fmap.prs2, imap.prs2)
      uop.pop3       := fmap.prs3 // only FP has 3rd operand
      uop.stale_pdst := Mux(uop.dst_rtype === RT_FLT,  fmap.stale_pdst, imap.stale_pdst)
   }

   //-------------------------------------------------------------
   // pipeline registers

   val ren2_will_fire = ren2_valids zip io.dis_inst_can_proceed map {case (v,c) => v && c && !io.kill}

   // will ALL ren2 uops proceed to dispatch?
   val ren2_will_proceed =
      if (renameLatency == 2) (ren2_valids zip ren2_will_fire map {case (v,f) => (v === f)}).reduce(_&_)
      else io.dis_inst_can_proceed.reduce(_&_)



   val ren2_imapvalues = if (renameLatency == 2) RegEnable(imaptable.io.values, ren2_will_proceed)
                         else imaptable.io.values
   val ren2_fmapvalues = if (renameLatency == 2 && usingFPU) RegEnable(fmaptable.io.values, ren2_will_proceed)
                         else if (usingFPU) fmaptable.io.values
                         else new MapTableOutput(1)

   for (w <- 0 until pl_width)
   {
      if (renameLatency == 1)
      {
         ren2_valids(w) := ren1_will_fire(w)
         ren2_uops(w)   := GetNewUopAndBrMask(ren1_uops(w), io.brinfo)
      }
      else
      {
         require (renameLatency == 2)
         val r_valid = RegInit(false.B)
         val r_uop   = Reg(new MicroOp())

         when (io.kill)
         {
            r_valid := false.B
         }
         .elsewhen (ren2_will_proceed)
         {
            r_valid := ren1_will_fire(w)
            r_uop := GetNewUopAndBrMask(ren1_uops(w), io.brinfo)
         }
         .otherwise
         {
            r_valid := r_valid && !ren2_will_fire(w) // clear bit if uop gets dispatched
            r_uop := GetNewUopAndBrMask(r_uop, io.brinfo)
         }

         ren2_valids(w) := r_valid
         ren2_uops  (w) := r_uop
      }
   }

   //-------------------------------------------------------------
   // Busy Table

   ibusytable.io.ren_will_fire := ren2_will_fire
   ibusytable.io.ren_uops := ren2_uops  // expects pdst to be set up.
   ibusytable.io.map_table := ren2_imapvalues
   ibusytable.io.wb_valids := io.int_wakeups.map(_.valid)
   ibusytable.io.wb_pdsts := io.int_wakeups.map(_.bits.uop.pdst)

   assert (!(io.int_wakeups.map(x => x.valid && x.bits.uop.dst_rtype =/= RT_FIX).reduce(_|_)),
      "[rename] int wakeup is not waking up a Int register.")

   for (w <- 0 until pl_width)
   {
      assert (!(
         ren2_will_fire(w) &&
         ren2_uops(w).lrs1_rtype === RT_FIX &&
         ren2_uops(w).pop1 =/= ibusytable.io.map_table(w).prs1),
         "[rename] ren2 maptable prs1 value don't match uop's values.")
      assert (!(
         ren2_will_fire(w) &&
         ren2_uops(w).lrs2_rtype === RT_FIX &&
         ren2_uops(w).pop2 =/= ibusytable.io.map_table(w).prs2),
         "[rename] ren2 maptable prs2 value don't match uop's values.")
   }

   if (usingFPU) {
      fbusytable.io.ren_will_fire := ren2_will_fire
      // expects pdst to be set up.
      fbusytable.io.ren_uops := ren2_uops
      fbusytable.io.map_table := ren2_fmapvalues
      fbusytable.io.wb_valids := io.fp_wakeups.map(_.valid)
      fbusytable.io.wb_pdsts := io.fp_wakeups.map(_.bits.uop.pdst)

      assert (!(io.fp_wakeups.map(x => x.valid && x.bits.uop.dst_rtype =/= RT_FLT).reduce(_|_)),
         "[rename] fp wakeup is not waking up a FP register.")
   }
   for ((uop, w) <- ren2_uops.zipWithIndex)
   {
      val ibusy = ibusytable.io.values(w)
      val fbusy = if (usingFPU) fbusytable.io.values(w) else Wire(new BusyTableOutput)
      if (!usingFPU) fbusy := DontCare

      uop.prs1_busy := Mux(uop.lrs1_rtype === RT_FLT, fbusy.prs1_busy, ibusy.prs1_busy)
      uop.prs2_busy := Mux(uop.lrs2_rtype === RT_FLT, fbusy.prs2_busy, ibusy.prs2_busy)
      uop.prs3_busy := fbusy.prs3_busy

      val valid = ren2_valids(w)
      assert (!(valid && ibusy.prs1_busy && uop.lrs1_rtype === RT_FIX && uop.lrs1 === 0.U), "[rename] x0 is busy??")
      assert (!(valid && ibusy.prs2_busy && uop.lrs2_rtype === RT_FIX && uop.lrs2 === 0.U), "[rename] x0 is busy??")
   }

   //-------------------------------------------------------------
   // Outputs

   io.ren1_mask := ren1_will_fire
   io.ren1_uops := ren1_uops

   io.ren2_mask := ren2_will_fire
   io.ren2_uops := ren2_uops map {u => GetNewUopAndBrMask(u, io.brinfo)}

   for (w <- 0 until pl_width)
   {
      val ifl_can_proceed = ifreelist.io.can_allocate(w) && ren1_uops(w).dst_rtype === RT_FIX
      val ffl_can_proceed = if (usingFPU) {
        (ffreelist.io.can_allocate(w) && ren1_uops(w).dst_rtype === RT_FLT)
      }
      else {
        false.B
      }
      // Push back against Decode stage if Rename1 can't proceed (and Rename2/Dispatch can't receive).
      io.inst_can_proceed(w) :=
         ren2_will_proceed &&
         ((ren1_uops(w).dst_rtype =/= RT_FIX && ren1_uops(w).dst_rtype =/= RT_FLT) ||
         ifl_can_proceed ||
         ffl_can_proceed)
   }


   //-------------------------------------------------------------
   // Debug signals

   io.debug.ifreelist  := ifreelist.io.debug.freelist
   io.debug.iisprlist  := ifreelist.io.debug.isprlist
   io.debug.ibusytable := ibusytable.io.debug.busytable

   io.debug.ffreelist  := (if (usingFPU) ffreelist.io.debug.freelist else 0.U)
   io.debug.fisprlist  := (if (usingFPU) ffreelist.io.debug.isprlist else 0.U)
   io.debug.fbusytable := (if (usingFPU) fbusytable.io.debug.busytable else 0.U)


}

