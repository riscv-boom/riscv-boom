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
//
// Supports 1-cycle and 2-cycle latencies. (aka, passthrough versus registers between ren1 and ren2).
//    - ren1: read the map tables and allocate a new physical register from the freelist.
//    - ren2: read the busy table for the physical operands.
//
// Ren1 data is provided as an output to be fed directly into the ROB.


package boom.exu

import Chisel._
import chisel3.core.DontCare
import freechips.rocketchip.config.Parameters
import boom.common._
import boom.util._
import boom.vec.ScalarOpFreeList


class RenameStageIO(
   val pl_width: Int,
   val num_int_pregs: Int,
   val num_fp_pregs: Int,
   val num_vec_pregs: Int,
   val num_int_wb_ports: Int,
   val num_fp_wb_ports: Int,
   val num_vec_wb_ports: Int)
   (implicit p: Parameters) extends BoomBundle()(p)
{
   private val int_preg_sz = log2Up(num_int_pregs)
   private val fp_preg_sz = log2Up(num_fp_pregs)
   private val vec_preg_sz = log2Up(num_vec_pregs)
   val inst_can_proceed = Vec(pl_width, Bool()).asOutput

   val kill      = Bool(INPUT)

   val dec_will_fire = Vec(pl_width, Bool()).asInput // will commit state updates
   val dec_uops  = Vec(pl_width, new MicroOp()).asInput

   // physical specifiers now available (but not the busy/ready status of the operands).
   val ren1_mask = Vec(pl_width, Bool().asOutput) // mask of valid instructions
   val ren1_uops = Vec(pl_width, new MicroOp().asOutput)

   // physical specifiers available AND busy/ready status available.
   val ren2_mask  = Vec(pl_width, Bool().asOutput) // mask of valid instructions
   val ren2_uops  = Vec(pl_width, new MicroOp().asOutput)

   // branch resolution (execute)
   val brinfo    = new BrResolutionInfo().asInput

   val dis_inst_can_proceed = Vec(DISPATCH_WIDTH, Bool()).asInput

   // issue stage (fast wakeup)
   val int_wakeups = Vec(num_int_wb_ports, Valid(new ExeUnitResp(xLen))).flip
   val fp_wakeups = Vec(num_fp_wb_ports, Valid(new ExeUnitResp(fLen+1))).flip
   val vec_wakeups = Vec(num_vec_wb_ports, Valid(new ExeUnitResp(vecStripLen))).flip
   // commit stage
   val com_valids = Vec(pl_width, Bool()).asInput
   val com_uops   = Vec(pl_width, new MicroOp()).asInput
   val com_rbk_valids = Vec(pl_width, Bool()).asInput

   val flush_pipeline = Bool(INPUT) // only used for SCR (single-cycle reset)

   val vl = UInt(width=VL_SZ).asInput

   val debug_rob_empty = Bool(INPUT)
   val debug = new DebugRenameStageIO(num_int_pregs, num_fp_pregs, num_vec_pregs).asOutput
}


class DebugRenameStageIO(int_num_pregs: Int, fp_num_pregs: Int, vec_num_pregs: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val ifreelist = Bits(width=int_num_pregs)
   val iisprlist = Bits(width=int_num_pregs)
   val ibusytable = UInt(width=int_num_pregs)
   val ffreelist = Bits(width=fp_num_pregs)
   val fisprlist = Bits(width=fp_num_pregs)
   val fbusytable = UInt(width=fp_num_pregs)
   val vfreelist = Bits(width=vec_num_pregs)
   val visprlist = Bits(width=vec_num_pregs)
   val vbusytable = UInt(width=vec_num_pregs)

   override def cloneType: this.type = new DebugRenameStageIO(int_num_pregs, fp_num_pregs, vec_num_pregs).asInstanceOf[this.type]
}


class RenameStage(
   pl_width: Int,
   num_int_wb_ports: Int,
   num_fp_wb_ports: Int,
   num_vec_wb_ports: Int)
(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new RenameStageIO(pl_width, numIntPhysRegs, numFpPhysRegs, numVecPhysRegs, num_int_wb_ports, num_fp_wb_ports, num_vec_wb_ports))

   // integer registers
   val imaptable = Module(new RenameMapTable(
      pl_width,
      RT_FIX.litValue,
      32,
      numIntPhysRegs))
   val ifreelist = Module(new RenameFreeList(
      pl_width,
      RT_FIX.litValue,
      numIntPhysRegs,
      32))
   val ibusytable = Module(new BusyTable(
      pl_width,
      RT_FIX.litValue,
      num_pregs = numIntPhysRegs,
      num_read_ports = pl_width*2,
      num_wb_ports = num_int_wb_ports,
      isVector = false))

   // floating point registers
   var fmaptable: RenameMapTable = null
   var ffreelist: RenameFreeList = null
   var fbusytable: BusyTable = null

   if (usingFPU) {
      fmaptable = Module(new RenameMapTable(
         pl_width,
         RT_FLT.litValue,
         64,
         numFpPhysRegs))
      ffreelist = Module(new RenameFreeList(
         pl_width,
         RT_FLT.litValue,
         numFpPhysRegs,
         LOGICAL_REG_COUNT))
      fbusytable = Module(new BusyTable(
         pl_width,
         RT_FLT.litValue,
         num_pregs = numFpPhysRegs,
         num_read_ports = pl_width*3,
         num_wb_ports = num_fp_wb_ports,
         isVector = false))
   }

   var vmaptable: RenameMapTable = null
   var vfreelist: RenameFreeList = null
   var vbusytable: BusyTable = null

   var vpmaptable: RenameMapTable = null
   var vpfreelist: RenameFreeList = null
   var vpbusytable: BusyTable = null

   var vsfreelist: ScalarOpFreeList = null

   if (usingVec) {
      vmaptable = Module(new RenameMapTable(
         pl_width,
         RT_VEC.litValue,
         32,
         numVecPhysRegs)) // Todo change to num vec phys regs
      vfreelist = Module(new RenameFreeList(
         pl_width,
         RT_VEC.litValue,
         numVecPhysRegs,
         32))
      vbusytable = Module(new BusyTable(
         pl_width,
         RT_VEC.litValue,
         num_pregs = numVecPhysRegs,
         num_read_ports = pl_width*3,
         num_wb_ports = num_vec_wb_ports,
         isVector = true)) // TODO: Figure out what this is

      vpmaptable = Module(new RenameMapTable(
         pl_width,
         RT_VPRED.litValue,
         1,
         numVecPhysPRegs))
      vpfreelist = Module(new RenameFreeList(
         pl_width,
         RT_VPRED.litValue,
         numVecPhysPRegs,
         1))
      vpbusytable= Module(new BusyTable(
         pl_width,
         RT_VPRED.litValue,
         num_pregs = numVecPhysPRegs,
         num_read_ports = pl_width,
         num_wb_ports = num_vec_wb_ports,
         isVector = true))

      vsfreelist = Module(new ScalarOpFreeList(pl_width))
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

   val freelists = Seq(ifreelist) ++ (if (usingFPU) Seq(ffreelist) else Seq()) ++ (if (usingVec) Seq(vfreelist, vpfreelist) else Seq())
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
      val v_preg = if (usingVec) vfreelist.io.req_pregs(w) else 0.U
      val vp_preg= if (usingVec) vpfreelist.io.req_pregs(w) else 0.U
      uop.pdst := i_preg
      when (usingFPU.B && uop.dst_rtype === RT_FLT) { uop.pdst := f_preg }
      when (usingVec.B && uop.dst_rtype === RT_VEC) { uop.pdst := v_preg }
      when (usingVec.B && uop.writes_vp)            { uop.vp_pdst := vp_preg }
   }


   if (usingVec) {
      // "Freelist" for scalar operand buffer rows
      // TODO_Vec: Roll this into above
      vsfreelist.io.brinfo         := io.brinfo
      vsfreelist.io.kill           := io.kill
      vsfreelist.io.ren_will_fire  := ren1_will_fire
      vsfreelist.io.ren_uops       := ren1_uops map {u => GetNewUopAndBrMask(u, io.brinfo)}
      vsfreelist.io.ren_br_vals    := ren1_br_vals

      vsfreelist.io.com_valids     := io.com_valids
      vsfreelist.io.com_uops       := io.com_uops
      vsfreelist.io.com_rbk_valids := io.com_rbk_valids
      vsfreelist.io.flush_pipeline := io.flush_pipeline

      for (w <- 0 until pl_width) {
         ren1_uops(w).vscopb_idx := vsfreelist.io.req_scopb_idx(w)
      }
   }


   //-------------------------------------------------------------
   // Rename Table

   val maptables = Seq(imaptable) ++ (if (usingFPU) Seq(fmaptable) else Seq()) ++ (if (usingVec) Seq(vmaptable, vpmaptable) else Seq())
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
   if (usingFPU) fmaptable.io.debug_freelist_can_allocate  := ffreelist.io.can_allocate
   if (usingVec) vmaptable.io.debug_freelist_can_allocate  := vfreelist.io.can_allocate
   if (usingVec) vpmaptable.io.debug_freelist_can_allocate := vpfreelist.io.can_allocate
   for ((uop, w) <- ren1_uops.zipWithIndex)
   {
      val imap = imaptable.io.values(w)

      uop.pop1          := imap.prs1
      uop.pop2          := imap.prs2
      uop.stale_pdst    := imap.stale_pdst
      if (usingFPU) {
         val fmap = fmaptable.io.values(w)
         when (uop.lrs1_rtype === RT_FLT) { uop.pop1       := fmap.prs1 }
         when (uop.lrs2_rtype === RT_FLT) { uop.pop2       := fmap.prs2 }
         uop.pop3                                          := fmap.prs3
         when (uop.dst_rtype  === RT_FLT) { uop.stale_pdst := fmap.stale_pdst }
      }
      if (usingVec) {
         val vmap = vmaptable.io.values(w)
         when (uop.lrs1_rtype === RT_VEC) { uop.pop1       := vmap.prs1 }
         when (uop.lrs2_rtype === RT_VEC) { uop.pop2       := vmap.prs2 }
         when (uop.lrs3_rtype === RT_VEC) { uop.pop3       := vmap.prs3 }
         when (uop.dst_rtype  === RT_VEC) { uop.stale_pdst := vmap.stale_pdst }

         val vpmap = vpmaptable.io.values(w)
         when (uop.vp_type =/= VPRED_X)   { uop.vp_pop     := vpmap.prs1 }
         when (uop.writes_vp)             { uop.stale_vp_pdst := vpmap.stale_pdst }
      }
   }

   //-------------------------------------------------------------
   // pipeline registers

   val ren2_will_fire = ren2_valids zip io.dis_inst_can_proceed map {case (v,c) => v && c && !io.kill}

   // will ALL ren2 uops proceed to dispatch?
   val ren2_will_proceed =
      if (renameLatency == 2) (ren2_valids zip ren2_will_fire map {case (v,f) => (v === f)}).reduce(_&_)
      else io.dis_inst_can_proceed.reduce(_&_)




   val fmap_vals  = if (usingFPU) fmaptable.io.values else Wire(new MapTableOutput(1))
   val vmap_vals  = if (usingVec) vmaptable.io.values else Wire(new MapTableOutput(1))
   val vpmap_vals = if (usingVec) vpmaptable.io.values else Wire(new MapTableOutput(1))


   val ren2_imapvalues  = if (renameLatency == 2) RegEnable(imaptable.io.values, ren2_will_proceed)
                          else imaptable.io.values
   val ren2_fmapvalues  = if (renameLatency == 2) RegEnable(fmap_vals, ren2_will_proceed)
                          else fmap_vals
   val ren2_vmapvalues  = if (renameLatency == 2) RegEnable(vmap_vals, ren2_will_proceed)
                          else vmap_vals
   val ren2_vpmapvalues = if (renameLatency == 2) RegEnable(vpmap_vals, ren2_will_proceed)
                          else vpmap_vals
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
         val r_valid = Reg(init = false.B)
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
   ibusytable.io.ren_uops      := ren2_uops  // expects pdst to be set up.
   ibusytable.io.map_table     := ren2_imapvalues
   ibusytable.io.wb_valids     := io.int_wakeups.map(_.valid)
   ibusytable.io.wb_pdsts      := io.int_wakeups.map(_.bits.uop.pdst)

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
      fbusytable.io.ren_uops      := ren2_uops  // expects pdst to be set up.
      fbusytable.io.map_table     := ren2_fmapvalues
      fbusytable.io.wb_valids     := io.fp_wakeups.map(_.valid)
      fbusytable.io.wb_pdsts      := io.fp_wakeups.map(_.bits.uop.pdst)

      assert (!(io.fp_wakeups.map(x => x.valid && x.bits.uop.dst_rtype =/= RT_FLT).reduce(_|_)),
         "[rename] fp wakeup is not waking up a FP register.")
   }

   if (usingVec) {
      vbusytable.io.ren_will_fire := ren2_will_fire
      vbusytable.io.ren_uops      := ren2_uops
      vbusytable.io.map_table     := ren2_vmapvalues
      vbusytable.io.wb_valids     := io.vec_wakeups.map(_.valid)
      vbusytable.io.wb_pdsts      := io.vec_wakeups.map(_.bits.uop.pdst)
      vbusytable.io.wb_eidxs      := io.vec_wakeups.map(x=>x.bits.uop.eidx + x.bits.uop.rate) // TODO_Vec: This is probably bad
      vbusytable.io.vl            := io.vl
      assert (!(io.vec_wakeups.map(x => x.valid && x.bits.uop.dst_rtype =/= RT_VEC).reduce(_|_)),
         "[rename] vec wakeup is not waking up a VEC register.")

      vpbusytable.io.ren_will_fire:= ren2_will_fire
      vpbusytable.io.ren_uops     := ren2_uops
      vpbusytable.io.map_table    := ren2_vpmapvalues
      vpbusytable.io.wb_valids    := io.vec_wakeups.map(x => x.valid && x.bits.uop.writes_vp)
      vpbusytable.io.wb_pdsts     := io.vec_wakeups.map(x => x.bits.uop.vp_pdst)
      vpbusytable.io.wb_eidxs     := io.vec_wakeups.map(x => x.bits.uop.eidx + x.bits.uop.rate)
      vpbusytable.io.vl           := io.vl
   }
   for ((uop, w) <- ren2_uops.zipWithIndex)
   {
      val ibusy  = ibusytable.io.values(w)
      val fbusy  = if (usingFPU) fbusytable.io.values(w) else new BusyTableOutput
      val vbusy  = if (usingVec) vbusytable.io.values(w) else new BusyTableOutput
      val vpbusy = if (usingVec) vpbusytable.io.values(w) else new BusyTableOutput

      uop.prs1_busy := ibusy.prs1_busy
      uop.prs2_busy := ibusy.prs2_busy
      uop.prs3_busy := false.B
      uop.pvp_busy  := false.B
      if (usingFPU) {
         when (uop.lrs1_rtype === RT_FLT) { uop.prs1_busy := fbusy.prs1_busy }
         when (uop.lrs2_rtype === RT_FLT) { uop.prs2_busy := fbusy.prs2_busy }
         when (uop.lrs3_rtype === RT_FLT) { uop.prs3_busy := fbusy.prs3_busy }
      }
      if (usingVec) {
         when (uop.lrs1_rtype === RT_VEC) { uop.prs1_busy := vbusy.prs1_busy ; uop.prs1_eidx := vbusy.prs1_eidx }
         when (uop.lrs2_rtype === RT_VEC) { uop.prs2_busy := vbusy.prs2_busy ; uop.prs2_eidx := vbusy.prs2_eidx }
         when (uop.lrs3_rtype === RT_VEC) { uop.prs3_busy := vbusy.prs3_busy ; uop.prs3_eidx := vbusy.prs3_eidx }
         when (uop.vp_type =/= VPRED_X)   { uop.pvp_busy  := vpbusy.prs1_busy; uop.pvp_eidx  := vpbusy.prs1_eidx }
      }

      val valid = ren2_valids(w)
      assert (!(valid && ibusy.prs1_busy && uop.lrs1_rtype === RT_FIX && uop.lrs1 === UInt(0)), "[rename] x0 is busy??")
      assert (!(valid && ibusy.prs2_busy && uop.lrs2_rtype === RT_FIX && uop.lrs2 === UInt(0)), "[rename] x0 is busy??")
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

      // Push back against Decode stage if Rename1 can't proceed (and Rename2/Dispatch can't receive).
      io.inst_can_proceed(w) :=
         ren2_will_proceed &&
         ((ren1_uops(w).dst_rtype =/= RT_FIX && ren1_uops(w).dst_rtype =/= RT_FLT && ren1_uops(w).dst_rtype =/= RT_VEC) ||
         (ifreelist.io.can_allocate(w) && ren1_uops(w).dst_rtype === RT_FIX) ||
         (if (usingFPU) (ffreelist.io.can_allocate(w) && ren1_uops(w).dst_rtype === RT_FLT) else false.B) ||
         (if (usingVec) (vfreelist.io.can_allocate(w) && ren1_uops(w).dst_rtype === RT_VEC) else false.B)) &&
         (if (usingVec) (vsfreelist.io.can_allocate(w) || !ren1_uops(w).use_vscopb) else true.B) &&
         (if (usingVec) (vpfreelist.io.can_allocate(w) || !ren1_uops(w).writes_vp) else true.B)
   }


   //-------------------------------------------------------------
   // Debug signals

   io.debug.ifreelist  := ifreelist.io.debug.freelist
   io.debug.iisprlist  := ifreelist.io.debug.isprlist
   io.debug.ibusytable := ibusytable.io.debug.busytable
   if (usingFPU) {
      io.debug.ffreelist  := ffreelist.io.debug.freelist
      io.debug.fisprlist  := ffreelist.io.debug.isprlist
      io.debug.fbusytable := fbusytable.io.debug.busytable
   } else {
      io.debug.ffreelist  := DontCare
      io.debug.fisprlist  := DontCare
      io.debug.fbusytable := DontCare
   }
   if (usingVec) {
      io.debug.vfreelist  := vfreelist.io.debug.freelist
      io.debug.visprlist  := vfreelist.io.debug.isprlist
      io.debug.vbusytable := vbusytable.io.debug.busytable
   } else {
      io.debug.vfreelist  := DontCare
      io.debug.visprlist  := DontCare
      io.debug.vbusytable := DontCare
   }

   override val compileOptions = chisel3.core.ExplicitCompileOptions.NotStrict.copy(explicitInvalidate = true)
}
