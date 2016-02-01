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
{

import Chisel._
import Node._
import cde.Parameters

//-------------------------------------------------------------
//-------------------------------------------------------------

class RenameMapTableElementIo(pl_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val element            = UInt(OUTPUT, PREG_SZ)

   val wens               = Vec.fill(pl_width) { Bool(INPUT) }
   val ren_pdsts          = Vec.fill(pl_width) { UInt(INPUT, PREG_SZ) }

   val ren_br_vals        = Vec.fill(pl_width) { Bool(INPUT) }
   val ren_br_tags        = Vec.fill(pl_width) { UInt(INPUT, BR_TAG_SZ) }

   val br_mispredict      = Bool(INPUT)
   val br_mispredict_tag  = UInt(INPUT, BR_TAG_SZ)

   // rollback (on exceptions)
   // TODO REMOVE THIS ROLLBACK PORT, since wens is mutually exclusive with rollback_wens
   val rollback_wen        = Bool(INPUT)
   val rollback_stale_pdst = UInt(INPUT, PREG_SZ)

   // TODO scr option
   val flush_pipeline      = Bool(INPUT)
   val commit_wen          = Bool(INPUT)
   val commit_pdst         = UInt(INPUT, PREG_SZ)
   val committed_element   = UInt(OUTPUT, PREG_SZ)

   override def cloneType: this.type = new RenameMapTableElementIo(pl_width).asInstanceOf[this.type]
}

class RenameMapTableElement(pipeline_width: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new RenameMapTableElementIo(pipeline_width)

   // Note: I don't use a "valid" signal, since it's annoying to deal with and
   // only necessary until the map tables are filled. So instead I reset the
   // map table to all point to P0. I'm not sure which is less expensive.  The
   // corner-case to deal with is the "stale" register needs to be correct and
   // valid, so that freeing it won't free an actual register that was handed
   // out in the meantime. A software solution is also possible, but I'm
   // unwilling to trust that.

   val element = Reg(init = UInt(0, PREG_SZ))

   // handle branch speculation
   val element_br_copies = Mem(MAX_BR_COUNT, UInt(width = PREG_SZ))


   // this is possibly the hardest piece of code I have ever had to reason about in my LIFE.
   // Or maybe that's the 5am talking.
   // on every branch, make a copy of the rename map table state
   // if jal/jalr, we want to capture our own setting of this register
   // We need to know the AGE of the branch!
   // 1st, is "wen" (incoming)
   // 2nd, is older instructions in same bundle
   // 3rd, current element

   for (w <- 0 until pipeline_width)
   {
      var elm_cases = Array((Bool(false),  UInt(0,PREG_SZ)))

      for (xx <- w to 0 by -1)
      {
         elm_cases ++= Array((io.wens(xx),  io.ren_pdsts(xx)))
      }

      when (io.ren_br_vals(w))
      {
         element_br_copies(io.ren_br_tags(w)) := MuxCase(element, elm_cases)
      }
   }


   // reset table on mispredict
   when(io.br_mispredict)
   {
      element := element_br_copies(io.br_mispredict_tag)
   }
   // rollback to the previous mapping
   .elsewhen (io.rollback_wen)
   {
      element := io.rollback_stale_pdst
   }
   // free list is giving us a new pdst
   .elsewhen (io.wens.reduce(_|_))
   {
      // give write priority to the last instruction in the bundle
      element := PriorityMux(io.wens.reverse, io.ren_pdsts.reverse)
   }

   if (ENABLE_COMMIT_MAP_TABLE)
   {
      val committed_element = Reg(init=UInt(0,PREG_SZ))
      when (io.commit_wen)
      {
         committed_element := io.commit_pdst
      }
      when (io.flush_pipeline)
      {
         element := committed_element
      }
      io.committed_element := committed_element
   }

   // outputs
   io.element  := element
}

//-------------------------------------------------------------
//-------------------------------------------------------------

class FreeListIo(num_phys_registers: Int, pl_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val req_preg_vals = Vec.fill(pl_width) { Bool(INPUT) }
   val req_pregs     = Vec.fill(pl_width) { UInt(OUTPUT, log2Up(num_phys_registers)) }

   // committed and newly freed register
   val enq_vals      = Vec.fill(pl_width) {Bool(INPUT)}
   val enq_pregs     = Vec.fill(pl_width) {UInt(INPUT, log2Up(num_phys_registers))}

   // do we have space to service incoming requests? (per inst granularity)
   val can_allocate  = Vec.fill(pl_width) {Bool(OUTPUT)}

   // handle branches (save copy of freelist on branch, merge on mispredict)
   val ren_br_vals   = Vec.fill(pl_width) {Bool(INPUT)}
   val ren_br_tags   = Vec.fill(pl_width) {UInt(INPUT, BR_TAG_SZ)}

   // handle mispredicts
   val br_mispredict_val = Bool(INPUT)
   val br_mispredict_tag = UInt(INPUT, BR_TAG_SZ)

   // rollback (on exceptions)
   val rollback_wens  = Vec.fill(pl_width) {Bool(INPUT)}
   val rollback_pdsts = Vec.fill(pl_width) {UInt(INPUT, log2Up(num_phys_registers))}

   // or...
   // TODO there are TWO free-list IOs now, based on constants. What is the best way to handle these two designs?
   // perhaps freelist.scala, and instantiate which-ever one I want?
   // TODO naming is inconsistent
   // TODO combine with rollback, whatever?
   val flush_pipeline = Bool(INPUT)
   val com_wens       = Vec.fill(pl_width) {Bool(INPUT)}
   val com_uops       = Vec.fill(pl_width) {new MicroOp().asInput()}

   val debug = new Bundle {
      val freelist = Bits(width=num_phys_registers)
      val isprlist = Bits(width=num_phys_registers)
   }.asOutput
}

// provide a fixed set of renamed destination registers
// i.e., it doesn't matter if a previous UOP needs a pdst or not
// this prevents a dependency chain from existing between UOPs when trying to
// compute a pdst to give away (as well as computing if an available free
// register exists
class RenameFreeList(num_phys_registers: Int // number of physical registers
                    , pl_width: Int          // pipeline width ("dispatch group size")
                     )(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new FreeListIo(num_phys_registers, pl_width)

   // ** FREE LIST TABLE ** //
   val free_list = Reg(init=(~Bits(1,num_phys_registers)))

   // track all allocations that have occurred since branch passed by
   // can quickly reset pipeline on branch mispredict
   val allocation_lists = Reg(Vec(MAX_BR_COUNT, Bits(width = num_phys_registers)))

   // TODO why is this a Vec? can I do this all on one bit-vector?
   val enq_mask = Wire(Vec(pl_width, Bits(width = num_phys_registers)))

   // ------------------------------------------
   // find new,free physical registers

   val requested_pregs_oh_array = Array.fill(pl_width,num_phys_registers){Bool(false)}
   val requested_pregs_oh       = Wire(Vec(pl_width, Bits(width=num_phys_registers)))
   val requested_pregs          = Wire(Vec(pl_width, UInt(width=log2Up(num_phys_registers))))
   var allocated                = Wire(Vec(pl_width, Bool())) // did each inst get allocated a register?

   // init
   for (w <- 0 until pl_width)
   {
      allocated(w) := Bool(false)
   }


   for (i <- 1 until num_phys_registers) // note: p0 stays zero
   {
      val next_allocated = Wire(Vec(pl_width, Bool()))
      var can_allocate = free_list(i)

      for (w <- 0 until pl_width)
      {
         requested_pregs_oh_array(w)(i) = can_allocate && !allocated(w)

         next_allocated(w) := can_allocate | allocated(w)
         can_allocate = can_allocate && allocated(w)
      }

      allocated = next_allocated
   }

   for (w <- 0 until pl_width)
   {
      requested_pregs_oh(w) := Vec(requested_pregs_oh_array(w)).toBits
      requested_pregs(w) := PriorityEncoder(requested_pregs_oh(w))
   }


   // ------------------------------------------
   // Calculate next Free List
   val req_free_list = Wire(Bits(width = num_phys_registers))
   val enq_free_list = Wire(Bits(width = num_phys_registers))
   req_free_list := free_list
   enq_free_list := free_list

   // ** Set requested PREG to "Not Free" ** //

   // bit vector of newly allocated physical registers
   var just_allocated_mask = Bits(0, num_phys_registers)

   // track which allocation_lists just got cleared out by a branch,
   // to enforce a write priority to allocation_lists()
   val br_cleared = Wire(Vec(MAX_BR_COUNT, Bool()))
   for (i <- 0 until MAX_BR_COUNT) { br_cleared(i) := Bool(false) }

   for (w <- pl_width-1 to 0 by -1)
   {
      // When branching, start a fresh copy of the allocation_list
      // but don't forget to bypass in the allocations within our bundle
      when (io.ren_br_vals(w))
      {
         allocation_lists(io.ren_br_tags(w)) := just_allocated_mask
         br_cleared(io.ren_br_tags(w)) := Bool(true)
      }

      // check that we both request a register and was able to allocate a register
      just_allocated_mask = Mux(io.req_preg_vals(w) && allocated(w), requested_pregs_oh(w) | just_allocated_mask,
                                                                     just_allocated_mask)
   }

   for (i <- 0 until MAX_BR_COUNT)
   {
      when (!br_cleared(i))
      {
         allocation_lists(i) := allocation_lists(i) | just_allocated_mask
      }
   }


   // ** Set enqueued PREG to "Free" ** //
   for (w <- 0 until pl_width)
   {
      enq_mask(w) := Bits(0,num_phys_registers)
      when (io.enq_vals(w))
      {
         enq_mask(w) := UInt(1) << io.enq_pregs(w)
      }
      .elsewhen (io.rollback_wens(w))
      {
         enq_mask(w) := UInt(1) << io.rollback_pdsts(w)
      }
   }


   // Update the Free List
   when (!io.br_mispredict_val)
   {
      free_list := (free_list & ~(just_allocated_mask)) | (enq_mask.reduce(_|_))
   }

   // Handle Misprediction
   //merge misspeculated allocation_list with free_list
   val allocation_list = Wire(Bits(width = num_phys_registers))
   allocation_list := allocation_lists(io.br_mispredict_tag)

   when (io.br_mispredict_val)
   {
      // include newly freed register as well!
      free_list := allocation_list | free_list | (enq_mask.reduce(_|_))

      // set other branch allocation_lists to zero where allocation_list(j) == 1...
      for (i <- 0 until MAX_BR_COUNT)
      {
         allocation_lists(i) := allocation_lists(i) & ~allocation_list
      }
   }


   // OPTIONALLY: handle single-cycle resets
   // Committed Free List tracks what the free list is at the commit point,
   // allowing for a single-cycle reset of the rename state on a pipeline flush.
   if (ENABLE_COMMIT_MAP_TABLE)
   {
      val committed_free_list = Reg(init=(~Bits(1,num_phys_registers)))

      val com_mask = Wire(Vec(pl_width, Bits(width=num_phys_registers)))
      val stale_mask = Wire(Vec(pl_width, Bits(width=num_phys_registers)))
      for (w <- 0 until pl_width)
      {
         com_mask(w) := Bits(0,width=num_phys_registers)
         stale_mask(w) := Bits(0,width=num_phys_registers)
         when (io.com_wens(w))
         {
            com_mask(w) := UInt(1) << io.com_uops(w).pdst
            stale_mask(w) := UInt(1) << io.com_uops(w).stale_pdst
         }
      }

      committed_free_list := (committed_free_list & ~(com_mask.reduce(_|_))) | stale_mask.reduce(_|_)

      when (io.flush_pipeline)
      {
         free_list := committed_free_list
      }
      io.debug.isprlist := committed_free_list
   }



   // ** SET OUTPUTS ** //
   io.req_pregs := requested_pregs

   io.can_allocate := allocated

   io.debug.freelist := free_list
}


//-------------------------------------------------------------
//-------------------------------------------------------------

// internally bypasses newly busy registers (.write) to the read ports (.read)
// num_operands is the maximum number of operands per instruction (.e.g., 2 normally, but 3 if FMAs are supported)
class BusyTableIo(pipeline_width:Int, num_read_ports:Int, num_wb_ports:Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   // reading out the busy bits
   val p_rs           = Vec.fill(num_read_ports) {UInt(INPUT, width=PREG_SZ)}
   val p_rs_busy      = Vec.fill(num_read_ports) {Bool(OUTPUT)}

   def prs(i:Int, w:Int):UInt      = p_rs     (w+i*pipeline_width)
   def prs_busy(i:Int, w:Int):Bool = p_rs_busy(w+i*pipeline_width)

   // marking new registers as busy
   val allocated_pdst = Vec.fill(pipeline_width) {(new ValidIO(UInt(width=PREG_SZ))).flip}

   // marking registers being written back as unbusy
   val unbusy_pdst    = Vec.fill(num_wb_ports) {(new ValidIO(UInt(width = PREG_SZ))).flip}

   val debug = new Bundle { val bsy_table= Bits(OUTPUT, width=PHYS_REG_COUNT) }
}

// Register P0 is always NOT_BUSY, and cannot be set to BUSY
// Note: I do NOT bypass from newly busied registers to the read ports.
// That bypass check should be done elsewhere (this is to get it off the
// critical path).
class BusyTable(pipeline_width:Int, num_read_ports:Int, num_wb_ports:Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new BusyTableIo(pipeline_width, num_read_ports, num_wb_ports)

   def BUSY     = Bool(true)
   def NOT_BUSY = Bool(false)

   val table_bsy = Reg(init=Bits(0,PHYS_REG_COUNT))

   for (wb_idx <- 0 until num_wb_ports)
   {
      when (io.unbusy_pdst(wb_idx).valid)
      {
         table_bsy(io.unbusy_pdst(wb_idx).bits) := NOT_BUSY
      }
   }

   for (w <- 0 until pipeline_width)
   {
      when (io.allocated_pdst(w).valid && io.allocated_pdst(w).bits =/= UInt(0))
      {
         table_bsy(io.allocated_pdst(w).bits) := BUSY
      }
   }

   // handle bypassing a clearing of the busy-bit
   for (ridx <- 0 until num_read_ports)
   {
      val just_cleared = io.unbusy_pdst.map(p => p.valid && (p.bits === io.p_rs(ridx))).reduce(_|_)
      // note: no bypassing of the newly busied (that is done outside this module)
      io.p_rs_busy(ridx) := (table_bsy(io.p_rs(ridx)) && !just_cleared)
   }

   io.debug.bsy_table := table_bsy
}

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

class RenameStageIO(pl_width: Int, num_wb_ports: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val ren_mask  = Vec.fill(pl_width) {Bool().asOutput} // mask of valid instructions
   val inst_can_proceed = Vec.fill(pl_width) {Bool(OUTPUT)}

   val kill      = Bool(INPUT)

   val dec_mask  = Vec.fill(pl_width){ Bool().asInput }

   val dec_uops  = Vec.fill(pl_width) {new MicroOp().asInput}
   val ren_uops  = Vec.fill(pl_width) {new MicroOp().asOutput}

   val ren_pred_info = new BranchPredictionResp().asInput

   // branch resolution (execute)
   val brinfo    = new BrResolutionInfo().asInput
   val get_pred  = new GetPredictionInfo().flip

   val dis_inst_can_proceed = Vec.fill(DISPATCH_WIDTH) {Bool(INPUT)}

   // issue stage (fast wakeup)
   val wb_valids = Vec.fill(num_wb_ports) {Bool(INPUT)}
   val wb_pdsts  = Vec.fill(num_wb_ports) {UInt(INPUT, PREG_SZ)}

   // commit stage
   val com_valids = Vec.fill(pl_width) {Bool(INPUT)}
   val com_uops   = Vec.fill(pl_width) {new MicroOp().asInput}
   val com_rbk_valids = Vec.fill(pl_width) {Bool(INPUT)}

   val flush_pipeline = Bool(INPUT) // TODO only used for SCR (single-cycle reset)

   val debug = new Bundle {
      val freelist = Bits(width=PHYS_REG_COUNT)
      val isprlist = Bits(width=PHYS_REG_COUNT)
      val bsy_table = UInt(width=PHYS_REG_COUNT)
   }.asOutput
}


class RenameStage(pl_width: Int, num_wb_ports: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new RenameStageIO(pl_width, num_wb_ports)

   val ren_br_vals = Wire(Vec(pl_width, Bool()))
   val freelist_can_allocate = Wire(Vec(pl_width, Bool()))

   val max_operands = if(!usingFPU) 2 else 3

   //-------------------------------------------------------------
   // Set outputs up... we'll write in the pop*/pdst info below
   for (w <- 0 until pl_width)
   {
      io.ren_mask(w)         := io.dec_mask(w) && io.inst_can_proceed(w) && !io.kill
      io.ren_uops(w)         := io.dec_uops(w)
      io.ren_uops(w).br_mask := GetNewBrMask(io.brinfo, io.dec_uops(w))
      ren_br_vals(w)         := Mux(io.dec_mask(w), io.dec_uops(w).allocate_brtag, Bool(false))
   }


   //-------------------------------------------------------------
   // Rename Table

   val map_table_io = Vec.fill(LOGICAL_REG_COUNT) { Module(new RenameMapTableElement(DECODE_WIDTH)).io }

   for (i <- 0 until LOGICAL_REG_COUNT)
   {
      map_table_io(i).rollback_wen := Bool(false)
      map_table_io(i).rollback_stale_pdst := io.com_uops(0).stale_pdst

      map_table_io(i).commit_wen := Bool(false)
      map_table_io(i).commit_pdst := io.com_uops(0).pdst

      for (w <- 0 until pl_width)
      {
         map_table_io(i).wens(w)        := io.ren_uops(w).ldst === UInt(i) &&
                                           io.ren_mask(w) &&
                                           io.ren_uops(w).ldst_val &&
                                           (io.ren_uops(w).dst_rtype === RT_FIX ||
                                             io.ren_uops(w).dst_rtype === RT_FLT) &&
                                           !io.kill &&
                                           io.inst_can_proceed(w) &&
                                           freelist_can_allocate(w)
         map_table_io(i).ren_pdsts(w)   := io.ren_uops(w).pdst

         map_table_io(i).ren_br_vals(w) := ren_br_vals(w)
         map_table_io(i).ren_br_tags(w) := io.ren_uops(w).br_tag
      }

      map_table_io(i).br_mispredict     := io.brinfo.mispredict
      map_table_io(i).br_mispredict_tag := io.brinfo.tag

      map_table_io(i).flush_pipeline    := io.flush_pipeline
   }

   // backwards, because rollback must give highest priority to 0 (the oldest instruction)
   for (w <- pl_width-1 to 0 by -1)
   {
      val ldst = io.com_uops(w).ldst

      when (io.com_rbk_valids(w))
      {
         map_table_io(ldst).rollback_wen        := Bool(true)
         map_table_io(ldst).rollback_stale_pdst := io.com_uops(w).stale_pdst
      }
   }

   if (ENABLE_COMMIT_MAP_TABLE)
   {
      for (w <- 0 until pl_width)
      {
         val ldst = io.com_uops(w).ldst
         when (io.com_valids(w) && (io.com_uops(w).dst_rtype === RT_FIX || io.com_uops(w).dst_rtype === RT_FLT))
         {
            map_table_io(ldst).commit_wen := Bool(true)
            map_table_io(ldst).commit_pdst := io.com_uops(w).pdst
         }
      }
   }

   // read out the map-table entries ASAP, then deal with bypassing busy-bits later
   private val map_table_output = Wire(Vec(pl_width*3, UInt(width=PREG_SZ)))
   def map_table_prs1(w:Int) = map_table_output(w+0*pl_width)
   def map_table_prs2(w:Int) = map_table_output(w+1*pl_width)
   def map_table_prs3(w:Int) = map_table_output(w+2*pl_width)


   for (w <- 0 until pl_width)
   {
      map_table_prs1(w) := map_table_io(io.ren_uops(w).lrs1).element
      map_table_prs2(w) := map_table_io(io.ren_uops(w).lrs2).element
      if (max_operands > 2)
         map_table_prs3(w) := map_table_io(io.ren_uops(w).lrs3).element
      else
         map_table_prs3(w) := UInt(0)
   }

   // Bypass the physical register mappings
   val prs1_was_bypassed = Wire(Vec(pl_width, Bool()))
   val prs2_was_bypassed = Wire(Vec(pl_width, Bool()))
   val prs3_was_bypassed = Wire(Vec(pl_width, Bool()))

   for (w <- 0 until pl_width)
   {
      var rs1_cases =  Array((Bool(false),  UInt(0,PREG_SZ)))
      var rs2_cases =  Array((Bool(false),  UInt(0,PREG_SZ)))
      var rs3_cases =  Array((Bool(false),  UInt(0,PREG_SZ)))
      var stale_cases= Array((Bool(false),  UInt(0,PREG_SZ)))

      prs1_was_bypassed(w) := Bool(false)
      prs2_was_bypassed(w) := Bool(false)
      prs3_was_bypassed(w) := Bool(false)

      // Handle bypassing new physical destinations to operands (and stale destination)
      // scalastyle:off
      for (xx <- w-1 to 0 by -1)
      {
         rs1_cases  ++= Array(((io.ren_uops(w).lrs1_rtype === RT_FIX || io.ren_uops(w).lrs1_rtype === RT_FLT)
                                                                 && io.ren_mask(xx) && io.ren_uops(xx).ldst_val && (io.ren_uops(w).lrs1 === io.ren_uops(xx).ldst), (io.ren_uops(xx).pdst)))
         rs2_cases  ++= Array(((io.ren_uops(w).lrs2_rtype === RT_FIX || io.ren_uops(w).lrs2_rtype === RT_FLT)
                                                                 && io.ren_mask(xx) && io.ren_uops(xx).ldst_val && (io.ren_uops(w).lrs2 === io.ren_uops(xx).ldst), (io.ren_uops(xx).pdst)))
         rs3_cases  ++= Array((io.ren_uops(w).frs3_en            && io.ren_mask(xx) && io.ren_uops(xx).ldst_val && (io.ren_uops(w).lrs3 === io.ren_uops(xx).ldst), (io.ren_uops(xx).pdst)))
         stale_cases++= Array(( io.ren_uops(w).ldst_val          && io.ren_mask(xx) && io.ren_uops(xx).ldst_val && (io.ren_uops(w).ldst === io.ren_uops(xx).ldst), (io.ren_uops(xx).pdst)))

         when ((io.ren_uops(w).lrs1_rtype === RT_FIX || io.ren_uops(w).lrs1_rtype === RT_FLT) && io.ren_mask(xx) && io.ren_uops(xx).ldst_val && (io.ren_uops(w).lrs1 === io.ren_uops(xx).ldst))
            { prs1_was_bypassed(w) := Bool(true) }
         when ((io.ren_uops(w).lrs2_rtype === RT_FIX || io.ren_uops(w).lrs2_rtype === RT_FLT) && io.ren_mask(xx) && io.ren_uops(xx).ldst_val && (io.ren_uops(w).lrs2 === io.ren_uops(xx).ldst))
            { prs2_was_bypassed(w) := Bool(true) }
         when (io.ren_uops(w).frs3_en                                                         && io.ren_mask(xx) && io.ren_uops(xx).ldst_val && (io.ren_uops(w).lrs3 === io.ren_uops(xx).ldst))
            { prs3_was_bypassed(w) := Bool(true) }
      }

      // add default case where we can just read the map table for our information
      rs1_cases ++= Array(((io.ren_uops(w).lrs1_rtype === RT_FIX || io.ren_uops(w).lrs1_rtype === RT_FLT) &&
                           (io.ren_uops(w).lrs1 =/= UInt(0)), map_table_prs1(w)))
      rs2_cases ++= Array(((io.ren_uops(w).lrs2_rtype === RT_FIX || io.ren_uops(w).lrs2_rtype === RT_FLT) &&
                           (io.ren_uops(w).lrs2 =/= UInt(0)), map_table_prs2(w)))
      rs3_cases ++= Array((io.ren_uops(w).frs3_en  && 
                           (io.ren_uops(w).lrs3 =/= UInt(0)), map_table_prs3(w)))

      io.ren_uops(w).pop1                       := MuxCase(io.ren_uops(w).lrs1, rs1_cases)
      io.ren_uops(w).pop2                       := MuxCase(io.ren_uops(w).lrs2, rs2_cases)
      if (max_operands > 2){io.ren_uops(w).pop3 := MuxCase(io.ren_uops(w).lrs3, rs3_cases)}
      io.ren_uops(w).stale_pdst                 := MuxCase(map_table_io(io.ren_uops(w).ldst).element, stale_cases)

   }


   //-------------------------------------------------------------
   // Busy Table

   val freelist_req_pregs = Wire(Vec(pl_width, UInt(width=PREG_SZ)))

   // 3 WB ports for now
   // 1st is back-to-back bypassable ALU ops
   // 2nd is memory/muldiv
   // TODO 3rd is ALU ops that aren't bypassable... can maybe remove this? or set TTL countdown on 1st port?
   // TODO optimize - too many write ports, but how to deal with that? (slow + fast...)
   val bsy_table = Module(new BusyTable(pipeline_width=pl_width, num_read_ports = pl_width*max_operands, num_wb_ports=num_wb_ports))

      for (w <- 0 until pl_width)
      {
         // Reading the Busy Bits
         // for critical path reasons, we speculatively read out the busy-bits assuming no dependencies between uops
         // then verify if the uop actually uses a register and if it depends on a newly unfreed register
         bsy_table.io.prs(0,w) := map_table_prs1(w)
         bsy_table.io.prs(1,w) := map_table_prs2(w)

         io.ren_uops(w).prs1_busy := (io.ren_uops(w).lrs1_rtype === RT_FIX || io.ren_uops(w).lrs1_rtype === RT_FLT) && (bsy_table.io.prs_busy(0,w) || prs1_was_bypassed(w))
         io.ren_uops(w).prs2_busy := (io.ren_uops(w).lrs2_rtype === RT_FIX || io.ren_uops(w).lrs2_rtype === RT_FLT) && (bsy_table.io.prs_busy(1,w) || prs2_was_bypassed(w))

         if (max_operands > 2)
         {
            bsy_table.io.prs(2,w) := map_table_prs3(w)
            io.ren_uops(w).prs3_busy := (io.ren_uops(w).frs3_en) && (bsy_table.io.prs_busy(2,w) || prs3_was_bypassed(w))
         }
         else
         {
            io.ren_uops(w).prs3_busy := Bool(false)
         }


          // Updating the Table (new busy register)
         bsy_table.io.allocated_pdst(w).valid := freelist_can_allocate(w) &&
                                                  io.ren_mask(w) &&
                                                  io.ren_uops(w).ldst_val &&
                                                  (io.ren_uops(w).dst_rtype === RT_FIX || io.ren_uops(w).dst_rtype === RT_FLT)
         bsy_table.io.allocated_pdst(w).bits  := freelist_req_pregs(w)
      }

      // Clear Busy-bit
      for (i <- 0 until num_wb_ports)
      {
         bsy_table.io.unbusy_pdst(i).valid := io.wb_valids(i)
         bsy_table.io.unbusy_pdst(i).bits  := io.wb_pdsts(i)
      }

   // scalastyle:on

   //-------------------------------------------------------------
   // Free List

   val freelist = Module(new RenameFreeList(PHYS_REG_COUNT, pl_width))

      for (w <- 0 until pl_width)
      {
         freelist.io.req_preg_vals(w) := (io.inst_can_proceed(w) &&
                                         !io.kill &&
                                         io.ren_mask(w) &&
                                         io.ren_uops(w).ldst_val &&
                                         (io.ren_uops(w).dst_rtype === RT_FIX || io.ren_uops(w).dst_rtype === RT_FLT))
      }
      freelist_req_pregs := freelist.io.req_pregs

      for (w <- 0 until pl_width)
      {
         freelist.io.enq_vals(w)    := io.com_valids(w) &&
                                       (io.com_uops(w).dst_rtype === RT_FIX || io.com_uops(w).dst_rtype === RT_FLT) &&
                                       (io.com_uops(w).stale_pdst =/= UInt(0))
         freelist.io.enq_pregs(w)   := io.com_uops(w).stale_pdst

         freelist.io.ren_br_vals(w) := ren_br_vals(w)
         freelist.io.ren_br_tags(w) := io.ren_uops(w).br_tag

         freelist_can_allocate(w)   := freelist.io.can_allocate(w)

         freelist.io.rollback_wens(w)  := io.com_rbk_valids(w) &&
                                        (io.com_uops(w).pdst =/= UInt(0)) &&
                                        (io.com_uops(w).dst_rtype === RT_FIX || io.com_uops(w).dst_rtype === RT_FLT)
         freelist.io.rollback_pdsts(w) := io.com_uops(w).pdst

         freelist.io.com_wens(w)    := io.com_valids(w) &&
                                       (io.com_uops(w).pdst =/= UInt(0)) &&
                                       (io.com_uops(w).dst_rtype === RT_FIX || io.com_uops(w).dst_rtype === RT_FLT)
         freelist.io.com_uops(w)    := io.com_uops(w)
      }

      freelist.io.br_mispredict_val := io.brinfo.mispredict
      freelist.io.br_mispredict_tag := io.brinfo.tag

      freelist.io.flush_pipeline := io.flush_pipeline


   // x0 is a special-case and should not be renamed
   for (w <- 0 until pl_width)
   {
      io.ren_uops(w).pdst := Mux(io.ren_uops(w).ldst === UInt(0), UInt(0), freelist_req_pregs(w))
   }


   //-------------------------------------------------------------
   // Branch Predictor Snapshots

   // Each branch prediction must snapshot the predictor (history state, etc.).
   // On a mispredict, the snapshot must be used to reset the predictor.
   // TODO use Mem(), but it chokes on the undefines in VCS
   //val prediction_copies = Mem(MAX_BR_COUNT, new BranchPredictionResp)
   val prediction_copies = Vec.fill(MAX_BR_COUNT) {Reg(new BranchPredictionResp)}

   for (w <- 0 until pl_width)
   {
      when(ren_br_vals(w))
      {
         prediction_copies(io.ren_uops(w).br_tag) := io.ren_pred_info
      }
   }

   io.get_pred.info := prediction_copies(io.get_pred.br_tag)


   //-------------------------------------------------------------
   // Outputs
   for (w <- 0 until pl_width)
   {
      // TODO REFACTOR, make == rt_x?
      io.inst_can_proceed(w) := (freelist.io.can_allocate(w) ||
                                 (io.ren_uops(w).dst_rtype =/= RT_FIX && io.ren_uops(w).dst_rtype =/= RT_FLT)) &&
                                io.dis_inst_can_proceed(w)
   }


   //-------------------------------------------------------------
   // Debug signals

   io.debug.freelist  := freelist.io.debug.freelist
   io.debug.isprlist  := freelist.io.debug.isprlist
   io.debug.bsy_table := bsy_table.io.debug.bsy_table
}



}

