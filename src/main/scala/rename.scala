//**************************************************************************
// RISCV Processor Datapath: Rename Logic
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2012 Feb 14


package BOOM
{

import Chisel._
import Node._

import scala.collection.mutable.ArrayBuffer

//-------------------------------------------------------------
//-------------------------------------------------------------
 
class RenameMapTableElementIo(pl_width: Int) extends Bundle()
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

   override def clone = new RenameMapTableElementIo(pl_width).asInstanceOf[this.type]
}

class RenameMapTableElement(pipeline_width: Int) extends Module
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
   val element_br_copies = Mem(out=UInt(width = PREG_SZ), n=MAX_BR_COUNT)


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
 
class FreeListIo(num_phys_registers: Int, pl_width: Int) extends Bundle()
{
   val req_preg_vals = Vec.fill(pl_width) { Bool(INPUT) }
   val req_pregs     = Vec.fill(pl_width) { UInt(OUTPUT, log2Up(num_phys_registers)) }

   // committed and newly freed register
   val enq_vals      = Vec.fill(pl_width) {Bool(INPUT)}
   val enq_pregs     = Vec.fill(pl_width) {UInt(INPUT, log2Up(num_phys_registers))}

   // do we have space to service incoming requests?
   val can_allocate  = Vec.fill(pl_width) {Bool(OUTPUT)} // is there a register that we can allocate to this inst? (per inst granularity)

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
   // TODO there are TWO free-list IOs now, based on constants. What is the best way to handle these two designs? perhaps freelist.scala, and instantiate which-ever one I want?
   // TODO naming is inconsistent 
   // TODO combine with rollback, whatever?
   val flush_pipeline = Bool(INPUT)
   val commit_wens    = Vec.fill(pl_width) {Bool(INPUT)}
   val commit_pdsts   = Vec.fill(pl_width) {UInt(INPUT, log2Up(num_phys_registers))} // remove from ISPR list

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
                    , zero_is_zero: Boolean  // is the zero register always zero? TRUE for XPRs, FALSE for FPRs. TODO not fully supported (since we hide behind P0 in this logic)
                     ) extends Module
{
   val io = new FreeListIo(num_phys_registers, pl_width)

   // ** FREE LIST TABLE ** //
   val free_list = Reg(init=(~Bits(1,num_phys_registers)))

   // track all allocations that have occurred since branch passed by
   // can quickly reset pipeline on branch mispredict
   val allocation_lists = Vec.fill(MAX_BR_COUNT) { Reg(outType=Bits(width = num_phys_registers)) }

   val enq_mask = Vec.fill(pl_width) {Bits(width = num_phys_registers)} // TODO why is this a Vec? can I do this all on one bit-vector?

   // ------------------------------------------
   // find new,free physical registers

   val requested_pregs_oh_array = Array.fill(pl_width,num_phys_registers){Bool(false)}
   val requested_pregs_oh       = Vec.fill(pl_width){Bits(width=num_phys_registers)}
   val requested_pregs          = Vec.fill(pl_width){UInt(width=log2Up(num_phys_registers))}
   var allocated                = Vec.fill(pl_width){Bool()} // did each inst get allocated a register?

   // init
   for (w <- 0 until pl_width)
   {
      allocated(w) := Bool(false)
   }


   for (i <- 1 until num_phys_registers) // zero_is_zero
   {
      val next_allocated = Vec.fill(pl_width){Bool()}
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
      requested_pregs(w) := OHToUInt(requested_pregs_oh(w))

      // TODO use OHToUInt here
      //requested_pregs(w) := UInt(0)
      //for (i <- 0 until num_phys_registers)
      //{
      //   when (requested_pregs_oh(w)(i))
      //   {
      //      requested_pregs(w) := UInt(i)
      //   }
      //}
   }


   // ------------------------------------------
   // Calculate next Free List
   val req_free_list = Bits(width = num_phys_registers)
   val enq_free_list = Bits(width = num_phys_registers)
   req_free_list := free_list
   enq_free_list := free_list
   
   // ** Set requested PREG to "Not Free" ** //

   // bit vector of newly allocated physical registers
   var just_allocated_mask = Bits(0, num_phys_registers)

   // track which allocation_lists just got cleared out by a branch,
   // to enforce a write priority to allocation_lists()
   val br_cleared = Vec.fill(MAX_BR_COUNT) { Bool() }
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
   val allocation_list = Bits(width = num_phys_registers)
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
   // "inflight speculative physical registers" list track registers that are
   // given out speculatively, and remove them once their instruction is
   // committed. If a branch mispredicts, use the branch's "allocation_list" to
   // clear out those registers.
   if (ENABLE_COMMIT_MAP_TABLE)
   {
      val ispr_list= Reg(init=Bits(0,num_phys_registers))

      val com_mask = Vec.fill(pl_width) {Bits(width=num_phys_registers)}
      for (w <- 0 until pl_width)
      {
         com_mask(w) := Bits(0,width=num_phys_registers)  
         when (io.commit_wens(w))
         {
            com_mask(w) := UInt(1) << io.commit_pdsts(w)
         }
      }

      when (io.flush_pipeline)
      {
         ispr_list := Bits(0)
      }
      .elsewhen (io.br_mispredict_val)
      {
         ispr_list := ~allocation_list & ispr_list & ~(com_mask.reduce(_|_))
      }
      .otherwise
      {
         ispr_list := (ispr_list & ~(com_mask.reduce(_|_))) | just_allocated_mask
      }


      when (io.flush_pipeline)
      {
         free_list := free_list | ispr_list
      }
      io.debug.isprlist := ispr_list
   }


   
   // ** SET OUTPUTS ** //
   io.req_pregs := requested_pregs

   io.can_allocate := allocated

   io.debug.freelist := free_list
}


//-------------------------------------------------------------
//-------------------------------------------------------------

class BTReadPortIn extends Bundle
{
   val prs1        = UInt(width = PREG_SZ)
   val prs2        = UInt(width = PREG_SZ)
}
class BTReadPortOut extends Bundle
{
   val prs1_busy   = Bool()
   val prs2_busy   = Bool()
}

class BTWritePort extends Bundle
{
   // "New" PReg, which is being dispatched and thus will be busy
   val valid   = Bool()
   val pdst    = UInt(PREG_SZ)
//   val new_br_mask = Bits(MAX_BR_COUNT) TODO do I need to handle clearing/resetting on speculative instructions?
}

// internally bypasses newly busy registers (.write) to the read ports (.read)
class BusyTableIo(pipeline_width: Int, num_wb_ports: Int) extends Bundle()
{
   // reading out the busy bits
   val read_in  = Vec.fill(pipeline_width) { new BTReadPortIn().asInput }
   val read_out = Vec.fill(pipeline_width) { new BTReadPortOut().asOutput }

   // marking new registers as being busy
   val write_valid = Vec.fill(pipeline_width) { Bool(INPUT) }
   val write_pdst  = Vec.fill(pipeline_width) { UInt(INPUT, PREG_SZ) }

   // "Old" PReg, which is being written back, and thus will be !Busy
   val old_valids  = Vec.fill(num_wb_ports) { Bool().asInput }
   val old_pdsts = Vec.fill(num_wb_ports) { UInt(width = PREG_SZ).asInput }
   
   val debug = new Bundle 
   {
      val bsy_table= UInt(OUTPUT, width=PHYS_REG_COUNT)
   }
}

// Register P0 is always NOT_BUSY, and cannot be set to BUSY
// Note: I do NOT bypass from newly busied registers to the read ports.
// That bypass check should be done elsewhere (this is to get it off the
// critical path).
class BusyTable(pipeline_width: Int, num_wb_ports: Int) extends Module
{
   val io = new BusyTableIo(pipeline_width, num_wb_ports)

   def BUSY     = Bool(true)
   def NOT_BUSY = Bool(false)

   val table_bsy = Reg(init=UInt(0,PHYS_REG_COUNT))

   // write ports unbusy-ing registers TODO come up with better name than "old_*"
   // TODO need to get rid of so many write ports
   for (wb_idx <- 0 until num_wb_ports)
   {
      when (io.old_valids(wb_idx))
      {
         table_bsy(io.old_pdsts(wb_idx)) := NOT_BUSY
      }
   }

   for (w <- 0 until pipeline_width)
   {
      when (io.write_valid(w) && io.write_pdst(w) != UInt(0))
      {
         table_bsy(io.write_pdst(w)) := BUSY
      }
   }


   for (w <- 0 until pipeline_width)
   {
      // handle bypassing a clearing of the busy-bit
      var prs1_just_cleared = Bool(false)
      var prs2_just_cleared = Bool(false)

      for (i <- 0 until num_wb_ports)
      {
         prs1_just_cleared = (io.old_valids(i) && (io.old_pdsts(i) === io.read_in(w).prs1)) | prs1_just_cleared
         prs2_just_cleared = (io.old_valids(i) && (io.old_pdsts(i) === io.read_in(w).prs2)) | prs2_just_cleared
      }

      // note: no bypassing of the newly busied (that is done outside this module)  
      io.read_out(w).prs1_busy := (table_bsy(io.read_in(w).prs1) && !prs1_just_cleared) 
      io.read_out(w).prs2_busy := (table_bsy(io.read_in(w).prs2) && !prs2_just_cleared) 
   }

   // debug
   io.debug.bsy_table := table_bsy
}
 
//-------------------------------------------------------------                 
//-------------------------------------------------------------                 
//-------------------------------------------------------------                 
//-------------------------------------------------------------                 
 
class RenameStageIO(pl_width: Int, num_wb_ports: Int) extends Bundle
{
   val ren_mask  = Vec.fill(pl_width) {Bool().asOutput} // mask of valid instructions
   val inst_can_proceed = Vec.fill(pl_width) {Bool(OUTPUT)}

   val kill      = Bool(INPUT)

   val dec_mask  = Vec.fill(pl_width){ Bool().asInput } 

   val dec_uops  = Vec.fill(pl_width) {new MicroOp().asInput}
   val ren_uops  = Vec.fill(pl_width) {new MicroOp().asOutput}

   val brinfo    = new BrResolutionInfo().asInput

   val dis_inst_can_proceed = Vec.fill(DISPATCH_WIDTH) {Bool(INPUT)}

   // issue stage (fast wakeup)
   val wb_valids = Vec.fill(num_wb_ports) {Bool(INPUT)}
   val wb_pdsts  = Vec.fill(num_wb_ports) {UInt(INPUT, PREG_SZ)}

   // commit stage
   val com_valids = Vec.fill(pl_width) {Bool(INPUT)}
   val com_uops   = Vec.fill(pl_width) {new MicroOp().asInput}
   val com_rbk_valids = Vec.fill(pl_width) {Bool(INPUT)}

   val flush_pipeline = Bool(INPUT) // TODO only used for SCR (single-cycle reset)

   // debug
   val debug = new Bundle {
      val freelist = Bits(width=PHYS_REG_COUNT)
      val isprlist = Bits(width=PHYS_REG_COUNT)
      val map_table = Vec.fill(LOGICAL_REG_COUNT) {new Bundle{
         val valid   = Bool()
         val rbk_wen = Bool()
         val element = UInt()
         val committed_element = UInt()
      }}
      val bsy_table = UInt(width=PHYS_REG_COUNT)
   }.asOutput
}


class RenameStage(pl_width: Int, num_wb_ports: Int) extends Module
{
   val io = new RenameStageIO(pl_width, num_wb_ports)

   val ren_br_vals = Vec.fill(pl_width) { Bool() }
   val freelist_can_allocate = Vec.fill(pl_width) { Bool() }

   //-------------------------------------------------------------
   // Set outputs up... we'll write in the pop*/pdst info below
   for (w <- 0 until pl_width)
   {

      io.ren_mask(w) := io.dec_mask(w) && io.inst_can_proceed(w) && !io.kill

      io.ren_uops(w)         := io.dec_uops(w)
      io.ren_uops(w).br_mask := GetNewBrMask(io.brinfo, io.dec_uops(w))

      ren_br_vals(w)       := Mux(io.dec_mask(w), io.dec_uops(w).is_br_or_jmp && !io.dec_uops(w).is_jal, Bool(false))
      // TODO create a bit "allocates_br_msk" instead
                            
      io.ren_uops(w).pdst_rtype := io.dec_uops(w).ldst_rtype
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
         map_table_io(i).wens(w)          :=   io.ren_uops(w).ldst === UInt(i) &&
                                               io.ren_mask(w) &&
                                               io.ren_uops(w).ldst_val &&
                                               io.ren_uops(w).ldst_rtype === RT_FIX &&
                                               !io.kill &&
                                               io.inst_can_proceed(w) && 
                                               freelist_can_allocate(w)
         map_table_io(i).ren_pdsts(w)     := io.ren_uops(w).pdst
         
         map_table_io(i).ren_br_vals(w)   := ren_br_vals(w)
         map_table_io(i).ren_br_tags(w)   := io.ren_uops(w).br_tag
      }

      map_table_io(i).br_mispredict       := io.brinfo.mispredict
      map_table_io(i).br_mispredict_tag   := io.brinfo.tag

      map_table_io(i).flush_pipeline      := io.flush_pipeline
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
         when (io.com_valids(w) && io.com_uops(w).pdst_rtype === RT_FIX)
         {
            map_table_io(ldst).commit_wen := Bool(true)
            map_table_io(ldst).commit_pdst := io.com_uops(w).pdst
         }
      }
   }

   // read out the map-table entries ASAP, then deal with bypassing busy-bits and actually prs1/prs2 later
   val map_table_output = Vec.fill(pl_width) {new Bundle{val prs1=UInt(); val prs2=UInt();}}
   for (w <- 0 until pl_width)
   {
      map_table_output(w).prs1 := map_table_io(io.ren_uops(w).lrs1).element
      map_table_output(w).prs2 := map_table_io(io.ren_uops(w).lrs2).element
   }

   // Bypass the physical register mappings
   val prs1_was_bypassed = Vec.fill(pl_width) {Bool()}
   val prs2_was_bypassed = Vec.fill(pl_width) {Bool()}

   for (w <- 0 until pl_width)
   {
      var rs1_cases =  Array((Bool(false),  UInt(0,PREG_SZ)))
      var rs2_cases =  Array((Bool(false),  UInt(0,PREG_SZ)))
      var stale_cases= Array((Bool(false),  UInt(0,PREG_SZ)))

      prs1_was_bypassed(w) := Bool(false)
      prs2_was_bypassed(w) := Bool(false)

      // Handle bypassing new physical destinations to operands (and stale destination)
      for (xx <- 0 until w)
      {
         rs1_cases  ++= Array(((io.ren_uops(w).lrs1_rtype === RT_FIX) && io.ren_mask(xx) && io.ren_uops(xx).ldst_val && (io.ren_uops(w).lrs1 === io.ren_uops(xx).ldst), (io.ren_uops(xx).pdst)))
         rs2_cases  ++= Array(((io.ren_uops(w).lrs2_rtype === RT_FIX) && io.ren_mask(xx) && io.ren_uops(xx).ldst_val && (io.ren_uops(w).lrs2 === io.ren_uops(xx).ldst), (io.ren_uops(xx).pdst)))
         stale_cases++= Array(( io.ren_uops(w).ldst_val               && io.ren_mask(xx) && io.ren_uops(xx).ldst_val && (io.ren_uops(w).ldst === io.ren_uops(xx).ldst), (io.ren_uops(xx).pdst)))

         when ((io.ren_uops(w).lrs1_rtype === RT_FIX) && io.ren_mask(xx) && io.ren_uops(xx).ldst_val && (io.ren_uops(w).lrs1 === io.ren_uops(xx).ldst))
            { prs1_was_bypassed(w) := Bool(true) }
         when ((io.ren_uops(w).lrs2_rtype === RT_FIX) && io.ren_mask(xx) && io.ren_uops(xx).ldst_val && (io.ren_uops(w).lrs2 === io.ren_uops(xx).ldst))
            { prs2_was_bypassed(w) := Bool(true) }
      }

      // add default case where we can just read the map table for our information
      rs1_cases   ++= Array(((io.ren_uops(w).lrs1_rtype === RT_FIX) && (io.ren_uops(w).lrs1 != UInt(0)), map_table_output(w).prs1))
      rs2_cases   ++= Array(((io.ren_uops(w).lrs2_rtype === RT_FIX) && (io.ren_uops(w).lrs2 != UInt(0)), map_table_output(w).prs2))

      io.ren_uops(w).pop1       := MuxCase(io.ren_uops(w).lrs1, rs1_cases)
      io.ren_uops(w).pop2       := MuxCase(io.ren_uops(w).lrs2, rs2_cases)
      io.ren_uops(w).stale_pdst := MuxCase(map_table_io(io.ren_uops(w).ldst).element, stale_cases)
   }


   //-------------------------------------------------------------
   // Busy Table (For Fixed Point GPRs)

   val freelist_req_pregs = Vec.fill(pl_width) { UInt(width = PREG_SZ) }

   // 3 WB ports for now
   // 1st is back-to-back bypassable ALU ops
   // 2nd is memory/muldiv
   // TODO 3rd is ALU ops that aren't bypassable... can maybe remove this? or set TTL countdown on 1st port?
   // TODO optimize - too many write ports, but how to deal with that? (slow + fast...)
   val bsy_table = Module(new BusyTable(pipeline_width = pl_width, num_wb_ports = num_wb_ports))

      for (w <- 0 until pl_width)
      {
         // Reading the Busy Bits
         // for critical path reasons, we speculatively read out the busy-bits assuming no dependencies between uops
         // then verify if the uop actually uses a register and if it depends on a newly unfreed register
         bsy_table.io.read_in(w).prs1  := map_table_output(w).prs1
         bsy_table.io.read_in(w).prs2  := map_table_output(w).prs2

         io.ren_uops(w).prs1_busy := io.ren_uops(w).lrs1_rtype === RT_FIX &&
                                       (bsy_table.io.read_out(w).prs1_busy || prs1_was_bypassed(w))
         io.ren_uops(w).prs2_busy := io.ren_uops(w).lrs2_rtype === RT_FIX &&
                                       (bsy_table.io.read_out(w).prs2_busy || prs2_was_bypassed(w))


          // Updating the Table (new busy register)
         bsy_table.io.write_valid(w) := freelist_can_allocate(w) &&
                                        io.ren_mask(w) && 
                                        io.ren_uops(w).ldst_val &&
                                        (io.ren_uops(w).ldst_rtype === RT_FIX)
         bsy_table.io.write_pdst(w) := freelist_req_pregs(w)
      }
 
      // Clear Busy-bit
      bsy_table.io.old_valids := io.wb_valids
      bsy_table.io.old_pdsts := io.wb_pdsts


   //-------------------------------------------------------------
   // Free List (Fixed Point)

   val freelist = Module(new RenameFreeList(PHYS_REG_COUNT, pl_width, zero_is_zero = true))

      for (w <- 0 until pl_width)
      {
         freelist.io.req_preg_vals(w) := (io.inst_can_proceed(w) &&
                                         !io.kill &&
                                         io.ren_mask(w) && 
                                         io.ren_uops(w).ldst_val &&
                                         (io.ren_uops(w).ldst_rtype === RT_FIX))
      }
      freelist_req_pregs := freelist.io.req_pregs

      for (w <- 0 until pl_width)
      {
         freelist.io.enq_vals(w)    := io.com_valids(w) &&
                                       (io.com_uops(w).pdst_rtype === RT_FIX) && (io.com_uops(w).stale_pdst != UInt(0))
         freelist.io.enq_pregs(w)   := io.com_uops(w).stale_pdst
                
         freelist.io.ren_br_vals(w) := ren_br_vals(w)
         freelist.io.ren_br_tags(w) := io.ren_uops(w).br_tag

         freelist_can_allocate(w)   := freelist.io.can_allocate(w)

         freelist.io.rollback_wens(w)  := io.com_rbk_valids(w) && 
                                        (io.com_uops(w).pdst != UInt(0)) &&
                                        (io.com_uops(w).pdst_rtype === RT_FIX)
         freelist.io.rollback_pdsts(w) := io.com_uops(w).pdst

         freelist.io.commit_wens(w) := io.com_valids(w) &&
                                       (io.com_uops(w).pdst != UInt(0)) &&
                                       (io.com_uops(w).pdst_rtype === RT_FIX)
         freelist.io.commit_pdsts(w) := io.com_uops(w).pdst
      }

      freelist.io.br_mispredict_val := io.brinfo.mispredict
      freelist.io.br_mispredict_tag := io.brinfo.tag

      freelist.io.flush_pipeline := io.flush_pipeline


   // for some instructions, pass through the logical destination as the physical destination
   // (admittedly, not sure which ones anymore. MTPCR has been removed)
   for (w <- 0 until pl_width)
   {
      io.ren_uops(w).pdst := Mux((io.ren_uops(w).ldst_val && (io.ren_uops(w).ldst_rtype === RT_FIX)),
                                                                              freelist_req_pregs(w),
                                                                              io.ren_uops(w).ldst)
   }



   //-------------------------------------------------------------
   // Outputs
   for (w <- 0 until pl_width)
   {
      io.inst_can_proceed(w) := ((freelist.io.can_allocate(w) && io.ren_uops(w).ldst_rtype === RT_FIX) || io.ren_uops(w).ldst_rtype != RT_FIX) && io.dis_inst_can_proceed(w)
   }


   //-------------------------------------------------------------
   // Debug signals

   io.debug.freelist := freelist.io.debug.freelist
   io.debug.isprlist := freelist.io.debug.isprlist

   for (i <- 0 until LOGICAL_REG_COUNT)
   {
      io.debug.map_table(i).rbk_wen := map_table_io(i).rollback_wen
      io.debug.map_table(i).element := map_table_io(i).element
      io.debug.map_table(i).committed_element := map_table_io(i).committed_element
   }
   io.debug.bsy_table:= bsy_table.io.debug.bsy_table
}



}

