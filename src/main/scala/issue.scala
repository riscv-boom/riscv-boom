//**************************************************************************
// RISCV Processor Issue Logic
//--------------------------------------------------------------------------
//

package BOOM
{
import Chisel._
import Node._

import FUCode._

import scala.collection.mutable.ArrayBuffer

//-------------------------------------------------------------
// Entry Slot in the Issue Station

class IntegerIssueSlotIo(num_wakeup_ports: Int) extends BOOMCoreBundle
{
   val id_num         = UInt(INPUT)

   val valid          = Bool(OUTPUT)
   val request        = Bool(OUTPUT)
   val issue          = Bool(INPUT)

   val request_hp     = Bool(OUTPUT) // high priority request

   val kill           = Bool(INPUT)

   val brinfo         = new BrResolutionInfo().asInput()

   // slow wakeup for uops with variable, unknown latencies (cache misses, div, etc.)
   val wakeup_vals    = Vec.fill(num_wakeup_ports) { Bool(INPUT) }
   val wakeup_dsts    = Vec.fill(num_wakeup_ports) { UInt(INPUT, PREG_SZ) }

   val in_wen         = Bool(INPUT)
   val is_2nd_uop     = Bool(INPUT)
   val inUop          = new MicroOp().asInput()

   val outUop         = new MicroOp().asOutput()

   val debug = new Bundle {
      val p1 = Bool()
      val p2 = Bool()
      val p3 = Bool()
   }.asOutput
}

class IntegerIssueSlot(num_slow_wakeup_ports: Int) extends Module with BOOMCoreParameters
{
   val io = new IntegerIssueSlotIo(num_slow_wakeup_ports)

   val next_p1 = Bool()
   val next_p2 = Bool()
   val next_p3 = Bool()

   val slot_valid    = Reg(init = Bool(false))
   val slot_p1       = Reg(init = Bool(false), next = next_p1)
   val slot_p2       = Reg(init = Bool(false), next = next_p2)
   val slot_p3       = Reg(init = Bool(false), next = next_p3)

   val slotUop = Reg(init = NullMicroOp)

   when (io.kill || io.issue)
   {
      slot_valid   := Bool(false)
   }
   .elsewhen (io.in_wen)
   {
      slot_valid := Bool(true)
      slotUop    := io.inUop

      // special case breaking up insts into micro-ops
      // for now, only stores/amos supported
      when (io.inUop.uopc === uopSTA || io.inUop.uopc === uopAMO_AG)
      {
         when (io.is_2nd_uop)
         {
            slotUop.uopc := uopSTD
            slotUop.lrs1_rtype := RT_X
            slotUop.lrs2_rtype := RT_FIX
         }
         .otherwise
         {
            slotUop.lrs1_rtype := RT_FIX
            slotUop.lrs2_rtype := RT_X
         }
      }
   }


   // Wakeup Compare Logic
   next_p1 := Bool(false)
   next_p2 := Bool(false)
   next_p3 := Bool(false)

   when (io.in_wen)
   {
      next_p1 := !(io.inUop.prs1_busy)
      next_p2 := !(io.inUop.prs2_busy)
      next_p3 := !(io.inUop.prs3_busy)

      // only for stores for now..
      when (io.inUop.uopc === uopSTA || io.inUop.uopc === uopAMO_AG)
      {
         when (io.is_2nd_uop)
         {
            next_p1 := Bool(true)
         }
         .otherwise
         {
            next_p2 := Bool(true)
         }
      }
   }
   .otherwise
   {
      // slot is valid...
      next_p1 := slot_p1
      next_p2 := slot_p2
      next_p3 := slot_p3

      for (i <- 0 until num_slow_wakeup_ports)
      {
         when (io.wakeup_vals(i) && (io.wakeup_dsts(i) === slotUop.pop1))
         {
            next_p1 := Bool(true)
         }
         when (io.wakeup_vals(i) && (io.wakeup_dsts(i) === slotUop.pop2))
         {
            next_p2 := Bool(true)
         }
         when (io.wakeup_vals(i) && (io.wakeup_dsts(i) === slotUop.pop3))
         {
            next_p3 := Bool(true)
         }
      }
   }


   // Handle branch misspeculations
   when (IsKilledByBranch(io.brinfo, slotUop))
   {
      slot_valid   := Bool(false)
   }
   when (!io.in_wen)
   {
      slotUop.br_mask := GetNewBrMask(io.brinfo, slotUop)
   }

   //-------------------------------------------------------------
   // High Priority Request
   // Allow the issue window to demand a "high priority" request.
   // The issue-select logic will consider high priority requests first.

   // Hi 152 students! You should only need to modify code in here!
   //
   // - "slot_valid" is a signal that is high when the slot uop is valid.
   // - "slotUop" contains the micro-op in this slot.
   // - The signal "io.in_wen" is high when a new uop is being written into the slot.
   // - The signal "io.request" is high when the uop is requesting to be issued.
   // - The signal "io.issue" is high when the uop is being issued.

   val high_priority = Bool()

   high_priority := Bool(false)
//   high_priority := slotUop.is_br_or_jmp // <<-- is the uop a branch or jmp instruction?


   //-------------------------------------------------------------
   // Request Logic
   io.request    := slot_valid && slot_p1 && slot_p2 && slot_p3 && !io.kill
   io.request_hp := io.request && high_priority

   //assign outputs
   io.valid    := slot_valid
   io.outUop   := slotUop

   // debug outputs
   io.debug.p1 := slot_p1
   io.debug.p2 := slot_p2
   io.debug.p3 := slot_p3
}

//-------------------------------------------------------------
//-------------------------------------------------------------



class IssueUnitIO(issue_width: Int, num_wakeup_ports: Int) extends BOOMCoreBundle
{
   // TODO pass in as "implicit conf" object
   val dis_mask  = Vec.fill(DISPATCH_WIDTH) { Bool(INPUT) }
   val dis_uops  = Vec.fill(DISPATCH_WIDTH) { new MicroOp().asInput() }
   val dis_inst_can_proceed = Vec.fill(DISPATCH_WIDTH) { Bool(OUTPUT) }

   val iss_valids = Vec.fill(issue_width) { Bool(OUTPUT) }
   val iss_uops   = Vec.fill(issue_width) { new MicroOp().asOutput() }

   // tell the issue unit what each execution pipeline has in terms of functional units
   val fu_types   = Vec.fill(issue_width) { Bits(INPUT, FUC_SZ) }

   val brinfo = new BrResolutionInfo().asInput
   val flush_pipeline = Bool(INPUT)

   // Wakeup
   // val wakeup = Vec.fill(...) { new WakeUp.asInput() }
   val wakeup_vals = Vec.fill(num_wakeup_ports) { Bool(INPUT) }
   val wakeup_pdsts = Vec.fill(num_wakeup_ports) { UInt(INPUT, PREG_SZ) }

   val debug = new BOOMCoreBundle
   {
      val slot = Vec.fill(INTEGER_ISSUE_SLOT_COUNT) { new Bundle {
         val valid   = Bool()
         val uop     = new MicroOp()
         val request = Bool()
         val issue   = Bool()
         val in_wen  = Bool()
         val p1      = Bool()
         val p2      = Bool()
         val p3      = Bool()
      }}
   }.asOutput
}

class IssueUnit(issue_width: Int, num_wakeup_ports: Int) extends Module with BOOMCoreParameters
{
   val io = new IssueUnitIO(issue_width, num_wakeup_ports)

   val iss_valid      = Bool()


   val nullUop = NullMicroOp
   nullUop.pdst := UInt(0) // TODO what do I need here? maybe not this one.
   nullUop.pop1 := UInt(0)
   nullUop.pop2 := UInt(0)
   nullUop.pop3 := UInt(0)
   nullUop.dst_rtype := RT_X
   nullUop.lrs1_rtype := RT_X
   nullUop.lrs2_rtype := RT_X


   //-------------------------------------------------------------
   // Issue Table

   val issue_slot_io = Vec.fill(INTEGER_ISSUE_SLOT_COUNT) { Module(new IntegerIssueSlot(num_wakeup_ports)).io }

   // double width, since some instructions break into two micro-ops
   val entry_wen_oh  = Vec.fill(INTEGER_ISSUE_SLOT_COUNT){ Bits(width=DISPATCH_WIDTH) }
   val entry_wen_2nd = Vec.fill(INTEGER_ISSUE_SLOT_COUNT){Bool()}


   for (i <- 0 until INTEGER_ISSUE_SLOT_COUNT)
   {
      issue_slot_io(i).id_num := UInt(i)

      issue_slot_io(i).in_wen := entry_wen_oh(i).orR
      issue_slot_io(i).inUop  := Mux1H(entry_wen_oh(i), io.dis_uops)
      issue_slot_io(i).is_2nd_uop := entry_wen_2nd(i)

      for (w <- 0 until num_wakeup_ports)
      {
         issue_slot_io(i).wakeup_vals(w) := io.wakeup_vals(w)
         issue_slot_io(i).wakeup_dsts(w) := io.wakeup_pdsts(w)
      }

      issue_slot_io(i).brinfo := io.brinfo
      issue_slot_io(i).kill   := io.flush_pipeline
   }

   //-------------------------------------------------------------
   // Dispatch/Entry Logic
   // find a slot to enter a new dispatched instruction

   val entry_wen_oh_array = Array.fill(INTEGER_ISSUE_SLOT_COUNT,2*DISPATCH_WIDTH){Bool(false)}
   var allocated = Vec.fill(2*DISPATCH_WIDTH){Bool(false)} // did an instruction find an issue width?


   for (i <- 0 until INTEGER_ISSUE_SLOT_COUNT)
   {
      var next_allocated = Vec.fill(2*DISPATCH_WIDTH){Bool()}
      var can_allocate = !(issue_slot_io(i).valid)

      for (w <- 0 until 2*DISPATCH_WIDTH)
      {
         entry_wen_oh_array(i)(w) = can_allocate && !(allocated(w))

         next_allocated(w) := can_allocate | allocated(w)
//         next_allocated(w) = can_allocate | allocated(w)
         can_allocate = can_allocate && allocated(w)
      }

      allocated = next_allocated
   }


   // if we can find an issue slot, do we actually need it?
   // also, translate from Scala data structures to Chisel Vecs
   for (i <- 0 until INTEGER_ISSUE_SLOT_COUNT)
   {
      val temp_1stuop_val = Vec.fill(DISPATCH_WIDTH){Bool()}
      val temp_2nduop_val = Vec.fill(DISPATCH_WIDTH){Bool()}

      for (w <- 0 until DISPATCH_WIDTH)
      {
         // TODO add ctrl bit for "allocates iss_slot"
         temp_1stuop_val (w) := io.dis_mask(w) &&
                                 !io.dis_uops(w).exception &&
                                 !io.dis_uops(w).sret &&
                                 !io.dis_uops(w).is_fence &&
                                 !io.dis_uops(w).is_fencei &&
                                 entry_wen_oh_array(i)(2*w)

         temp_2nduop_val(w)  := io.dis_mask(w) &&
                                 (io.dis_uops(w).uopc === uopSTA || io.dis_uops(w).uopc === uopAMO_AG) &&
                                 !io.dis_uops(w).exception &&
                                 entry_wen_oh_array(i)(2*w+1)

      }
      entry_wen_oh(i) := temp_1stuop_val.toBits | temp_2nduop_val.toBits
      entry_wen_2nd(i) := (temp_2nduop_val).reduce(_|_)
   }

   for (w <- 0 until DISPATCH_WIDTH)
   {
      // TODO don't require everything to be allocated, when we aren't a store
      io.dis_inst_can_proceed(w) := allocated(2*w) && allocated(2*w+1)
   }

   //-------------------------------------------------------------
   // Issue Select Logic

//   val requests:Bits = null
//   val requests = (Vec(
//                     Vec.tabulate(INTEGER_ISSUE_SLOT_COUNT)(i => issue_slot_io(i).request_hp) ++
//                     Vec.tabulate(INTEGER_ISSUE_SLOT_COUNT)(i => issue_slot_io(i).request)
//                     )).toBits

   val num_requestors = INTEGER_ISSUE_SLOT_COUNT

   for (w <- 0 until issue_width)
   {
      io.iss_valids(w) := Bool(false)
      io.iss_uops(w)   := nullUop
   }

   // TODO can we use flatten to get an array of bools on issue_slot(*).request?
   val lo_request_not_satisfied = Array.fill(num_requestors){Bool()}
   val hi_request_not_satisfied = Array.fill(num_requestors){Bool()}

   for (i <- 0 until num_requestors)
   {
      lo_request_not_satisfied(i) = issue_slot_io(i).request
      hi_request_not_satisfied(i) = issue_slot_io(i).request_hp
      issue_slot_io(i).issue := Bool(false) // default
   }


   for (w <- 0 until issue_width)
   {
      var port_issued = Bool(false)

      // first look for high priority requests
      for (i <- 0 until num_requestors)
      {
         val can_allocate = (issue_slot_io(i).outUop.fu_code & io.fu_types(w)) != Bits(0)

         when (hi_request_not_satisfied(i) && can_allocate && !port_issued)
         {
            issue_slot_io(i).issue := Bool(true)
            io.iss_valids(w)       := Bool(true)
            io.iss_uops(w)         := issue_slot_io(i).outUop
         }

         val port_already_in_use     = port_issued
         port_issued                 = (hi_request_not_satisfied(i) && can_allocate) | port_issued
         // deassert lo_request if hi_request is 1.
         lo_request_not_satisfied(i) = (lo_request_not_satisfied(i) && !hi_request_not_satisfied(i))
         // if request is 0, stay 0. only stay 1 if request is true and can't allocate
         hi_request_not_satisfied(i) = (hi_request_not_satisfied(i) && (!can_allocate || port_already_in_use))
      }

      // now look for low priority requests
      for (i <- 0 until num_requestors)
      {
         val can_allocate = (issue_slot_io(i).outUop.fu_code & io.fu_types(w)) != Bits(0)

         when (lo_request_not_satisfied(i) && can_allocate && !port_issued)
         {
            issue_slot_io(i).issue := Bool(true)
            io.iss_valids(w)       := Bool(true)
            io.iss_uops(w)         := issue_slot_io(i).outUop
         }

         val port_already_in_use     = port_issued
         port_issued                 = (lo_request_not_satisfied(i) && can_allocate) | port_issued
         // if request is 0, stay 0. only stay 1 if request is true and can't allocate or port already in use
         lo_request_not_satisfied(i) = (lo_request_not_satisfied(i) && (!can_allocate || port_already_in_use))
      }
   }


   //-------------------------------------------------------------
   // set up outputs


   //-------------------------------------------------------------
   // pass printf debug info out to higher level module

   for (i <- 0 until INTEGER_ISSUE_SLOT_COUNT)
   {
      io.debug.slot(i).valid   := issue_slot_io(i).valid
      io.debug.slot(i).uop     := issue_slot_io(i).outUop
      io.debug.slot(i).request := issue_slot_io(i).request
      io.debug.slot(i).issue   := issue_slot_io(i).issue
      io.debug.slot(i).in_wen  := issue_slot_io(i).in_wen

      io.debug.slot(i).p1 := issue_slot_io(i).debug.p1
      io.debug.slot(i).p2 := issue_slot_io(i).debug.p2
      io.debug.slot(i).p3 := issue_slot_io(i).debug.p3
   }


}

}
