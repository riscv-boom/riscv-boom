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

// stores (and AMOs) are "broken down" into 2 uops, but stored within a single issue-slot.

class IssueSlotIo(num_wakeup_ports: Int) extends BOOMCoreBundle
{
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
   val inUop          = new MicroOp().asInput()

   val outUop         = new MicroOp().asOutput()

   val debug = new Bundle {
      val p1 = Bool()
      val p2 = Bool()
      val p3 = Bool()
   }.asOutput
}

class IssueSlot(num_slow_wakeup_ports: Int) extends Module with BOOMCoreParameters
{
   val io = new IssueSlotIo(num_slow_wakeup_ports)

   // slot invalid?
   // slot is valid, holding 1 uop
   // slot is valid, holds 2 uops (like a store)
   val s_invalid :: s_valid_1 :: s_valid_2 :: Nil = Enum(UInt(),3)
   def isInvalid = slot_state === s_invalid
   def isValid = slot_state != s_invalid

   val next_p1 = Bool()
   val next_p2 = Bool()
   val next_p3 = Bool()

   val slot_state    = Reg(init = s_invalid)
   val slot_p1       = Reg(init = Bool(false), next = next_p1)
   val slot_p2       = Reg(init = Bool(false), next = next_p2)
   val slot_p3       = Reg(init = Bool(false), next = next_p3)
   val slot_is_2uops = Reg(Bool())

   val slotUop = Reg(init = NullMicroOp)

   when (io.kill || (io.issue && (slot_state === s_valid_1)) ||
                    (io.issue && (slot_state === s_valid_2) && slot_p1 && slot_p2))
   {
      slot_state := s_invalid
   }
   .elsewhen (io.issue && (slot_state === s_valid_2))
   {
      slot_state := s_valid_1
      when (slot_p1)
      {
         slotUop.uopc := uopSTD
         slotUop.lrs1_rtype := RT_X
      }
      .otherwise
      {
         slotUop.lrs2_rtype := RT_X
      }
   }
   .elsewhen (io.in_wen)
   {
      slot_state := s_valid_1
      slotUop    := io.inUop

      // special case "storing" 2 uops within one issue slot
      // for now, only stores/amos supported
      when (io.inUop.uopc === uopSTA || io.inUop.uopc === uopAMO_AG)
      {
         slot_state := s_valid_2
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
   }
   .otherwise
   {
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
      slot_state   := s_invalid
   }
   when (!io.in_wen)
   {
      slotUop.br_mask := GetNewBrMask(io.brinfo, slotUop)
   }

   //-------------------------------------------------------------
   // High Priority Request
   // Allow the issue window to demand a "high priority" request.
   // The issue-select logic will consider high priority requests first.

   val high_priority = Bool()

//   high_priority := Bool(false)
   high_priority := slotUop.is_br_or_jmp // <<-- is the uop a branch or jmp instruction?


   //-------------------------------------------------------------
   // Request Logic
   io.request    := (slot_state === s_valid_1 || slot_state === s_valid_2) &&
                     slot_p1 && slot_p2 && slot_p3 && !io.kill

   when (slot_state === s_valid_1)
   {
      io.request := slot_p1 && slot_p2 && slot_p3 && !io.kill
   }
   .elsewhen (slot_state === s_valid_2)
   {
      io.request := (slot_p1 || slot_p2)  && !io.kill
   }
   .otherwise
   {
      io.request := Bool(false)
   }


   //assign outputs
   io.valid      := isValid
   io.outUop     := slotUop
   io.request_hp := io.request && high_priority
   when (slot_state === s_valid_2)
   {
      when (slot_p1 && slot_p2)
      {
         ; // send out the entire instruction as one uop
      }
      .elsewhen (slot_p1)
      {
         io.outUop.uopc := slotUop.uopc
         io.outUop.lrs2_rtype := RT_X
      }
      .elsewhen (slot_p2)
      {
         io.outUop.uopc := uopSTD
         io.outUop.lrs1_rtype := RT_X
      }
   }

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
      val slot = Vec.fill(ISSUE_SLOT_COUNT) { new Bundle {
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
   nullUop.pdst := UInt(0)
   nullUop.pop1 := UInt(0)
   nullUop.pop2 := UInt(0)
   nullUop.pop3 := UInt(0)
   nullUop.dst_rtype := RT_X
   nullUop.lrs1_rtype := RT_X
   nullUop.lrs2_rtype := RT_X


   //-------------------------------------------------------------
   // Issue Table

   val issue_slot_io = Vec.fill(ISSUE_SLOT_COUNT) { Module(new IssueSlot(num_wakeup_ports)).io }

   val entry_wen_oh  = Vec.fill(ISSUE_SLOT_COUNT){ Bits(width=DISPATCH_WIDTH) }


   for (i <- 0 until ISSUE_SLOT_COUNT)
   {
      issue_slot_io(i).in_wen := entry_wen_oh(i).orR
      issue_slot_io(i).inUop  := Mux1H(entry_wen_oh(i), io.dis_uops)

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

   val entry_wen_oh_array = Array.fill(ISSUE_SLOT_COUNT,DISPATCH_WIDTH){Bool(false)}
   var allocated = Vec.fill(DISPATCH_WIDTH){Bool(false)} // did an instruction find an issue width?


   for (i <- 0 until ISSUE_SLOT_COUNT)
   {
      var next_allocated = Vec.fill(DISPATCH_WIDTH){Bool()}
      var can_allocate = !(issue_slot_io(i).valid)

      for (w <- 0 until DISPATCH_WIDTH)
      {
         entry_wen_oh_array(i)(w) = can_allocate && !(allocated(w))

         next_allocated(w) := can_allocate | allocated(w)
         can_allocate = can_allocate && allocated(w)
      }

      allocated = next_allocated
   }


   // if we can find an issue slot, do we actually need it?
   // also, translate from Scala data structures to Chisel Vecs
   for (i <- 0 until ISSUE_SLOT_COUNT)
   {
      val temp_uop_val = Vec.fill(DISPATCH_WIDTH){Bool()}

      for (w <- 0 until DISPATCH_WIDTH)
      {
         // TODO add ctrl bit for "allocates iss_slot"
         temp_uop_val (w) := io.dis_mask(w) &&
                             !io.dis_uops(w).exception &&
                             !io.dis_uops(w).is_fence &&
                             !io.dis_uops(w).is_fencei &&
                             entry_wen_oh_array(i)(w)
      }
      entry_wen_oh(i) := temp_uop_val.toBits
   }

   for (w <- 0 until DISPATCH_WIDTH)
   {
      io.dis_inst_can_proceed(w) := allocated(w)
   }

   //-------------------------------------------------------------
   // Issue Select Logic

//   val requests:Bits = null
//   val requests = (Vec(
//                     Vec.tabulate(ISSUE_SLOT_COUNT)(i => issue_slot_io(i).request_hp) ++
//                     Vec.tabulate(ISSUE_SLOT_COUNT)(i => issue_slot_io(i).request)
//                     )).toBits

   val num_requestors = ISSUE_SLOT_COUNT

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

   for (i <- 0 until ISSUE_SLOT_COUNT)
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
