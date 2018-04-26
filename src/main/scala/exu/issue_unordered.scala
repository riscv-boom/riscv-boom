//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//

package boom.exu

import Chisel._
import freechips.rocketchip.config.Parameters
import FUConstants._
import freechips.rocketchip.util.Str
import scala.collection.mutable.ArrayBuffer
import boom.common._

class IssueUnitStatic(
   params: IssueParams,
   num_wakeup_ports: Int)
   (implicit p: Parameters)
   extends IssueUnit(params.numEntries, params.issueWidth, num_wakeup_ports, params.iqType)
{
   //-------------------------------------------------------------
   // Issue Table

   val entry_wen_oh  = Vec.fill(num_issue_slots){ Wire(Bits(width=DISPATCH_WIDTH)) }
   for (i <- 0 until num_issue_slots)
   {
      issue_slots(i).in_uop.valid := entry_wen_oh(i).orR
      issue_slots(i).in_uop.bits  := Mux1H(entry_wen_oh(i), dis_uops)
      issue_slots(i).wakeup_dsts  := io.wakeup_pdsts
      issue_slots(i).ldspec_dst   := io.mem_ldSpecWakeup
      issue_slots(i).brinfo       := io.brinfo
      issue_slots(i).kill         := io.flush_pipeline
      issue_slots(i).clear        := false.B
   }

   //-------------------------------------------------------------
   // Dispatch/Entry Logic
   // find a slot to enter a new dispatched instruction

   val entry_wen_oh_array = Array.fill(num_issue_slots,DISPATCH_WIDTH){Bool(false)}
   var allocated = Vec.fill(DISPATCH_WIDTH){Bool(false)} // did an instruction find an issue width?


   for (i <- 0 until num_issue_slots)
   {
      var next_allocated = Wire(Vec(DISPATCH_WIDTH, Bool()))
      var can_allocate = !(issue_slots(i).valid)

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
   for (i <- 0 until num_issue_slots)
   {
      val temp_uop_val = Wire(Vec(DISPATCH_WIDTH, Bool()))

      for (w <- 0 until DISPATCH_WIDTH)
      {
         // TODO add ctrl bit for "allocates iss_slot"
         temp_uop_val (w) := io.dis_valids(w) &&
                             !dis_uops(w).exception &&
                             !dis_uops(w).is_fence &&
                             !dis_uops(w).is_fencei &&
                             entry_wen_oh_array(i)(w)
      }
      entry_wen_oh(i) := temp_uop_val.asUInt
   }

   for (w <- 0 until DISPATCH_WIDTH)
   {
      io.dis_readys(w) := allocated(w)
   }

   //-------------------------------------------------------------
   // Issue Select Logic

   val num_requestors = num_issue_slots

   for (w <- 0 until issue_width)
   {
      io.iss_valids(w) := Bool(false)
      io.iss_uops(w)   := NullMicroOp
      // unsure if this is overkill
      io.iss_uops(w).pop1 := UInt(0)
      io.iss_uops(w).pop2 := UInt(0)
      io.iss_uops(w).pop3 := UInt(0)
      io.iss_uops(w).lrs1_rtype := RT_X
      io.iss_uops(w).lrs2_rtype := RT_X
   }

   // TODO can we use flatten to get an array of bools on issue_slot(*).request?
   val lo_request_not_satisfied = Array.fill(num_requestors){Bool()}
   val hi_request_not_satisfied = Array.fill(num_requestors){Bool()}

   for (i <- 0 until num_requestors)
   {
      lo_request_not_satisfied(i) = issue_slots(i).request
      hi_request_not_satisfied(i) = issue_slots(i).request_hp
      issue_slots(i).grant := Bool(false) // default
   }


   for (w <- 0 until issue_width)
   {
      var port_issued = Bool(false)

      // first look for high priority requests
      for (i <- 0 until num_requestors)
      {
         val can_allocate = (issue_slots(i).uop.fu_code & io.fu_types(w)) =/= Bits(0)

         when (hi_request_not_satisfied(i) && can_allocate && !port_issued)
         {
            issue_slots(i).grant := Bool(true)
            io.iss_valids(w)     := Bool(true)
            io.iss_uops(w)       := issue_slots(i).uop
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
         val can_allocate = (issue_slots(i).uop.fu_code & io.fu_types(w)) =/= Bits(0)

         when (lo_request_not_satisfied(i) && can_allocate && !port_issued)
         {
            issue_slots(i).grant := Bool(true)
            io.iss_valids(w)     := Bool(true)
            io.iss_uops(w)       := issue_slots(i).uop
         }

         val port_already_in_use     = port_issued
         port_issued                 = (lo_request_not_satisfied(i) && can_allocate) | port_issued
         // if request is 0, stay 0. only stay 1 if request is true and can't allocate or port already in use
         lo_request_not_satisfied(i) = (lo_request_not_satisfied(i) && (!can_allocate || port_already_in_use))
      }
   }
}

