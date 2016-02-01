//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//

package boom
{
import Chisel._
import Node._
import cde.Parameters

import FUCode._
import rocket.Str
import scala.collection.mutable.ArrayBuffer

//-------------------------------------------------------------
//-------------------------------------------------------------

class IssueUnitCollasping(num_issue_slots: Int, issue_width: Int, num_wakeup_ports: Int)(implicit p: Parameters)
   extends IssueUnit(num_issue_slots, issue_width, num_wakeup_ports)
{
   //-------------------------------------------------------------
   // Figure out how much to shift entries by

   val MAX_SHIFT = DISPATCH_WIDTH
   val shamt_oh = Array.fill(num_issue_slots){Bits(width=issue_width)}
   // count total grants before this entry, and tus how many to shift upwards by
   val shamt = Array.fill(num_issue_slots){Bits(width=log2Up(issue_width+1))}


   val vacants = issue_slots.map(s => !(s.valid)) ++ io.dis_mask.map(!_.toBool)
   val shamts_oh = Array.fill(num_issue_slots+DISPATCH_WIDTH) {Wire(Bits(width=MAX_SHIFT))}
   // track how many to shift up this entry by by counting previous vacant spots
   def SaturatingCounterOH(count_oh:Bits, inc: Bool, max: Int): Bits =
   {
      val next = Wire(Bits(width=max))
      next := count_oh
      when (count_oh === Bits(0) && inc)
      {
         next := Bits(1)
      }
      .elsewhen (!count_oh(max-1) && inc)
      {
         next := (count_oh << UInt(1))
      }
      next
   }
   shamts_oh(0) := Bits(0)
   for (i <- 1 until num_issue_slots + DISPATCH_WIDTH)
   {
      shamts_oh(i) := SaturatingCounterOH(shamts_oh(i-1), vacants(i-1), MAX_SHIFT)
   }

   //-------------------------------------------------------------

   // which entries' uops will still be next cycle? (not being issued and vacated)
   val will_be_valid = (0 until num_issue_slots).map(i => issue_slots(i).will_be_valid) ++
                       (0 until DISPATCH_WIDTH).map(i => io.dis_mask(i) &&
                                                         !io.dis_uops(i).exception &&
                                                         !io.dis_uops(i).is_fence &&
                                                         !io.dis_uops(i).is_fencei)

   val uops = issue_slots.map(s=>s.updated_uop) ++ dis_uops.map(s=>s)
   for (i <- 0 until num_issue_slots)
   {
      issue_slots(i).in_uop.valid := Bool(false)
      issue_slots(i).in_uop.bits  := uops(i+1)
      for (j <- 1 to MAX_SHIFT by 1)
      {
         when (shamts_oh(i+j) === Bits(1 << (j-1)))
         {
            issue_slots(i).in_uop.valid := will_be_valid(i+j)
            issue_slots(i).in_uop.bits  := uops(i+j)
         }
      }
      issue_slots(i).wakeup_dsts  := io.wakeup_pdsts
      issue_slots(i).brinfo       := io.brinfo
      issue_slots(i).kill         := io.flush_pipeline
      issue_slots(i).clear        := shamts_oh(i) != Bits(0)
   }

   //-------------------------------------------------------------
   // Dispatch/Entry Logic
   // did we find a spot to slide the new dispatched uops into?

   val will_be_available = (0 until num_issue_slots).map(i =>
                              (!issue_slots(i).will_be_valid || issue_slots(i).clear) && !(issue_slots(i).in_uop.valid))
   val num_available = PopCount(will_be_available)
   for (w <- 0 until DISPATCH_WIDTH)
   {
      io.dis_readys(w) := RegNext(num_available > UInt(w))
   }

   //-------------------------------------------------------------
   // Issue Select Logic

   // set default
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

   val requests = issue_slots.map(s => s.request)
   val port_issued = Array.fill(issue_width){Bool()}
   for (w <- 0 until issue_width)
   {
      port_issued(w) = Bool(false)
   }

   for (i <- 0 until num_issue_slots)
   {
      issue_slots(i).grant := Bool(false)
      var uop_issued = Bool(false)

      for (w <- 0 until issue_width)
      {
         val can_allocate = (issue_slots(i).uop.fu_code & io.fu_types(w)) != Bits(0)

         when (requests(i) && !uop_issued && can_allocate && !port_issued(w))
         {
            issue_slots(i).grant := Bool(true)
            io.iss_valids(w) := Bool(true)
            io.iss_uops(w) := issue_slots(i).uop
         }
         val was_port_issued_yet = port_issued(w)
         port_issued(w) = (requests(i) && !uop_issued && can_allocate) | port_issued(w)
         uop_issued = (requests(i) && can_allocate && !was_port_issued_yet) | uop_issued
      }
   }

}

}
