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

import chisel3._
import chisel3.util.{log2Ceil, PopCount, Fill}
import freechips.rocketchip.config.Parameters

import FUConstants._
import freechips.rocketchip.util.Str
import scala.collection.mutable.ArrayBuffer
import boom.common._

//-------------------------------------------------------------
//-------------------------------------------------------------

class IssueUnitCollasping(
   params: IssueParams,
   containsVec: Boolean,
   isVec: Boolean,
   num_wakeup_ports: Int)
   (implicit p: Parameters)
   extends IssueUnit(params.numEntries, params.issueWidth, containsVec, num_wakeup_ports, isVec, params.iqType)
{
   //-------------------------------------------------------------
   // Figure out how much to shift entries by

   val MAX_SHIFT = DISPATCH_WIDTH
   val shamt_oh = Array.fill(num_issue_slots){UInt(width=issue_width.W)}
   // count total grants before this entry, and tus how many to shift upwards by
   val shamt = Array.fill(num_issue_slots){UInt(width=log2Ceil(issue_width+1).W)}


   val vacants = issue_slots.map(s => !(s.valid)) ++ io.dis_valids.map(!_.toBool)
   val shamts_oh = Array.fill(num_issue_slots+DISPATCH_WIDTH) {Wire(UInt(width=MAX_SHIFT.W))}
   // track how many to shift up this entry by by counting previous vacant spots
   def SaturatingCounterOH(count_oh:UInt, inc: Bool, max: Int): UInt =
   {
      val next = Wire(UInt(width=max.W))
      next := count_oh
      when (count_oh === 0.U && inc)
      {
         next := 1.U
      }
      .elsewhen (!count_oh(max-1) && inc)
      {
         next := (count_oh << 1.U)
      }
      next
   }
   shamts_oh(0) := 0.U
   for (i <- 1 until num_issue_slots + DISPATCH_WIDTH)
   {
      shamts_oh(i) := SaturatingCounterOH(shamts_oh(i-1), vacants(i-1), MAX_SHIFT)
   }

   //-------------------------------------------------------------

   // which entries' uops will still be next cycle? (not being issued and vacated)
   val will_be_valid = (0 until num_issue_slots).map(i => issue_slots(i).will_be_valid) ++
                       (0 until DISPATCH_WIDTH).map(i => io.dis_valids(i) &&
                                                         !io.dis_uops(i).exception &&
                                                         !io.dis_uops(i).is_fence &&
                                                         !io.dis_uops(i).is_fencei)

   val uops = issue_slots.map(s=>s.updated_uop) ++ dis_uops.map(s=>s)
   for (i <- 0 until num_issue_slots)
   {
      issue_slots(i).in_uop.valid := false.B
      issue_slots(i).in_uop.bits  := uops(i+1)
      for (j <- 1 to MAX_SHIFT by 1)
      {
         when (shamts_oh(i+j) === (1 << (j-1)).U)
         {
            issue_slots(i).in_uop.valid := will_be_valid(i+j)
            issue_slots(i).in_uop.bits  := uops(i+j)
         }
      }
      issue_slots(i).wakeup_dsts  := io.wakeup_pdsts
      issue_slots(i).brinfo       := io.brinfo
      issue_slots(i).kill         := io.flush_pipeline
      issue_slots(i).clear        := shamts_oh(i) =/= 0.U
      issue_slots(i).vl           := io.vl
   }

   //-------------------------------------------------------------
   // Dispatch/Entry Logic
   // did we find a spot to slide the new dispatched uops into?

   val will_be_available = (0 until num_issue_slots).map(i =>
                              (!issue_slots(i).will_be_valid || issue_slots(i).clear) && !(issue_slots(i).in_uop.valid))
   val num_available = PopCount(will_be_available)
   for (w <- 0 until DISPATCH_WIDTH)
   {
      io.dis_readys(w) := RegNext(num_available > w.U)
   }

   //-------------------------------------------------------------
   // Issue Select Logic

   // set default
   for (w <- 0 until issue_width)
   {
      io.iss_valids(w) := false.B
      io.iss_uops(w)   := NullMicroOp
      // unsure if this is overkill
      io.iss_uops(w).pop1 := 0.U
      io.iss_uops(w).pop2 := 0.U
      io.iss_uops(w).pop3 := 0.U
      io.iss_uops(w).lrs1_rtype := RT_X
      io.iss_uops(w).lrs2_rtype := RT_X
   }

   val requests = issue_slots.map(s => s.request)
   val port_issued = Array.fill(issue_width){Bool()}
   for (w <- 0 until issue_width)
   {
      port_issued(w) = false.B
   }

   val all_available_fu = io.fu_types.reduce(_|_)

   val min_eidx = Wire(UInt(width=VL_SZ.W))
   if (containsVec && !isVec) {
      min_eidx := issue_slots.foldLeft(Fill(VL_SZ, 1.U)) {
         (x, y) => Mux((x > y.uop.eidx) && y.request && y.uop.vec_val && (y.uop.fu_code & all_available_fu) =/= 0.U, y.uop.eidx, x)
      }
   }
   else
   {
      min_eidx := 0.U
   }

   for (i <- 0 until num_issue_slots)
   {
      issue_slots(i).grant := false.B
      var uop_issued = false.B

      for (w <- 0 until issue_width)
      {
         val can_allocate = (issue_slots(i).uop.fu_code & io.fu_types(w)) =/= 0.U
         val vec_eidx_issue = !containsVec.B || issue_slots(i).uop.eidx === min_eidx || !issue_slots(i).uop.vec_val || isVec.B
         val was_port_issued_yet = port_issued(w)
         when (requests(i) && !uop_issued && can_allocate && !port_issued(w) && vec_eidx_issue)
         {
            issue_slots(i).grant := true.B
            io.iss_valids(w)     := true.B
            io.iss_uops(w)       := issue_slots(i).uop
         }
         port_issued(w) = (requests(i) && !uop_issued          && can_allocate && vec_eidx_issue) | port_issued(w)
         uop_issued     = (requests(i) && !was_port_issued_yet && can_allocate && vec_eidx_issue) | uop_issued
         // TODO_Vec: Does this break when issue_width > 1? Probably not? It does rip me
      }
   }
}

