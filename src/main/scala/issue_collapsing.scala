//**************************************************************************
// RISCV Processor Issue Logic
//--------------------------------------------------------------------------
//

package BOOM
{
import Chisel._
import Node._

import FUCode._
import rocket.Str

import scala.collection.mutable.ArrayBuffer

//-------------------------------------------------------------
//-------------------------------------------------------------

class IssueUnitIO(issue_width: Int, num_wakeup_ports: Int) extends BOOMCoreBundle
{
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
   val wakeup_pdsts = Vec.fill(num_wakeup_ports) { Valid(UInt(width=PREG_SZ)).flip }
}

class IssueUnit(num_issue_slots: Int, issue_width: Int, num_wakeup_ports: Int) extends Module with BOOMCoreParameters
{
   val io = new IssueUnitIO(issue_width, num_wakeup_ports)

   val num_requestors = num_issue_slots
 
   val shamt_oh = Array.fill(num_requestors){Bits(width=issue_width)}
   // count total grants before this entry, and tus how many to shift upwards by
   val shamt = Array.fill(num_requestors){Bits(width=log2Up(issue_width+1))}
        
   //-------------------------------------------------------------
   // Issue Table

   val issue_slots = Vec.fill(num_issue_slots) {Module(new IssueSlot(num_wakeup_ports)).io}
   
   
   //-------------------------------------------------------------
   // Figure out how much to shift entries by

   val MAX_SHIFT = DISPATCH_WIDTH


   val vacants = issue_slots.map(s => !(s.valid)) ++ io.dis_mask.map(!_.toBool)
   val shamts_oh = Array.fill(num_requestors+DISPATCH_WIDTH) {Bits(width=MAX_SHIFT)}
   // track how many to shift up this entry by by counting previous vacant spots
   def SaturatingCounterOH(count_oh:Bits, inc: Bool, max: Int): Bits =
   {
      val next = Bits(width=max)
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
   for (i <- 1 until num_requestors + DISPATCH_WIDTH)
   {
      shamts_oh(i) := SaturatingCounterOH(shamts_oh(i-1), vacants(i-1), MAX_SHIFT)
   }

   //-------------------------------------------------------------

   // which entries' uops will still be next cycle? (not being issued and vacated)
   val will_be_valid = (0 until num_requestors).map(i => issue_slots(i).will_be_valid) ++
                       (0 until DISPATCH_WIDTH).map(i => io.dis_mask(i) &&
                                                         !io.dis_uops(i).exception &&
                                                         !io.dis_uops(i).is_fence &&
                                                         !io.dis_uops(i).is_fencei)

   val dis_uops = Array.fill(DISPATCH_WIDTH) {new MicroOp()}
   for (w <- 0 until DISPATCH_WIDTH)
   {
      dis_uops(w) := io.dis_uops(w)
      dis_uops(w).iw_state := s_valid_1
      // special case "storing" 2 uops within one issue slot
      // for now, only stores/amos supported
      when (dis_uops(w).uopc === uopSTA || dis_uops(w).uopc === uopAMO_AG)
      {
         dis_uops(w).iw_state := s_valid_2
      }
   }

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

   for (w <- 0 until DISPATCH_WIDTH)
   {
      io.dis_inst_can_proceed(w) := shamts_oh(num_requestors+w) >= Bits(1 << w)
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
   
   for (i <- 0 until num_requestors)
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
         port_issued(w) = (requests(i) && !uop_issued  && can_allocate) | port_issued(w)
         uop_issued = (requests(i) && can_allocate) | uop_issued
      }
   }

   //-------------------------------------------------------------

   assert (PopCount(issue_slots.map(s => s.grant)) <= UInt(issue_width), "Issue window giving out too many grants.")
   
   //-------------------------------------------------------------

   if (DEBUG_PRINTF)
   {
      for (i <- 0 until num_issue_slots)
      {
         printf("  integer_issue_slot[%d](%s)(Req:%s):wen=%s P:(%s,%s,%s) OP:(%d,%d,%d) PDST:%d %s [%s[DASM(%x)]"+end+" 0x%x: %d] ri:%d bm=%d imm=0x%x sh:%x\n"
            , UInt(i, log2Up(num_issue_slots))
            , Mux(issue_slots(i).valid, Str("V"), Str("-"))
            , Mux(issue_slots(i).request, Str(u_red + "R" + end), Str(grn + "-" + end))
            , Mux(issue_slots(i).in_uop.valid, Str(u_wht + "W" + end),  Str(grn + " " + end))
            , Mux(issue_slots(i).debug.p1, Str("!"), Str(" "))
            , Mux(issue_slots(i).debug.p2, Str("!"), Str(" "))
            , Mux(issue_slots(i).debug.p3, Str("!"), Str(" "))
            , issue_slots(i).uop.pop1
            , issue_slots(i).uop.pop2
            , issue_slots(i).uop.pop3
            , issue_slots(i).uop.pdst
            , Mux(issue_slots(i).uop.dst_rtype === RT_FIX, Str("X"),
              Mux(issue_slots(i).uop.dst_rtype === RT_X, Str("-"),
              Mux(issue_slots(i).uop.dst_rtype === RT_FLT, Str("f"),
              Mux(issue_slots(i).uop.dst_rtype === RT_PAS, Str("C"), Str("?")))))
            , Mux(issue_slots(i).valid, Str(b_wht), Str(grn))
            , issue_slots(i).uop.inst
            , issue_slots(i).uop.pc(31,0)
            , issue_slots(i).uop.uopc
            , issue_slots(i).uop.rob_idx
            , issue_slots(i).uop.br_mask
            , issue_slots(i).uop.imm_packed
            , shamts_oh(i)
            )
      }
   }

}

}
