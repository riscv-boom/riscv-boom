//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom
{
import Chisel._
import Node._

import FUCode._
import rocket.Str

import scala.collection.mutable.ArrayBuffer

//-------------------------------------------------------------
//-------------------------------------------------------------

class IssueUnitIO(issue_width: Int, num_wakeup_ports: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val dis_mask       = Vec.fill(DISPATCH_WIDTH) { Bool(INPUT) }
   val dis_uops       = Vec.fill(DISPATCH_WIDTH) { new MicroOp().asInput() }
   val dis_readys     = Vec.fill(DISPATCH_WIDTH) { Bool(OUTPUT) }

   val iss_valids     = Vec.fill(issue_width) { Bool(OUTPUT) }
   val iss_uops       = Vec.fill(issue_width) { new MicroOp().asOutput() }
   val wakeup_pdsts   = Vec.fill(num_wakeup_ports) { Valid(UInt(width=PREG_SZ)).flip }

   // tell the issue unit what each execution pipeline has in terms of functional units
   val fu_types       = Vec.fill(issue_width) { Bits(INPUT, FUC_SZ) }

   val brinfo         = new BrResolutionInfo().asInput
   val flush_pipeline = Bool(INPUT)

}

abstract class IssueUnit(num_issue_slots: Int, issue_width: Int, num_wakeup_ports: Int)(implicit p: Parameters) 
   extends BoomModule()(p)
{
   val io = new IssueUnitIO(issue_width, num_wakeup_ports)

   //-------------------------------------------------------------
   // Set up the dispatch uops
   // special case "storing" 2 uops within one issue slot.

   val dis_uops = Array.fill(DISPATCH_WIDTH) {Wire(new MicroOp())}
   for (w <- 0 until DISPATCH_WIDTH)
   {
      dis_uops(w) := io.dis_uops(w)
      dis_uops(w).iw_state := s_valid_1
      when (dis_uops(w).uopc === uopSTA || dis_uops(w).uopc === uopAMO_AG)
      {
         dis_uops(w).iw_state := s_valid_2
      }
   }

   //-------------------------------------------------------------
   // Issue Table

   val issue_slots = Vec.fill(num_issue_slots) {Module(new IssueSlot(num_wakeup_ports)).io}

   //-------------------------------------------------------------

   assert (PopCount(issue_slots.map(s => s.grant)) <= UInt(issue_width), "Issue window giving out too many grants.")

   //-------------------------------------------------------------

   if (DEBUG_PRINTF)
   {
      for (i <- 0 until num_issue_slots)
      {
         printf("  integer_issue_slot[%d](%s)(Req:%s):wen=%s P:(%s,%s,%s) OP:(%d,%d,%d) PDST:%d %s [%s[DASM(%x)]"+
               end+" 0x%x: %d] ri:%d bm=%d imm=0x%x\n"
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
            )
      }
   }
}


}
