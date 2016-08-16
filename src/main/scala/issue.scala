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
import cde.Parameters

import FUConstants._
import rocket.Str


//-------------------------------------------------------------
//-------------------------------------------------------------

class IssueUnitIO(issue_width: Int, num_wakeup_ports: Int, num_vec_wakeup_ports: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val dis_valids     = Vec(DISPATCH_WIDTH, Bool(INPUT))
   val dis_uops       = Vec(DISPATCH_WIDTH, new MicroOp()).asInput
   val dis_readys     = Vec(DISPATCH_WIDTH, Bool(OUTPUT))

   val iss_valids     = Vec(issue_width, Bool(OUTPUT))
   val iss_uops       = Vec(issue_width, new MicroOp().asOutput)
   val wakeup_pdsts   = Vec(num_wakeup_ports, Valid(UInt(width=PREG_SZ))).flip
   val wakeup_vec_pdsts= Vec(num_vec_wakeup_ports, Valid(UInt(width = log2Up(numPhysVecRegisters)))).flip

   // tell the issue unit what each execution pipeline has in terms of functional units
   val fu_types       = Vec(issue_width, Bits(INPUT, FUC_SZ))

   val brinfo         = new BrResolutionInfo().asInput
   val flush_pipeline = Bool(INPUT)

   val tsc_reg        = UInt(INPUT, xLen)
}

abstract class IssueUnit(num_issue_slots: Int, issue_width: Int, num_wakeup_ports: Int, num_vec_wakeup_ports: Int)(implicit p: Parameters)
   extends BoomModule()(p)
{
   val io = new IssueUnitIO(issue_width, num_wakeup_ports, num_vec_wakeup_ports)

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

   val issue_slots = Vec.fill(num_issue_slots) {Module(new IssueSlot(num_wakeup_ports, num_vec_wakeup_ports)).io}

   //-------------------------------------------------------------

   assert (PopCount(issue_slots.map(s => s.grant)) <= UInt(issue_width), "Issue window giving out too many grants.")

   //-------------------------------------------------------------

   if (O3PIPEVIEW_PRINTF)
   {
      for (i <- 0 until ISSUE_WIDTH)
      {
         // only print stores once!
         when (io.iss_valids(i) && io.iss_uops(i).uopc =/= uopSTD)
         {
            printf("%d; O3PipeView:issue: %d\n",
               io.iss_uops(i).debug_events.fetch_seq,
               io.tsc_reg)
         }
      }
   }

   if (DEBUG_PRINTF)
   {
      for (i <- 0 until num_issue_slots)
      {
         printf("  integer_issue_slot[%d](%c)(Req:%c):wen=%c P:(%c,%c,%c) OP:(%d,%d,%d// %d,%d) PDST:%d,%d %c [%c[DASM(%x)]" +
               end + " 0x%x: %d] ri:%d bm=%d imm=0x%x\n"
            , UInt(i, log2Up(num_issue_slots))
            , Mux(issue_slots(i).valid, Str("V"), Str("-"))
//            , Mux(issue_slots(i).request, Str(u_red + "R" + end), Str(grn + "-" + end))
//            , Mux(issue_slots(i).in_uop.valid, Str(u_wht + "W" + end),  Str(grn + " " + end))
            , Mux(issue_slots(i).request, Str("R"), Str("-"))
            , Mux(issue_slots(i).in_uop.valid, Str("W"),  Str(" "))
            , Mux(issue_slots(i).debug.p1, Str("!"), Str(" "))
            , Mux(issue_slots(i).debug.p2, Str("!"), Str(" "))
            , Mux(issue_slots(i).debug.p3, Str("!"), Str(" "))
            , issue_slots(i).uop.pop1
            , issue_slots(i).uop.pop2
            , issue_slots(i).uop.pop3
            , issue_slots(i).uop.vec.pvrs1
            , issue_slots(i).uop.vec.pvrs2
            , issue_slots(i).uop.pdst
            , issue_slots(i).uop.vec.pvdst
            , Mux(issue_slots(i).uop.dst_rtype === RT_FIX, Str("X"),
              Mux(issue_slots(i).uop.dst_rtype === RT_X, Str("-"),
              Mux(issue_slots(i).uop.dst_rtype === RT_FLT, Str("f"),
              Mux(issue_slots(i).uop.dst_rtype === RT_PAS, Str("C"),
              Mux(issue_slots(i).uop.dst_rtype === RT_VEC, Str("V"), Str("?"))))))
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
