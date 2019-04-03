//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import boom.common._
import boom.exu.FUConstants._
import boom.util.{PrintUtil}

/**
 * Class used for configurations
 *
 * @param issueWidth amount of things that can be issued
 * @param numEntries size of issue queue
 * @param iqType type of issue queue
 */
case class IssueParams(
   issueWidth: Int = 1,
   numEntries: Int = 8,
   iqType: BigInt
)

/**
 * Constants for knowing about the status of a MicroOp
 */
trait IssueUnitConstants
{
   // invalid  : slot holds no valid uop.
   // s_valid_1: slot holds a valid uop.
   // s_valid_2: slot holds a store-like uop that may be broken into two micro-ops.
   val s_invalid :: s_valid_1 :: s_valid_2 :: Nil = Enum(3)
}

/**
 * What physical register is broadcasting its wakeup?
 * Is the physical register poisoned (aka, was it woken up by a speculative issue)?
 *
 * @param preg_sz size of physical destination register
 */
class IqWakeup(val preg_sz: Int) extends Bundle
{
   val pdst = UInt(width=preg_sz.W)
   val poisoned = Bool()
}

/**
 * IO bundle to interact with the issue unit
 *
 * @param issue_width amount of operations that can be issued at once
 * @param num_wakeup_ports number of wakeup ports for issue unit
 */
class IssueUnitIO(
   val issue_width: Int,
   val num_wakeup_ports: Int)
   (implicit p: Parameters) extends BoomBundle()(p)
{
   val dis_valids     = Input(Vec(DISPATCH_WIDTH, Bool()))
   val dis_uops       = Input(Vec(DISPATCH_WIDTH, new MicroOp()))
   val dis_readys     = Output(Vec(DISPATCH_WIDTH, Bool()))

   val iss_valids     = Output(Vec(issue_width, Bool()))
   val iss_uops       = Output(Vec(issue_width, new MicroOp()))
   val wakeup_pdsts   = Flipped(Vec(num_wakeup_ports, Valid(new IqWakeup(PREG_SZ))))

   val mem_ldSpecWakeup= Flipped(Valid(UInt(width=PREG_SZ.W)))

   // tell the issue unit what each execution pipeline has in terms of functional units
   val fu_types       = Input(Vec(issue_width, Bits(width=FUC_SZ.W)))

   val brinfo         = Input(new BrResolutionInfo())
   val flush_pipeline = Input(Bool())
   val sxt_ldMiss     = Input(Bool())

   val event_empty    = Output(Bool()) // used by HPM events; is the issue unit empty?

   val tsc_reg        = Input(UInt(width=xLen.W))
}

/**
 * Abstract top level issue unit
 *
 * @param num_issue_slots depth of issue queue
 * @param issue_width amoutn of operations that can be issued at once
 * @param num_wakeup_ports number of wakeup ports for issue unit
 * @param iqType type of issue queue (mem, int, fp)
 */
abstract class IssueUnit(
   val num_issue_slots: Int,
   val issue_width: Int,
   num_wakeup_ports: Int,
   val iqType: BigInt)
   (implicit p: Parameters)
   extends BoomModule()(p)
   with IssueUnitConstants
{
   val io = IO(new IssueUnitIO(issue_width, num_wakeup_ports))

   //-------------------------------------------------------------
   // Set up the dispatch uops
   // special case "storing" 2 uops within one issue slot.

   val dis_uops = Array.fill(DISPATCH_WIDTH) {Wire(new MicroOp())}
   for (w <- 0 until DISPATCH_WIDTH)
   {
      dis_uops(w) := io.dis_uops(w)
      dis_uops(w).iw_p1_poisoned := false.B
      dis_uops(w).iw_p2_poisoned := false.B
      dis_uops(w).iw_state := s_valid_1
      when ((dis_uops(w).uopc === uopSTA && dis_uops(w).lrs2_rtype === RT_FIX) || dis_uops(w).uopc === uopAMO_AG)
      {
         dis_uops(w).iw_state := s_valid_2
      }
   }

   //-------------------------------------------------------------
   // Issue Table

   val slots = for (i <- 0 until num_issue_slots) yield { val slot = Module(new IssueSlot(num_wakeup_ports)); slot }
   val issue_slots = VecInit(slots.map(_.io))

   io.event_empty := !(issue_slots.map(s => s.valid).reduce(_|_))

   //-------------------------------------------------------------

   assert (PopCount(issue_slots.map(s => s.grant)) <= issue_width.U, "[issue] window giving out too many grants.")

   // Check that a ldMiss signal was preceded by a ldSpecWakeup.
   // However, if the load gets killed before it hits SXT stage, we may see
   // the sxt_ldMiss signal (from some other load) by not the ldSpecWakeup signal.
   // So track branch kills for the last 4 cycles to remove false negatives.
   val brKills = RegInit(0.asUInt(width=4.W))
   brKills := Cat(brKills, (io.brinfo.valid && io.brinfo.mispredict) || io.flush_pipeline)
   assert (!(io.sxt_ldMiss && !RegNext(io.mem_ldSpecWakeup.valid, init=false.B) && brKills === 0.U),
      "[issue] IQ-" + iqType + " a ld miss was not preceded by a spec wakeup.")

   //-------------------------------------------------------------

   if (O3PIPEVIEW_PRINTF)
   {
      for (i <- 0 until issue_width)
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

   if (DEBUG_PRINTF_IQ)
   {
      printf(this.getType + " issue slots:\n")
      for (i <- 0 until num_issue_slots)
      {
         printf("    Slot[%d]: " +
                "V:%c Req:%c Wen:%c P:(%c,%c,%c) PRegs:Dst:(Typ:%c #:%d) Srcs:(%d,%d,%d) " +
                "[PC:0x%x Inst:DASM(%x) UOPCode:%d] RobIdx:%d BMsk:0x%x Imm:0x%x\n",
                i.U(log2Ceil(num_issue_slots).W),
                PrintUtil.ConvertChar(       issue_slots(i).valid, 'V'),
                PrintUtil.ConvertChar(     issue_slots(i).request, 'R'),
                PrintUtil.ConvertChar(issue_slots(i).in_uop.valid, 'W'),
                PrintUtil.ConvertChar(    issue_slots(i).debug.p1, '!'),
                PrintUtil.ConvertChar(    issue_slots(i).debug.p2, '!'),
                PrintUtil.ConvertChar(    issue_slots(i).debug.p3, '!'),
                Mux(issue_slots(i).uop.dst_rtype === RT_FIX, Str("X"),
                    Mux(issue_slots(i).uop.dst_rtype === RT_X, Str("-"),
                        Mux(issue_slots(i).uop.dst_rtype === RT_FLT, Str("f"),
                            Mux(issue_slots(i).uop.dst_rtype === RT_PAS, Str("C"), Str("?"))))),
                issue_slots(i).uop.pdst,
                issue_slots(i).uop.pop1,
                issue_slots(i).uop.pop2,
                issue_slots(i).uop.pop3,
                issue_slots(i).uop.pc(31,0),
                issue_slots(i).uop.inst,
                issue_slots(i).uop.uopc,
                issue_slots(i).uop.rob_idx,
                issue_slots(i).uop.br_mask,
                issue_slots(i).uop.imm_packed)
      }
   }

   def getType: String =
      if (iqType == IQT_INT.litValue) "int"
      else if (iqType == IQT_MEM.litValue) "mem"
      else if (iqType == IQT_FP.litValue) "fp"
      else "unknown"
}
