//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Slot Logic
//--------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Note: stores (and AMOs) are "broken down" into 2 uops, but stored within a single issue-slot.
// TODO XXX make a separate issueSlot for MemoryIssueSlots, and only they break apart stores.

package boom

import Chisel._
import FUConstants._
import cde.Parameters

class IssueSlotIO(num_wakeup_ports: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val valid          = Bool(OUTPUT)
   val will_be_valid  = Bool(OUTPUT) // TODO code review, do we need this signal so explicitely?
   val request        = Bool(OUTPUT)
   val request_hp     = Bool(OUTPUT)
   val grant          = Bool(INPUT)

   val brinfo         = new BrResolutionInfo().asInput
   val kill           = Bool(INPUT) // pipeline flush
   val clear          = Bool(INPUT) // entry being moved elsewhere (not mutually exclusive with grant)

   val wakeup_dsts    = Vec(num_wakeup_ports, Valid(UInt(width = PREG_SZ))).flip
   val in_uop         = Valid(new MicroOp()).flip // if valid, this WILL overwrite an entry!
   val updated_uop    = new MicroOp().asOutput // the updated slot uop; will be shifted upwards in a collasping queue.
   val uop            = new MicroOp().asOutput // the current Slot's uop. Sent down the pipeline when issued.

   val debug = {
     val result = new Bundle {
       val p1 = Bool()
       val p2 = Bool()
       val p3 = Bool()
    }
    result.asOutput
  }

   override def cloneType = new IssueSlotIO(num_wakeup_ports)(p).asInstanceOf[this.type]
}

class IssueSlot(num_slow_wakeup_ports: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new IssueSlotIO(num_slow_wakeup_ports)

   // slot invalid?
   // slot is valid, holding 1 uop
   // slot is valid, holds 2 uops (like a store)
   def isInvalid = slot_state === s_invalid
   def isValid = slot_state =/= s_invalid

   val updated_state = Wire(UInt()) // the next state of this slot (which might then get moved to a new slot)
   val updated_uopc  = Wire(Bits()) // the next uopc of this slot (which might then get moved to a new slot)
   val updated_lrs1_rtype = Wire(Bits()) // the next reg type of this slot (which might then get moved to a new slot)
   val updated_lrs2_rtype = Wire(Bits()) // the next reg type of this slot (which might then get moved to a new slot)
   val next_p1  = Wire(Bool())
   val next_p2  = Wire(Bool())
   val next_p3  = Wire(Bool())

   val slot_state    = Reg(init = s_invalid)
   val slot_p1       = Reg(init = Bool(false), next = next_p1)
   val slot_p2       = Reg(init = Bool(false), next = next_p2)
   val slot_p3       = Reg(init = Bool(false), next = next_p3)
   val slot_is_2uops = Reg(Bool())

   val slotUop = Reg(init = NullMicroOp)


   //-----------------------------------------------------------------------------
   // next slot state computation
   // compute the next state for THIS entry slot (in a collasping queue, the
   // current uop may get moved elsewhere, and a new slot can get moved into
   // here

   when (io.kill)
   {
      slot_state := s_invalid
   }
   .elsewhen (io.in_uop.valid)
   {
      slot_state := io.in_uop.bits.iw_state
   }
   .elsewhen (io.clear)
   {
      slot_state := s_invalid
   }
   .otherwise
   {
      slot_state := updated_state
   }

   //-----------------------------------------------------------------------------
   // "update" state
   // compute the next state for the micro-op in this slot. This micro-op may
   // be moved elsewhere, so the "updated_state" travels with it.

   // defaults
   updated_state := slot_state
   updated_uopc := slotUop.uopc
   updated_lrs1_rtype := slotUop.lrs1_rtype
   updated_lrs2_rtype := slotUop.lrs2_rtype

   when (io.kill ||
         (io.grant && (slot_state === s_valid_1)) ||
         (io.grant && (slot_state === s_valid_2) && slot_p1 && slot_p2))
   {
      updated_state := s_invalid
   }
   .elsewhen (io.grant && (slot_state === s_valid_2))
   {
      updated_state := s_valid_1
      when (slot_p1)
      {
         slotUop.uopc := uopSTD
         updated_uopc := uopSTD
         slotUop.lrs1_rtype := RT_X
         updated_lrs1_rtype := RT_X

      }
      .otherwise
      {
         slotUop.lrs2_rtype := RT_X
         updated_lrs2_rtype := RT_X
      }
   }

   when (io.in_uop.valid)
   {
      slotUop := io.in_uop.bits
      assert (isInvalid || io.clear || io.kill, "trying to overwrite a valid issue slot.")
   }

   // Wakeup Compare Logic
   next_p1 := Bool(false)
   next_p2 := Bool(false)
   next_p3 := Bool(false)

   // these signals are the "next_p*" for the current slot's micro-op.
   // they are important for shifting the current slotUop up to an other entry.
   // TODO need a better name for these signals
   val out_p1 = Wire(Bool()); out_p1 := slot_p1
   val out_p2 = Wire(Bool()); out_p2 := slot_p2
   val out_p3 = Wire(Bool()); out_p3 := slot_p3

   when (io.in_uop.valid)
   {
      next_p1 := !(io.in_uop.bits.prs1_busy)
      next_p2 := !(io.in_uop.bits.prs2_busy)
      next_p3 := !(io.in_uop.bits.prs3_busy)
   }
   .otherwise
   {
      next_p1 := out_p1
      next_p2 := out_p2
      next_p3 := out_p3
   }

   for (i <- 0 until num_slow_wakeup_ports)
   {
      when (io.wakeup_dsts(i).valid && (io.wakeup_dsts(i).bits === slotUop.pop1))
      {
         out_p1 := Bool(true)
      }
      when (io.wakeup_dsts(i).valid && (io.wakeup_dsts(i).bits === slotUop.pop2))
      {
         out_p2 := Bool(true)
      }
      when (io.wakeup_dsts(i).valid && (io.wakeup_dsts(i).bits === slotUop.pop3))
      {
         out_p3 := Bool(true)
      }
   }

   // Handle branch misspeculations
   val out_br_mask = GetNewBrMask(io.brinfo, slotUop)

   // was this micro-op killed by a branch? if yes, we can't let it be valid if
   // we compact it into an other entry
   when (IsKilledByBranch(io.brinfo, slotUop))
   {
      updated_state := s_invalid
   }

   when (!io.in_uop.valid)
   {
      slotUop.br_mask := out_br_mask
   }


   //-------------------------------------------------------------
   // Request Logic
   io.request := isValid && slot_p1 && slot_p2 && slot_p3 && !io.kill
   val high_priority = slotUop.is_br_or_jmp
//   io.request_hp := io.request && high_priority
   io.request_hp := Bool(false)

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
   io.valid         := isValid
   io.uop           := slotUop
   io.will_be_valid := isValid &&
                       !(io.grant && ((slot_state === s_valid_1) || (slot_state === s_valid_2) && slot_p1 && slot_p2))

   io.updated_uop           := slotUop
   io.updated_uop.iw_state  := updated_state
   io.updated_uop.uopc      := updated_uopc
   io.updated_uop.lrs1_rtype:= updated_lrs1_rtype
   io.updated_uop.lrs2_rtype:= updated_lrs2_rtype
   io.updated_uop.br_mask   := out_br_mask
   io.updated_uop.prs1_busy := !out_p1
   io.updated_uop.prs2_busy := !out_p2
   io.updated_uop.prs3_busy := !out_p3

   when (slot_state === s_valid_2)
   {
      when (slot_p1 && slot_p2)
      {
         ; // send out the entire instruction as one uop
      }
      .elsewhen (slot_p1)
      {
         io.uop.uopc := slotUop.uopc
         io.uop.lrs2_rtype := RT_X
      }
      .elsewhen (slot_p2)
      {
         io.uop.uopc := uopSTD
         io.uop.lrs1_rtype := RT_X
      }
   }

   // debug outputs
   io.debug.p1 := slot_p1
   io.debug.p2 := slot_p2
   io.debug.p3 := slot_p3
}

