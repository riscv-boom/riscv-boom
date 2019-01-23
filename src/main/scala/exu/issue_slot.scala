//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// RISCV Processor Issue Slot Logic
//--------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Note: stores (and AMOs) are "broken down" into 2 uops, but stored within a single issue-slot.
// TODO XXX make a separate issueSlot for MemoryIssueSlots, and only they break apart stores.
// TODO Disable ldspec for FP queue.


package boom.exu

import chisel3._
import chisel3.util.Valid
import FUConstants._
import freechips.rocketchip.config.Parameters
import boom.common._
import boom.util._

class IssueSlotIO(val num_wakeup_ports: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val valid          = Output(Bool())
   val will_be_valid  = Output(Bool()) // TODO code review, do we need this signal so explicitely?
   val request        = Output(Bool())
   val request_hp     = Output(Bool())
   val grant          = Input(Bool())

   val brinfo         = Input(new BrResolutionInfo())
   val kill           = Input(Bool()) // pipeline flush
   val clear          = Input(Bool()) // entry being moved elsewhere (not mutually exclusive with grant)
   val ldspec_miss    = Input(Bool()) // Previous cycle's speculative load wakeup was mispredicted.

   val wakeup_dsts    = Flipped(Vec(num_wakeup_ports, Valid(new IqWakeup(PREG_SZ))))
   val ldspec_dst     = Flipped(Valid(UInt(width=PREG_SZ.W)))
   val in_uop         = Flipped(Valid(new MicroOp())) // if valid, this WILL overwrite an entry!
   val updated_uop    = Output(new MicroOp()) // the updated slot uop; will be shifted upwards in a collasping queue.
   val uop            = Output(new MicroOp()) // the current Slot's uop. Sent down the pipeline when issued.

   val debug = {
     val result = new Bundle {
       val p1 = Bool()
       val p2 = Bool()
       val p3 = Bool()
       val state = UInt(width=2.W)
    }
    Output(result)
  }

}

class IssueSlot(num_slow_wakeup_ports: Int)(implicit p: Parameters)
   extends BoomModule()(p)
   with IssueUnitConstants
{
   val io = IO(new IssueSlotIO(num_slow_wakeup_ports))

   // slot invalid?
   // slot is valid, holding 1 uop
   // slot is valid, holds 2 uops (like a store)
   def isInvalid = slot_state === s_invalid
   def isValid = slot_state =/= s_invalid

   val updated_state      = Wire(UInt()) // the next state of this slot (which might then get moved to a new slot)
   val updated_uopc       = Wire(UInt()) // the next uopc of this slot (which might then get moved to a new slot)
   val updated_lrs1_rtype = Wire(UInt()) // the next reg type of this slot (which might then get moved to a new slot)
   val updated_lrs2_rtype = Wire(UInt()) // the next reg type of this slot (which might then get moved to a new slot)

   val slot_state         = RegInit(s_invalid)
   val slot_p1            = RegInit(false.B)
   val slot_p2            = RegInit(false.B)
   val slot_p3            = RegInit(false.B)

   // Poison if woken up by speculative load.
   // Poison lasts 1 cycle (as ldMiss will come on the next cycle).
   // SO if poisoned is true, set it to false!
   val slot_p1_poisoned   = RegInit(false.B)
   val slot_p2_poisoned   = RegInit(false.B)

   val slotUop = RegInit(NullMicroOp)

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

   when (io.kill)
   {
      updated_state := s_invalid
   }
   .elsewhen (
      (io.grant && (slot_state === s_valid_1)) ||
      (io.grant && (slot_state === s_valid_2) && slot_p1 && slot_p2))
   {
      // try to issue this uop.
      when (!(io.ldspec_miss && (slot_p1_poisoned || slot_p2_poisoned)))
      {
         updated_state := s_invalid
      }
   }
   .elsewhen (io.grant && (slot_state === s_valid_2))
   {
      when (!(io.ldspec_miss && (slot_p1_poisoned || slot_p2_poisoned)))
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
   }

   when (io.in_uop.valid)
   {
      slotUop := io.in_uop.bits
      assert (isInvalid || io.clear || io.kill, "trying to overwrite a valid issue slot.")
   }

   // Wakeup Compare Logic

   // these signals are the "next_p*" for the current slot's micro-op.
   // they are important for shifting the current slotUop up to an other entry.
   val updated_p1 = WireInit(slot_p1)
   val updated_p2 = WireInit(slot_p2)
   val updated_p3 = WireInit(slot_p3)
   // Even if slot is poisoned, set it to false immediately.
   val updated_p1_poisoned = WireInit(false.B)
   val updated_p2_poisoned = WireInit(false.B)

   when (io.in_uop.valid)
   {
      slot_p1 := !(io.in_uop.bits.prs1_busy)
      slot_p2 := !(io.in_uop.bits.prs2_busy)
      slot_p3 := !(io.in_uop.bits.prs3_busy)
      slot_p1_poisoned := io.in_uop.bits.iw_p1_poisoned
      slot_p2_poisoned := io.in_uop.bits.iw_p2_poisoned
   }
   .otherwise
   {
      slot_p1 := updated_p1
      slot_p2 := updated_p2
      slot_p3 := updated_p3
      slot_p1_poisoned := updated_p1_poisoned
      slot_p2_poisoned := updated_p2_poisoned
   }

   for (i <- 0 until num_slow_wakeup_ports)
   {
      when (io.wakeup_dsts(i).valid &&
         (io.wakeup_dsts(i).bits.pdst === slotUop.pop1) &&
         !(io.ldspec_miss && io.wakeup_dsts(i).bits.poisoned))
      {
         updated_p1 := true.B
      }
      when (io.wakeup_dsts(i).valid &&
         (io.wakeup_dsts(i).bits.pdst === slotUop.pop2) &&
         !(io.ldspec_miss && io.wakeup_dsts(i).bits.poisoned))
      {
         updated_p2 := true.B
      }
      when (io.wakeup_dsts(i).valid &&
         (io.wakeup_dsts(i).bits.pdst === slotUop.pop3))
      {
         updated_p3 := true.B
      }
   }

   assert (!(io.ldspec_dst.valid && io.ldspec_dst.bits === 0.U),
      "Loads to x0 should never speculatively wakeup other instructions")


   // TODO disable if FP IQ.
   when (io.ldspec_dst.valid && io.ldspec_dst.bits === slotUop.pop1 && slotUop.lrs1_rtype === RT_FIX)
   {
      updated_p1 := true.B
      updated_p1_poisoned := true.B
      assert (!slot_p1_poisoned)
   }
   when (io.ldspec_dst.valid && io.ldspec_dst.bits === slotUop.pop2 && slotUop.lrs2_rtype === RT_FIX)
   {
      updated_p2 := true.B
      updated_p2_poisoned := true.B
      assert (!slot_p2_poisoned)
   }

   when (io.ldspec_miss && slot_p1_poisoned)
   {
      assert(slotUop.pop1 =/= 0.U, "Poison bit can't be set for pop1=x0!")
      updated_p1 := false.B
   }
   when (io.ldspec_miss && slot_p2_poisoned)
   {
      assert(slotUop.pop2 =/= 0.U, "Poison bit can't be set for pop2=x0!")
      updated_p2 := false.B
   }


   // Handle branch misspeculations
   val updated_br_mask = GetNewBrMask(io.brinfo, slotUop)

   // was this micro-op killed by a branch? if yes, we can't let it be valid if
   // we compact it into an other entry
   when (IsKilledByBranch(io.brinfo, slotUop))
   {
      updated_state := s_invalid
   }

   when (!io.in_uop.valid)
   {
      slotUop.br_mask := updated_br_mask
   }


   //-------------------------------------------------------------
   // Request Logic
   io.request := isValid && slot_p1 && slot_p2 && slot_p3 && !io.kill
   val high_priority = slotUop.is_br_or_jmp
   io.request_hp := false.B

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
      io.request := false.B
   }


   //assign outputs
   io.valid := isValid
   io.uop := slotUop
   io.uop.iw_p1_poisoned := slot_p1_poisoned
   io.uop.iw_p2_poisoned := slot_p2_poisoned


   // micro-op will vacate due to grant.
   val may_vacate = io.grant && ((slot_state === s_valid_1) || (slot_state === s_valid_2) && slot_p1 && slot_p2)
   val squash_grant = io.ldspec_miss && (slot_p1_poisoned || slot_p2_poisoned)
   io.will_be_valid := isValid && !(may_vacate && !squash_grant)


   io.updated_uop           := slotUop
   io.updated_uop.iw_state  := updated_state
   io.updated_uop.uopc      := updated_uopc
   io.updated_uop.lrs1_rtype:= updated_lrs1_rtype
   io.updated_uop.lrs2_rtype:= updated_lrs2_rtype
   io.updated_uop.br_mask   := updated_br_mask
   io.updated_uop.prs1_busy := !updated_p1
   io.updated_uop.prs2_busy := !updated_p2
   io.updated_uop.prs3_busy := !updated_p3
   io.updated_uop.iw_p1_poisoned := updated_p1_poisoned
   io.updated_uop.iw_p2_poisoned := updated_p2_poisoned

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
   io.debug.state := slot_state

}

