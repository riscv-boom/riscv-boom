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

package boom.exu

import chisel3._
import chisel3.util.Valid
import FUConstants._
import freechips.rocketchip.config.Parameters
import boom.common._
import boom.util._

class IssueSlotIO(num_wakeup_ports: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val valid          = Output(Bool())
   val will_be_valid  = Output(Bool()) // TODO code review, do we need this signal so explicitely?
   val request        = Output(Bool())
   val request_hp     = Output(Bool())
   val grant          = Input(Bool())

   val brinfo         = Input(new BrResolutionInfo())
   val kill           = Input(Bool()) // pipeline flush
   val clear          = Input(Bool()) // entry being moved elsewhere (not mutually exclusive with grant)

   val wakeup_dsts    = Flipped(Vec(num_wakeup_ports, Valid(new WakeupPdst())))
   val in_uop         = Flipped(Valid(new MicroOp())) // if valid, this WILL overwrite an entry!
   val updated_uop    = Output(new MicroOp()) // the updated slot uop; will be shifted upwards in a collasping queue.
   val uop            = Output(new MicroOp()) // the current Slot's uop. Sent down the pipeline when issued.
      
   val vl             = Input(UInt(width=VL_SZ.W)) // The global vector length

   val lsu_stq_head        = Input(UInt())
   // TODO_vec: All this logic probably needs to be removed when separate vector memory unit is implemented
   // For now this tracks element indices of ops in the lsu ldq, we don't issue ops until the LDQ is ready to receive them
   // This is actually very bad since it is essentially unpipelined vector loads


   // TODO_Vec: This should probably get rolled into the wakeup system
   val fromfp_valid   = Input(Bool())
   val fromfp_op_id   = Input(UInt(width=2.W)) // Address of physical scalar vector register operand (thats a lot of adjectives)

   val fromint_valid  = Input(Bool())
   val fromint_op_id  = Input(UInt(width=2.W))
   val fromint_data   = Input(UInt(width=xLen.W))

   val retire         = Output(Bool()) // This MicroOp has finished issuing

   val debug = {
     val result = new Bundle {
       val p1 = Bool()
       val p2 = Bool()
       val p3 = Bool()
    }
    Output(result)
  }

   override def cloneType = new IssueSlotIO(num_wakeup_ports)(p).asInstanceOf[this.type]
}



// TODO_Vec: ContainsVec denotes issue slots which have to increment the eidx
//           Currently this is both vector issue queue and memory issue queue
//           In the future, memory issue queue will not have to increment the eidx,
//           So containsVec and isVec will have same meaning
class IssueSlot(num_slow_wakeup_ports: Int, containsVec: Boolean, isVec: Boolean)(implicit p: Parameters)
   extends BoomModule()(p)
   with IssueUnitConstants
{
   val io = IO(new IssueSlotIO(num_slow_wakeup_ports))

   val slotUop = RegInit(NullMicroOp)
   //   val slot_state    = Reg(init = s_invalid)
   val slot_state = slotUop.iw_state
   val slot_is_2uops = Reg(Bool())
   val slot_p1 = !slotUop.prs1_busy
   val slot_p2 = !slotUop.prs2_busy
   val slot_p3 = !slotUop.prs3_busy

   // slot invalid?
   // slot is valid, holding 1 uop
   // slot is valid, holds 2 uops (like a store)
   def isInvalid = slot_state === s_invalid
   def isValid = slot_state =/= s_invalid

   val updated_state      = Wire(UInt()) // the next state of this slot (which might then get moved to a new slot)
   val updated_eidx       = Wire(UInt())
   val updated_uopc       = Wire(UInt()) // the next uopc of this slot (which might then get moved to a new slot)
   val updated_lrs1_rtype = Wire(UInt()) // the next reg type of this slot (which might then get moved to a new slot)
   val updated_lrs2_rtype = Wire(UInt()) // the next reg type of this slot (which might then get moved to a new slot)
   val updated_lrs3_rtype = Wire(UInt())
   val updated_prs1_eidx  = Wire(UInt(width=VL_SZ.W))
   val updated_prs2_eidx  = Wire(UInt(width=VL_SZ.W))
   val updated_prs3_eidx  = Wire(UInt(width=VL_SZ.W))
   val updated_prs1_busy  = Wire(Bool())
   val updated_prs2_busy  = Wire(Bool())
   val updated_prs3_busy  = Wire(Bool())
   val updated_br_mask    = GetNewBrMask(io.brinfo, slotUop)

   val incr_eidx = slotUop.eidx + slotUop.rate
   val next_eidx = Mux(incr_eidx > io.vl, io.vl, incr_eidx) // The next eidx after this gets executed
                                                            // Updated_eidx may be set to this

   val next_incr_eidx = next_eidx + slotUop.rate
   val next_next_eidx = Mux(next_incr_eidx > io.vl, io.vl, next_incr_eidx)
   
   when (io.in_uop.valid)
   {
      slotUop := io.in_uop.bits
      assert(isInvalid || io.clear || io.kill)
   }
   .elsewhen (io.kill || io.clear)
   {
      slotUop.iw_state := s_invalid
   }
   .otherwise
   {
      slotUop.iw_state  := updated_state
      slotUop.eidx      := updated_eidx
      slotUop.lrs1_rtype:= updated_lrs1_rtype
      slotUop.lrs2_rtype:= updated_lrs2_rtype
      slotUop.lrs3_rtype:= updated_lrs3_rtype
      slotUop.prs1_busy := updated_prs1_busy
      slotUop.prs2_busy := updated_prs2_busy
      slotUop.prs3_busy := updated_prs3_busy
      slotUop.prs1_eidx := updated_prs1_eidx
      slotUop.prs2_eidx := updated_prs2_eidx
      slotUop.prs3_eidx := updated_prs3_eidx
      slotUop.br_mask   := updated_br_mask
   }

   //-----------------------------------------------------------------------------
   // "update" state
   // compute the next state for the micro-op in this slot. This micro-op may
   // be moved elsewhere, so the "updated_state" travels with it.

   // defaults
   updated_state       := slotUop.iw_state
   updated_uopc        := slotUop.uopc
   updated_eidx        := slotUop.eidx
   updated_lrs1_rtype  := slotUop.lrs1_rtype
   updated_lrs2_rtype  := slotUop.lrs2_rtype
   updated_lrs3_rtype  := slotUop.lrs3_rtype
   updated_prs1_busy   := slotUop.prs1_busy
   updated_prs2_busy   := slotUop.prs2_busy
   updated_prs3_busy   := slotUop.prs3_busy
   updated_prs1_eidx   := slotUop.prs1_eidx
   updated_prs2_eidx   := slotUop.prs2_eidx
   updated_prs3_eidx   := slotUop.prs3_eidx

   io.retire := false.B
   when (io.kill ||
         (io.grant && (slot_state === s_valid_1)) ||
         (io.grant && (slot_state === s_valid_2) && slot_p1 && slot_p2))
   {
      updated_state := s_invalid
      if (containsVec) {
         updated_eidx := next_eidx
         io.retire := true.B
         when (slotUop.vec_val && updated_eidx < io.vl && slotUop.uopc =/= uopVEXTRACT) {
            updated_state := slot_state
            io.retire := false.B
            when (slotUop.lrs1_rtype === RT_VEC) {
               updated_prs1_busy := next_next_eidx > slotUop.prs1_eidx
            }
            when (slotUop.lrs2_rtype === RT_VEC) {
               updated_prs2_busy := next_next_eidx > slotUop.prs2_eidx
            }
            when (slotUop.lrs3_rtype === RT_VEC) {
               updated_prs3_busy := next_next_eidx > slotUop.prs3_eidx
            }
         }
      } else {
         // This means we are in a issue queue which sends elements
         // sequentially to the vector pipeline

         // TODO_Vec: We can issue these before all operands are not busy
         // Maybe implement that in the future
         io.retire := false.B

         def sendToVec(rt: UInt): Bool = { rt === RT_FLT || rt === RT_FIX }
         when (slotUop.uopc === uopTOVEC && Array(slotUop.lrs1_rtype, slotUop.lrs2_rtype, slotUop.lrs3_rtype).map(p => sendToVec(p)).reduce(_||_)) {
            when (sendToVec(slotUop.lrs1_rtype)) {
               updated_lrs1_rtype := RT_X
               when (sendToVec(slotUop.lrs2_rtype) || sendToVec(slotUop.lrs3_rtype)) {
                  updated_state := slot_state
               }
            } .elsewhen (sendToVec(slotUop.lrs2_rtype)) {
               updated_lrs2_rtype := RT_X
               when (sendToVec(slotUop.lrs3_rtype)) {
                  updated_state := slot_state
               }
            } // This logic reissues the microop. We assume that any FLT operands are sent to vector pipeline
         }
      }
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
   } .otherwise {
      slotUop.eidx      := updated_eidx
      slotUop.prs1_busy := updated_prs1_busy
      slotUop.prs2_busy := updated_prs2_busy
      slotUop.prs3_busy := updated_prs3_busy
      slotUop.br_mask   := updated_br_mask
   }

   // TODO_Vec: Support vector chaining
   for (i <- 0 until num_slow_wakeup_ports)
   {
      when (io.wakeup_dsts(i).valid && (io.wakeup_dsts(i).bits.pdst === slotUop.pop1))
      {
         if (isVec) {
            when (slotUop.vec_val && slotUop.lrs1_rtype === RT_VEC) {
               updated_prs1_busy := Mux(io.grant, next_next_eidx, next_eidx) > io.wakeup_dsts(i).bits.eidx // TODO_Vec: Fix this somehow
               updated_prs1_eidx := io.wakeup_dsts(i).bits.eidx
            }
         } else {
            updated_prs1_busy := false.B
         }
      }
      when (io.wakeup_dsts(i).valid && (io.wakeup_dsts(i).bits.pdst === slotUop.pop2))
      {
         if (isVec) {
            when (slotUop.vec_val && slotUop.lrs2_rtype === RT_VEC) {
               updated_prs2_busy := Mux(io.grant, next_next_eidx, next_eidx) > io.wakeup_dsts(i).bits.eidx
               updated_prs2_eidx := io.wakeup_dsts(i).bits.eidx
            }
         } else {
            updated_prs2_busy := false.B
         }
      }
      when (io.wakeup_dsts(i).valid && (io.wakeup_dsts(i).bits.pdst === slotUop.pop3))
      {
         if (isVec) {
            when (slotUop.vec_val && slotUop.lrs3_rtype === RT_VEC) {
               updated_prs3_busy := Mux(io.grant, next_next_eidx, next_eidx) > io.wakeup_dsts(i).bits.eidx
               updated_prs3_eidx := io.wakeup_dsts(i).bits.eidx
            }
         } else {
            updated_prs3_busy := false.B
         }
      }
   }
   // Here we handle scalar vector operand comparisons, these are a form of "wakeup"
   if (isVec) {
      when (io.fromfp_valid) {
         when (io.fromfp_op_id === 0.U && slotUop.lrs1_rtype === RT_FLT) {
            updated_prs1_busy := false.B
         }
         when (io.fromfp_op_id === 1.U && slotUop.lrs2_rtype === RT_FLT) {
            updated_prs2_busy := false.B
         }
         when (io.fromfp_op_id === 2.U && slotUop.lrs3_rtype === RT_FLT) {
            updated_prs3_busy := false.B
         }
      }

      when (io.fromint_valid) {
         when (io.fromint_op_id === 0.U && slotUop.lrs1_rtype === RT_FIX) {
            updated_prs1_busy := false.B
         }
         when (io.fromint_op_id === 1.U && slotUop.lrs2_rtype === RT_FIX) {
            updated_prs2_busy := false.B
            when (slotUop.uopc === uopVEXTRACT) {
               updated_eidx := CalcEidxUpperMask(slotUop.rs1_vew, io.fromint_data)
            }
         }
         when (io.fromint_op_id === 2.U && slotUop.lrs3_rtype === RT_FIX) {
            updated_prs3_busy := false.B
         }
      }
   }


   // was this micro-op killed by a branch? if yes, we can't let it be valid if
   // we compact it into an other entry
   when (IsKilledByBranch(io.brinfo, slotUop))
   {
      updated_state := s_invalid
   }



   //-------------------------------------------------------------
   // Request Logic
   io.request := isValid && slot_p1 && slot_p2 && slot_p3 && !io.kill
   val high_priority = slotUop.is_br_or_jmp
//   io.request_hp := io.request && high_priority
   io.request_hp := false.B



   when (slot_state === s_valid_1)
   {
      io.request := slot_p1 && slot_p2 && slot_p3 && !io.kill
      if (containsVec){
         when (slotUop.is_load) {
            io.request := (slot_p1 && slot_p2 && slot_p3 && !io.kill)
         } .elsewhen (slotUop.is_store) {
            io.request := (slot_p1 && slot_p2 && slot_p3 && !io.kill
               && slotUop.stq_idx === io.lsu_stq_head)
         }
      }

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
   io.valid         := isValid
   io.uop           := slotUop
   if (containsVec) {
      io.will_be_valid := isValid &&
                          !(io.grant && (updated_eidx >= io.vl || !slotUop.vec_val)
                          && ((slot_state === s_valid_1) || (slot_state === s_valid_2) && slot_p1 && slot_p2))
   } else {
      io.will_be_valid := isValid &&
                          !(io.grant && slotUop.uopc =/= uopTOVEC
                          && ((slot_state === s_valid_1) || (slot_state === s_valid_2) && slot_p1 && slot_p2))
   }


   io.updated_uop           := slotUop
   io.updated_uop.iw_state  := updated_state
   io.updated_uop.uopc      := updated_uopc
   io.updated_uop.lrs1_rtype:= updated_lrs1_rtype
   io.updated_uop.lrs2_rtype:= updated_lrs2_rtype
   io.updated_uop.lrs3_rtype:= updated_lrs3_rtype
   io.updated_uop.br_mask   := updated_br_mask
   io.updated_uop.prs1_busy := updated_prs1_busy
   io.updated_uop.prs2_busy := updated_prs2_busy
   io.updated_uop.prs3_busy := updated_prs3_busy
   io.updated_uop.prs1_eidx := updated_prs1_eidx
   io.updated_uop.prs2_eidx := updated_prs2_eidx
   io.updated_uop.prs3_eidx := updated_prs3_eidx
   io.updated_uop.eidx      := updated_eidx

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

   override val compileOptions = chisel3.core.ExplicitCompileOptions.NotStrict.copy(explicitInvalidate = true)
}

