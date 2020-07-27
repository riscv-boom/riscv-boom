//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

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
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util._
import FUConstants._

class IssueSlotIO(val numWakeupPorts: Int)(implicit p: Parameters) extends BoomBundle
{
  val valid         = Output(Bool())
  val will_be_valid = Output(Bool()) // TODO code review, do we need this signal so explicitely?
  val request       = Output(Bool())
  val grant         = Input(Bool())
  val iss_uop       = Output(Valid(new MicroOp()))

  val in_uop        = Input(Valid(new MicroOp())) // if valid, this WILL overwrite an entry!
  val out_uop       = Output(new MicroOp())


  val brupdate      = Input(new BrUpdateInfo())
  val kill          = Input(Bool()) // pipeline flush
  val clear         = Input(Bool()) // entry being moved elsewhere (not mutually exclusive with grant)

  val ldspec_miss   = Input(Bool()) // Previous cycle's speculative load wakeup was mispredicted.
  val squash_grant  = Input(Bool())

  val wakeup_ports  = Flipped(Vec(numWakeupPorts, Valid(new ExeUnitResp(xLen))))
  val pred_wakeup_port = Flipped(Valid(UInt(log2Ceil(ftqSz).W)))
  val spec_ld_wakeup = Flipped(Vec(lsuWidth, Valid(UInt(width=maxPregSz.W))))
}

class IssueSlot(val numWakeupPorts: Int, val isMem: Boolean, val isFp: Boolean)(implicit p: Parameters)
  extends BoomModule
{
  val io = IO(new IssueSlotIO(numWakeupPorts))


  val slot_valid = RegInit(false.B)
  val slot_uop = Reg(new MicroOp())

  val next_valid = WireInit(slot_valid)
  val next_uop   = WireInit(UpdateBrMask(io.brupdate, slot_uop))

  val killed = IsKilledByBranch(io.brupdate, slot_uop)

  io.valid         := slot_valid
  io.out_uop       := next_uop
  io.will_be_valid := next_valid && !killed

  when (io.kill) {
    slot_valid := false.B
  } .elsewhen (io.in_uop.valid) {
    slot_valid := true.B
  } .elsewhen (io.clear) {
    slot_valid := false.B
  } .otherwise {
    slot_valid := next_valid && !killed
  }

  when (io.in_uop.valid) {
    slot_uop := io.in_uop.bits
    assert (!slot_valid || io.clear || io.kill)
  } .otherwise {
    slot_uop := next_uop
  }


  // Wakeups
  next_uop.iw_p1_poisoned := false.B
  next_uop.iw_p2_poisoned := false.B
  next_uop.iw_p1_bypass_hint := false.B
  next_uop.iw_p2_bypass_hint := false.B

  when (io.ldspec_miss && slot_uop.iw_p1_poisoned) {
    next_uop.prs1_busy := true.B
  }
  when (io.ldspec_miss && slot_uop.iw_p2_poisoned) {
    next_uop.prs2_busy := true.B
  }

  for (wakeup <- io.wakeup_ports) {
    when (wakeup.valid) {
      when (wakeup.bits.uop.pdst === slot_uop.prs1) {
        next_uop.prs1_busy := false.B
        next_uop.iw_p1_bypass_hint := wakeup.bits.uop.bypassable
      }
      when (wakeup.bits.uop.pdst === slot_uop.prs2) {
        next_uop.prs2_busy := false.B
        next_uop.iw_p2_bypass_hint := wakeup.bits.uop.bypassable
      }
      if (isFp)
        when (wakeup.bits.uop.pdst === slot_uop.prs3) { next_uop.prs3_busy := false.B }
    }
  }
  when (io.pred_wakeup_port.valid && io.pred_wakeup_port.bits === slot_uop.ppred) {
    next_uop.ppred_busy := false.B
  }

  for (spec_wakeup <- io.spec_ld_wakeup) {
    when (spec_wakeup.valid) {
      when (spec_wakeup.bits === slot_uop.prs1 && slot_uop.prs1_busy) {
        next_uop.prs1_busy := false.B
        next_uop.iw_p1_poisoned := true.B
        assert(!slot_uop.iw_p1_poisoned)
      }
      when (spec_wakeup.bits === slot_uop.prs2 && slot_uop.prs2_busy) {
        next_uop.prs2_busy := false.B
        next_uop.iw_p2_poisoned := true.B
        assert(!slot_uop.iw_p2_poisoned)
      }
    }
  }

  val agen_ready = (slot_valid && slot_uop.fu_code_is(FU_AGEN) &&
    !slot_uop.prs1_busy && !(slot_uop.ppred_busy && enableSFBOpt.B) && isMem.B)
  val dgen_ready = (slot_valid && slot_uop.fu_code_is(FU_DGEN) &&
    !slot_uop.prs2_busy && !(slot_uop.ppred_busy && enableSFBOpt.B) && isMem.B)

  io.request := (
    (slot_valid && !slot_uop.prs1_busy && !slot_uop.prs2_busy &&
      !(slot_uop.prs3_busy && isFp.B) &&
      !(slot_uop.ppred_busy && enableSFBOpt.B))
    || agen_ready
    || dgen_ready)

  io.iss_uop.valid := false.B
  io.iss_uop.bits  := slot_uop

  val squash_grant = ((io.ldspec_miss && (slot_uop.iw_p1_poisoned || slot_uop.iw_p2_poisoned)) ||
                      io.squash_grant)

  if (isMem) {
    when (slot_uop.fu_code === FU_STORE) {
      when (agen_ready) {
        when (io.grant && !squash_grant) {
          next_uop.fu_code      := FU_DGEN
        }
        io.iss_uop.bits.fu_code := FU_AGEN
      } .otherwise {
        when (io.grant && !squash_grant) {
          next_uop.fu_code      := FU_AGEN
        }
        io.iss_uop.bits.fu_code    := FU_DGEN
        io.iss_uop.bits.prs1       := slot_uop.prs2
        io.iss_uop.bits.lrs1_rtype := slot_uop.lrs2_rtype
        io.iss_uop.bits.iw_p1_bypass_hint := slot_uop.iw_p2_bypass_hint
      }
    } .elsewhen (slot_uop.fu_code === FU_DGEN) {
      io.iss_uop.bits.prs1       := slot_uop.prs2
      io.iss_uop.bits.lrs1_rtype := slot_uop.lrs2_rtype
      io.iss_uop.bits.iw_p1_bypass_hint := slot_uop.iw_p2_bypass_hint
    }
    io.iss_uop.bits.lrs2_rtype := RT_X
    io.iss_uop.bits.prs2       := io.iss_uop.bits.prs1 // helps with DCE
  }


  // Update state for current micro-op based on grant

  when (io.grant && !squash_grant) {
    next_valid := slot_uop.fu_code_is(FU_STORE) && isMem.B
    io.iss_uop.valid := true.B
  }
}
