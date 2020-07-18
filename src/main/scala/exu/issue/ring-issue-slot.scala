//******************************************************************************
// Copyright (c) 2015 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Ring Microarchitecture Issue Slot
//--------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util._
import FUConstants._

class FastWakeup(implicit p: Parameters) extends BoomBundle
{
  val pdst   = UInt(ipregSz.W)
  val status = UInt(operandStatusSz.W)
  val alu    = Bool()
}

/**
 * IO bundle to interact with Issue slot
 *
 * @param numWakeupPorts number of wakeup ports for the slot
 */
class RingIssueSlotIO(implicit p: Parameters) extends BoomBundle
{
  val valid         = Output(Bool())
  val will_be_valid = Output(Bool())
  val request       = Output(Bool())
  val request_chain = Output(Bool())
  val grant         = Input(Bool())

  val fu_avail      = Input(UInt(FUC_SZ.W))

  val brupdate      = Input(new BrUpdateInfo)
  val kill          = Input(Bool()) // pipeline flush
  val clear         = Input(Bool()) // entry being moved elsewhere (not mutually exclusive with grant)

  val fast_wakeup   = Input(Valid(new FastWakeup))
  val chain_wakeup  = Input(Valid(UInt(ipregSz.W)))
  val slow_wakeups  = Input(Vec(2       , Valid(UInt(ipregSz.W))))
  val load_wakeups  = Input(Vec(memWidth, Valid(UInt(ipregSz.W))))
  val load_nacks    = Input(Vec(memWidth, Bool()))
  val ll_wakeups    = Input(Vec(memWidth, Valid(UInt(ipregSz.W))))
  val pred_wakeup   = Input(Valid(UInt(ftqSz.W)))

  val in_uop        = Flipped(Valid(new MicroOp)) // Received from dispatch or another slot during compaction
  val out_uop       = Output(new MicroOp) // The updated slot uop; will be shifted upwards in a collasping queue
  val uop           = Output(new MicroOp) // The current slot's uop. Sent down the pipeline when issued

  val debug = {
    val result = new Bundle {
      val p1 = Bool()
      val p2 = Bool()
      val state = UInt(width=2.W)
    }
    Output(result)
  }
}

/**
 * Single issue slot. Holds a uop within the issue queue
 *
 * @param numWakeupPorts number of wakeup ports
 */
class RingIssueSlot(implicit p: Parameters)
  extends BoomModule
  with IssueUnitConstants
{
  //----------------------------------------------------------------------------------------------------
  // Helpers

  def wakeup(uop: MicroOp,
             fwu: Valid[FastWakeup],
             cwu: Valid[UInt],
             pwu: Valid[UInt],
             swu: Vec[Valid[UInt]],
             lwu: Vec[Valid[UInt]],
             llw: Vec[Valid[UInt]],
             ldn: Vec[Bool]): MicroOp = {

    val woke_uop = Wire(new MicroOp)
    woke_uop := uop

    val fwu_prs1 = fwu.bits.pdst === uop.prs1 && fwu.valid
    val fwu_prs2 = fwu.bits.pdst === uop.prs2 && fwu.valid

    val cwu_prs2 = cwu.bits === uop.prs2 && cwu.valid

    val lwu_prs1_hits = VecInit(lwu.map(wu => wu.bits === uop.prs1 && wu.valid && uop.lrs1_rtype === RT_FIX))
    val lwu_prs2_hits = VecInit(lwu.map(wu => wu.bits === uop.prs2 && wu.valid && uop.lrs2_rtype === RT_FIX))
    val lwu_prs1 = lwu_prs1_hits.reduce(_||_)
    val lwu_prs2 = lwu_prs2_hits.reduce(_||_)

    val swu_prs1 = (swu ++ llw).map(wu => wu.bits === uop.prs1 && wu.valid).reduce(_||_)
    val swu_prs2 = (swu ++ llw).map(wu => wu.bits === uop.prs2 && wu.valid).reduce(_||_)

    woke_uop.prs1_status := ( uop.prs1_status >> 1
                            | uop.prs1_status & 1.U
                            | Mux(fwu_prs1, fwu.bits.status, 0.U)
                            | Mux(lwu_prs1,             2.U, 0.U)
                            | Mux(swu_prs1,             1.U, 0.U) )
    woke_uop.prs2_status := ( uop.prs2_status >> 1
                            | uop.prs2_status & 1.U
                            | Mux(fwu_prs2, fwu.bits.status, 0.U)
                            | Mux(cwu_prs2,             1.U, 0.U)
                            | Mux(lwu_prs2,             2.U, 0.U)
                            | Mux(swu_prs2,             1.U, 0.U) )

    woke_uop.prs1_can_bypass_alu := uop.prs1_can_bypass_alu || fwu_prs1 && fwu.bits.alu
    woke_uop.prs2_can_bypass_alu := uop.prs2_can_bypass_alu || fwu_prs2 && fwu.bits.alu

    woke_uop.prs1_can_bypass_mem := (uop.prs1_can_bypass_mem.asUInt | lwu_prs1_hits.asUInt).asBools
    woke_uop.prs2_can_bypass_mem := (uop.prs2_can_bypass_mem.asUInt | lwu_prs2_hits.asUInt).asBools

    // Reset status to zero if woken up last cycle by a load which missed
    when ((uop.prs1_bypass_mem.asUInt & ldn.asUInt).orR) { woke_uop.prs1_status := 0.U }
    when ((uop.prs2_bypass_mem.asUInt & ldn.asUInt).orR) { woke_uop.prs2_status := 0.U }

    // Wakeup predicate
    when (pwu.bits === uop.ppred && pwu.valid) { woke_uop.ppred_busy := false.B }

    woke_uop
  }


  //----------------------------------------------------------------------------------------------------
  // Signals

  val io = IO(new RingIssueSlotIO)

  // slot invalid?
  // slot is valid, holding 1 uop
  // slot is valid, holds 2 uops (like a store)
  def is_invalid = state === s_invalid
  def is_valid = state =/= s_invalid

  val state           = RegInit(s_invalid)
  val next_state      = Wire(UInt()) // the next state of this slot (which might then get moved to a new slot)
  val next_uopc       = Wire(UInt()) // the next uopc of this slot (which might then get moved to a new slot)
  val next_lrs1_rtype = Wire(UInt()) // the next reg type of this slot (which might then get moved to a new slot)
  val next_lrs2_rtype = Wire(UInt()) // the next reg type of this slot (which might then get moved to a new slot)

  val slot_uop = RegInit(NullMicroOp)
  val next_uop = Mux(!is_valid || io.clear, io.in_uop.bits, io.out_uop)
  val woke_uop = wakeup(next_uop, io.fast_wakeup, io.chain_wakeup, io.pred_wakeup, io.slow_wakeups, io.load_wakeups, io.ll_wakeups, io.load_nacks)

  val p1 = slot_uop.prs1_ready
  val p2 = slot_uop.prs2_ready

  //----------------------------------------------------------------------------------------------------
  // next slot state computation
  // compute the next state for THIS entry slot (in a collasping queue, the
  // current uop may get moved elsewhere, and a new uop can enter

  when (io.kill) {
    state := s_invalid
  } .elsewhen (!is_valid || io.clear) {
    state := io.in_uop.bits.iw_state
  } .elsewhen (io.clear) {
    state := s_invalid
  } .otherwise {
    state := next_state
  }

  //----------------------------------------------------------------------------------------------------
  // "update" state
  // compute the next state for the micro-op in this slot. This micro-op may
  // be moved elsewhere, so the "next_state" travels with it.

  // defaults
  next_state := state
  next_uopc := slot_uop.uopc
  next_lrs1_rtype := slot_uop.lrs1_rtype
  next_lrs2_rtype := slot_uop.lrs2_rtype

  when (io.kill) {
    next_state := s_invalid
  } .elsewhen (io.grant && ((state === s_valid_1) || (state === s_valid_2) && p1 && p2 || (state === s_valid_3))) {
    next_state := s_invalid
  } .elsewhen (io.grant && (state === s_valid_2)) {
    next_state := s_valid_1
    when (p1) {
      next_uopc := uopSTD
      next_lrs1_rtype := RT_X
    } .otherwise {
      next_lrs2_rtype := RT_X
    }
  }

  slot_uop := woke_uop

  when (io.in_uop.valid) {
    assert (is_invalid || io.clear || io.kill, "trying to overwrite a valid issue slot.")
  }

  // Handle branch misspeculations
  val next_br_mask = GetNewBrMask(io.brupdate, slot_uop)

  // was this micro-op killed by a branch? if yes, we can't let it be valid if
  // we compact it into an other entry
  when (IsKilledByBranch(io.brupdate, slot_uop)) {
    next_state := s_invalid
  }

  //----------------------------------------------------------------------------------------------------
  // Request Logic

  val can_request = (io.fu_avail & slot_uop.fu_code).orR && slot_uop.ppred_ready

  when (state === s_valid_1) {
    io.request := p1 && p2 && can_request
  } .elsewhen (state === s_valid_2) {
    io.request := (p1 || p2) && can_request
  } .otherwise {
    io.request := false.B
  }

  when (state === s_valid_3) {
    io.request_chain := slot_uop.prs2_status(1,0).orR
  } .otherwise {
    io.request_chain := false.B
  }

  //----------------------------------------------------------------------------------------------------
  // Assign Outputs

  io.valid := is_valid
  io.uop := slot_uop

  // micro-op will vacate due to grant.
  val may_vacate = io.grant && ((state === s_valid_1) || (state === s_valid_2) && p1 && p2)
  io.will_be_valid := is_valid && !may_vacate

  io.out_uop            := slot_uop
  io.out_uop.iw_state   := next_state
  io.out_uop.uopc       := next_uopc
  io.out_uop.lrs1_rtype := next_lrs1_rtype
  io.out_uop.lrs2_rtype := next_lrs2_rtype
  io.out_uop.br_mask    := next_br_mask

  when (state === s_valid_2) {
    when (p1 && p2) {
      ; // send out the entire instruction as one uop
    } .elsewhen (p1) {
      io.uop.uopc := slot_uop.uopc
      io.uop.lrs2_rtype := RT_X
    } .elsewhen (p2) {
      io.uop.uopc := uopSTD
      io.uop.lrs1_rtype := RT_X
    }
  }

  // debug outputs
  io.debug.p1 := p1
  io.debug.p2 := p2
  io.debug.state := state
}
