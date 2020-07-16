//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util.{log2Ceil, PopCount}

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import FUConstants._
import boom.common._

/**
 * Specific type of issue unit
 *
 * @param params issue queue params
 * @param numWakeupPorts number of wakeup ports for the issue queue
 */
class IssueUnitCollapsing(
  params: IssueParams,
  numWakeupPorts: Int)
  (implicit p: Parameters)
  extends IssueUnit(params.numEntries, params.issueWidth, numWakeupPorts, params.iqType, params.dispatchWidth)
{
  //-------------------------------------------------------------
  // Figure out how much to shift entries by

  // Slow slots only shift 1 per cycle, these reduce critical path
  val nSlowSlots = params.numSlowEntries
  // Fast slots can shift up to dispatchWidth per cycle, so they can handle full dispatch throughput
  val nFastSlots = numIssueSlots - nSlowSlots

  require (nFastSlots >= dispatchWidth)
  require (nFastSlots <= numIssueSlots)


  val vacants = issue_slots.map(s => !(s.valid)) ++ io.dis_uops.map(_.valid).map(!_.asBool)
  val shamts_oh = Array.fill(numIssueSlots+dispatchWidth) {Wire(UInt(width=dispatchWidth.W))}
  // track how many to shift up this entry by by counting previous vacant spots
  def SaturatingCounterOH(count_oh:UInt, inc: Bool, max: Int): UInt = {
     val next = Wire(UInt(width=max.W))
     next := count_oh
     when (count_oh === 0.U && inc) {
       next := 1.U
     } .elsewhen (!count_oh(max-1) && inc) {
       next := (count_oh << 1.U)
     }
     next
  }
  shamts_oh(0) := 0.U
  for (i <- 1 until numIssueSlots + dispatchWidth) {
    val shift = if (i < nSlowSlots) (dispatchWidth min 1 + (i * (dispatchWidth-1)/nSlowSlots).toInt) else dispatchWidth
    shamts_oh(i) := SaturatingCounterOH(shamts_oh(i-1), vacants(i-1), shift)
  }

  //-------------------------------------------------------------

  // which entries' uops will still be next cycle? (not being issued and vacated)
  val will_be_valid = (0 until numIssueSlots).map(i => issue_slots(i).will_be_valid) ++
                      (0 until dispatchWidth).map(i => io.dis_uops(i).valid &&
                                                        !dis_uops(i).exception &&
                                                        !dis_uops(i).is_fence &&
                                                        !dis_uops(i).is_fencei)

  val uops = issue_slots.map(s=>s.out_uop) ++ dis_uops.map(s=>s)
  for (i <- 0 until numIssueSlots) {
    issue_slots(i).in_uop.valid := false.B
    issue_slots(i).in_uop.bits  := uops(i+1)
    for (j <- 1 to dispatchWidth by 1) {
      when (shamts_oh(i+j) === (1 << (j-1)).U) {
        issue_slots(i).in_uop.valid := will_be_valid(i+j)
        issue_slots(i).in_uop.bits  := uops(i+j)
      }
    }
    issue_slots(i).clear        := shamts_oh(i) =/= 0.U
  }

  //-------------------------------------------------------------
  // Dispatch/Entry Logic
  // did we find a spot to slide the new dispatched uops into?

  // Only look at the fast slots to determine readiness to dispatch.
  // Slow slot do not compact fast enough to make this calculation valid
  val is_available = Reg(Vec(nFastSlots, Bool()))
  is_available := VecInit((nSlowSlots until numIssueSlots).map(i =>
    (!issue_slots(i).will_be_valid || issue_slots(i).clear) && !(issue_slots(i).in_uop.valid)))
  for (w <- 0 until dispatchWidth) {
    io.dis_uops(w).ready := RegNext(PopCount(is_available) > w.U + PopCount(io.dis_uops.map(_.fire)))
    // io.dis_uops(w).ready := RegNext(PopCount(will_be_available) > w.U)
    assert (!io.dis_uops(w).ready || (shamts_oh(w+numIssueSlots) >> w) =/= 0.U)
  }

  //-------------------------------------------------------------
  // Issue Select Logic
  val requests = issue_slots.map(s => s.request)
  val port_issued = Array.fill(issueWidth){Bool()}
  for (w <- 0 until issueWidth) {
    port_issued(w) = false.B
  }

  val iss_select_mask = Array.ofDim[Boolean](issueWidth, numIssueSlots)
  if (params.useFullIssueSel) {
    for (w <- 0 until issueWidth) {
      for (i <- 0 until numIssueSlots) {
        iss_select_mask(w)(i) = true
      }
    }
  } else {
    for (w <- 0 until issueWidth) {
      for (i <- 0 until numIssueSlots) {
        iss_select_mask(w)(i) = (w % 2) == (i % 2)
      }
      iss_select_mask(w)(0) = true
    }
  }

  for (i <- 0 until numIssueSlots) {
    issue_slots(i).grant := false.B
    var uop_issued = false.B

    for (w <- 0 until issueWidth) {
      val can_allocate = ((issue_slots(i).iss_uop.bits.fu_code & io.fu_types(w)) =/= 0.U) && iss_select_mask(w)(i).B

      when (requests(i) && !uop_issued && can_allocate && !port_issued(w)) {
        issue_slots(i).grant := true.B
        io.iss_uops(w) := issue_slots(i).iss_uop
      }
      val was_port_issued_yet = port_issued(w)
      port_issued(w) = (requests(i) && !uop_issued && can_allocate) | port_issued(w)
      uop_issued = (requests(i) && can_allocate && !was_port_issued_yet) | uop_issued
    }
  }
}
