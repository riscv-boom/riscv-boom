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
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import boom.common._

class IssueUnitBanked(
  params: IssueParams,
  numWakeupPorts: Int
)(implicit p: Parameters)
  extends IssueUnit(params, numWakeupPorts)
{
  val innerParams = params.copy(issueWidth = 1)

  val issue_units = (0 until params.issueWidth).map { w =>
    val u = Module(new IssueUnitCollapsing(innerParams, numWakeupPorts)).suggestName(s"col_${w}")
    u.io.wakeup_ports := io.wakeup_ports
    u.io.pred_wakeup_port := io.pred_wakeup_port
    u.io.child_rebusys := io.child_rebusys
    u.io.brupdate := io.brupdate
    u.io.flush_pipeline := io.flush_pipeline
    u.io.squash_grant := io.squash_grant
    u.io.tsc_reg := io.tsc_reg

    u.io.fu_types(0) := io.fu_types(w)
    u
  }

  for (w <- 0 until params.dispatchWidth) {
    val col_sel = if (enableColumnALUWrites) {
      require (params.iqType == IQ_ALU)
      require (isPow2(params.issueWidth))
      UIntToOH(io.dis_uops(w).bits.pdst(log2Ceil(params.issueWidth)-1,0))
    } else {
      val sel = RegInit((1 << (w % params.issueWidth)).U(params.issueWidth.W))
      sel := (sel << 1) | sel(params.issueWidth-1)
      sel
    }
    io.dis_uops(w).ready := (VecInit(issue_units.map(_.io.dis_uops(w).ready)).asUInt & col_sel) =/= 0.U

    for (i <- 0 until params.issueWidth) {
      issue_units(i).io.dis_uops(w).valid := col_sel(i) && io.dis_uops(w).valid
      issue_units(i).io.dis_uops(w).bits  := io.dis_uops(w).bits
    }

  }
  for (i <- 0 until params.issueWidth) {
    io.iss_uops(i) := issue_units(i).io.iss_uops(0)
  }
}
