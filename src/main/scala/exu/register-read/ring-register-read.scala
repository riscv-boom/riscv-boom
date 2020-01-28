//******************************************************************************
// Copyright (c) 2012 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Ring Microarchitecture Register Read
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util._

class RingRegisterReadIO
  (implicit p: Parameters) extends BoomBundle
{
  // issued micro-ops
  val iss_uops = Input(Vec(coreWidth, Valid(new MicroOp)))

  // interface with register file's read ports
  val rf_read_ports = Flipped(Vec(coreWidth, new BankReadPort(ipregSz, xLen)))

  val bypass = Input(new BypassData(coreWidth, xLen))

  // send micro-ops to the execution pipelines
  val exe_reqs = Vec(coreWidth, new DecoupledIO(new FuncUnitReq(xLen)))

  val kill   = Input(Bool())
  val brinfo = Input(new BrResolutionInfo)
}

/**
 * Handle the register read and bypass network for the OoO backend
 * interfaces with the issue window on the enqueue side, and the execution
 * pipelines on the dequeue side.
 *
 * @param supportedUnitsArray seq of SupportedFuncUnits classes indicating what the functional units do
 */
class RingRegisterRead(supportedUnitsArray: Seq[SupportedFuncUnits])
  (implicit p: Parameters) extends BoomModule
{
  val io = IO(new RegisterReadIO(numReadPortsPerColumn))

  val rrd_valids       = Wire(Vec(coreWidth, Bool()))
  val rrd_uops         = Wire(Vec(coreWidth, new MicroOp))

  val exe_reg_valids   = RegInit(VecInit(Seq.fill(coreWidth) { false.B }))
  val exe_reg_uops     = Reg(Vec(coreWidth, new MicroOp))
  val exe_reg_rs1_data = Reg(Vec(coreWidth, Bits(xLen.W)))
  val exe_reg_rs2_data = Reg(Vec(coreWidth, Bits(xLen.W)))

  //-------------------------------------------------------------
  // hook up inputs

  // TODO wouldn't it be better to put rrdd after the registers?
  for (w <- 0 until coreWidth) {
    val rrd_decode_unit = Module(new RegisterReadDecode(supportedUnitsArray(w)))
    rrd_decode_unit.io.iss_valid := io.iss_valids(w)
    rrd_decode_unit.io.iss_uop   := io.iss_uops(w)

    rrd_valids(w) := RegNext(rrd_decode_unit.io.rrd_valid &&
                     !IsKilledByBranch(io.brinfo, rrd_decode_unit.io.rrd_uop))
    rrd_uops(w)   := RegNext(GetNewUopAndBrMask(rrd_decode_unit.io.rrd_uop, io.brinfo))
  }

  //-------------------------------------------------------------
  // read ports TODO: rewrite this as crossbar

  val prs1_addr_cols = Wire(Vec(coreWidth, UInt(coreWidth.W)))
  val prs2_addr_cols = Wire(Vec(coreWidth, UInt(coreWidth.W)))

  prs1_addr_cols := Transpose(io.iss_uops.map(_.op1_col))
  prs2_addr_cols := Transpose(io.iss_uops.map(_.op2_col))

  val rrd_rs1_data = Wire(Vec(coreWidth, Bits(xLen.W)))
  val rrd_rs2_data = Wire(Vec(coreWidth, Bits(xLen.W)))
  rrd_rs1_data := DontCare
  rrd_rs2_data := DontCare

  // Col -> Bank Address Crossbar
  for (w <- 0 until coreWidth) {
    io.rf_read_ports(w).prs1_addr := Mux1H(prs1_addr_col(w), io.iss_uops.map(_.prs1))
    io.rf_read_ports(w).prs2_addr := Mux1H(prs2_addr_col(w), io.iss_uops.map(_.prs2))

    rrd_rs1_data(w) := io.rf_read_ports(w).prs1_data
    rrd_rs2_data(w) := io.rf_read_ports(w).prs2_data
  }

  // Setup exe uops
  for (w <- 0 until coreWidth) {
    val rrd_kill = io.kill || IsKilledByBranch(io.brinfo, rrd_uops(w))

    exe_reg_valids(w) := !rrd_kill && rrd_valids(w)
    // TODO use only the valids signal, don't require us to set nullUop
    // why is it like this in the first place?
    exe_reg_uops(w)   := Mux(rrd_kill, NullMicroOp, rrd_uops(w))

    exe_reg_uops(w).br_mask := GetNewBrMask(io.brinfo, rrd_uops(w))
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // BYPASS MUXES -----------------------------------------------
  // performed at the end of the register read stage TODO: do this in the ring-style

  // NOTES: this code is fairly hard-coded. Sorry.
  // ASSUMPTIONS:
  //    - rs3 is used for FPU ops which are NOT bypassed (so don't check
  //       them!).
  //    - only bypass integer registers.

  val bypassed_rs1_data = Wire(Vec(issueWidth, Bits(registerWidth.W)))
  val bypassed_rs2_data = Wire(Vec(issueWidth, Bits(registerWidth.W)))

  for (w <- 0 until issueWidth) {
    val numReadPorts = numReadPortsArray(w)
    var rs1_cases = Array((false.B, 0.U(registerWidth.W)))
    var rs2_cases = Array((false.B, 0.U(registerWidth.W)))

    val prs1       = rrd_uops(w).prs1
    val lrs1_rtype = rrd_uops(w).lrs1_rtype
    val prs2       = rrd_uops(w).prs2
    val lrs2_rtype = rrd_uops(w).lrs2_rtype

    for (b <- 0 until io.bypass.getNumPorts)
    {
      // can't use "io.bypass.valid(b) since it would create a combinational loop on branch kills"
      rs1_cases ++= Array((io.bypass.valid(b) && (prs1 === io.bypass.uop(b).pdst) && io.bypass.uop(b).rf_wen
        && io.bypass.uop(b).dst_rtype === RT_FIX && lrs1_rtype === RT_FIX && (prs1 =/= 0.U), io.bypass.data(b)))
      rs2_cases ++= Array((io.bypass.valid(b) && (prs2 === io.bypass.uop(b).pdst) && io.bypass.uop(b).rf_wen
        && io.bypass.uop(b).dst_rtype === RT_FIX && lrs2_rtype === RT_FIX && (prs2 =/= 0.U), io.bypass.data(b)))
    }

    if (numReadPorts > 0) bypassed_rs1_data(w) := MuxCase(rrd_rs1_data(w), rs1_cases)
    if (numReadPorts > 1) bypassed_rs2_data(w) := MuxCase(rrd_rs2_data(w), rs2_cases)
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Execute Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  for (w <- 0 until issueWidth) {
    val numReadPorts = numReadPortsArray(w)
    if (numReadPorts > 0) exe_reg_rs1_data(w) := bypassed_rs1_data(w)
    if (numReadPorts > 1) exe_reg_rs2_data(w) := bypassed_rs2_data(w)
    if (numReadPorts > 2) exe_reg_rs3_data(w) := rrd_rs3_data(w)
    // ASSUMPTION: rs3 is FPU which is NOT bypassed
  }
  // TODO add assert to detect bypass conflicts on non-bypassable things
  // TODO add assert that checks bypassing to verify there isn't something it hits rs3

  //-------------------------------------------------------------
  // set outputs to execute pipelines
  for (w <- 0 until issueWidth) {
    val numReadPorts = numReadPortsArray(w)

    io.exe_reqs(w).valid    := exe_reg_valids(w)
    io.exe_reqs(w).bits.uop := exe_reg_uops(w)
    if (numReadPorts > 0) io.exe_reqs(w).bits.rs1_data := exe_reg_rs1_data(w)
    if (numReadPorts > 1) io.exe_reqs(w).bits.rs2_data := exe_reg_rs2_data(w)
    if (numReadPorts > 2) io.exe_reqs(w).bits.rs3_data := exe_reg_rs3_data(w)
  }
}
