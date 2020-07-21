//******************************************************************************
// Copyright (c) 2012 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Register Read
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util._

import boom.common._
import boom.util._

import boom.exu.FUConstants._

/**
 * Handle the register read and bypass network for the OoO backend
 * interfaces with the issue window on the enqueue side, and the execution
 * pipelines on the dequeue side.
 *
 */
class RegisterRead(
  issueWidth: Int,
  numTotalReadPorts: Int,
  numReadPortsArray: Seq[Int],
                        // each exe_unit must tell us how many max
                        // operands it can accept (the sum should equal
                        // numTotalReadPorts)
  numTotalBypassPorts: Int,
  numTotalPredBypassPorts: Int,
  registerWidth: Int
)(implicit p: Parameters) extends BoomModule
{
  val io = IO(new Bundle {
    // issued micro-ops
    val iss_uops   = Input(Vec(issueWidth, Valid(new MicroOp())))

    // interface with register file's read ports
    val rf_read_ports = Flipped(Vec(numTotalReadPorts, new RegisterFileReadPortIO(maxPregSz, registerWidth)))
    val prf_read_ports = Flipped(Vec(issueWidth, new RegisterFileReadPortIO(log2Ceil(ftqSz), 1)))
    val immrf_read_ports = Flipped(Vec(issueWidth, new RegisterFileReadPortIO(immPregSz, LONGEST_IMM_SZ)))
    // immediate pregs are unbusied after they are read
    val imm_wakeups = Output(Vec(issueWidth, Valid(new ExeUnitResp(1))))

    val bypass = Input(Vec(numTotalBypassPorts, Valid(new ExeUnitResp(registerWidth))))
    val pred_bypass = Input(Vec(numTotalPredBypassPorts, Valid(new ExeUnitResp(1))))

    // send micro-ops to the execution pipelines
    val exe_reqs = Vec(issueWidth, Output(Valid(new FuncUnitReq(registerWidth))))

    val kill   = Input(Bool())
    val brupdate = Input(new BrUpdateInfo())
  })

  val rrd_valids       = Reg(Vec(issueWidth, Bool()))
  val rrd_uops         = Reg(Vec(issueWidth, new MicroOp()))

  val exe_reg_valids   = RegInit(VecInit(Seq.fill(issueWidth) { false.B }))
  val exe_reg_uops     = Reg(Vec(issueWidth, new MicroOp()))
  val exe_reg_rs1_data = Reg(Vec(issueWidth, Bits(registerWidth.W)))
  val exe_reg_rs2_data = Reg(Vec(issueWidth, Bits(registerWidth.W)))
  val exe_reg_rs3_data = Reg(Vec(issueWidth, Bits(registerWidth.W)))
  val exe_reg_pred_data = Reg(Vec(issueWidth, Bool()))
  val exe_reg_imm_data = Reg(Vec(issueWidth, UInt(xLen.W)))

  //-------------------------------------------------------------
  // hook up inputs

  for (w <- 0 until issueWidth) {
    rrd_valids(w) := io.iss_uops(w).valid && !IsKilledByBranch(io.brupdate, io.iss_uops(w).bits)
    rrd_uops(w)   := GetNewUopAndBrMask(io.iss_uops(w).bits, io.brupdate)
  }

  //-------------------------------------------------------------
  // read ports

  require (numTotalReadPorts == numReadPortsArray.reduce(_+_))

  val rrd_rs1_data   = Wire(Vec(issueWidth, Bits(registerWidth.W)))
  val rrd_rs2_data   = Wire(Vec(issueWidth, Bits(registerWidth.W)))
  val rrd_rs3_data   = Wire(Vec(issueWidth, Bits(registerWidth.W)))
  val rrd_pred_data  = Wire(Vec(issueWidth, Bool()))
  val rrd_imm_data   = Wire(Vec(issueWidth, UInt(LONGEST_IMM_SZ.W)))
  rrd_rs1_data := DontCare
  rrd_rs2_data := DontCare
  rrd_rs3_data := DontCare
  rrd_pred_data := DontCare

  io.prf_read_ports := DontCare

  var idx = 0 // index into flattened read_ports array
  for (w <- 0 until issueWidth) {
    val numReadPorts = numReadPortsArray(w)

    // NOTE:
    // rrdLatency==1, we need to send read address at end of ISS stage,
    //    in order to get read data back at end of RRD stage.

    val rs1_addr = io.iss_uops(w).bits.prs1
    val rs2_addr = io.iss_uops(w).bits.prs2
    val rs3_addr = io.iss_uops(w).bits.prs3
    val pred_addr = io.iss_uops(w).bits.ppred
    val imm_addr = io.iss_uops(w).bits.pimm

    if (numReadPorts > 0) io.rf_read_ports(idx+0).addr := rs1_addr
    if (numReadPorts > 1) io.rf_read_ports(idx+1).addr := rs2_addr
    if (numReadPorts > 2) io.rf_read_ports(idx+2).addr := rs3_addr

    if (enableSFBOpt) io.prf_read_ports(w).addr := pred_addr
    io.immrf_read_ports(w).addr := imm_addr

    if (numReadPorts > 0) rrd_rs1_data(w) := io.rf_read_ports(idx+0).data
    if (numReadPorts > 1) rrd_rs2_data(w) := io.rf_read_ports(idx+1).data
    if (numReadPorts > 2) rrd_rs3_data(w) := io.rf_read_ports(idx+2).data

    if (enableSFBOpt) rrd_pred_data(w) := Mux(RegNext(io.iss_uops(w).bits.is_sfb_shadow), io.prf_read_ports(w).data, false.B)
    rrd_imm_data(w) := io.immrf_read_ports(w).data

    val rrd_kill = io.kill || IsKilledByBranch(io.brupdate, rrd_uops(w))

    exe_reg_valids(w) := Mux(rrd_kill, false.B, rrd_valids(w))
    // TODO use only the valids signal, don't require us to set nullUop
    exe_reg_uops(w)   := Mux(rrd_kill, NullMicroOp, rrd_uops(w))

    exe_reg_uops(w).br_mask := GetNewBrMask(io.brupdate, rrd_uops(w))

    idx += numReadPorts
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // BYPASS MUXES -----------------------------------------------
  // performed at the end of the register read stage

  // NOTES: this code is fairly hard-coded. Sorry.
  // ASSUMPTIONS:
  //    - rs3 is used for FPU ops which are NOT bypassed (so don't check
  //       them!).
  //    - only bypass integer registers.
  val bypass_data = io.bypass.map(_.bits.data)
  val pred_bypass_data = io.pred_bypass.map(_.bits.data)

  for (w <- 0 until issueWidth) {
    val numReadPorts = numReadPortsArray(w)
    val rs1_bypass_hits  = Wire(Vec(numTotalBypassPorts, Bool()))
    val rs2_bypass_hits  = Wire(Vec(numTotalBypassPorts, Bool()))
    val pred_bypass_hits = Wire(Vec(numTotalPredBypassPorts, Bool()))

    val prs1       = rrd_uops(w).prs1
    val lrs1_rtype = rrd_uops(w).lrs1_rtype
    val prs2       = rrd_uops(w).prs2
    val lrs2_rtype = rrd_uops(w).lrs2_rtype
    val prs3       = rrd_uops(w).prs3
    val ppred      = rrd_uops(w).ppred

    for (b <- 0 until numTotalBypassPorts)
    {
      val bypass = io.bypass(b)
      rs1_bypass_hits(b) := bypass.valid && prs1 === bypass.bits.uop.pdst && bypass.bits.uop.rf_wen && bypass.bits.uop.dst_rtype === RT_FIX
      rs2_bypass_hits(b) := bypass.valid && prs2 === bypass.bits.uop.pdst && bypass.bits.uop.rf_wen && bypass.bits.uop.dst_rtype === RT_FIX
    }

    for (b <- 0 until numTotalPredBypassPorts)
    {
      val bypass = io.pred_bypass(b)
      pred_bypass_hits(b) := bypass.valid && (ppred === bypass.bits.uop.pdst) && bypass.bits.uop.is_sfb_br
    }
    exe_reg_rs1_data(w)  := rrd_rs1_data(w)
    exe_reg_rs2_data(w)  := rrd_rs2_data(w)
    exe_reg_rs3_data(w)  := rrd_rs3_data(w)
    exe_reg_pred_data(w) := rrd_pred_data(w)
    exe_reg_imm_data(w)  := Mux(rrd_uops(w).imm_sel === IS_SH,
      Sext(rrd_uops(w).pimm, xLen),
      Sext(ImmGen(rrd_imm_data(w), rrd_uops(w).imm_sel), xLen)
    )
    if (numTotalBypassPorts > 0) {
      when (rs1_bypass_hits.reduce(_||_)) {
        exe_reg_rs1_data(w)  := Mux1H(rs1_bypass_hits, bypass_data)
      }
      when (rs2_bypass_hits.reduce(_||_)) {
        exe_reg_rs2_data(w)  := Mux1H(rs2_bypass_hits, bypass_data)
      }
    }
    if (numTotalPredBypassPorts > 0) {
      when (pred_bypass_hits.reduce(_||_)) {
        exe_reg_pred_data(w) := Mux1H(pred_bypass_hits, pred_bypass_data)
      }
    }
    when (lrs1_rtype === RT_ZERO) { exe_reg_rs1_data(w) := 0.U }
    when (lrs2_rtype === RT_ZERO) { exe_reg_rs2_data(w) := 0.U }
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Execute Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------


  //-------------------------------------------------------------
  // set outputs to execute pipelines
  for (w <- 0 until issueWidth) {
    val numReadPorts = numReadPortsArray(w)

    io.exe_reqs(w).valid    := exe_reg_valids(w)
    io.exe_reqs(w).bits.uop := exe_reg_uops(w)
    io.exe_reqs(w).bits.rs1_data := exe_reg_rs1_data(w)
    io.exe_reqs(w).bits.rs2_data := exe_reg_rs2_data(w)
    io.exe_reqs(w).bits.rs3_data := exe_reg_rs3_data(w)
    io.exe_reqs(w).bits.pred_data := exe_reg_pred_data(w)
    io.exe_reqs(w).bits.imm_data := exe_reg_imm_data(w)

    io.imm_wakeups(w).valid := ( exe_reg_valids(w) &&
                                !exe_reg_uops(w).fu_code_is(FU_DGEN) &&
                                !exe_reg_uops(w).imm_sel.isOneOf(IS_N, IS_SH))
    io.imm_wakeups(w).bits     := DontCare
    io.imm_wakeups(w).bits.uop := exe_reg_uops(w)
  }
}
