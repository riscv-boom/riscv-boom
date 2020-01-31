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
  val iss_uops   = Input(Vec(coreWidth, new MicroOp))
  val iss_valids = Input(Vec(coreWidth, Bool()))

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
  val io = IO(new RingRegisterReadIO)

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
  // read ports

  val prs1_addr_cols = Transpose(VecInit(io.iss_uops.map(_.op1_col)))
  val prs2_addr_cols = Transpose(VecInit(io.iss_uops.map(_.op2_col)))

  // Col -> Bank Address Crossbar
  for (w <- 0 until coreWidth) {
    io.rf_read_ports(w).prs1_addr := Mux1H(prs1_addr_cols(w), io.iss_uops.map(_.prs1))
    io.rf_read_ports(w).prs2_addr := Mux1H(prs2_addr_cols(w), io.iss_uops.map(_.prs2))
  }

  val rrd_rs1_data = Wire(Vec(coreWidth, Bits(xLen.W)))
  val rrd_rs2_data = Wire(Vec(coreWidth, Bits(xLen.W)))
  rrd_rs1_data := DontCare
  rrd_rs2_data := DontCare

  val prs1_data_banks = RegNext(Transpose(prs1_addr_cols))
  val prs2_data_banks = RegNext(Transpose(prs2_addr_cols))

  // Bank -> Col Data Crossbar
  for (w <- 0 until coreWidth) {
    rrd_rs1_data(w) := Mux1H(prs1_data_banks(w), io.rf_read_ports.map(_.prs1_data))
    rrd_rs2_data(w) := Mux1H(prs2_data_banks(w), io.rf_read_ports.map(_.prs2_data))
  }

  // Setup exe uops
  // TODO These registers and the bypassing logic should be moved to the new exu module
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

  val bypassed_rs1_data = Wire(Vec(coreWidth, Bits(xLen.W)))
  val bypassed_rs2_data = Wire(Vec(coreWidth, Bits(xLen.W)))

  for (w <- 0 until coreWidth) {
    bypassed_rs1_data(w) := Mux(rrd_uops(w).prs1_bypass, io.bypass.data(w), rrd_rs1_data(w))
    bypassed_rs2_data(w) := Mux(rrd_uops(w).prs2_bypass, io.bypass.data(w), rrd_rs2_data(w))
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Execute Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  for (w <- 0 until coreWidth) {
    exe_reg_rs1_data(w) := bypassed_rs1_data(w)
    exe_reg_rs2_data(w) := bypassed_rs2_data(w)
  }

  //-------------------------------------------------------------
  // set outputs to execute pipelines
  for (w <- 0 until coreWidth) {
    io.exe_reqs(w).valid    := exe_reg_valids(w)
    io.exe_reqs(w).bits.uop := exe_reg_uops(w)
    io.exe_reqs(w).bits.rs1_data := exe_reg_rs1_data(w)
    io.exe_reqs(w).bits.rs2_data := exe_reg_rs2_data(w)
  }
}
