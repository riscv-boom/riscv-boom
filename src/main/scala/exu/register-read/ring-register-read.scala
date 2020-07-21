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
  val rf_read_ports  = Flipped(Vec(coreWidth, new BankReadPorts(log2Ceil(numIntPhysRegs/coreWidth), xLen)))
  val prf_read_ports = Flipped(Vec(coreWidth, new RegisterFileReadPortIO(ftqSz, 1)))

  // send micro-ops to the execution pipelines
  val exe_reqs = Vec(coreWidth, new DecoupledIO(new FuncUnitReq(xLen)))

  val kill     = Input(Bool())
  val brupdate = Input(new BrUpdateInfo)
}

/**
 * @param supportedUnitsArray seq of SupportedFuncUnits classes indicating what the functional units do
 */
class RingRegisterRead(implicit p: Parameters) extends BoomModule
{
  val io = IO(new RingRegisterReadIO)

  val rrd_valids       = Wire(Vec(coreWidth, Bool()))
  val rrd_uops         = Wire(Vec(coreWidth, new MicroOp))

  //-------------------------------------------------------------
  // hook up inputs

  // TODO wouldn't it be better to put rrdd after the registers?
  for (w <- 0 until coreWidth) {
    val supportedUnits = new SupportedFuncUnits(
                           alu  = true ,
                           jmp  = true ,
                           mem  = true ,
                           muld = true ,
                           csr  = true ,
                           fpu  = false,
                           fdiv = false,
                           ifpu = usingFPU)

    val rrd_decode_unit = Module(new RegisterReadDecode(supportedUnits))
    rrd_decode_unit.io.iss_valid := io.iss_valids(w)
    rrd_decode_unit.io.iss_uop   := io.iss_uops(w)

    rrd_valids(w) := RegNext(rrd_decode_unit.io.rrd_valid &&
                     !IsKilledByBranch(io.brupdate, rrd_decode_unit.io.rrd_uop) && !io.kill)
    rrd_uops(w)   := RegNext(GetNewUopAndBrMask(rrd_decode_unit.io.rrd_uop, io.brupdate))
  }

  //-------------------------------------------------------------
  // read ports

  io.prf_read_ports := DontCare

  val addr_xbar_reqs = Transpose(io.iss_uops.map(u => Seq(u.prs1_port, u.prs2_port)).reduce(_++_))
  val specifiers     = io.iss_uops.map(u => Seq(u.prs1, u.prs2)).reduce(_++_)

  // Col -> Port Address Crossbar
  for (w <- 0 until coreWidth) {
    for (p <- 0 until numIrfReadPortsPerBank) {
      val n = numIrfReadPortsPerBank
      io.rf_read_ports(w).addr(p) := Mux1H(addr_xbar_reqs(n*w + p), specifiers)
    }

    if (enableSFBOpt) io.prf_read_ports(w).addr := io.iss_uops(w).ppred
  }

  val rrd_rs1_data  = Wire(Vec(coreWidth, Bits(xLen.W)))
  val rrd_rs2_data  = Wire(Vec(coreWidth, Bits(xLen.W)))
  val rrd_pred_data = Wire(Vec(coreWidth, Bool()))
  rrd_pred_data    := DontCare

  // Port -> Col Data Crossbar
  for (w <- 0 until coreWidth) {
    val irf_port_data = io.rf_read_ports.map(_.data.toSeq).reduce(_++_)

    rrd_rs1_data(w) := Mux(rrd_uops(w).prs1 === 0.U,
                         0.U,
                         Mux1H(rrd_uops(w).prs1_port, irf_port_data))
    rrd_rs2_data(w) := Mux(rrd_uops(w).prs2 === 0.U,
                         0.U,
                         Mux1H(rrd_uops(w).prs2_port, irf_port_data))

    if (enableSFBOpt) rrd_pred_data(w) := Mux(rrd_uops(w).is_sfb_shadow, io.prf_read_ports(w).data, false.B)
  }

  //-------------------------------------------------------------
  // set outputs to execute pipelines

  for (w <- 0 until coreWidth) {
    io.exe_reqs(w).valid          := rrd_valids(w)
    io.exe_reqs(w).bits.uop       := rrd_uops(w)
    io.exe_reqs(w).bits.rs1_data  := rrd_rs1_data(w)
    io.exe_reqs(w).bits.rs2_data  := rrd_rs2_data(w)
    io.exe_reqs(w).bits.pred_data := rrd_pred_data(w)
  }
}
