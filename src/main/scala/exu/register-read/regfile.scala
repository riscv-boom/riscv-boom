//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Register File (Abstract class and Synthesizable RegFile)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import scala.collection.mutable.ArrayBuffer

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util.{BoomCoreStringPrefix}

abstract class RegisterFile[T <: Data](
  dType: T,
  numRegisters: Int,
  numReadPorts: Int,
  numWritePorts: Int)
  (implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle {
    val arb_read_reqs  = Vec(numReadPorts, Flipped(Decoupled(UInt(log2Ceil(numRegisters).W))))
    val rrd_read_resps = Vec(numReadPorts, Output(dType))

    val write_ports = Vec(numWritePorts, Flipped(Valid(new Bundle {
      val addr = Output(UInt(maxPregSz.W))
      val data = Output(dType)
    })))
  })

  // ensure there is only 1 writer per register (unless to preg0)
  if (numWritePorts > 1) {
    for (i <- 0 until (numWritePorts - 1)) {
      for (j <- (i + 1) until numWritePorts) {
        assert(!io.write_ports(i).valid ||
               !io.write_ports(j).valid ||
               (io.write_ports(i).bits.addr =/= io.write_ports(j).bits.addr),
          "[regfile] too many writers a register")
      }
    }
  }
}

class BankedRF[T <: Data](
  dType: T,
  numBanks: Int,
  numLogicalReadPortsPerBank: Int,
  numRegisters: Int,
  numLogicalReadPorts: Int,
  numPhysicalReadPorts: Int,
  numWritePorts: Int,
  typeStr: String
)(implicit p: Parameters)
    extends RegisterFile(dType, numRegisters, numLogicalReadPorts, numWritePorts)
{
  require(isPow2(numBanks))
  require(numRegisters % numBanks == 0)
  def bankIdx(i: UInt): UInt = i(log2Ceil(numBanks)-1,0)
  val rfs = (0 until numBanks) map { w => Module(new PartiallyPortedRF(
    dType,
    numRegisters / numBanks,
    numLogicalReadPortsPerBank,
    numPhysicalReadPorts,
    numWritePorts,
    typeStr + s" Bank ${w}"
  )) }
  if (numBanks == 1) {
    require(numLogicalReadPortsPerBank == numLogicalReadPorts)
    io <> rfs(0).io
  } else {
    for (i <- 0 until numWritePorts) {
      for (w <- 0 until numBanks) {
        rfs(w).io.write_ports(i).valid     := io.write_ports(i).valid && bankIdx(io.write_ports(i).bits.addr) === w.U
        rfs(w).io.write_ports(i).bits.addr := io.write_ports(i).bits.addr >> log2Ceil(numBanks)
        rfs(w).io.write_ports(i).bits.data := io.write_ports(i).bits.data
      }
    }
    if (numLogicalReadPortsPerBank == numLogicalReadPorts) {
      for (i <- 0 until numLogicalReadPorts) {
        val bidx = bankIdx(io.arb_read_reqs(i).bits)
        for (w <- 0 until numBanks) {
          rfs(w).io.arb_read_reqs(i).valid := io.arb_read_reqs(i).valid && bankIdx(io.arb_read_reqs(i).bits) === w.U
          rfs(w).io.arb_read_reqs(i).bits  := io.arb_read_reqs(i).bits >> log2Ceil(numBanks)
        }
        io.arb_read_reqs(i).ready := rfs.map(_.io.arb_read_reqs(i).ready).reduce(_||_)
        val data_sel = RegNext(UIntToOH(bidx))
        io.rrd_read_resps(i) := Mux1H(data_sel, rfs.map(_.io.rrd_read_resps(i)))
      }
    }
  }
  override def toString: String = rfs.map(_.toString).mkString
}

class PartiallyPortedRF[T <: Data](
  dType: T,
  numRegisters: Int,
  numLogicalReadPorts: Int,
  numPhysicalReadPorts: Int,
  numWritePorts: Int,
  typeStr: String
)(implicit p: Parameters)
    extends RegisterFile(dType, numRegisters, numLogicalReadPorts, numWritePorts)
{
  val rf = Module(new FullyPortedRF(
    dType = dType,
    numRegisters = numRegisters,
    numReadPorts = numPhysicalReadPorts,
    numWritePorts = numWritePorts,
    typeStr = "Partially Ported " + typeStr,
  ))
  rf.io.write_ports := io.write_ports

  val port_issued = Array.fill(numPhysicalReadPorts) { false.B }
  val port_addrs  = Array.fill(numPhysicalReadPorts) { 0.U(log2Ceil(numRegisters).W) }
  val data_sels   = Wire(Vec(numLogicalReadPorts , UInt(numPhysicalReadPorts.W)))
  data_sels := DontCare

  val supportPortSharing = false

  for (i <- 0 until numLogicalReadPorts) {
    var read_issued = false.B
    for (j <- 0 until numPhysicalReadPorts) {
      val issue_read = WireInit(false.B)
      val use_port = WireInit(false.B)
      if (supportPortSharing) {
        when (!read_issued && port_issued(j) && io.arb_read_reqs(i).valid && io.arb_read_reqs(i).bits === port_addrs(j)) {
          issue_read := true.B
          data_sels(i) := UIntToOH(j.U)
        }
      }
      when (!read_issued && !port_issued(j) && io.arb_read_reqs(i).valid) {
        issue_read := true.B
        use_port := true.B
        data_sels(i) := UIntToOH(j.U)
      }
      val was_port_issued_yet = port_issued(j)
      port_issued(j) = use_port || port_issued(j)
      port_addrs(j) = port_addrs(j) | Mux(was_port_issued_yet || !use_port, 0.U, io.arb_read_reqs(i).bits)
      read_issued = issue_read || read_issued
    }
    io.arb_read_reqs(i).ready := read_issued
  }
  for (j <- 0 until numPhysicalReadPorts) {
    rf.io.arb_read_reqs(j).valid := port_issued(j)
    rf.io.arb_read_reqs(j).bits  := port_addrs(j)
    assert(rf.io.arb_read_reqs(j).ready)
  }

  val rrd_data_sels = RegNext(data_sels)

  for (i <- 0 until numLogicalReadPorts) {
    io.rrd_read_resps(i) := Mux1H(rrd_data_sels(i).toBools, rf.io.rrd_read_resps)
  }
  override def toString: String = rf.toString
}


class FullyPortedRF[T <: Data](
  dType: T,
  numRegisters: Int,
  numReadPorts: Int,
  numWritePorts: Int,
  typeStr: String,
)(implicit p: Parameters)
    extends RegisterFile(dType, numRegisters, numReadPorts, numWritePorts)
{
  val rf_cost = (numReadPorts + numWritePorts) * (numReadPorts + 2*numWritePorts)
  override def toString: String = BoomCoreStringPrefix(
    "==" + typeStr + " Regfile==",
    "Num RF Read Ports     : " + numReadPorts,
    "Num RF Write Ports    : " + numWritePorts,
    "RF Cost (R+W)*(R+2W)  : " + rf_cost)

  dontTouch(io)

  io.arb_read_reqs.map(p => p.ready := true.B)

  val regfile = Mem(numRegisters, dType)

  (0 until numReadPorts) map {p => io.rrd_read_resps(p) := regfile(RegNext(io.arb_read_reqs(p).bits)) }

  io.write_ports map { p => when (p.valid) { regfile(p.bits.addr) := p.bits.data }}
}


