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
import boom.util._

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
  bankedWritePortArray: Seq[Option[Int]],
  typeStr: String
)(implicit p: Parameters)
    extends RegisterFile(dType, numRegisters, numLogicalReadPorts, numWritePorts)
{
  require(isPow2(numBanks))
  require(numRegisters % numBanks == 0)
  require(bankedWritePortArray.length == numWritePorts)
  val numDedicatedWritePorts = bankedWritePortArray.flatten.length
  val writePortsPerBank = if (numDedicatedWritePorts == 0) {
    numWritePorts
  } else {
    numWritePorts - numDedicatedWritePorts + 1
  }

  def bankIdx(i: UInt): UInt = i(log2Ceil(numBanks)-1,0)
  val rfs = (0 until numBanks) map { w => Module(new PartiallyPortedRF(
    dType,
    numRegisters / numBanks,
    numLogicalReadPortsPerBank,
    numPhysicalReadPorts,
    writePortsPerBank,
    typeStr + s" Bank ${w}"
  )) }
  if (numBanks == 1) {
    require(numLogicalReadPortsPerBank == numLogicalReadPorts)
    io <> rfs(0).io
  } else {
    val widxs = Array.fill(numBanks)(0)
    for (i <- 0 until numWritePorts) {
      if (bankedWritePortArray(i) != None) {
        val bank = bankedWritePortArray(i).get
        val widx = widxs(bank)
        rfs(bank).io.write_ports(widx).valid     := io.write_ports(i).valid
        rfs(bank).io.write_ports(widx).bits.addr := io.write_ports(i).bits.addr >> log2Ceil(numBanks)
        rfs(bank).io.write_ports(widx).bits.data := io.write_ports(i).bits.data
        assert(!io.write_ports(i).valid || bankIdx(io.write_ports(i).bits.addr) === bank.U)
        widxs(bank) = widx + 1
      } else {
        for (w <- 0 until numBanks) {
          val widx = widxs(w)
          rfs(w).io.write_ports(widx).valid     := io.write_ports(i).valid && bankIdx(io.write_ports(i).bits.addr) === w.U
          rfs(w).io.write_ports(widx).bits.addr := io.write_ports(i).bits.addr >> log2Ceil(numBanks)
          rfs(w).io.write_ports(widx).bits.data := io.write_ports(i).bits.data
          widxs(w) = widx + 1
        }
      }
    }
    require(widxs.forall(_ == writePortsPerBank), widxs.mkString(","))
    if (numLogicalReadPortsPerBank == numLogicalReadPorts) {
      for (i <- 0 until numLogicalReadPorts) {
        val bidx = bankIdx(io.arb_read_reqs(i).bits)
        for (w <- 0 until numBanks) {
          rfs(w).io.arb_read_reqs(i).valid := io.arb_read_reqs(i).valid && bankIdx(io.arb_read_reqs(i).bits) === w.U
          rfs(w).io.arb_read_reqs(i).bits  := io.arb_read_reqs(i).bits >> log2Ceil(numBanks)
        }
        val arb_data_sel = UIntToOH(bidx)
        val rrd_data_sel = RegNext(arb_data_sel)
        io.arb_read_reqs(i).ready := Mux1H(arb_data_sel, rfs.map(_.io.arb_read_reqs(i).ready))
        io.rrd_read_resps(i)      := Mux1H(rrd_data_sel, rfs.map(_.io.rrd_read_resps(i)))
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

  val oh_reads = io.arb_read_reqs.map { r => Fill(numRegisters, r.valid) & UIntToOH(r.bits) }.reduce(_|_)
  val port_sels = SelectFirstN(oh_reads, numPhysicalReadPorts)
  val port_addrs = port_sels.map(x => OHToUInt(x))
  val data_sels = VecInit(io.arb_read_reqs.map { r => VecInit(port_sels.map { s => s(r.bits) }) })

  for (i <- 0 until numLogicalReadPorts) {
    io.arb_read_reqs(i).ready := data_sels(i).reduce(_||_)
  }

  for (j <- 0 until numPhysicalReadPorts) {
    rf.io.arb_read_reqs(j).valid := PopCount(oh_reads) > j.U
    rf.io.arb_read_reqs(j).bits  := port_addrs(j)
    assert(rf.io.arb_read_reqs(j).ready)
  }

  val rrd_data_sels = RegNext(data_sels)

  for (i <- 0 until numLogicalReadPorts) {
    io.rrd_read_resps(i) := Mux1H(rrd_data_sels(i), rf.io.rrd_read_resps)
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


