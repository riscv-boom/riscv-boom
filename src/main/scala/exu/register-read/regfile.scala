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

abstract class RegisterFile(
  numRegisters: Int,
  numReadPorts: Int,
  numWritePorts: Int,
  registerWidth: Int)
  (implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle {
    val read_ports = Vec(numReadPorts, new Bundle {
      val addr = Input(Valid(UInt(maxPregSz.W)))
      val data = Output(UInt(registerWidth.W))
    })
    val write_ports = Vec(numWritePorts, Flipped(Valid(new Bundle {
      val addr = Output(UInt(maxPregSz.W))
      val data = Output(UInt(registerWidth.W))
    })))
  })

  private val rf_cost = (numReadPorts + numWritePorts) * (numReadPorts + 2*numWritePorts)
  private val type_str = if (registerWidth == fLen+1) "Floating Point" else "Integer"
  override def toString: String = BoomCoreStringPrefix(
    "==" + type_str + " Regfile==",
    "Num RF Read Ports     : " + numReadPorts,
    "Num RF Write Ports    : " + numWritePorts,
    "RF Cost (R+W)*(R+2W)  : " + rf_cost)

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

class RegisterFileSynthesizable(
   numRegisters: Int,
   numReadPorts: Int,
   numWritePorts: Int,
   registerWidth: Int)
   (implicit p: Parameters)
   extends RegisterFile(numRegisters, numReadPorts, numWritePorts, registerWidth)
{

  val regfile = Mem(numRegisters, UInt(registerWidth.W))

  for (i <- 0 until numReadPorts) {
    io.read_ports(i).data := regfile(io.read_ports(i).addr.bits)
  }


  for (wport <- io.write_ports) {
    when (wport.valid) {
      regfile(wport.bits.addr) := wport.bits.data
    }
  }

}
