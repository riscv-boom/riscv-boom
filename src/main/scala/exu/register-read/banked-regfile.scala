//******************************************************************************
// Copyright (c) 2013 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Banked Register File
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import scala.collection.mutable.ArrayBuffer

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util.{BoomCoreStringPrefix}

class BankReadPort(val addrWidth: Int, val dataWidth: Int)(implicit p: Parameters) extends BoomBundle
{
  val prs1_addr = Input(UInt(addrWidth.W))
  val prs1_data = Output(UInt(dataWidth.W))

  val prs2_addr = Input(UInt(addrWidth.W))
  val prs2_data = Output(UInt(dataWidth.W))
}

class BankWritePort(val addrWidth: Int, val dataWidth: Int)(implicit p: Parameters) extends BoomBundle
{
  val addr = UInt(addrWidth.W)
  val data = UInt(dataWidth.W)
}

/**
 * Utility function to turn ExeUnitResps to match the regfile's WritePort I/Os.
 */
object GetBankWritePort
{
  def apply(enq: DecoupledIO[ExeUnitResp], addrWidth: Int, dataWidth: Int, rtype: UInt)
    (implicit p: Parameters): Valid[BankWritePort] = {
     val wport = Wire(Valid(new BankWritePort(addrWidth, dataWidth)))

     wport.valid     := enq.valid && enq.bits.uop.dst_rtype === rtype
     wport.bits.addr := enq.bits.uop.pdst
     wport.bits.data := enq.bits.data
     enq.ready       := true.B
     wport
  }
}

/**
 * Abstract banked regfile
 *
 * @param numRegisters total number of registers (to be divided between banks)
 * @param registerWidth size of registers in bits
 */
abstract class BankedRegisterFile(
  numRegisters: Int,
  registerWidth: Int)
  (implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle {
    val read_ports  = Vec(coreWidth, new BankReadPort(maxPregSz, registerWidth))
    val write_ports = Flipped(Vec(coreWidth, Valid(new BankWritePort(maxPregSz, registerWidth))))
  })

  private val rf_cost = coreWidth * (2 + 1) * (2 + 1*2) // TODO Does this estimate even make much sense?
  private val type_str = if (registerWidth == fLen+1) "Floating Point" else "Integer"
  override def toString: String = BoomCoreStringPrefix(
    "==" + type_str + " Regfile==",
    "Num RF Read Ports     : " + coreWidth * 2,
    "Num RF Write Ports    : " + coreWidth,
    "RF Cost               : " + rf_cost,
    "Bypassable Units      : " + bypassableArray)
}

/**
 * @param numRegisters number of registers
 * @param registerWidth size of registers in bits
 */
class BankedRegisterFileSynthesizable(
   numRegisters: Int,
   registerWidth: Int,
   (implicit p: Parameters)
   extends BankedRegisterFile(numRegisters, registerWidth)
{
  // --------------------------------------------------------------

  val numRegsPerBank = numRegisters / coreWidth
  require (numRegisters % coreWidth == 0)

  val regfile = Vec(coreWidth, Mem(numRegisters, UInt(registerWidth.W)))

  // --------------------------------------------------------------
  // Read ports

  // Regfile addresses are registered here rather than in regread
  // so that this synthezied regfile can easily be replaced by SRAMs.
  for (w <- 0 until coreWidth) {
    val prs1_addr = RegNext(io.read_ports(w).prs1_addr)
    val prs2_addr = RegNext(io.read_ports(w).prs2_addr)

    io.read_ports(w).prs1_data :=
      Mux(prs1_addr === 0.U,
        0.U,
        regfile(w)(prs1_addr))

    io.read_ports(w).prs2_data :=
      Mux(prs2_addr === 0.U,
        0.U,
        regfile(w)(prs2_addr))
  }

  // --------------------------------------------------------------
  // Write ports.

  for (w -> 0 until coreWidth) {
    when (write_ports(w).valid && (write_ports(w).bits.addr =/= 0.U)) {
      regfile(w)(write_ports(w).bits.addr) := write_ports(w).bits.data
    }
  }
}
