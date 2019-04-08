//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
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

/**
 * IO bundle for a register read port
 *
 * @param addr_width size of register address in bits
 * @param data_width size of register in bits
 */
class RegisterFileReadPortIO(val addr_width: Int, val data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
  val addr = Input(UInt(addr_width.W))
  val data = Output(UInt(data_width.W))
}

/**
 * IO bundle for the register write port
 *
 * @param addr_width size of register address in bits
 * @param data_width size of register in bits
 */
class RegisterFileWritePort(val addr_width: Int, val data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
  val addr = UInt(addr_width.W)
  val data = UInt(data_width.W)
}

/**
 * Utility function to turn ExeUnitResps to match the regfile's WritePort I/Os.
 */
object WritePort
{
  def apply(enq: DecoupledIO[ExeUnitResp], addr_width: Int, data_width: Int)
    (implicit p: Parameters): DecoupledIO[RegisterFileWritePort] =
  {
    val wport = Wire(Decoupled(new RegisterFileWritePort(addr_width, data_width)))
    wport.valid := enq.valid && enq.bits.uop.dst_rtype =/= RT_X
    wport.bits.addr := enq.bits.uop.pdst
    wport.bits.data := enq.bits.data
    enq.ready := wport.ready

    wport
  }
}

/**
 * Register file abstract class
 *
 * @param num_registers number of registers
 * @param num_read_ports number of read ports
 * @param num_write_ports number of write ports
 * @param register_width size of registers in bits
 * @param bypassable_array list of write ports from func units to the read port of the regfile
 */
abstract class RegisterFile(
  num_registers: Int,
  num_read_ports: Int,
  num_write_ports: Int,
  register_width: Int,
  bypassable_array: Seq[Boolean]) // which write ports can be bypassed to the read ports?
  (implicit p: Parameters) extends BoomModule()(p)
{
  val io = IO(new BoomBundle()(p)
  {
    val read_ports = Vec(num_read_ports, new RegisterFileReadPortIO(PREG_SZ, register_width))
    val write_ports = Flipped(Vec(num_write_ports, Decoupled(new RegisterFileWritePort(PREG_SZ, register_width))))
  })

  private val rf_cost = (num_read_ports + num_write_ports) * (num_read_ports + 2*num_write_ports)
  private val type_str = if (register_width == fLen+1) "Floating Point" else "Integer"
  override def toString: String =
    "\n   ==" + type_str + " Regfile==" +
    "\n   Num RF Read Ports     : " + num_read_ports +
    "\n   Num RF Write Ports    : " + num_write_ports +
    "\n   RF Cost (R+W)*(R+2W)  : " + rf_cost +
    "\n   Bypassable Units      : " + bypassable_array
}

/**
 * A synthesizable model of a Register File. You will likely want to blackbox this for more than modest port counts.
 *
 * @param num_registers number of registers
 * @param num_read_ports number of read ports
 * @param num_write_ports number of write ports
 * @param register_width size of registers in bits
 * @param bypassable_array list of write ports from func units to the read port of the regfile
 */
class RegisterFileSynthesizable(
   num_registers: Int,
   num_read_ports: Int,
   num_write_ports: Int,
   register_width: Int,
   bypassable_array: Seq[Boolean])
   (implicit p: Parameters)
   extends RegisterFile(num_registers, num_read_ports, num_write_ports, register_width, bypassable_array)
{
  // --------------------------------------------------------------

  val regfile = Mem(num_registers, UInt(register_width.W))

  // --------------------------------------------------------------
  // Read ports.

  val read_data = Wire(Vec(num_read_ports, UInt(register_width.W)))

  // Register the read port addresses to give a full cycle to the RegisterRead Stage (if desired).
  val read_addrs =
    if (regreadLatency == 0)
    {
      io.read_ports map {_.addr}
    }
    else
    {
      require (regreadLatency == 1)
      io.read_ports.map(p => RegNext(p.addr))
    }

  for (i <- 0 until num_read_ports)
  {
    read_data(i) :=
      Mux(read_addrs(i) === 0.U,
        0.U,
        regfile(read_addrs(i)))
  }

  // --------------------------------------------------------------
  // Bypass out of the ALU's write ports.
  // We are assuming we cannot bypass a writer to a reader within the regfile memory
  // for a write that occurs at the end of cycle S1 and a read that returns data on cycle S1.
  // But since these bypasses are expensive, and not all write ports need to bypass their data,
  // only perform the w->r bypass on a select number of write ports.

  require (bypassable_array.length == io.write_ports.length)

  if (bypassable_array.reduce(_||_))
  {
    val bypassable_wports = ArrayBuffer[DecoupledIO[RegisterFileWritePort]]()
    io.write_ports zip bypassable_array map { case (wport, b) => if (b) { bypassable_wports += wport} }

    for (i <- 0 until num_read_ports)
    {
      val bypass_ens = bypassable_wports.map(x => x.valid &&
        x.bits.addr =/= 0.U &&
        x.bits.addr === read_addrs(i))

      val bypass_data = Mux1H(VecInit(bypass_ens), VecInit(bypassable_wports.map(_.bits.data)))

      io.read_ports(i).data := Mux(bypass_ens.reduce(_|_), bypass_data, read_data(i))
    }
  }
  else
  {
    for (i <- 0 until num_read_ports)
    {
      io.read_ports(i).data := read_data(i)
    }
  }

  // --------------------------------------------------------------
  // Write ports.

  for (wport <- io.write_ports)
  {
    wport.ready := true.B
    when (wport.valid && (wport.bits.addr =/= 0.U))
    {
      regfile(wport.bits.addr) := wport.bits.data
    }
  }

  // ensure there is only 1 writer per register (unless to preg0)
  if (num_write_ports > 1)
  {
    for (i <- 0 until (num_write_ports - 1))
    {
      for (j <- (i + 1) until num_write_ports)
      {
        assert(!io.write_ports(i).valid ||
               !io.write_ports(j).valid ||
               (io.write_ports(i).bits.addr =/= io.write_ports(j).bits.addr) ||
               (io.write_ports(i).bits.addr === 0.U), // note: you only have to check one here
          "[regfile] too many writers a register")
      }
    }
  }
}
