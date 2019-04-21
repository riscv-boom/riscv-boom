//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Custom Register File (Blackboxes and Models)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import scala.collection.mutable.ArrayBuffer

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

/**
 * Custom RegFile used for Berkeley tapeouts
 *
 * @param numRegisters number of registers
 * @param numReadPorts number of read ports
 * @param numWritePorts number of write ports
 * @param registerWidth size of registers in bits
 * @param bypassableArray list of write ports from func units to the read port of the regfile
 */
class RegisterFileSeqCustomArray(
  numRegisters: Int,
  numReadPorts: Int,
  numWritePorts: Int,
  registerWidth: Int,
  bypassableArray: Seq[Boolean])(implicit p: Parameters)
  extends RegisterFile(numRegisters, numReadPorts, numWritePorts, registerWidth, bypassableArray)
{
  // instantiate either the regfile model or the blackbox
  val regfile =
  if (enableCustomRfModel) {
    Module(new RegisterFileArrayModel(numRegisters, numReadPorts, numWritePorts, registerWidth))
  } else {
    Module(new RegisterFileArray(numRegisters, numReadPorts, numWritePorts, registerWidth))
  }
  regfile.io.clock := clock

  // decode addrs into OH's
  val waddr_OH = Wire(Vec(numWritePorts, UInt(numRegisters.W)))
  val raddr_OH = Reg( Vec(numReadPorts , UInt(numRegisters.W)))

  for (w <-0 until numWritePorts) {
    regfile.io.WD(w) := io.write_ports(w).bits.data
    waddr_OH(w) := UIntToOH(io.write_ports(w).bits.addr) // what register are you writing in OH
  }

  val read_data = Wire(Vec(numReadPorts, UInt(registerWidth.W)))
  for (r <- 0 until numReadPorts) {
    read_data(r) := Mux(RegNext(io.read_ports(r).addr === 0.U), 0.U, regfile.io.RD(r))
    raddr_OH(r) := UIntToOH(io.read_ports(r).addr) // what register are you reading in OH
  }

  // wire to see if a particular register is being written to
  val write_select_OH = Wire(Vec(numRegisters, UInt(numWritePorts.W)))

  // on a per register basis, enabling read/writes based on the read/write ports
  for (i <- 0 until numRegisters) {
    if (i == 0) {
      // physical register 0 (P0) is always zero
      write_select_OH(i) := 0.U
      regfile.io.OE(i)   := 0.U
      regfile.io.WE(i)   := false.B
      regfile.io.WS(i)   := 0.U
    } else {
      // setup reads
      regfile.io.OE(i) := VecInit(raddr_OH.map{ item => item(i) }).asUInt // read from a particular register

      // setup writes
      write_select_OH(i) := VecInit(
        (waddr_OH zip io.write_ports).map{case (item, wp) => item(i) && wp.valid}).asUInt // write to a register
      regfile.io.WE(i) := write_select_OH(i).orR // someone is writing to that register
      regfile.io.WS(i) := OHToUInt(write_select_OH(i)) // who is writing to that register
    }

    if (i > 0) {
      // ensure there is only 1 writer to a preg (unless its preg0)
      assert((PopCount(write_select_OH(i).asBools) <= 1.U) || (i.U === 0.U),
        "[custom_regfile] write-select has too many writers to this register p[" + i + "]")
    }
  }

  // bypass out of the ALU's write ports.

  require (bypassableArray.length == io.write_ports.length)

  if (bypassableArray.reduce(_||_)) {
    // bypass specific write ports
    val bypassable_wports = ArrayBuffer[Valid[RegisterFileWritePort]]()
    io.write_ports zip bypassableArray map { case (wport, b) => if (b) { bypassable_wports += wport} }

    for (i <- 0 until numReadPorts) {
      // bypass if matches with the read port input
      val bypass_enables = bypassable_wports.map(x => x.valid && x.bits.addr =/= 0.U &&
        x.bits.addr === RegNext(io.read_ports(i).addr))

      // bypass the data from the specific write port if it matches
      val bypass_data = Mux1H(VecInit(bypass_enables), VecInit(bypassable_wports.map(_.bits.data)))

      // if there was a match pass to the read port output
      io.read_ports(i).data := Mux(bypass_enables.reduce(_|_), bypass_data, read_data(i))

      assert(PopCount(VecInit(bypass_enables)) <= 1.U,
        "[custom_regfile] multiple write ports are trying to bypass to a single read port")
    }
  } else {
    // no bypassing so connect the read data output to the read port
    for (i <- 0 until numReadPorts) {
      io.read_ports(i).data := read_data(i)
    }
  }
}

/**
 * IO trait for the register file to provide shared I/O trait between the BlackBox
 * and model modules.
 */
trait HasRegisterFileIO extends chisel3.experimental.BaseModule
{
  // require these parameters exist
  val numRegisters: Int
  val numReadPorts: Int
  val numWritePorts: Int
  val registerWidth: Int

  val io = IO(new Bundle {
    val clock = Input(Clock())
    val WE = Input(Vec(numRegisters, Bool()))
    val WD = Input(Vec(numWritePorts, UInt(registerWidth.W)))
    val RD = Output(Vec(numReadPorts, UInt(registerWidth.W)))
    val WS = Input(Vec(numRegisters, UInt(log2Ceil(numWritePorts).W)))
    val OE = Input(Vec(numRegisters, UInt(numReadPorts.W)))
  })
}

/**
 * BlackBoxed register file array
 *
 * @param numRegisters number of registers
 * @param numReadPorts number of read ports for the regfile
 * @param numWritePorts number of write ports to the regfile
 * @param registerWidth register width in bits
 */
class RegisterFileArray(
  val numRegisters: Int,
  val numReadPorts: Int,
  val numWritePorts: Int,
  val registerWidth: Int)
  extends BlackBox
  with HasRegisterFileIO
{
}

/**
 * Model of the above blackboxed RegisterFileArray. This should not be
 * synthesized.
 *
 * @param numRegisters number of registers
 * @param numReadPorts number of read ports for the regfile
 * @param numWritePorts number of write ports to the regfile
 * @param registerWidth register width in bits
 */
class RegisterFileArrayModel(
  val numRegisters: Int,
  val numReadPorts: Int,
  val numWritePorts: Int,
  val registerWidth: Int)
  extends Module
  with HasRegisterFileIO
{
  println("WARNING: This register file model should not be synthesized")

  // cheat a bit, since we don't simulate tri-states fighting over a single wire
  // we need to first set the wire to 0.U to make firrtl happy
  for (rPortIdx <- 0 until numReadPorts) {
    io.RD(rPortIdx) := 0.U // technically this should be high-Z
  }

  for (i <- 0 until numRegisters) yield {
    val register = Module(new RegFileRegisterModel(
      numReadPorts,
      numWritePorts,
      registerWidth))
    register.io.we := io.WE(i)
    register.io.ws := io.WS(i)
    register.io.wd := io.WD
    register.io.oe := io.OE(i)

    for (rPortIdx <- 0 until numReadPorts) {
      when (io.OE(i)(rPortIdx)) {
        io.RD(rPortIdx) := register.io.rd(rPortIdx)
      }
    }
  }

  // assert only one reader set for each read port.
  for (rPortIdx <- 0 until numReadPorts) {
    assert (PopCount(io.OE.map(e => e(rPortIdx))) <= 1.U,
      "[custom_regfile] OE(*)(" + rPortIdx + ") has too many enables set.")
  }
}

/**
 * Model of a single register
 */
class RegFileRegisterModel(
  numReadPorts: Int,
  numWritePorts: Int,
  registerWidth: Int) extends Module
{
  val io = IO(new Bundle {
    val we = Input(Bool())
    val ws = Input(UInt(log2Ceil(numWritePorts).W))
    val wd = Input(Vec(numWritePorts, UInt(registerWidth.W)))
    val oe = Input(UInt(numReadPorts.W))
    val rd = Output(Vec(numReadPorts, UInt(registerWidth.W)))
  })

  val rd = Wire(Vec(numReadPorts, Vec(registerWidth, Bool())))

  for (i <- 0 until registerWidth) yield {
    val bit = Module(new RegFileBitModel(
      numReadPorts,
      numWritePorts))
    bit.io.we := io.we
    bit.io.ws := io.ws
    bit.io.wd := VecInit(io.wd.map{ wPortData => wPortData(i) })
    bit.io.oe := io.oe
    for (rPortIdx <- 0 until numReadPorts) yield {
      rd(rPortIdx)(i) := bit.io.rd(rPortIdx)
    }
  }

  io.rd := rd.map{ rPortDataBools => rPortDataBools.asUInt }
}

/**
 * Model of a register file bit
 */
class RegFileBitModel(
  numReadPorts: Int,
  numWritePorts: Int) extends Module
{
  val io = IO(new Bundle {
    val we  = Input(Bool())
    val ws  = Input(UInt(log2Ceil(numWritePorts).W))
    val wd  = Input(Vec(numWritePorts, Bool()))
    val oe  = Input(UInt(numReadPorts.W))
    val rd  = Output(Vec(numReadPorts, Bool()))
  })

  val din = Wire(Bool())
  val dout = Wire(Bool())

  // Z is not used in Chisel... thus assigning dummy value of true.B
  val z = true.B

  // model tri-state buffer to output line
  (io.rd zip io.oe.asBools).map{ case (rd_bit, oe_bit) => rd_bit := Mux(oe_bit, dout, z) }

  // note: normally this would have the Z connected to any unused inputs into the mux
  val mux_out = Mux1H(UIntToOH(io.ws), io.wd)
  din := mux_out

  // ensure that there is only 1 write port writing
  assert (!(io.we && io.ws >= numWritePorts.U),
    "[custom_regfile_bit] write-select set to invalid value.")

  // d flip-flop model
  val dff = Reg(Bool())
  when (io.we) {
    dff := din
  }
  dout := dff
}
