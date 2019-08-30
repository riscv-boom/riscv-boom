//******************************************************************************
// Copyright (c) 2018 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.tests

import scala.util.Random

import org.scalatest._

import chisel3._
import chisel3.util._
import chisel3.iotesters._

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.system._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import boom.system._

import boom.exu.{RegisterFile, RegisterFileSynthesizable, RegisterFileSeqCustomArray}

/**
 * Main register file tester
 */
class RegisterFileTester extends ChiselFlatSpec
{
  // boom parameters (and modifications to them)
  val boomParams: Parameters = BoomTestUtils.getBoomParameters("BoomConfig")
  implicit val p: Parameters = boomParams.alterPartial
  {
    case BoomTilesKey => boomParams(BoomTilesKey) map { r =>
      r.copy(
        core = r.core.copy(
          enableCustomRfModel = true,
          regreadLatency = 1
        )
      )
    }
  }

  // class parameters
  val MAX_READ_PORTS = 4
  val MAX_WRITE_PORTS = 4
  val NUM_REGISTERS = 6
  val REGISTER_WIDTH = 64

  // classes to test
  val REG_FILES = Seq("RegisterFileSynthesizeable", "RegisterFileSeqCustomArray")

  // test both regfiles and iterations of the r/w port counts
  for (regfile_type <- REG_FILES)
  {
    for (rport_cnt <- 1 to MAX_READ_PORTS; wport_cnt <- 1 to MAX_WRITE_PORTS)
    {
      // get the correct register file to test
      def regfile = regfile_type match {
        case "RegisterFileSynthesizeable" => new RegisterFileSynthesizable(
                                               num_registers = NUM_REGISTERS,
                                               num_read_ports = rport_cnt,
                                               num_write_ports = wport_cnt,
                                               register_width = REGISTER_WIDTH,
                                               bypassable_array = Seq.fill(wport_cnt)(true))
        case "RegisterFileSeqCustomArray" => new RegisterFileSeqCustomArray(
                                               num_registers = NUM_REGISTERS,
                                               num_read_ports = rport_cnt,
                                               num_write_ports = wport_cnt,
                                               register_width = REGISTER_WIDTH,
                                               bypassable_array = Seq.fill(wport_cnt)(true))
        case _ => throw new Exception("Invalid type of RegisterFile")
      }

      // note: the "it" keyword copies the "behavior"
      behavior of s"""$regfile_type with #readPorts: $rport_cnt,
        #writePorts: $wport_cnt, #Regs: $NUM_REGISTERS,
        Width: $REGISTER_WIDTH""".stripMargin.replaceAll("\n", " ")

      // ----------------
      // TESTING SECTION
      // ----------------

      it should s"have preg0 be 0" in
      {
        chisel3.iotesters.Driver(() => regfile, "verilator")
        {
          (c) => new ZeroRegisterTest(c, rport_cnt, wport_cnt)
        } should be (true)
      }

      it should s"not be able to write to the same register from 2 write ports" in
      {
        chisel3.iotesters.Driver(() => regfile, "verilator")
        {
          (c) => new TwoWritesToSameRegisterTest(c, wport_cnt, NUM_REGISTERS)
        } should be (false)
      }

      it should s"be able to write/read to all registers" in
      {
        chisel3.iotesters.Driver(() => regfile, "verilator")
        {
          (c) => new OverallRegistersTest(c, rport_cnt, wport_cnt, NUM_REGISTERS, REGISTER_WIDTH)
        } should be (true)
      }
    }
  }
}

/**
 * Read/writes from register preg0 and make sure that it gets 0
 */
class ZeroRegisterTest[R <: RegisterFile](
  c: R,
  num_read_ports: Int,
  num_write_ports: Int) extends PeekPokeTester(c)
{
  // write to preg0 in all write ports
  for (i <- 0 until num_write_ports)
  {
    // wait for ready to be asserted
    while (peek(c.io.write_ports(i).ready) == 0)
    {
      step(1)
    }

    poke(c.io.write_ports(i).valid, true.B)
    poke(c.io.write_ports(i).bits.addr, 0.U)
    poke(c.io.write_ports(i).bits.data, 5.U) // some dummy value

    step(1)

    poke(c.io.write_ports(i).valid, false.B) // to not trigger assert

    // read preg0
    for (j <- 0 until num_read_ports)
    {
      poke(c.io.read_ports(j).addr, 0.U)
    }

    step(1)

    for (j <- 0 until num_read_ports)
    {
      expect(c.io.read_ports(j).data, 0.U)
    }
  }
}

/**
 * Check that there are not two writes to the same register
 */
class TwoWritesToSameRegisterTest[R <: RegisterFile](
  c: R,
  num_write_ports: Int,
  num_registers: Int) extends PeekPokeTester(c)
{
  if (num_write_ports > 1 && num_registers > 1)
  {
    // chose two random write ports
    val rand = new Random()
    val firstWPort = rand.nextInt(num_write_ports)
    var secondWPort = rand.nextInt(num_write_ports)
    while (secondWPort == firstWPort)
    {
      secondWPort = rand.nextInt(num_write_ports)
    }

    // wait for ready to be asserted for both
    while (peek(c.io.write_ports(firstWPort).ready) == 0 &&
          peek(c.io.write_ports(secondWPort).ready) == 0)
    {
      step(1)
    }

    // read from both write ports
    poke(c.io.write_ports(firstWPort).valid, true.B)
    poke(c.io.write_ports(firstWPort).bits.addr, 1.U)
    poke(c.io.write_ports(firstWPort).bits.data, 5.U) // some dummy value

    poke(c.io.write_ports(secondWPort).valid, true.B)
    poke(c.io.write_ports(secondWPort).bits.addr, 1.U)
    poke(c.io.write_ports(secondWPort).bits.data, 10.U) // some dummy value

    try
    {
      // this should assert
      step(1)
    }
    catch
    {
      case _: Throwable => println("This was supposed to throw an assertion")
    }
  }
  else {
    // hacky way to avoid PeekPoke backend showing that this is an error
    expect(false, ">1 write port (or only 1 register) should pass the test")
  }
}

/**
 * Read/write to all registers in the register file
 */
class OverallRegistersTest[R <: RegisterFile](
  c: R,
  num_read_ports: Int,
  num_write_ports: Int,
  num_registers: Int,
  register_width: Int) extends PeekPokeTester(c)
{
  for (i <- 0 until num_write_ports)
  {
    poke(c.io.write_ports(i).valid, false.B)
  }

  // zero out the registers
  for (i <- 0 until num_registers)
  {
    while (peek(c.io.write_ports(0).ready) == 0)
    {
      step(1)
    }
    poke(c.io.write_ports(0).valid, true.B)
    poke(c.io.write_ports(0).bits.addr, i.U)
    poke(c.io.write_ports(0).bits.data, 0.U) // some dummy value

    step(1)
  }

  // write random values into the registers
  val rand = new Random()
  val nums_written = Seq(BigInt(0)) ++
    (for (i <- 1 until num_registers) yield BigInt(register_width, rand)) // first register write is nullified
  var which_wp = 0
  for (i <- 0 until num_registers)
  {
    while (peek(c.io.write_ports(which_wp).ready) == 0)
    {
      step(1)
    }
    poke(c.io.write_ports(which_wp).valid, true.B)
    poke(c.io.write_ports(which_wp).bits.addr, i.U)
    poke(c.io.write_ports(which_wp).bits.data, nums_written(i).U) // some rand value

    // use all wp throughout writing the registers
    which_wp = (which_wp + 1) % num_write_ports
    if (which_wp == 0)
    {
      step(1)
    }
  }

  // finish up writing registers
  step(1)

  // valid == false.B to make sure no asserts are fired
  for (i <- 0 until num_write_ports)
  {
    poke(c.io.write_ports(i).valid, false.B)
  }

  step(1)

  var which_rp = 0 // which rp is being used to read the value
  var active_rp_count = 0 // how many active read ports are being read from
  var count_regs_read = 0 // count the amount of regs read in
  for (reg_idx <- 0 until num_registers)
  {
    poke(c.io.read_ports(which_rp).addr, reg_idx.U)
    active_rp_count = active_rp_count + 1

    which_rp = (which_rp + 1) % num_read_ports
    if (which_rp == 0)
    {
      step(1)

      // read as many as possible
      for (active_rp_idx <- 0 until active_rp_count)
      {
        expect(c.io.read_ports(active_rp_idx).data, nums_written(count_regs_read).U)
        count_regs_read = count_regs_read + 1
      }

      active_rp_count = 0
    }
  }

  step(1)

  // read as many as possible that are left
  for (active_rp_idx <- 0 until active_rp_count)
  {
    expect(c.io.read_ports(active_rp_idx).data, nums_written(count_regs_read).U)
    count_regs_read = count_regs_read + 1
  }
}
