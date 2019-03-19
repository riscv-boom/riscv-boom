//******************************************************************************
// Copyright (c) 2018 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Abraham Gonzalez
//------------------------------------------------------------------------------

package boom.tests

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

import boom.exu.{RegisterFileBehavorial}

/**
 * Basic register file tests
 */
class RegisterFileTest(c: RegisterFileBehavorial) extends PeekPokeTester(c) {
  poke(c.io.read_ports(0).addr, 0.U)

  step(1)

  println(s"addr readout = ${peek(c.io.read_ports(0).addr)}")

  expect(c.io.read_ports(0).data, 0.U)
}

class RegisterFileTester extends ChiselFlatSpec {
  behavior of "RegisterFileBehavioral"

  implicit val p: Parameters = BoomTestUtils.getBoomParameters("BoomConfig")

  backends foreach { backend =>
    it should s"perform correct r/w: backend($backend)" in {
      chisel3.iotesters.Driver(() => new RegisterFileBehavorial(
                                           num_registers = 10,
                                           num_read_ports = 1,
                                           num_write_ports = 1,
                                           register_width = 32,
                                           bypassable_array = Seq(true)),
                               "verilator")
        {
          (c) => new RegisterFileTest(c)
        } should be (true)
    }
  }
}
