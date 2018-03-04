//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Datapath Register File (Custom Blackboxes and Models)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom

import Chisel._
import freechips.rocketchip.config.Parameters

import scala.collection.mutable.ArrayBuffer

class RegisterFileSeqCustomArray(
   num_registers: Int,
   num_read_ports: Int,
   num_write_ports: Int,
   register_width: Int,
   bypassable_array: Seq[Boolean])
   (implicit p: Parameters)
   extends RegisterFile(num_registers, num_read_ports, num_write_ports, register_width, bypassable_array)
{

   // --------------------------------------------------------------

   val regfile =
      if (enableCustomRfModel)
         Module(new RegisterFileArrayModel(num_registers, num_read_ports, num_write_ports, register_width))
      else
         Module(new RegisterFileArray(num_registers, num_read_ports, num_write_ports, register_width))
   regfile.io.clock := clock

  // Decode addr
   val waddr_OH = Wire(Vec(num_write_ports, UInt(width = num_registers)))
   val raddr_OH = Reg(Vec(num_read_ports, UInt(width = num_registers)))
   val write_select_OH = Wire(Vec(num_registers, UInt(width = num_write_ports)))

   for (w <-0 until num_write_ports) {
      regfile.io.WD(w) := io.write_ports(w).bits.data
      waddr_OH(w) := UIntToOH(io.write_ports(w).bits.addr)
      io.write_ports(w).ready := Bool(true)
   }

   val read_data = Wire(Vec(num_read_ports, UInt(width = register_width)))
   for (r <-0 until num_read_ports) {
      read_data(r) := Mux(RegNext(io.read_ports(r).addr === 0.U), 0.U, regfile.io.RD(r))
      raddr_OH(r) := UIntToOH(io.read_ports(r).addr)
   }

   for (i <- 0 until num_registers) {
      if (i == 0) {
         // P0 is always zero.
         regfile.io.OE(0) := 0.U
         regfile.io.WE(0) := false.B
         regfile.io.WS(0) := 0.U
      } else {
         regfile.io.OE(i) := Cat(raddr_OH(5)(i), raddr_OH(4)(i), raddr_OH(3)(i), raddr_OH(2)(i), raddr_OH(1)(i), raddr_OH(0)(i))
         write_select_OH(i) := Cat(
            waddr_OH(2)(i) && io.write_ports(2).valid,
            waddr_OH(1)(i) && io.write_ports(1).valid,
            waddr_OH(0)(i) && io.write_ports(0).valid)
         regfile.io.WE(i) := write_select_OH(i).orR
         regfile.io.WS(i) := OHToUInt(write_select_OH(i))
      }

      //printf("regfile.WS(%d)=%d, ws_OH=0x%x\n", i.U, regfile.io.WS(i), write_select_OH(i))
      if (i > 0) {
         assert(PopCount(write_select_OH(i).toBools) <= 1.U,
            "[regfile] write-select has too many writers to this register p[" + i + "]")
      }
   }


   // --------------------------------------------------------------
   // Bypass out of the ALU's write ports.

   require (bypassable_array.length == io.write_ports.length)

   if (bypassable_array.reduce(_||_))
   {
      val bypassable_wports = ArrayBuffer[DecoupledIO[RegisterFileWritePort]]()
      io.write_ports zip bypassable_array map { case (wport, b) => if (b) { bypassable_wports += wport} }

      for (i <- 0 until num_read_ports)
      {
         val bypass_ens = bypassable_wports.map(x => x.valid &&
                                                  x.bits.addr =/= UInt(0) &&
                                                  x.bits.addr === RegNext(io.read_ports(i).addr))

         val bypass_data = Mux1H(Vec(bypass_ens), Vec(bypassable_wports.map(_.bits.data)))

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
}

// Provide shared I/O trait between BlackBox and Module.
trait HasRegisterFileIO extends chisel3.experimental.BaseModule
{
   // Require these parameters exist.
   val num_registers: Int
   val num_read_ports: Int
   val num_write_ports: Int
   val register_width: Int

   val io = IO(new Bundle {
      val clock = Clock(INPUT)
      val WE = Vec(num_registers, Bool()).asInput
      val WD = Vec(num_write_ports, UInt(width = register_width)).asInput
      val RD = Vec(num_read_ports, UInt(width = register_width)).asOutput
      val WS = Vec(num_registers, UInt(width = log2Ceil(num_write_ports))).asInput
      val OE = Vec(num_registers, UInt(width = num_read_ports)).asInput
   })
}

class RegisterFileArray(
   val num_registers: Int,
   val num_read_ports: Int,
   val num_write_ports: Int,
   val register_width: Int)
   extends BlackBox
   with HasRegisterFileIO
{
}


// This is a model of the above blackbox RegisterFileArray. Don't ship this.
class RegisterFileArrayModel(
   val num_registers: Int,
   val num_read_ports: Int,
   val num_write_ports: Int,
   val register_width: Int)
   extends Module
   with HasRegisterFileIO
{
   // Where we're going, we don't need roads. Or parameterization.
   require (num_read_ports == 6)
   require (num_write_ports == 3)
   require (register_width == 64)

   for (i <- 0 until num_registers) yield
   {
      val register = Module(new RegisterFile6r3wRegisterModel())
      register.io.we := io.WE(i)
      register.io.ws := io.WS(i)
      register.io.wd0 := io.WD(0)
      register.io.wd1 := io.WD(1)
      register.io.wd2 := io.WD(2)
      register.io.oe  := io.OE(i)

      // cheat a bit, since we don't simulate tri-states fighting over a single wire.
      when (io.OE(i)(0)) { io.RD(0) := register.io.rd0 }
      when (io.OE(i)(1)) { io.RD(1) := register.io.rd1 }
      when (io.OE(i)(2)) { io.RD(2) := register.io.rd2 }
      when (io.OE(i)(3)) { io.RD(3) := register.io.rd3 }
      when (io.OE(i)(4)) { io.RD(4) := register.io.rd4 }
      when (io.OE(i)(5)) { io.RD(5) := register.io.rd5 }
   }

   // assert only one reader set for each read port.
   assert (PopCount(io.OE.map(e => e(0))) <= 1.U, "[rf] OE(*)(0) has too many enables set.")
   assert (PopCount(io.OE.map(e => e(1))) <= 1.U, "[rf] OE(*)(1) has too many enables set.")
   assert (PopCount(io.OE.map(e => e(2))) <= 1.U, "[rf] OE(*)(2) has too many enables set.")
   assert (PopCount(io.OE.map(e => e(3))) <= 1.U, "[rf] OE(*)(3) has too many enables set.")
   assert (PopCount(io.OE.map(e => e(4))) <= 1.U, "[rf] OE(*)(4) has too many enables set.")
   assert (PopCount(io.OE.map(e => e(5))) <= 1.U, "[rf] OE(*)(5) has too many enables set.")
}


// This is a model of a register file row (which covers one register). Don't ship this either.
class RegisterFile6r3wRegisterModel extends Module
{
   val io = IO(new Bundle {
      val we  = Bool(INPUT)
      val ws  = UInt(INPUT, width = 2)
      val wd0 = UInt(INPUT, width = 64)
      val wd1 = UInt(INPUT, width = 64)
      val wd2 = UInt(INPUT, width = 64)
      val oe  = UInt(INPUT, width = 6)
      val rd0 = UInt(OUTPUT, width = 64)
      val rd1 = UInt(OUTPUT, width = 64)
      val rd2 = UInt(OUTPUT, width = 64)
      val rd3 = UInt(OUTPUT, width = 64)
      val rd4 = UInt(OUTPUT, width = 64)
      val rd5 = UInt(OUTPUT, width = 64)
   })

   val rd0 = Wire(Vec(64, Bool()))
   val rd1 = Wire(Vec(64, Bool()))
   val rd2 = Wire(Vec(64, Bool()))
   val rd3 = Wire(Vec(64, Bool()))
   val rd4 = Wire(Vec(64, Bool()))
   val rd5 = Wire(Vec(64, Bool()))

   for (i <- 0 until 64) yield
   {
      val bit = Module(new Rf6r3wBitModel())
      bit.io.we := io.we
      bit.io.ws := io.ws
      bit.io.wd0 := io.wd0(i)
      bit.io.wd1 := io.wd1(i)
      bit.io.wd2 := io.wd2(i)
      bit.io.oe := io.oe
      rd0(i) := bit.io.rd0
      rd1(i) := bit.io.rd1
      rd2(i) := bit.io.rd2
      rd3(i) := bit.io.rd3
      rd4(i) := bit.io.rd4
      rd5(i) := bit.io.rd5
   }

   io.rd0 := rd0.asUInt
   io.rd1 := rd1.asUInt
   io.rd2 := rd2.asUInt
   io.rd3 := rd3.asUInt
   io.rd4 := rd4.asUInt
   io.rd5 := rd5.asUInt

}


// This is only a model of a register file bit. Warranty voided if synthesized.
class Rf6r3wBitModel extends Module
{
   val io = IO(new Bundle {
      val we  = Bool(INPUT)
      val ws  = UInt(INPUT, width = 2)
      val wd0 = Bool(INPUT)
      val wd1 = Bool(INPUT)
      val wd2 = Bool(INPUT)
      val oe  = UInt(INPUT, width = 6)
      val rd0 = Bool(OUTPUT)
      val rd1 = Bool(OUTPUT)
      val rd2 = Bool(OUTPUT)
      val rd3 = Bool(OUTPUT)
      val rd4 = Bool(OUTPUT)
      val rd5 = Bool(OUTPUT)
   })

   val din = Wire(Bool())
   val dout = Wire(Bool())

   // I don't have a way to specify Z... let's give it something else instead.
   val z = true.B
   io.rd0 := Mux(io.oe(0), dout, z)
   io.rd1 := Mux(io.oe(1), dout, z)
   io.rd2 := Mux(io.oe(2), dout, z)
   io.rd3 := Mux(io.oe(3), dout, z)
   io.rd4 := Mux(io.oe(4), dout, z)
   io.rd5 := Mux(io.oe(5), dout, z)

   val mux_out =
      Mux(io.ws === 0.U, io.wd0,
      Mux(io.ws === 1.U, io.wd1,
      Mux(io.ws === 2.U, io.wd2,
         z)))
   din := mux_out

   assert (!(io.we && io.ws === 3.U), "[rfbitmodel] write-select set to invalid value.")

   val dff = Reg(Bool())
   when (io.we)
   {
      dff := din
   }
   dout := dff

}

