//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Datapath Register File
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2013 May 1


package boom

import Chisel._
import cde.Parameters

class RegisterFileReadPortIO(addr_width: Int, data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val addr = UInt(INPUT, addr_width)
   val data = Bits(OUTPUT, data_width)
   override def cloneType = new RegisterFileReadPortIO(addr_width, data_width)(p).asInstanceOf[this.type]
}

class RegisterFileWritePort(addr_width: Int, data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val addr = UInt(width = addr_width)
   val data = Bits(width = data_width)
   override def cloneType = new RegisterFileWritePort(addr_width, data_width)(p).asInstanceOf[this.type]
}


// utility function to turn ExeUnitResps to match the regfile's WritePort I/Os.
object WritePort
{
   def apply(enq: DecoupledIO[ExeUnitResp], addr_width: Int, data_width: Int)
   (implicit p: Parameters): DecoupledIO[RegisterFileWritePort] =
   {
      val wport = Wire(Decoupled(new RegisterFileWritePort(addr_width, data_width)))
      wport.valid := enq.valid
      wport.bits.addr := enq.bits.uop.pdst
      wport.bits.data := enq.bits.data
      enq.ready := wport.ready

      wport
   }
}


class RegisterFile( num_registers: Int
                  , num_read_ports: Int
                  , num_write_ports: Int
                  , register_width: Int
                  , enable_bypassing: Boolean)
       (implicit p: Parameters) extends BoomModule()(p)
{
   val io = new BoomBundle()(p)
   {
      val read_ports = Vec(num_read_ports, new RegisterFileReadPortIO(PREG_SZ, register_width))
      val write_ports = Vec(num_write_ports, Decoupled(new RegisterFileWritePort(PREG_SZ, register_width))).flip
   }

   // --------------------------------------------------------------

   val regfile = Mem(num_registers, Bits(width=register_width))

   // --------------------------------------------------------------
   // Read ports.

   val read_data = Wire(Vec(num_read_ports, Bits(width = register_width)))

   for (i <- 0 until num_read_ports)
   {
      read_data(i) := Mux(io.read_ports(i).addr === UInt(0), Bits(0),
                                                            regfile(io.read_ports(i).addr))
   }


   // --------------------------------------------------------------
   // Bypass out of the ALU's write ports.

   if (enable_bypassing)
   {
      for (i <- 0 until num_read_ports)
      {
         val bypass_ens = io.write_ports.map(x => x.valid &&
                                                  x.bits.addr =/= UInt(0) &&
                                                  x.bits.addr === io.read_ports(i).addr)

         val bypass_data = Mux1H(Vec(bypass_ens), Vec(io.write_ports.map(_.bits.data)))

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
      wport.ready := Bool(true)
      when (wport.valid && (wport.bits.addr =/= UInt(0)))
      {
         regfile(wport.bits.addr) := wport.bits.data
      }
   }


   // --------------------------------------------------------------

   private val rf_cost = (num_read_ports+num_write_ports)*(num_read_ports+2*num_write_ports)
   private val type_str = if (register_width == fLen+1) "Floating Point" else "Integer"
   override def toString: String =
      "\n   ==" + type_str + " Regfile==" +
      "\n   Num RF Read Ports     : " + num_read_ports +
      "\n   Num RF Write Ports    : " + num_write_ports +
      "\n   RF Cost (R+W)*(R+2W)  : " + rf_cost

}

