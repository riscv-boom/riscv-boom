//**************************************************************************
// RISCV Processor Datapath Register File
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2013 May 1


package BOOM
{

import Chisel._
import Node._


class RegisterFileReadPortIO(addr_width: Int, data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val addr = UInt(INPUT, addr_width)
   val data = Bits(OUTPUT, data_width)
}

class RegisterFileWritePortIO(addr_width: Int, data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val wen  = Bool(INPUT)
   val addr = UInt(INPUT, addr_width)
   val data = Bits(INPUT, data_width)
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
      val read_ports = Vec.fill(num_read_ports) { (new RegisterFileReadPortIO(PREG_SZ, register_width)) }
      val write_ports = Vec.fill(num_write_ports) { (new RegisterFileWritePortIO(PREG_SZ, register_width)) }

//      val debug = new BoomBundle()(p) {
//         val registers = Vec.fill(num_registers) { Bits(width = register_width) }
//      }.asOutput
   }

   // --------------------------------------------------------------

   val regfile = Mem(Bits(width=register_width), num_registers)

   // --------------------------------------------------------------

   val read_data = Vec.fill(num_read_ports) { Bits(width = register_width) }
   for (i <- 0 until num_read_ports)
   {
      read_data(i) := Mux(io.read_ports(i).addr === UInt(0), Bits(0),
                                                            regfile(io.read_ports(i).addr))
   }

   // --------------------------------------------------------------
   // bypass out of the ALU's write ports

   if (enable_bypassing)
   {
      for (i <- 0 until num_read_ports)
      {
         val bypass_ens = io.write_ports.map(x => x.wen &&
                                                  x.addr != UInt(0) &&
                                                  x.addr === io.read_ports(i).addr)

         val bypass_data = Mux1H(Vec(bypass_ens), Vec(io.write_ports.map(_.data)))

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

   for (i <- 0 until num_write_ports)
   {
      when (io.write_ports(i).wen && (io.write_ports(i).addr != UInt(0)))
      {
         regfile(io.write_ports(i).addr) := io.write_ports(i).data
      }
//      if (DEBUG_PRINTF)
//      {
//         printf("writeport[%d], %s -> p%d = 0x%x\n", UInt(i), Mux(io.write_ports(i).wen, Str("WEN"), Str(" "))
//            , io.write_ports(i).addr
//            , io.write_ports(i).data
//            );
//      }
   }


   // Debug
//   for (i <- 0 until num_registers)
//   {
//      io.debug.registers(i) := regfile(UInt(i))
//   }
}


}
