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


package boom.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import scala.collection.mutable.ArrayBuffer
import boom.common._
import boom.util._


// A behavorial model of a Register File. You will likely want to blackbox this for more than modest port counts.
class VectorPredRegisterFileBehavioral(
   num_registers: Int,
   num_read_ports: Int,
   num_write_ports: Int,
   register_width: Int,
   bypassable_array: Seq[Boolean])
   (implicit p: Parameters)
      extends RegisterFile(num_registers, num_read_ports, num_write_ports, register_width, bypassable_array)
with freechips.rocketchip.rocket.constants.VecCfgConstants
{
   // --------------------------------------------------------------

   val regfile = SyncReadMem(num_registers, Vec(register_width, Bool()))

   // --------------------------------------------------------------
   // Read ports.

   val read_data = Wire(Vec(num_read_ports, UInt(width = register_width.W)))

   // Register the read port addresses to give a full cycle to the RegisterRead Stage (if desired).
   val read_addrs = io.read_ports map { p => p.addr }

   for (i <- 0 until num_read_ports)
   {
      read_data(i) := regfile.read(read_addrs(i), io.read_ports(i).enable).asUInt
   }


   require (bypassable_array.length == io.write_ports.length)
   assert (!bypassable_array.reduce(_||_), "No bypassing for vector regfile")

   for (i <- 0 until num_read_ports)
   {
      io.read_ports(i).data := read_data(i)
   }


   // --------------------------------------------------------------
   // Write ports.
   for (wport <- io.write_ports)
   {
      when (wport.valid && (wport.bits.addr =/= 0.U))
      {
         val full_mask = Wire(UInt(width=64.W))
         val full_data = Wire(UInt(width=64.W))
         full_mask := wport.bits.mask << wport.bits.eidx
         full_data := wport.bits.data << wport.bits.eidx
         regfile.write(wport.bits.addr, VecInit(full_data.toBools),
                                        VecInit(full_mask.toBools))
      }
   }
}
