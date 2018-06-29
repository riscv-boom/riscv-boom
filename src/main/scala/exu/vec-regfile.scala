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
class VectorRegisterFileBehavorial(
   num_registers: Int,
   num_read_ports: Int,
   num_write_ports: Int,
   register_width: Int,
   bypassable_array: Seq[Boolean])
   (implicit p: Parameters)
      extends RegisterFile(num_registers, num_read_ports, num_write_ports, register_width, bypassable_array)
with freechips.rocketchip.rocket.constants.VecCfgConstants
{
   // There's no way this is the easiest way to do this
   def toVec (data:UInt, elem_width:Int, elem_count:Int):Vec[UInt] = {
      Vec((0 until elem_count) map {i =>
         data(elem_width*(i+1)-1, elem_width*i)
      })
   }

   // --------------------------------------------------------------

   val regfile = SyncReadMem(num_registers, Vec(register_width/8, UInt(width=8.W)))

   // --------------------------------------------------------------
   // Read ports.

   val read_data = Wire(Vec(num_read_ports, UInt(width = register_width.W)))

   // Register the read port addresses to give a full cycle to the RegisterRead Stage (if desired).
   val read_addrs = io.read_ports map { p => p.addr }

   for (i <- 0 until num_read_ports)
   {
      read_data(i) :=
         Mux(RegNext(read_addrs(i)) === UInt(0),
            UInt(0),
            regfile.read(read_addrs(i), io.read_ports(i).enable).asUInt) // TODO_Vec: Maybe gate this off?
   }


   require (bypassable_array.length == io.write_ports.length)
   assert (!bypassable_array.reduce(_||_), "No bypassing for vector regfile")

   for (i <- 0 until num_read_ports)
   {
      io.read_ports(i).data := read_data(i)
   }

   def toBytes(wdata:UInt, bitmask:UInt, eidx:UInt, rd_vew:UInt): (Seq[Bool], Vec[UInt]) = {
      val mask = Reverse(Cat((0 until register_width/8) map {i => bitmask(i)}))
      val eidx_shifted = eidx << MuxLookup(rd_vew, VEW_8, Array(
         VEW_8  -> UInt(0),
         VEW_16 -> UInt(1),
         VEW_32 -> UInt(2),
         VEW_64 -> UInt(3)))
      val strip_off = eidx_shifted(3,0)
      val shifted_mask = mask << (strip_off)
      val shifted_wdata = wdata << (strip_off << 3)
      val gen_mask = shifted_mask(register_width/8-1, 0).toBools
      val gen_data = toVec(shifted_wdata, 8, register_width/8)
      (gen_mask, gen_data)
   }

   // --------------------------------------------------------------
   // Write ports.
   for (wport <- io.write_ports)
   {
      wport.ready := Bool(true)
      when (wport.valid && (wport.bits.addr =/= UInt(0)))
      {
         val (gen_mask, gen_wdata) = toBytes(wport.bits.data, wport.bits.mask, wport.bits.eidx, wport.bits.rd_vew)
         regfile.write(wport.bits.addr, gen_wdata, gen_mask)
      }
   }
}
