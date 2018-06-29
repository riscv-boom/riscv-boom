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
   // --------------------------------------------------------------

   val regfile = Mem(num_registers, UInt(width=register_width.W))

   // --------------------------------------------------------------
   // Read ports.

   val read_data = Wire(Vec(num_read_ports, UInt(width = register_width.W)))

   // Register the read port addresses to give a full cycle to the RegisterRead Stage (if desired).
   val read_addrs =
      if (regreadLatency == 0) {
         io.read_ports map {_.addr}
      } else {
         require (regreadLatency == 1)
         io.read_ports.map(p => RegNext(p.addr))
      }

   for (i <- 0 until num_read_ports)
   {
      read_data(i) :=
         Mux(read_addrs(i) === UInt(0),
            UInt(0),
            regfile(read_addrs(i)))
   }


   require (bypassable_array.length == io.write_ports.length)
   assert (!bypassable_array.reduce(_||_), "No bypassing for vector regfile")

   for (i <- 0 until num_read_ports)
   {
      io.read_ports(i).data := read_data(i)
   }

   def toBytes(wdata:UInt, bitmask:UInt, eidx:UInt, rd_vew:UInt): (UInt, UInt) = {
      val mask = Reverse(Cat((0 until 128) map {i => bitmask(i / 8)}))
      val eidx_shifted = eidx << MuxLookup(rd_vew, VEW_8, Array(
         VEW_8  -> UInt(0),
         VEW_16 -> UInt(1),
         VEW_32 -> UInt(2),
         VEW_64 -> UInt(3)))
      val strip_off = eidx_shifted(3,0)
      val shifted_mask = mask << (strip_off << 3)
      val shifted_wdata = wdata << (strip_off << 3)
      (shifted_mask, shifted_wdata)
   }

   // --------------------------------------------------------------
   // Write ports.
   for (wport <- io.write_ports)
   {
      wport.ready := Bool(true)
      when (wport.valid && (wport.bits.addr =/= UInt(0)))
      {
         val (gen_mask, gen_wdata) = toBytes(wport.bits.data, wport.bits.mask, wport.bits.eidx, wport.bits.rd_vew)
         val to_keep = regfile(wport.bits.addr) & ~gen_mask
         val to_write = gen_wdata & gen_mask
         regfile(wport.bits.addr) := to_keep | to_write
      }
   }
}
