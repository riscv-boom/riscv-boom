//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Rename BusyTable
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util._

/**
 * IO bundle to interact with the busy table.
 * Internally bypasses newly busy registers (.write) to the read ports (.read)
 *
 * @param ren_width renamee width
 * @param num_pregs number of physical registers
 * @param num_read_ports number of read ports to the regfile
 * @param num_wb_ports number of writeback ports to the regfile
 */
class BusyTableHelperIo(
   val ren_width: Int,
   val num_pregs: Int,
   val num_read_ports: Int,
   val num_wb_ports: Int)
   (implicit p: Parameters) extends BoomBundle()(p)
{
   private val preg_sz = log2Ceil(num_pregs)

   // reading out the busy bits
   val prs           = Input(Vec(num_read_ports, UInt(preg_sz.W)))
   val prs_busy      = Output(Vec(num_read_ports, Bool()))

   // marking new registers as busy
   val allocated_pdst = Flipped(Vec(ren_width, new ValidIO(UInt(preg_sz.W))))

   // marking registers being written back as unbusy
   val unbusy_pdst    = Flipped(Vec(num_wb_ports, new ValidIO(UInt(preg_sz.W))))

   val debug = new Bundle { val busytable= Output(Bits(num_pregs.W)) }
}

/**
 * Register P0 is always NOT_BUSY, and cannot be set to BUSY
 * Note: I do NOT bypass from newly busied registers to the read ports.
 * That bypass check should be done elsewhere (this is to get it off the
 * critical path).
 *
 * @param ren_width rename_width
 * @param num_pregs number of physical registers
 * @param num_read_ports number of read ports to the regfile
 * @param num_wb_ports number of writeback ports to the regfile
 */
class BusyTableHelper(
   ren_width: Int,
   num_pregs: Int,
   num_read_ports: Int,
   num_wb_ports: Int)
   (implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new BusyTableHelperIo(ren_width, num_pregs, num_read_ports, num_wb_ports))

   def BUSY     = true.B
   def NOT_BUSY = false.B

   //TODO BUG chisel3
   val table_bsy = RegInit(VecInit(Seq.fill(num_pregs){false.B}))

   for (wb_idx <- 0 until num_wb_ports)
   {
      when (io.unbusy_pdst(wb_idx).valid)
      {
         table_bsy(io.unbusy_pdst(wb_idx).bits) := NOT_BUSY
      }
   }

   for (w <- 0 until ren_width)
   {
      when (io.allocated_pdst(w).valid && io.allocated_pdst(w).bits =/= 0.U)
      {
         table_bsy(io.allocated_pdst(w).bits) := BUSY
      }
   }

   // handle bypassing a clearing of the busy-bit
   for (ridx <- 0 until num_read_ports)
   {
      val just_cleared = io.unbusy_pdst.map(p => p.valid && (p.bits === io.prs(ridx))).reduce(_|_)
      // note: no bypassing of the newly busied (that is done outside this module)
      io.prs_busy(ridx) := (table_bsy(io.prs(ridx)) && !just_cleared)
   }

   io.debug.busytable := table_bsy.asUInt
}

/**
 * Bundle indicating what physical register is busy
 */
class BusyTableOutput extends Bundle
{
   val prs1_busy = Bool()
   val prs2_busy = Bool()
   val prs3_busy = Bool()
}

/**
 * Busy table indicating which physical registers are currently busy
 *
 * @param ren_width rename width
 * @param com_width commit_width
 * @param rtype type of register the free list is operating on
 * @param num_pregs number of physical registers
 * @param num_read_ports number of read ports to the regfile
 * @param num_wb_ports number of writeback ports to the regfile
 */
class BusyTable(
   ren_width: Int,
   rtype: BigInt,
   num_pregs: Int,
   num_read_ports: Int,
   num_wb_ports: Int)
   (implicit p: Parameters) extends BoomModule()(p)
{
   private val preg_sz = log2Ceil(num_pregs)

   val io = IO(new Bundle
   {
      // Inputs
      val ren_will_fire         = Input(Vec(ren_width, Bool()))
      val ren_uops              = Input(Vec(ren_width, new MicroOp()))

      val map_table             = Input(Vec(ren_width, new MapTableOutput(preg_sz)))

      val wb_valids             = Input(Vec(num_wb_ports, Bool()))
      val wb_pdsts              = Input(Vec(num_wb_ports, UInt(preg_sz.W)))

      // Outputs
      val values                = Output(Vec(ren_width, new BusyTableOutput()))

      val debug                 = new Bundle { val busytable= Output(Bits(num_pregs.W)) }
   })

   val busy_table = Module(new BusyTableHelper(
      ren_width = ren_width,
      num_pregs = num_pregs,
      num_read_ports = num_read_ports,
      num_wb_ports = num_wb_ports))

   // figure out if we need to bypass a newly allocated physical register from a previous instruction in this cycle.
   val prs1_was_bypassed = WireInit(VecInit(Seq.fill(ren_width) {false.B}))
   val prs2_was_bypassed = WireInit(VecInit(Seq.fill(ren_width) {false.B}))
   val prs3_was_bypassed = WireInit(VecInit(Seq.fill(ren_width) {false.B}))
   for {
      w <- 0 until ren_width
      xx <- w-1 to 0 by -1
   }{
      when (io.ren_uops(w).lrs1_rtype === rtype.U && io.ren_will_fire(xx) && io.ren_uops(xx).ldst_val &&
            io.ren_uops(xx).dst_rtype === rtype.U && (io.ren_uops(w).lrs1 === io.ren_uops(xx).ldst))
         { prs1_was_bypassed(w) := true.B }
      when (io.ren_uops(w).lrs2_rtype === rtype.U && io.ren_will_fire(xx) && io.ren_uops(xx).ldst_val &&
            io.ren_uops(xx).dst_rtype === rtype.U && (io.ren_uops(w).lrs2 === io.ren_uops(xx).ldst))
         { prs2_was_bypassed(w) := true.B }

      if (rtype == RT_FLT.litValue)
      {
         when (io.ren_uops(w).frs3_en && io.ren_will_fire(xx) && io.ren_uops(xx).ldst_val &&
               io.ren_uops(xx).dst_rtype === rtype.U && (io.ren_uops(w).lrs3 === io.ren_uops(xx).ldst))
            { prs3_was_bypassed(w) := true.B }
      }
   }

   if (rtype == RT_FIX.litValue || (rtype == RT_FLT.litValue && ren_width*3 == num_read_ports))
   {
      // In this case we statically allocate busy table ports to uops
      var rd_idx = 0
      for (w <- 0 until ren_width)
      {
         // Reading the Busy Bits
         // for critical path reasons, we speculatively read out the busy-bits assuming no dependencies between uops
         // then verify if the uop actually uses a register and if it depends on a newly unfreed register
         busy_table.io.prs(rd_idx) := io.map_table(w).prs1
         io.values(w).prs1_busy := io.ren_uops(w).lrs1_rtype === rtype.U &&
                                   (busy_table.io.prs_busy(rd_idx) || prs1_was_bypassed(w))
         rd_idx += 1


         busy_table.io.prs(rd_idx) := io.map_table(w).prs2
         io.values(w).prs2_busy := io.ren_uops(w).lrs2_rtype === rtype.U &&
                                   (busy_table.io.prs_busy(rd_idx) || prs2_was_bypassed(w))
         rd_idx += 1

         if (rtype == RT_FLT.litValue)
         {
            busy_table.io.prs(rd_idx) := io.map_table(w).prs3
            io.values(w).prs3_busy := (io.ren_uops(w).frs3_en) && (busy_table.io.prs_busy(rd_idx) || prs3_was_bypassed(w))
            rd_idx += 1
         }
         else
         {
            io.values(w).prs3_busy := false.B
         }
      }
      require(rd_idx == num_read_ports)
   }
   else
   {
      // In this case we dynamically allocate busy table ports
      var rd_idx = 0.U
      busy_table.io.prs := DontCare
      for (w <- 0 until ren_width)
      {
         val read_lrs1 = io.ren_uops(w).lrs1_rtype === rtype.U
         when (read_lrs1) { busy_table.io.prs(rd_idx) := io.map_table(w).prs1 }
         io.values(w).prs1_busy := read_lrs1 && (busy_table.io.prs_busy(rd_idx) || prs1_was_bypassed(w))
         rd_idx = rd_idx + read_lrs1

         val read_lrs2 = io.ren_uops(w).lrs2_rtype === rtype.U
         when (read_lrs2) { busy_table.io.prs(rd_idx) := io.map_table(w).prs2 }
         io.values(w).prs2_busy := read_lrs2 && (busy_table.io.prs_busy(rd_idx) || prs2_was_bypassed(w))
         rd_idx = rd_idx + read_lrs2

         val read_lrs3 = io.ren_uops(w).lrs3_rtype === rtype.U
         when (read_lrs3) { busy_table.io.prs(rd_idx) := io.map_table(w).prs3 }
         io.values(w).prs3_busy := read_lrs3 && (busy_table.io.prs_busy(rd_idx) || prs3_was_bypassed(w))
         rd_idx = rd_idx + read_lrs3
      }
      assert(rd_idx <= num_read_ports.U,
         "We are trying to use more busy table read ports than we are allowed to!")
   }

   for (w <- 0 until ren_width)
   {
       // Updating the Table (new busy register)
      busy_table.io.allocated_pdst(w).valid := io.ren_will_fire(w) &&
                                               io.ren_uops(w).ldst_val &&
                                               io.ren_uops(w).dst_rtype === rtype.U
      busy_table.io.allocated_pdst(w).bits  := io.ren_uops(w).pdst
   }

   // Clear Busy-bit
   for (i <- 0 until num_wb_ports)
   {
      busy_table.io.unbusy_pdst(i).valid := io.wb_valids(i)
      busy_table.io.unbusy_pdst(i).bits  := io.wb_pdsts(i)
   }

   // scalastyle:on
   io.debug := busy_table.io.debug
}
