//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Rename Map Table
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util._

class RenameMapTableElementIo(val pl_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val element            = Output(UInt(PREG_SZ.W))

   val wens               = Input(Vec(pl_width, Bool()))
   val ren_pdsts          = Input(Vec(pl_width, UInt(PREG_SZ.W)))

   val ren_br_vals        = Input(Vec(pl_width, Bool()))
   val ren_br_tags        = Input(Vec(pl_width, UInt(BR_TAG_SZ.W)))

   val br_mispredict      = Input(Bool())
   val br_mispredict_tag  = Input(UInt(BR_TAG_SZ.W))

   // rollback (on exceptions)
   // TODO REMOVE THIS ROLLBACK PORT, since wens is mutually exclusive with rollback_wens
   val rollback_wen        = Input(Bool())
   val rollback_stale_pdst = Input(UInt(PREG_SZ.W))

   // TODO scr option
   val flush_pipeline      = Input(Bool())
   val commit_wen          = Input(Bool())
   val commit_pdst         = Input(UInt(PREG_SZ.W))
   val committed_element   = Output(UInt(PREG_SZ.W))

}

class RenameMapTableElement(pipeline_width: Int, always_zero: Boolean)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new RenameMapTableElementIo(pipeline_width))

   // Note: I don't use a "valid" signal, since it's annoying to deal with and
   // only necessary until the map tables are filled. So instead I reset the
   // map table to all point to P0. I'm not sure which is less expensive.  The
   // corner-case to deal with is the "stale" register needs to be correct and
   // valid, so that freeing it won't free an actual register that was handed
   // out in the meantime. A software solution is also possible, but I'm
   // unwilling to trust that.

   val element = RegInit(0.U(PREG_SZ.W))

   // handle branch speculation
   val element_br_copies = Mem(MAX_BR_COUNT, UInt(PREG_SZ.W))

   // this is possibly the hardest piece of code I have ever had to reason about in my LIFE.
   // Or maybe that's the 5am talking.
   // on every branch, make a copy of the rename map table state
   // if jal/jalr, we want to capture our own setting of this register
   // We need to know the AGE of the branch!
   // 1st, is "wen" (incoming)
   // 2nd, is older instructions in same bundle
   // 3rd, current element

   for (w <- 0 until pipeline_width)
   {
      var elm_cases = Array((false.B, 0.U(PREG_SZ.W)))

      for (xx <- w to 0 by -1)
      {
         elm_cases ++= Array((io.wens(xx),  io.ren_pdsts(xx)))
      }

      when (io.ren_br_vals(w))
      {
         element_br_copies(io.ren_br_tags(w)) := MuxCase(element, elm_cases)
      }
   }

   // reset table on mispredict
   when(io.br_mispredict)
   {
      element := element_br_copies(io.br_mispredict_tag)
   }
   // rollback to the previous mapping
   .elsewhen (io.rollback_wen)
   {
      element := io.rollback_stale_pdst
   }
   // free list is giving us a new pdst
   .elsewhen (io.wens.reduce(_|_))
   {
      // give write priority to the last instruction in the bundle
      element := PriorityMux(io.wens.reverse, io.ren_pdsts.reverse)
   }

   if (ENABLE_COMMIT_MAP_TABLE)
   {
      val committed_element = RegInit(0.U(PREG_SZ.W))
      when (io.commit_wen)
      {
         committed_element := io.commit_pdst
      }
      when (io.flush_pipeline)
      {
         element := committed_element
      }
      io.committed_element := committed_element
   }

   // outputs
   io.element := element

   if (always_zero) io.element := 0.U
}

// Pass out the new physical register specifiers.
class MapTableOutput(val preg_sz: Int) extends Bundle
{
   val prs1              = UInt(preg_sz.W)
   val prs2              = UInt(preg_sz.W)
   val prs3              = UInt(preg_sz.W)
   val stale_pdst        = UInt(preg_sz.W)
}

class RenameMapTable(
   pl_width: Int,
   rtype: BigInt,
   num_logical_registers: Int,
   num_physical_registers: Int
   )(implicit p: Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
{
   private val preg_sz = log2Ceil(num_physical_registers)

   val io = IO(new BoomBundle()(p)
   {
      // Inputs
      val brinfo           = Input(new BrResolutionInfo())
      val kill             = Input(Bool())

      val ren_will_fire    = Input(Vec(pl_width, Bool()))
      val ren_uops         = Input(Vec(pl_width, new MicroOp()))
      val ren_br_vals      = Input(Vec(pl_width, Bool()))

      val com_valids       = Input(Vec(pl_width, Bool()))
      val com_uops         = Input(Vec(pl_width, new MicroOp()))
      val com_rbk_valids   = Input(Vec(pl_width, Bool()))
      val flush_pipeline   = Input(Bool()) // only used for SCR (single-cycle reset)

      val debug_inst_can_proceed = Input(Vec(pl_width, Bool()))
      val debug_freelist_can_allocate = Input(Vec(pl_width, Bool()))

      // Outputs
      val values           = Output(Vec(pl_width, new MapTableOutput(preg_sz)))
   })

   val entries = for (i <- 0 until num_logical_registers) yield
   {
      val entry = Module(new RenameMapTableElement(pl_width, always_zero = (i==0 && rtype == RT_FIX.litValue)))
      entry
   }
   val map_table_io = VecInit(entries.map(_.io))

   map_table_io.zipWithIndex.map{ case (entry, i) =>
   {
      // TODO get rid of this extra, init logic
      entry.rollback_wen := false.B
      entry.rollback_stale_pdst := io.com_uops(0).stale_pdst
      entry.commit_wen := false.B
      entry.commit_pdst := io.com_uops(0).pdst

      for (w <- 0 until pl_width)
      {
         entry.wens(w)        := io.ren_uops(w).ldst === i.U &&
                                           io.ren_will_fire(w) &&
                                           io.ren_uops(w).ldst_val &&
                                           io.ren_uops(w).dst_rtype === rtype.U &&
                                           !io.kill

         assert (!(entry.wens(w) && !io.debug_inst_can_proceed(w)), "[maptable] wen shouldn't be high.")
         assert (!(entry.wens(w) && !io.debug_freelist_can_allocate(w)), "[maptable] wen shouldn't be high.")

         entry.ren_pdsts(w)   := io.ren_uops(w).pdst
         entry.ren_br_tags(w) := io.ren_uops(w).br_tag
      }
      entry.ren_br_vals := io.ren_br_vals

      entry.br_mispredict     := io.brinfo.mispredict
      entry.br_mispredict_tag := io.brinfo.tag

      entry.flush_pipeline    := io.flush_pipeline
   }}

   // backwards, because rollback must give highest priority to 0 (the oldest instruction)
   for (w <- pl_width-1 to 0 by -1)
   {
      val ldst = io.com_uops(w).ldst
      when (io.com_rbk_valids(w) && io.com_uops(w).dst_rtype === rtype.U)
      {
         map_table_io(ldst).rollback_wen        := true.B
         map_table_io(ldst).rollback_stale_pdst := io.com_uops(w).stale_pdst
      }
   }

   if (ENABLE_COMMIT_MAP_TABLE)
   {
      for (w <- 0 until pl_width)
      {
         val ldst = io.com_uops(w).ldst
         when (io.com_valids(w) && (io.com_uops(w).dst_rtype === rtype.U))
         {
            map_table_io(ldst).commit_wen := true.B
            map_table_io(ldst).commit_pdst := io.com_uops(w).pdst
         }
      }
   }

   // Read out the map-table entries ASAP, then deal with bypassing busy-bits later.
   //private val map_table_output = Wire(Vec(pl_width*3, UInt(PREG_SZ.W)))
   private val map_table_output = Seq.fill(pl_width*3)(Wire(UInt(PREG_SZ.W)))
   def map_table_prs1(w:Int) = map_table_output(w+0*pl_width)
   def map_table_prs2(w:Int) = map_table_output(w+1*pl_width)
   def map_table_prs3(w:Int) = map_table_output(w+2*pl_width)

   for (w <- 0 until pl_width)
   {
      map_table_prs1(w) := map_table_io(io.ren_uops(w).lrs1).element
      map_table_prs2(w) := map_table_io(io.ren_uops(w).lrs2).element
      if (rtype == RT_FLT.litValue)
      {
         map_table_prs3(w) := map_table_io(io.ren_uops(w).lrs3).element
      }
      else
      {
         map_table_prs3(w) := 0.U
      }
   }

   // Bypass the physical register mappings
   for (w <- 0 until pl_width)
   {
      var rs1_cases =  Array((false.B, 0.U(PREG_SZ.W)))
      var rs2_cases =  Array((false.B, 0.U(PREG_SZ.W)))
      var rs3_cases =  Array((false.B, 0.U(PREG_SZ.W)))
      var stale_cases= Array((false.B, 0.U(PREG_SZ.W)))

      // Handle bypassing new physical destinations to operands (and stale destination)
      // scalastyle:off
      for (xx <- w-1 to 0 by -1)
      {
         rs1_cases  ++= Array((io.ren_uops(w).lrs1_rtype === rtype.U && io.ren_will_fire(xx) && io.ren_uops(xx).ldst_val && io.ren_uops(xx).dst_rtype === rtype.U && (io.ren_uops(w).lrs1 === io.ren_uops(xx).ldst), (io.ren_uops(xx).pdst)))
         rs2_cases  ++= Array((io.ren_uops(w).lrs2_rtype === rtype.U && io.ren_will_fire(xx) && io.ren_uops(xx).ldst_val && io.ren_uops(xx).dst_rtype === rtype.U && (io.ren_uops(w).lrs2 === io.ren_uops(xx).ldst), (io.ren_uops(xx).pdst)))
         stale_cases++= Array((io.ren_uops(w).dst_rtype === rtype.U  && io.ren_will_fire(xx) && io.ren_uops(xx).ldst_val && io.ren_uops(xx).dst_rtype === rtype.U && (io.ren_uops(w).ldst === io.ren_uops(xx).ldst), (io.ren_uops(xx).pdst)))

         if (rtype == RT_FLT.litValue)
         {
            rs3_cases  ++= Array((
                  io.ren_uops(w).frs3_en && io.ren_will_fire(xx) && io.ren_uops(xx).ldst_val && io.ren_uops(xx).dst_rtype === rtype.U && (io.ren_uops(w).lrs3 === io.ren_uops(xx).ldst),
                  (io.ren_uops(xx).pdst)))
         }
      }

      // add default case where we can just read the map table for our information
      if (rtype == RT_FIX.litValue)
      {
         rs1_cases ++= Array((io.ren_uops(w).lrs1_rtype === rtype.U && (io.ren_uops(w).lrs1 =/= 0.U), map_table_prs1(w)))
         rs2_cases ++= Array((io.ren_uops(w).lrs2_rtype === rtype.U && (io.ren_uops(w).lrs2 =/= 0.U), map_table_prs2(w)))
      }
      else
      {
         rs1_cases ++= Array((io.ren_uops(w).lrs1_rtype === rtype.U, map_table_prs1(w)))
         rs2_cases ++= Array((io.ren_uops(w).lrs2_rtype === rtype.U, map_table_prs2(w)))
      }
      rs3_cases ++= Array((io.ren_uops(w).frs3_en, map_table_prs3(w)))

      // Set outputs.
      io.values(w).prs1       := MuxCase(io.ren_uops(w).lrs1, rs1_cases)
      io.values(w).prs2       := MuxCase(io.ren_uops(w).lrs2, rs2_cases)
      if (rtype == RT_FLT.litValue)
         {io.values(w).prs3 := MuxCase(io.ren_uops(w).lrs3, rs3_cases)}
      io.values(w).stale_pdst := MuxCase(map_table_io(io.ren_uops(w).ldst).element, stale_cases)

      if (rtype == RT_FIX.litValue)
      {
         assert (!(io.ren_uops(w).lrs1 === 0.U && io.ren_uops(w).lrs1_rtype === RT_FIX && io.values(w).prs1 =/= 0.U), "lrs1==0 but maptable(" + w + ") returning non-zero.")
         assert (!(io.ren_uops(w).lrs2 === 0.U && io.ren_uops(w).lrs2_rtype === RT_FIX && io.values(w).prs2 =/= 0.U), "lrs2==0 but maptable(" + w + ") returning non-zero.")
         assert (!(io.ren_uops(w).lrs1 === 0.U && io.ren_uops(w).lrs1_rtype === RT_FIX && map_table_prs1(w) =/= 0.U), "lrs1==0 but maptable(" + w + ") returning non-zero.")
         assert (!(io.ren_uops(w).lrs2 === 0.U && io.ren_uops(w).lrs2_rtype === RT_FIX && map_table_prs2(w) =/= 0.U), "lrs2==0 but maptable(" + w + ") returning non-zero.")
      }
      // scalastyle:on
   }
}
