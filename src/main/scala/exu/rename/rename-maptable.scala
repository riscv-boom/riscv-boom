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

/**
 * Rename map table element IO
 *
 * @param plWidth pipeline width
 */
class RenameMapTableElementIo(val plWidth: Int, val pregSz: Int)(implicit p: Parameters) extends BoomBundle
{
  val element            = Output(UInt(pregSz.W))

  val wens               = Input(Vec(plWidth, Bool()))
  val ren_pdsts          = Input(Vec(plWidth, UInt(pregSz.W)))

  val ren_br_vals        = Input(Vec(plWidth, Bool()))
  val ren_br_tags        = Input(Vec(plWidth, UInt(brTagSz.W)))

  val br_mispredict      = Input(Bool())
  val br_mispredict_tag  = Input(UInt(brTagSz.W))

  // rollback (on exceptions)
  // TODO REMOVE THIS ROLLBACK PORT, since wens is mutually exclusive with rollback_wens
  val rollback_wen        = Input(Bool())
  val rollback_stale_pdst = Input(UInt(pregSz.W))

  // TODO scr option
  val flush_pipeline      = Input(Bool())
  val commit_wen          = Input(Bool())
  val commit_pdst         = Input(UInt(pregSz.W))
  val committed_element   = Output(UInt(pregSz.W))
}

/**
 * Element in the Rename Map Table. Keeps track of the idx of the physical register, and extra data
 * to recover from branches
 *
 * @param plWidth pipeline width
 * @param always_zero the element is always zero (used for x0)
 */
class RenameMapTableElement(val plWidth: Int, val always_zero: Boolean, val pregSz: Int)(implicit p: Parameters) extends BoomModule
{
  val io = IO(new RenameMapTableElementIo(plWidth, pregSz))

  // Note: I don't use a "valid" signal, since it's annoying to deal with and
  // only necessary until the map tables are filled. So instead I reset the
  // map table to all point to P0. I'm not sure which is less expensive. The
  // corner-case to deal with is the "stale" register needs to be correct and
  // valid, so that freeing it won't free an actual register that was handed
  // out in the meantime. A software solution is also possible, but I'm
  // unwilling to trust that.

  val element = RegInit(0.U(pregSz.W))

  // handle branch speculation
  val element_br_copies = Mem(maxBrCount, UInt(pregSz.W))

  // this is possibly the hardest piece of code I have ever had to reason about in my LIFE.
  // Or maybe that's the 5am talking.
  // on every branch, make a copy of the rename map table state
  // if jal/jalr, we want to capture our own setting of this register
  // We need to know the AGE of the branch!
  // 1st, is "wen" (incoming)
  // 2nd, is older instructions in same bundle
  // 3rd, current element

  for (w <- 0 until plWidth) {
    var elm_cases = Array((false.B, 0.U(pregSz.W)))

    for (xx <- w to 0 by -1) {
       elm_cases ++= Array((io.wens(xx),  io.ren_pdsts(xx)))
    }

    when (io.ren_br_vals(w)) {
      element_br_copies(io.ren_br_tags(w)) := MuxCase(element, elm_cases)
    }
  }

  when(io.br_mispredict) {
    // reset table on mispredict
    element := element_br_copies(io.br_mispredict_tag)
  } .elsewhen (io.rollback_wen) {
    // rollback to the previous mappin
     element := io.rollback_stale_pdst
  } .elsewhen (io.wens.reduce(_|_)) {
    // free list is giving us a new pdst
    // give write priority to the last instruction in the bundle
    element := PriorityMux(io.wens.reverse, io.ren_pdsts.reverse)
  }

  if (enableCommitMapTable) {
    val committed_element = RegInit(0.U(pregSz.W))
    when (io.commit_wen) {
      committed_element := io.commit_pdst
    }
    when (io.flush_pipeline) {
      element := committed_element
    }
    io.committed_element := committed_element
  }

  // outputs
  io.element := element

  if (always_zero) io.element := 0.U
}

/**
 * Output bundle of the map table
 * Pass out the new physical register specifiers.
 *
 * @param pregSz size of the physical register index in bits
 */
class MapTableOutput(val pregSz: Int) extends Bundle
{
  val prs1       = UInt(pregSz.W)
  val prs2       = UInt(pregSz.W)
  val prs3       = UInt(pregSz.W)
  val stale_pdst = UInt(pregSz.W)
}

/**
 * Rename map table which maps architectural registers to physical registers
 *
 * @param plWidth pipeline width
 * @param rtype type of registers being mapped
 * @param numLregs number of logical ISA registers
 * @param numPregs number of physical registers
 */
class RenameMapTable(
  val plWidth: Int,
  val rtype: BigInt,
  val numLregs: Int,
  val numPregs: Int
  )(implicit p: Parameters) extends BoomModule
  with HasBoomCoreParameters
{
  val pregSz = log2Ceil(numPregs)

  val io = IO(new BoomBundle {
    // Inputs
    val brinfo           = Input(new BrResolutionInfo())
    val kill             = Input(Bool())

    val ren_will_fire    = Input(Vec(plWidth, Bool()))
    val ren_uops         = Input(Vec(plWidth, new MicroOp()))
    val ren_br_vals      = Input(Vec(plWidth, Bool()))

    val com_valids       = Input(Vec(plWidth, Bool()))
    val com_uops         = Input(Vec(plWidth, new MicroOp()))
    val com_rbk_valids   = Input(Vec(plWidth, Bool()))
    val flush_pipeline   = Input(Bool()) // only used for SCR (single-cycle reset)

    val debug_inst_can_proceed = Input(Vec(plWidth, Bool()))
    val debug_freelist_can_allocate = Input(Vec(plWidth, Bool()))

    // Outputs
    val values           = Output(Vec(plWidth, new MapTableOutput(pregSz)))
  })

  val entries = for (i <- 0 until numLregs) yield {
    val entry = Module(new RenameMapTableElement(plWidth, always_zero = (i==0 && rtype == RT_FIX.litValue), pregSz))
    entry
  }
  val map_table_io = VecInit(entries.map(_.io))

  map_table_io.zipWithIndex.map{ case (entry, i) => {
    // TODO get rid of this extra, init logic
    entry.rollback_wen := false.B
    entry.rollback_stale_pdst := io.com_uops(0).stale_pdst
    entry.commit_wen := false.B
    entry.commit_pdst := io.com_uops(0).pdst

    for (w <- 0 until plWidth) {
      entry.wens(w) := io.ren_uops(w).ldst === i.U &&
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
  for (w <- plWidth-1 to 0 by -1) {
    val ldst = io.com_uops(w).ldst
    when (io.com_rbk_valids(w) && io.com_uops(w).dst_rtype === rtype.U) {
      map_table_io(ldst).rollback_wen        := true.B
      map_table_io(ldst).rollback_stale_pdst := io.com_uops(w).stale_pdst
    }
  }

  if (enableCommitMapTable) {
    for (w <- 0 until plWidth) {
      val ldst = io.com_uops(w).ldst
      when (io.com_valids(w) && (io.com_uops(w).dst_rtype === rtype.U)) {
        map_table_io(ldst).commit_wen := true.B
        map_table_io(ldst).commit_pdst := io.com_uops(w).pdst
      }
    }
  }

  // Read out the map-table entries ASAP, then deal with bypassing busy-bits later.
  //private val map_table_output = Wire(Vec(plWidth*3, UInt(pregSz.W)))
  private val map_table_output = Seq.fill(plWidth*3)(Wire(UInt(pregSz.W)))
  def map_table_prs1(w:Int) = map_table_output(w+0*plWidth)
  def map_table_prs2(w:Int) = map_table_output(w+1*plWidth)
  def map_table_prs3(w:Int) = map_table_output(w+2*plWidth)

  for (w <- 0 until plWidth) {
    map_table_prs1(w) := map_table_io(io.ren_uops(w).lrs1).element
    map_table_prs2(w) := map_table_io(io.ren_uops(w).lrs2).element
    if (rtype == RT_FLT.litValue) {
      map_table_prs3(w) := map_table_io(io.ren_uops(w).lrs3).element
    } else {
      map_table_prs3(w) := 0.U
    }
  }

  // Bypass the physical register mappings
  for (w <- 0 until plWidth) {
    var rs1_cases =  Array((false.B, 0.U(pregSz.W)))
    var rs2_cases =  Array((false.B, 0.U(pregSz.W)))
    var rs3_cases =  Array((false.B, 0.U(pregSz.W)))
    var stale_cases= Array((false.B, 0.U(pregSz.W)))

    // Handle bypassing new physical destinations to operands (and stale destination)
    // scalastyle:off
    for (xx <- w-1 to 0 by -1) {
      rs1_cases ++= Array((
        io.ren_uops(w).lrs1_rtype === rtype.U &&
        io.ren_will_fire(xx) &&
        io.ren_uops(xx).ldst_val &&
        io.ren_uops(xx).dst_rtype === rtype.U &&
        (io.ren_uops(w).lrs1 === io.ren_uops(xx).ldst),
        (io.ren_uops(xx).pdst)))
      rs2_cases ++= Array((
        io.ren_uops(w).lrs2_rtype === rtype.U &&
        io.ren_will_fire(xx) &&
        io.ren_uops(xx).ldst_val &&
        io.ren_uops(xx).dst_rtype === rtype.U &&
        (io.ren_uops(w).lrs2 === io.ren_uops(xx).ldst),
        (io.ren_uops(xx).pdst)))
      stale_cases ++= Array((
        io.ren_uops(w).dst_rtype === rtype.U &&
        io.ren_will_fire(xx) &&
        io.ren_uops(xx).ldst_val &&
        io.ren_uops(xx).dst_rtype === rtype.U &&
        (io.ren_uops(w).ldst === io.ren_uops(xx).ldst),
        (io.ren_uops(xx).pdst)))

      if (rtype == RT_FLT.litValue) {
        rs3_cases  ++= Array((
          io.ren_uops(w).frs3_en &&
          io.ren_will_fire(xx) &&
          io.ren_uops(xx).ldst_val &&
          io.ren_uops(xx).dst_rtype === rtype.U &&
          (io.ren_uops(w).lrs3 === io.ren_uops(xx).ldst),
          (io.ren_uops(xx).pdst)))
       }
    }

    // Add default case where we can just read the map table
    if (rtype == RT_FIX.litValue) {
       rs1_cases ++= Array((io.ren_uops(w).lrs1_rtype === rtype.U && (io.ren_uops(w).lrs1 =/= 0.U), map_table_prs1(w)))
       rs2_cases ++= Array((io.ren_uops(w).lrs2_rtype === rtype.U && (io.ren_uops(w).lrs2 =/= 0.U), map_table_prs2(w)))
    } else {
       rs1_cases ++= Array((io.ren_uops(w).lrs1_rtype === rtype.U, map_table_prs1(w)))
       rs2_cases ++= Array((io.ren_uops(w).lrs2_rtype === rtype.U, map_table_prs2(w)))
    }
    rs3_cases ++= Array((io.ren_uops(w).frs3_en, map_table_prs3(w)))

    // Set outputs.
    io.values(w).prs1       := MuxCase(io.ren_uops(w).lrs1, rs1_cases)
    io.values(w).prs2       := MuxCase(io.ren_uops(w).lrs2, rs2_cases)
    if (rtype == RT_FLT.litValue)
      io.values(w).prs3     := MuxCase(io.ren_uops(w).lrs3, rs3_cases)
    io.values(w).stale_pdst := MuxCase(map_table_io(io.ren_uops(w).ldst).element, stale_cases)

    if (rtype == RT_FIX.litValue) {
      assert (!(io.ren_uops(w).lrs1 === 0.U && io.ren_uops(w).lrs1_rtype === RT_FIX && io.values(w).prs1 =/= 0.U),
        "lrs1==0 but maptable(" + w + ") returning non-zero.")
      assert (!(io.ren_uops(w).lrs2 === 0.U && io.ren_uops(w).lrs2_rtype === RT_FIX && io.values(w).prs2 =/= 0.U),
        "lrs2==0 but maptable(" + w + ") returning non-zero.")
      assert (!(io.ren_uops(w).lrs1 === 0.U && io.ren_uops(w).lrs1_rtype === RT_FIX && map_table_prs1(w) =/= 0.U),
        "lrs1==0 but maptable(" + w + ") returning non-zero.")
      assert (!(io.ren_uops(w).lrs2 === 0.U && io.ren_uops(w).lrs2_rtype === RT_FIX && map_table_prs2(w) =/= 0.U),
        "lrs2==0 but maptable(" + w + ") returning non-zero.")
    }
    // scalastyle:on
  }
}
