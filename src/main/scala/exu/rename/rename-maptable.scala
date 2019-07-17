//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
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
import boom.common._
import boom.util._
import freechips.rocketchip.config.Parameters

class MapReq(val lregSz: Int) extends Bundle
{
  val lrs1 = UInt(lregSz.W)
  val lrs2 = UInt(lregSz.W)
  val lrs3 = UInt(lregSz.W)
  val ldst = UInt(lregSz.W)
}

class MapResp(val pregSz: Int) extends Bundle
{
  val prs1 = UInt(pregSz.W)
  val prs2 = UInt(pregSz.W)
  val prs3 = UInt(pregSz.W)
  val stale_pdst = UInt(pregSz.W)
}

class RemapReq(val lregSz: Int, val pregSz: Int) extends Bundle
{
  val ldst = UInt(lregSz.W)
  val pdst = UInt(pregSz.W)
  val valid = Bool()
}

class RenameMapTable(
  val plWidth: Int,
  val numLregs: Int,
  val numPregs: Int,
  val float: Boolean)
  (implicit p: Parameters) extends BoomModule
{
  val pregSz = log2Ceil(numPregs)

  val io = IO(new BoomBundle()(p) {
    // Logical sources -> physical sources.
    val map_reqs    = Input(Vec(plWidth, new MapReq(lregSz)))
    val map_resps   = Output(Vec(plWidth, new MapResp(pregSz)))

    // Remapping an ldst to a newly allocated pdst?
    val remap_reqs  = Input(Vec(plWidth, new RemapReq(lregSz, pregSz)))

    // Dispatching branches: need to take snapshots of table state.
    val ren_br_tags = Input(Vec(plWidth, Valid(UInt(brTagSz.W))))

    // Signals for restoring state following misspeculation.
    val brinfo      = Input(new BrResolutionInfo)
    val rollback    = Input(Bool())
  })

  // The map table register array and its branch snapshots.
  val map_table = RegInit(VecInit(Seq.fill(numLregs){0.U(pregSz.W)}))
  val br_snapshots = Reg(Vec(maxBrCount, Vec(numLregs, UInt(pregSz.W))))

  // The intermediate states of the map table following modification by each pipeline slot.
  val remap_table = Wire(Vec(plWidth+1, Vec(numLregs, UInt(pregSz.W))))

  // Uops requesting changes to the map table.
  val remap_pdsts = io.remap_reqs map (_.pdst)
  val remap_ldsts_oh = io.remap_reqs map (req => UIntToOH(req.ldst) & Fill(numLregs, req.valid.asUInt))

  // Figure out the new mappings seen by each pipeline slot.
  for (i <- 0 until numLregs) {
    if (i == 0 && !float) {
      for (j <- 0 until plWidth+1) {
        remap_table(j)(i) := 0.U
      }
    } else {
      val remapped_row = (remap_ldsts_oh.map(ldst => ldst(i)) zip remap_pdsts)
        .scanLeft(map_table(i)) {case (pdst, (ldst, new_pdst)) => Mux(ldst, new_pdst, pdst)}

      for (j <- 0 until plWidth+1) {
        remap_table(j)(i) := remapped_row(j)
      }
    }
  }

  // Create snapshots of new mappings.
  for (i <- 0 until plWidth) {
    when (io.ren_br_tags(i).valid) {
      br_snapshots(io.ren_br_tags(i).bits) := remap_table(i+1)
    }
  }

  when (io.brinfo.mispredict) {
    // Restore the map table to a branch snapshot.
    map_table := br_snapshots(io.brinfo.tag)
  } .otherwise {
    // Update mappings.
    map_table := remap_table(plWidth)
  }

  // Read out mappings.
  for (i <- 0 until plWidth) {
    val remapped_col = remap_table(i)
    io.map_resps(i).prs1 := remapped_col(io.map_reqs(i).lrs1)
    io.map_resps(i).prs2 := remapped_col(io.map_reqs(i).lrs2)
    if (float) io.map_resps(i).prs3 := remapped_col(io.map_reqs(i).lrs3) else io.map_resps(i).prs3 := 0.U(pregSz.W)
    io.map_resps(i).stale_pdst := remapped_col(io.map_reqs(i).ldst)
  }

  // Don't flag the creation of duplicate 'p0' mappings during rollback.
  // These cases may occur soon after reset, as all maptable entries are initialized to 'p0'.
  io.remap_reqs map (req => (req.pdst, req.valid)) foreach {case (p,r) =>
    assert (!r || !map_table.contains(p) || p === 0.U && io.rollback, "[maptable] Trying to write a duplicate mapping.")}
}
