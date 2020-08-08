//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
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
    val remap_reqs  = Input(Vec(plWidth, Valid(new RemapReq(lregSz, pregSz))))
    val commit_reqs = Input(Vec(plWidth, Valid(new RemapReq(lregSz, pregSz))))

    // Dispatching branches: need to take snapshots of table state.
    val ren_br_tags = Input(Vec(plWidth, Valid(UInt(brTagSz.W))))

    // Signals for restoring state following misspeculation.
    val brupdate    = Input(new BrUpdateInfo)
    val flashback   = Input(Bool())
  })

  // The map table register array and its branch snapshots.
  val map_table        = RegInit(VecInit(Seq.fill(numLregs){0.U(pregSz.W)}))
  val commit_map_table = RegInit(VecInit(Seq.fill(numLregs){0.U(pregSz.W)}))
  val br_snapshots     = Reg(Vec(maxBrCount, Vec(numLregs, UInt(pregSz.W))))

  // Delay the write of new allocations by a cycle
  val r_remap_reqs = RegNext(io.remap_reqs)

  // The intermediate states of the map table following modification by each pipeline slot.
  val remap_table  = Wire(Vec(plWidth+1, Vec(numLregs, UInt(pregSz.W))))

  // Write ports into the map tables
  val remap_pdsts  = r_remap_reqs map (_.bits.pdst)
  val remap_ldsts  = r_remap_reqs map (r => Mux(r.valid, UIntToOH(r.bits.ldst), 0.U))

  val commit_pdsts = io.commit_reqs map (_.bits.pdst)
  val commit_ldsts = io.commit_reqs map (r => Mux(r.valid, UIntToOH(r.bits.ldst), 0.U))

  // Figure out the new mappings seen by each pipeline slot.
  for (i <- 0 until numLregs) {
    if (i == 0 && !float) {
      for (j <- 0 until plWidth+1) {
        remap_table(j)(i) := 0.U
      }
      commit_map_table(i) := 0.U
    } else {
      val remapped_row = (remap_pdsts zip remap_ldsts.map(_(i)))
        .scanLeft(map_table(i)) { case (l,(r,v)) => Mux(v,r,l) }
      for (j <- 0 until plWidth+1) {
        remap_table(j)(i) := remapped_row(j)
      }
      commit_map_table(i) := (commit_pdsts zip commit_ldsts.map(_(i)))
        .foldLeft(commit_map_table(i)) { case (l,(r,v)) => Mux(v,r,l) }
    }
  }

  // Create snapshots of new mappings.
  for (i <- 0 until plWidth) {
    when (RegNext(io.ren_br_tags(i).valid)) {
      br_snapshots(RegNext(io.ren_br_tags(i).bits)) := remap_table(i+1)
    }
  }

  when (io.flashback) {
    map_table := commit_map_table
  } .elsewhen (io.brupdate.b2.mispredict) {
    map_table := br_snapshots(io.brupdate.b2.uop.br_tag)
  } .otherwise {
    map_table := remap_table(plWidth)
  }

  // Read out mappings.
  for (i <- 0 until plWidth) {
    io.map_resps(i).prs1       := r_remap_reqs.foldLeft(map_table(io.map_reqs(i).lrs1)) ((p,r) =>
                                    Mux(r.valid && r.bits.ldst === io.map_reqs(i).lrs1, r.bits.pdst, p))
    io.map_resps(i).prs2       := r_remap_reqs.foldLeft(map_table(io.map_reqs(i).lrs2)) ((p,r) =>
                                    Mux(r.valid && r.bits.ldst === io.map_reqs(i).lrs2, r.bits.pdst, p))
    io.map_resps(i).prs3       := r_remap_reqs.foldLeft(map_table(io.map_reqs(i).lrs3)) ((p,r) =>
                                    Mux(r.valid && r.bits.ldst === io.map_reqs(i).lrs3, r.bits.pdst, p))
    io.map_resps(i).stale_pdst := r_remap_reqs.foldLeft(map_table(io.map_reqs(i).ldst)) ((p,r) =>
                                    Mux(r.valid && r.bits.ldst === io.map_reqs(i).ldst, r.bits.pdst, p))

    if (!float) io.map_resps(i).prs3 := DontCare
  }

  r_remap_reqs map (req => (req.bits.pdst, req.valid)) foreach { case (p,r) =>
    assert (!r || !map_table.contains(p), "[maptable] Trying to write a duplicate mapping.") }
}
