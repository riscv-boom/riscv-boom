package boom.exu

import chisel3._
import chisel3.util._
import boom.common._
import boom.util._
import freechips.rocketchip.config.Parameters

class MapResp(val pregSz: Int) extends Bundle
{
  val prs1 = UInt(pregSz.W)
  val prs2 = UInt(pregSz.W)
  val prs3 = UInt(pregSz.W)
  val stale_pdst = UInt(pregSz.W)
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
    val ren_uops = Input(Vec(plWidth, new MicroOp))
    val map_resps = Output(Vec(plWidth, new MapResp(pregSz)))

    // Remapping an ldst to a newly allocated pdst?
    val ren_remap_reqs = Input(Vec(plWidth, Bool()))

    // Dispatching branches: need to take snapshots of table state.
    val ren_br_tags = Input(Vec(plWidth, Valid(UInt(brTagSz.W))))

    // Signals for restoring state following misspeculation.
    val brinfo = Input(new BrResolutionInfo)
    val rollback = Input(Bool())
    val rbk_uops = Input(Vec(plWidth, new MicroOp))
    val rbk_valids = Input(Vec(plWidth, Bool()))
  })

  // The map table register array and its branch snapshots.
  val map_table = RegInit(VecInit(Seq.fill(numLregs){0.U(pregSz.W)}))
  val br_snapshots = Reg(Vec(maxBrCount, Vec(numLregs, UInt(pregSz.W))))

  // The intermediate states of the map table following modification by each pipeline slot.
  val remap_table = Wire(Vec(plWidth+1, Vec(numLregs, UInt(pregSz.W))))

  // Uops requesting changes to the map table.
  val remap_reqs = Mux(io.rollback, VecInit(io.rbk_valids.reverse), io.ren_remap_reqs)
  val remap_ldsts = io.ren_uops zip io.rbk_uops.reverse map {case (ren, rbk) => Mux(io.rollback, rbk.ldst, ren.ldst)}
  val remap_pdsts = io.ren_uops zip io.rbk_uops.reverse map {case (ren, rbk) => Mux(io.rollback, rbk.stale_pdst, ren.pdst)}
  val remap_ldst_reqs = (remap_ldsts zip remap_reqs) map {case (ldst, req) => UIntToOH(ldst) & Fill(numLregs, req.asUInt)}

  // Figure out the new mappings seen by each pipeline slot.
  for (i <- 0 until numLregs) {
    if (i == 0 && !float) {
      for (j <- 0 until plWidth+1) {
        remap_table(j)(i) := 0.U
      }
    } else {
      val remapped_row = (remap_ldst_reqs.map(ldst => ldst(i)) zip remap_pdsts)
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
    io.map_resps(i).prs1 := remapped_col(io.ren_uops(i).lrs1)
    io.map_resps(i).prs2 := remapped_col(io.ren_uops(i).lrs2)
    if (float) io.map_resps(i).prs3 := remapped_col(io.ren_uops(i).lrs3) else io.map_resps(i).prs3 := 0.U(pregSz.W)
    io.map_resps(i).stale_pdst := remapped_col(io.ren_uops(i).ldst)
  }
}
