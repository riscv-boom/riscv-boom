//******************************************************************************
// Copyright (c) 2015 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Column Arbiter
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util._

class ColumnArbiter(implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle {
    val uops = Input(Vec(coreWidth, Valid(new MicroOp)))
    val gnts = Output(Vec(coreWidth, UInt(coreWidth.W)))
  })

  def PickColumn(
    col1: UInt,
    b1  : Bool,
    col2: UInt,
    b2  : Bool,
    rnd : UInt) =
  {
    RotateLeft(Mux(b1, col1,
               Mux(b2, col2, rnd)))
  }

  val rnd = RegInit(1.U(coreWidth.W))
  rnd := RotateLeft(rnd)

  var rnd_col = rnd
  var prev_pdst_cols = Seq.empty[UInt]

  for (w <- 0 until coreWidth) {
    val uop = io.uops(w)

    var prs1_col = uop.bits.prs1_col
    var prs2_col = uop.bits.prs2_col

    var prs1_busy = uop.bits.prs1_busy
    var prs2_busy = uop.bits.prs2_busy

    for ((bp_uop, bp_col) <- io.uops zip prev_pdst_cols) {
      val prs1_do_bypass = uop.bits.lrs1 === bp_uop.bits.ldst && bp_uop.bits.writes_irf && bp_uop.valid
      val prs2_do_bypass = uop.bits.lrs2 === bp_uop.bits.ldst && bp_uop.bits.writes_irf && bp_uop.valid

      prs1_col = Mux(prs1_do_bypass, bp_col, prs1_col)
      prs2_col = Mux(prs2_do_bypass, bp_col, prs2_col)

      prs1_busy = prs1_busy || prs1_do_bypass
      prs2_busy = prs2_busy || prs2_do_bypass
    }

    val pdst_col = PickColumn(prs1_col,
                             prs1_busy,
                             prs2_col,
                             prs2_busy,
                             rnd_col)
    prev_pdst_cols ++= Seq(pdst_col)
    io.gnts(w) := pdst_col & Fill(coreWidth, uop.valid)

    rnd_col = RotateLeft(rnd_col)
  }
}
