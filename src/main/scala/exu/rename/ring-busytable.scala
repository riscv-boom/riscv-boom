//******************************************************************************
// Copyright (c) 2015 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Ring BusyTable
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._
import boom.common._
import boom.util._
import freechips.rocketchip.config.Parameters

class RingBusyTable(
  val plWidth: Int,
  val numPregs: Int,
  val numWbPorts: Int)
  (implicit p: Parameters) extends BoomModule
{
  def DecodePreg(preg: UInt) = {
    val tmp = UIntToOH(preg)
    var ret = 0.U(1.W)
    val k = 1 << log2Ceil(numPregs/coreWidth)
    val n = numPregs / coreWidth

    for (w <- 0 until coreWidth) {
      ret = Cat(tmp(k*w+n-1, k*w), ret)
    }

    ret(numPregs,1)
  }

  val io = IO(new BoomBundle()(p) {
    val ren_uops = Input(Vec(plWidth, new MicroOp))
    val busy_resps = Output(Vec(plWidth, new BusyResp))
    val rebusy_reqs = Input(Vec(plWidth, Bool()))

    val wb_pdsts = Input(Vec(numWbPorts, UInt(ipregSz.W)))
    val wb_valids = Input(Vec(numWbPorts, Bool()))

    val debug = new Bundle { val busytable = Output(Bits(numPregs.W)) }
  })

  val busy_table = RegInit(0.U(numPregs.W))

  busy_table := ( busy_table
                & ~(io.wb_pdsts zip io.wb_valids) .map {case (pdst, valid) =>
                     DecodePreg(pdst) & Fill(numPregs, valid)}.reduce(_|_)
                |  (io.ren_uops zip io.rebusy_reqs) .map {case (uop, req)  =>
                     DecodePreg(uop.pdst) & Fill(numPregs, req)}.reduce(_|_)
                )

  val load_table = RegInit(0.U(numPregs.W))

  load_table:= ( load_table
                & ~(io.wb_pdsts zip io.wb_valids) .map {case (pdst, valid) =>
                     DecodePreg(pdst) & Fill(numPregs, valid)}.reduce(_|_)
                |  (io.ren_uops zip io.rebusy_reqs) .map {case (uop, req)  =>
                     DecodePreg(uop.pdst) & Fill(numPregs, req && uop.uses_ldq)}.reduce(_|_)
                )


  // Read the busy table.
  for (i <- 0 until plWidth) {
    io.busy_resps(i).prs1_busy := (busy_table & DecodePreg(io.ren_uops(i).prs1)).orR
    io.busy_resps(i).prs2_busy := (busy_table & DecodePreg(io.ren_uops(i).prs2)).orR

    io.busy_resps(i).prs1_load := (load_table & DecodePreg(io.ren_uops(i).prs1)).orR
    io.busy_resps(i).prs2_load := (load_table & DecodePreg(io.ren_uops(i).prs2)).orR
  }

  io.debug.busytable := busy_table
}
