//******************************************************************************
// Copyright (c) 2020 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Issue Arbiters for Ring Microarchitecture
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util._

class RegisterReadArbiter(implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle {
    val reqs = Input(Vec(coreWidth, Bool()))
    val uops = Input(Vec(coreWidth, new MicroOp))

    val gnts = Output(Vec(coreWidth, Bool()))
  })

  val prs1_bank_reqs = Transpose(VecInit((0 until coreWidth).map(w => io.uops(w).prs1_col & Fill(coreWidth, io.reqs(w) && io.uops(w).prs1_reads_irf))))
  val prs2_bank_reqs = Transpose(VecInit((0 until coreWidth).map(w => io.uops(w).prs2_col & Fill(coreWidth, io.reqs(w) && io.uops(w).prs2_reads_irf))))

  val prs1_bank_gnts = Transpose(VecInit(prs1_bank_reqs.map(r => PriorityEncoderOH(r))))
  val prs2_bank_gnts = Transpose(VecInit(prs2_bank_reqs.map(r => PriorityEncoderOH(r))))

  for (w <- 0 until coreWidth) {
    io.gnts(w) := (prs1_bank_gnts(w).orR || !io.uops(w).prs1_reads_irf && io.reqs(w)) &&
                  (prs2_bank_gnts(w).orR || !io.uops(w).prs2_reads_irf && io.reqs(w))
  }
}

class ExecutionArbiter(implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle {
    val reqs = Input(Vec(coreWidth, Bool()))
    val uops = Input(Vec(coreWidth, new MicroOp))

    val gnts = Output(Vec(coreWidth, Bool()))
  })

  val shared_exe_reqs = Transpose(VecInit((0 until coreWidth).map(w => io.uops(w).shared_eu_code)))
  val shared_exe_gnts = Transpose(VecInit(shared_exe_reqs.map(r => PriorityEncoderOH(r))))

  for (w <- 0 until coreWidth) {
    io.gnts(w) := shared_exe_gnts(w).orR || !io.uops(w).shared_eu_code.orR && io.reqs(w)
  }
}

class WritebackArbiter(implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle {
    val reqs = Input(Vec(coreWidth, Bool()))
    val uops = Input(Vec(coreWidth, new MicroOp))

    val gnts = Output(Vec(coreWidth, Bool()))

    val fire = Input(Vec(coreWidth, Bool()))
  })

  val maxLatency = 3
  val wb_table = Reg(Vec(coreWidth, UInt(maxLatency.W)))

  for (w <- 0 until coreWidth) {
    val latency = io.uops(w).exe_wb_latency

    io.gnts(w) := !(wb_table(w) & latency).orR && io.reqs(w)
    wb_table(w) := Mux(io.fire(w), wb_table(w) | latency, wb_table(w)) >> 1
  }
}
