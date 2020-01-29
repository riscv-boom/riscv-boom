//******************************************************************************
// Copyright (c) 2020 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Issue Arbiters for Ring Microarchitecture
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

class RegisterReadArbiter extends BoomModule
{
  val io = IO(new BoomBundle {
    val reqs = Input(Vec(coreWidth, Bool()))
    val uops = Input(Vec(coreWidth, new MicroOp))

    val gnts = Output(Vec(coreWidth, Bool()))
  })

  val prs1_bank_reqs = Transpose((0 until w).map(w => io.uops(w).prs1_col & Fill(coreWidth, io.reqs(w) && io.uops(w).prs1_do_read)))
  val prs2_bank_reqs = Transpose((0 until w).map(w => io.uops(w).prs2_col & Fill(coreWidth, io.reqs(w) && io.uops(w).prs2_do_read)))

  val prs1_bank_gnts = Transpose(prs1_bank_reqs.map(r => PriorityEncoderOH(r)))
  val prs2_bank_gnts = Transpose(prs2_bank_reqs.map(r => PriorityEncoderOH(r)))

  for (w <- 0 until coreWidth) {
    io.gnts(w) := (prs1_bank_gnts(w).orR || !io.uops(w).prs1_do_read && io.reqs(w)) &&
                  (prs2_bank_gnts(w).orR || !io.uops(w).prs2_do_read && io.reqs(w))
  }
}

class ExecutionArbiter extends BoomModule
{
  val io = IO(new BoomBundle {
    val reqs = Input(Vec(coreWidth, Bool()))
    val uops = Input(Vec(coreWidth, new MicroOp))

    val gnts = Output(Vec(coreWidth, Bool()))
  })


}

class WritebackArbiter extends BoomModule
{
  val io = IO(new BoomBundle {
    val reqs = Input(Vec(coreWidth, Bool()))
    val uops = Input(Vec(coreWidth, new MicroOp))

    val gnts = Output(Vec(coreWidth, Bool()))

    val fire = Input(Vec(coreWidth, Bool()))
  })
}

