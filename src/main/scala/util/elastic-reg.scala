//******************************************************************************
// Copyright (c) 2018 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.util

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters}

import boom.common._
import boom.exu.{BrUpdateInfo}

/**
 * Implements the same interface as chisel3.util.Queue.
 * Effectively a two-entry Queue.
 *
 * @param gen the type of Data object to store in the register
 */
class ElasticReg[T <: Data](gen: T) extends Module
{
  val entries = 2
  val io = IO(new QueueIO(gen, entries) {})

  private val valid = RegInit(VecInit(Seq.fill(entries) { false.B }))
  private val elts = Reg(Vec(entries, gen))

  for (i <- 0 until entries) {
    def paddedValid(i: Int) = if (i == -1) true.B else if (i == entries) false.B else valid(i)

    val wdata = if (i == entries-1) io.enq.bits else Mux(valid(i+1), elts(i+1), io.enq.bits)
    val wen = Mux(io.deq.ready,
                  paddedValid(i+1) || io.enq.fire() && ((i == 0).B || valid(i)),
                  io.enq.fire() && paddedValid(i-1) && !valid(i))
    when (wen) { elts(i) := wdata }

    valid(i) := Mux(io.deq.ready,
                    paddedValid(i+1) || io.enq.fire() && ((i == 0).B || valid(i)),
                    io.enq.fire() && paddedValid(i-1) || valid(i))
  }

  io.enq.ready := !valid(entries-1)
  io.deq.valid := valid(0)
  io.deq.bits := elts.head

  io.count := PopCount(valid.asUInt)
}

/**
 * Companion object to ElasticReg which enqueues a data type
 * and returns a dequeued Data object.
 */
object ElasticReg
{
  def apply[T <: Data](enq: DecoupledIO[T]): DecoupledIO[T] = {
    val q = Module(new ElasticReg(enq.bits.cloneType))
    q.io.enq <> enq
    q.io.deq
  }
}

class MicroOpElasticReg(implicit p: Parameters) extends BoomModule
{
  val entries = 2
  val io = IO(new BoomBundle {
    val enq = Flipped(DecoupledIO(new MicroOp))
    val deq = DecoupledIO(new MicroOp)

    val brupdate = Input(new BrUpdateInfo)
    val kill     = Input(Bool())
  })

  private val valid = RegInit(VecInit(Seq.fill(entries) { false.B }))
  private val uops  = Reg(Vec(entries, new MicroOp))

  for (i <- 0 until entries) {
    def paddedValid(i: Int) = if (i == -1) true.B else if (i == entries) false.B else valid(i)

    val sel_uop  = if (i == entries-1) io.enq.bits else Mux(valid(i+1), uops(i+1), io.enq.bits)
    val next_uop = GetNewUopAndBrMask(sel_uop, io.brupdate)
    val wen = Mux(io.deq.ready,
                  paddedValid(i+1) || io.enq.fire() && ((i == 0).B || valid(i)),
                  io.enq.fire() && paddedValid(i-1) && !valid(i))
    when (wen) { uops(i) := next_uop }

    valid(i) := Mux(io.deq.ready,
                    paddedValid(i+1) || io.enq.fire() && ((i == 0).B || valid(i)),
                    io.enq.fire() && paddedValid(i-1) || valid(i)) && !io.kill && !IsKilledByBranch(io.brupdate, next_uop)
  }

  io.enq.ready := !valid(entries-1)
  io.deq.valid := valid(0)
  io.deq.bits  := uops.head
}

class MicroOpPipelineRegister(implicit p: Parameters) extends BoomModule
{
  val entries = 2
  val io = IO(new BoomBundle {
    val enq = new BoomBundle {
      val uops   = Input(Vec(coreWidth, new MicroOp))
      val valids = Input(Vec(coreWidth, Bool()))
      val ready  = Output(Bool())
    }
    val deq = new BoomBundle {
      val uops   = Output(Vec(coreWidth, new MicroOp))
      val valids = Output(Vec(coreWidth, Bool()))
      val stalls = Input (Vec(coreWidth, Bool()))
    }

    val wakeups  = Input(Vec(coreWidth*3, Valid(UInt(ipregSz.W))))

    val brupdate = Input(new BrUpdateInfo)
    val kill     = Input(Bool())
  })

  val valids = RegInit(VecInit(Seq.fill(entries) { VecInit(Seq.fill(coreWidth) { false.B }) }))
  val uops   = Reg(Vec(entries, Vec(coreWidth, new MicroOp)))

  def paddedValid (i: Int) = if (i == -1) true.B else if (i == entries) false.B else valids(i).reduce(_||_)

  def DoWakeup(uop: MicroOp, wakeups: Vec[Valid[UInt]]) = {
    val new_uop = WireInit(uop)

    val prs1_wakeup = wakeups.map(w => w.valid && w.bits === uop.prs1).reduce(_||_)
    val prs2_wakeup = wakeups.map(w => w.valid && w.bits === uop.prs2).reduce(_||_)

    new_uop.prs1_busy := uop.prs1_busy && !prs1_wakeup
    new_uop.prs2_busy := uop.prs2_busy && !prs2_wakeup

    new_uop
  }

  val enq_fire  = io.enq.ready && io.enq.valids.reduce(_||_)
  val deq_ready = !io.deq.stalls.last

  for (i <- 0 until entries) {
    val wen = Mux(deq_ready,
                  paddedValid(i+1) || enq_fire && ((i == 0).B || paddedValid(i)),
                  enq_fire && paddedValid(i-1) && !paddedValid(i))

    val sel_valids  = if (i == entries-1) Mux(wen, io.enq.valids,
                                                   Mux(deq_ready, VecInit((0.U(coreWidth.W)).asBools), valids(i)))
                      else if (i == 0)    Mux(wen, Mux(paddedValid(i+1), valids(i+1), io.enq.valids),
                                                   VecInit((valids(i).asUInt & io.deq.stalls.asUInt).asBools))
                      else                Mux(wen, Mux(paddedValid(i+1), valids(i+1), io.enq.valids),
                                                   Mux(deq_ready, VecInit((0.U(coreWidth.W)).asBools), valids(i)))
    val sel_uops    = if (i == entries-1) Mux(wen, io.enq.uops, uops(i))
                      else                Mux(wen, Mux(paddedValid(i+1), uops(i+1), io.enq.uops), uops(i))
    val next_uops   = sel_uops.map(u => GetNewUopAndBrMask(DoWakeup(u, io.wakeups), io.brupdate))
    var next_valids = Mux(io.kill, VecInit((0.U(coreWidth.W)).asBools), sel_valids)

    uops(i)   := next_uops
    valids(i) := next_valids
  }

  io.enq.ready  := !paddedValid(entries-1)
  io.deq.valids := valids(0)
  io.deq.uops   := uops(0).map(u => GetNewUopAndBrMask(u, io.brupdate))
}
