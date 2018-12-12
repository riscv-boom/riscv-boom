//******************************************************************************
// Copyright (c) 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom.util

import chisel3._
import chisel3.util._

/** Implements the same interface as chisel3.util.Queue.
  * Effectively a two-entry Queue.
  *  */
class ElasticReg[T <: Data](gen: T) extends Module {
   val entries = 2
   val io = IO(new QueueIO(gen, entries) {})

   private val valid = RegInit(VecInit(Seq.fill(entries) { false.B }))
   private val elts = Reg(Vec(entries, gen))

   for (i <- 0 until entries) {
      def paddedValid(i: Int) = if (i == -1) true.B else if (i == entries) false.B else valid(i)

      val wdata = if (i == entries-1) io.enq.bits else Mux(valid(i+1), elts(i+1), io.enq.bits)
      val wen =
         Mux(io.deq.ready,
            paddedValid(i+1) || io.enq.fire() && ((i == 0).B || valid(i)),
            io.enq.fire() && paddedValid(i-1) && !valid(i))
      when (wen) { elts(i) := wdata }

      valid(i) :=
         Mux(io.deq.ready,
            paddedValid(i+1) || io.enq.fire() && ((i == 0).B || valid(i)),
            io.enq.fire() && paddedValid(i-1) || valid(i))
   }

   io.enq.ready := !valid(entries-1)
   io.deq.valid := valid(0)
   io.deq.bits := elts.head

   io.count := PopCount(valid.asUInt)
}

object ElasticReg
{
   def apply[T <: Data](enq: DecoupledIO[T]): DecoupledIO[T] = {
      val q = Module(new ElasticReg(enq.bits.cloneType))
      q.io.enq <> enq
      q.io.deq
   }
}
