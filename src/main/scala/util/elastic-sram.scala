//******************************************************************************
// Copyright (c) 2018 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

package boom.util

import chisel3._
import chisel3.util._

/**
 * Bundle serving as the write port to the ESram.
 *
 * @param idx_sz the size of the idx to index into the memory
 * @param gen the type of Data object to store in the memory
 */
class ESramWritePort[T <: Data](val idx_sz: Int, private val gen: T) extends Bundle
{
   val idx = UInt(idx_sz.W)
   val data = gen
}

/**
 * Implements a decoupled SRAM which can be back-pressured on the read response side,
 * and thus will in turn back-pressure the read request side.
 *
 * Assumptions:
 *   - If banked for 1rw, allowed to drop write updates
 *
 * @param num_entries the number of logical entries in the memory
 * @param gen the type of Data to store in the memory
 * @param dualported use 1r1w SeqMem or use banking with two 1rw SeqMems
 */
class ElasticSeqMem[T <: Data](
   num_entries: Int,
   gen: T,
   dualported: Boolean = true)
   extends Module
{
   private val idx_sz = log2Ceil(num_entries)

   val io = IO(new Bundle
   {
      // read request on cycle S0 (pass in read address)
      val rreq = Flipped(Decoupled(UInt(idx_sz.W)))
      // read response on cycle S1 (receive read output)
      val rresp = Decoupled(gen)

      val write = Flipped(Valid(new ESramWritePort(idx_sz, gen)))

      // clear out queued inflight requests, but allow current response to be valid.
      val flush = Input(Bool())
   })

   private val ram = SyncReadMem(num_entries, gen)

   when (io.write.valid)
   {
      ram.write(io.write.bits.idx, io.write.bits.data)
   }

   // Shadow flop.
   // we provide a shadow flop to decouple the not-ready read response from the
   // read request.

   // Replay s0 onto s1 if s1_resp is not ready,
   // as we need to maintain the same read index as the last cycle.
   val s1_replay = io.rresp.ready

   val s0_valid = Wire(Bool())
   val s0_ridx = Wire(UInt(idx_sz.W))
   val last_val = RegNext(s0_valid)
   val last_idx = RegNext(s0_ridx)
   s0_valid := Mux(s1_replay, last_val, io.rreq.valid)
   s0_ridx := Mux(s1_replay, last_idx, io.rreq.bits)

   val s1_valid = RegNext(s0_valid)
   io.rresp.valid := s1_valid
   io.rresp.bits := ram.read(s0_ridx, s0_valid)

   when (io.flush)
   {
      // TODO XXX
      last_val := false.B
   }
}
