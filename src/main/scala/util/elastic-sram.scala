package boom.util

import Chisel._


class ESramWritePort[T <: Data](idx_sz: Int, gen: T) extends Bundle
{
   val idx = UInt(width = idx_sz)
   val data = gen

   override def cloneType = new ESramWritePort(idx_sz, gen).asInstanceOf[this.type]
}

/** Implements a decoupled SRAM which can be back-pressured on the read response side,
  * and thus will in turn back-pressure the read request side.
  *
  * @param num_entries the number of logical entries in our memory
  * @param gen the type of Data to store in our memory
  * @param dualported use 1r1w SeqMem or use banking with two 1rw SeqMems
  *
  * Assumptions:
  *   - If banked for 1rw, allowed to drop write updates
  *  */
class ElasticSeqMem[T <: Data](
   num_entries: Int,
   gen: T,
   dualported: Boolean = true)
   extends Module
{
   private val idx_sz = log2Ceil(num_entries)

   val io = IO(new Bundle {
      // read request on cycle S0 (pass in read address)
      val rreq = Decoupled(UInt(width=idx_sz)).flip
      // read response on cycle S1 (receive read output)
      val rresp = Decoupled(gen)

      val write = Valid(new ESramWritePort(idx_sz, gen)).flip

      // clear out queued inflight requests, but allow current response to be valid.
      val flush = Bool(INPUT)
   })

   private val ram = SeqMem(num_entries, gen)

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
   val s0_ridx = Wire(UInt(width=idx_sz))
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

