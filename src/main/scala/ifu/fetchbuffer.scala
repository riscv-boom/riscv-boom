//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Fetch Buffer
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Takes a FetchBundle and converts into a vector of MicroOps.

package boom

import chisel3._
import chisel3.util._
import chisel3.core.DontCare
import freechips.rocketchip.config.Parameters


class FetchBufferResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val uops = Vec(DECODE_WIDTH, new MicroOp())
}

class FetchBuffer(num_entries: Int)(implicit p: Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
{
   val io = IO(new BoomBundle()(p)
   {
      // use withReset to clear out the fetch buffer.
      val enq = Flipped(Decoupled(new FetchBundle()))
      val deq = new DecoupledIO(new FetchBufferResp())
   })

   val queue =
      Module(new Queue(
         gen=new FetchBundle,
         entries=num_entries,
         pipe=false,
         flow=false))

   queue.io.enq <> io.enq

   io.deq.valid <> queue.io.deq.valid
   io.deq.ready <> queue.io.deq.ready

   require (DECODE_WIDTH == FETCH_WIDTH)

   for (i <- 0 until DECODE_WIDTH)
   {
      // 1:1, so pass everything straight through!
      require (coreInstBytes==4)
      io.deq.bits.uops(i)                := DontCare
      io.deq.bits.uops(i).valid          := queue.io.deq.bits.mask(i)
      io.deq.bits.uops(i).pc             := (queue.io.deq.bits.pc.asSInt & SInt(-(FETCH_WIDTH*coreInstBytes))).asUInt + (i << 2).U
      io.deq.bits.uops(i).fetch_pc_lob   := queue.io.deq.bits.pc
      io.deq.bits.uops(i).ftq_idx        := queue.io.deq.bits.ftq_idx
      io.deq.bits.uops(i).pc_lob         := ~(~queue.io.deq.bits.pc | (fetchWidth*coreInstBytes-1).U) + (i << 2).U
      io.deq.bits.uops(i).inst           := queue.io.deq.bits.insts(i)
      io.deq.bits.uops(i).xcpt_pf_if     := queue.io.deq.bits.xcpt_pf_if
      io.deq.bits.uops(i).xcpt_ae_if     := queue.io.deq.bits.xcpt_ae_if
      io.deq.bits.uops(i).replay_if      := queue.io.deq.bits.replay_if
      io.deq.bits.uops(i).xcpt_ma_if     := queue.io.deq.bits.xcpt_ma_if_oh(i)
      io.deq.bits.uops(i).br_prediction  := queue.io.deq.bits.bpu_info(i)
      io.deq.bits.uops(i).debug_events   := queue.io.deq.bits.debug_events(i)
   }

   override val compileOptions = chisel3.core.ExplicitCompileOptions.NotStrict.copy(explicitInvalidate = true)
}

