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
   val uops = Vec(decodeWidth, new MicroOp())
}

// num_entries: effectively the number of full-sized fetch packets we can hold.
class FetchBuffer(num_entries: Int)(implicit p: Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
{
   val io = IO(new BoomBundle()(p)
   {
      val enq = Flipped(Decoupled(new FetchBundle()))
      val deq = new DecoupledIO(new FetchBufferResp())

      val clear = Input(Bool())
      // The mask comes too late to use, so we might need to mask off
      // instructions that were enqueued on the previous cycle.
//      val s1_mask = UInt(width=fetchWidth.W) // TODO XXX
   })


   // TODO blackbox with a bitvector for read-addrs, write-addrs.
   private val num_elements = num_entries*fetchWidth
   private val ram = Mem(num_elements, new MicroOp())
   private val write_ptr = RegInit(0.asUInt(width=log2Ceil(num_elements).W))
   private val read_ptr = RegInit(0.asUInt(width=log2Ceil(num_elements).W))
   private val count = RegInit(0.asUInt(width=log2Ceil(num_elements).W))

   //-------------------------------------------------------------
   // **** Enqueue Uops ****
   //-------------------------------------------------------------

   // shift down based on Priority(valids) TODO

   io.enq.ready := count < (num_elements-fetchWidth).U

   val uops = Wire(Vec(fetchWidth, new MicroOp()))
   for (i <- 0 until fetchWidth)
   {
      require (coreInstBytes==4)
      uops(i)                := DontCare
      uops(i).valid          := io.enq.valid && io.enq.bits.mask(i)
      uops(i).pc             := (io.enq.bits.pc.asSInt & (-(fetchWidth*coreInstBytes)).S).asUInt + (i << 2).U
      uops(i).fetch_pc_lob   := io.enq.bits.pc
      uops(i).ftq_idx        := io.enq.bits.ftq_idx
      uops(i).pc_lob         := ~(~io.enq.bits.pc | (fetchWidth*coreInstBytes-1).U) + (i << 2).U
      uops(i).inst           := io.enq.bits.insts(i)
      uops(i).xcpt_pf_if     := io.enq.bits.xcpt_pf_if
      uops(i).xcpt_ae_if     := io.enq.bits.xcpt_ae_if
      uops(i).replay_if      := io.enq.bits.replay_if
      uops(i).xcpt_ma_if     := io.enq.bits.xcpt_ma_if_oh(i)
      uops(i).br_prediction  := io.enq.bits.bpu_info(i)
      uops(i).debug_events   := io.enq.bits.debug_events(i)
   }

   var woffset = WireInit(0.asUInt(width=(log2Ceil(fetchWidth)+1).W))
   for (i <- 0 until fetchWidth)
   {
      when (io.enq.fire() && io.enq.bits.mask(i))
      {
         ram(write_ptr+woffset) := uops(i)
      }
      woffset = woffset + Mux(io.enq.fire() && io.enq.bits.mask(i), 1.U, 0.U)
   }

   val enq_count = Mux(io.enq.fire(), PopCount(io.enq.bits.mask), 0.U)
   val deq_count =
      Mux(io.deq.ready,
         Mux(count < decodeWidth.U, count, decodeWidth.U),
         0.U)
   count := count + enq_count - deq_count

   // TODO turn into bit-vector
   write_ptr := write_ptr + woffset
   read_ptr := read_ptr + deq_count


   //-------------------------------------------------------------
   // **** Dequeue Uops ****
   //-------------------------------------------------------------

   io.deq.valid := count > 0.U
   for (w <- 0 until decodeWidth)
   {
      io.deq.bits.uops(w) := ram(read_ptr + w.U)
      io.deq.bits.uops(w).valid := count > w.U
   }


   //-------------------------------------------------------------
   // **** Clear ****
   //-------------------------------------------------------------

   when (io.clear)
   {
      count := 0.U
      write_ptr := 0.U
      read_ptr := 0.U
   }


   //-------------------------------------------------------------
   // **** Printfs ****
   //-------------------------------------------------------------

   if (DEBUG_PRINTF)
   {
      // TODO a problem if we don't check the f3_valid?
      printf(" Fetch3 : (%d mask: %x) pc=0x%x enq_count (%d) %d\n",
         io.enq.valid,
         io.enq.bits.mask,
         io.enq.bits.pc,
         enq_count,
         io.clear
         )

      printf(" FB RAM :     count (%d) WA: %d, RA: %d ",
         count,
         write_ptr,
         read_ptr
         )

      printf("\n Fetch4 : deq_count (%d)\n",
         deq_count
         )
   }

   //-------------------------------------------------------------
   // **** Asserts ****
   //-------------------------------------------------------------

   assert (count >= deq_count, "[fetchbuffer] Trying to dequeue more uops than are available.")
   assert (woffset === enq_count, "[fetchbuffer] woffset =/= enqcount")

   override val compileOptions = chisel3.core.ExplicitCompileOptions.NotStrict.copy(explicitInvalidate = true)
}

