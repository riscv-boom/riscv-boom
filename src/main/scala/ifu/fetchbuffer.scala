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

      // Was the pipeline redirected? Clear/reset the fetchbuffer.
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

   // Step 1: convert FetchPacket into a vector of MicroOps.
   // Step 2: Compact/shift all MicroOps down towards index=0 (compress out any invalid MicroOps).
   // Step 3: Write CompactedMicroOps into the RAM.

   io.enq.ready := count < (num_elements-fetchWidth).U

   // Input microops.
   val in_uops = Wire(Vec(fetchWidth, new MicroOp()))

   // Compacted/shifted microops (and the shifted valid mask).
   val compact_mask = Wire(Vec(fetchWidth, Bool()))
   val compact_uops = Wire(Vec(fetchWidth, new MicroOp()))

   // Step 1. Convert input FetchPacket into an array of MicroOps.
   for (i <- 0 until fetchWidth)
   {
      require (coreInstBytes==4)
      in_uops(i)                := DontCare
      in_uops(i).valid          := io.enq.valid && io.enq.bits.mask(i)
      in_uops(i).pc             := (io.enq.bits.pc.asSInt & (-(fetchWidth*coreInstBytes)).S).asUInt + (i << 2).U
      in_uops(i).fetch_pc_lob   := io.enq.bits.pc
      in_uops(i).ftq_idx        := io.enq.bits.ftq_idx
      in_uops(i).pc_lob         := ~(~io.enq.bits.pc | (fetchWidth*coreInstBytes-1).U) + (i << 2).U
      in_uops(i).inst           := io.enq.bits.insts(i)
      in_uops(i).xcpt_pf_if     := io.enq.bits.xcpt_pf_if
      in_uops(i).xcpt_ae_if     := io.enq.bits.xcpt_ae_if
      in_uops(i).replay_if      := io.enq.bits.replay_if
      in_uops(i).xcpt_ma_if     := io.enq.bits.xcpt_ma_if_oh(i)
      in_uops(i).br_prediction  := io.enq.bits.bpu_info(i)
      in_uops(i).debug_events   := io.enq.bits.debug_events(i)
   }

   // Step 2. Shift valids towards 0.
   val first_index = PriorityEncoder(io.enq.bits.mask)
   for (i <- 0 until fetchWidth)
   {
      val selects_oh = Wire(UInt(width=fetchWidth.W))
      selects_oh   := UIntToOH(i.U + first_index)

      val invalid = first_index >= (fetchWidth - i).U // out-of-bounds
      compact_uops(i) := Mux1H(selects_oh, in_uops)
      compact_mask(i) := Mux1H(selects_oh, io.enq.bits.mask) && !invalid

      //printf(" shift [" + i + "] enq: %x compact: %d, selects_oh: %x, oob: %d\n",
      //   io.enq.bits.mask, compact_mask(i), selects_oh, invalid)
   }



   // all enqueuing uops have been compacted
   val enq_count = Mux(io.enq.fire(), PopCount(io.enq.bits.mask), 0.U)
   for (i <- 0 until fetchWidth)
   {
      when (io.enq.fire() && i.U < enq_count)
      {
         ram(write_ptr + i.U) := compact_uops(i)
         assert (compact_mask(i))
      }
      .otherwise
      {
         assert (!io.enq.fire() || !compact_mask(i))
      }
   }

   val deq_count =
      Mux(io.deq.ready,
         Mux(count < decodeWidth.U, count, decodeWidth.U),
         0.U)
   count := count + enq_count - deq_count

   // TODO turn into bit-vector
   write_ptr := write_ptr + enq_count
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
      printf(" Fetch3 : (%d mask: %x [%d] smask: %x) pc=0x%x enq_count (%d) %d\n",
         io.enq.valid,
         io.enq.bits.mask,
         first_index,
         compact_mask.asUInt,
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

   override val compileOptions = chisel3.core.ExplicitCompileOptions.NotStrict.copy(explicitInvalidate = true)
}

