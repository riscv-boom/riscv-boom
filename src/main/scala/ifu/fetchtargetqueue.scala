//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Fetch Target Queue (FTQ)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Each entry in the FTQ holds the fetch address and branch prediction snapshot state.

// TODO Dev steps:
//   !- get enq_ptr working
//   !- pass off ftq_idx to instructions
//   !- pass off ftq_idx to ROB/branches
//   !- get commit_ptr from ROB
//    - update pointer on exception/flush
//    - update pointer on mispredict
//   !- get dequeue ptr to move 1/cycle to match commit_ptr
//    - start using fetch-pcs to drive ROB redirections
//    - start using fetch-pcs to drive JALR, branch mispredictions, CSR I/Os
//    - remove ROB's PCFile
//    - get miss-info working
//    - store BIM info;
//    - get BIM updating properly.
//    - use BIM to drive BTB.
//    - store history in here. reset on misprediction.
//    - setup gshare predictor?



package boom

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

case class FtqParameters(
   nEntries: Int = 16
)

class FTQBundle(implicit p: Parameters) extends BoomBundle()(p)
{
   val fetch_pc = UInt(width = vaddrBitsExtended.W) // TODO compress out high-order bits
   val history = UInt(width = GLOBAL_HISTORY_LENGTH.W)

//   val bim_info =
//   val bpd_info =
}

// Track the oldest mispredicted cfi instruction for a given fetch entry.
class CfiMissInfo(fetch_width: Int) extends Bundle
{
   val valid = Bool() // Is there a branch or jr in this fetch that was mispredicted?
   val taken = Bool() // If a branch, was it taken?
   val cfi_idx = UInt(width=log2Up(fetch_width).W) // which instruction in fetch packet?

   override def cloneType: this.type = new CfiMissInfo(fetch_width).asInstanceOf[this.type]
}

class FtqFlushInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val ftq_idx = UInt(width=log2Up(ftqSz).W)
   val flush_typ = FlushTypes()
}

class FetchTargetQueue(num_entries: Int)(implicit p: Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
{
   private val idx_sz = log2Up(num_entries)

   val io = IO(new BoomBundle()(p)
   {
      // Enqueue one entry for every fetch cycle.
      val enq = Flipped(Decoupled(new FTQBundle()))
      // Pass to FetchBuffer (newly fetched instructions).
      val enq_idx = Output(UInt(width=idx_sz.W))
      // ROB tells us the youngest committed ftq_idx to remove from FTQ.
      val deq = Flipped(Valid(UInt(width=idx_sz.W)))

      // Redirect the frontend as we set fit.
      val pc_request = Valid(new PCReq()) //TODO XXX

      // on any sort misprediction or rob flush, reset the enq_ptr.
//      val flush = Flipped(Valid(UInt(width=idx_sz.W)))
      val flush = Flipped(Valid(new FtqFlushInfo()))

//      val br_unit = new BranchUnitResp().asInput
      val brinfo = new BrResolutionInfo().asInput

      val debug_rob_empty = Input(Bool())
   })

   val deq_ptr = Counter(num_entries)
   val enq_ptr = Counter(num_entries)
   val maybe_full = RegInit(false.B)
   val ptr_match = enq_ptr.value === deq_ptr.value
   val empty = ptr_match && !maybe_full
   val full = ptr_match && maybe_full

   // What is the commit point of the process? Dequeue entries until deq_ptr matches commit_ptr.
   val commit_ptr = RegInit(0.asUInt(log2Up(num_entries).W))

   val ram = Mem(num_entries, new FTQBundle())

   // TODO track oldest mispredicted entry in flip-flops
   val cfi_info = Reg(Vec(num_entries, new CfiMissInfo(fetchWidth)))


   private val do_enq = WireInit(io.enq.fire())
   private val do_deq = WireInit(deq_ptr.value =/= commit_ptr)

   private def nullCfiInfo(): CfiMissInfo =
   {
      val b = new CfiMissInfo(fetchWidth)
      b.valid := false.B
      b.taken := false.B
      b.cfi_idx := 0.U
      b
   }


   when (do_enq) {
     ram(enq_ptr.value) := io.enq.bits
//     cfi_info(enq_ptr.value) := nullCfiInfo()
     enq_ptr.inc()
   }
   when (do_deq) {
     deq_ptr.inc()
   }
   when (do_enq != do_deq) {
     maybe_full := do_enq
   }

   io.enq.ready := !full
   io.enq_idx := enq_ptr.value

   when (io.deq.valid)
   {
      commit_ptr := io.deq.bits
      assert (!empty, "[ftq] nothing to deqeue.")
   }

   when (io.flush.valid)
   {
      enq_ptr.value := WrapInc(io.flush.bits.ftq_idx, num_entries)
   }

   when (io.brinfo.valid && io.brinfo.mispredict)
   {
      enq_ptr.value := io.brinfo.ftq_idx
   }

//   io.deq.valid := !empty
//   io.deq.bits := ram(deq_ptr.value)

//   private val ptr_diff = enq_ptr.value - deq_ptr.value
//   if (isPow2(num_entries)) {
//     io.count := Cat(maybe_full && ptr_match, ptr_diff)
//   } else {
//     io.count := Mux(ptr_match,
//                     Mux(maybe_full,
//                       num_entries.asUInt, 0.U),
//                     Mux(deq_ptr.value > enq_ptr.value,
//                       num_entries.asUInt + ptr_diff, ptr_diff))
//   }


   // TODO check against empty rob that commit_ptr == enq_ptr.
//   assert(!(io.debug_rob_empty)

//   when (io.br_unit

   //-------------------------------------------------------------
   // **** Printfs ****
   //-------------------------------------------------------------

   if (DEBUG_PRINTF)
   {
      printf("FTQ: %c %c: %d; commit: %c:%d brinfo: %c:%d [%d %d %d] rob:%c\n",
         Mux(io.enq.valid, Str("V"), Str("-")),
         Mux(io.enq.ready, Str("R"), Str("-")),
         io.enq_idx,
         Mux(io.deq.valid, Str("C"), Str("-")),
         io.deq.bits,
         Mux(io.brinfo.valid && io.brinfo.mispredict, Str("M"), Str("-")),
         io.brinfo.ftq_idx,
         enq_ptr.value, commit_ptr, deq_ptr.value,
         Mux(io.debug_rob_empty, Str("E"), Str("-"))
      )
      val w = 4
      for (
         i <- 0 until (num_entries/w);
         j <- 0 until w
      ){
         val idx = i+j*(num_entries/w)
         printf(" [%d %c%c%c pc=0x%x]",
            idx.asUInt(width=5.W),
            Mux(enq_ptr.value === idx.U, Str("E"), Str(" ")),
            Mux(commit_ptr === idx.U, Str("C"), Str(" ")),
            Mux(deq_ptr.value === idx.U, Str("D"), Str(" ")),
            0.U
         )
         if (j == w-1) printf("\n")
      }
   }




}

