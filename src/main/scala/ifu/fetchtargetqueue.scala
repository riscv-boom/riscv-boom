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
//   !- update pointer on exception/flush
//   !- update pointer on mispredict
//   !- get dequeue ptr to move 1/cycle to match commit_ptr
//    - start using fetch-pcs to drive ROB redirections
//   !- start using fetch-pcs to drive JALR, branch mispredictions
//    - start using fetch-pcs to drive CSR I/Os
//    - remove ROB's PCFile
//   !- get miss-info working
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
   nEntries: Int = 24
)

class FTQBundle(implicit p: Parameters) extends BoomBundle()(p)
{
   val fetch_pc = UInt(width = vaddrBitsExtended.W) // TODO compress out high-order bits
   val history = UInt(width = GLOBAL_HISTORY_LENGTH.W)

//   val bim_info =
//   val bpd_info =
}

// Track the oldest mispredicted cfi instruction for a given fetch entry.
class CfiMissInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val valid = Bool() // Is there a branch or jr in this fetch that was mispredicted?
   val taken = Bool() // If a branch, was it taken?
   val cfi_idx = UInt(width=log2Up(fetchWidth).W) // which instruction in fetch packet?
}

// provide a port for a FunctionalUnit to get the PC of an instruction.
// And for JALRs, the PC of the next instruction.
class GetPCFromFtqIO(implicit p: Parameters) extends BoomBundle()(p)
{
   val ftq_idx  = Input(UInt(log2Up(ftqSz).W))
   val fetch_pc = Output(UInt(vaddrBitsExtended.W))
   // the next_pc may not be valid (stalled or still being fetched)
   val next_val = Output(Bool())
   val next_pc  = Output(UInt(vaddrBitsExtended.W))
}

// The ROB needs to tell us if there's a pipeline flush (and what type)
// so we can drive the frontend with the correct redirected PC.
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

      // Give PC info to BranchUnit.
      val get_ftq_pc = new GetPCFromFtqIO()
      // Redirect the frontend as we set fit.
      val pc_request = Valid(new PCReq()) //TODO XXX

      // on any sort misprediction or rob flush, reset the enq_ptr.
      val flush = Flipped(Valid(new FtqFlushInfo()))

      // BranchResolutionUnit tells us the outcome of branches/jumps.
      val brinfo = new BrResolutionInfo().asInput

      val debug_rob_empty = Input(Bool()) // TODO can we build asserts off of this?
   })

   val deq_ptr = Counter(num_entries)
   val enq_ptr = Counter(num_entries)
   val maybe_full = RegInit(false.B)
   val ptr_match = enq_ptr.value === deq_ptr.value
   val empty = ptr_match && !maybe_full
   val full = ptr_match && maybe_full

   // What is the current commit point of the processor? Dequeue entries until deq_ptr matches commit_ptr.
   val commit_ptr = RegInit(0.asUInt(log2Up(num_entries).W))

   val ram = Mem(num_entries, new FTQBundle())
   val cfi_info = Reg(Vec(num_entries, new CfiMissInfo()))


   private val do_enq = WireInit(io.enq.fire())
   private val do_deq = WireInit(deq_ptr.value =/= commit_ptr)

   private def nullCfiInfo(): CfiMissInfo =
   {
      val b = Wire(new CfiMissInfo())
      b.valid := false.B
      b.taken := false.B
      b.cfi_idx := 0.U
      b
   }


   when (do_enq) {
     ram(enq_ptr.value) := io.enq.bits
     cfi_info(enq_ptr.value) := nullCfiInfo()
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
   }

   when (io.flush.valid)
   {
      enq_ptr.value := WrapInc(io.flush.bits.ftq_idx, num_entries)
   }

   when (io.brinfo.valid && io.brinfo.mispredict)
   {
      val new_ptr = WrapInc(io.brinfo.ftq_idx, num_entries)
      enq_ptr.value := new_ptr
      // If ptr is adjusted, we deleted entries and thus can't be full.
      maybe_full := (enq_ptr.value === new_ptr)

      cfi_info(io.brinfo.ftq_idx).valid := true.B
      cfi_info(io.brinfo.ftq_idx).taken := io.brinfo.taken
      cfi_info(io.brinfo.ftq_idx).cfi_idx := io.brinfo.pc_lob >> log2Ceil(coreInstBytes)
   }


   //-------------------------------------------------------------
   // **** Read out data ****
   //-------------------------------------------------------------

   // Send current-PC/next-PC info to the BranchResolutionUnit.
   // TODO only perform the read when a branch instruction requests it.
   val curr_idx = io.get_ftq_pc.ftq_idx
   io.get_ftq_pc.fetch_pc := ram(curr_idx).fetch_pc
   io.get_ftq_pc.next_pc := ram(WrapInc(curr_idx, num_entries)).fetch_pc
   io.get_ftq_pc.next_val := WrapInc(curr_idx, num_entries) =/= enq_ptr.value



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
         printf(" [%d %c%c%c pc=0x%x ms:%c%c-%d]",
            idx.asUInt(width=5.W),
            Mux(enq_ptr.value === idx.U, Str("E"), Str(" ")),
            Mux(commit_ptr === idx.U, Str("C"), Str(" ")),
            Mux(deq_ptr.value === idx.U, Str("D"), Str(" ")),
            ram(idx).fetch_pc,
            Mux(cfi_info(idx).valid, Str("V"), Str(" ")),
            Mux(cfi_info(idx).taken, Str("T"), Str(" ")),
            cfi_info(idx).cfi_idx
         )
         if (j == w-1) printf("\n")
      }
   }




}

