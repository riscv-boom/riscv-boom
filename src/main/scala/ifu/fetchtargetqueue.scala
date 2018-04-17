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
//
// TODO:
// * reduce port counts.


package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str
import boom.bpu._
import boom.common._
import boom.exu._
import boom.util._

case class FtqParameters(
   nEntries: Int = 16
)

class FTQBundle(implicit p: Parameters) extends BoomBundle()(p)
{
   val fetch_pc = UInt(width = vaddrBitsExtended.W) // TODO compress out high-order bits
   val history = UInt(width = GLOBAL_HISTORY_LENGTH.W)
   val bim_info = new BimStorage
   val bpd_info = UInt(width = BPD_INFO_SIZE.W)
}

// Initially, store a random branch entry for BIM (set cfi_type==branch).
// If a branch is resolved that matches the stored BIM entry, set "executed" to high.
// Otherwise, if a branch is resolved and mispredicted, track the oldest
// mispredicted cfi instruction for a given fetch entry.
class CfiMissInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val executed = Bool()      // Was the branch stored here for the BIM executed?
                              // Check the cfi_idx matches and cfi_type == branch.
                              // Is DontCare if a misprediction occurred.
   val mispredicted = Bool()  // Was a branch or jump mispredicted in this fetch group?
   val taken = Bool()         // If a branch, was it taken?
   val cfi_idx = UInt(width=log2Up(fetchWidth).W) // which instruction in fetch group?
   val cfi_type = CfiType()   // What kind of instruction is stored here?
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

      // Restore predictor history on a branch mispredict or pipeline flush.
      val restore_history = Valid(new RestoreHistory)

      // on any sort misprediction or rob flush, reset the enq_ptr, make a PC redirect request.
      val flush = Flipped(Valid(new FlushSignals()))
      // Redirect the frontend as we see fit (due to ROB/flush interactions).
      val take_pc = Valid(new PCReq())
      // Tell the CSRFile what the fetch-pc at the FTQ's Commit Head is.
      // Still need the low-order bits of the PC from the ROB to know the true Commit PC.
      val com_ftq_idx = Input(UInt(width=log2Up(ftqSz).W))
      val com_fetch_pc = Output(UInt(width=vaddrBitsExtended.W))

      val bim_update = Valid(new BimUpdate)
      val bpd_update = Valid(new BpdUpdate)

      // BranchResolutionUnit tells us the outcome of branches/jumps.
      val brinfo = Input(new BrResolutionInfo())
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

   private def initCfiInfo(br_seen: Bool, cfi_idx: UInt): CfiMissInfo =
   {
      val b = Wire(new CfiMissInfo())
      b.executed := false.B
      b.mispredicted := false.B
      b.taken := false.B
      b.cfi_idx := cfi_idx
      b.cfi_type := Mux(br_seen, CfiType.branch, CfiType.none)
      b
   }


   //-------------------------------------------------------------
   // **** Pointer Arithmetic and Enqueueing of Data ****
   //-------------------------------------------------------------

   private val do_enq = WireInit(io.enq.fire())
   private val do_deq = WireInit(deq_ptr.value =/= commit_ptr)

   when (do_enq) {
     ram(enq_ptr.value) := io.enq.bits
     cfi_info(enq_ptr.value) := initCfiInfo(io.enq.bits.bim_info.br_seen, io.enq.bits.bim_info.cfi_idx)
     enq_ptr.inc()
   }
   when (do_deq) {
     deq_ptr.inc()
   }
   when (do_enq =/= do_deq) {
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

   //-------------------------------------------------------------
   // **** Handle Mispredictions ****
   //-------------------------------------------------------------

   when (io.brinfo.valid && io.brinfo.mispredict)
   {
      val new_ptr = WrapInc(io.brinfo.ftq_idx, num_entries)
      enq_ptr.value := new_ptr
      // If ptr is adjusted, we deleted entries and thus can't be full.
      maybe_full := (enq_ptr.value === new_ptr)
   }

   when (io.brinfo.valid)
   {
      val prev_executed       = cfi_info(io.brinfo.ftq_idx).executed
      val prev_mispredicted   = cfi_info(io.brinfo.ftq_idx).mispredicted
      val prev_cfi_idx        = cfi_info(io.brinfo.ftq_idx).cfi_idx
      val new_cfi_idx         = io.brinfo.getCfiIdx

      when (
         (io.brinfo.mispredict && !prev_mispredicted) ||
         (io.brinfo.mispredict && (new_cfi_idx < prev_cfi_idx)))
      {
         // Overwrite if a misprediction occurs and is older than previous misprediction, if any.
         cfi_info(io.brinfo.ftq_idx).mispredicted := true.B
         cfi_info(io.brinfo.ftq_idx).taken := io.brinfo.taken
         cfi_info(io.brinfo.ftq_idx).cfi_idx := new_cfi_idx
         cfi_info(io.brinfo.ftq_idx).cfi_type := io.brinfo.cfi_type
      }
      .elsewhen (!prev_mispredicted && (new_cfi_idx === prev_cfi_idx))
      {
         cfi_info(io.brinfo.ftq_idx).executed := true.B
         cfi_info(io.brinfo.ftq_idx).taken := io.brinfo.taken
      }
   }



   //-------------------------------------------------------------
   // **** Commit Data Read ****
   //-------------------------------------------------------------

   // Dequeue entry (it's been committed) and update predictors.
   when (do_deq)
   {
      val com_data = ram(deq_ptr.value)
      val miss_data = cfi_info(deq_ptr.value)
      val com_cntr = com_data.bim_info.value
      val com_taken = miss_data.taken
      val saturated = (com_cntr === 0.U && !com_taken) || (com_cntr === 3.U && com_taken)

      io.bim_update.valid :=
         miss_data.cfi_type === CfiType.branch &&
         (miss_data.mispredicted) ||
         (!miss_data.mispredicted && miss_data.executed && !saturated)

      io.bim_update.bits.entry_idx    := com_data.bim_info.entry_idx
      io.bim_update.bits.cntr_value   := com_cntr
      io.bim_update.bits.cfi_idx      := miss_data.cfi_idx
      io.bim_update.bits.taken        := miss_data.taken
      io.bim_update.bits.mispredicted := miss_data.mispredicted


      io.bpd_update.valid              := true.B
      io.bpd_update.bits.mispredict    := miss_data.mispredicted
      io.bpd_update.bits.taken         := miss_data.taken
      io.bpd_update.bits.miss_cfi_idx  := miss_data.cfi_idx
      io.bpd_update.bits.fetch_pc      := com_data.fetch_pc
      io.bpd_update.bits.history       := com_data.history
      io.bpd_update.bits.info          := com_data.bpd_info


      if (DEBUG_PRINTF)
      {
         printf("FTQ: deq[%d]=0x%x hist=0x%x bim[%d=%x]:%c %c%c %d-%d %d\n",
            deq_ptr.value,
            com_data.fetch_pc,
            com_data.history,
            io.bim_update.bits.entry_idx,
            io.bim_update.bits.entry_idx,
            Mux(io.bim_update.valid, Str("V"), Str(" ")),
            Mux(io.bim_update.bits.mispredicted, Str("M"), Str(" ")),
            Mux(io.bim_update.bits.taken, Str("T"), Str(" ")),
            io.bim_update.bits.cfi_idx,
            io.bim_update.bits.cntr_value,
            miss_data.cfi_type
            )
      }
   }
   .otherwise
   {
      if (DEBUG_PRINTF) printf("FTQ: no dequeue\n")
      io.bim_update.valid := false.B
      io.bpd_update.valid := false.B
   }

   //-------------------------------------------------------------
   // **** Restore Predictor History ****
   //-------------------------------------------------------------


   io.restore_history.valid := (io.brinfo.valid && io.brinfo.mispredict) || io.flush.valid
   io.restore_history.bits.history := 0.U
   io.restore_history.bits.taken := io.brinfo.valid && io.brinfo.taken

   when (io.restore_history.valid)
   {
      val ridx = Mux(io.flush.valid, io.com_ftq_idx, io.brinfo.ftq_idx)
      io.restore_history.bits.history := ram(ridx).history
   }


   //-------------------------------------------------------------
   // **** BranchResolutionUnit Read ****
   //-------------------------------------------------------------

   // Send current-PC/next-PC info to the BranchResolutionUnit.
   // TODO only perform the read when a branch instruction requests it.
   val curr_idx = io.get_ftq_pc.ftq_idx
   io.get_ftq_pc.fetch_pc := ram(curr_idx).fetch_pc
   io.get_ftq_pc.next_pc := ram(WrapInc(curr_idx, num_entries)).fetch_pc
   io.get_ftq_pc.next_val := WrapInc(curr_idx, num_entries) =/= enq_ptr.value


   //-------------------------------------------------------------
   // **** Handle Flush/Pipeline Redirections ****
   //-------------------------------------------------------------

   val com_pc = Wire(UInt())
   val com_pc_plus4 = Wire(UInt())

   io.take_pc.valid := io.flush.valid && !FlushTypes.useCsrEvec(io.flush.bits.flush_typ)
   io.take_pc.bits.addr := Mux(FlushTypes.useSamePC(io.flush.bits.flush_typ), com_pc, com_pc_plus4)

   // TODO CLEANUP this is wonky: the exception occurs 1 cycle faster than flushing,
   io.com_fetch_pc := ram(io.com_ftq_idx).fetch_pc
   com_pc := RegNext(AlignPCToBoundary(io.com_fetch_pc, icBlockBytes)) + io.flush.bits.pc_lob
   com_pc_plus4 := com_pc + 4.U // TODO RVC

   assert (RegNext(io.com_ftq_idx) === io.flush.bits.ftq_idx, "[ftq] this code depends on this assumption")

   //-------------------------------------------------------------
   // **** Printfs ****
   //-------------------------------------------------------------

   if (DEBUG_PRINTF && DEBUG_PRINTF_FTQ)
   {
      printf("FTQ: %c %c: %d; commit: %c:%d brinfo: %c:%d [%d %d %d]\n",
         Mux(io.enq.valid, Str("V"), Str("-")),
         Mux(io.enq.ready, Str("R"), Str("-")),
         io.enq_idx,
         Mux(io.deq.valid, Str("C"), Str("-")),
         io.deq.bits,
         Mux(io.brinfo.valid && io.brinfo.mispredict, Str("M"), Str("-")),
         io.brinfo.ftq_idx,
         enq_ptr.value, commit_ptr, deq_ptr.value
      )

      val w = 4
      for (
         i <- 0 until (num_entries/w);
         j <- 0 until w
      ){
         val idx = i+j*(num_entries/w)
         printf(" [%d %c%c%c pc=0x%x 0x%x [h] ms:%c%c%c%d-%d bim[%d]:0x%x]",
            idx.asUInt(width=5.W),
            Mux(enq_ptr.value === idx.U, Str("E"), Str(" ")),
            Mux(commit_ptr === idx.U, Str("C"), Str(" ")),
            Mux(deq_ptr.value === idx.U, Str("D"), Str(" ")),
            ram(idx).fetch_pc(31,0),
            ram(idx).history,
            Mux(cfi_info(idx).executed, Str("E"), Str(" ")),
            Mux(cfi_info(idx).mispredicted, Str("V"), Str(" ")),
            Mux(cfi_info(idx).taken, Str("T"), Str(" ")),
            cfi_info(idx).cfi_type,
            cfi_info(idx).cfi_idx,
            ram(idx).bim_info.entry_idx,
            ram(idx).bim_info.value
         )
         if (j == w-1) printf("\n")
      }
   }

   // force to show up in the waveform
   val debug_deq_ptr = deq_ptr.value
   dontTouch(debug_deq_ptr)
}

