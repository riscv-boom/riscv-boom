//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

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
import chisel3.core.{DontCare}
import chisel3.experimental.{dontTouch}

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.util.{Str}

import boom.bpu._
import boom.common._
import boom.exu._
import boom.util._

/**
 * FTQ Parameters used in configurations
 *
 * @param nEntries # of entries in the FTQ
 */
case class FtqParameters(
  nEntries: Int = 16
)

/**
 * Bundle to add to the FTQ RAM and to be used as the pass in IO
 */
class FTQBundle(implicit p: Parameters) extends BoomBundle
{
  val fetch_pc = UInt(vaddrBitsExtended.W) // TODO compress out high-order bits
  val history = UInt(globalHistoryLength.W)
  val bim_info = new BimStorage
  val bpd_info = UInt(bpdInfoSize.W)
}

/**
 * Initially, store a random branch entry for BIM (set cfi_type==branch).
 * If a branch is resolved that matches the stored BIM entry, set "executed" to high.
 * Otherwise, if a branch is resolved and mispredicted, track the oldest
 * mispredicted cfi instruction for a given fetch entry.
 */
class CfiMissInfo(implicit p: Parameters) extends BoomBundle
{
  val executed = Bool()      // Was the branch stored here for the BIM executed?
                             // Check the cfi_idx matches and cfi_type == branch.
                             // Is DontCare if a misprediction occurred.
  val mispredicted = Bool()  // Was a branch or jump mispredicted in this fetch group?
  val taken = Bool()         // If a branch, was it taken?
  val cfi_idx = UInt(log2Ceil(fetchWidth).W) // which instruction in fetch group?
  val cfi_type = CfiType()   // What kind of instruction is stored here?
}

/**
 * IO to provide a port for a FunctionalUnit to get the PC of an instruction.
 * And for JALRs, the PC of the next instruction.
 */
class GetPCFromFtqIO(implicit p: Parameters) extends BoomBundle
{
  val ftq_idx  = Input(UInt(log2Ceil(ftqSz).W))
  val fetch_pc = Output(UInt(vaddrBitsExtended.W))
  // the next_pc may not be valid (stalled or still being fetched)
  val next_pc  = Valid(UInt(vaddrBitsExtended.W))
}

/**
 * Queue to store the fetch PC and other relevant branch predictor signals that are inflight in the
 * processor.
 *
 * @param num_entries # of entries in the FTQ
 */
class FetchTargetQueue(num_entries: Int)(implicit p: Parameters) extends BoomModule
  with HasBoomCoreParameters
{
  private val idx_sz = log2Ceil(num_entries)

  val io = IO(new BoomBundle {
    // Enqueue one entry for every fetch cycle.
    val enq = Flipped(Decoupled(new FTQBundle()))
    // Pass to FetchBuffer (newly fetched instructions).
    val enq_idx = Output(UInt(idx_sz.W))
    // ROB tells us the youngest committed ftq_idx to remove from FTQ.
    val deq = Flipped(Valid(UInt(idx_sz.W)))

    // Give PC info to BranchUnit.
    val get_ftq_pc = new GetPCFromFtqIO()

    // Restore predictor history on a branch mispredict or pipeline flush.
    val restore_history = Valid(new RestoreHistory)

    // on any sort misprediction or rob flush, reset the enq_ptr, make a PC redirect request.
    val flush = Flipped(Valid(new CommitExceptionSignals()))
    // Redirect the frontend as we see fit (due to ROB/flush interactions).
    val take_pc = Valid(new PCReq())

    // Tell the CSRFile what the fetch-pc at the FTQ's Commit Head is.
    // Still need the low-order bits of the PC from the ROB to know the true Commit PC.
    val com_ftq_idx = Input(UInt(log2Ceil(ftqSz).W))
    val com_fetch_pc = Output(UInt(vaddrBitsExtended.W))

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
  val commit_ptr = RegInit(0.asUInt(log2Ceil(num_entries).W))

  val ram = Mem(num_entries, new FTQBundle())
  ram.suggestName("ftq_bundle_ram")
  val cfi_info = Reg(Vec(num_entries, new CfiMissInfo()))

  private def initCfiInfo(br_seen: Bool, cfi_idx: UInt): CfiMissInfo = {
    val b = Wire(new CfiMissInfo())
    b.executed := false.B
    b.mispredicted := false.B
    b.taken := false.B
    b.cfi_idx := cfi_idx
    b.cfi_type := Mux(br_seen, CfiType.BRANCH, CfiType.NONE)
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

  when (io.deq.valid || io.flush.valid) {
    assert (!(io.deq.valid && io.flush.valid && io.deq.bits =/= io.flush.bits.ftq_idx),
      "FTQ received conflicting flush and deq on same cycle")
    commit_ptr := Mux(io.flush.valid, io.flush.bits.ftq_idx, io.deq.bits)
  }

  //-------------------------------------------------------------
  // **** Handle Mispredictions/Flush ****
  //-------------------------------------------------------------

  when ((io.brinfo.valid && io.brinfo.mispredict) || io.flush.valid) {
    // Flush signal is sent out at commit, so that should override
    // earlier branch mispredict signals.
    val new_ptr = WrapInc(Mux(io.flush.valid,
                            io.flush.bits.ftq_idx,
                            io.brinfo.ftq_idx),
                          num_entries)
    enq_ptr.value := new_ptr
    // If ptr is adjusted, we deleted entries and thus can't be full.
    maybe_full := (enq_ptr.value === new_ptr)
  }

  when (io.brinfo.valid) {
    val prev_executed       = cfi_info(io.brinfo.ftq_idx).executed
    val prev_mispredicted   = cfi_info(io.brinfo.ftq_idx).mispredicted
    val prev_cfi_idx        = cfi_info(io.brinfo.ftq_idx).cfi_idx
    val new_cfi_idx         = io.brinfo.getCfiIdx

    when ((io.brinfo.mispredict && !prev_mispredicted) ||
          (io.brinfo.mispredict && (new_cfi_idx < prev_cfi_idx))) {
      // Overwrite if a misprediction occurs and is older than previous misprediction, if any.
      cfi_info(io.brinfo.ftq_idx).mispredicted := true.B
      cfi_info(io.brinfo.ftq_idx).taken := io.brinfo.taken
      cfi_info(io.brinfo.ftq_idx).cfi_idx := new_cfi_idx
      cfi_info(io.brinfo.ftq_idx).cfi_type := io.brinfo.cfi_type
    } .elsewhen (!prev_mispredicted && (new_cfi_idx === prev_cfi_idx)) {
      cfi_info(io.brinfo.ftq_idx).executed := true.B
      cfi_info(io.brinfo.ftq_idx).taken := io.brinfo.taken
    }
  }

  //-------------------------------------------------------------
  // **** Commit Data Read ****
  //-------------------------------------------------------------

  if (DEBUG_PRINTF) {
    printf("FTQ:\n")
  }

  // Dequeue entry (it's been committed) and update predictors.
  when (do_deq) {
    val com_data = ram(deq_ptr.value)
    val miss_data = cfi_info(deq_ptr.value)
    val com_cntr = com_data.bim_info.value
    val com_taken = miss_data.taken
    val saturated = (com_cntr === 0.U && !com_taken) || (com_cntr === 3.U && com_taken)

    io.bim_update.valid :=
      miss_data.cfi_type === CfiType.BRANCH &&
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

    if (DEBUG_PRINTF) {
      val cfiTypeStrs = CfiTypeToChars(miss_data.cfi_type)
      printf("    Dequeue: Ptr:%d FPC:0x%x Hist:0x%x BIM:(Idx:(%d=%x) V:%c Mispred:%c Taken:%c CfiIdx:%d CntVal:%d CfiType:%c%c%c%c\n",
        deq_ptr.value,
        com_data.fetch_pc,
        com_data.history,
        io.bim_update.bits.entry_idx,
        io.bim_update.bits.entry_idx,
        BoolToChar(io.bim_update.valid, 'V'),
        BoolToChar(io.bim_update.bits.mispredicted, 'M'),
        BoolToChar(io.bim_update.bits.taken, 'T'),
        io.bim_update.bits.cfi_idx,
        io.bim_update.bits.cntr_value,
        cfiTypeStrs(0),
        cfiTypeStrs(1),
        cfiTypeStrs(2),
        cfiTypeStrs(3))
    }
  } .otherwise {
    if (DEBUG_PRINTF) printf("    No dequeue\n")
    io.bim_update.valid := false.B
    io.bpd_update.valid := false.B
    io.bim_update.bits := DontCare
    io.bpd_update.bits := DontCare
  }

  //-------------------------------------------------------------
  // **** Restore Predictor History ****
  //-------------------------------------------------------------

  io.restore_history.valid := (io.brinfo.valid && io.brinfo.mispredict) || io.flush.valid
  io.restore_history.bits.history := 0.U
  io.restore_history.bits.taken := io.brinfo.valid && io.brinfo.taken

  when (io.restore_history.valid) {
    val ridx = Mux(io.flush.valid, io.flush.bits.ftq_idx, io.brinfo.ftq_idx)
    io.restore_history.bits.history := ram(ridx).history
  }

  //-------------------------------------------------------------
  // **** BranchResolutionUnit Read ****
  //-------------------------------------------------------------

  // Send current-PC/next-PC info to the BranchResolutionUnit.
  // TODO only perform the read when a branch instruction requests it.
  val curr_idx = io.get_ftq_pc.ftq_idx
  io.get_ftq_pc.fetch_pc := ram(curr_idx).fetch_pc
  io.get_ftq_pc.next_pc.bits := ram(WrapInc(curr_idx, num_entries)).fetch_pc
  io.get_ftq_pc.next_pc.valid := WrapInc(curr_idx, num_entries) =/= enq_ptr.value

  //-------------------------------------------------------------
  // **** Handle Flush/Pipeline Redirections ****
  //-------------------------------------------------------------

  val com_pc = Wire(UInt())
  val com_pc_next = Wire(UInt())

  io.take_pc.valid := io.flush.valid && !FlushTypes.useCsrEvec(io.flush.bits.flush_typ)
  io.take_pc.bits.addr := Mux(FlushTypes.useSamePC(io.flush.bits.flush_typ), com_pc, com_pc_next)

  // TODO CLEANUP this is wonky: the exception occurs 1 cycle faster than flushing,
  io.com_fetch_pc := ram(io.com_ftq_idx).fetch_pc
  com_pc := (RegNext(AlignPCToBoundary(io.com_fetch_pc, icBlockBytes))
            + io.flush.bits.pc_lob
            - Mux(io.flush.bits.edge_inst, 2.U, 0.U))
  com_pc_next := com_pc + Mux(io.flush.bits.is_rvc, 2.U, 4.U)

  assert (!(io.flush.valid && RegNext(io.com_ftq_idx) =/= io.flush.bits.ftq_idx),
    "[ftq] this code depends on this assumption")

  //-------------------------------------------------------------
  // **** Printfs ****
  //-------------------------------------------------------------

  if (DEBUG_PRINTF && DEBUG_PRINTF_FTQ) {
    printf("    Enq:(V:%c Rdy:%c Idx:%d) Commit:(V:%c Idx:%d) BRInfo:(V&Mispred:%c Idx:%d) Enq,Cmt,DeqPtrs:(%d %d %d)\n",
      BoolToChar(io.enq.valid, 'V'),
      BoolToChar(io.enq.ready, 'R'),
      io.enq_idx,
      BoolToChar(io.deq.valid, 'C'),
      io.deq.bits,
      BoolToChar(io.brinfo.valid && io.brinfo.mispredict, 'M'),
      io.brinfo.ftq_idx,
      enq_ptr.value,
      commit_ptr,
      deq_ptr.value)

    printf("    ")
    val w = 1
    for (
      i <- 0 until (num_entries/w);
      j <- 0 until w
    ) {
      val idx = i+j*(num_entries/w)
      val cfiTypeStrs = CfiTypeToChars(cfi_info(idx).cfi_type)
      printf("[Entry:%d Enq,Cmt,DeqPtr:(%c %c %c) PC:0x%x Hist:0x%x " +
        "CFI:(Exec,Mispred,Taken:(%c %c %c) Type:%c%c%c%c Idx:%d) BIM:(Idx:%d Val:0x%x)] ",
        idx.asUInt(width=5.W),
        BoolToChar(enq_ptr.value === idx.U, 'E', ' '),
        BoolToChar(   commit_ptr === idx.U, 'C', ' '),
        BoolToChar(deq_ptr.value === idx.U, 'D', ' '),
        ram(idx).fetch_pc(31,0),
        ram(idx).history,
        BoolToChar(    cfi_info(idx).executed, 'E', ' '),
        BoolToChar(cfi_info(idx).mispredicted, 'V', ' '),
        BoolToChar(       cfi_info(idx).taken, 'T', ' '),
        cfiTypeStrs(0),
        cfiTypeStrs(1),
        cfiTypeStrs(2),
        cfiTypeStrs(3),
        cfi_info(idx).cfi_idx,
        ram(idx).bim_info.entry_idx,
        ram(idx).bim_info.value)
      if (j == w-1) printf("\n    ")
    }
    printf("\n")
  }

  // force to show up in the waveform
  val debug_deq_ptr = deq_ptr.value
  dontTouch(debug_deq_ptr)
}
