//******************************************************************************
// Copyright (c) 2013 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// The RoCC shim unit. Similar to the LSU, in that we need to allocate entries
// for instruction bits at dispatch, and send commands strictly in order.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.v3.exu

import chisel3._
import chisel3.util._
import midas.targetutils.SynthesizePrintf

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.{RoCCCoreIO, RoCCInstruction}
import freechips.rocketchip.rocket._

import boom.v3.common._
import boom.v3.util._

/**
  * IO Bundle representing RoCC shim interface with the core
  */
class RoCCShimCoreIO(implicit p: Parameters) extends BoomBundle
{
  // Decode Stage
  val dis_rocc_vals    = Input(Vec(coreWidth, Bool()))
  val dis_uops         = Input(Vec(coreWidth, new MicroOp))
  val rxq_full         = Output(Bool())
  val rxq_empty        = Output(Bool())
  val rxq_idx          = Output(Vec(coreWidth, UInt(log2Ceil(numRxqEntries).W)))
  val rob_pnr_idx      = Input(UInt(robAddrSz.W))
  val rob_head_idx     = Input(UInt(robAddrSz.W))

  val rocc             = Flipped(new RoCCCoreIO)
}

/**
 * IO bundle representing the different signals to interact with the RoCC
 * Vaguely follows the IO of a functional unit.
  */
class RoCCShimIO(implicit p: Parameters) extends BoomBundle
{
  val core             = new RoCCShimCoreIO

  val req              = Flipped(new DecoupledIO(new FuncUnitReq(xLen)))
  val resp             = new DecoupledIO(new FuncUnitResp(xLen))
  val brupdate           = Input(new BrUpdateInfo())
  val status           = Input(new MStatus)
  val exception        = Input(Bool())
}

/**
  * Structure similar to LSU
  *  - Holds instruction and operand bits prior to issuing RoCC inst to
  *    accelerator
  *  - After issue, holds queue of translations between logical and physical
  *    specifiers to handle RoCC responses
  */
class RoCCShim(implicit p: Parameters) extends BoomModule
{
  val io = IO(new RoCCShimIO)

  io.req.ready := true.B
  io.core.rocc.exception := false.B
  io.core.rocc.mem.req.ready := false.B
  io.core.rocc.mem.s2_nack := false.B
  io.core.rocc.mem.s2_nack_cause_raw := false.B
  io.core.rocc.mem.s2_uncached := false.B
  io.core.rocc.mem.s2_paddr := DontCare
  io.core.rocc.mem.resp.valid := false.B
  io.core.rocc.mem.resp.bits := DontCare
  io.core.rocc.mem.replay_next := false.B
  io.core.rocc.mem.s2_xcpt.ma.ld := false.B
  io.core.rocc.mem.s2_xcpt.ma.st := false.B
  io.core.rocc.mem.s2_xcpt.pf.ld := false.B
  io.core.rocc.mem.s2_xcpt.pf.st := false.B
  io.core.rocc.mem.s2_xcpt.gf.ld := false.B
  io.core.rocc.mem.s2_xcpt.gf.st := false.B
  io.core.rocc.mem.s2_xcpt.ae.ld := false.B
  io.core.rocc.mem.s2_xcpt.ae.st := false.B
  io.core.rocc.mem.s2_gpa := DontCare
  io.core.rocc.mem.s2_gpa_is_pte := false.B
  io.core.rocc.mem.uncached_resp.map(r => {
    r.valid := false.B
    r.bits := DontCare
  })
  io.core.rocc.mem.ordered := false.B
  io.core.rocc.mem.perf.acquire := false.B
  io.core.rocc.mem.perf.release := false.B
  io.core.rocc.mem.perf.grant := false.B
  io.core.rocc.mem.perf.tlbMiss := false.B
  io.core.rocc.mem.perf.blocked := false.B
  io.core.rocc.mem.perf.canAcceptStoreThenLoad := false.B
  io.core.rocc.mem.perf.canAcceptStoreThenRMW := false.B
  io.core.rocc.mem.perf.canAcceptLoadThenLoad := false.B
  io.core.rocc.mem.perf.storeBufferEmptyAfterLoad := false.B
  io.core.rocc.mem.perf.storeBufferEmptyAfterStore := false.B
  io.core.rocc.mem.clock_enabled := false.B
  io.core.rocc.mem.store_pending := false.B

  // RoCC execute queue. Wait for PNR, holds operands and inst bits
  val rxq_val       = Reg(Vec(numRxqEntries, Bool()))
  val rxq_op_val    = Reg(Vec(numRxqEntries, Bool()))
  val rxq_committed = Reg(Vec(numRxqEntries, Bool()))
  val rxq_uop       = Reg(Vec(numRxqEntries, new MicroOp()))
  val rxq_inst      = Reg(Vec(numRxqEntries, UInt(32.W)))
  val rxq_rs1       = Reg(Vec(numRxqEntries, UInt(xLen.W)))
  val rxq_rs2       = Reg(Vec(numRxqEntries, UInt(xLen.W)))

  // DIAG (temporary): split STAGE=rxq-issue into "never issued from IQ" vs
  // "issued but capture guard blocked". Per-entry latches, reset at (re)alloc
  // (enq) and on full reset/flush.
  val diag_req_seen = RegInit(VecInit(Seq.fill(numRxqEntries)(false.B))) // io.req.valid ever fired
  val diag_blk_exc  = RegInit(VecInit(Seq.fill(numRxqEntries)(false.B))) // blocked by io.exception
  val diag_blk_br   = RegInit(VecInit(Seq.fill(numRxqEntries)(false.B))) // blocked by IsKilledByBranch

  // RoCC commit queue. Wait for response, or immediate unbusy
  val rcq           = Module(new Queue(new MicroOp(), numRcqEntries))

  // The instruction we are waiting for response from
  val rxq_head     = RegInit(0.U(log2Ceil(numRxqEntries).W))
  // The next instruction we are waiting to "commit" through PNR
  val rxq_com_head = RegInit(0.U(log2Ceil(numRxqEntries).W))
  val rxq_tail     = RegInit(0.U(log2Ceil(numRxqEntries).W))


  // Decode
  val rocc_idx = WireInit(0.U)
  val br_mask = WireInit(0.U(maxBrCount.W))
  var enq_val = false.B

  assert(PopCount(io.core.dis_rocc_vals) <= 1.U)
  for (w <- 0 until coreWidth) {
    val enq_this = !enq_val && io.core.dis_rocc_vals(w) && io.core.dis_uops(w).uopc === uopROCC
    when (enq_this) {
      rocc_idx := w.U
    }

    io.core.rxq_idx(w) := Mux(enq_val, WrapInc(rxq_tail, numRxqEntries), rxq_tail)

    enq_val = enq_val || enq_this
  }

  when (enq_val) {
    rxq_val      (rxq_tail) := true.B
    rxq_op_val   (rxq_tail) := false.B
    rxq_committed(rxq_tail) := false.B
    rxq_uop      (rxq_tail) := io.core.dis_uops(rocc_idx)
    rxq_inst     (rxq_tail) := io.core.dis_uops(rocc_idx).inst
    rxq_tail                := WrapInc(rxq_tail, numRxqEntries)
    diag_req_seen(rxq_tail) := false.B // fresh entry
    diag_blk_exc (rxq_tail) := false.B
    diag_blk_br  (rxq_tail) := false.B
  }

  // DIAG: latch that io.req fired for this entry (regardless of the capture
  // guard), and if the guard blocked it, why. Distinguishes never-issued from
  // issued-but-capture-blocked at the hang.
  when (io.req.valid) {
    val d_idx = io.req.bits.uop.rxq_idx
    diag_req_seen(d_idx) := true.B
    val d_killed = IsKilledByBranch(io.brupdate, io.req.bits.uop)
    val d_exc    = io.exception || RegNext(io.exception)
    when (d_killed || d_exc) {
      diag_blk_exc(d_idx) := d_exc
      diag_blk_br (d_idx) := d_killed
    }
  }

  // Wait for operands
  when (io.req.valid && !IsKilledByBranch(io.brupdate, io.req.bits.uop)
     && !io.exception && !RegNext(io.exception)) {
    val rxq_idx = io.req.bits.uop.rxq_idx
    assert(io.req.bits.uop.rob_idx === rxq_uop(rxq_idx).rob_idx,
      "Mismatch between RoCCUnit request and RoCC execute head")
    assert(rxq_val(rxq_idx),
      "Trying to execute rocc inst without the instruction bits")

    rxq_op_val   (rxq_idx)      := true.B
    rxq_uop      (rxq_idx).pdst := io.req.bits.uop.pdst
    rxq_rs1      (rxq_idx)      := io.req.bits.rs1_data
    rxq_rs2      (rxq_idx)      := io.req.bits.rs2_data
  }

  // Wait for ROB to OK us to execute
  when (rxq_val   (rxq_com_head) &&
        IsOlder(rxq_uop(rxq_com_head).rob_idx, io.core.rob_pnr_idx, io.core.rob_head_idx)) {
    rxq_committed(rxq_com_head)   := true.B
    rxq_com_head                  := WrapInc(rxq_com_head, numRxqEntries)
  }

  // Execute
  io.core.rocc.cmd.valid := false.B
  io.core.rocc.cmd.bits  := DontCare
  rcq.io.enq.valid       := false.B
  rcq.io.enq.bits        := rxq_uop(rxq_head)
  when (rxq_op_val   (rxq_head) &&
        rxq_val      (rxq_head) &&
        rxq_committed(rxq_head) &&
        io.core.rocc.cmd.ready &&
        rcq.io.enq.ready) {
    io.core.rocc.cmd.valid         := true.B
    io.core.rocc.cmd.bits.inst     := rxq_inst(rxq_head).asTypeOf(new RoCCInstruction)
    io.core.rocc.cmd.bits.rs1      := rxq_rs1(rxq_head)
    io.core.rocc.cmd.bits.rs2      := rxq_rs2(rxq_head)
    io.core.rocc.cmd.bits.status   := io.status
    rcq.io.enq.valid               := true.B

    rxq_val(rxq_head)              := false.B
    rxq_head                       := WrapInc(rxq_head, numRxqEntries)
  }


  io.core.rxq_full  := WrapInc(rxq_tail, numRxqEntries) === rxq_head
  io.core.rxq_empty := rxq_tail === rxq_head

  //--------------------------
  // Branches
  for (i <- 0 until numRxqEntries) {
    when (rxq_val(i)) {
      rxq_uop(i).br_mask := GetNewBrMask(io.brupdate, rxq_uop(i))
      when (IsKilledByBranch(io.brupdate, rxq_uop(i))) {
        rxq_val(i)      := false.B
        rxq_op_val(i)   := false.B
      }
    }
  }
  when (io.brupdate.b2.mispredict && !io.exception) {
    rxq_tail := io.brupdate.b2.uop.rxq_idx
  }


  //--------------------------
  // Exception / Reset

  // FIX: on any pipeline flush (io.exception = rob.io.flush.valid), FULLY reset
  // the RXQ instead of preserving "committed" (past-PNR) entries. A flush is
  // taken at the ROB head and squashes/re-fetches everything younger, including
  // past-PNR RoCC uops between head and PNR; stock preserved their RXQ entries
  // -> orphan (ROB re-fetches, RXQ keeps a stale copy) -> ROB-head wedge. Full
  // reset keeps the RXQ consistent with the ROB (safe here: the only RoCC op is
  // the idempotent prefetch, so any re-fetch/re-issue is harmless).
  val pnr_flush_fix = false // PNRFIX_TOGGLE  (false = stock, for A/B)
  when (reset.asBool || (io.exception && pnr_flush_fix.B)) {
    rxq_tail     := 0.U
    rxq_head     := 0.U
    rxq_com_head := 0.U
    for (i <- 0 until numRxqEntries) {
      rxq_val(i)       := false.B
      rxq_op_val(i)    := false.B
      rxq_committed(i) := false.B
      diag_req_seen(i) := false.B
      diag_blk_exc(i)  := false.B
      diag_blk_br(i)   := false.B
    }
  } .elsewhen (io.exception) {
    // STOCK behavior (fix disabled): rewind tail, preserve committed entries
    rxq_tail := rxq_com_head
    for (i <- 0 until numRxqEntries) {
      when (!rxq_committed(i)) {
        rxq_val(i)      := false.B
        rxq_op_val(i)   := false.B
      }
    }
  }

  // FIX (rxq-reclaim, 2026-07-08): rxq_head advances ONLY on fire (needs
  // rxq_val && rxq_op_val && rxq_committed) and rxq_com_head ONLY on commit
  // (needs rxq_val). But a branch-kill (IsKilledByBranch above) or an exception
  // clears rxq_val WITHOUT advancing either pointer, so a killed head/com_head
  // permanently strands the RoCC pipe -> ROB-head wedge (observed on mcf as
  // roccdiag2 STAGE=rxq-issue-BLOCKED-OTHER @ ~5.18B cyc; reproduces with
  // fast-load-use OFF, so it is NOT the spec-load poison). Reclaim killed
  // entries by skipping them, preserving head <= com_head <= tail. SAFE: the
  // only RoCC op here is the idempotent software prefetch, so dropping a killed
  // copy is harmless (the ROB re-fetches/re-executes it if it was real).
  val rxq_reclaim_fix = true // RECLAIMFIX_TOGGLE
  when (rxq_reclaim_fix.B && !rxq_val(rxq_com_head) && (rxq_com_head =/= rxq_tail)) {
    rxq_com_head := WrapInc(rxq_com_head, numRxqEntries)
  }
  when (rxq_reclaim_fix.B && !rxq_val(rxq_head) && (rxq_head =/= rxq_com_head)) {
    rxq_head := WrapInc(rxq_head, numRxqEntries)
  }



  //------------------
  // Handle responses

  // Either we get a response, or the RoCC op expects no response
  val handle_resp = ((io.core.rocc.resp.valid || rcq.io.deq.bits.dst_rtype === RT_X)
                  && io.resp.ready
                  && rcq.io.deq.valid)

  io.core.rocc.resp.ready := io.resp.ready && rcq.io.deq.bits.dst_rtype =/= RT_X
  io.resp.valid           := false.B
  io.resp.bits            := DontCare
  rcq.io.deq.ready        := false.B
  when (handle_resp) {
    assert((rcq.io.deq.bits.dst_rtype === RT_X)
        || io.core.rocc.resp.bits.rd === rcq.io.deq.bits.ldst,
      "RoCC response destination register does not match expected")

    io.resp.valid              := true.B
    io.resp.bits.uop           := rcq.io.deq.bits
    io.resp.bits.data          := io.core.rocc.resp.bits.data

    rcq.io.deq.ready           := true.B
  }

  // DIAG (temporary): fallback probe for the revert-to-stock experiment. If
  // stock ALSO hangs, name where the RoCC op is stuck — the RXQ-fire side or
  // the RCQ-writeback side. Trip at 4000 cyc (< the 8192-cyc pipeline-hung
  // assert). Remove once the hang is resolved.
  val diag_rxq_empty   = rxq_tail === rxq_head
  val diag_outstanding = !diag_rxq_empty || rcq.io.deq.valid
  val diag_resp_fire   = io.resp.valid && io.resp.ready
  val diag_stall       = RegInit(0.U(20.W))
  diag_stall := Mux(diag_outstanding && !diag_resp_fire, diag_stall + 1.U, 0.U)
  val diag_trip = diag_stall > 4000.U
  // RXQ-fire side — split rxq-issue by whether io.req ever fired (Hyp1 vs Hyp2):
  assert(!(diag_trip && !diag_rxq_empty && !rxq_op_val(rxq_head) && !diag_req_seen(rxq_head)),
    "[roccdiag2] STAGE=rxq-issue-NEVERREQ: uop never issued from IQ (io.req never fired)")
  assert(!(diag_trip && !diag_rxq_empty && !rxq_op_val(rxq_head) && diag_req_seen(rxq_head)
           && diag_blk_exc(rxq_head)),
    "[roccdiag2] STAGE=rxq-issue-BLOCKED-EXC: issued but capture blocked by io.exception")
  assert(!(diag_trip && !diag_rxq_empty && !rxq_op_val(rxq_head) && diag_req_seen(rxq_head)
           && diag_blk_br(rxq_head) && !diag_blk_exc(rxq_head)),
    "[roccdiag2] STAGE=rxq-issue-BLOCKED-BR: issued but capture blocked by IsKilledByBranch")
  assert(!(diag_trip && !diag_rxq_empty && !rxq_op_val(rxq_head) && diag_req_seen(rxq_head)
           && !diag_blk_exc(rxq_head) && !diag_blk_br(rxq_head)),
    "[roccdiag2] STAGE=rxq-issue-BLOCKED-OTHER: issued, op_val unset, no guard reason latched")
  assert(!(diag_trip && !diag_rxq_empty && rxq_op_val(rxq_head) && !rxq_committed(rxq_head)),
    "[roccdiag2] STAGE=rxq-pnr: RXQ head has operand but not committed (PNR)")
  assert(!(diag_trip && !diag_rxq_empty && rxq_op_val(rxq_head) && rxq_committed(rxq_head)
           && !io.core.rocc.cmd.ready),
    "[roccdiag2] STAGE=rxq-cmd: RXQ head committed but accelerator cmd.ready low")
  assert(!(diag_trip && !diag_rxq_empty && rxq_op_val(rxq_head) && rxq_committed(rxq_head)
           && io.core.rocc.cmd.ready && !rcq.io.enq.ready),
    "[roccdiag2] STAGE=rxq-enq: RXQ head ready but RCQ full (enq.ready low)")
  // RCQ-writeback side (buffered RT_X op can't retire):
  assert(!(diag_trip && rcq.io.deq.valid && (rcq.io.deq.bits.dst_rtype === RT_X) && !io.resp.ready),
    "[roccdiag2] STAGE=rcq-writeback: RCQ RT_X head but io.resp.ready low (ll_wbarb in(2) starved)")

  // SYNTHPRINT (temporary): once the RXQ head has been stalled > 200 cyc (above
  // normal RoCC-path occupancy, below the 4000-cyc assert trip), stream the
  // shim head state every cycle. Watching io.req.valid over time deconfounds
  // NEVERREQ vs BLOCKED-EXC without a resettable latch. Volume is capped by the
  // 4000-cyc assert halt (~3800 lines/probe). Wrapped in SynthesizePrintf so
  // ONLY this printf is synthesized on FPGA (not Rocket/BOOM's other printfs).
  when (!diag_rxq_empty && diag_stall > 200.U) {
    SynthesizePrintf(printf("[pf-shim] stall=%d head=%d rob=%d opval=%d comm=%d val=%d cmdrdy=%d resprdy=%d reqv=%d reqidx=%d reqrob=%d exc=%d rexc=%d bmis=%d reqseen=%d blkexc=%d blkbr=%d\n",
      diag_stall, rxq_head, rxq_uop(rxq_head).rob_idx,
      rxq_op_val(rxq_head), rxq_committed(rxq_head), rxq_val(rxq_head),
      io.core.rocc.cmd.ready, io.resp.ready,
      io.req.valid, io.req.bits.uop.rxq_idx, io.req.bits.uop.rob_idx,
      io.exception, RegNext(io.exception), io.brupdate.b2.mispredict,
      diag_req_seen(rxq_head), diag_blk_exc(rxq_head), diag_blk_br(rxq_head)))
  }
}
