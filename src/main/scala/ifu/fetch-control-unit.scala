//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Fetch Control Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// This has lots of the brains behind fetching instructions and managing the
// branch predictors and deciding which instructions to fetch next.
//
// Stages:
//    * F0 -- next PC select
//    * F1 -- icache SRAM access
//    * F2 -- icache response/pre-decode
//    * F3 -- branch-check/verification/redirect-compute
//    * F4 -- take redirect

package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.core.{withReset, DontCare}
import chisel3.experimental.{dontTouch}

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.util._

import boom.bpu._
import boom.common._
import boom.exu._
import boom.util.{BoolToChar, AgePriorityEncoder, ElasticReg}

/**
 * Bundle passed into the FetchBuffer and used to combine multiple
 * relevant signals together.
 */
class FetchBundle(implicit p: Parameters) extends BoomBundle
{
  val pc            = UInt(vaddrBitsExtended.W)
  val edge_inst     = Bool() // True if 1st instruction in this bundle is pc - 2
  val ftq_idx       = UInt(log2Ceil(ftqSz).W)
  val insts         = Vec(fetchWidth, Bits(32.W))
  val mask          = Bits(fetchWidth.W) // mark which words are valid instructions
  val xcpt_pf_if    = Bool() // I-TLB miss (instruction fetch fault).
  val xcpt_ae_if    = Bool() // Access exception.
  val replay_if     = Bool() // the I$ demands we replay the instruction fetch.
  val xcpt_ma_if_oh = UInt(fetchWidth.W)
                           // A cfi branched to a misaligned address --
                           // one-hot encoding (1:1 with insts).

  val bpu_info      = Vec(fetchWidth, new BranchPredInfo) // TODO remove

  val debug_events  = Vec(fetchWidth, new DebugStageEvents)
}

/**
 * Fetch control unit that interfaces with the branch predictor pipeline externally.
 * This holds and manages things like the Fetch Target Queue, Fetch Buffer,
 * Branch Checker, Branch Decoder, and more.
 */
class FetchControlUnit(implicit p: Parameters) extends BoomModule
  with HasL1ICacheBankedParameters
{
  val io = IO(new BoomBundle {
    val imem_req          = Valid(new freechips.rocketchip.rocket.FrontendReq)
    val imem_resp         = Flipped(Decoupled(new freechips.rocketchip.rocket.FrontendResp))

    val f2_btb_resp       = Flipped(Valid(new BoomBTBResp))
    val f2_bpd_resp       = Flipped(Valid(new BpdResp))
    val f3_ras_update     = Valid(new RasUpdate)
    val f3_bpd_resp       = Flipped(Valid(new BpdResp))
    val f3_btb_update     = Valid(new BoomBTBUpdate)
    val f3_is_br          = Output(Vec(fetchWidth, Bool()))

    val f2_redirect       = Output(Bool())
    val f3_stall          = Output(Bool())
    val f3_clear          = Output(Bool())
    val f4_redirect       = Output(Bool())
    val f4_taken          = Output(Bool())

    val bim_update        = Valid(new BimUpdate)
    val bpd_update        = Valid(new BpdUpdate)

    val ftq_restore_history= Valid(new RestoreHistory)

    val br_unit           = Input(new BranchUnitResp())
    val get_pc            = new GetPCFromFtqIO()

    val tsc_reg           = Input(UInt(xLen.W))

    val clear_fetchbuffer = Input(Bool())

    val commit            = Flipped(Valid(UInt(ftqSz.W)))
    val flush_info        = Flipped(Valid(new CommitExceptionSignals))
    val flush_take_pc     = Input(Bool())
    val flush_pc          = Input(UInt((vaddrBits+1).W)) // TODO rename; no longer catch-all flush_pc

    val com_ftq_idx       = Input(UInt(log2Ceil(ftqSz).W)) // ROB tells us the commit pointer so we
                                                           // can read out the PC.
    val com_fetch_pc      = Output(UInt(vaddrBitsExtended.W)) // tell CSRFile the fetch-pc at the FTQ head.

    // sfence needs to steal the TLB CAM part.
    val sfence_take_pc    = Input(Bool())
    val sfence_addr       = Input(UInt((vaddrBits+1).W))

    val fetchpacket       = new DecoupledIO(new FetchBufferResp)
  })

  val bchecker = Module (new BranchChecker)
  val ftq = Module(new FetchTargetQueue(num_entries = ftqSz))
  val fb = if (useNewFetchBuffer) Module(new    FetchBuffer(numEntries=fetchBufferSz*coreWidth))
           else                   Module(new OldFetchBuffer(numEntries=fetchBufferSz*coreWidth))
  val monitor: Option[FetchMonitor] = (useFetchMonitor).option(Module(new FetchMonitor))

  val br_unit = io.br_unit
  val fseq_reg = RegInit(0.U(xLen.W))
  val f0_redirect_pc = Wire(UInt(vaddrBitsExtended.W))

  val clear_f3        = WireInit(false.B)
  val q_f3_imemresp   = withReset(reset.toBool || clear_f3) {
                          Module(new ElasticReg(gen = new freechips.rocketchip.rocket.FrontendResp)) }
  val q_f3_btb_resp   = withReset(reset.toBool || clear_f3) { Module(new ElasticReg(gen = Valid(new BoomBTBResp))) }
  val f3_req          = Wire(Valid(new PCReq()))
  val f3_fetch_bundle = Wire(new FetchBundle)
  val f3_valid        = q_f3_imemresp.io.deq.valid

  dontTouch(f3_fetch_bundle)

  val r_f4_valid = RegInit(false.B)

  // Can the F3 stage proceed?
  val f4_ready = fb.io.enq.ready && ftq.io.enq.ready
  val f4_fire = f3_valid && f4_ready

  // F4 Redirection path.
  val r_f4_req = Reg(Valid(new PCReq()))
  val r_f4_taken = RegInit(false.B)
  val r_f4_fetchpc = Reg(UInt())

  // F4 Instruction path.
  val r_f4_fetch_bundle = RegEnable(f3_fetch_bundle, f4_fire)

  io.f3_stall := !f4_ready
  io.f3_clear := clear_f3

  //-------------------------------------------------------------
  // **** Helper Functions ****
  //-------------------------------------------------------------

  private def KillMask(m_enable: Bool, m_idx: UInt, m_width: Int): UInt = {
    val mask = Wire(Bits(m_width.W))
    mask := Fill(m_width, m_enable) & (Fill(m_width, 1.U) << 1.U << m_idx)
    mask
  }

  private def GetRandomCfiIdx(br_mask: UInt): UInt = {
    // TODO lower toggle frequency
    val c = Counter(fetchWidth)
    c.inc()
    val ret = AgePriorityEncoder(br_mask.asBools, c.value)
    ret
  }

  //-------------------------------------------------------------
  // **** NextPC Select (F0) ****
  //-------------------------------------------------------------

  val f0_redirect_val =
    br_unit.take_pc ||
    io.flush_take_pc ||
    io.sfence_take_pc ||
    (io.f2_btb_resp.valid && io.f2_btb_resp.bits.taken && io.imem_resp.ready) ||
    (r_f4_valid && r_f4_req.valid)

  io.imem_req.valid   := f0_redirect_val // tell front-end we had an unexpected change in the stream
  io.imem_req.bits.pc := f0_redirect_pc
  io.imem_req.bits.speculative := !(io.flush_take_pc)
  io.imem_resp.ready  := q_f3_imemresp.io.enq.ready

  f0_redirect_pc :=
    Mux(io.sfence_take_pc,
      io.sfence_addr,
    Mux(ftq.io.take_pc.valid,
      ftq.io.take_pc.bits.addr,
    Mux(io.flush_take_pc,
      io.flush_pc,
    Mux(br_unit.take_pc,
      br_unit.target,
    Mux(r_f4_valid && r_f4_req.valid,
      r_f4_req.bits.addr,
      io.f2_btb_resp.bits.target)))))

  //-------------------------------------------------------------
  // **** ICache Access (F1) ****
  //-------------------------------------------------------------

  // twiddle thumbs

  //-------------------------------------------------------------
  // **** ICache Response/Pre-decode (F2) ****
  //-------------------------------------------------------------

  q_f3_imemresp.io.enq.valid := io.imem_resp.valid
  q_f3_btb_resp.io.enq.valid := io.imem_resp.valid

  q_f3_imemresp.io.enq.bits := io.imem_resp.bits
  q_f3_btb_resp.io.enq.bits := io.f2_btb_resp

  //-------------------------------------------------------------
  // **** F3 ****
  //-------------------------------------------------------------

  clear_f3 := io.clear_fetchbuffer || (r_f4_valid && r_f4_req.valid)

  val f3_imemresp = q_f3_imemresp.io.deq.bits
  val f3_btb_resp = q_f3_btb_resp.io.deq.bits

  q_f3_imemresp.io.deq.ready := f4_ready
  q_f3_btb_resp.io.deq.ready := f4_ready

  // round off to nearest fetch boundary
  val f3_aligned_pc = alignToFetchBoundary(f3_imemresp.pc)
  val f3_debug_pcs  = Wire(Vec(fetchWidth, UInt(vaddrBitsExtended.W)))
  val f3_valid_mask = Wire(Vec(fetchWidth, Bool()))
  val is_br     = Wire(Vec(fetchWidth, Bool()))
  val is_jal    = Wire(Vec(fetchWidth, Bool()))
  val is_jr     = Wire(Vec(fetchWidth, Bool()))
  val is_call   = Wire(Vec(fetchWidth, Bool()))
  val is_ret    = Wire(Vec(fetchWidth, Bool()))
  val is_rvc    = Wire(Vec(fetchWidth, Bool()))
  val br_targs  = Wire(Vec(fetchWidth, UInt(vaddrBitsExtended.W)))
  val jal_targs = Wire(Vec(fetchWidth, UInt(vaddrBitsExtended.W)))
  // catch misaligned jumps -- let backend handle misaligned
  // branches though since only taken branches are exceptions.
  val jal_targs_ma = Wire(Vec(fetchWidth, Bool()))

  // Tracks trailing 16b of previous fetch packet
  val prev_half    = Reg(UInt(coreInstBits.W))
  // Tracks if last fetchpacket contained a half-inst
  val prev_is_half = RegInit(false.B)
  // Tracks nextpc after the previous fetch bundle
  val prev_nextpc = Reg(UInt(vaddrBitsExtended.W))

  val use_prev = prev_is_half && f3_fetch_bundle.pc === prev_nextpc

  assert(fetchWidth >= 4 || !usingCompressed) // Logic gets kind of annoying with fetchWidth = 2
  for (i <- 0 until fetchWidth) {
    val bpd_decoder = Module(new BranchDecode)
    val is_valid = Wire(Bool())
    val inst = Wire(UInt((2*coreInstBits).W))
    if (!usingCompressed) {
      is_valid := true.B
      inst     := f3_imemresp.data(i*coreInstBits+coreInstBits-1,i*coreInstBits)
      f3_fetch_bundle.edge_inst := false.B
    } else if (i == 0) {
      when (use_prev) {
        inst := Cat(f3_imemresp.data(15,0), prev_half)
        f3_fetch_bundle.edge_inst := true.B
      } .otherwise {
        inst := f3_imemresp.data(31,0)
        f3_fetch_bundle.edge_inst := false.B
      }
      is_valid := true.B
    } else if (i == 1) {
      // Need special case since 0th instruction may carry over the wrap around
      inst     := f3_imemresp.data(i*coreInstBits+2*coreInstBits-1,i*coreInstBits)
      is_valid := use_prev || !(f3_valid_mask(i-1) && f3_fetch_bundle.insts(i-1)(1,0) === 3.U)
    } else if (icIsBanked && i == (fetchWidth / 2) - 1) {
      // If we are using a banked I$ we could get cut-off halfway through the fetch bundle
      inst     := f3_imemresp.data(i*coreInstBits+2*coreInstBits-1,i*coreInstBits)
      is_valid := !(f3_valid_mask(i-1) && f3_fetch_bundle.insts(i-1)(1,0) === 3.U) &&
                  !(inst(1,0) === 3.U && !f3_imemresp.mask(i+1))
    } else if (i == fetchWidth - 1) {
      inst     := Cat(0.U(16.W), f3_imemresp.data(fetchWidth*coreInstBits-1,i*coreInstBits))
      is_valid := !((f3_valid_mask(i-1) && f3_fetch_bundle.insts(i-1)(1,0) === 3.U) ||
                    inst(1,0) === 3.U)
    } else {
      inst     := f3_imemresp.data(i*coreInstBits+2*coreInstBits-1,i*coreInstBits)
      is_valid := !(f3_valid_mask(i-1) && f3_fetch_bundle.insts(i-1)(1,0) === 3.U)
    }
    f3_fetch_bundle.insts(i) := inst

    // TODO do not compute a vector of targets
    val pc = (f3_aligned_pc
            + (i << log2Ceil(coreInstBytes)).U
            - Mux(use_prev && (i == 0).B, 2.U, 0.U))
    f3_debug_pcs(i)     := pc
    bpd_decoder.io.inst := ExpandRVC(inst)
    bpd_decoder.io.pc   := pc

    f3_valid_mask(i) := f3_valid && f3_imemresp.mask(i) && is_valid
    is_br(i)     := f3_valid && bpd_decoder.io.is_br   && f3_imemresp.mask(i) && is_valid
    is_jal(i)    := f3_valid && bpd_decoder.io.is_jal  && f3_imemresp.mask(i) && is_valid
    is_jr(i)     := f3_valid && bpd_decoder.io.is_jalr && f3_imemresp.mask(i) && is_valid
    is_call(i)   := f3_valid && bpd_decoder.io.is_call && f3_imemresp.mask(i) && is_valid
    is_ret(i)    := f3_valid && bpd_decoder.io.is_ret  && f3_imemresp.mask(i) && is_valid
    is_rvc(i)    := f3_valid_mask(i) && inst(1,0) =/= 3.U && usingCompressed.B
    br_targs(i)  := bpd_decoder.io.target
    jal_targs(i) := bpd_decoder.io.target
    jal_targs_ma(i) := jal_targs(i)(1) && is_jal(i) && !usingCompressed.B
  }

  val f3_br_seen = f3_valid &&
                   !f3_imemresp.xcpt.pf.inst &&
                   is_br.reduce(_|_) &&
                   (!is_jal.reduce(_|_) || (PriorityEncoder(is_br.asUInt) < PriorityEncoder(is_jal.asUInt))) &&
                   (!is_jr.reduce(_|_) || (PriorityEncoder(is_br.asUInt) < PriorityEncoder(is_jr.asUInt)))
  val f3_jr_seen = f3_valid &&
                   !f3_imemresp.xcpt.pf.inst &&
                   is_jr.reduce(_|_) &&
                   (!is_jal.reduce(_|_) || (PriorityEncoder(is_jr.asUInt) < PriorityEncoder(is_jal.asUInt)))

  // What does the BIM predict?
//  val f3_bim_predictions = is_br.asUInt & f3_btb_resp.bits.bim_resp.bits.getTakens()
//  val f3_bim_br_taken = f3_bim_predictions.orR
//  val f3_bim_br_idx = PriorityEncoder(f3_bim_predictions)
//  val f3_bim_target = br_targs(f3_bim_br_idx)

  // Does the BPD have a prediction to make (in the case of a BTB miss?)
  // Calculate in F3 but don't redirect until F4.
  io.f3_is_br := is_br
  val f3_bpd_predictions = is_br.asUInt & io.f3_bpd_resp.bits.takens
  val f3_bpd_br_taken = f3_bpd_predictions.orR
  val f3_bpd_br_idx = PriorityEncoder(f3_bpd_predictions)
  val f3_bpd_target = br_targs(f3_bpd_br_idx)
  // check for jumps -- if we decide to override a taken BTB and choose "nextline" we don't want to miss the JAL.
  val f3_has_jal = is_jal.reduce(_|_)
  val f3_jal_idx = PriorityEncoder(is_jal.asUInt)
  val f3_jal_target = jal_targs(f3_jal_idx)
  val f3_bpd_btb_update_valid = WireInit(false.B) // does the BPD's choice cause a BTB update?
  val f3_bpd_may_redirect_taken = WireInit(false.B) // request towards a taken branch target
  val f3_bpd_may_redirect_next = WireInit(false.B) // override taken prediction and fetch the next line (or take JAL)
  val f3_bpd_may_redirect = f3_bpd_may_redirect_taken || f3_bpd_may_redirect_next
  val f3_bpd_redirect_cfiidx =
    Mux(f3_bpd_may_redirect_taken,
      f3_bpd_br_idx,
    Mux(f3_has_jal,
      f3_jal_idx,
      (fetchWidth-1).U))
  val f3_bpd_redirect_target =
    Mux(f3_bpd_may_redirect_taken,
      f3_bpd_target,
    Mux(f3_has_jal,
      f3_jal_target,
      nextFetchStart(f3_aligned_pc)))

  // mask out instructions after predicted branch
  val f3_kill_mask = Wire(UInt(fetchWidth.W))
  val f3_btb_mask = Wire(UInt(fetchWidth.W))
  val f3_bpd_mask = Wire(UInt(fetchWidth.W))

  when (f3_valid && f4_ready && !r_f4_req.valid) {
    val last_idx  = Mux(inLastChunk(f3_fetch_bundle.pc) && icIsBanked.B,
                      (fetchWidth/2-1).U, (fetchWidth-1).U)
    prev_is_half := (usingCompressed.B
    && !(f3_valid_mask(last_idx-1.U) && f3_fetch_bundle.insts(last_idx-1.U)(1,0) === 3.U)
    && !f3_kill_mask(last_idx)
    && f3_btb_mask(last_idx)
    && f3_bpd_mask(last_idx)
    && f3_fetch_bundle.insts(last_idx)(1,0) === 3.U)
    prev_half    := f3_fetch_bundle.insts(last_idx)(15,0)
    prev_nextpc  := alignToFetchBoundary(f3_fetch_bundle.pc) + Mux(inLastChunk(f3_fetch_bundle.pc) && icIsBanked.B,
                                                                 bankBytes.U,
                                                                 fetchBytes.U)
  }

  when (f3_valid && f3_btb_resp.valid) {
    // btb made a prediction
    // Make a redirect request if:
    //    - the BPD (br) comes earlier than the BTB's redirection.
    //    - If both the BTB and the BPD predicted a branch, the BPD wins (if disagree).
    //       * involves refetching the next cacheline and undoing the current packet's mask if we "undo" the BT's
    //       taken branch.

    val btb_idx = f3_btb_resp.bits.cfi_idx

    when (BpredType.isAlwaysTaken(f3_btb_resp.bits.bpd_type)) {// TODO XXX BUG look at actual inst, not BTB
      f3_bpd_may_redirect_taken := io.f3_bpd_resp.valid && f3_bpd_br_taken && f3_bpd_br_idx < btb_idx

      assert (f3_btb_resp.bits.taken)
    } .elsewhen (f3_btb_resp.bits.taken) {
      // does the bpd predict the branch is taken too? (assuming bpd_valid)
      val bpd_agrees_with_btb = f3_bpd_predictions(btb_idx)
      f3_bpd_may_redirect_taken := io.f3_bpd_resp.valid && f3_bpd_br_taken &&
        (f3_bpd_br_idx < btb_idx || !bpd_agrees_with_btb)
        // XXX in this scenario, ignore the btb mask and go with the bpd mask
      f3_bpd_may_redirect_next := io.f3_bpd_resp.valid && !f3_bpd_br_taken

      assert (BpredType.isBranch(f3_btb_resp.bits.bpd_type))
    } .elsewhen (!f3_btb_resp.bits.taken) {
      f3_bpd_may_redirect_taken := io.f3_bpd_resp.valid && f3_bpd_br_taken
    }
  } .otherwise {
    // BTB made no prediction - let the BPD do what it wants
    f3_bpd_may_redirect_taken := io.f3_bpd_resp.valid && f3_bpd_br_taken
    // add branch to the BTB if we think it will be taken
    f3_bpd_btb_update_valid := f3_bpd_may_redirect_taken
  }

  assert (PopCount(VecInit(f3_bpd_may_redirect_taken, f3_bpd_may_redirect_next)) <= 1.U,
    "[bpd_pipeline] mutually-exclusive signals firing")

  // catch any BTB mispredictions (and fix-up missed JALs)
  bchecker.io.valid := f3_valid
  bchecker.io.inst_mask := VecInit(f3_imemresp.mask.asBools)
  bchecker.io.is_br  := is_br
  bchecker.io.is_jal := is_jal
  bchecker.io.is_jr  := is_jr
  bchecker.io.is_call  := is_call
  bchecker.io.is_ret   := is_ret
  bchecker.io.is_rvc   := is_rvc
  bchecker.io.br_targs := br_targs
  bchecker.io.jal_targs := jal_targs
  bchecker.io.fetch_pc := f3_imemresp.pc
  bchecker.io.aligned_pc := f3_aligned_pc
  bchecker.io.btb_resp := f3_btb_resp
  bchecker.io.bpd_resp := io.f3_bpd_resp

  // who wins? bchecker or bpd? or jal?
  val jal_overrides_bpd = f3_has_jal && f3_jal_idx < f3_bpd_redirect_cfiidx && f3_bpd_may_redirect_taken
  val f3_bpd_overrides_bcheck =
    f3_bpd_may_redirect &&
    !jal_overrides_bpd &&
    (!bchecker.io.req.valid || (f3_bpd_redirect_cfiidx < bchecker.io.req_cfi_idx))
  f3_req.valid := f3_valid && (bchecker.io.req.valid ||
                  (f3_bpd_may_redirect && !jal_overrides_bpd)) // && !(f0_redirect_val)
  f3_req.bits.addr := Mux(f3_bpd_overrides_bcheck, f3_bpd_redirect_target, bchecker.io.req.bits.addr)


  // TODO this logic is broken and vestigial. Do update correctly (remove RegNext)
  val f3_btb_update_bits = Wire(new BoomBTBUpdate)
  io.f3_btb_update.valid := RegNext(bchecker.io.btb_update.valid || f3_bpd_btb_update_valid)
  io.f3_btb_update.bits := RegNext(f3_btb_update_bits)
  f3_btb_update_bits := bchecker.io.btb_update.bits
  when (f3_bpd_overrides_bcheck) {
    f3_btb_update_bits.target   := f3_bpd_target
    f3_btb_update_bits.cfi_idx  := f3_bpd_br_idx
    f3_btb_update_bits.bpd_type := BpredType.BRANCH
    f3_btb_update_bits.cfi_type := CfiType.branch
  }

  io.f3_ras_update := bchecker.io.ras_update

  f3_kill_mask := KillMask(
    f3_req.valid,
    Mux(f3_bpd_overrides_bcheck, f3_bpd_redirect_cfiidx, bchecker.io.req_cfi_idx),
    fetchWidth)

  f3_btb_mask := Mux(f3_btb_resp.valid && !f3_req.valid,
                   f3_btb_resp.bits.mask,
                   Fill(fetchWidth, 1.U(1.W)))
  f3_bpd_mask := Fill(fetchWidth, 1.U(1.W))  // TODO XXX add back bpd
//  f3_bpd_mask := Mux(io.f3_bpu_request.valid && !f3_req.valid,
//                 io.f3_bpu_request.bits.mask,
//                 Fill(fetchWidth, UInt(1,1)))
  f3_fetch_bundle.mask := (f3_imemresp.mask
                          & ~f3_kill_mask
                          & f3_btb_mask
                          & f3_bpd_mask
                          & f3_valid_mask.asUInt())


  val f3_taken = WireInit(false.B) // was a branch taken in the F3 stage?
  when (f3_req.valid) {
    // f3_bpd only requests taken redirections on btb misses.
    // f3_req via bchecker only ever requests nextline_pc or jump targets (which we don't track in ghistory).
    f3_taken := Mux(f3_bpd_overrides_bcheck, (f3_bpd_may_redirect_taken && !jal_overrides_bpd), false.B)
  } .elsewhen (f3_btb_resp.valid) {
    f3_taken := f3_btb_resp.bits.taken
    // TODO XXX f3_taken logic is wrong. it looks to be missing bpd? Or is that f3_req.valid?
  }

  f3_fetch_bundle.pc := f3_imemresp.pc
  f3_fetch_bundle.ftq_idx := ftq.io.enq_idx
  f3_fetch_bundle.xcpt_pf_if := f3_imemresp.xcpt.pf.inst
  f3_fetch_bundle.xcpt_ae_if := f3_imemresp.xcpt.ae.inst
  f3_fetch_bundle.replay_if :=  f3_imemresp.replay
  f3_fetch_bundle.xcpt_ma_if_oh := jal_targs_ma.asUInt

  for (w <- 0 until fetchWidth) {
    f3_fetch_bundle.debug_events(w).fetch_seq := DontCare
  }

  for (w <- 0 until fetchWidth) {
    f3_fetch_bundle.bpu_info(w).btb_blame     := false.B
    f3_fetch_bundle.bpu_info(w).btb_hit       := f3_btb_resp.valid
    f3_fetch_bundle.bpu_info(w).btb_taken     := false.B

    f3_fetch_bundle.bpu_info(w).bpd_blame     := false.B
    f3_fetch_bundle.bpu_info(w).bpd_hit       := io.f3_bpd_resp.valid
    f3_fetch_bundle.bpu_info(w).bpd_taken     := io.f3_bpd_resp.bits.takens(w.U)
    f3_fetch_bundle.bpu_info(w).bim_resp      := f3_btb_resp.bits.bim_resp.bits
    f3_fetch_bundle.bpu_info(w).bpd_resp      := io.f3_bpd_resp.bits

    when (w.U === f3_bpd_br_idx && f3_bpd_overrides_bcheck) {
      f3_fetch_bundle.bpu_info(w).bpd_blame := true.B
    }
    // TODO deal with blame with bpd
//    .elsewhen (w.U === f3_btb_resp.bits.cfi_idx && io.f3_bpu_request.valid && !f3_req.valid) {
//      f3_fetch_bundle.bpu_info(w).bpd_blame := true.B
//    }
    .elsewhen (w.U === f3_btb_resp.bits.cfi_idx && f3_btb_resp.valid && !f3_req.valid) {
       f3_fetch_bundle.bpu_info(w).btb_blame := true.B
    }

    when (w.U === f3_btb_resp.bits.cfi_idx && f3_btb_resp.valid) {
      f3_fetch_bundle.bpu_info(w).btb_taken := f3_btb_resp.bits.taken
    }
  }

  //-------------------------------------------------------------
  // **** F4 ****
  //-------------------------------------------------------------

  when (io.clear_fetchbuffer || r_f4_req.valid) {
    r_f4_valid := false.B
    r_f4_req.valid := false.B
  } .elsewhen (f4_ready) {
    r_f4_valid := f3_valid && !(r_f4_valid && r_f4_req.valid)
    r_f4_req := f3_req
    r_f4_fetchpc := f3_imemresp.pc
    r_f4_taken := f3_taken
  }

  assert (!(r_f4_req.valid && !r_f4_valid),
    "[fetch] f4-request is high but f4_valid is not.")
  assert (!(io.clear_fetchbuffer && !(br_unit.take_pc || io.flush_take_pc || io.sfence_take_pc)),
    "[fetch] F4 should be cleared if a F0_redirect due to BRU/Flush/Sfence.")

  //-------------------------------------------------------------
  // **** FetchBuffer Enqueue ****
  //-------------------------------------------------------------

  // Fetch Buffer
  fb.io.enq.valid := f3_valid && !r_f4_req.valid && f4_ready && f3_fetch_bundle.mask =/= 0.U
  fb.io.enq.bits  := f3_fetch_bundle
  fb.io.clear := io.clear_fetchbuffer

  for (i <- 0 until fetchWidth) {
    if (i == 0) {
      fb.io.enq.bits.debug_events(i).fetch_seq := fseq_reg
    } else {
      fb.io.enq.bits.debug_events(i).fetch_seq := fseq_reg +
        PopCount(f3_fetch_bundle.mask.asUInt()(i-1,0))
    }
  }

  //-------------------------------------------------------------
  // **** FetchTargetQueue ****
  //-------------------------------------------------------------

  ftq.io.enq.valid := f3_valid && !r_f4_req.valid && f4_ready
  ftq.io.enq.bits.fetch_pc := f3_imemresp.pc
  ftq.io.enq.bits.history := io.f3_bpd_resp.bits.history
  ftq.io.enq.bits.bpd_info := io.f3_bpd_resp.bits.info
  when (f3_btb_resp.bits.bim_resp.valid) {
    ftq.io.enq.bits.bim_info.value := f3_btb_resp.bits.bim_resp.bits.getCounterValue(f3_btb_resp.bits.cfi_idx)
    ftq.io.enq.bits.bim_info.entry_idx := f3_btb_resp.bits.bim_resp.bits.entry_idx
  } .otherwise {
    ftq.io.enq.bits.bim_info.value := 2.U
    ftq.io.enq.bits.bim_info.entry_idx := 0.U
  }

  ftq.io.enq.bits.bim_info.br_seen := (is_br.asUInt & f3_imemresp.mask) =/= 0.U
  ftq.io.enq.bits.bim_info.cfi_idx := GetRandomCfiIdx(is_br.asUInt & f3_imemresp.mask)

  ftq.io.deq := io.commit
  ftq.io.brinfo := br_unit.brinfo
  io.get_pc <> ftq.io.get_ftq_pc
  ftq.io.flush := io.flush_info
  ftq.io.com_ftq_idx := io.com_ftq_idx
  io.com_fetch_pc := ftq.io.com_fetch_pc
  io.ftq_restore_history <> ftq.io.restore_history

  io.f2_redirect := io.f2_btb_resp.valid && io.f2_btb_resp.bits.taken && io.imem_resp.ready
  io.f4_redirect := r_f4_valid && r_f4_req.valid
  io.f4_taken    := r_f4_taken

  io.bim_update := ftq.io.bim_update
  io.bpd_update := ftq.io.bpd_update

  //-------------------------------------------------------------
  // **** Frontend Response ****
  //-------------------------------------------------------------

  io.fetchpacket.bits.uops map { _ := DontCare }
  io.fetchpacket <> fb.io.deq

  // enable/disable depending on parameters (should be disconnected in normal tapeouts)
  if (useFetchMonitor) {
    monitor.get.io.fire := io.fetchpacket.fire()
    monitor.get.io.uops := io.fetchpacket.bits.uops
    monitor.get.io.clear := io.clear_fetchbuffer
  }

  //-------------------------------------------------------------
  // **** Pipeview Support ****
  //-------------------------------------------------------------

  if (O3PIPEVIEW_PRINTF) {
    when (fb.io.enq.fire()) {
      fseq_reg := fseq_reg + PopCount(fb.io.enq.bits.mask)
      val bundle = fb.io.enq.bits
      for (i <- 0 until fetchWidth) {
        when (bundle.mask(i)) {
          // TODO for now, manually set the fetch_tsc to point to when the fetch
          // started. This doesn't properly account for i-cache and i-tlb misses. :(
          // Also not factoring in NPC.
          printf("%d; O3PipeView:fetch:%d:0x%x:0:%d:DASM(%x)\n",
            bundle.debug_events(i).fetch_seq,
            io.tsc_reg - (2*O3_CYCLE_TIME).U,
            f3_debug_pcs(i),
            bundle.debug_events(i).fetch_seq,
            bundle.insts(i))
        }
      }
    }
  }

  //-------------------------------------------------------------
  // **** Assertions ****
  //-------------------------------------------------------------

  // check if enqueue'd PC is a target of the previous valid enqueue'd PC.
  // clear checking if misprediction/flush/etc.
  val last_valid      = RegInit(false.B)
  val last_pc         = Reg(UInt(vaddrBitsExtended.W))
  val last_target     = Reg(UInt(vaddrBitsExtended.W))
  val last_nextlinepc = Reg(UInt(vaddrBitsExtended.W))
  val last_cfi_type   = Reg(UInt(CfiType.SZ.W))

  val cfi_idx         = (fetchWidth-1).U - PriorityEncoder(Reverse(f3_fetch_bundle.mask))
  val fetch_pc        = f3_fetch_bundle.pc
  val curr_aligned_pc = alignToFetchBoundary(fetch_pc)
  val cfi_pc          = (curr_aligned_pc
                        + (cfi_idx << log2Ceil(coreInstBytes).U)
                        - Mux(f3_fetch_bundle.edge_inst && cfi_idx === 0.U, 2.U, 0.U))

  when (fb.io.enq.fire() &&
        !f3_fetch_bundle.replay_if &&
        !f3_fetch_bundle.xcpt_pf_if &&
        !f3_fetch_bundle.xcpt_ae_if) {

    assert (f3_fetch_bundle.mask =/= 0.U)
    val curr_inst = ExpandRVC(if (fetchWidth == 1) f3_fetch_bundle.insts(0) else f3_fetch_bundle.insts(cfi_idx))
    last_valid     := true.B
    last_pc        := cfi_pc
    last_nextlinepc := nextFetchStart(curr_aligned_pc)

    val cfi_type = GetCfiType(curr_inst)
    last_cfi_type := cfi_type
    last_target := Mux(cfi_type === CfiType.jal,
      ComputeJALTarget(cfi_pc, curr_inst, xLen),
      ComputeBranchTarget(cfi_pc, curr_inst, xLen))

    when (last_valid) {
      // check for error
      when (last_cfi_type === CfiType.none) {
        assert (fetch_pc ===  last_nextlinepc,
          "[fetch] A non-cfi instruction is followed by the wrong instruction.")
      } .elsewhen (last_cfi_type === CfiType.jal) {
        // ignore misaligned fetches -- we should have marked the instruction as excepting,
        // but when it makes a misaligned fetch request the I$ gives us back an aligned PC.
        val f_pc = (fetch_pc(vaddrBitsExtended-1, log2Ceil(coreInstBytes))
                  - Mux(f3_fetch_bundle.edge_inst, 1.U, 0.U))
        val targ = last_target(vaddrBitsExtended-1, log2Ceil(coreInstBytes))
        when (f_pc =/= targ) {
          printf("about to abort: [fetch] JAL is followed by the wrong instruction. 0x%x =/= 0x%x\n",
            f_pc, targ)
          printf("fetch_pc: 0x%x, last_target: 0x%x, last_nextlinepc: 0x%x\n",
            fetch_pc, last_target, last_nextlinepc)
        }
        assert (f_pc === targ, "[fetch] JAL is followed by the wrong instruction.")
      } .elsewhen (last_cfi_type === CfiType.branch) {
        // again, ignore misaligned fetches -- an exception should be caught.
        val f_pc = (fetch_pc(vaddrBitsExtended-1, log2Ceil(coreInstBytes))
                  - Mux(f3_fetch_bundle.edge_inst, 1.U, 0.U))
        val targ = last_target(vaddrBitsExtended-1, log2Ceil(coreInstBytes))
        when (f_pc =/= targ && fetch_pc =/= last_nextlinepc) {
          printf("about to abort: [fetch] Branch is followed by the wrong instruction\n")
          printf("0x%x =/= 0x%x, 0x%x =/= 0x%x\n", f_pc, targ, fetch_pc, last_nextlinepc)
        }
        assert (fetch_pc === last_nextlinepc || f_pc === targ,
          "[fetch] branch is followed by the wrong instruction.")
      } .otherwise {
        // we can't verify JALR instruction stream integrity --  /throws hands up.
        assert (last_cfi_type === CfiType.jalr, "[fetch] Should be a JALR if none of the others were valid.")
      }
    }
  }

  when (io.clear_fetchbuffer ||
        (fb.io.enq.fire() &&
    (f3_fetch_bundle.replay_if || f3_fetch_bundle.xcpt_pf_if || f3_fetch_bundle.xcpt_ae_if))) {
    last_valid := false.B
  }

  when (fb.io.enq.fire() &&
        !f3_fetch_bundle.replay_if &&
        !f3_fetch_bundle.xcpt_pf_if &&
        !f3_fetch_bundle.xcpt_ae_if) {
    // check that, if there is a jal, the last valid instruction is not after him.
    // <beq, jal, bne, ...>, either the beq or jal may be the last instruction, but because
    // the jal dominates everything after it, nothing valid can be after it.
    val f3_is_jal = VecInit(f3_fetch_bundle.insts map {x =>
      GetCfiType(ExpandRVC(x)) === CfiType.jal}).asUInt & f3_fetch_bundle.mask
    val f3_jal_idx = PriorityEncoder(f3_is_jal)
    val has_jal = f3_is_jal.orR

    assert (!(has_jal && f3_jal_idx < cfi_idx), "[fetch] JAL was not taken.")
  }

  // Check that all elastic registers in the same stage show the same control signals.
  assert(q_f3_imemresp.io.deq.valid === q_f3_btb_resp.io.deq.valid)
  assert(q_f3_imemresp.io.enq.valid === q_f3_btb_resp.io.enq.valid)
  assert(q_f3_imemresp.io.deq.ready === q_f3_btb_resp.io.deq.ready)
  assert(q_f3_imemresp.io.enq.ready === q_f3_btb_resp.io.enq.ready)

  //-------------------------------------------------------------
  // **** Printfs ****
  //-------------------------------------------------------------

  if (DEBUG_PRINTF) {
    // Fetch Stage 1
    printf("Fetch Controller:\n")
    printf("    Fetch1:\n")
    printf("        BRUnit: V:%c Tkn:%c Mispred:%c F0Redir:%c TakePc:%c RedirPc:0x%x\n",
      BoolToChar(io.br_unit.brinfo.valid, 'V'),
      BoolToChar(io.br_unit.brinfo.taken, 'T'),
      BoolToChar(io.br_unit.brinfo.mispredict, 'M'),
      BoolToChar(f0_redirect_val, 'T'),
      Mux(io.flush_take_pc, Str("F"),
        Mux(io.br_unit.take_pc, Str("B"), Str(" "))),
      f0_redirect_pc)

    // Fetch Stage 2
    printf("    Fetch2:\n")
    printf("        IMemResp: V:%c Rdy:%c PC:0x%x Msk:0x%x\n",
      BoolToChar(io.imem_resp.valid, 'V'),
      BoolToChar(io.imem_resp.ready, 'R'),
      io.imem_resp.bits.pc,
      io.imem_resp.bits.mask)

    printf("        ")
    for (w <- fetchWidth to 1 by -1) {// count in reverse
      printf("DASM(%x) ", io.imem_resp.bits.data((w*coreInstBits)-1, (w-1)*coreInstBits))
      // split extra long fetch into 2 lines
      if (fetchWidth == 8 && w == 5) {
        printf("\n        ")
      }
    }
    printf("\n")

    printf("        IMemResp: BTB: Tkn:%c Idx:%d TRG:0x%x\n",
      BoolToChar(io.imem_resp.bits.btb.taken, 'T'),
      io.imem_resp.bits.btb.bridx,
      io.imem_resp.bits.btb.target(19,0))

    // Fetch Stage 3
    printf("    Fetch3:\n")
    printf("        FbEnq: V:%c PC:0x%x Msk:0x%x\n",
      BoolToChar(fb.io.enq.valid, 'V'),
      fb.io.enq.bits.pc,
      fb.io.enq.bits.mask)
  }
}
