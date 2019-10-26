//******************************************************************************
// Copyright (c) 2017 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Frontend
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch
import chisel3.core.{withReset}
import chisel3.internal.sourceinfo.{SourceInfo}

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._

import boom.common._
import boom.exu.{CommitExceptionSignals, BranchDecode, BrUpdateInfo}
import boom.util.{BoomCoreStringPrefix}


class FrontendResp(implicit p: Parameters) extends BoomBundle()(p) {
  val pc = UInt(vaddrBitsExtended.W)  // ID stage PC
  val data = UInt((fetchWidth * coreInstBits).W)
  val mask = UInt(fetchWidth.W)
  val xcpt = new FrontendExceptions
  val ghist = new GlobalHistory

  val debug_bsrc = UInt(BSRC_SZ.W)
}

class GlobalHistory(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  // For the dual banked case, each bank ignores the contribution of the
  // last bank to the history. Thus we have to track the most recent update to the
  // history in that case
  val old_history = UInt(globalHistoryLength.W)
  val new_history = Valid(Bool())

  def histories(bank: Int) = {
    if (nBanks == 1) {
      old_history
    } else {
      require(nBanks == 2)
      if (bank == 0) {
        old_history
      } else {
        (old_history << new_history.valid) | (new_history.bits && new_history.valid)
      }
    }
  }
}

/**
 * Parameters to manage a L1 Banked ICache
 */
trait HasBoomFrontendParameters extends HasL1ICacheParameters
{
  // How many banks does the ICache use?
  val nBanks = if (cacheParams.fetchBytes <= 8) 1 else 2
  // How many bytes wide is a bank?
  val bankBytes = fetchBytes/nBanks

  val bankWidth = fetchWidth/nBanks

  require(nBanks == 1 || nBanks == 2)


  def history_update(history: GlobalHistory, branches: UInt, cfi_taken_br: Bool, cfi_idx: UInt, cfi_valid: Bool, addr: UInt) = {
    val new_history = Wire(new GlobalHistory)
    if (nBanks == 1) {
      // In the single bank case every bank sees the history including the previous bank
      new_history.new_history := DontCare
      new_history.old_history := Mux(cfi_taken_br    , history.histories(0) << 1 | 1.U,
                                 Mux(branches =/= 0.U, history.histories(0) << 1,
                                                       history.histories(0)))
    } else {
      // In the two bank case every bank ignore the history added by the previous bank
      val base = history.histories(1)
      val ignore_second_bank = (cfi_valid && cfi_idx < bankWidth.U) || mayNotBeDualBanked(addr)
      when (ignore_second_bank) {
        new_history.old_history := history.histories(1)
        new_history.new_history.valid := (cfi_taken_br && cfi_idx < bankWidth.U) ||
                                         (branches(bankWidth-1,0) =/= 0.U)
        new_history.new_history.bits  := cfi_taken_br && cfi_idx < bankWidth.U
      } .otherwise {
        new_history.old_history := Mux(cfi_taken_br && cfi_idx < bankWidth.U, history.histories(1) << 1 | 1.U,
                                   Mux(branches(bankWidth-1,0) =/= 0.U      , history.histories(1) << 1,
                                                                              history.histories(1)))
        new_history.new_history.valid := branches(2*bankWidth-1, bankWidth-1) =/= 0.U
        new_history.new_history.bits  := cfi_taken_br
      }
    }
    new_history
  }

  // How many "chunks"/interleavings make up a cache line?
  val numChunks = cacheParams.blockBytes / bankBytes

  // Which bank is the address pointing to?
  def bank(addr: UInt) = if (nBanks == 2) addr(log2Ceil(bankBytes)) else 0.U
  def mayNotBeDualBanked(addr: UInt) = {
    require(nBanks == 2)
    addr(blockOffBits-1, log2Ceil(bankBytes)) === (numChunks-1).U
  }

  def blockAlign(addr: UInt) = ~(~addr | (cacheParams.blockBytes-1).U)
  def bankAlign(addr: UInt) = ~(~addr | (bankBytes-1).U)

  def fetchIdx(addr: UInt) = addr >> log2Ceil(fetchBytes)

  def nextBank(addr: UInt) = bankAlign(addr) + bankBytes.U
  def nextFetch(addr: UInt) = {
    if (nBanks == 1) {
      bankAlign(addr) + bankBytes.U
    } else {
      require(nBanks == 2)
      bankAlign(addr) + Mux(mayNotBeDualBanked(addr), bankBytes.U, fetchBytes.U)
    }
  }

  def fetchMask(addr: UInt) = {
    val idx = addr.extract(log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))
    if (nBanks == 1) {
      ((1 << fetchWidth)-1).U << idx
    } else {
      val shamt = idx.extract(log2Ceil(fetchWidth)-2, 0)
      val end_mask = Mux(mayNotBeDualBanked(addr), Fill(fetchWidth/2, 1.U), Fill(fetchWidth, 1.U))
      ((1 << fetchWidth)-1).U << shamt & end_mask
    }
  }
}


/**
 * Bundle passed into the FetchBuffer and used to combine multiple
 * relevant signals together.
 */
class FetchBundle(implicit p: Parameters) extends BoomBundle
{
  val pc            = UInt(vaddrBitsExtended.W)
  val edge_inst     = Bool() // True if 1st instruction in this bundle is pc - 2
  val insts         = Vec(fetchWidth, Bits(32.W))
  val exp_insts     = Vec(fetchWidth, Bits(32.W))

  val cfi_idx       = Valid(UInt(log2Ceil(fetchWidth).W))

  val ftq_idx       = UInt(log2Ceil(ftqSz).W)
  val mask          = Vec(fetchWidth, Bool()) // mark which words are valid instructions

  val br_mask       = Vec(fetchWidth, Bool())
  val jal_mask      = Vec(fetchWidth, Bool())

  val ghist         = new GlobalHistory

  val xcpt_pf_if    = Bool() // I-TLB miss (instruction fetch fault).
  val xcpt_ae_if    = Bool() // Access exception.

  val bp_debug_if_oh= Vec(fetchWidth, Bool())
  val bp_xcpt_if_oh = Vec(fetchWidth, Bool())


  // Source of the prediction for this bundle
  val debug_bsrc    = UInt(BSRC_SZ.W)
}



/**
 * IO for the BOOM Frontend to/from the CPU
 */
class BoomFrontendIO(implicit p: Parameters) extends BoomBundle
{
  // Give the backend a packet of instructions.
  val fetchpacket       = Flipped(new DecoupledIO(new FetchBufferResp))

  val get_pc            = Flipped(new GetPCFromFtqIO())

  // Breakpoint info
  val status            = Output(new MStatus)
  val bp                = Output(Vec(nBreakpoints, new BP))

  val sfence = Valid(new SFenceReq)

  val brupdate          = Output(new BrUpdateInfo)

  // Redirects change the PC
  val redirect_flush   = Output(Bool()) // Flush and hang the frontend?
  val redirect_val     = Output(Bool()) // Redirect the frontend?
  val redirect_pc      = Output(UInt()) // Where do we redirect to?
  val redirect_ftq_idx = Output(UInt()) // Which ftq entry should we reset to?
  val redirect_ghist   = Output(new GlobalHistory) // What are we setting as the global history?
  val redirect_flush_ghist = Output(Bool()) // Do we reset the ghist in the FTQ?

  val commit = Valid(UInt(ftqSz.W))

  val flush_icache = Output(Bool())
}

/**
 * Top level Frontend class
 *
 * @param icacheParams parameters for the icache
 * @param hartid id for the hardware thread of the core
 */
class BoomFrontend(val icacheParams: ICacheParams, hartid: Int)(implicit p: Parameters) extends LazyModule
{
  lazy val module = new BoomFrontendModule(this)
  val icache = LazyModule(new boom.ifu.ICache(icacheParams, hartid))
  val masterNode = icache.masterNode
}

/**
 * Bundle wrapping the IO for the Frontend as a whole
 *
 * @param outer top level Frontend class
 */
class BoomFrontendBundle(val outer: BoomFrontend) extends CoreBundle()(outer.p)
  with HasExternallyDrivenTileConstants
{
  val cpu = Flipped(new BoomFrontendIO())
  val ptw = new TLBPTWIO()
  val errors = new ICacheErrors
}

/**
 * Main Frontend module that connects the icache, TLB, fetch controller,
 * and branch prediction pipeline together.
 *
 * @param outer top level Frontend class
 */
class BoomFrontendModule(outer: BoomFrontend) extends LazyModuleImp(outer)
  with HasBoomCoreParameters
  with HasBoomFrontendParameters
{
  val io = IO(new BoomFrontendBundle(outer))
  implicit val edge = outer.masterNode.edges.out(0)
  require(fetchWidth*coreInstBytes == outer.icacheParams.fetchBytes)

  val bpd = Module(new BranchPredictor)

  val icache = outer.icache.module
  icache.io.hartid     := io.hartid
  icache.io.invalidate := io.cpu.flush_icache
  val tlb = Module(new TLB(true, log2Ceil(fetchBytes), TLBConfig(nTLBEntries)))
  io.ptw <> tlb.io.ptw

  // --------------------------------------------------------
  // **** NextPC Select (F0) ****
  //      Send request to ICache
  // --------------------------------------------------------

  val s0_vpc       = WireInit(0.U(vaddrBitsExtended.W))
  val s0_ghist     = WireInit((0.U).asTypeOf(new GlobalHistory))
  val s0_valid     = WireInit(false.B)
  val s0_is_replay = WireInit(false.B)
  val s0_is_sfence = WireInit(false.B)
  val s0_replay_resp = Wire(new TLBResp)
  val s0_replay_bpd_resp = Wire(new BranchPredictionBundle)
  val s0_replay_ppc  = Wire(UInt())




  when (RegNext(reset.asBool) && !reset.asBool) {
    s0_valid   := true.B
    s0_vpc     := io.reset_vector
    s0_ghist   := (0.U).asTypeOf(new GlobalHistory)
  }

  icache.io.req.valid     := s0_valid
  icache.io.req.bits.addr := s0_vpc

  bpd.io.f0_req.valid      := s0_valid && !s0_is_replay
  bpd.io.f0_req.bits.pc    := s0_vpc
  bpd.io.f0_req.bits.ghist := s0_ghist

  // --------------------------------------------------------
  // **** ICache Access (F1) ****
  //      Translate VPC
  // --------------------------------------------------------
  val s1_vpc       = RegNext(s0_vpc)
  val s1_valid     = RegNext(s0_valid, false.B)
  val s1_ghist     = RegNext(s0_ghist)
  val s1_is_replay = RegNext(s0_is_replay)
  val s1_is_sfence = RegNext(s0_is_sfence)
  val f1_clear     = WireInit(false.B)

  tlb.io.req.valid      := (s1_valid && !s1_is_replay && !f1_clear) || s1_is_sfence
  tlb.io.req.bits.cmd   := DontCare
  tlb.io.req.bits.vaddr := s1_vpc
  tlb.io.req.bits.passthrough := false.B
  tlb.io.req.bits.size  := log2Ceil(coreInstBytes * fetchWidth).U
  tlb.io.sfence         := RegNext(io.cpu.sfence)
  tlb.io.kill           := false.B

  val s1_tlb_miss = !s1_is_replay && tlb.io.resp.miss
  val s1_tlb_resp = Mux(s1_is_replay, RegNext(s0_replay_resp), tlb.io.resp)
  val s1_ppc  = Mux(s1_is_replay, RegNext(s0_replay_ppc), tlb.io.resp.paddr)
  val s1_bpd_resp = Mux(s1_is_replay, RegNext(s0_replay_bpd_resp), bpd.io.f1_resp)

  icache.io.s1_paddr := s1_ppc
  icache.io.s1_kill  := tlb.io.resp.miss || f1_clear

  val f1_mask = fetchMask(s1_vpc)
  val f1_redirects = (0 until fetchWidth) map { i =>
    f1_mask(i) && s1_bpd_resp.preds(i).predicted_pc.valid &&
    (s1_bpd_resp.preds(i).is_jal ||
      (s1_bpd_resp.preds(i).is_br && s1_bpd_resp.preds(i).taken))
  }
  val f1_redirect_idx = PriorityEncoder(f1_redirects)
  val f1_targs = s1_bpd_resp.preds.map(_.predicted_pc.bits)
  val f1_predicted_target = Mux(f1_redirects.reduce(_||_),
      f1_targs(f1_redirect_idx),
      nextFetch(s1_vpc))

  val f1_predicted_ghist = history_update(s1_ghist,
    s1_bpd_resp.preds.map(_.is_br).asUInt,
    s1_bpd_resp.preds(f1_redirect_idx).taken && s1_bpd_resp.preds(f1_redirect_idx).is_br,
    f1_redirect_idx,
    f1_redirects(f1_redirect_idx),
    s1_vpc)

  when (s1_valid && !s1_tlb_miss) {
    // Stop fetching on fault
    s0_valid     := !(s1_tlb_resp.ae.inst || s1_tlb_resp.pf.inst)

    s0_vpc       := f1_predicted_target
    s0_ghist     := f1_predicted_ghist
    s0_is_replay := false.B
  }

  // --------------------------------------------------------
  // **** ICache Response (F2) ****
  // --------------------------------------------------------

  val s2_valid = RegNext(s1_valid && !f1_clear, false.B)
  val s2_vpc   = RegNext(s1_vpc)
  val s2_ghist = Reg(new GlobalHistory)
  s2_ghist := s1_ghist
  val s2_ppc  = RegNext(s1_ppc)
  val s2_bsrc = WireInit(BSRC_1)
  val f2_clear = WireInit(false.B)
  val s2_tlb_resp = RegNext(s1_tlb_resp)
  val s2_is_replay = RegNext(s1_is_replay) && s2_valid
  val s2_xcpt = s2_valid && (s2_tlb_resp.ae.inst || s2_tlb_resp.pf.inst) && !s2_is_replay
  val f3_ready = Wire(Bool())

  icache.io.s2_kill := s2_xcpt

  val f2_bpd_resp = Mux(s2_is_replay, RegNext(s1_bpd_resp), bpd.io.f2_resp)
  val f2_mask = fetchMask(s2_vpc)
  val f2_redirects = (0 until fetchWidth) map { i =>
    f2_mask(i) && f2_bpd_resp.preds(i).predicted_pc.valid &&
    (f2_bpd_resp.preds(i).is_jal ||
      (f2_bpd_resp.preds(i).is_br && f2_bpd_resp.preds(i).taken))
  }
  val f2_redirect_idx = PriorityEncoder(f2_redirects)
  val f2_targs = f2_bpd_resp.preds.map(_.predicted_pc.bits)
  val f2_predicted_target = Mux(f2_redirects.reduce(_||_),
      f2_targs(f2_redirect_idx),
      nextFetch(s2_vpc))
  val f2_predicted_ghist = history_update(
    s2_ghist,
    f2_bpd_resp.preds.map(_.is_br).asUInt,
    f2_bpd_resp.preds(f2_redirect_idx).is_br && f2_bpd_resp.preds(f2_redirect_idx).taken,
    f2_redirect_idx,
    f2_redirects(f2_redirect_idx),
    s2_vpc)



  when ((s2_valid && !icache.io.resp.valid) ||
        (s2_valid && icache.io.resp.valid && !f3_ready)) {
    s0_valid := !((s2_tlb_resp.ae.inst || s2_tlb_resp.pf.inst) && !s2_is_replay)
    s0_vpc   := s2_vpc
    s0_is_replay := s2_valid && icache.io.resp.valid

    s0_ghist := s2_ghist

    f1_clear := true.B
  } .elsewhen (s2_valid && f3_ready) {
    when (s1_valid && s1_vpc === f2_predicted_target) {
      // We trust our prediction of what the global history for the next branch should be
      s2_ghist := f2_predicted_ghist
    }
    when ((s1_valid && s1_vpc =/= f2_predicted_target) || !s1_valid) {
      f1_clear := true.B

      s0_valid     := !((s2_tlb_resp.ae.inst || s2_tlb_resp.pf.inst) && !s2_is_replay)
      s0_vpc       := f2_predicted_target
      s0_is_replay := false.B
      s0_ghist     := f2_predicted_ghist
      s2_bsrc      := BSRC_2
    }
  }
  s0_replay_bpd_resp := f2_bpd_resp
  s0_replay_resp := s2_tlb_resp
  s0_replay_ppc  := s2_ppc

  // --------------------------------------------------------
  // **** F3 ****
  // --------------------------------------------------------
  val f3_clear = WireInit(false.B)
  val f3 = withReset(reset.toBool || f3_clear) {
    Module(new Queue(new FrontendResp, 1, pipe=true, flow=false)) }

  // Queue up the bpd resp as well, incase f4 backpressures f3
  // This is "flow" because the response (enq) arrives in f3, not f2
  val f3_bpd_resp = withReset(reset.toBool || f3_clear) {
    Module(new Queue(new BranchPredictionBundle, 1, pipe=true, flow=true)) }



  val f4_ready = Wire(Bool())
  f3_ready := f3.io.enq.ready
  f3.io.enq.valid   := s2_valid && icache.io.resp.valid && !f2_clear
  f3.io.enq.bits.pc := s2_vpc
  f3.io.enq.bits.data  := Mux(s2_xcpt, 0.U, icache.io.resp.bits.data)
  f3.io.enq.bits.ghist := s2_ghist
  f3.io.enq.bits.mask := fetchMask(s2_vpc)
  f3.io.enq.bits.xcpt := s2_tlb_resp
  f3.io.enq.bits.debug_bsrc := s2_bsrc

  // The BPD resp comes in f3
  f3_bpd_resp.io.enq.valid := f3.io.deq.valid && RegNext(f3.io.enq.ready)
  f3_bpd_resp.io.enq.bits  := Mux(RegNext(s2_is_replay), RegNext(f2_bpd_resp), bpd.io.f3_resp)

  f3.io.deq.ready := f4_ready
  f3_bpd_resp.io.deq.ready := f4_ready


  val f3_imemresp     = f3.io.deq.bits
  val f3_data         = f3_imemresp.data
  val f3_aligned_pc   = bankAlign(f3_imemresp.pc)

  val f3_redirects    = Wire(Vec(fetchWidth, Bool()))
  val f3_targs        = Wire(Vec(fetchWidth, UInt(vaddrBitsExtended.W)))

  val f3_fetch_bundle = Wire(new FetchBundle)
  f3_fetch_bundle.pc := f3_imemresp.pc
  f3_fetch_bundle.ftq_idx := 0.U // This gets assigned later
  f3_fetch_bundle.xcpt_pf_if := f3_imemresp.xcpt.pf.inst
  f3_fetch_bundle.xcpt_ae_if := f3_imemresp.xcpt.ae.inst
  f3_fetch_bundle.debug_bsrc := f3_imemresp.debug_bsrc

  // Tracks trailing 16b of previous fetch packet
  val f3_prev_half    = Reg(UInt(coreInstBits.W))
  // Tracks if last fetchpacket contained a half-inst
  val f3_prev_is_half = RegInit(false.B)

  require(fetchWidth >= 4) // Logic gets kind of annoying with fetchWidth = 2
  var redirect_found = false.B
  for (i <- 0 until fetchWidth) {
    val bpd_decoder = Module(new BranchDecode)
    val is_valid = Wire(Bool())
    val inst = Wire(UInt((2*coreInstBits).W))
    if (i == 0) {
      when (f3_prev_is_half) {
        inst := Cat(f3_data(15,0), f3_prev_half)
        f3_fetch_bundle.edge_inst := true.B
      } .otherwise {
        inst := f3_data(31,0)
        f3_fetch_bundle.edge_inst := false.B
      }
      is_valid := true.B
    } else if (i == 1) {
      // Need special case since 0th instruction may carry over the wrap around
      inst     := f3_data(i*coreInstBits+2*coreInstBits-1,i*coreInstBits)
      is_valid := f3_prev_is_half || !(f3_fetch_bundle.mask(i-1) && f3_fetch_bundle.insts(i-1)(1,0) === 3.U)
    } else if ((nBanks == 2) && i == (fetchWidth / 2) - 1) {
      // If we are using a banked I$ we could get cut-off halfway through the fetch bundle
      inst     := f3_data(i*coreInstBits+2*coreInstBits-1,i*coreInstBits)
      is_valid := !(f3_fetch_bundle.mask(i-1) && f3_fetch_bundle.insts(i-1)(1,0) === 3.U) &&
                  !(inst(1,0) === 3.U && !f3_imemresp.mask(i+1))
    } else if (i == fetchWidth - 1) {
      inst     := Cat(0.U(16.W), f3_data(fetchWidth*coreInstBits-1,i*coreInstBits))
      is_valid := !((f3_fetch_bundle.mask(i-1) && f3_fetch_bundle.insts(i-1)(1,0) === 3.U) ||
                    inst(1,0) === 3.U)
    } else {
      inst     := f3_data(i*coreInstBits+2*coreInstBits-1,i*coreInstBits)
      is_valid := !(f3_fetch_bundle.mask(i-1) && f3_fetch_bundle.insts(i-1)(1,0) === 3.U)
    }
    f3_fetch_bundle.insts(i) := inst

    // TODO do not compute a vector of targets
    val pc = (f3_aligned_pc
            + (i << log2Ceil(coreInstBytes)).U
            - Mux(f3_prev_is_half && (i == 0).B, 2.U, 0.U))

    val bpu = Module(new BreakpointUnit(nBreakpoints))
    bpu.io.status := io.cpu.status
    bpu.io.bp     := io.cpu.bp
    bpu.io.pc     := pc
    bpu.io.ea     := DontCare

    val exp_inst = ExpandRVC(inst)

    bpd_decoder.io.inst := exp_inst
    bpd_decoder.io.pc   := pc

    f3_fetch_bundle.exp_insts(i) := exp_inst

    f3_fetch_bundle.mask(i) := f3.io.deq.valid && f3_imemresp.mask(i) && is_valid && !redirect_found
    f3_targs(i)             := Mux(bpd_decoder.io.is_jalr, f3_bpd_resp.io.deq.bits.preds(i).predicted_pc.bits, bpd_decoder.io.target)

    f3_redirects(i)    := f3_fetch_bundle.mask(i) && (
      bpd_decoder.io.is_jal ||
        (bpd_decoder.io.is_br && f3_bpd_resp.io.deq.bits.preds(i).taken))


    f3_fetch_bundle.br_mask(i)  := f3_fetch_bundle.mask(i) && bpd_decoder.io.is_br
    f3_fetch_bundle.jal_mask(i) := f3_fetch_bundle.mask(i) && (bpd_decoder.io.is_jal || bpd_decoder.io.is_jalr)

    redirect_found = redirect_found || f3_redirects(i)

    f3_fetch_bundle.bp_debug_if_oh(i) := bpu.io.debug_if
    f3_fetch_bundle.bp_xcpt_if_oh (i) := bpu.io.xcpt_if
  }

  f3_fetch_bundle.ghist := f3.io.deq.bits.ghist

  when (f3.io.deq.fire()) {
    val last_idx = if (nBanks == 2) {
      Mux(mayNotBeDualBanked(f3_fetch_bundle.pc), (fetchWidth/2-1).U, (fetchWidth-1).U)
    } else {
      (fetchWidth-1).U
    }
    f3_prev_is_half := (!(f3_fetch_bundle.mask(last_idx-1.U) && f3_fetch_bundle.insts(last_idx-1.U)(1,0) === 3.U)
                     && f3_fetch_bundle.insts(last_idx)(1,0) === 3.U)
    f3_prev_half    := f3_fetch_bundle.insts(last_idx)(15,0)
    assert(f3_bpd_resp.io.deq.bits.pc === f3_fetch_bundle.pc)
  }

  when (f3_clear) {
    f3_prev_is_half := false.B
  }

  f3_fetch_bundle.cfi_idx.valid := f3_redirects.reduce(_||_)
  f3_fetch_bundle.cfi_idx.bits  := PriorityEncoder(f3_redirects)

  // Redirect earlier stages only if the later stage
  // can consume this packet
  val f3_predicted_target = Mux(f3_redirects.reduce(_||_), f3_targs(PriorityEncoder(f3_redirects)),
    nextFetch(f3_fetch_bundle.pc))
  val f3_predicted_ghist = history_update(
    f3_fetch_bundle.ghist,
    f3_fetch_bundle.br_mask.asUInt,
    f3_fetch_bundle.cfi_idx.valid && f3_fetch_bundle.br_mask(f3_fetch_bundle.cfi_idx.bits),
    f3_fetch_bundle.cfi_idx.bits,
    f3_fetch_bundle.cfi_idx.valid,
    f3_fetch_bundle.pc)

  when (f3.io.deq.valid && f4_ready) {

    when (f3_redirects.reduce(_||_)) {
      f3_prev_is_half := false.B
    }
    when (s2_valid && s2_vpc === f3_predicted_target) {
      f3.io.enq.bits.ghist := f3_predicted_ghist
    } .elsewhen (!s2_valid && s1_valid && s1_vpc === f3_predicted_target) {
      s2_ghist := f3_predicted_ghist
    } .elsewhen (( s2_valid &&  s2_vpc =/= f3_predicted_target)             ||
          (!s2_valid &&  s1_valid && s1_vpc =/= f3_predicted_target) ||
          (!s2_valid && !s1_valid)) {
      f2_clear := true.B
      f1_clear := true.B

      s0_valid     := !(f3_fetch_bundle.xcpt_pf_if || f3_fetch_bundle.xcpt_ae_if)
      s0_vpc       := f3_predicted_target
      s0_is_replay := false.B
      s0_ghist     := f3_predicted_ghist

      f3_fetch_bundle.debug_bsrc := BSRC_3
    }
  }

  // -------------------------------------------------------
  // **** F4 ****
  // -------------------------------------------------------
  val f4_clear = WireInit(false.B)
  val f4 = withReset(reset.toBool || f4_clear) {
    Module(new Queue(new FetchBundle, 1, pipe=true, flow=false))}

  val fb  = Module(new FetchBuffer(numEntries=numFetchBufferEntries))
  val ftq = Module(new FetchTargetQueue(num_entries=ftqSz))

  f4_ready := f4.io.enq.ready
  f4.io.enq.valid := f3.io.deq.valid && !f3_clear
  f4.io.enq.bits  := f3_fetch_bundle
  f4.io.deq.ready := fb.io.enq.ready && ftq.io.enq.ready

  fb.io.enq.valid := f4.io.deq.valid && ftq.io.enq.ready
  fb.io.enq.bits  := f4.io.deq.bits
  fb.io.enq.bits.ftq_idx := ftq.io.enq_idx

  ftq.io.enq.valid          := f4.io.deq.valid && fb.io.enq.ready
  ftq.io.enq.bits           := f4.io.deq.bits

  bpd.io.update := ftq.io.bpdupdate


  // -------------------------------------------------------
  // **** To Core (F5) ****
  // -------------------------------------------------------

  io.cpu.fetchpacket <> fb.io.deq
  io.cpu.get_pc <> ftq.io.get_ftq_pc
  ftq.io.deq := io.cpu.commit
  ftq.io.brupdate := io.cpu.brupdate

  ftq.io.redirect.valid   := io.cpu.redirect_val
  ftq.io.redirect.bits    := io.cpu.redirect_ftq_idx
  ftq.io.redirect_flush_ghist := io.cpu.redirect_flush_ghist
  fb.io.clear := false.B

  when (io.cpu.sfence.valid) {
    fb.io.clear := true.B
    f4_clear    := true.B
    f3_clear    := true.B
    f2_clear    := true.B
    f1_clear    := true.B

    s0_valid     := false.B
    s0_vpc       := io.cpu.sfence.bits.addr
    s0_is_replay := false.B
    s0_is_sfence := true.B

  }.elsewhen (io.cpu.redirect_flush) {
    fb.io.clear := true.B
    f4_clear    := true.B
    f3_clear    := true.B
    f2_clear    := true.B
    f1_clear    := true.B

    s0_valid     := io.cpu.redirect_val
    s0_vpc       := io.cpu.redirect_pc
    s0_ghist     := io.cpu.redirect_ghist
    s0_is_replay := false.B

    ftq.io.redirect.valid := true.B
    ftq.io.redirect.bits  := io.cpu.redirect_ftq_idx
  }



  override def toString: String =
    (BoomCoreStringPrefix("====Overall Frontend Params====") + "\n"
    + icache.toString)
}
