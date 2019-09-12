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
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{ICacheLogicalTreeNode}

import boom.common._
import boom.exu.{BranchUnitResp, CommitExceptionSignals, BranchDecode}
import boom.util.{BoomCoreStringPrefix}


class FrontendResp(implicit p: Parameters) extends BoomBundle()(p) {
  val pc = UInt(vaddrBitsExtended.W)  // ID stage PC
  val data = UInt((fetchWidth * coreInstBits).W)
  val mask = UInt(fetchWidth.W)
  val xcpt = new FrontendExceptions
}

/**
 * Parameters to manage a L1 Banked ICache
 */
trait HasL1ICacheBankedParameters extends HasL1ICacheParameters
{
  // Use a bank interleaved I$ if our fetch width is wide enough.
  val icIsBanked = fetchBytes > 8
  // How many bytes wide is a bank?
  val bankBytes = if (icIsBanked) fetchBytes/2 else fetchBytes
  // How many "chunks"/interleavings make up a cache line?
  def numChunks = cacheParams.blockBytes / bankBytes

  // Which bank is the address pointing to?
  def bank(addr: UInt) = addr(log2Ceil(bankBytes))
  def inLastChunk(addr: UInt) = addr(blockOffBits-1, log2Ceil(bankBytes)) === (numChunks-1).U

  // Round address down to the nearest fetch boundary.
  def alignToFetchBoundary(addr: UInt) = {
    if (icIsBanked) ~(~addr | (bankBytes.U-1.U))
    else ~(~addr | (fetchBytes.U-1.U))
  }

  // Input: an ALIGNED pc.
  // For the fall-through next PC, where does the next fetch pc start?
  // This is complicated by cache-line wraparound.
  def nextFetchStart(addr: UInt) = {
    if (icIsBanked) {
      addr + Mux(inLastChunk(addr), bankBytes.U, fetchBytes.U)
    } else {
      addr + fetchBytes.U
    }
  }

  // For a given fetch address, what is the mask of validly fetched instructions.
  def fetchMask(addr: UInt) = {
    // where is the first instruction, aligned to a log(fetchWidth) boundary?
    val idx = addr.extract(log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))
    if (icIsBanked) {
      // shave off the msb of idx since we are aligned to half-fetchWidth boundaries.
      val shamt = idx.extract(log2Ceil(fetchWidth)-2, 0)
      val end_mask = Mux(inLastChunk(addr), Fill(fetchWidth/2, 1.U), Fill(fetchWidth, 1.U))
      ((1 << fetchWidth)-1).U << shamt & end_mask
    } else {
      ((1 << fetchWidth)-1).U << idx
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
  val ftq_idx       = UInt(log2Ceil(ftqSz).W)
  val mask          = Vec(fetchWidth, Bool()) // mark which words are valid instructions
  val xcpt_pf_if    = Bool() // I-TLB miss (instruction fetch fault).
  val xcpt_ae_if    = Bool() // Access exception.
  val xcpt_ma_if_oh = Vec(fetchWidth, Bool())
                           // A cfi branched to a misaligned address --
                           // one-hot encoding (1:1 with insts).
  val bp_debug_if_oh= Vec(fetchWidth, Bool())
  val bp_xcpt_if_oh = Vec(fetchWidth, Bool())
}


/**
 * IO for the BOOM Frontend to/from the CPU
 */
class BoomFrontendIO(implicit p: Parameters) extends BoomBundle
{
  // Give the backend a packet of instructions.
  val fetchpacket       = Flipped(new DecoupledIO(new FetchBufferResp))

  val br_unit           = Output(new BranchUnitResp())
  val get_pc            = Flipped(new GetPCFromFtqIO())

  // Breakpoint info
  val status            = Output(new MStatus)
  val bp                = Output(Vec(nBreakpoints, new BP))

  val sfence = Valid(new SFenceReq)

  // Redirects change the PC
  val redirect_val     = Output(Bool())
  val redirect_pc      = Output(UInt())
  val redirect_ftq_idx = Output(UInt())

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
  with HasL1ICacheParameters
  with HasL1ICacheBankedParameters
{
  val io = IO(new BoomFrontendBundle(outer))
  dontTouch(io)
  implicit val edge = outer.masterNode.edges.out(0)
  require(fetchWidth*coreInstBytes == outer.icacheParams.fetchBytes)

  val icache = outer.icache.module
  dontTouch(icache.io)
  icache.io.hartid     := io.hartid
  icache.io.invalidate := io.cpu.flush_icache
  val tlb = Module(new TLB(true, log2Ceil(fetchBytes), TLBConfig(nTLBEntries)))
  io.ptw <> tlb.io.ptw

  // --------------------------------------------------------
  // **** NextPC Select (F0) ****
  //      Send request to ICache
  // --------------------------------------------------------

  val s0_vpc       = WireInit(0.U(vaddrBitsExtended.W))
  val s0_valid     = WireInit(false.B)
  val s0_is_replay = WireInit(false.B)
  val s0_is_sfence = WireInit(false.B)
  val s0_replay_resp = Wire(new TLBResp)
  val s0_replay_ppc  = Wire(UInt())

  when (RegNext(reset.asBool) && !reset.asBool) {
    s0_valid := true.B
    s0_vpc   := io.reset_vector
  }

  icache.io.req.valid     := s0_valid
  icache.io.req.bits.addr := s0_vpc

  // --------------------------------------------------------
  // **** ICache Access (F1) ****
  //      Translate VPC
  // --------------------------------------------------------
  val s1_vpc       = RegNext(s0_vpc)
  val s1_valid     = RegNext(s0_valid, false.B)
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

  icache.io.s1_paddr := s1_ppc
  icache.io.s1_kill  := tlb.io.resp.miss || f1_clear

  when (s1_valid && !s1_tlb_miss) {
    s0_valid := true.B
    s0_vpc   := nextFetchStart(alignToFetchBoundary(s1_vpc))
  }

  // --------------------------------------------------------
  // **** ICache Response (F2) ****
  // --------------------------------------------------------

  val s2_valid = RegNext(s1_valid && !f1_clear, false.B)
  val s2_vpc = RegNext(s1_vpc)
  val s2_ppc = RegNext(s1_ppc)
  val f2_clear = WireInit(false.B)
  val s2_tlb_resp = RegNext(s1_tlb_resp)
  val s2_is_replay = RegNext(s1_is_replay) && s2_valid
  val s2_xcpt = s2_valid && (s2_tlb_resp.ae.inst || s2_tlb_resp.pf.inst) && !s2_is_replay
  val f3_ready = Wire(Bool())

  icache.io.s2_kill := s2_xcpt

  when ((s2_valid && !icache.io.resp.valid) ||
        (s2_valid && icache.io.resp.valid && !f3_ready)) {
    s0_valid := true.B
    s0_vpc   := s2_vpc
    s0_is_replay := s2_valid && icache.io.resp.valid

    f1_clear := true.B
  }
  s0_replay_resp := s2_tlb_resp
  s0_replay_ppc  := s2_ppc

  // --------------------------------------------------------
  // **** F3 ****
  // --------------------------------------------------------
  val f3_clear = WireInit(false.B)
  val f3 = withReset(reset.toBool || f3_clear) {
    Module(new Queue(new FrontendResp, 1, pipe=true, flow=false)) }
  val f4_ready = Wire(Bool())
  dontTouch(f3.io)
  f3_ready := f3.io.enq.ready
  f3.io.enq.valid   := s2_valid && icache.io.resp.valid && !f2_clear
  f3.io.enq.bits.pc := s2_vpc
  f3.io.enq.bits.data := Mux(s2_xcpt, 0.U, icache.io.resp.bits.data)
  f3.io.enq.bits.mask := fetchMask(s2_vpc)
  f3.io.enq.bits.xcpt := s2_tlb_resp

  f3.io.deq.ready := f4_ready
  val f3_imemresp     = f3.io.deq.bits
  val f3_data         = f3_imemresp.data
  val f3_aligned_pc   = alignToFetchBoundary(f3_imemresp.pc)
  val f3_targs        = Wire(Vec(fetchWidth, UInt()))

  val f3_fetch_bundle = Wire(new FetchBundle)
  f3_fetch_bundle.pc := f3_imemresp.pc
  f3_fetch_bundle.ftq_idx := 0.U // This gets assigned later
  f3_fetch_bundle.xcpt_pf_if := f3_imemresp.xcpt.pf.inst
  f3_fetch_bundle.xcpt_ae_if := f3_imemresp.xcpt.ae.inst

  // Tracks trailing 16b of previous fetch packet
  val f3_prev_half    = Reg(UInt(coreInstBits.W))
  // Tracks if last fetchpacket contained a half-inst
  val f3_prev_is_half = RegInit(false.B)

  assert(fetchWidth >= 4 || !usingCompressed) // Logic gets kind of annoying with fetchWidth = 2
  for (i <- 0 until fetchWidth) {
    val bpd_decoder = Module(new BranchDecode)
    val is_valid = Wire(Bool())
    val inst = Wire(UInt((2*coreInstBits).W))
    if (!usingCompressed) {
      is_valid := true.B
      inst     := f3_data(i*coreInstBits+coreInstBits-1,i*coreInstBits)
      f3_fetch_bundle.edge_inst := false.B
    } else if (i == 0) {
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
    } else if (icIsBanked && i == (fetchWidth / 2) - 1) {
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

    f3_fetch_bundle.mask(i) := f3.io.deq.valid && f3_imemresp.mask(i) && is_valid
    f3_targs(i)        := bpd_decoder.io.target
    f3_fetch_bundle.xcpt_ma_if_oh(i) := (f3_targs(i)(1) && bpd_decoder.io.is_jal
                        && f3_imemresp.mask(i) && !usingCompressed.B)

    f3_fetch_bundle.bp_debug_if_oh(i) := bpu.io.debug_if
    f3_fetch_bundle.bp_xcpt_if_oh (i) := bpu.io.xcpt_if
  }

  when (f3.io.deq.fire()) {
    val last_idx  = Mux(inLastChunk(f3_fetch_bundle.pc) && icIsBanked.B,
                      (fetchWidth/2-1).U, (fetchWidth-1).U)
    f3_prev_is_half := (usingCompressed.B
                     && !(f3_fetch_bundle.mask(last_idx-1.U) && f3_fetch_bundle.insts(last_idx-1.U)(1,0) === 3.U)
                     && f3_fetch_bundle.insts(last_idx)(1,0) === 3.U)
    f3_prev_half    := f3_fetch_bundle.insts(last_idx)(15,0)
  }

  when (f3_clear) {
    f3_prev_is_half := false.B
  }


  // -------------------------------------------------------
  // **** F4 ****
  // -------------------------------------------------------
  val f4_clear = WireInit(false.B)
  val f4 = withReset(reset.toBool || f4_clear) {
    Module(new Queue(new FetchBundle, 1, pipe=true, flow=false))}
  dontTouch(f4.io)
  val fb  = Module(new FetchBuffer(numEntries=numFetchBufferEntries))
  val ftq = Module(new FetchTargetQueue(num_entries=ftqSz))
  dontTouch(fb.io)
  dontTouch(ftq.io)

  f4_ready := f4.io.enq.ready
  f4.io.enq.valid := f3.io.deq.valid && !f3_clear
  f4.io.enq.bits  := f3_fetch_bundle
  f4.io.deq.ready := fb.io.enq.ready && ftq.io.enq.ready

  fb.io.enq.valid := f4.io.deq.valid && ftq.io.enq.ready
  fb.io.enq.bits  := f4.io.deq.bits
  fb.io.enq.bits.ftq_idx := ftq.io.enq_idx

  ftq.io.enq.valid         := f4.io.deq.valid && fb.io.enq.ready
  ftq.io.enq.bits.fetch_pc := f4.io.deq.bits.pc

  // -------------------------------------------------------
  // **** To Core (F5) ****
  // -------------------------------------------------------

  io.cpu.fetchpacket <> fb.io.deq
  io.cpu.get_pc <> ftq.io.get_ftq_pc
  ftq.io.deq := io.cpu.commit


  ftq.io.redirect.valid := io.cpu.redirect_val
  ftq.io.redirect.bits  := io.cpu.redirect_ftq_idx
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

  }.elsewhen (io.cpu.redirect_val) {
    fb.io.clear := true.B
    f4_clear    := true.B
    f3_clear    := true.B
    f2_clear    := true.B
    f1_clear    := true.B

    s0_valid     := true.B
    s0_vpc       := io.cpu.redirect_pc
    s0_is_replay := false.B

    ftq.io.redirect.valid := true.B
    ftq.io.redirect.bits  := io.cpu.redirect_ftq_idx
  }

  fb.io.clear := io.cpu.redirect_val


  override def toString: String =
    (BoomCoreStringPrefix("====Overall Frontend Params====") + "\n"
    + icache.toString)
}


