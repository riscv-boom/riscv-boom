//******************************************************************************
// Ported from Rocket-Chip
// See LICENSE.Berkeley and LICENSE.SiFive in Rocket-Chip for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.lsu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._

import boom.common._
import boom.exu.BrResolutionInfo
import boom.util.{IsKilledByBranch, GetNewBrMask, BranchKillableQueue, IsOlder}

class BoomDCacheReqInternal(implicit p: Parameters) extends BoomDCacheReq()(p)
  with HasL1HellaCacheParameters
{
  // miss info
  val tag_match = Bool()
  val old_meta  = new L1Metadata
  val way_en    = UInt(nWays.W)

  // Used in the MSHRs
  val sdq_id    = UInt(log2Ceil(cfg.nSDQ).W)
}


class BoomMSHR(id: Int)(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
  with HasL1HellaCacheParameters
{
  val io = IO(new Bundle {
    val req_pri_val = Input(Bool())
    val req_pri_rdy = Output(Bool())
    val req_sec_val = Input(Bool())
    val req_sec_rdy = Output(Bool())

    val brinfo       = Input(new BrResolutionInfo)
    val rob_pnr_idx  = Input(UInt(robAddrSz.W))
    val rob_head_idx = Input(UInt(robAddrSz.W))

    val req         = Input(new BoomDCacheReqInternal)

    val idx_match   = Output(Bool())
    val way_match   = Output(Bool())
    val tag         = Output(UInt(tagBits.W))

    val mem_acquire = Decoupled(new TLBundleA(edge.bundle))

    val mem_grant   = Flipped(Valid(new TLBundleD(edge.bundle)))
    val mem_finish  = Decoupled(new TLBundleE(edge.bundle))

    val refill      = Decoupled(new L1DataWriteReq)

    val meta_write  = Decoupled(new L1MetaWriteReq)
    val wb_req      = Decoupled(new WritebackReq(edge.bundle))

    // Replays go through the cache pipeline again
    val replay      = Decoupled(new BoomDCacheReqInternal)
    // Resp go straight out to the core
    val resp        = Decoupled(new BoomDCacheResp)

    val probe_rdy   = Output(Bool())
  })

  dontTouch(io)

  // TODO: Optimize this. We don't want to mess with cache during speculation
  // s_refill_req      : Make a request for a new cache line
  // s_refill_resp     : Store the refill response into our buffer
  // s_drain_rpq_loads : Drain out loads from the rpq
  //                   : If miss was misspeculated, go to s_invalid
  // s_wb_req          : Write back the evicted cache line
  // s_wb_resp         : Finish writing back the evicted cache line
  // s_meta_write_req  : Write the metadata for new cache lne
  // s_meta_write_resp :

  val s_invalid :: s_refill_req :: s_refill_resp :: s_drain_rpq_loads :: s_wb_req :: s_wb_resp :: s_meta_clear :: s_commit_line :: s_drain_rpq :: s_meta_write_req :: s_meta_write_resp ::  Nil = Enum(11)
  val state = RegInit(s_invalid)

  val req     = Reg(new BoomDCacheReqInternal)
  val req_idx = req.addr(untagBits-1, blockOffBits)
  val req_tag = req.addr >> untagBits
  val req_block_addr = (req.addr >> blockOffBits) << blockOffBits
  val req_needs_wb = RegInit(false.B)
  val idx_match = req_idx === io.req.addr(untagBits-1, blockOffBits)
  val way_match = req.way_en === io.req.way_en

  val new_coh = RegInit(ClientMetadata.onReset)
  val (_, shrink_param, coh_on_clear) = req.old_meta.coh.onCacheControl(M_FLUSH)
  val grow_param = new_coh.onAccess(req.uop.mem_cmd)._2
  val coh_on_grant = new_coh.onGrant(req.uop.mem_cmd, io.mem_grant.bits.param)

  // We only accept secondary misses if we haven't yet sent an Acquire to outer memory
  // or if the Acquire that was sent will obtain a Grant with sufficient permissions
  // to let us replay this new request. I.e. we don't handle multiple outstanding
  // Acquires on the same block for now.
  val (cmd_requires_second_acquire, is_hit_again, _, dirtier_coh, dirtier_cmd) =
    new_coh.onSecondaryAccess(req.uop.mem_cmd, io.req.uop.mem_cmd)

  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)
  val sec_rdy = idx_match && !cmd_requires_second_acquire && !state.isOneOf(s_invalid, s_meta_write_req, s_meta_write_resp)// Always accept secondary misses

  val rpq = Module(new BranchKillableQueue(new BoomDCacheReqInternal, cfg.nRPQ))
  rpq.io.brinfo := io.brinfo
  rpq.io.flush  := false.B // we should never need to flush this?

  rpq.io.enq.valid := ((io.req_pri_val && io.req_pri_rdy) || (io.req_sec_val && io.req_sec_rdy)) && !isPrefetch(io.req.uop.mem_cmd)
  rpq.io.enq.bits  := io.req
  rpq.io.deq.ready := false.B


  val grantackq = Module(new Queue(new TLBundleE(edge.bundle), 1))
  val can_finish = state.isOneOf(s_invalid, s_refill_req)
  grantackq.io.enq.valid := refill_done && edge.isRequest(io.mem_grant.bits)
  grantackq.io.enq.bits  := edge.GrantAck(io.mem_grant.bits)
  io.mem_finish.valid    := grantackq.io.deq.valid && can_finish
  io.mem_finish.bits     := grantackq.io.deq.bits
  grantackq.io.deq.ready := io.mem_finish.ready && can_finish

  val load_buffer = Mem(cacheDataBeats,
                        UInt(encRowBits.W))
  val refill_ctr  = Reg(UInt(log2Ceil(cacheDataBeats).W))
  val commit_line = Reg(Bool())

  io.probe_rdy   := (state === s_invalid) || !idx_match
  io.idx_match   := (state =/= s_invalid) && idx_match
  io.way_match   := (state =/= s_invalid) && way_match
  io.tag         := req_tag
  io.meta_write.valid  := false.B
  io.req_pri_rdy       := false.B
  io.req_sec_rdy       := sec_rdy && rpq.io.enq.ready && (state =/= s_invalid)
  io.mem_acquire.valid := false.B
  io.refill.valid      := false.B
  io.replay.valid      := false.B
  io.wb_req.valid      := false.B
  io.resp.valid        := false.B

  when (state === s_invalid) {
    io.req_pri_rdy := true.B

    when (io.req_pri_val && io.req_pri_rdy) {
      assert(rpq.io.enq.ready)

      req := io.req
      val old_coh   = io.req.old_meta.coh
      req_needs_wb := old_coh.onCacheControl(M_FLUSH)._1 // does the line we are evicting need to be written back
      val (is_hit, _, coh_on_hit) = old_coh.onAccess(io.req.uop.mem_cmd)

      when (io.req.tag_match) {
        when (is_hit) { // set dirty bit
          assert(isWrite(io.req.uop.mem_cmd))
          new_coh := coh_on_hit
          state   := s_meta_write_req
        } .otherwise { // upgrade permissions
          new_coh := old_coh
          state   := s_refill_req
        }
      } .otherwise { // writeback if necessary and refill
        new_coh := ClientMetadata.onReset
        state   := s_refill_req
      }
    }
  }
  .elsewhen (state === s_refill_req) {
    io.mem_acquire.valid := true.B
    io.mem_acquire.bits  := edge.AcquireBlock(
      fromSource      = id.U,
      toAddress       = Cat(req_tag, req_idx) << blockOffBits,
      lgSize          = lgCacheBlockBytes.U,
      growPermissions = grow_param)._2
    when (io.mem_acquire.fire()) {
      state := s_refill_resp
    }
  } .elsewhen (state === s_refill_resp) {
    when (io.mem_grant.fire()) {
      load_buffer(refill_address_inc >> rowOffBits) := io.mem_grant.bits.data
    }
    when (refill_done) {
      state := s_drain_rpq_loads
      commit_line := false.B
      new_coh := coh_on_grant
    }
  } .elsewhen (state === s_drain_rpq_loads) {
    // TODO: This should not be a PNR check
    // val drain_load = (isRead(rpq.io.deq.bits.uop.mem_cmd) &&
    //                   (rpq.io.deq.bits.is_hella ||
    //                     IsOlder(rpq.io.deq.bits.uop.rob_idx, io.rob_pnr_idx, io.rob_head_idx)))
    val drain_load = isRead(rpq.io.deq.bits.uop.mem_cmd) // drain all loads for now
    val rp_addr = Cat(req_tag, req_idx, rpq.io.deq.bits.addr(blockOffBits-1,0))
    val loadgen = new LoadGen(rpq.io.deq.bits.uop.mem_size, rpq.io.deq.bits.uop.mem_signed,
      Cat(req_tag, req_idx, rpq.io.deq.bits.addr(blockOffBits-1,0)),
      load_buffer(rpq.io.deq.bits.addr >> rowOffBits), false.B, wordBytes)

    rpq.io.deq.ready  := io.resp.ready && drain_load
    io.resp.valid     := rpq.io.deq.valid && drain_load
    io.resp.bits.uop  := rpq.io.deq.bits.uop
    io.resp.bits.data := loadgen.data // TODO: Fix
    io.resp.bits.is_hella := rpq.io.deq.bits.is_hella
    when (rpq.io.deq.fire()) {
      commit_line := true.B
    }
      .elsewhen (rpq.io.empty && !commit_line)
    {
      state := s_invalid
    } .elsewhen (rpq.io.empty || (rpq.io.deq.valid && isWrite(rpq.io.deq.bits.uop.mem_cmd))) {
      state := Mux(req_needs_wb, s_wb_req, s_meta_clear)
    }
  } .elsewhen (state === s_wb_req) {
    io.wb_req.valid          := true.B
    io.wb_req.bits.tag       := req.old_meta.tag
    io.wb_req.bits.idx       := req_idx
    io.wb_req.bits.param     := shrink_param
    io.wb_req.bits.way_en    := req.way_en
    io.wb_req.bits.source    := id.U
    io.wb_req.bits.voluntary := true.B
    when (io.wb_req.fire()) {
      state := s_wb_resp
    }
  } .elsewhen (state === s_wb_resp) {
    when (io.mem_grant.fire()) {
      state := s_meta_clear
    }
  } .elsewhen (state === s_meta_clear) {
    io.meta_write.valid         := true.B
    io.meta_write.bits.idx      := req_idx
    io.meta_write.bits.data.coh := coh_on_clear
    io.meta_write.bits.data.tag := req_tag
    io.meta_write.bits.way_en   := req.way_en

    when (io.meta_write.fire()) {
      state      := s_commit_line
      refill_ctr := 0.U
    }
  } .elsewhen (state === s_commit_line) {
    io.refill.valid       := true.B
    io.refill.bits.addr   := req_block_addr | (refill_ctr << rowOffBits)
    io.refill.bits.way_en := req.way_en
    io.refill.bits.wmask  := ~(0.U(rowWords.W))
    io.refill.bits.data   := load_buffer(refill_ctr)
    when (io.refill.fire()) {
      refill_ctr := refill_ctr + 1.U
      when (refill_ctr === (cacheDataBeats - 1).U) {
        state := s_drain_rpq
      }
    }
  } .elsewhen (state === s_drain_rpq) {
    io.replay <> rpq.io.deq
    io.replay.bits.way_en    := req.way_en
    io.replay.bits.addr := Cat(req_tag, req_idx, rpq.io.deq.bits.addr(blockOffBits-1,0))
    when (rpq.io.empty) {
      state := s_meta_write_req
    }
  } .elsewhen (state === s_meta_write_req) {
    // Why do we write this meta before emptying the RPQ? Doesn't this allow
    // stores to go out-of-order?
    io.meta_write.valid         := true.B
    io.meta_write.bits.idx      := req_idx
    io.meta_write.bits.data.coh := new_coh
    io.meta_write.bits.data.tag := req_tag
    io.meta_write.bits.way_en   := req.way_en
    when (io.meta_write.fire()) {
      state := s_meta_write_resp
    }
  } .elsewhen (state === s_meta_write_resp) {
    // This wait state allows us to catch RAW hazards on the tags
    state := s_invalid
  }


}

class BoomIOMSHR(id: Int)(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
  with HasL1HellaCacheParameters
{
  val io = IO(new Bundle {
    val req  = Flipped(Decoupled(new BoomDCacheReq))
    val resp = Decoupled(new BoomDCacheResp)
    val mem_access = Decoupled(new TLBundleA(edge.bundle))
    val mem_ack    = Flipped(Valid(new TLBundleD(edge.bundle)))

    // We don't need brinfo in here because uncacheable operations are guaranteed non-speculative
  })
  dontTouch(io)

  def beatOffset(addr: UInt) = addr.extract(beatOffBits-1, wordOffBits)

  def wordFromBeat(addr: UInt, dat: UInt) = {
    val shift = Cat(beatOffset(addr), 0.U((wordOffBits+log2Ceil(wordBytes)).W))
    (dat >> shift)(wordBits-1, 0)
  }

  val req = Reg(new BoomDCacheReq)
  val grant_word = Reg(UInt(wordBits.W))

  val s_idle :: s_mem_access :: s_mem_ack :: s_resp :: Nil = Enum(4)

  val state = RegInit(s_idle)
  io.req.ready := state === s_idle

  val loadgen = new LoadGen(req.uop.mem_size, req.uop.mem_signed, req.addr, grant_word, false.B, wordBytes)

  val a_source  = id.U
  val a_address = req.addr
  val a_size    = req.uop.mem_size
  val a_data    = Fill(beatWords, req.data)

  val get      = edge.Get(a_source, a_address, a_size)._2
  val put      = edge.Put(a_source, a_address, a_size, a_data)._2
  val atomics  = if (edge.manager.anySupportLogical) {
    MuxLookup(req.uop.mem_cmd, (0.U).asTypeOf(new TLBundleA(edge.bundle)), Array(
      M_XA_SWAP -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.SWAP)._2,
      M_XA_XOR  -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.XOR) ._2,
      M_XA_OR   -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.OR)  ._2,
      M_XA_AND  -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.AND) ._2,
      M_XA_ADD  -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.ADD)._2,
      M_XA_MIN  -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MIN)._2,
      M_XA_MAX  -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MAX)._2,
      M_XA_MINU -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MINU)._2,
      M_XA_MAXU -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MAXU)._2))
  } else {
    // If no managers support atomics, assert fail if processor asks for them
    assert(state === s_idle || !isAMO(req.uop.mem_cmd))
    Wire(new TLBundleA(edge.bundle))
  }
  assert(state === s_idle || req.uop.mem_cmd =/= M_XSC)

  io.mem_access.valid := state === s_mem_access
  io.mem_access.bits  := Mux(isAMO(req.uop.mem_cmd), atomics, Mux(isRead(req.uop.mem_cmd), get, put))

  io.resp.valid     := state === s_resp
  io.resp.bits.uop  := req.uop
  io.resp.bits.data := loadgen.data

  when (io.req.fire()) {
    req   := io.req.bits
    state := s_mem_access
  }
  when (io.mem_access.fire()) {
    state := s_mem_ack
  }
  when (state === s_mem_ack && io.mem_ack.valid) {
    state := s_resp
    when (isRead(req.uop.mem_cmd)) {
      grant_word := wordFromBeat(req.addr, io.mem_ack.bits.data)
    }
  }
  when (io.resp.fire()) {
    state := s_idle
  }
}


class BoomMSHRFile(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
  with HasL1HellaCacheParameters
{
  val io = IO(new Bundle {
    val req  = Flipped(Decoupled(new BoomDCacheReqInternal))
    val resp = Decoupled(new BoomDCacheResp)
    val secondary_miss = Output(Bool())

    val brinfo       = Input(new BrResolutionInfo)
    val rob_pnr_idx  = Input(UInt(robAddrSz.W))
    val rob_head_idx = Input(UInt(robAddrSz.W))

    val mem_acquire  = Decoupled(new TLBundleA(edge.bundle))
    val mem_grant    = Flipped(Valid(new TLBundleD(edge.bundle)))
    val mem_finish   = Decoupled(new TLBundleE(edge.bundle))

    val refill     = Decoupled(new L1DataWriteReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val replay     = Decoupled(new BoomDCacheReqInternal)
    val wb_req     = Decoupled(new WritebackReq(edge.bundle))

    val fence_rdy = Output(Bool())
    val probe_rdy = Output(Bool())
  })
  dontTouch(io)

  val cacheable = edge.manager.supportsAcquireBFast(io.req.bits.addr, lgCacheBlockBytes.U)

  val sdq_val      = RegInit(0.U(cfg.nSDQ.W))
  val sdq_alloc_id = PriorityEncoder(~sdq_val(cfg.nSDQ-1,0))
  val sdq_rdy      = !sdq_val.andR
  val sdq_enq      = io.req.fire() && cacheable && isWrite(io.req.bits.uop.mem_cmd)
  val sdq          = Mem(cfg.nSDQ, UInt(coreDataBits.W))

  when (sdq_enq) {
    sdq(sdq_alloc_id) := io.req.bits.data
  }

  val idx_matches = Wire(Vec(cfg.nMSHRs, Bool()))
  val tag_list    = Wire(Vec(cfg.nMSHRs, UInt(tagBits.W)))
  val tag_match   = Mux1H(idx_matches, tag_list) === io.req.bits.addr >> untagBits

  val wb_tag_list = Wire(Vec(cfg.nMSHRs, UInt(tagBits.W)))

  val meta_write_arb = Module(new Arbiter(new L1MetaWriteReq           , cfg.nMSHRs))
  val wb_req_arb     = Module(new Arbiter(new WritebackReq(edge.bundle), cfg.nMSHRs))
  val replay_arb     = Module(new Arbiter(new BoomDCacheReqInternal    , cfg.nMSHRs))
  val alloc_arb      = Module(new Arbiter(    Bool()                   , cfg.nMSHRs))
  val resp_arb       = Module(new Arbiter(new BoomDCacheResp           , cfg.nMSHRs + nIOMSHRs))
  val refill_arb     = Module(new Arbiter(new L1DataWriteReq           , cfg.nMSHRs))

  var idx_match = false.B
  var way_match = false.B
  var pri_rdy   = false.B
  var sec_rdy   = false.B

  io.fence_rdy := true.B
  io.probe_rdy := true.B

  val mshrs = (0 until cfg.nMSHRs) map { i =>
    val mshr = Module(new BoomMSHR(i))

    idx_matches(i) := mshr.io.idx_match
    tag_list(i)    := mshr.io.tag
    wb_tag_list(i) := mshr.io.wb_req.bits.tag

    alloc_arb.io.in(i).valid := mshr.io.req_pri_rdy
    alloc_arb.io.in(i).bits  := DontCare // Hack to get an Arbiter with no data

    mshr.io.req_pri_val  := alloc_arb.io.in(i).ready
    mshr.io.req_sec_val  := io.req.valid && sdq_rdy && tag_match && cacheable
    mshr.io.req          := io.req.bits
    mshr.io.req.sdq_id   := sdq_alloc_id

    mshr.io.brinfo       := io.brinfo
    mshr.io.rob_pnr_idx  := io.rob_pnr_idx
    mshr.io.rob_head_idx := io.rob_head_idx

    meta_write_arb.io.in(i) <> mshr.io.meta_write
    wb_req_arb.io.in(i)     <> mshr.io.wb_req
    replay_arb.io.in(i)     <> mshr.io.replay
    refill_arb.io.in(i)     <> mshr.io.refill

    mshr.io.mem_grant.valid := io.mem_grant.valid && io.mem_grant.bits.source === i.U
    mshr.io.mem_grant.bits  := io.mem_grant.bits



    pri_rdy   = pri_rdy || mshr.io.req_pri_rdy
    sec_rdy   = sec_rdy || mshr.io.req_sec_rdy
    idx_match = idx_match || mshr.io.idx_match
    way_match = way_match || mshr.io.way_match

    resp_arb.io.in(i) <> mshr.io.resp

    when (!mshr.io.req_pri_rdy) {
      io.fence_rdy := false.B
    }
    when (!mshr.io.probe_rdy) {
      io.probe_rdy := false.B
    }

    mshr
  }

  alloc_arb.io.out.ready := io.req.valid && sdq_rdy && cacheable && !idx_match

  io.meta_write <> meta_write_arb.io.out
  io.wb_req     <> wb_req_arb.io.out

  val mmio_alloc_arb = Module(new Arbiter(Bool(), nIOMSHRs))


  var mmio_rdy = false.B

  val mmios = (0 until nIOMSHRs) map { i =>
    val id = cfg.nMSHRs + i
    val mshr = Module(new BoomIOMSHR(id))

    mmio_alloc_arb.io.in(i).valid := mshr.io.req.ready
    mmio_alloc_arb.io.in(i).bits  := DontCare
    mshr.io.req.valid := mmio_alloc_arb.io.in(i).ready
    mshr.io.req.bits  := io.req.bits

    mmio_rdy = mmio_rdy || mshr.io.req.ready

    mshr.io.mem_ack.bits  := io.mem_grant.bits
    mshr.io.mem_ack.valid := io.mem_grant.valid && io.mem_grant.bits.source === id.U

    resp_arb.io.in(id) <> mshr.io.resp
    when (!mshr.io.req.ready) {
      io.fence_rdy := false.B
    }
    mshr
  }

  mmio_alloc_arb.io.out.ready := io.req.valid && !cacheable

  TLArbiter.lowestFromSeq(edge, io.mem_acquire, mshrs.map(_.io.mem_acquire) ++ mmios.map(_.io.mem_access))
  TLArbiter.lowestFromSeq(edge, io.mem_finish,  mshrs.map(_.io.mem_finish))

  io.resp           <> resp_arb.io.out
  io.req.ready      := Mux(!cacheable, mmio_rdy, sdq_rdy && Mux(idx_match, tag_match && sec_rdy, pri_rdy))
  io.secondary_miss := idx_match && way_match
  io.refill         <> refill_arb.io.out

  val free_sdq = io.replay.fire() && isWrite(io.replay.bits.uop.mem_cmd)
  io.replay.bits.data := sdq(RegEnable(replay_arb.io.out.bits.sdq_id, free_sdq))

  io.replay <> replay_arb.io.out

  when (io.replay.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UIntToOH(replay_arb.io.out.bits.sdq_id) & Fill(cfg.nSDQ, free_sdq)) |
      PriorityEncoderOH(~sdq_val(cfg.nSDQ-1,0)) & Fill(cfg.nSDQ, sdq_enq)
  }

}

/**
 * Top level class wrapping a non-blocking dcache.
 *
 * @param hartid hardware thread for the cache
 */
class BoomNonBlockingDCache(hartid: Int)(implicit p: Parameters) extends LazyModule
{
  private val tileParams = p(TileKey)
  protected val cfg = tileParams.dcache.get

  protected def cacheClientParameters = cfg.scratch.map(x => Seq()).getOrElse(Seq(TLClientParameters(
    name          = s"Core ${hartid} DCache",
    sourceId      = IdRange(0, 1 max cfg.nMSHRs),
    supportsProbe = TransferSizes(cfg.blockBytes, cfg.blockBytes))))

  protected def mmioClientParameters = Seq(TLClientParameters(
    name          = s"Core ${hartid} DCache MMIO",
    sourceId      = IdRange(firstMMIO, firstMMIO + cfg.nMMIOs),
    requestFifo   = true))

  def firstMMIO = (cacheClientParameters.map(_.sourceId.end) :+ 0).max

  val node = TLClientNode(Seq(TLClientPortParameters(
    cacheClientParameters ++ mmioClientParameters,
    minLatency = 1)))

  lazy val module = new BoomNonBlockingDCacheModule(this)

  def flushOnFenceI = cfg.scratch.isEmpty && !node.edges.out(0).manager.managers.forall(m => !m.supportsAcquireT || !m.executable || m.regionType >= RegionType.TRACKED || m.regionType <= RegionType.UNCACHEABLE)

  require(!tileParams.core.haveCFlush || cfg.scratch.isEmpty, "CFLUSH_D_L1 instruction requires a D$")
}


class BoomDCacheBundle(implicit p: Parameters) extends BoomBundle()(p) {
  val hartid = Input(UInt(hartIdLen.W))
  val errors = new DCacheErrors
  val lsu   = Flipped(new LSUDMemIO)
}

class BoomNonBlockingDCacheModule(outer: BoomNonBlockingDCache) extends LazyModuleImp(outer)
  with HasL1HellaCacheParameters
{
  implicit val edge = outer.node.edges.out(0)
  val (tl_out, _) = outer.node.out(0)
  val io = IO(new BoomDCacheBundle)
  dontTouch(io)

  private val fifoManagers = edge.manager.managers.filter(TLFIFOFixer.allUncacheable)
  fifoManagers.foreach { m =>
    require (m.fifoId == fifoManagers.head.fifoId,
      s"IOMSHRs must be FIFO for all regions with effects, but HellaCache sees ${m.nodePath.map(_.name)}")
  }

  val wb = Module(new WritebackUnit)
  val prober = Module(new ProbeUnit)
  val mshrs = Module(new BoomMSHRFile)
  mshrs.io := DontCare
  mshrs.io.brinfo := io.lsu.brinfo
  mshrs.io.rob_pnr_idx  := io.lsu.rob_pnr_idx
  mshrs.io.rob_head_idx := io.lsu.rob_head_idx

  // tags
  def onReset = L1Metadata(0.U, ClientMetadata.onReset)
  val meta = Module(new L1MetadataArray(onReset _))
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, 2))
  // 0 goes to MSHR refills, 1 goes to prober
  val metaReadArb = Module(new Arbiter(new L1MetaReadReq, 4))
  // 0 goes to MSHR replays, 1 goes to prober, 2 goes to wb, 3 goes to pipeline
  dontTouch(meta.io)
  meta.io.write <> metaWriteArb.io.out
  meta.io.read  <> metaReadArb.io.out

  // data
  val data = Module(new DataArray)
  val dataWriteArb = Module(new Arbiter(new L1DataWriteReq, 2))
  // 0 goes to pipeline, 1 goes to MSHR refills
  val dataReadArb = Module(new Arbiter(new L1DataReadReq, 3))
  // 0 goes to MSHR replays, 1 goes to wb, 2 goes to pipeline
  dontTouch(data.io)
  data.io.write <> dataWriteArb.io.out
  data.io.read  <> dataReadArb.io.out

  // ------------
  // New requests

  io.lsu.req.ready := metaReadArb.io.in(3).ready && dataReadArb.io.in(2).ready
  // Tag read for new requests
  metaReadArb.io.in(3).valid       := io.lsu.req.valid
  metaReadArb.io.in(3).bits.idx    := io.lsu.req.bits.addr >> blockOffBits
  metaReadArb.io.in(3).bits.way_en := DontCare
  metaReadArb.io.in(3).bits.tag    := DontCare
  // Data read for new requests
  dataReadArb.io.in(2).valid       := io.lsu.req.valid
  dataReadArb.io.in(2).bits.addr   := io.lsu.req.bits.addr
  dataReadArb.io.in(2).bits.way_en := ~0.U(nWays.W)

  // ------------
  // MSHR Replays
  val replay_req = Wire(new BoomDCacheReq)
  replay_req.uop        := mshrs.io.replay.bits.uop
  replay_req.addr       := mshrs.io.replay.bits.addr
  replay_req.data       := mshrs.io.replay.bits.data
  replay_req.is_hella   := mshrs.io.replay.bits.is_hella
  mshrs.io.replay.ready := metaReadArb.io.in(0).ready && dataReadArb.io.in(0).ready
  // Tag read for MSHR replays
  // We don't actually need to read the metadata, for replays we already know our way
  metaReadArb.io.in(0).valid       := mshrs.io.replay.valid
  metaReadArb.io.in(0).bits.idx    := mshrs.io.replay.bits.addr >> blockOffBits
  metaReadArb.io.in(0).bits.way_en := DontCare
  metaReadArb.io.in(0).bits.tag    := DontCare
  // Data read for MSHR replays
  dataReadArb.io.in(0).valid       := mshrs.io.replay.valid
  dataReadArb.io.in(0).bits.addr   := mshrs.io.replay.bits.addr
  dataReadArb.io.in(0).bits.way_en := mshrs.io.replay.bits.way_en

  // -----------
  // Write-backs
  val wb_fire = wb.io.meta_read.fire() && wb.io.data_req.fire()
  val wb_req = Wire(new BoomDCacheReq) // This is for debugging
  wb_req.uop      := NullMicroOp
  wb_req.addr     := Cat(wb.io.meta_read.bits.tag, wb.io.data_req.bits.addr)
  wb_req.data     := DontCare
  wb_req.is_hella := false.B
  // Couple the two decoupled interfaces of the WBUnit's meta_read and data_read
  // Tag read for write-back
  metaReadArb.io.in(2).valid := wb.io.meta_read.valid
  metaReadArb.io.in(2).bits  := wb.io.meta_read.bits
  wb.io.meta_read.ready := metaReadArb.io.in(2).ready && dataReadArb.io.in(1).ready
  // Data read for write-back
  dataReadArb.io.in(1).valid := wb.io.data_req.valid
  dataReadArb.io.in(1).bits  := wb.io.data_req.bits
  wb.io.data_req.ready  := metaReadArb.io.in(2).ready && dataReadArb.io.in(1).ready
  assert(!(wb.io.meta_read.fire() ^ wb.io.data_req.fire()))

  // -------
  // Prober
  val prober_fire  = prober.io.meta_read.fire()
  val prober_req   = Wire(new BoomDCacheReq) // This is for debugging
  prober_req.uop      := NullMicroOp
  prober_req.addr     := Cat(prober.io.meta_read.bits.tag, prober.io.meta_read.bits.idx) << blockOffBits
  prober_req.data     := DontCare
  prober_req.is_hella := false.B
  // Tag read for prober
  metaReadArb.io.in(1)  <> prober.io.meta_read
  // Prober does not need to read data array

  val s0_valid = io.lsu.req.fire() || mshrs.io.replay.fire() || wb_fire || prober_fire
  val s0_req   = Mux(io.lsu.req.fire(), io.lsu.req.bits,
                 Mux(wb_fire          , wb_req,
                 Mux(prober_fire      , prober_req
                                      , replay_req)))
  val s0_send_resp = io.lsu.req.fire() || (mshrs.io.replay.fire() && isRead(mshrs.io.replay.bits.uop.mem_cmd)) // Does this request need to send a response

  val s1_valid = RegNext(s0_valid &&
                         !IsKilledByBranch(io.lsu.brinfo, s0_req.uop), init=false.B) && !io.lsu.s1_kill
  assert(!(io.lsu.s1_kill && !RegNext(io.lsu.req.fire())))
  val s1_req          = Reg(new BoomDCacheReq)
  s1_req             := s0_req
  s1_req.uop.br_mask := GetNewBrMask(io.lsu.brinfo, s0_req.uop)
  val s1_addr         = s1_req.addr
  val s1_nack         = s1_req.addr(idxMSB,idxLSB) === prober.io.meta_write.bits.idx && !prober.io.req.ready
  val s1_send_resp     = RegNext(s0_send_resp)
  val s1_is_probe      = RegNext(prober_fire)
  val s1_is_replay     = RegNext(mshrs.io.replay.fire())
  val s1_replay_way_en = RegNext(mshrs.io.replay.bits.way_en) // For replays, the metadata isn't written yet

  // tag check
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta.io.resp(w).tag === (s1_addr >> untagBits)).asUInt
  val s1_tag_match_way = Mux(s1_is_replay, s1_replay_way_en, wayMap((w: Int) => s1_tag_eq_way(w) && meta.io.resp(w).coh.isValid()).asUInt)


  val s2_valid = RegNext(s1_valid && !IsKilledByBranch(io.lsu.brinfo, s1_req.uop))
  val s2_req   = Reg(new BoomDCacheReq)
  s2_req             := s1_req
  s2_req.uop.br_mask := GetNewBrMask(io.lsu.brinfo, s1_req.uop)
  val s2_is_probe  = RegNext(s1_is_probe)
  val s2_is_replay = RegNext(s1_is_replay)


  val s2_tag_match_way = RegNext(s1_tag_match_way)
  val s2_tag_match = s2_tag_match_way.orR
  val s2_hit_state = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegNext(meta.io.resp(w).coh)))
  val (s2_has_permission, _, s2_new_hit_state) = s2_hit_state.onAccess(s2_req.uop.mem_cmd)
  //
  val s2_hit = s2_tag_match && ((s2_has_permission && s2_hit_state === s2_new_hit_state) || s2_is_replay)
  assert(!(s2_is_replay && !s2_hit), "Replays should always hit")

  // lr/sc
  val lrsc_count = RegInit(0.U(log2Ceil(lrscCycles).W))
  val lrsc_valid = lrsc_count > lrscBackoff.U
  val lrsc_addr  = Reg(UInt())
  val (s2_lr, s2_sc) = (s2_req.uop.mem_cmd === M_XLR, s2_req.uop.mem_cmd === M_XSC)
  val s2_lrsc_addr_match = lrsc_valid && lrsc_addr === (s2_req.addr >> blockOffBits)
  val s2_sc_fail = s2_sc && !s2_lrsc_addr_match
  when (lrsc_count > 0.U) { lrsc_count := lrsc_count - 1.U }
  when ((s2_valid && s2_hit) || (s2_is_replay && s2_req.uop.mem_cmd =/= M_FLUSH_ALL)) {
    when (s2_lr) {
      lrsc_count := (lrscCycles - 1).U
      lrsc_addr := s2_req.addr >> blockOffBits
    }
    when (lrsc_count > 0.U) {
      lrsc_count := 0.U
    }
  }

  val s2_data = Wire(Vec(nWays, UInt(encRowBits.W)))
  for (w <- 0 until nWays) {
    val regs = Reg(Vec(rowWords, UInt(encDataBits.W)))
    for (i <- 0 until rowWords) {
      regs(i) := data.io.resp(w) >> encDataBits*i
    }
    s2_data(w) := regs.asUInt
  }
  val s2_data_muxed = Mux1H(s2_tag_match_way, s2_data)
  val s2_word_idx   = if (doNarrowRead) 0.U else s2_req.addr(log2Ceil(rowWords*coreDataBytes)-1, log2Ceil(wordBytes))

  // replacement policy
  val replacer = cacheParams.replacement
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegNext(replacer.way))
  val s2_repl_meta = Mux1H(s2_replaced_way_en, wayMap((w: Int) => RegNext(meta.io.resp(w))).toSeq)

  // Miss handling
  mshrs.io.req.valid          := s2_valid && !s2_hit && (isPrefetch(s2_req.uop.mem_cmd) || isRead(s2_req.uop.mem_cmd) || isWrite(s2_req.uop.mem_cmd)) && !s2_is_probe
  assert(!(mshrs.io.req.valid && s2_is_replay), "Replays should not need to go back into MSHRs")
  mshrs.io.req.bits.uop       := s2_req.uop
  mshrs.io.req.bits.addr      := s2_req.addr
  mshrs.io.req.bits.tag_match := s2_tag_match
  mshrs.io.req.bits.old_meta  := Mux(s2_tag_match, L1Metadata(s2_repl_meta.tag, s2_hit_state), s2_repl_meta)
  mshrs.io.req.bits.way_en    := Mux(s2_tag_match, s2_tag_match_way, s2_replaced_way_en)
  mshrs.io.req.bits.data      := s2_req.data
  mshrs.io.req.bits.is_hella  := s2_req.is_hella
  when (mshrs.io.req.fire()) { replacer.miss }
  tl_out.a <> mshrs.io.mem_acquire

  // probes and releases
  prober.io.req.valid   := tl_out.b.valid
  tl_out.b.ready        := prober.io.req.ready
  prober.io.req.bits    := tl_out.b.bits
  prober.io.way_en      := s2_tag_match_way
  prober.io.block_state := s2_hit_state
  metaWriteArb.io.in(1) <> prober.io.meta_write
  prober.io.mshr_rdy    := mshrs.io.probe_rdy

  // refills
  val grant_has_data = edge.hasData(tl_out.d.bits)
  mshrs.io.mem_grant.valid := tl_out.d.fire()
  mshrs.io.mem_grant.bits  := tl_out.d.bits
  tl_out.d.ready           := true.B // MSHRs should always be ready for refills

  dataWriteArb.io.in(1) <> mshrs.io.refill
  metaWriteArb.io.in(0) <> mshrs.io.meta_write

  tl_out.e <> mshrs.io.mem_finish

  // writebacks
  val wbArb = Module(new Arbiter(new WritebackReq(edge.bundle), 2))
  // 0 goes to prober, 1 goes to MSHR evictions
  wbArb.io.in(0)       <> prober.io.wb_req
  wbArb.io.in(1)       <> mshrs.io.wb_req
  wb.io.req            <> wbArb.io.out
  wb.io.data_resp      := s2_data_muxed
  TLArbiter.lowest(edge, tl_out.c, wb.io.release, prober.io.rep)

  val s2_nack_hit    = RegNext(s1_nack) // nack because of prober
  val s2_nack_victim = s2_valid &&  s2_hit && mshrs.io.secondary_miss // Nack when we hit something currently being evicted
  val s2_nack_miss   = s2_valid && !s2_hit && !mshrs.io.req.ready // MSHRs not ready for request
  val s2_nack        = s2_nack_miss || s2_nack_hit || s2_nack_victim
  val s2_send_resp = (RegNext(s1_send_resp) &&
                      (s2_hit ||
                       s2_nack ||
                       (mshrs.io.req.fire() && isWrite(s2_req.uop.mem_cmd))))
  // hits always send a response
  // If MSHR is not available, LSU has to replay this request later
  // If MSHR is available and this is a store, we don't need to wait for resp later

  // load data gen
  val s2_data_word = s2_data_muxed >> Cat(s2_word_idx, 0.U(log2Ceil(coreDataBits).W))
  val loadgen = new LoadGen(s2_req.uop.mem_size, s2_req.uop.mem_signed, s2_req.addr,
                            s2_data_word, s2_sc, wordBytes)

  // Mux between cache responses and uncache responses
  val cache_resp   = Wire(Valid(new BoomDCacheResp))
  cache_resp.valid         := s2_valid && s2_send_resp
  cache_resp.bits.uop      := s2_req.uop
  cache_resp.bits.data     := loadgen.data // TODO: Fix, add lrsc
  cache_resp.bits.nack     := s2_nack // TODO: Fix
  cache_resp.bits.is_hella := s2_req.is_hella

  val uncache_resp = Wire(Valid(new BoomDCacheResp))
  uncache_resp.bits     := mshrs.io.resp.bits
  uncache_resp.valid    := mshrs.io.resp.valid
  mshrs.io.resp.ready := !cache_resp.valid // We can backpressure the MSHRs, but not cache hits

  io.lsu.resp := Mux(mshrs.io.resp.fire(), uncache_resp, cache_resp)

  // Store/amo hits
  val amoalu   = Module(new AMOALU(xLen))
  amoalu.io.mask := new StoreGen(s2_req.uop.mem_size, s2_req.addr, 0.U, xLen/8).mask
  amoalu.io.cmd  := s2_req.uop.mem_cmd
  amoalu.io.lhs  := s2_data_word
  amoalu.io.rhs  := s2_req.data
  val s3_valid = RegNext(s2_valid && s2_hit && isWrite(s2_req.uop.mem_cmd))
  val s3_req   = RegNext(s2_req)
  s3_req.data := amoalu.io.out
  val s3_way   = RegNext(s2_tag_match_way)

  dataWriteArb.io.in(0).valid       := s3_valid
  dataWriteArb.io.in(0).bits.addr   := s3_req.addr
  dataWriteArb.io.in(0).bits.wmask  := UIntToOH(s3_req.addr.extract(rowOffBits-1,offsetlsb))
  dataWriteArb.io.in(0).bits.data   := Fill(rowWords, s3_req.data)
  dataWriteArb.io.in(0).bits.way_en := s3_way


  io.lsu.ordered := mshrs.io.fence_rdy && !s1_valid && !s2_valid
}
