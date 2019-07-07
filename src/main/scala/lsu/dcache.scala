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
import boom.util.{IsKilledByBranch, GetNewBrMask, BranchKillableQueue, IsOlder, UpdateBrMask, AgePriorityEncoder, WrapInc}

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
    val exception    = Input(Bool())
    val rob_pnr_idx  = Input(UInt(robAddrSz.W))
    val rob_head_idx = Input(UInt(robAddrSz.W))

    val req         = Input(new BoomDCacheReqInternal)

    val idx = Output(Valid(UInt()))
    val way = Output(Valid(UInt()))
    val tag = Output(Valid(UInt()))

    val mem_acquire = Decoupled(new TLBundleA(edge.bundle))

    val mem_grant   = Flipped(Valid(new TLBundleD(edge.bundle)))
    val mem_finish  = Decoupled(new TLBundleE(edge.bundle))

    val refill      = Decoupled(new L1DataWriteReq)

    val meta_write  = Decoupled(new L1MetaWriteReq)
    val wb_req      = Decoupled(new WritebackReq(edge.bundle))

    // To inform the prefetcher when we are commiting the fetch of this line
    val commit_val  = Output(Bool())
    val commit_addr = Output(UInt(coreMaxAddrBits.W))
    val commit_cmd  = Output(UInt(M_SZ.W))

    // When we hold a prefetch or speculated load, and we need to clear this
    val clearable   = Output(Bool())
    val clr_entry   = Input(Bool())

    // Replays go through the cache pipeline again
    val replay      = Decoupled(new BoomDCacheReqInternal)
    // Resp go straight out to the core
    val resp        = Decoupled(new BoomDCacheResp)

    val probe_rdy   = Output(Bool())
  })

  // TODO: Optimize this. We don't want to mess with cache during speculation
  // s_refill_req      : Make a request for a new cache line
  // s_refill_resp     : Store the refill response into our buffer
  // s_drain_rpq_loads : Drain out loads from the rpq
  //                   : If miss was misspeculated, go to s_invalid
  // s_wb_req          : Write back the evicted cache line
  // s_wb_resp         : Finish writing back the evicted cache line
  // s_meta_write_req  : Write the metadata for new cache lne
  // s_meta_write_resp :

  val s_invalid :: s_refill_req :: s_refill_resp :: s_drain_rpq_loads :: s_wb_req :: s_wb_resp :: s_meta_clear :: s_commit_line :: s_meta_write_req :: s_meta_write_resp :: s_drain_rpq :: Nil = Enum(11)
  val state = RegInit(s_invalid)

  val req     = Reg(new BoomDCacheReqInternal)
  val req_idx = req.addr(untagBits-1, blockOffBits)
  val req_tag = req.addr >> untagBits
  val req_block_addr = (req.addr >> blockOffBits) << blockOffBits
  val req_needs_wb = RegInit(false.B)
  val idx_match = req_idx === io.req.addr(untagBits-1, blockOffBits)
  val way_match = req.way_en === io.req.way_en
  val tag_match = req_tag === io.req.addr >> untagBits

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
  val sec_rdy = (idx_match &&
                 !cmd_requires_second_acquire &&
                 !state.isOneOf(s_invalid, s_meta_write_req, s_meta_write_resp, s_drain_rpq))// Always accept secondary misses

  val rpq = Module(new BranchKillableQueue(new BoomDCacheReqInternal, cfg.nRPQ, u => u.uses_ldq))
  rpq.io.brinfo := io.brinfo
  rpq.io.flush  := io.exception

  rpq.io.enq.valid := ((io.req_pri_val && io.req_pri_rdy) || (io.req_sec_val && io.req_sec_rdy)) && !isPrefetch(io.req.uop.mem_cmd)
  rpq.io.enq.bits  := io.req
  rpq.io.deq.ready := false.B


  val grantackq = Module(new Queue(new TLBundleE(edge.bundle), 1))
  val can_finish = state === s_drain_rpq_loads
  grantackq.io.enq.valid := refill_done && edge.isRequest(io.mem_grant.bits)
  grantackq.io.enq.bits  := edge.GrantAck(io.mem_grant.bits)
  io.mem_finish.valid    := grantackq.io.deq.valid && can_finish
  io.mem_finish.bits     := grantackq.io.deq.bits
  grantackq.io.deq.ready := io.mem_finish.ready && can_finish

  val load_buffer = Mem(cacheDataBeats,
                        UInt(encRowBits.W))
  val refill_ctr  = Reg(UInt(log2Ceil(cacheDataBeats).W))
  val commit_line = Reg(Bool())

  io.probe_rdy   := true.B

  io.idx.valid := state =/= s_invalid
  io.idx.bits  := req_idx
  io.tag.valid := state =/= s_invalid
  io.tag.bits  := req_tag
  io.way.valid := !state.isOneOf(s_invalid, s_refill_req, s_refill_resp, s_drain_rpq_loads)
  io.way.bits  := req.way_en

  io.meta_write.valid  := false.B
  io.req_pri_rdy       := false.B
  io.req_sec_rdy       := sec_rdy && rpq.io.enq.ready
  io.mem_acquire.valid := false.B
  io.refill.valid      := false.B
  io.replay.valid      := false.B
  io.wb_req.valid      := false.B
  io.resp.valid        := false.B
  io.commit_val        := false.B
  io.commit_addr       := req.addr
  io.commit_cmd        := Mux(ClientStates.hasWritePermission(new_coh.state), M_PFW, M_PFR)
  io.clearable         := false.B

  when (io.req_sec_val && io.req_sec_rdy) {
    req.uop.mem_cmd := dirtier_cmd
    when (is_hit_again) {
      new_coh := dirtier_coh
    }
  }

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
  } .elsewhen (state === s_refill_req) {
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
    val drain_load = isRead(rpq.io.deq.bits.uop.mem_cmd) && !isWrite(rpq.io.deq.bits.uop.mem_cmd)
    // drain all loads for now
    val rp_addr = Cat(req_tag, req_idx, rpq.io.deq.bits.addr(blockOffBits-1,0))
    val word_idx  = if (rowWords == 1) 0.U else rp_addr(log2Up(rowWords*coreDataBytes)-1, log2Up(wordBytes))
    val data      = load_buffer(rpq.io.deq.bits.addr >> rowOffBits)
    val data_word = data >> Cat(word_idx, 0.U(log2Up(coreDataBits).W))
    val loadgen = new LoadGen(rpq.io.deq.bits.uop.mem_size, rpq.io.deq.bits.uop.mem_signed,
      Cat(req_tag, req_idx, rpq.io.deq.bits.addr(blockOffBits-1,0)),
      data_word, false.B, wordBytes)

    rpq.io.deq.ready  := io.resp.ready && drain_load
    io.resp.valid     := rpq.io.deq.valid && drain_load
    io.resp.bits.uop  := rpq.io.deq.bits.uop
    io.resp.bits.data := loadgen.data
    io.resp.bits.is_hella := rpq.io.deq.bits.is_hella
    when (rpq.io.deq.fire()) {
      commit_line   := true.B
      io.commit_val := true.B
    }
      .elsewhen (rpq.io.empty && !commit_line)
    {
      io.clearable := true.B
      when (io.clr_entry && !rpq.io.enq.fire()) {
        state := s_invalid
      }
    } .elsewhen (rpq.io.empty || (rpq.io.deq.valid && !drain_load)) {
      io.commit_val := true.B
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
        state := s_meta_write_req
      }
    }
  } .elsewhen (state === s_meta_write_req) {
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
    state := s_drain_rpq
  }.elsewhen (state === s_drain_rpq) {
    io.replay <> rpq.io.deq
    io.replay.bits.way_en    := req.way_en
    io.replay.bits.addr := Cat(req_tag, req_idx, rpq.io.deq.bits.addr(blockOffBits-1,0))
    when (rpq.io.empty ) {
      state := s_invalid
    }
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

  val send_resp = isRead(req.uop.mem_cmd)

  io.resp.valid     := (state === s_resp) && send_resp
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
  when (state === s_resp) {
    when (!send_resp || io.resp.fire()) {
      state := s_idle
    }
  }
}


class BoomMSHRFile(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
  with HasL1HellaCacheParameters
{
  val io = IO(new Bundle {
    val req  = Flipped(Vec(memWidth, Decoupled(new BoomDCacheReqInternal)))
    val resp = Decoupled(new BoomDCacheResp)
    val secondary_miss = Output(Vec(memWidth, Bool()))
    val block_hit      = Output(Vec(memWidth, Bool()))

    val brinfo       = Input(new BrResolutionInfo)
    val exception    = Input(Bool())
    val rob_pnr_idx  = Input(UInt(robAddrSz.W))
    val rob_head_idx = Input(UInt(robAddrSz.W))

    val mem_acquire  = Decoupled(new TLBundleA(edge.bundle))
    val mem_grant    = Flipped(Valid(new TLBundleD(edge.bundle)))
    val mem_finish   = Decoupled(new TLBundleE(edge.bundle))

    val refill     = Decoupled(new L1DataWriteReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val replay     = Decoupled(new BoomDCacheReqInternal)
    val prefetch   = Decoupled(new BoomDCacheReq)
    val wb_req     = Decoupled(new WritebackReq(edge.bundle))

    val clear_all = Input(Bool()) // Clears all uncommitted MSHRs to prepare for fence

    val fence_rdy = Output(Bool())
    val probe_rdy = Output(Bool())
  })

  val req_idx = OHToUInt(io.req.map(_.valid))
  val req     = io.req(req_idx)

  val prefetcher: DataPrefetcher = if (enablePrefetching) Module(new NLPrefetcher)
                                                     else Module(new NullPrefetcher)

  io.prefetch <> prefetcher.io.prefetch


  val cacheable = edge.manager.supportsAcquireBFast(req.bits.addr, lgCacheBlockBytes.U)

  val sdq_val      = RegInit(0.U(cfg.nSDQ.W))
  val sdq_alloc_id = PriorityEncoder(~sdq_val(cfg.nSDQ-1,0))
  val sdq_rdy      = !sdq_val.andR
  val sdq_enq      = req.fire() && cacheable && isWrite(req.bits.uop.mem_cmd)
  val sdq          = Mem(cfg.nSDQ, UInt(coreDataBits.W))

  when (sdq_enq) {
    sdq(sdq_alloc_id) := req.bits.data
  }

  val idx_matches = Wire(Vec(memWidth, Vec(cfg.nMSHRs, Bool())))
  val tag_matches = Wire(Vec(memWidth, Vec(cfg.nMSHRs, Bool())))
  val way_matches = Wire(Vec(memWidth, Vec(cfg.nMSHRs, Bool())))

  val tag_match   = VecInit((0 until memWidth).map(i => Mux1H(idx_matches(i), tag_matches(i))))
  val idx_match   = VecInit((0 until memWidth).map(i => idx_matches(i).reduce(_||_)))
  val way_match   = VecInit((0 until memWidth).map(i => Mux1H(idx_matches(i), way_matches(i))))

  val wb_tag_list = Wire(Vec(cfg.nMSHRs, UInt(tagBits.W)))

  val meta_write_arb = Module(new Arbiter(new L1MetaWriteReq           , cfg.nMSHRs))
  val wb_req_arb     = Module(new Arbiter(new WritebackReq(edge.bundle), cfg.nMSHRs))
  val replay_arb     = Module(new Arbiter(new BoomDCacheReqInternal    , cfg.nMSHRs))
  val resp_arb       = Module(new Arbiter(new BoomDCacheResp           , cfg.nMSHRs + nIOMSHRs))
  val refill_arb     = Module(new Arbiter(new L1DataWriteReq           , cfg.nMSHRs))

  val commit_vals    = Wire(Vec(cfg.nMSHRs, Bool()))
  val commit_addrs   = Wire(Vec(cfg.nMSHRs, UInt(coreMaxAddrBits.W)))
  val commit_cmds    = Wire(Vec(cfg.nMSHRs, UInt(M_SZ.W)))

  var sec_rdy   = false.B

  io.fence_rdy := true.B
  io.probe_rdy := true.B

  val mshr_alloc_idx = Wire(UInt())
  val mshr_clear_idx = Wire(UInt())
  val pri_rdy = WireInit(false.B)
  val pri_val = req.valid && sdq_rdy && cacheable && !idx_match(req_idx)
  val mshrs = (0 until cfg.nMSHRs) map { i =>
    val mshr = Module(new BoomMSHR(i))

    for (ii <- 0 until memWidth) {
      idx_matches(ii)(i) := mshr.io.idx.valid &&
                           (mshr.io.idx.bits === io.req(ii).bits.addr(untagBits-1,blockOffBits))
      tag_matches(ii)(i) := mshr.io.tag.valid &&
                           (mshr.io.tag.bits === io.req(ii).bits.addr >> untagBits)
      way_matches(ii)(i) := mshr.io.way.valid &&
                           (mshr.io.way.bits === io.req(ii).bits.way_en)
    }

    wb_tag_list(i) := mshr.io.wb_req.bits.tag
    mshr.io.req_pri_val  := (i.U === mshr_alloc_idx) && pri_val
    when (i.U === mshr_alloc_idx) {
      pri_rdy := mshr.io.req_pri_rdy
    }

    mshr.io.req_sec_val  := req.valid && sdq_rdy && tag_match(req_idx) && cacheable
    mshr.io.req          := req.bits
    mshr.io.req.sdq_id   := sdq_alloc_id

    mshr.io.brinfo       := io.brinfo
    mshr.io.exception    := io.exception
    mshr.io.rob_pnr_idx  := io.rob_pnr_idx
    mshr.io.rob_head_idx := io.rob_head_idx

    mshr.io.clr_entry    := mshr.io.clearable && ((pri_val && !pri_rdy && (i.U === mshr_clear_idx)) ||              // Clear because no MSHRs are ready
                                                  io.clear_all                                      ||              // Clear because core is asking us to fence
                                                  (idx_match(req_idx) && !tag_match(req_idx))       ||              // Clear because can't have multiple MSHR with same idx
                                                  (idx_match(req_idx) &&  tag_match(req_idx) && !mshr.io.req_sec_rdy)) // Clear because we are a write, but this was a prefetch read
    meta_write_arb.io.in(i) <> mshr.io.meta_write
    wb_req_arb.io.in(i)     <> mshr.io.wb_req
    replay_arb.io.in(i)     <> mshr.io.replay
    refill_arb.io.in(i)     <> mshr.io.refill

    commit_vals(i)  := mshr.io.commit_val
    commit_addrs(i) := mshr.io.commit_addr
    commit_cmds(i)  := mshr.io.commit_cmd

    mshr.io.mem_grant.valid := io.mem_grant.valid && io.mem_grant.bits.source === i.U
    mshr.io.mem_grant.bits  := io.mem_grant.bits

    sec_rdy   = sec_rdy || mshr.io.req_sec_rdy

    resp_arb.io.in(i) <> mshr.io.resp

    when (!mshr.io.req_pri_rdy) {
      io.fence_rdy := false.B
    }
    when (!mshr.io.probe_rdy) {
      io.probe_rdy := false.B
    }

    mshr
  }

  // Try to round-robin the MSHRs
  val mshr_head      = RegInit(0.U(log2Ceil(cfg.nMSHRs).W))
  mshr_alloc_idx    := RegNext(AgePriorityEncoder(mshrs.map(m=>m.io.req_pri_rdy), mshr_head))
  when (pri_rdy && pri_val) { mshr_head := WrapInc(mshr_head, cfg.nMSHRs) }

  // Clear a random MSHR. TODO: Is this suboptimal for prefetching?
  mshr_clear_idx    := PriorityEncoder(mshrs.map(m=>m.io.clearable))


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
    mshr.io.req.bits  := req.bits

    mmio_rdy = mmio_rdy || mshr.io.req.ready

    mshr.io.mem_ack.bits  := io.mem_grant.bits
    mshr.io.mem_ack.valid := io.mem_grant.valid && io.mem_grant.bits.source === id.U

    resp_arb.io.in(id) <> mshr.io.resp
    when (!mshr.io.req.ready) {
      io.fence_rdy := false.B
    }
    mshr
  }

  mmio_alloc_arb.io.out.ready := req.valid && !cacheable

  TLArbiter.lowestFromSeq(edge, io.mem_acquire, mshrs.map(_.io.mem_acquire) ++ mmios.map(_.io.mem_access))
  TLArbiter.lowestFromSeq(edge, io.mem_finish,  mshrs.map(_.io.mem_finish))

  io.resp           <> resp_arb.io.out
  for (i <- 0 until memWidth) {
    io.req(i).ready      := (i.U === req_idx) &&
                            Mux(!cacheable, mmio_rdy, sdq_rdy && Mux(idx_match(i), tag_match(i) && sec_rdy, pri_rdy))
    io.secondary_miss(i) := idx_match(i) && way_match(i) && !tag_match(i)
    io.block_hit(i)      := idx_match(i) && tag_match(i)
  }
  io.refill         <> refill_arb.io.out

  val free_sdq = io.replay.fire() && isWrite(io.replay.bits.uop.mem_cmd)
  io.replay.bits.data := sdq(RegEnable(replay_arb.io.out.bits.sdq_id, free_sdq))

  io.replay <> replay_arb.io.out

  when (io.replay.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UIntToOH(replay_arb.io.out.bits.sdq_id) & Fill(cfg.nSDQ, free_sdq)) |
      PriorityEncoderOH(~sdq_val(cfg.nSDQ-1,0)) & Fill(cfg.nSDQ, sdq_enq)
  }

  prefetcher.io.mshr_avail    := RegNext(pri_rdy)
  prefetcher.io.req_val       := RegNext(commit_vals.reduce(_||_))
  prefetcher.io.req_addr      := RegNext(Mux1H(commit_vals, commit_addrs))
  prefetcher.io.req_cmd       := RegNext(Mux1H(commit_vals, commit_cmds))
}

class BoomL1MetaReadReq(implicit p: Parameters) extends BoomBundle()(p) {
  val req = Vec(memWidth, new L1MetaReadReq)
}

class BoomL1DataReadReq(implicit p: Parameters) extends BoomBundle()(p) {
  val req = Vec(memWidth, new L1DataReadReq)
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
  with HasBoomCoreParameters
{
  implicit val edge = outer.node.edges.out(0)
  val (tl_out, _) = outer.node.out(0)
  val io = IO(new BoomDCacheBundle)

  private val fifoManagers = edge.manager.managers.filter(TLFIFOFixer.allUncacheable)
  fifoManagers.foreach { m =>
    require (m.fifoId == fifoManagers.head.fifoId,
      s"IOMSHRs must be FIFO for all regions with effects, but HellaCache sees ${m.nodePath.map(_.name)}")
  }

  val wb = Module(new WritebackUnit)
  val prober = Module(new ProbeUnit)
  val mshrs = Module(new BoomMSHRFile)
  mshrs.io.clear_all    := io.lsu.force_order
  mshrs.io.brinfo       := io.lsu.brinfo
  mshrs.io.exception    := io.lsu.exception
  mshrs.io.rob_pnr_idx  := io.lsu.rob_pnr_idx
  mshrs.io.rob_head_idx := io.lsu.rob_head_idx

  def widthMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))

  // tags
  def onReset = L1Metadata(0.U, ClientMetadata.onReset)
  val meta = Seq.fill(memWidth) { Module(new L1MetadataArray(onReset _)) }
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, 2))
  // 0 goes to MSHR refills, 1 goes to prober
  val metaReadArb = Module(new Arbiter(new BoomL1MetaReadReq, 5))
  // 0 goes to MSHR replays, 1 goes to prober, 2 goes to wb, 3 goes to pipeline, 4 goes to prefetch

  metaReadArb.io.in := DontCare
  for (i <- 0 until memWidth) {
    meta(i).io.write.valid := metaWriteArb.io.out.fire()
    meta(i).io.write.bits  := metaWriteArb.io.out.bits
    meta(i).io.read.valid  := metaReadArb.io.out.valid
    meta(i).io.read.bits   := metaReadArb.io.out.bits.req(i)
  }
  metaReadArb.io.out.ready  := meta.map(_.io.read.ready).reduce(_&&_)
  metaWriteArb.io.out.ready := meta.map(_.io.write.ready).reduce(_&&_)

  // data
  val data = Seq.fill(memWidth) { Module(new DataArray) }
  val dataWriteArb = Module(new Arbiter(new L1DataWriteReq, 2))
  // 0 goes to pipeline, 1 goes to MSHR refills
  val dataReadArb = Module(new Arbiter(new BoomL1DataReadReq, 3))
  // 0 goes to MSHR replays, 1 goes to wb, 2 goes to pipeline

  dataReadArb.io.in := DontCare
  for (i <- 0 until memWidth) {
    data(i).io.write.valid := dataWriteArb.io.out.fire()
    data(i).io.write.bits  := dataWriteArb.io.out.bits
    data(i).io.read.valid  := dataReadArb.io.out.valid
    data(i).io.read.bits   := dataReadArb.io.out.bits.req(i)
  }
  dataReadArb.io.out.ready  := data.map(_.io.read.ready).reduce(_&&_)
  dataWriteArb.io.out.ready := data.map(_.io.write.ready).reduce(_&&_)

  // ------------
  // New requests

  io.lsu.req.ready := metaReadArb.io.in(3).ready && dataReadArb.io.in(2).ready
  assert(!(io.lsu.req.valid &&
    io.lsu.req.bits.map(x => x.valid && x.bits.uop.uses_stq).reduce(_||_) &&
    io.lsu.req.bits.map(x => x.valid && x.bits.uop.uses_ldq).reduce(_||_)))
  metaReadArb.io.in(3).valid       := io.lsu.req.valid
  dataReadArb.io.in(2).valid       := io.lsu.req.valid
  for (i <- 0 until memWidth) {
    // Tag read for new requests
    metaReadArb.io.in(3).bits.req(i).idx    := io.lsu.req.bits(i).bits.addr >> blockOffBits
    metaReadArb.io.in(3).bits.req(i).way_en := DontCare
    metaReadArb.io.in(3).bits.req(i).tag    := DontCare
    // Data read for new requests

    dataReadArb.io.in(2).bits.req(i).addr   := io.lsu.req.bits(i).bits.addr
    dataReadArb.io.in(2).bits.req(i).way_en := ~0.U(nWays.W)
  }

  // ------------
  // MSHR Replays
  val replay_req = Wire(Vec(memWidth, new BoomDCacheReq))
  replay_req               := DontCare
  replay_req(0).uop        := mshrs.io.replay.bits.uop
  replay_req(0).addr       := mshrs.io.replay.bits.addr
  replay_req(0).data       := mshrs.io.replay.bits.data
  replay_req(0).is_hella   := mshrs.io.replay.bits.is_hella
  mshrs.io.replay.ready := metaReadArb.io.in(0).ready && dataReadArb.io.in(0).ready
  // Tag read for MSHR replays
  // We don't actually need to read the metadata, for replays we already know our way
  metaReadArb.io.in(0).valid              := mshrs.io.replay.valid
  metaReadArb.io.in(0).bits.req(0).idx    := mshrs.io.replay.bits.addr >> blockOffBits
  metaReadArb.io.in(0).bits.req(0).way_en := DontCare
  metaReadArb.io.in(0).bits.req(0).tag    := DontCare
  // Data read for MSHR replays
  dataReadArb.io.in(0).valid              := mshrs.io.replay.valid
  dataReadArb.io.in(0).bits.req(0).addr   := mshrs.io.replay.bits.addr
  dataReadArb.io.in(0).bits.req(0).way_en := mshrs.io.replay.bits.way_en

  // -----------
  // Write-backs
  val wb_fire = wb.io.meta_read.fire() && wb.io.data_req.fire()
  val wb_req = Wire(Vec(memWidth, new BoomDCacheReq)) // This is for debugging
  wb_req             := DontCare
  wb_req(0).uop      := NullMicroOp
  wb_req(0).addr     := Cat(wb.io.meta_read.bits.tag, wb.io.data_req.bits.addr)
  wb_req(0).data     := DontCare
  wb_req(0).is_hella := false.B
  // Couple the two decoupled interfaces of the WBUnit's meta_read and data_read
  // Tag read for write-back
  metaReadArb.io.in(2).valid        := wb.io.meta_read.valid
  metaReadArb.io.in(2).bits.req(0)  := wb.io.meta_read.bits
  wb.io.meta_read.ready := metaReadArb.io.in(2).ready && dataReadArb.io.in(1).ready
  // Data read for write-back
  dataReadArb.io.in(1).valid        := wb.io.data_req.valid
  dataReadArb.io.in(1).bits.req(0)  := wb.io.data_req.bits
  wb.io.data_req.ready  := metaReadArb.io.in(2).ready && dataReadArb.io.in(1).ready
  assert(!(wb.io.meta_read.fire() ^ wb.io.data_req.fire()))

  // -------
  // Prober
  val prober_fire  = prober.io.meta_read.fire()
  val prober_req   = Wire(Vec(memWidth, new BoomDCacheReq)) // This is for debugging
  prober_req             := DontCare
  prober_req(0).uop      := NullMicroOp
  prober_req(0).addr     := Cat(prober.io.meta_read.bits.tag,
                                prober.io.meta_read.bits.idx) << blockOffBits
  prober_req(0).data     := DontCare
  prober_req(0).is_hella := false.B
  // Tag read for prober
  metaReadArb.io.in(1).valid       := prober.io.meta_read.valid
  metaReadArb.io.in(1).bits.req(0) := prober.io.meta_read.bits
  prober.io.meta_read.ready := metaReadArb.io.in(1).ready
  // Prober does not need to read data array

  // -------
  // Prefetcher
  val prefetch_fire = mshrs.io.prefetch.fire()
  val prefetch_req  = Wire(Vec(memWidth, new BoomDCacheReq))
  prefetch_req     := DontCare
  prefetch_req(0)  := mshrs.io.prefetch.bits
  // Tag read for prefetch
  metaReadArb.io.in(4).valid              := mshrs.io.prefetch.valid
  metaReadArb.io.in(4).bits.req(0).idx    := mshrs.io.prefetch.bits.addr >> blockOffBits
  metaReadArb.io.in(4).bits.req(0).way_en := DontCare
  metaReadArb.io.in(4).bits.req(0).tag    := DontCare
  mshrs.io.prefetch.ready := metaReadArb.io.in(4).ready
  // Prefetch does not need to read data array

  val s0_valid = Mux(io.lsu.req.fire(), VecInit(io.lsu.req.bits.map(_.valid)),
                 Mux(mshrs.io.replay.fire() ||
                     wb_fire                ||
                     prober_fire            ||
                     prefetch_fire           , VecInit(1.U(memWidth.W).asBools)
                                             , VecInit(0.U(memWidth.W).asBools)))
  val s0_req   = Mux(io.lsu.req.fire(), VecInit(io.lsu.req.bits.map(_.bits)),
                 Mux(wb_fire          , wb_req,
                 Mux(prober_fire      , prober_req,
                 Mux(prefetch_fire    , prefetch_req
                                      , replay_req))))

  // Does this request need to send a response or nack
  val s0_send_resp_or_nack = Mux(io.lsu.req.fire(),
                                 s0_valid,
                                 VecInit(Mux(mshrs.io.replay.fire() && isRead(mshrs.io.replay.bits.uop.mem_cmd), 1.U(memWidth.W), 0.U(memWidth.W)).asBools))


  val s1_req          = RegNext(s0_req)
  for (i <- 0 until memWidth)
    s1_req(i).uop.br_mask := GetNewBrMask(io.lsu.brinfo, s0_req(i).uop)
  val s2_store_failed = Wire(Bool())
  val s1_valid = widthMap( i =>
                   RegNext(s0_valid(i)                                     &&
                           !IsKilledByBranch(io.lsu.brinfo, s0_req(i).uop) &&
                           !(io.lsu.exception && s0_req(i).uop.uses_ldq)   &&
                           !(s2_store_failed && io.lsu.req.fire() && s0_req(i).uop.uses_stq),
                           init=false.B))
  for (i <- 0 until memWidth) {
    assert(!(io.lsu.s1_kill(i) && !RegNext(io.lsu.req.fire()) && !RegNext(io.lsu.req.bits(i).valid)))
  }

  val s1_addr         = widthMap(i => s1_req(i).addr)
  val s1_nack         = widthMap(i => s1_req(i).addr(idxMSB,idxLSB) === prober.io.meta_write.bits.idx && !prober.io.req.ready)
  val s1_send_resp_or_nack = RegNext(s0_send_resp_or_nack)
  val s1_is_lsu        = RegNext(io.lsu.req.fire())
  val s1_is_probe      = RegNext(prober_fire)
  val s1_is_replay     = RegNext(mshrs.io.replay.fire())
  val s1_replay_way_en = RegNext(mshrs.io.replay.bits.way_en) // For replays, the metadata isn't written yet

  // tag check
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way    = widthMap(i => wayMap((w: Int) => meta(i).io.resp(w).tag === (s1_addr(i) >> untagBits)).asUInt)
  val s1_tag_match_way = widthMap(i => Mux(s1_is_replay,
                                           s1_replay_way_en,
                                           wayMap((w: Int) => s1_tag_eq_way(i)(w) && meta(i).io.resp(w).coh.isValid()).asUInt))


  val s2_req   = RegNext(s1_req)
  val s2_valid = widthMap(i =>
                 RegNext(s1_valid(i)                                     &&
                         !io.lsu.s1_kill(i)                              &&
                         !IsKilledByBranch(io.lsu.brinfo, s1_req(i).uop) &&
                         !(io.lsu.exception && s1_req(i).uop.uses_ldq)   &&
                         !(s2_store_failed && s1_is_lsu && s1_req(i).uop.uses_stq)))
  for (i <- 0 until memWidth)
    s2_req(i).uop.br_mask := GetNewBrMask(io.lsu.brinfo, s1_req(i).uop)
  val s2_is_lsu    = RegNext(s1_is_lsu)
  val s2_is_probe  = RegNext(s1_is_probe)
  val s2_is_replay = RegNext(s1_is_replay)


  val s2_tag_match_way  = RegNext(s1_tag_match_way)
  val s2_tag_match      = widthMap(i => s2_tag_match_way(i).orR)
  val s2_hit_state      = widthMap(i => Mux1H(s2_tag_match_way(i), wayMap((w: Int) => RegNext(meta(i).io.resp(w).coh))))
  val s2_has_permission = widthMap(i => s2_hit_state(i).onAccess(s2_req(i).uop.mem_cmd)._1)
  val s2_new_hit_state  = widthMap(i => s2_hit_state(i).onAccess(s2_req(i).uop.mem_cmd)._3)

  val s2_hit = widthMap(i => ((s2_tag_match(i)                         &&
                               s2_has_permission(i)                    &&
                               s2_hit_state(i) === s2_new_hit_state(i) &&
                               !mshrs.io.block_hit(i)) || s2_is_replay) && s2_valid(i))

  assert(!(s2_is_replay && (s2_hit.asUInt =/= 1.U)), "Replays should always hit")

  // lr/sc
  val lrsc_count = RegInit(0.U(log2Ceil(lrscCycles).W))
  val lrsc_valid = lrsc_count > lrscBackoff.U
  val lrsc_addr  = Reg(UInt())
  val (s2_lr, s2_sc) = (s2_req(0).uop.mem_cmd === M_XLR, s2_req(0).uop.mem_cmd === M_XSC)
  for (i <- 0 until memWidth) {
    assert((i == 0).B || (s2_req(i).uop.mem_cmd =/= M_XLR && s2_req(i).uop.mem_cmd =/= M_XSC),
      "LR/SC has to go through the first pipe")
  }
  val s2_lrsc_addr_match = lrsc_valid && lrsc_addr === (s2_req(0).addr >> blockOffBits)
  val s2_sc_fail = s2_sc && !s2_lrsc_addr_match
  when (lrsc_count > 0.U) { lrsc_count := lrsc_count - 1.U }
  when ((s2_valid(0) && s2_hit(0)) || (s2_is_replay && s2_req(0).uop.mem_cmd =/= M_FLUSH_ALL)) {
    when (s2_lr) {
      lrsc_count := (lrscCycles - 1).U
      lrsc_addr := s2_req(0).addr >> blockOffBits
    }
    when (lrsc_count > 0.U) {
      lrsc_count := 0.U
    }
  }

  val s2_data = Wire(Vec(memWidth, Vec(nWays, UInt(encRowBits.W))))
  for (j <- 0 until memWidth) {
    for (w <- 0 until nWays) {
      val regs = Reg(Vec(rowWords, UInt(encDataBits.W)))
      for (i <- 0 until rowWords) {
        regs(i) := data(j).io.resp(w) >> encDataBits*i
      }
      s2_data(j)(w) := regs.asUInt
    }
  }
  require(doNarrowRead)
  val s2_data_muxed = widthMap(i => Mux1H(s2_tag_match_way(i), s2_data(i)))
  val s2_word_idx   = widthMap(i => if (doNarrowRead) 0.U else s2_req(i).addr(log2Up(rowWords*coreDataBytes)-1, log2Up(wordBytes)))

  // replacement policy
  val replacer = cacheParams.replacement
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegNext(replacer.way))
  val s2_repl_meta = widthMap(i => Mux1H(s2_replaced_way_en, wayMap((w: Int) => RegNext(meta(i).io.resp(w))).toSeq))

  val s2_nack_hit    = RegNext(s1_nack) // nack because of incoming probe
  val s2_nack_victim = widthMap(i => s2_valid(i) &&  s2_hit(i) && mshrs.io.secondary_miss(i)) // Nack when we hit something currently being evicted
  val s2_nack_miss   = widthMap(i => s2_valid(i) && !s2_hit(i) && !mshrs.io.req(i).ready) // MSHRs not ready for request
  val s2_nack        = widthMap(i => (s2_nack_miss(i) || s2_nack_hit(i) || s2_nack_victim(i)) && !s2_is_replay)
  val s2_send_resp   = widthMap(i => RegNext(s1_send_resp_or_nack(i)) &&
                                     !s2_nack(i)                   &&
                                     (s2_hit(i) || (mshrs.io.req(i).fire() && isWrite(s2_req(i).uop.mem_cmd) && !isRead(s2_req(i).uop.mem_cmd))))
  val s2_send_nack   = widthMap(i => RegNext(s1_send_resp_or_nack(i)) && s2_nack(i))
  for (i <- 0 until memWidth)
    assert(!(s2_send_resp(i) && s2_send_nack(i)))

  // hits always send a response
  // If MSHR is not available, LSU has to replay this request later
  // If MSHR is available and this is only a store(not a amo), we don't need to wait for resp later
  s2_store_failed := s2_valid(0) && s2_nack(0) && s2_send_nack(0) && s2_req(0).uop.uses_stq

  // Miss handling
  for (i <- 0 until memWidth) {
    mshrs.io.req(i).valid := s2_valid(i)          &&
                            !RegNext(s1_nack(i))  &&
                            !s2_hit(i)            &&
                            !s2_is_probe          &&
                            !IsKilledByBranch(io.lsu.brinfo, s2_req(i).uop) &&
                            !(io.lsu.exception && s2_req(i).uop.uses_ldq)   &&
                            (isPrefetch(s2_req(i).uop.mem_cmd) ||
                             isRead(s2_req(i).uop.mem_cmd)     ||
                             isWrite(s2_req(i).uop.mem_cmd))
    assert(!(mshrs.io.req(i).valid && s2_is_replay), "Replays should not need to go back into MSHRs")
    mshrs.io.req(i).bits.uop         := s2_req(i).uop
    mshrs.io.req(i).bits.uop.br_mask := GetNewBrMask(io.lsu.brinfo, s2_req(i).uop)
    mshrs.io.req(i).bits.addr        := s2_req(i).addr
    mshrs.io.req(i).bits.tag_match   := s2_tag_match(i)
    mshrs.io.req(i).bits.old_meta    := Mux(s2_tag_match(i), L1Metadata(s2_repl_meta(i).tag, s2_hit_state(i)), s2_repl_meta(i))
    mshrs.io.req(i).bits.way_en      := Mux(s2_tag_match(i), s2_tag_match_way(i), s2_replaced_way_en)
    mshrs.io.req(i).bits.sdq_id      := DontCare // this is set inside MSHR
    mshrs.io.req(i).bits.data        := s2_req(i).data
    mshrs.io.req(i).bits.is_hella    := s2_req(i).is_hella
    when (mshrs.io.req(i).fire()) { replacer.miss }
  }
  tl_out.a <> mshrs.io.mem_acquire

  // probes and releases
  prober.io.req.valid   := tl_out.b.valid
  tl_out.b.ready        := prober.io.req.ready
  prober.io.req.bits    := tl_out.b.bits
  prober.io.way_en      := s2_tag_match_way(0)
  prober.io.block_state := s2_hit_state(0)
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
  wb.io.data_resp      := s2_data_muxed(0)
  TLArbiter.lowest(edge, tl_out.c, wb.io.release, prober.io.rep)

  // load data gen
  val s2_data_word_prebypass = widthMap(i => s2_data_muxed(i) >> Cat(s2_word_idx(i), 0.U(log2Ceil(coreDataBits).W)))
  val s2_data_word           = Wire(Vec(memWidth, UInt()))
  val loadgen = (0 until memWidth).map( i => new LoadGen(s2_req(i).uop.mem_size, s2_req(i).uop.mem_signed, s2_req(i).addr,
                                                         s2_data_word(i), s2_sc, wordBytes))

  // Mux between cache responses and uncache responses
  val cache_resp   = Wire(Vec(memWidth, Valid(new BoomDCacheResp)))
  for (i <- 0 until memWidth) {
    cache_resp(i).valid         := s2_valid(i) && s2_send_resp(i)
    cache_resp(i).bits.uop      := s2_req(i).uop
    cache_resp(i).bits.data     := loadgen(i).data | s2_sc_fail
    cache_resp(i).bits.is_hella := s2_req(i).is_hella
  }

  val uncache_resp = Wire(Valid(new BoomDCacheResp))
  uncache_resp.bits     := mshrs.io.resp.bits
  uncache_resp.valid    := mshrs.io.resp.valid

  var mshr_fired = false.B
  for (i <- 0 until memWidth) {
    val fire_mshr = !cache_resp(i).valid && !mshr_fired && uncache_resp.valid
    val resp_valid = cache_resp(i).valid || fire_mshr
    val resp = Mux(cache_resp(i).valid, cache_resp(i), uncache_resp)

    io.lsu.resp(i).valid := resp_valid                                   &&
                           !(io.lsu.exception && resp.bits.uop.uses_ldq) &&
                           !IsKilledByBranch(io.lsu.brinfo, resp.bits.uop)
    io.lsu.resp(i).bits  := UpdateBrMask(io.lsu.brinfo, resp.bits)

    io.lsu.nack(i).valid := s2_valid(i) && s2_send_nack(i)               &&
                           !(io.lsu.exception && s2_req(i).uop.uses_ldq) &&
                           !IsKilledByBranch(io.lsu.brinfo, s2_req(i).uop)
    io.lsu.nack(i).bits  := UpdateBrMask(io.lsu.brinfo, s2_req(i))
    assert(!(io.lsu.nack(i).valid && !s2_is_lsu))

    mshr_fired = mshr_fired || fire_mshr
  }
  mshrs.io.resp.ready := mshr_fired

  // Store/amo hits
  // Stores/AMOs only go through the 0th pipe
  val s3_req   = RegNext(s2_req(0))
  val s3_valid = RegNext(s2_valid(0) && s2_hit(0) && isWrite(s2_req(0).uop.mem_cmd) &&
                         !s2_sc_fail && !(s2_send_nack(0) && s2_nack(0)))
  // For bypassing
  val s4_req   = RegNext(s3_req)
  val s4_valid = RegNext(s3_valid)
  val s5_req   = RegNext(s4_req)
  val s5_valid = RegNext(s4_valid)

  // TODO: Is this the right time to bypass?
  val s3_bypass = widthMap(i => s3_valid && ((s2_req(i).addr >> wordOffBits) === (s3_req.addr >> wordOffBits)))
  val s4_bypass = widthMap(i => s4_valid && ((s2_req(i).addr >> wordOffBits) === (s4_req.addr >> wordOffBits)))
  val s5_bypass = widthMap(i => s5_valid && ((s2_req(i).addr >> wordOffBits) === (s5_req.addr >> wordOffBits)))

  // Store -> Load bypassing
  s2_data_word := widthMap(i => Mux(s3_bypass(i), s3_req.data,
                                Mux(s4_bypass(i), s4_req.data,
                                Mux(s5_bypass(i), s5_req.data,
                                                  s2_data_word_prebypass(i)))))
  val amoalu   = Module(new AMOALU(xLen))
  amoalu.io.mask := new StoreGen(s2_req(0).uop.mem_size, s2_req(0).addr, 0.U, xLen/8).mask
  amoalu.io.cmd  := s2_req(0).uop.mem_cmd
  amoalu.io.lhs  := s2_data_word(0)
  amoalu.io.rhs  := s2_req(0).data


  s3_req.data := amoalu.io.out
  val s3_way   = RegNext(s2_tag_match_way(0))

  dataWriteArb.io.in(0).valid       := s3_valid
  dataWriteArb.io.in(0).bits.addr   := s3_req.addr
  dataWriteArb.io.in(0).bits.wmask  := UIntToOH(s3_req.addr.extract(rowOffBits-1,offsetlsb))
  dataWriteArb.io.in(0).bits.data   := Fill(rowWords, s3_req.data)
  dataWriteArb.io.in(0).bits.way_en := s3_way


  io.lsu.ordered := mshrs.io.fence_rdy && (s1_valid.asUInt === 0.U) && (s2_valid.asUInt === 0.U)
}
