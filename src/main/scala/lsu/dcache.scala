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

    val idx_match   = Output(Bool())
    val way_match   = Output(Bool())
    val tag_match   = Output(Bool())


    val mem_acquire = Decoupled(new TLBundleA(edge.bundle))

    val mem_grant   = Flipped(Valid(new TLBundleD(edge.bundle)))
    val mem_finish  = Decoupled(new TLBundleE(edge.bundle))

    val refill      = Decoupled(new L1DataWriteReq)

    val meta_write  = Decoupled(new L1MetaWriteReq)
    val meta_read   = Decoupled(new L1MetaReadReq)
    val meta_resp   = Input(Valid(new L1Metadata))
    val wb_req      = Decoupled(new WritebackReq(edge.bundle))

    // To inform the prefetcher when we are commiting the fetch of this line
    val commit_val  = Output(Bool())
    val commit_addr = Output(UInt(coreMaxAddrBits.W))
    val commit_cmd  = Output(UInt(M_SZ.W))

    // Replays go through the cache pipeline again
    val replay      = Decoupled(new BoomDCacheReqInternal)
    // Resp go straight out to the core
    val resp        = Decoupled(new BoomDCacheResp)

    // Writeback unit tells us when it is done processing our wb
    val wb_resp     = Input(Bool())

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

  val s_invalid :: s_refill_req :: s_refill_resp :: s_drain_rpq_loads :: s_meta_read :: s_meta_resp_1 :: s_meta_resp_2 :: s_meta_clear :: s_wb_meta_read :: s_wb_req :: s_wb_resp :: s_commit_line :: s_drain_rpq :: s_meta_write_req :: s_mem_finish :: Nil = Enum(15)
  val state = RegInit(s_invalid)

  val req     = Reg(new BoomDCacheReqInternal)
  val req_idx = req.addr(untagBits-1, blockOffBits)
  val req_tag = req.addr >> untagBits
  val req_block_addr = (req.addr >> blockOffBits) << blockOffBits
  val req_needs_wb = RegInit(false.B)
  val idx_match = req_idx === io.req.addr(untagBits-1, blockOffBits)
  val way_match = req.way_en === io.req.way_en
  val tag_match = req_tag === io.req.addr >> untagBits
  val tag_match_old = req.old_meta.tag === io.req.addr >> untagBits

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
                 !state.isOneOf(s_invalid, s_meta_write_req, s_mem_finish))// Always accept secondary misses

  val rpq = Module(new BranchKillableQueue(new BoomDCacheReqInternal, cfg.nRPQ, u => u.uses_ldq, false))
  rpq.io.brinfo := io.brinfo
  rpq.io.flush  := io.exception

  rpq.io.enq.valid := ((io.req_pri_val && io.req_pri_rdy) || (io.req_sec_val && io.req_sec_rdy)) && !isPrefetch(io.req.uop.mem_cmd)
  rpq.io.enq.bits  := io.req
  rpq.io.deq.ready := false.B


  val grantack = Reg(Valid(new TLBundleE(edge.bundle)))
  val load_buffer = Mem(cacheDataBeats,
                        UInt(encRowBits.W))
  val refill_ctr  = Reg(UInt(log2Ceil(cacheDataBeats).W))
  val commit_line = Reg(Bool())
  val grant_had_data = Reg(Bool())

  // Block probes if a tag write we started is still in the pipeline
  val meta_hazard = RegInit(0.U(2.W))
  when (meta_hazard =/= 0.U) { meta_hazard := meta_hazard + 1.U }
  when (io.meta_write.fire()) { meta_hazard := 1.U }
  io.probe_rdy   := (meta_hazard === 0.U && state.isOneOf(s_invalid, s_refill_req, s_refill_resp)) || !idx_match
  io.idx_match   := (state =/= s_invalid) && idx_match
  io.way_match   := !state.isOneOf(s_invalid, s_refill_req, s_refill_resp, s_drain_rpq_loads) && way_match
  io.tag_match   := (state =/= s_invalid) && tag_match
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
  io.meta_read.valid   := false.B
  io.mem_finish.valid  := false.B

  when (io.req_sec_val && io.req_sec_rdy) {
    req.uop.mem_cmd := dirtier_cmd
    when (is_hit_again) {
      new_coh := dirtier_coh
    }
  }

  when (state === s_invalid) {
    io.req_pri_rdy := true.B
    grant_had_data := false.B

    when (io.req_pri_val && io.req_pri_rdy) {
      grantack.valid := false.B
      refill_ctr := 0.U
      assert(rpq.io.enq.ready)
      req := io.req
      val old_coh   = io.req.old_meta.coh
      req_needs_wb := old_coh.onCacheControl(M_FLUSH)._1 // does the line we are evicting need to be written back
      val (is_hit, _, coh_on_hit) = old_coh.onAccess(io.req.uop.mem_cmd)

      when (io.req.tag_match) {
        when (is_hit) { // set dirty bit
          assert(isWrite(io.req.uop.mem_cmd))
          new_coh := coh_on_hit
          state   := s_drain_rpq
        } .otherwise { // upgrade permissions
          new_coh := old_coh
          state   := s_refill_req
        }
      } .otherwise { // refill and writeback if necessary
        new_coh := ClientMetadata.onReset
        state   := s_refill_req
      }
    }
  } .elsewhen (state === s_refill_req) {
    io.mem_acquire.valid := true.B
    // TODO: Use AcquirePerm if just doing permissions acquire
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
      grant_had_data := edge.hasData(io.mem_grant.bits)
      load_buffer(refill_address_inc >> rowOffBits) := io.mem_grant.bits.data
    }
    when (refill_done) {
      grantack.valid := edge.isRequest(io.mem_grant.bits)
      grantack.bits := edge.GrantAck(io.mem_grant.bits)
      state := Mux(grant_had_data, s_drain_rpq_loads, s_drain_rpq)
      assert(!(!grant_had_data && req_needs_wb))
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
      when (!rpq.io.enq.fire()) {
        state := s_mem_finish
      }
    } .elsewhen (rpq.io.empty || (rpq.io.deq.valid && !drain_load)) {
      io.commit_val := true.B
      state := s_meta_read
    }
  } .elsewhen (state === s_meta_read) {
    io.meta_read.valid := true.B
    io.meta_read.bits.idx := req_idx
    io.meta_read.bits.tag := req_tag
    io.meta_read.bits.way_en := req.way_en
    when (io.meta_read.fire()) {
      state := s_meta_resp_1
    }
  } .elsewhen (state === s_meta_resp_1) {
    state := s_meta_resp_2
  } .elsewhen (state === s_meta_resp_2) {
    val needs_wb = io.meta_resp.bits.coh.onCacheControl(M_FLUSH)._1
    state := Mux(!io.meta_resp.valid, s_meta_read, // Prober could have nack'd this read
             Mux(needs_wb, s_meta_clear, s_commit_line))
  } .elsewhen (state === s_meta_clear) {
    io.meta_write.valid         := true.B
    io.meta_write.bits.idx      := req_idx
    io.meta_write.bits.data.coh := coh_on_clear
    io.meta_write.bits.data.tag := req_tag
    io.meta_write.bits.way_en   := req.way_en

    when (io.meta_write.fire()) {
      state      := s_wb_req
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
    when (io.wb_resp) {
      state := s_commit_line
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
    when (io.replay.fire() && isWrite(rpq.io.deq.bits.uop.mem_cmd)) {
      // Set dirty big
      val (is_hit, _, coh_on_hit) = new_coh.onAccess(rpq.io.deq.bits.uop.mem_cmd)
      assert(is_hit, "We still don't have permissions for this store")
      new_coh := coh_on_hit
    }
    when (rpq.io.empty && !rpq.io.enq.valid) {
      state := s_meta_write_req
    }
  } .elsewhen (state === s_meta_write_req) {
    io.meta_write.valid         := true.B
    io.meta_write.bits.idx      := req_idx
    io.meta_write.bits.data.coh := new_coh
    io.meta_write.bits.data.tag := req_tag
    io.meta_write.bits.way_en   := req.way_en
    when (io.meta_write.fire()) {
      state := s_mem_finish
    }
  } .elsewhen (state === s_mem_finish) {
    io.mem_finish.valid := grantack.valid
    io.mem_finish.bits  := grantack.bits
    when (io.mem_finish.fire() || !grantack.valid) {
      grantack.valid := false.B
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
    val req  = Flipped(Decoupled(new BoomDCacheReqInternal))
    val resp = Decoupled(new BoomDCacheResp)
    val secondary_miss = Output(Bool())
    val block_hit = Output(Bool())

    val brinfo       = Input(new BrResolutionInfo)
    val exception    = Input(Bool())
    val rob_pnr_idx  = Input(UInt(robAddrSz.W))
    val rob_head_idx = Input(UInt(robAddrSz.W))

    val mem_acquire  = Decoupled(new TLBundleA(edge.bundle))
    val mem_grant    = Flipped(Valid(new TLBundleD(edge.bundle)))
    val mem_finish   = Decoupled(new TLBundleE(edge.bundle))

    val refill     = Decoupled(new L1DataWriteReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val meta_read  = Decoupled(new L1MetaReadReq)
    val meta_resp  = Input(Valid(new L1Metadata))
    val replay     = Decoupled(new BoomDCacheReqInternal)
    val prefetch   = Decoupled(new BoomDCacheReq)
    val wb_req     = Decoupled(new WritebackReq(edge.bundle))

    val clear_all = Input(Bool()) // Clears all uncommitted MSHRs to prepare for fence

    val wb_resp   = Input(Bool())

    val fence_rdy = Output(Bool())
    val probe_rdy = Output(Bool())
  })

  val prefetcher: DataPrefetcher = if (enablePrefetching) Module(new NLPrefetcher)
                                                     else Module(new NullPrefetcher)

  io.prefetch <> prefetcher.io.prefetch


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
  val tag_matches = Wire(Vec(cfg.nMSHRs, Bool()))
  val tag_match   = Mux1H(idx_matches, tag_matches)

  val wb_tag_list = Wire(Vec(cfg.nMSHRs, UInt(tagBits.W)))

  val meta_write_arb = Module(new Arbiter(new L1MetaWriteReq           , cfg.nMSHRs))
  val meta_read_arb  = Module(new Arbiter(new L1MetaReadReq            , cfg.nMSHRs))
  val wb_req_arb     = Module(new Arbiter(new WritebackReq(edge.bundle), cfg.nMSHRs))
  val replay_arb     = Module(new Arbiter(new BoomDCacheReqInternal    , cfg.nMSHRs))
  val resp_arb       = Module(new Arbiter(new BoomDCacheResp           , cfg.nMSHRs + nIOMSHRs))
  val refill_arb     = Module(new Arbiter(new L1DataWriteReq           , cfg.nMSHRs))

  val commit_vals    = Wire(Vec(cfg.nMSHRs, Bool()))
  val commit_addrs   = Wire(Vec(cfg.nMSHRs, UInt(coreMaxAddrBits.W)))
  val commit_cmds    = Wire(Vec(cfg.nMSHRs, UInt(M_SZ.W)))

  var way_match = false.B
  var sec_rdy   = false.B

  io.fence_rdy := true.B
  io.probe_rdy := true.B

  val idx_match = idx_matches.reduce(_||_)
  val mshr_alloc_idx = Wire(UInt())
  val pri_rdy = WireInit(false.B)
  val pri_val = io.req.valid && sdq_rdy && cacheable && !idx_match
  val mshrs = (0 until cfg.nMSHRs) map { i =>
    val mshr = Module(new BoomMSHR(i))

    idx_matches(i) := mshr.io.idx_match
    tag_matches(i) := mshr.io.tag_match
    wb_tag_list(i) := mshr.io.wb_req.bits.tag



    mshr.io.req_pri_val  := (i.U === mshr_alloc_idx) && pri_val
    when (i.U === mshr_alloc_idx) {
      pri_rdy := mshr.io.req_pri_rdy
    }

    mshr.io.req_sec_val  := io.req.valid && sdq_rdy && tag_match && cacheable
    mshr.io.req          := io.req.bits
    mshr.io.req.sdq_id   := sdq_alloc_id

    mshr.io.brinfo       := io.brinfo
    mshr.io.exception    := io.exception
    mshr.io.rob_pnr_idx  := io.rob_pnr_idx
    mshr.io.rob_head_idx := io.rob_head_idx

    mshr.io.wb_resp      := io.wb_resp

    meta_write_arb.io.in(i) <> mshr.io.meta_write
    meta_read_arb.io.in(i)  <> mshr.io.meta_read
    mshr.io.meta_resp       := io.meta_resp
    wb_req_arb.io.in(i)     <> mshr.io.wb_req
    replay_arb.io.in(i)     <> mshr.io.replay
    refill_arb.io.in(i)     <> mshr.io.refill

    commit_vals(i)  := mshr.io.commit_val
    commit_addrs(i) := mshr.io.commit_addr
    commit_cmds(i)  := mshr.io.commit_cmd

    mshr.io.mem_grant.valid := io.mem_grant.valid && io.mem_grant.bits.source === i.U
    mshr.io.mem_grant.bits  := io.mem_grant.bits

    sec_rdy   = sec_rdy || mshr.io.req_sec_rdy
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

  // Try to round-robin the MSHRs
  val mshr_head      = RegInit(0.U(log2Ceil(cfg.nMSHRs).W))
  mshr_alloc_idx    := RegNext(AgePriorityEncoder(mshrs.map(m=>m.io.req_pri_rdy), mshr_head))
  when (pri_rdy && pri_val) { mshr_head := WrapInc(mshr_head, cfg.nMSHRs) }



  io.meta_write <> meta_write_arb.io.out
  io.meta_read  <> meta_read_arb.io.out
  io.wb_req     <> wb_req_arb.io.out

  val mmio_alloc_arb = Module(new Arbiter(Bool(), nIOMSHRs))


  var mmio_rdy = false.B

  val mmios = (0 until nIOMSHRs) map { i =>
    val id = cfg.nMSHRs + 1 + i // +1 for wb unit
    val mshr = Module(new BoomIOMSHR(id))

    mmio_alloc_arb.io.in(i).valid := mshr.io.req.ready
    mmio_alloc_arb.io.in(i).bits  := DontCare
    mshr.io.req.valid := mmio_alloc_arb.io.in(i).ready
    mshr.io.req.bits  := io.req.bits

    mmio_rdy = mmio_rdy || mshr.io.req.ready

    mshr.io.mem_ack.bits  := io.mem_grant.bits
    mshr.io.mem_ack.valid := io.mem_grant.valid && io.mem_grant.bits.source === id.U

    resp_arb.io.in(cfg.nMSHRs + i) <> mshr.io.resp
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
  io.secondary_miss := idx_match && way_match && !tag_match
  io.block_hit      := idx_match && tag_match
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


class BoomWritebackUnit(implicit edge: TLEdgeOut, p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val req = Flipped(Decoupled(new WritebackReq(edge.bundle)))
    val meta_read = Decoupled(new L1MetaReadReq)
    val resp = Output(Bool())
    val idx = Output(Valid(UInt()))
    val data_req = Decoupled(new L1DataReadReq)
    val data_resp = Input(UInt(encRowBits.W))
    val mem_grant = Input(Bool())
    val release = Decoupled(new TLBundleC(edge.bundle))
  }

  val req = Reg(new WritebackReq(edge.bundle))
  val s_invalid :: s_fill_buffer :: s_active :: s_grant :: Nil = Enum(4)
  val state = RegInit(s_invalid)
  val r1_data_req_fired = RegInit(false.B)
  val r2_data_req_fired = RegInit(false.B)
  val r1_data_req_cnt = Reg(UInt(log2Up(refillCycles+1).W))
  val r2_data_req_cnt = Reg(UInt(log2Up(refillCycles+1).W))
  val data_req_cnt = RegInit(0.U(log2Up(refillCycles+1).W))
  val (_, last_beat, all_beats_done, beat_count) = edge.count(io.release)
  val wb_buffer = Reg(Vec(refillCycles, UInt(encRowBits.W)))

  io.idx.valid       := state =/= s_invalid
  io.idx.bits        := req.idx
  io.release.valid   := false.B
  io.release.bits    := DontCare
  io.req.ready       := false.B
  io.meta_read.valid := false.B
  io.meta_read.bits  := DontCare
  io.data_req.valid  := false.B
  io.data_req.bits   := DontCare
  io.resp       := false.B

  when (state === s_invalid) {
    io.req.ready := true.B
    when (io.req.fire()) {
      state := s_fill_buffer
      data_req_cnt := 0.U
      req := io.req.bits
    }
  } .elsewhen (state === s_fill_buffer) {
    io.meta_read.valid := data_req_cnt < refillCycles.U
    io.meta_read.bits.idx := req.idx
    io.meta_read.bits.tag := req.tag

    io.data_req.valid := data_req_cnt < refillCycles.U
    io.data_req.bits.way_en := req.way_en
    io.data_req.bits.addr := (if(refillCycles > 1)
                              Cat(req.idx, data_req_cnt(log2Up(refillCycles)-1,0))
                            else req.idx) << rowOffBits

    r1_data_req_fired := false.B
    r1_data_req_cnt   := 0.U
    r2_data_req_fired := r1_data_req_fired
    r2_data_req_cnt   := r1_data_req_cnt
    when (io.data_req.fire() && io.meta_read.fire()) {
      r1_data_req_fired := true.B
      r1_data_req_cnt   := data_req_cnt
      data_req_cnt := data_req_cnt + 1.U
    }
    when (r2_data_req_fired) {
      wb_buffer(r2_data_req_cnt) := io.data_resp
      when (r2_data_req_cnt === (refillCycles-1).U) {
        io.resp := true.B
        state := s_active
        data_req_cnt := 0.U
      }
    }
  } .elsewhen (state === s_active) {
    io.release.valid := data_req_cnt < refillCycles.U

    val r_address = Cat(req.tag, req.idx) << blockOffBits
    val id = cfg.nMSHRs
    val probeResponse = edge.ProbeAck(
                          fromSource = id.U,
                          toAddress = r_address,
                          lgSize = lgCacheBlockBytes.U,
                          reportPermissions = req.param,
                          data = wb_buffer(data_req_cnt))

    val voluntaryRelease = edge.Release(
                          fromSource = id.U,
                          toAddress = r_address,
                          lgSize = lgCacheBlockBytes.U,
                          shrinkPermissions = req.param,
                          data = wb_buffer(data_req_cnt))._2

    io.release.bits := Mux(req.voluntary, voluntaryRelease, probeResponse)

    when (io.release.fire()) {
      data_req_cnt := data_req_cnt + 1.U
    }
    when ((data_req_cnt === (refillCycles-1).U) && io.release.fire()) {
      state := Mux(io.mem_grant || !req.voluntary, s_invalid, s_grant)
    }
  } .elsewhen (state === s_grant) {
    when (io.mem_grant) {
      state := s_invalid
    }
  }
}

class BoomProbeUnit(implicit edge: TLEdgeOut, p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val req = Flipped(Decoupled(new TLBundleB(edge.bundle)))
    val rep = Decoupled(new TLBundleC(edge.bundle))
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val wb_req = Decoupled(new WritebackReq(edge.bundle))
    val way_en = Input(UInt(nWays.W))
    val wb_rdy = Input(Bool()) // Is writeback unit currently busy? If so need to retry meta read when its done
    val mshr_rdy = Input(Bool()) // Is MSHR ready for this request to proceed?
    val mshr_wb_rdy = Output(Bool()) // Should we block MSHR writebacks while we finish our own?
    val block_state = Input(new ClientMetadata())
  }

  val (s_invalid :: s_meta_read :: s_meta_resp :: s_mshr_req ::
       s_mshr_resp :: s_release :: s_writeback_req :: s_writeback_resp ::
       s_meta_write :: s_meta_write_resp :: Nil) = Enum(10)
  val state = RegInit(s_invalid)

  val req = Reg(new TLBundleB(edge.bundle))
  val req_idx = req.address(idxMSB, idxLSB)
  val req_tag = req.address >> untagBits

  val way_en = Reg(UInt())
  val tag_matches = way_en.orR
  val old_coh = Reg(new ClientMetadata)
  val miss_coh = ClientMetadata.onReset
  val reply_coh = Mux(tag_matches, old_coh, miss_coh)
  val (is_dirty, report_param, new_coh) = reply_coh.onProbe(req.param)

  io.req.ready := state === s_invalid
  io.rep.valid := state === s_release
  io.rep.bits := edge.ProbeAck(req, report_param)

  assert(!io.rep.valid || !edge.hasData(io.rep.bits),
    "ProbeUnit should not send ProbeAcks with data, WritebackUnit should handle it")

  io.meta_read.valid := state === s_meta_read
  io.meta_read.bits.idx := req_idx
  io.meta_read.bits.tag := req_tag

  io.meta_write.valid := state === s_meta_write
  io.meta_write.bits.way_en := way_en
  io.meta_write.bits.idx := req_idx
  io.meta_write.bits.data.tag := req_tag
  io.meta_write.bits.data.coh := new_coh

  io.wb_req.valid := state === s_writeback_req
  io.wb_req.bits.source := req.source
  io.wb_req.bits.idx := req_idx
  io.wb_req.bits.tag := req_tag
  io.wb_req.bits.param := report_param
  io.wb_req.bits.way_en := way_en
  io.wb_req.bits.voluntary := false.B

  io.mshr_wb_rdy := !state.isOneOf(s_release, s_writeback_req, s_writeback_resp, s_meta_write, s_meta_write_resp)



  // state === s_invalid
  when (io.req.fire()) {
    state := s_meta_read
    req := io.req.bits
  }

  // state === s_meta_read
  when (io.meta_read.fire()) {
    state := s_meta_resp
  }

  // we need to wait one cycle for the metadata to be read from the array
  when (state === s_meta_resp) {
    state := s_mshr_req
  }

  when (state === s_mshr_req) {
    old_coh := io.block_state
    way_en := io.way_en
    // if the read didn't go through, we need to retry
    state := Mux(io.mshr_rdy && io.wb_rdy, s_mshr_resp, s_meta_read)
  }

  when (state === s_mshr_resp) {
    state := Mux(tag_matches && is_dirty, s_writeback_req, s_release)
  }

  when (state === s_release && io.rep.ready) {
    state := Mux(tag_matches, s_meta_write, s_invalid)
  }

  // state === s_writeback_req
  when (io.wb_req.fire()) {
    state := s_writeback_resp
  }

  // wait for the writeback request to finish before updating the metadata
  when (state === s_writeback_resp && io.wb_req.ready) {
    state := s_meta_write
  }

  when (io.meta_write.fire()) {
    state := s_meta_write_resp
  }

  when (state === s_meta_write_resp) {
    state := s_invalid
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
    sourceId      = IdRange(0, 1 max (cfg.nMSHRs + 1)),
    supportsProbe = TransferSizes(cfg.blockBytes, cfg.blockBytes))))

  protected def mmioClientParameters = Seq(TLClientParameters(
    name          = s"Core ${hartid} DCache MMIO",
    sourceId      = IdRange(cfg.nMSHRs + 1, cfg.nMSHRs + 1 + cfg.nMMIOs),
    requestFifo   = true))

  val node = TLClientNode(Seq(TLClientPortParameters(
    cacheClientParameters ++ mmioClientParameters,
    minLatency = 1)))

  lazy val module = new BoomNonBlockingDCacheModule(this)

  def flushOnFenceI = cfg.scratch.isEmpty && !node.edges.out(0).manager.managers.forall(m => !m.supportsAcquireT || !m.executable || m.regionType >= RegionType.TRACKED || m.regionType <= RegionType.IDEMPOTENT)

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

  private val fifoManagers = edge.manager.managers.filter(TLFIFOFixer.allVolatile)
  fifoManagers.foreach { m =>
    require (m.fifoId == fifoManagers.head.fifoId,
      s"IOMSHRs must be FIFO for all regions with effects, but HellaCache sees ${m.nodePath.map(_.name)}")
  }

  val wb = Module(new BoomWritebackUnit)
  val prober = Module(new BoomProbeUnit)
  val mshrs = Module(new BoomMSHRFile)
  mshrs.io.clear_all    := io.lsu.force_order
  mshrs.io.brinfo       := io.lsu.brinfo
  mshrs.io.exception    := io.lsu.exception
  mshrs.io.rob_pnr_idx  := io.lsu.rob_pnr_idx
  mshrs.io.rob_head_idx := io.lsu.rob_head_idx

  // tags
  def onReset = L1Metadata(0.U, ClientMetadata.onReset)
  val meta = Module(new L1MetadataArray(onReset _))
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, 2))
  // 0 goes to MSHR refills, 1 goes to prober
  val metaReadArb = Module(new Arbiter(new L1MetaReadReq, 6))
  // 0 goes to MSHR replays, 1 goes to prober, 2 goes to wb, 3 goes to MSHR meta read,
  // 4 goes to pipeline, 5 goes to prefetcher
  meta.io.write <> metaWriteArb.io.out
  meta.io.read  <> metaReadArb.io.out

  // data
  val data = Module(new DataArray)
  val dataWriteArb = Module(new Arbiter(new L1DataWriteReq, 2))
  // 0 goes to pipeline, 1 goes to MSHR refills
  val dataReadArb = Module(new Arbiter(new L1DataReadReq, 3))
  // 0 goes to MSHR replays, 1 goes to wb, 2 goes to pipeline

  data.io.write <> dataWriteArb.io.out
  data.io.read  <> dataReadArb.io.out

  // ------------
  // New requests

  io.lsu.req.ready := metaReadArb.io.in(4).ready && dataReadArb.io.in(2).ready
  // Tag read for new requests
  metaReadArb.io.in(4).valid       := io.lsu.req.valid
  metaReadArb.io.in(4).bits.idx    := io.lsu.req.bits.addr >> blockOffBits
  metaReadArb.io.in(4).bits.way_en := DontCare
  metaReadArb.io.in(4).bits.tag    := DontCare
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
  // MSHR Meta read
  val mshr_read_req = Wire(new BoomDCacheReq)
  mshr_read_req.uop      := NullMicroOp
  mshr_read_req.addr     := Cat(mshrs.io.meta_read.bits.tag, mshrs.io.meta_read.bits.idx) << blockOffBits
  mshr_read_req.data     := DontCare
  mshr_read_req.is_hella := false.B
  metaReadArb.io.in(3)   <> mshrs.io.meta_read

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

  // -------
  // Prefetcher
  val prefetch_fire = mshrs.io.prefetch.fire()
  val prefetch_req  = mshrs.io.prefetch.bits
  // Tag read for prefetch
  metaReadArb.io.in(5).valid       := mshrs.io.prefetch.valid
  metaReadArb.io.in(5).bits.idx    := mshrs.io.prefetch.bits.addr >> blockOffBits
  metaReadArb.io.in(5).bits.way_en := DontCare
  metaReadArb.io.in(5).bits.tag    := DontCare
  mshrs.io.prefetch.ready := metaReadArb.io.in(5).ready
  // Prefetch does not need to read data array

  val s0_valid = io.lsu.req.fire() || mshrs.io.replay.fire() || wb_fire || prober_fire || prefetch_fire || mshrs.io.meta_read.fire()
  val s0_req   = Mux(io.lsu.req.fire()        , io.lsu.req.bits,
                 Mux(wb_fire                  , wb_req,
                 Mux(prober_fire              , prober_req,
                 Mux(prefetch_fire            , prefetch_req,
                 Mux(mshrs.io.meta_read.fire(), mshr_read_req
                                              , replay_req)))))
  val s0_send_resp_or_nack = io.lsu.req.fire() || (mshrs.io.replay.fire() && isRead(mshrs.io.replay.bits.uop.mem_cmd)) // Does this request need to send a response or nack

  val s1_req          = RegNext(s0_req)
  s1_req.uop.br_mask := GetNewBrMask(io.lsu.brinfo, s0_req.uop)
  val s2_store_failed = Wire(Bool())
  val s1_valid = RegNext(s0_valid                                     &&
                         !IsKilledByBranch(io.lsu.brinfo, s0_req.uop) &&
                         !(io.lsu.exception && s0_req.uop.uses_ldq)   &&
                         !(s2_store_failed && io.lsu.req.fire() && s0_req.uop.uses_stq),
                         init=false.B)

  assert(!(io.lsu.s1_kill && !RegNext(io.lsu.req.fire())))
  val s1_addr         = s1_req.addr
  val s1_nack         = s1_req.addr(idxMSB,idxLSB) === prober.io.meta_write.bits.idx && !prober.io.req.ready
  val s1_send_resp_or_nack = RegNext(s0_send_resp_or_nack)
  val s1_is_lsu        = RegNext(io.lsu.req.fire()) // TODO make this a bundle
  val s1_is_probe      = RegNext(prober_fire)
  val s1_is_replay     = RegNext(mshrs.io.replay.fire())
  val s1_is_wb         = RegNext(wb_fire)
  val s1_is_mshr_read  = RegNext(mshrs.io.meta_read.fire())
  val s1_mshr_meta_read_way_en = RegNext(mshrs.io.meta_read.bits.way_en)
  val s1_replay_way_en = RegNext(mshrs.io.replay.bits.way_en) // For replays, the metadata isn't written yet
  val s1_wb_way_en     = RegNext(wb.io.data_req.bits.way_en)

  // tag check
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta.io.resp(w).tag === (s1_addr >> untagBits)).asUInt
  val s1_tag_match_way = Mux(s1_is_replay, s1_replay_way_en,
                         Mux(s1_is_wb,     s1_wb_way_en,
                         Mux(s1_is_mshr_read, s1_mshr_meta_read_way_en,
                           wayMap((w: Int) => s1_tag_eq_way(w) && meta.io.resp(w).coh.isValid()).asUInt)))


  val s2_req   = RegNext(s1_req)
  val s2_valid = RegNext(s1_valid &&
                         !io.lsu.s1_kill &&
                         !IsKilledByBranch(io.lsu.brinfo, s1_req.uop) &&
                         !(io.lsu.exception && s1_req.uop.uses_ldq) &&
                         !(s2_store_failed && s1_is_lsu && s1_req.uop.uses_stq))
  s2_req.uop.br_mask := GetNewBrMask(io.lsu.brinfo, s1_req.uop)
  val s2_is_lsu    = RegNext(s1_is_lsu)
  val s2_is_probe  = RegNext(s1_is_probe)
  val s2_is_replay = RegNext(s1_is_replay)
  val s2_is_wb     = RegNext(s1_is_wb)


  val s2_tag_match_way = RegNext(s1_tag_match_way)
  val s2_tag_match = s2_tag_match_way.orR
  val s2_hit_state = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegNext(meta.io.resp(w).coh)))
  val (s2_has_permission, _, s2_new_hit_state) = s2_hit_state.onAccess(s2_req.uop.mem_cmd)

  val s2_hit = (s2_tag_match && (s2_has_permission && s2_hit_state === s2_new_hit_state) && !mshrs.io.block_hit) || s2_is_replay || s2_is_wb
  val s2_nack = Wire(Bool())
  assert(!(s2_is_replay && !s2_hit), "Replays should always hit")
  assert(!(s2_is_wb && !s2_hit), "Writeback should always see data hit")

  // lr/sc
  val lrsc_count = RegInit(0.U(log2Ceil(lrscCycles).W))
  val lrsc_valid = lrsc_count > lrscBackoff.U
  val lrsc_addr  = Reg(UInt())
  val s2_lr = s2_req.uop.mem_cmd === M_XLR && !RegNext(s1_nack)
  val s2_sc = s2_req.uop.mem_cmd === M_XSC && !RegNext(s1_nack)
  val s2_lrsc_addr_match = lrsc_valid && lrsc_addr === (s2_req.addr >> blockOffBits)
  val s2_sc_fail = s2_sc && !s2_lrsc_addr_match
  when (lrsc_count > 0.U) { lrsc_count := lrsc_count - 1.U }
  when ((s2_valid && s2_hit && !s2_nack) || (s2_is_replay && s2_req.uop.mem_cmd =/= M_FLUSH_ALL)) {
    when (s2_lr) {
      lrsc_count := (lrscCycles - 1).U
      lrsc_addr := s2_req.addr >> blockOffBits
    }
    when (lrsc_count > 0.U) {
      lrsc_count := 0.U
    }
  }
  when (s2_valid && !s2_hit && s2_lrsc_addr_match && !s2_nack) {
    lrsc_count := 0.U
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
  val s2_word_idx   = if (doNarrowRead) 0.U else s2_req.addr(log2Up(rowWords*coreDataBytes)-1, log2Up(wordBytes))

  // replacement policy
  val replacer = cacheParams.replacement
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegNext(replacer.way))
  val s2_repl_meta = Mux1H(s2_replaced_way_en, wayMap((w: Int) => RegNext(meta.io.resp(w))).toSeq)

  val s2_nack_hit    = RegNext(s1_nack) // nack because of incoming probe
  val s2_nack_victim = s2_valid &&  s2_hit && mshrs.io.secondary_miss // Nack when we hit something currently being evicted
  val s2_nack_miss   = s2_valid && !s2_hit && !mshrs.io.req.ready // MSHRs not ready for request
  s2_nack           := (s2_nack_miss || s2_nack_hit || s2_nack_victim) && !s2_is_replay
  val s2_send_resp = (RegNext(s1_send_resp_or_nack) && !s2_nack &&
                      (s2_hit || (mshrs.io.req.fire() && isWrite(s2_req.uop.mem_cmd) && !isRead(s2_req.uop.mem_cmd))))
  val s2_send_nack = (RegNext(s1_send_resp_or_nack) && s2_nack)
  assert(!(s2_send_resp && s2_send_nack))

  // hits always send a response
  // If MSHR is not available, LSU has to replay this request later
  // If MSHR is available and this is only a store(not a amo), we don't need to wait for resp later
  s2_store_failed := s2_valid && s2_nack && s2_send_nack && s2_req.uop.uses_stq

  // Miss handling
  mshrs.io.req.valid := s2_valid          &&
                        !RegNext(s1_nack) &&
                        !s2_hit           &&
                        !s2_is_probe      &&
                        !IsKilledByBranch(io.lsu.brinfo, s2_req.uop) &&
                        !(io.lsu.exception && s2_req.uop.uses_ldq) &&
                        (isPrefetch(s2_req.uop.mem_cmd) ||
                         isRead(s2_req.uop.mem_cmd) ||
                         isWrite(s2_req.uop.mem_cmd))
  assert(!(mshrs.io.req.valid && s2_is_replay), "Replays should not need to go back into MSHRs")
  mshrs.io.req.bits.uop         := s2_req.uop
  mshrs.io.req.bits.uop.br_mask := GetNewBrMask(io.lsu.brinfo, s2_req.uop)
  mshrs.io.req.bits.addr        := s2_req.addr
  mshrs.io.req.bits.tag_match   := s2_tag_match
  mshrs.io.req.bits.old_meta    := Mux(s2_tag_match, L1Metadata(s2_repl_meta.tag, s2_hit_state), s2_repl_meta)
  mshrs.io.req.bits.way_en      := Mux(s2_tag_match, s2_tag_match_way, s2_replaced_way_en)
  mshrs.io.req.bits.sdq_id      := DontCare // this is set inside MSHR
  mshrs.io.req.bits.data        := s2_req.data
  mshrs.io.req.bits.is_hella    := s2_req.is_hella
  mshrs.io.meta_resp.valid      := !s2_nack_hit || prober.io.mshr_wb_rdy
  mshrs.io.meta_resp.bits       := Mux1H(s2_tag_match_way, RegNext(meta.io.resp))
  when (mshrs.io.req.fire()) { replacer.miss }
  tl_out.a <> mshrs.io.mem_acquire

  // probes and releases
  prober.io.req.valid   := tl_out.b.valid && !lrsc_valid
  tl_out.b.ready        := prober.io.req.ready && !lrsc_valid
  prober.io.req.bits    := tl_out.b.bits
  prober.io.way_en      := s2_tag_match_way
  prober.io.block_state := s2_hit_state
  metaWriteArb.io.in(1) <> prober.io.meta_write
  prober.io.mshr_rdy    := mshrs.io.probe_rdy
  prober.io.wb_rdy      := (prober.io.meta_write.bits.idx =/= wb.io.idx.bits) || !wb.io.idx.valid

  // refills
  mshrs.io.mem_grant.valid := tl_out.d.fire() && tl_out.d.bits.source =/= cfg.nMSHRs.U
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
  wb.io.data_resp       := s2_data_muxed
  mshrs.io.wb_resp      := wb.io.resp
  wb.io.mem_grant       := tl_out.d.fire() && tl_out.d.bits.source === cfg.nMSHRs.U
  TLArbiter.lowest(edge, tl_out.c, wb.io.release, prober.io.rep)

  // load data gen
  val s2_data_word_prebypass = s2_data_muxed >> Cat(s2_word_idx, 0.U(log2Ceil(coreDataBits).W))
  val s2_data_word = Wire(UInt())
  val loadgen = new LoadGen(s2_req.uop.mem_size, s2_req.uop.mem_signed, s2_req.addr,
                            s2_data_word, s2_sc, wordBytes)

  // Mux between cache responses and uncache responses
  val cache_resp   = Wire(Valid(new BoomDCacheResp))
  cache_resp.valid         := s2_valid && s2_send_resp
  cache_resp.bits.uop      := s2_req.uop
  cache_resp.bits.data     := loadgen.data | s2_sc_fail
  cache_resp.bits.is_hella := s2_req.is_hella

  val uncache_resp = Wire(Valid(new BoomDCacheResp))
  uncache_resp.bits     := mshrs.io.resp.bits
  uncache_resp.valid    := mshrs.io.resp.valid
  mshrs.io.resp.ready := !cache_resp.valid // We can backpressure the MSHRs, but not cache hits

  val resp = Mux(mshrs.io.resp.fire(), uncache_resp, cache_resp)
  io.lsu.resp.valid := resp.valid &&
                       !(io.lsu.exception && resp.bits.uop.uses_ldq) &&
                       !IsKilledByBranch(io.lsu.brinfo, resp.bits.uop)
  io.lsu.resp.bits  := UpdateBrMask(io.lsu.brinfo, resp.bits)

  io.lsu.nack.valid := s2_valid && s2_send_nack &&
                       !(io.lsu.exception && s2_req.uop.uses_ldq) &&
                       !IsKilledByBranch(io.lsu.brinfo, s2_req.uop)
  io.lsu.nack.bits  := UpdateBrMask(io.lsu.brinfo, s2_req)
  assert(!(io.lsu.nack.valid && !s2_is_lsu))

  // Store/amo hits
  val s3_req   = RegNext(s2_req)
  val s3_valid = RegNext(s2_valid && s2_hit && isWrite(s2_req.uop.mem_cmd) &&
                         !s2_sc_fail && !(s2_send_nack && s2_nack))
  // For bypassing
  val s4_req   = RegNext(s3_req)
  val s4_valid = RegNext(s3_valid)
  val s5_req   = RegNext(s4_req)
  val s5_valid = RegNext(s4_valid)

  // TODO: Is this the right time to bypass?
  val s3_bypass = s3_valid && ((s2_req.addr >> wordOffBits) === (s3_req.addr >> wordOffBits))
  val s4_bypass = s4_valid && ((s2_req.addr >> wordOffBits) === (s4_req.addr >> wordOffBits))
  val s5_bypass = s5_valid && ((s2_req.addr >> wordOffBits) === (s5_req.addr >> wordOffBits))

  // Store -> Load bypassing
  s2_data_word := Mux(s3_bypass, s3_req.data,
                  Mux(s4_bypass, s4_req.data,
                  Mux(s5_bypass, s5_req.data,
                                 s2_data_word_prebypass)))
  val amoalu   = Module(new AMOALU(xLen))
  amoalu.io.mask := new StoreGen(s2_req.uop.mem_size, s2_req.addr, 0.U, xLen/8).mask
  amoalu.io.cmd  := s2_req.uop.mem_cmd
  amoalu.io.lhs  := s2_data_word
  amoalu.io.rhs  := s2_req.data


  s3_req.data := amoalu.io.out
  val s3_way   = RegNext(s2_tag_match_way)

  dataWriteArb.io.in(0).valid       := s3_valid
  dataWriteArb.io.in(0).bits.addr   := s3_req.addr
  dataWriteArb.io.in(0).bits.wmask  := UIntToOH(s3_req.addr.extract(rowOffBits-1,offsetlsb))
  dataWriteArb.io.in(0).bits.data   := Fill(rowWords, s3_req.data)
  dataWriteArb.io.in(0).bits.way_en := s3_way


  io.lsu.ordered := mshrs.io.fence_rdy && !s1_valid && !s2_valid
}
