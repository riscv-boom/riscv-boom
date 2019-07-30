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
    val drain_load = (isRead(rpq.io.deq.bits.uop.mem_cmd) &&
                     !isWrite(rpq.io.deq.bits.uop.mem_cmd) &&
                     (rpq.io.deq.bits.uop.mem_cmd =/= M_XLR)) // LR should go through replay
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
