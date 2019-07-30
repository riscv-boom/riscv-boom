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
  val acked = RegInit(false.B)

  io.idx.valid       := state =/= s_invalid
  io.idx.bits        := req.idx
  io.release.valid   := false.B
  io.release.bits    := DontCare
  io.req.ready       := false.B
  io.meta_read.valid := false.B
  io.meta_read.bits  := DontCare
  io.data_req.valid  := false.B
  io.data_req.bits   := DontCare
  io.resp            := false.B

  when (state === s_invalid) {
    io.req.ready := true.B
    when (io.req.fire()) {
      state := s_fill_buffer
      data_req_cnt := 0.U
      req := io.req.bits
      acked := false.B
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

    when (io.mem_grant) {
      acked := true.B
    }
    when (io.release.fire()) {
      data_req_cnt := data_req_cnt + 1.U
    }
    when ((data_req_cnt === (refillCycles-1).U) && io.release.fire()) {
      state := Mux(req.voluntary, s_grant, s_invalid)
    }
  } .elsewhen (state === s_grant) {
    when (io.mem_grant) {
      acked := true.B
    }
    when (acked) {
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
  val debug_sc_fail_addr = RegInit(0.U)
  val debug_sc_fail_cnt  = RegInit(0.U(8.W))

  val lrsc_count = RegInit(0.U(log2Ceil(lrscCycles).W))
  val lrsc_valid = lrsc_count > lrscBackoff.U
  val lrsc_addr  = Reg(UInt())
  val s2_lr = s2_req.uop.mem_cmd === M_XLR && (!RegNext(s1_nack) || s2_is_replay)
  val s2_sc = s2_req.uop.mem_cmd === M_XSC && (!RegNext(s1_nack) || s2_is_replay)
  val s2_lrsc_addr_match = lrsc_valid && lrsc_addr === (s2_req.addr >> blockOffBits)
  val s2_sc_fail = s2_sc && !s2_lrsc_addr_match
  when (lrsc_count > 0.U) { lrsc_count := lrsc_count - 1.U }
  when (s2_valid && ((s2_is_lsu && s2_hit && !s2_nack) ||
                     (s2_is_replay && s2_req.uop.mem_cmd =/= M_FLUSH_ALL))) {
    when (s2_lr) {
      lrsc_count := (lrscCycles - 1).U
      lrsc_addr := s2_req.addr >> blockOffBits
    }
    when (lrsc_count > 0.U) {
      lrsc_count := 0.U
    }
  }
  when (s2_valid && s2_is_lsu && !s2_hit && !(s2_has_permission && s2_tag_match) && s2_lrsc_addr_match && !s2_nack) {
    lrsc_count := 0.U
  }

  when (s2_valid) {
    when (s2_req.addr === debug_sc_fail_addr) {
      when (s2_sc_fail) {
        debug_sc_fail_cnt := debug_sc_fail_cnt + 1.U
      } .elsewhen (s2_sc) {
        debug_sc_fail_cnt := 0.U
      }
    } .otherwise {
      when (s2_sc_fail) {
        debug_sc_fail_addr := s2_req.addr
        debug_sc_fail_cnt  := 1.U
      }
    }
  }
  assert(debug_sc_fail_cnt < 100.U, "L1DCache failed too many SCs in a row")

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
                        s2_is_lsu         &&
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
