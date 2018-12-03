// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package boom.lsu

import Chisel._
import chisel3.experimental.dontTouch
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._
import boom.common._
import boom.util._
import boom.exu.BrResolutionInfo

class SecureHellaCacheArbiter(n: Int)(implicit p: Parameters) extends Module
{
  val io = new Bundle {
    val requestor = Vec(n, new SecureHellaCacheIO).flip
    val mem = new SecureHellaCacheIO
  }

  if (n == 1) {
    io.mem <> io.requestor.head
  } else {
    val s1_id = Reg(UInt())
    val s2_id = Reg(next=s1_id)

    io.mem.keep_clock_enabled := io.requestor.map(_.keep_clock_enabled).reduce(_||_)

    io.mem.req.valid := io.requestor.map(_.req.valid).reduce(_||_)
    io.requestor(0).req.ready := io.mem.req.ready
    for (i <- 1 until n)
      io.requestor(i).req.ready := io.requestor(i-1).req.ready && !io.requestor(i-1).req.valid

    for (i <- n-1 to 0 by -1) {
      val req = io.requestor(i).req
      def connect_s0() = {
        io.mem.req.bits.cmd := req.bits.cmd
        io.mem.req.bits.typ := req.bits.typ
        io.mem.req.bits.addr := req.bits.addr
        io.mem.req.bits.phys := req.bits.phys
        io.mem.req.bits.uop := req.bits.uop
        io.mem.req.bits.ignore_spec_info := req.bits.ignore_spec_info
        io.mem.req.bits.tag := Cat(req.bits.tag, UInt(i, log2Up(n)))
        s1_id := UInt(i)
      }
      def connect_s1() = {
        io.mem.s1_kill := io.requestor(i).s1_kill
        io.mem.s1_data := io.requestor(i).s1_data
      }
      def connect_s2() = {
        io.mem.s2_kill := io.requestor(i).s2_kill
      }

      if (i == n-1) {
        connect_s0()
        connect_s1()
        connect_s2()
      } else {
        when (req.valid) { connect_s0() }
        when (s1_id === UInt(i)) { connect_s1() }
        when (s2_id === UInt(i)) { connect_s2() }
      }
    }

    for (i <- 0 until n) {
      val resp = io.requestor(i).resp
      val tag_hit = io.mem.resp.bits.tag(log2Up(n)-1,0) === UInt(i)
      resp.valid := io.mem.resp.valid && tag_hit
      io.requestor(i).s2_xcpt := io.mem.s2_xcpt
      io.requestor(i).ordered := io.mem.ordered
      io.requestor(i).perf := io.mem.perf
      io.requestor(i).s2_nack := io.mem.s2_nack && s2_id === UInt(i)
      io.requestor(i).s2_nack_cause_raw := io.mem.s2_nack_cause_raw
      resp.bits := io.mem.resp.bits
      resp.bits.tag := io.mem.resp.bits.tag >> log2Up(n)

      io.requestor(i).replay_next := io.mem.replay_next
    }
  }
}

trait HasSpecData {
  val spec_data = UInt(width=64)
  val use_spec_data = Bool()
}

class SecureReplayInternal(implicit p: Parameters) extends ReplayInternal()(p)
    with HasSpecData

class SecureReplay(implicit p: Parameters) extends Replay()(p)
    with HasSpecData


class SecureHellaCacheReq(implicit p: Parameters) extends HellaCacheReq()(p) {
  val uop = new MicroOp()
  val ignore_spec_info = Bool()
}

class SecureHellaCacheIO(implicit p: Parameters) extends HellaCacheIO()(p) {
  override val req = Decoupled(new SecureHellaCacheReq)
}

class SpecInfo(implicit p: Parameters) extends BoomBundle()(p) {
  val brinfo = new BrResolutionInfo()
  val kill = Bool()
  val rob_pnr_idx = UInt(width=ROB_ADDR_SZ)
}

class SecureMSHRReq(implicit p: Parameters) extends MSHRReq()(p) {
  val uop = new MicroOp()
  val killed = Bool()
  val ignore_spec_info = Bool()
}


class SecureMSHRReqInternal(implicit p: Parameters) extends MSHRReqInternal()(p) {
  val uop = new MicroOp()
  val killed = Bool()
  val ignore_spec_info = Bool()
}

class SecureL1RefillReq(implicit p: Parameters) extends L1RefillReq()(p) {
  val data = UInt(OUTPUT, width=64)
}


class SecureMSHR(id: Int)(implicit edge: TLEdgeOut, p: Parameters) extends L1HellaCacheModule()(p) with HasBoomCoreParameters {
  val io = new Bundle {
    val req_pri_val    = Bool(INPUT)
    val req_pri_rdy    = Bool(OUTPUT)
    val req_sec_val    = Bool(INPUT)
    val req_sec_rdy    = Bool(OUTPUT)
    val req_bits       = new SecureMSHRReqInternal().asInput

    val idx_match       = Bool(OUTPUT)
    val tag             = Bits(OUTPUT, tagBits)

    val mem_acquire  = Decoupled(new TLBundleA(edge.bundle))
    val mem_grant = Valid(new TLBundleD(edge.bundle)).flip
    val mem_finish = Decoupled(new TLBundleE(edge.bundle))

    val refill = Decoupled(new SecureL1RefillReq().asOutput) // Data is bypassed
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val replay = Decoupled(new SecureReplayInternal)
    val wb_req = Decoupled(new WritebackReq(edge.bundle))
    val probe_rdy = Bool(OUTPUT)

    val debug_req = new SecureMSHRReqInternal().asOutput

    val brinfo = new BrResolutionInfo().asInput
    val kill = Bool(INPUT)
    val rob_pnr_idx = UInt(INPUT, width=ROB_ADDR_SZ)

    val victim_safe  = Bool(OUTPUT)
    val req_nacked   = Bool(INPUT)
    val evict_refill = Bool(INPUT)
  }

  val s_invalid :: s_wb_req :: s_wb_resp :: s_meta_clear :: s_refill_req :: s_refill_resp :: s_meta_write_req :: s_meta_write_resp :: s_drain_rpq_ld :: s_drain_rpq :: s_spec_wait :: s_commit_resp :: Nil = Enum(UInt(), 12)
  val state = Reg(init=s_invalid)
  val nonspeculative = Reg(Bool())  // Is the refill still speculative?
  val killed = Reg(Bool())          // Has the refill been killed by misspeculation since its initiation?

  val req = Reg(new SecureMSHRReqInternal())
  io.debug_req := req
  dontTouch(io.debug_req)
  val req_idx = req.addr(untagBits-1,blockOffBits)
  val req_tag = req.addr >> untagBits
  val req_block_addr = (req.addr >> blockOffBits) << blockOffBits
  val idx_match = req_idx === io.req_bits.addr(untagBits-1,blockOffBits)

  val data = Reg(Vec(8, UInt(width=64))) // TODO_sec: Find parameters that affect thes
  val spec_buf_ptr = UInt(width=3)

  val new_coh = Reg(init=ClientMetadata.onReset)
  val (_, shrink_param, coh_on_clear)    = req.old_meta.coh.onCacheControl(M_FLUSH)
  val grow_param                                  = new_coh.onAccess(req.cmd)._2
  val coh_on_grant                                = new_coh.onGrant(req.cmd, io.mem_grant.bits.param)
  // We only accept secondary misses if we haven't yet sent an Acquire to outer memory
  // or if the Acquire that was sent will obtain a Grant with sufficient permissions
  // to let us replay this new request. I.e. we don't handle multiple outstanding
  // Acquires on the same block for now.
  val (cmd_requires_second_acquire, is_hit_again, _, dirtier_coh, dirtier_cmd) =
    new_coh.onSecondaryAccess(req.cmd, io.req_bits.cmd)

  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)
  val commit_counter = RegInit(UInt(0, width=3))
  val commit_done = commit_counter === UInt(7) && io.refill.ready
  val sec_rdy = idx_match &&
                   ((state.isOneOf(s_refill_req, s_refill_resp) &&
                      !cmd_requires_second_acquire && !refill_done) || 
                   state === s_spec_wait)
  assert(!(io.mem_grant.valid && !(state === s_refill_resp || state === s_wb_resp)))
  val rpq = Module(new Queue(new SecureReplayInternal, cfg.nRPQ))
  rpq.io.enq.valid := (io.req_pri_val && io.req_pri_rdy || io.req_sec_val && sec_rdy) && !isPrefetch(io.req_bits.cmd)
  rpq.io.enq.bits := io.req_bits
  rpq.io.deq.ready := (io.replay.ready && state === s_drain_rpq) ||
                       state === s_invalid ||
                      (io.replay.ready && state === s_drain_rpq_ld && isRead(rpq.io.deq.bits.cmd))
  val store_enqueued = Reg(Bool())
  val waiting_load = (rpq.io.deq.valid && isRead(rpq.io.deq.bits.cmd)) || (!rpq.io.deq.valid && rpq.io.enq.valid && isRead(rpq.io.enq.bits.cmd)) 

  when (state === s_drain_rpq_ld && !waiting_load) {
    state := s_spec_wait
  }
  when (state === s_drain_rpq && !rpq.io.deq.valid) {
    state := s_invalid
  }
  when (state === s_meta_write_resp) {
    // this wait state allows us to catch RAW hazards on the tags via nack_victim
    state := s_drain_rpq
  }
  when (state === s_meta_write_req && io.meta_write.ready) {
    state := s_meta_write_resp
  }
  when (state === s_refill_resp && refill_done) {
    new_coh := coh_on_grant
    state := s_drain_rpq_ld
  }
  when (state === s_spec_wait) {
    val needs_wb = req.old_meta.coh.onCacheControl(M_FLUSH)._1
    val next_state = Mux(needs_wb, s_wb_req, s_meta_clear)
    when (waiting_load) {
      state := s_drain_rpq_ld  // Drain the rpq if a load has been enqueued while waiting for speculation to resolve.
    }.elsewhen (store_enqueued) {
      state := next_state      // A store to this cache block guarantees the refill is nonspeculative.
    }.elsewhen (killed && !req.ignore_spec_info) {
      state := s_invalid       // Kill the refill.
    }.elsewhen(nonspeculative || req.ignore_spec_info) {
      state := next_state      // Don't commit refill until marked as nonspeculative by PNR. A refill may be falsely marked as nonspecuative after it has been killed, which is why the killed transition has priority.
    }.elsewhen (io.req_nacked && idx_match || io.req_nacked && io.evict_refill) {
      state := s_invalid
    }
  }
  when (state === s_commit_resp && io.refill.ready) {
    commit_counter := commit_counter + UInt(1)
  }
  when (state === s_commit_resp && commit_done) {
    state := s_meta_write_req
  }
  when (io.mem_acquire.fire()) { // s_refill_req
    state := s_refill_resp
  }
  when (state === s_meta_clear && io.meta_write.ready) {
    state := s_commit_resp
  }
  when (state === s_wb_resp && io.mem_grant.valid) {
    state := s_meta_clear
  }
  when (io.wb_req.fire()) { // s_wb_req
    state := s_wb_resp
  }
  when (io.req_sec_val && io.req_sec_rdy) { // s_wb_req, s_wb_resp, s_refill_req
    //If we get a secondary miss that needs more permissions before we've sent
    //  out the primary miss's Acquire, we can upgrade the permissions we're 
    //  going to ask for in s_refill_req
    req.cmd := dirtier_cmd
    when (is_hit_again) {
      new_coh := dirtier_coh
    }
  }
  when (io.req_pri_val && io.req_pri_rdy) {
    req := io.req_bits
    store_enqueued := rpq.io.enq.valid && isWrite(rpq.io.enq.bits.cmd)
    nonspeculative := false.B
    killed := io.req_bits.killed

    val old_coh = io.req_bits.old_meta.coh
    val (is_hit, _, coh_on_hit) = old_coh.onAccess(io.req_bits.cmd)
    when (io.req_bits.tag_match) {
      when (is_hit) { // set dirty bit
        new_coh := coh_on_hit
        state := s_meta_write_req
      }.otherwise { // upgrade permissions
        new_coh := old_coh
        state := s_refill_req
      }
    }.otherwise { // writeback if necessary and refill
      new_coh := ClientMetadata.onReset
      state := s_refill_req
    }
  }.otherwise {
    req.uop.br_mask := GetNewBrMask(io.brinfo, req.uop)  // Deassert branch mask bits as branches are resolved.
    store_enqueued := store_enqueued || (rpq.io.enq.valid && isWrite(rpq.io.enq.bits.cmd))  // Has a store been missed on this cacheline?
    nonspeculative := nonspeculative || IsOlder(req.uop.rob_idx, io.rob_pnr_idx, ROB_ADDR_SZ)  // Check whether refill is still speculative.
    killed := killed || IsKilledByBranch(io.brinfo, req.uop) || io.kill  // Check whether refill has been killed by misspeculation.
  }
  when (io.mem_grant.valid && state === s_refill_resp) {
     data(refill_address_inc >> 3) := io.mem_grant.bits.data
  }
  dontTouch(data)

  val grantackq = Module(new Queue(io.mem_finish.bits, 1))
  val can_finish = state.isOneOf(s_invalid, s_refill_req)
  grantackq.io.enq.valid := refill_done && edge.isRequest(io.mem_grant.bits)
  grantackq.io.enq.bits := edge.GrantAck(io.mem_grant.bits)
  io.mem_finish.valid := grantackq.io.deq.valid && can_finish
  io.mem_finish.bits := grantackq.io.deq.bits
  grantackq.io.deq.ready := io.mem_finish.ready && can_finish

  io.idx_match := (state =/= s_invalid) && idx_match 
  io.refill.valid := state === s_commit_resp
  io.refill.bits.way_en := req.way_en
  io.refill.bits.addr := req_block_addr | (commit_counter << 3)
  io.refill.bits.data := data(commit_counter)
  io.tag := req_tag 
  io.req_pri_rdy := state === s_invalid
  io.req_sec_rdy := sec_rdy && rpq.io.enq.ready

  val meta_hazard = Reg(init=UInt(0,2))
  when (meta_hazard =/= UInt(0)) { meta_hazard := meta_hazard + 1 }
  when (io.meta_write.fire()) { meta_hazard := 1 }
  io.probe_rdy := !idx_match || meta_hazard === 0

  io.meta_write.valid := state.isOneOf(s_meta_write_req, s_meta_clear)
  io.meta_write.bits.idx := req_idx
  io.meta_write.bits.data.coh := Mux(state === s_meta_clear, coh_on_clear, new_coh)
  io.meta_write.bits.data.tag := io.tag
  io.meta_write.bits.way_en := req.way_en

  io.wb_req.valid := state === s_wb_req
  io.wb_req.bits.source := UInt(id)
  io.wb_req.bits.tag := req.old_meta.tag
  io.wb_req.bits.idx := req_idx
  io.wb_req.bits.param := shrink_param
  io.wb_req.bits.way_en := req.way_en
  io.wb_req.bits.voluntary := Bool(true)

  io.mem_acquire.valid := state === s_refill_req && grantackq.io.enq.ready
  io.mem_acquire.bits := edge.AcquireBlock(
                                fromSource = UInt(id),
                                toAddress = Cat(io.tag, req_idx) << blockOffBits,
                                lgSize = lgCacheBlockBytes,
                                growPermissions = grow_param)._2

  io.meta_read.valid := (state === s_drain_rpq ||
                         state === s_drain_rpq_ld)
  io.meta_read.bits.idx := req_idx
  io.meta_read.bits.tag := io.tag

  io.replay.valid := (state === s_drain_rpq && rpq.io.deq.valid) ||
                     (state === s_drain_rpq_ld && rpq.io.deq.valid && isRead(rpq.io.deq.bits.cmd))
  io.replay.bits := rpq.io.deq.bits
  io.replay.bits.phys := Bool(true)
  io.replay.bits.addr := Cat(io.tag, req_idx, rpq.io.deq.bits.addr(blockOffBits-1,0))
  io.replay.bits.spec_data := data(rpq.io.deq.bits.addr(blockOffBits-1,blockOffBits-3))
  io.replay.bits.use_spec_data := state === s_drain_rpq_ld
  when (!io.meta_read.ready) {
    rpq.io.deq.ready := Bool(false)
    io.replay.bits.cmd := M_FLUSH_ALL /* nop */
  }

  io.victim_safe := state === s_spec_wait
}

class SecureMSHRFile(implicit edge: TLEdgeOut, p: Parameters) extends L1HellaCacheModule()(p) with HasBoomCoreParameters {
  val io = new Bundle {
    val req = Decoupled(new SecureMSHRReq).flip
    val resp = Decoupled(new HellaCacheResp)
    val secondary_miss = Bool(OUTPUT)

    val mem_acquire  = Decoupled(new TLBundleA(edge.bundle))
    val mem_grant = Valid(new TLBundleD(edge.bundle)).flip
    val mem_finish = Decoupled(new TLBundleE(edge.bundle))

    val refill = Decoupled(new SecureL1RefillReq().asOutput)
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val replay = Decoupled(new SecureReplay)
    val wb_req = Decoupled(new WritebackReq(edge.bundle))

    val probe_rdy = Bool(OUTPUT)
    val fence_rdy = Bool(OUTPUT)
    val replay_next = Bool(OUTPUT)

    val brinfo = new BrResolutionInfo().asInput
    val kill = Bool(INPUT)
    val rob_pnr_idx = UInt(INPUT, width=ROB_ADDR_SZ)
    val req_nacked = Bool(INPUT)
  }

  // determine if the request is cacheable or not
  val cacheable = edge.manager.supportsAcquireBFast(io.req.bits.addr, lgCacheBlockBytes)

  val sdq_val = Reg(init=Bits(0, cfg.nSDQ))
  val sdq_alloc_id = PriorityEncoder(~sdq_val(cfg.nSDQ-1,0))
  val sdq_rdy = !sdq_val.andR
  val sdq_enq = io.req.valid && io.req.ready && cacheable && isWrite(io.req.bits.cmd)
  val sdq = Mem(cfg.nSDQ, io.req.bits.data)
  when (sdq_enq) { sdq(sdq_alloc_id) := io.req.bits.data }

  val idxMatch = Wire(Vec(cfg.nMSHRs, Bool()))
  val tagList = Wire(Vec(cfg.nMSHRs, Bits(width = tagBits)))
  val tag_match = Mux1H(idxMatch, tagList) === io.req.bits.addr >> untagBits

  val wbTagList = Wire(Vec(cfg.nMSHRs, Bits()))
  val meta_read_arb = Module(new Arbiter(new L1MetaReadReq, cfg.nMSHRs))
  val meta_write_arb = Module(new Arbiter(new L1MetaWriteReq, cfg.nMSHRs))
  val wb_req_arb = Module(new Arbiter(new WritebackReq(edge.bundle), cfg.nMSHRs))
  val replay_arb = Module(new Arbiter(new SecureReplayInternal, cfg.nMSHRs))
  val alloc_arb = Module(new Arbiter(Bool(), cfg.nMSHRs))
  val refill_arb = Module(new Arbiter(new SecureL1RefillReq, cfg.nMSHRs))

  var idx_match = Bool(false)
  var pri_rdy = Bool(false)
  var sec_rdy = Bool(false)

  io.fence_rdy := true
  io.probe_rdy := true

  val victim_safe = Wire(Vec(cfg.nMSHRs, Bool()))
  val evict_counter = Reg(init=UInt(1, width=cfg.nMSHRs))
  evict_counter := Cat(evict_counter(cfg.nMSHRs-2,0), evict_counter(cfg.nMSHRs-1))

  val mshrs = (0 until cfg.nMSHRs) map { i =>
    val mshr = Module(new SecureMSHR(i))

    idxMatch(i) := mshr.io.idx_match
    tagList(i) := mshr.io.tag
    wbTagList(i) := mshr.io.wb_req.bits.tag

    alloc_arb.io.in(i).valid := mshr.io.req_pri_rdy
    mshr.io.req_pri_val := alloc_arb.io.in(i).ready

    mshr.io.req_sec_val := io.req.valid && sdq_rdy && tag_match
    mshr.io.req_bits := io.req.bits
    mshr.io.req_bits.uop := io.req.bits.uop
    mshr.io.req_bits.killed := io.req.bits.killed
    mshr.io.req_bits.ignore_spec_info := io.req.bits.ignore_spec_info
    mshr.io.req_bits.sdq_id := sdq_alloc_id

    meta_read_arb.io.in(i) <> mshr.io.meta_read
    meta_write_arb.io.in(i) <> mshr.io.meta_write
    wb_req_arb.io.in(i) <> mshr.io.wb_req
    replay_arb.io.in(i) <> mshr.io.replay
    refill_arb.io.in(i) <> mshr.io.refill

    mshr.io.mem_grant.valid := io.mem_grant.valid && io.mem_grant.bits.source === UInt(i)
    mshr.io.mem_grant.bits := io.mem_grant.bits

    pri_rdy = pri_rdy || mshr.io.req_pri_rdy
    sec_rdy = sec_rdy || mshr.io.req_sec_rdy
    idx_match = idx_match || mshr.io.idx_match

    when (!mshr.io.req_pri_rdy) { io.fence_rdy := false }
    when (!mshr.io.probe_rdy) { io.probe_rdy := false }

    mshr.io.brinfo := io.brinfo
    mshr.io.kill := io.kill
    mshr.io.rob_pnr_idx := io.rob_pnr_idx
    mshr.io.req_nacked := io.req_nacked && cacheable
    mshr.io.evict_refill := !idxMatch.reduce(_||_) && evict_counter(i).toBool

    victim_safe(i) := mshr.io.victim_safe

    mshr
  }

  alloc_arb.io.out.ready := io.req.valid && sdq_rdy && cacheable && !idx_match

  io.meta_read <> meta_read_arb.io.out
  io.meta_write <> meta_write_arb.io.out
  io.wb_req <> wb_req_arb.io.out
  io.refill <> refill_arb.io.out

  val mmio_alloc_arb = Module(new Arbiter(Bool(), nIOMSHRs))
  val resp_arb = Module(new Arbiter(new HellaCacheResp, nIOMSHRs))

  var mmio_rdy = Bool(false)
  io.replay_next := Bool(false)

  val mmios = (0 until nIOMSHRs) map { i =>
    val id = cfg.nMSHRs + i
    val mshr = Module(new IOMSHR(id))

    mmio_alloc_arb.io.in(i).valid := mshr.io.req.ready
    mshr.io.req.valid := mmio_alloc_arb.io.in(i).ready
    mshr.io.req.bits := io.req.bits

    mmio_rdy = mmio_rdy || mshr.io.req.ready

    mshr.io.mem_ack.bits := io.mem_grant.bits
    mshr.io.mem_ack.valid := io.mem_grant.valid && io.mem_grant.bits.source === UInt(id)

    resp_arb.io.in(i) <> mshr.io.resp

    when (!mshr.io.req.ready) { io.fence_rdy := Bool(false) }
    when (mshr.io.replay_next) { io.replay_next := Bool(true) }

    mshr
  }

  mmio_alloc_arb.io.out.ready := io.req.valid && !cacheable

  TLArbiter.lowestFromSeq(edge, io.mem_acquire, mshrs.map(_.io.mem_acquire) ++ mmios.map(_.io.mem_access))
  TLArbiter.lowestFromSeq(edge, io.mem_finish,  mshrs.map(_.io.mem_finish))

  io.resp <> resp_arb.io.out
  io.req.ready := Mux(!cacheable,
                    mmio_rdy,
                    sdq_rdy && Mux(idx_match, tag_match && sec_rdy, pri_rdy))
  io.secondary_miss := idx_match && !Mux1H(idxMatch, victim_safe)

  val free_sdq = io.replay.fire() && isWrite(io.replay.bits.cmd)
  io.replay.bits.data := sdq(RegEnable(replay_arb.io.out.bits.sdq_id, free_sdq))
  io.replay <> replay_arb.io.out

  when (io.replay.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UIntToOH(replay_arb.io.out.bits.sdq_id) & Fill(cfg.nSDQ, free_sdq)) |
               PriorityEncoderOH(~sdq_val(cfg.nSDQ-1,0)) & Fill(cfg.nSDQ, sdq_enq)
  }
}

abstract class SecureHellaCache(hartid: Int)(implicit p: Parameters) extends LazyModule {
  protected val cfg = p(TileKey).dcache.get

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

  val module: SecureHellaCacheModule
}
class SecureHellaCacheBundle(val outer: SecureHellaCache)(implicit p: Parameters) extends CoreBundle()(p) {
  val hartid = UInt(INPUT, hartIdLen)
  val cpu = (new SecureHellaCacheIO).flip
  val ptw = new TLBPTWIO()
  val errors = new DCacheErrors
  val spec_info = new SpecInfo().asInput
}


class BoomSecureDCache(hartid: Int)(implicit p: Parameters) extends SecureHellaCache(hartid)(p) {
  override lazy val module = new BoomSecureDCacheModule(this) 
}

class SecureHellaCacheModule(outer: SecureHellaCache) extends LazyModuleImp(outer)
    with HasL1HellaCacheParameters {
  implicit val edge = outer.node.edges.out(0)
  val (tl_out, _) = outer.node.out(0)
  val io = IO(new SecureHellaCacheBundle(outer))
  dontTouch(io.cpu.resp) // Users like to monitor these fields even if the core ignores some signals
  dontTouch(io.cpu.s1_data)

  private val fifoManagers = edge.manager.managers.filter(TLFIFOFixer.allUncacheable)
  fifoManagers.foreach { m =>
    require (m.fifoId == fifoManagers.head.fifoId,
      s"IOMSHRs must be FIFO for all regions with effects, but HellaCache sees ${m.nodePath.map(_.name)}")
  }
}


class BoomSecureDCacheModule(outer: BoomSecureDCache) extends SecureHellaCacheModule(outer) {

  require(isPow2(nWays)) // TODO: relax this
  require(dataScratchpadSize == 0)
  require(!usingVM || untagBits <= pgIdxBits, s"untagBits($untagBits) > pgIdxBits($pgIdxBits)")

  // ECC is only supported on the data array
  require(cacheParams.tagCode.isInstanceOf[IdentityCode])
  val dECC = cacheParams.dataCode

  val wb = Module(new WritebackUnit)
  val prober = Module(new ProbeUnit)
  val mshrs = Module(new SecureMSHRFile)
  mshrs.io.brinfo := io.spec_info.brinfo
  mshrs.io.kill := io.spec_info.kill
  mshrs.io.rob_pnr_idx := io.spec_info.rob_pnr_idx

  io.cpu.req.ready := Bool(true)
  val s1_valid = Reg(next=io.cpu.req.fire(), init=Bool(false))
  val s1_req = Reg(io.cpu.req.bits)
  s1_req.uop := GetNewUopAndBrMask(io.cpu.req.bits.uop, io.spec_info.brinfo)

  val s1_valid_masked = s1_valid && !io.cpu.s1_kill
  val s1_replay = Reg(init=Bool(false))
  val s1_replay_data = Reg(UInt(width=64))
  val s1_use_replay_data = Reg(init=Bool(false))
  val s1_clk_en = Reg(Bool())
  val s1_sfence = s1_req.cmd === M_SFENCE

  val s2_valid = Reg(next=s1_valid_masked && !s1_sfence, init=Bool(false)) && !io.cpu.s2_xcpt.asUInt.orR
  val s2_req = Reg(io.cpu.req.bits)
  s2_req.uop := GetNewUopAndBrMask(io.cpu.req.bits.uop, io.spec_info.brinfo)
  val s2_replay = Reg(next=s1_replay, init=Bool(false)) && s2_req.cmd =/= M_FLUSH_ALL
  val s2_replay_data = Reg(next=s1_replay_data)
  val s2_use_replay_data = Reg(next=s1_use_replay_data)
  val s2_recycle = Wire(Bool())
  val s2_valid_masked = Wire(Bool())

  // Might we need to prevent a cachefill resulting from an operation in stage 1 or 2?
  val s1_killed = IsKilledByBranch(io.spec_info.brinfo, s1_req.uop) || io.spec_info.kill
  val s2_killed = IsKilledByBranch(io.spec_info.brinfo, s2_req.uop) || io.spec_info.kill || Reg(next=s1_killed)

  val s3_valid = Reg(init=Bool(false))
  val s3_req = Reg(io.cpu.req.bits)
  val s3_way = Reg(Bits())

  val s1_recycled = RegEnable(s2_recycle, Bool(false), s1_clk_en)
  val s1_read  = isRead(s1_req.cmd)
  val s1_write = isWrite(s1_req.cmd)
  val s1_readwrite = s1_read || s1_write || isPrefetch(s1_req.cmd)
  // check for unsupported operations
  assert(!s1_valid || !s1_req.cmd.isOneOf(M_PWR))

  val dtlb = Module(new TLB(false, log2Ceil(coreDataBytes), TLBConfig(nTLBEntries)))
  io.ptw <> dtlb.io.ptw
  dtlb.io.kill := io.cpu.s2_kill
  dtlb.io.req.valid := s1_valid && !io.cpu.s1_kill && s1_readwrite
  dtlb.io.req.bits.passthrough := s1_req.phys
  dtlb.io.req.bits.vaddr := s1_req.addr
  dtlb.io.req.bits.size := s1_req.typ
  dtlb.io.req.bits.cmd := s1_req.cmd
  when (!dtlb.io.req.ready && !io.cpu.req.bits.phys) { io.cpu.req.ready := Bool(false) }

  dtlb.io.sfence.valid := s1_valid && !io.cpu.s1_kill && s1_sfence
  dtlb.io.sfence.bits.rs1 := s1_req.typ(0)
  dtlb.io.sfence.bits.rs2 := s1_req.typ(1)
  dtlb.io.sfence.bits.addr := s1_req.addr
  dtlb.io.sfence.bits.asid := io.cpu.s1_data.data

  when (io.cpu.req.valid) {
    s1_req := io.cpu.req.bits
    s1_req.uop := GetNewUopAndBrMask(io.cpu.req.bits.uop, io.spec_info.brinfo)
  }
  when (wb.io.meta_read.valid) {
    s1_req.addr := Cat(wb.io.meta_read.bits.tag, wb.io.meta_read.bits.idx) << blockOffBits
    s1_req.phys := Bool(true)
  }
  when (prober.io.meta_read.valid) {
    s1_req.addr := Cat(prober.io.meta_read.bits.tag, prober.io.meta_read.bits.idx) << blockOffBits
    s1_req.phys := Bool(true)
  }
  when (mshrs.io.replay.valid) {    // Don't want replays to go through the cache pipeline - get them directly out of the MSHR fill buffer!
    s1_req := mshrs.io.replay.bits
    s1_replay_data := mshrs.io.replay.bits.spec_data
    s1_use_replay_data := mshrs.io.replay.bits.use_spec_data
  }
  when (s2_recycle) {
    s1_req := s2_req
    s1_req.uop := GetNewUopAndBrMask(s2_req.uop, io.spec_info.brinfo)
  }
  val s1_addr = dtlb.io.resp.paddr

  when (s1_clk_en) {
    s2_req.typ := s1_req.typ
    s2_req.phys := s1_req.phys
    s2_req.addr := s1_addr
    when (s1_write) {
      s2_req.data := Mux(s1_replay, mshrs.io.replay.bits.data, io.cpu.s1_data.data)
    }
    when (s1_recycled) { s2_req.data := s1_req.data }
    s2_req.tag := s1_req.tag
    s2_req.cmd := s1_req.cmd
    s2_req.uop := GetNewUopAndBrMask(s1_req.uop, io.spec_info.brinfo)
  }

  // tags
  def onReset = L1Metadata(UInt(0), ClientMetadata.onReset)
  val meta = Module(new L1MetadataArray(onReset _))
  val metaReadArb = Module(new Arbiter(new L1MetaReadReq, 5))
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, 2))
  meta.io.read <> metaReadArb.io.out
  meta.io.write <> metaWriteArb.io.out

  when (meta.io.write.valid) {
    printf("meta %x %x %x %x %x\n", meta.io.write.bits.idx, meta.io.write.bits.way_en, meta.io.write.bits.tag, meta.io.write.bits.data.coh.state, meta.io.write.bits.data.tag)
  }

  // data
  val data = Module(new DataArray)
  val readArb = Module(new Arbiter(new L1DataReadReq, 4))
  val writeArb = Module(new Arbiter(new L1DataWriteReq, 2))
  data.io.write.valid := writeArb.io.out.valid
  writeArb.io.out.ready := data.io.write.ready
  data.io.write.bits := writeArb.io.out.bits
  val wdata_encoded = (0 until rowWords).map(i => dECC.encode(writeArb.io.out.bits.data(coreDataBits*(i+1)-1,coreDataBits*i)))
  data.io.write.bits.data := wdata_encoded.asUInt

  when (data.io.write.valid) {
    printf("data %x %x %x\n", data.io.write.bits.way_en, data.io.write.bits.addr, data.io.write.bits.data)
  }

  // tag read for new requests
  metaReadArb.io.in(4).valid := io.cpu.req.valid
  metaReadArb.io.in(4).bits.idx := io.cpu.req.bits.addr >> blockOffBits
  when (!metaReadArb.io.in(4).ready) { io.cpu.req.ready := Bool(false) }

  // data read for new requests
  readArb.io.in(3).valid := io.cpu.req.valid
  readArb.io.in(3).bits.addr := io.cpu.req.bits.addr
  readArb.io.in(3).bits.way_en := ~UInt(0, nWays)
  when (!readArb.io.in(3).ready) { io.cpu.req.ready := Bool(false) }

  // recycled requests
  metaReadArb.io.in(0).valid := s2_recycle
  metaReadArb.io.in(0).bits.idx := s2_req.addr >> blockOffBits
  readArb.io.in(0).valid := s2_recycle
  readArb.io.in(0).bits.addr := s2_req.addr
  readArb.io.in(0).bits.way_en := ~UInt(0, nWays)

  // tag check and way muxing
  def wayMap[T <: Data](f: Int => T) = Vec((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta.io.resp(w).tag === (s1_addr >> untagBits)).asUInt
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && meta.io.resp(w).coh.isValid()).asUInt
  s1_clk_en := (metaReadArb.io.out.valid || (mshrs.io.replay.valid && mshrs.io.replay.bits.use_spec_data)) //TODO: should be metaReadArb.io.out.fire(), but triggers Verilog backend bug
  val s1_writeback = s1_clk_en && !s1_valid && !s1_replay
  val s2_tag_match_way = RegEnable(s1_tag_match_way, s1_clk_en)
  val s2_tag_match = s2_tag_match_way.orR
  val s2_hit_state = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegEnable(meta.io.resp(w).coh, s1_clk_en)))
  val (s2_has_permission, _, s2_new_hit_state) = s2_hit_state.onAccess(s2_req.cmd)
  val s2_hit = s2_tag_match && s2_has_permission && s2_hit_state === s2_new_hit_state

  // load-reserved/store-conditional
  val lrsc_count = Reg(init=UInt(0))
  val lrsc_valid = lrsc_count > lrscBackoff
  val lrsc_addr = Reg(UInt())
  val (s2_lr, s2_sc) = (s2_req.cmd === M_XLR, s2_req.cmd === M_XSC)
  val s2_lrsc_addr_match = lrsc_valid && lrsc_addr === (s2_req.addr >> blockOffBits)
  val s2_sc_fail = s2_sc && !s2_lrsc_addr_match
  when (lrsc_count > 0) { lrsc_count := lrsc_count - 1 }
  when (s2_valid_masked && s2_hit || s2_replay) {
    when (s2_lr) {
      lrsc_count := lrscCycles - 1
      lrsc_addr := s2_req.addr >> blockOffBits
    }
    when (lrsc_count > 0) {
      lrsc_count := 0
    }
  }

  val s2_data = Wire(Vec(nWays, Bits(width=encRowBits)))
  for (w <- 0 until nWays) {
    val regs = Reg(Vec(rowWords, Bits(width = encDataBits)))
    val en1 = s1_clk_en && s1_tag_eq_way(w)
    for (i <- 0 until regs.size) {
      val en = en1 && ((Bool(i == 0) || !Bool(doNarrowRead)) || s1_writeback)
      when (en) { regs(i) := data.io.resp(w) >> encDataBits*i }
    }
    s2_data(w) := regs.asUInt
  }
  val s2_data_muxed = Mux(s2_replay && s2_use_replay_data, s2_replay_data, Mux1H(s2_tag_match_way, s2_data))
  val s2_data_decoded = (0 until rowWords).map(i => dECC.decode(s2_data_muxed(encDataBits*(i+1)-1,encDataBits*i)))
  val s2_data_corrected = s2_data_decoded.map(_.corrected).asUInt
  val s2_data_uncorrected = s2_data_decoded.map(_.uncorrected).asUInt
  val s2_word_idx = if(doNarrowRead) UInt(0) else s2_req.addr(log2Up(rowWords*coreDataBytes)-1,log2Up(wordBytes))
  val s2_data_correctable = s2_data_decoded.map(_.correctable).asUInt()(s2_word_idx)

  // store/amo hits
  s3_valid := (s2_valid_masked && s2_hit || s2_replay) && !s2_sc_fail && isWrite(s2_req.cmd)
  val amoalu = Module(new AMOALU(xLen))
  when ((s2_valid || s2_replay) && (isWrite(s2_req.cmd) || s2_data_correctable)) {
    s3_req := s2_req
    s3_req.data := Mux(s2_data_correctable, s2_data_corrected, amoalu.io.out)
    s3_way := s2_tag_match_way
  }

  writeArb.io.in(0).bits.addr := s3_req.addr
  writeArb.io.in(0).bits.wmask := UIntToOH(s3_req.addr.extract(rowOffBits-1,offsetlsb))
  writeArb.io.in(0).bits.data := Fill(rowWords, s3_req.data)
  writeArb.io.in(0).valid := s3_valid
  writeArb.io.in(0).bits.way_en :=  s3_way

  // replacement policy
  val replacer = cacheParams.replacement
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegEnable(replacer.way, s1_clk_en))
  val s2_repl_meta = Mux1H(s2_replaced_way_en, wayMap((w: Int) => RegEnable(meta.io.resp(w), s1_clk_en && s1_replaced_way_en(w))).toSeq)

  // miss handling
  mshrs.io.req.valid := s2_valid_masked && !s2_hit && (isPrefetch(s2_req.cmd) || isRead(s2_req.cmd) || isWrite(s2_req.cmd))
  mshrs.io.req.bits := s2_req
  mshrs.io.req.bits.tag_match := s2_tag_match
  mshrs.io.req.bits.old_meta := Mux(s2_tag_match, L1Metadata(s2_repl_meta.tag, s2_hit_state), s2_repl_meta)
  mshrs.io.req.bits.way_en := Mux(s2_tag_match, s2_tag_match_way, s2_replaced_way_en)
  mshrs.io.req.bits.data := s2_req.data
  mshrs.io.req.bits.killed := s2_killed
  mshrs.io.req.bits.ignore_spec_info := s2_req.ignore_spec_info
  when (mshrs.io.req.fire()) { replacer.miss }
  tl_out.a <> mshrs.io.mem_acquire

  // replays
  readArb.io.in(1).valid := mshrs.io.replay.valid
  readArb.io.in(1).bits := mshrs.io.replay.bits
  readArb.io.in(1).bits.way_en := ~UInt(0, nWays)
  mshrs.io.replay.ready := readArb.io.in(1).ready
  s1_replay := mshrs.io.replay.valid && readArb.io.in(1).ready
  metaReadArb.io.in(1) <> mshrs.io.meta_read
  metaWriteArb.io.in(0) <> mshrs.io.meta_write

  // probes and releases
  prober.io.req.valid := tl_out.b.valid && !lrsc_valid
  tl_out.b.ready := prober.io.req.ready && !lrsc_valid
  prober.io.req.bits := tl_out.b.bits
  prober.io.way_en := s2_tag_match_way
  prober.io.block_state := s2_hit_state
  metaReadArb.io.in(2) <> prober.io.meta_read
  metaWriteArb.io.in(1) <> prober.io.meta_write
  prober.io.mshr_rdy := mshrs.io.probe_rdy

  // refills
  val grant_has_data = edge.hasData(tl_out.d.bits)
  mshrs.io.mem_grant.valid := tl_out.d.fire()
  mshrs.io.mem_grant.bits := tl_out.d.bits
  tl_out.d.ready := writeArb.io.in(1).ready || !grant_has_data
  /* The last clause here is necessary in order to prevent the responses for
   * the IOMSHRs from being written into the data array. It works because the
   * IOMSHR ids start right the ones for the regular MSHRs. */
  // writeArb.io.in(1).valid := tl_out.d.valid && grant_has_data &&
  //                              tl_out.d.bits.source < UInt(cfg.nMSHRs)
  writeArb.io.in(1) <> mshrs.io.refill
  writeArb.io.in(1).bits.wmask := ~UInt(0, rowWords)
  data.io.read <> readArb.io.out
  readArb.io.out.ready := !tl_out.d.valid || tl_out.d.ready // insert bubble if refill gets blocked
  tl_out.e <> mshrs.io.mem_finish

  // writebacks
  val wbArb = Module(new Arbiter(new WritebackReq(edge.bundle), 2))
  wbArb.io.in(0) <> prober.io.wb_req
  wbArb.io.in(1) <> mshrs.io.wb_req
  wb.io.req <> wbArb.io.out
  metaReadArb.io.in(3) <> wb.io.meta_read
  readArb.io.in(2) <> wb.io.data_req
  wb.io.data_resp := s2_data_corrected
  TLArbiter.lowest(edge, tl_out.c, wb.io.release, prober.io.rep)

  // store->load bypassing
  val s4_valid = Reg(next=s3_valid, init=Bool(false))
  val s4_req = RegEnable(s3_req, s3_valid && metaReadArb.io.out.valid)
  val bypasses = List(
    ((s2_valid_masked || s2_replay) && !s2_sc_fail, s2_req, amoalu.io.out),
    (s3_valid, s3_req, s3_req.data),
    (s4_valid, s4_req, s4_req.data)
  ).map(r => (r._1 && (s1_addr >> wordOffBits === r._2.addr >> wordOffBits) && isWrite(r._2.cmd), r._3))
  val s2_store_bypass_data = Reg(Bits(width = coreDataBits))
  val s2_store_bypass = Reg(Bool())
  when (s1_clk_en) {
    s2_store_bypass := false
    when (bypasses.map(_._1).reduce(_||_)) {
      s2_store_bypass_data := PriorityMux(bypasses)
      s2_store_bypass := true
    }
  }

  // load data subword mux/sign extension
  val s2_data_word_prebypass = s2_data_uncorrected >> Cat(s2_word_idx, Bits(0,log2Up(coreDataBits)))
  val s2_data_word = Mux(s2_store_bypass, s2_store_bypass_data, s2_data_word_prebypass)
  val loadgen = new LoadGen(s2_req.typ, mtSigned(s2_req.typ), s2_req.addr, s2_data_word, s2_sc, wordBytes)

  amoalu.io.mask := new StoreGen(s2_req.typ, s2_req.addr, 0.U, xLen/8).mask
  amoalu.io.cmd := s2_req.cmd
  amoalu.io.lhs := s2_data_word
  amoalu.io.rhs := s2_req.data

  // nack it like it's hot
  val s1_nack = dtlb.io.req.valid && dtlb.io.resp.miss ||
                s1_req.addr(idxMSB,idxLSB) === prober.io.meta_write.bits.idx && !prober.io.req.ready
  val s2_nack_hit = RegEnable(s1_nack, s1_valid || s1_replay)
  when (s2_nack_hit) { mshrs.io.req.valid := Bool(false) }
  val s2_nack_victim = s2_hit && mshrs.io.secondary_miss
  val s2_nack_miss = !s2_hit && !mshrs.io.req.ready
  val s2_nack = s2_nack_hit || s2_nack_victim || s2_nack_miss
  s2_valid_masked := s2_valid && !s2_nack && !io.cpu.s2_kill
  mshrs.io.req_nacked := s2_nack_miss

  val s2_recycle_ecc = (s2_valid || s2_replay) && s2_hit && s2_data_correctable
  val s2_recycle_next = Reg(init=Bool(false))
  when (s1_valid || s1_replay) { s2_recycle_next := s2_recycle_ecc }
  s2_recycle := s2_recycle_ecc || s2_recycle_next

  // after a nack, block until nack condition resolves to save energy
  val block_miss = Reg(init=Bool(false))
  block_miss := (s2_valid || block_miss) && s2_nack_miss
  when (block_miss) {
    io.cpu.req.ready := Bool(false)
  }

  val cache_resp = Wire(Valid(new HellaCacheResp))
  cache_resp.valid := (s2_replay || s2_valid_masked && s2_hit) && !s2_data_correctable
  cache_resp.bits := s2_req
  cache_resp.bits.has_data := isRead(s2_req.cmd)
  cache_resp.bits.data := loadgen.data | s2_sc_fail
  cache_resp.bits.store_data := s2_req.data
  cache_resp.bits.replay := s2_replay

  val uncache_resp = Wire(Valid(new HellaCacheResp))
  uncache_resp.bits := mshrs.io.resp.bits
  uncache_resp.valid := mshrs.io.resp.valid
  mshrs.io.resp.ready := Reg(next= !(s1_valid || s1_replay))

  io.cpu.s2_nack := s2_valid && s2_nack
  io.cpu.resp := Mux(mshrs.io.resp.ready, uncache_resp, cache_resp)
  io.cpu.resp.bits.data_word_bypass := loadgen.wordData
  io.cpu.resp.bits.data_raw := s2_data_word
  io.cpu.ordered := mshrs.io.fence_rdy && !s1_valid && !s2_valid
  io.cpu.replay_next := (s1_replay && s1_read) || mshrs.io.replay_next

  val s1_xcpt_valid = dtlb.io.req.valid && !s1_nack
  val s1_xcpt = dtlb.io.resp
  io.cpu.s2_xcpt := Mux(RegNext(s1_xcpt_valid), RegEnable(s1_xcpt, s1_clk_en), 0.U.asTypeOf(s1_xcpt))

  // performance events
  io.cpu.perf.acquire := edge.done(tl_out.a)
  io.cpu.perf.release := edge.done(tl_out.c)
  io.cpu.perf.tlbMiss := io.ptw.req.fire()

  // no clock-gating support
  io.cpu.clock_enabled := true
}
