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

class BoomMSHRReq(implicit p: Parameters) extends BoomBundle()(p)
  with HasL1HellaCacheParameters
{
  val uop = new MicroOp

  val addr   = UInt(coreMaxAddrBits.W)

  // miss info
  val tag_match = Bool()
  val old_meta  = new L1Metadata
  val way_en    = UInt(nWays.W)

  // for store misses
  val data      = UInt(coreDataBits.W)


}

class BoomMSHR(id: Int)(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
  with HasL1HellaCacheParameters
{
  val io = IO(new Bundle {
    val req_pri_val = Input(Bool())
    val req_pri_rdy = Output(Bool())
    val req_sec_val = Input(Bool())
    val req_sec_rdy = Output(Bool())

    val req         = Input(new BoomMSHRReq)
    val req_sdq_id  = Input(UInt(log2Ceil(cfg.nSDQ).W))

    val idx_match   = Output(Bool())
    val tag         = Output(UInt(tagBits.W))

//    val mem_acquire = Decoupled(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(Valid(new TLBundleD(edge.bundle)))
//    val mem_finish  = Decoupled(new TLBundleE(edge.bundle))

    val refill      = Output(new L1RefillReq)

    val meta_read   = Decoupled(new L1MetaReadReq)
    val meta_write  = Decoupled(new L1MetaWriteReq)
    val wb_req      = Decoupled(new WritebackReq(edge.bundle))
    val replay      = Decoupled(new ReplayInternal)

    val probe_rdy   = Output(Bool())
  })

}


class BoomMSHRFile(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
  with HasL1HellaCacheParameters
{
  val io = IO(new Bundle {
    val req = DecoupledIO(new BoomMSHRReq).flip

    val mem_acquire  = Decoupled(new TLBundleA(edge.bundle))
    val mem_grant    = Flipped(Valid(new TLBundleD(edge.bundle)))
    val mem_finished = Decoupled(new TLBundleE(edge.bundle))

    val refill     = Output(new L1RefillReq)
    val meta_read  = Decoupled(new L1MetaReadReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val replay     = Decoupled(new Replay)
    val wb_req     = Decoupled(new WritebackReq(edge.bundle))

    val fence_rdy = Output(Bool())
    val probe_rdy = Output(Bool())
  })

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
  val refill_mux  = Wire(Vec(cfg.nMSHRs, new L1RefillReq))

  val meta_read_arb  = Module(new Arbiter(new L1MetaReadReq            , cfg.nMSHRs))
  val meta_write_arb = Module(new Arbiter(new L1MetaWriteReq           , cfg.nMSHRs))
  val wb_req_arb     = Module(new Arbiter(new WritebackReq(edge.bundle), cfg.nMSHRs))
  val replay_arb     = Module(new Arbiter(new ReplayInternal           , cfg.nMSHRs))
  val alloc_arb      = Module(new Arbiter(    Bool()                   , cfg.nMSHRs))

  var idx_match = false.B
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

    mshr.io.req_pri_val := alloc_arb.io.in(i).ready
    mshr.io.req_sec_val := io.req.valid && sdq_rdy && tag_match
    mshr.io.req         := io.req.bits
    mshr.io.req_sdq_id  := sdq_alloc_id

    meta_read_arb.io.in(i)  <> mshr.io.meta_read
    meta_write_arb.io.in(i) <> mshr.io.meta_write
    wb_req_arb.io.in(i)     <> mshr.io.wb_req
    replay_arb.io.in(i)     <> mshr.io.replay

    mshr.io.mem_grant.valid := io.mem_grant.valid && io.mem_grant.bits.source === i.U
    mshr.io.mem_grant.bits  := io.mem_grant.bits

    refill_mux(i) := mshr.io.refill

    pri_rdy   = pri_rdy || mshr.io.req_pri_rdy
    sec_rdy   = sec_rdy || mshr.io.req_sec_rdy
    idx_match = idx_match || mshr.io.idx_match

    when (!mshr.io.req_pri_rdy) {
      io.fence_rdy := false.B
    }
    when (!mshr.io.probe_rdy) {
      io.probe_rdy := false.B
    }

    mshr
  }

  alloc_arb.io.out.ready := io.req.valid && sdq_rdy && cacheable && !idx_match

  io.meta_read  <> meta_read_arb.io.out
  io.meta_write <> meta_write_arb.io.out
  io.wb_req     <> wb_req_arb.io.out


  io.replay <> replay_arb.io.out



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

  // val wb = Module(new WritebackUnit)
  // val prober = Module(new ProbeUnit)
  val mshrs = Module(new BoomMSHRFile)
  mshrs.io := DontCare


  io.lsu.req.ready := true.B

  // tags
  def onReset = L1Metadata(0.U, ClientMetadata.onReset)
  val meta = Module(new L1MetadataArray(onReset _))
  dontTouch(meta.io)
  meta.io.write.valid := false.B
  meta.io.write.bits := DontCare

  // data
  val data = Module(new DataArray)
  dontTouch(data.io)
  data.io.write.valid := false.B
  data.io.write.bits := DontCare

  // Tag read for new requests
  meta.io.read.valid       := io.lsu.req.fire()
  meta.io.read.bits.idx    := io.lsu.req.bits.addr >> blockOffBits
  meta.io.read.bits.way_en := DontCare // This isn't used?
  meta.io.read.bits.tag    := DontCare // This isn't used?

  // Data read for new requests
  data.io.read.valid       := io.lsu.req.fire()
  data.io.read.bits.addr   := io.lsu.req.bits.addr
  data.io.read.bits.way_en := ~0.U(nWays.W)


  val s1_valid = RegNext(io.lsu.req.fire(), init=false.B)
  val s1_req   = RegNext(io.lsu.req.bits)
  val s1_addr  = s1_req.addr

  // tag check
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta.io.resp(w).tag === (s1_addr >> untagBits)).asUInt
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && meta.io.resp(w).coh.isValid()).asUInt


  val s2_valid = RegNext(s1_valid)
  val s2_req   = RegNext(s1_req)
  val s2_tag_match_way = RegNext(s1_tag_match_way)
  val s2_tag_match = s2_tag_match_way.orR
  val s2_hit_state = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegNext(meta.io.resp(w).coh)))
  val (s2_has_permission, _, s2_new_hit_state) = s2_hit_state.onAccess(s2_req.uop.mem_cmd)
  val s2_hit = s2_tag_match && s2_has_permission && s2_hit_state === s2_new_hit_state

  // replacement policy
  val replacer = cacheParams.replacement
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegNext(replacer.way))
  val s2_repl_meta = Mux1H(s2_replaced_way_en, wayMap((w: Int) => RegNext(meta.io.resp(w))).toSeq)

  // Miss handling
  mshrs.io.req.valid          := s2_valid && !s2_hit && (isPrefetch(s2_req.uop.mem_cmd) || isRead(s2_req.uop.mem_cmd) || isWrite(s2_req.uop.mem_cmd))
  mshrs.io.req.bits.uop       := s2_req.uop
  mshrs.io.req.bits.addr      := s2_req.addr
  mshrs.io.req.bits.tag_match := s2_tag_match
  mshrs.io.req.bits.old_meta  := Mux(s2_tag_match, L1Metadata(s2_repl_meta.tag, s2_hit_state), s2_repl_meta)
  mshrs.io.req.bits.way_en    := Mux(s2_tag_match, s2_tag_match_way, s2_replaced_way_en)
  mshrs.io.req.bits.data      := s2_req.data
  when (mshrs.io.req.fire()) { replacer.miss }

  val s2_nack_miss = s2_valid && !s2_hit && !mshrs.io.req.ready // MSHRs not ready for request
}
