// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package boom


import Chisel._
import Chisel.ImplicitConversions._
import chisel3.core.withReset
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import chisel3.internal.sourceinfo.SourceInfo

//class FrontendReq(implicit p: Parameters) extends CoreBundle()(p) {
//  val pc = UInt(width = vaddrBitsExtended)
//  val speculative = Bool()
//}

//class FrontendExceptions extends Bundle {
//  val pf = new Bundle {
//    val inst = Bool()
//  }
//  val ae = new Bundle {
//    val inst = Bool()
//  }
//}
//
//class FrontendResp(implicit p: Parameters) extends CoreBundle()(p) {
//  val btb = new BTBResp
//  val pc = UInt(width = vaddrBitsExtended)  // ID stage PC
//  val data = UInt(width = fetchWidth * coreInstBits)
//  val mask = Bits(width = fetchWidth)
//  val xcpt = new FrontendExceptions
//  val replay = Bool()
//}
//
//class FrontendPerfEvents extends Bundle {
//  val acquire = Bool()
//  val tlbMiss = Bool()
//}
//

// TODO CODEREVIEW these signals are generally useful to the FetchUnit, not just BTB predictions.
class BTBReqIO(implicit p: Parameters) extends CoreBundle()(p) {
  val req = Valid(new BTBReq).flip
  val s2_replay = Bool(INPUT) // is the Frontend replaying s2 into s0?
  val fqenq_valid = Bool(INPUT) // is the Frontend enqueuing instructions this cycle? TODO rename to "s2_valid?"
  val debug_fqenq_pc = UInt(INPUT, width = vaddrBitsExtended)
  val debug_fqenq_ready = Bool(INPUT) // verify this matches our own buffers
}

class BoomFrontendIO(implicit p: Parameters) extends CoreBundle()(p) {
  val req = Valid(new FrontendReq)
  val sfence = Valid(new SFenceReq)
  val resp = Decoupled(new FrontendResp).flip
  val flush_icache = Bool(OUTPUT)
  val npc = UInt(INPUT, width = vaddrBitsExtended)
  val btb_req = new BTBReqIO()
  val perf = new FrontendPerfEvents().asInput
}

class BoomFrontend(val icacheParams: ICacheParams, hartid: Int)(implicit p: Parameters) extends LazyModule {
  lazy val module = new BoomFrontendModule(this)
  val icache = LazyModule(new freechips.rocketchip.rocket.ICache(icacheParams, hartid))
  val masterNode = icache.masterNode
  val slaveNode = icache.slaveNode
}

class BoomFrontendBundle(val outer: BoomFrontend) extends CoreBundle()(outer.p)
    with HasExternallyDrivenTileConstants {
  val cpu = new BoomFrontendIO().flip
  val ptw = new TLBPTWIO()
  val errors = new ICacheErrors
}

class BoomFrontendModule(outer: BoomFrontend) extends LazyModuleImp(outer)
    with HasCoreParameters
    with HasL1ICacheParameters {
  val io = IO(new BoomFrontendBundle(outer))
  implicit val edge = outer.masterNode.edges.out(0)
  val icache = outer.icache.module
  require(fetchWidth*coreInstBytes == outer.icacheParams.fetchBytes)

  println("\tBuilding BOOM Frontend\n")

  val tlb = Module(new TLB(true, log2Ceil(fetchBytes), nTLBEntries))
  val fq = withReset(reset || io.cpu.req.valid) { Module(new ShiftQueue(new FrontendResp, 5, flow = true)) }

  val s0_valid = io.cpu.req.valid || !fq.io.mask(fq.io.mask.getWidth-3)
  val s1_valid = RegNext(s0_valid)
  val s1_pc = Reg(UInt(width=vaddrBitsExtended))
  val s1_speculative = Reg(Bool())
  val s2_valid = RegInit(false.B)
  val s2_pc = RegInit(t = UInt(width = vaddrBitsExtended), alignPC(io.reset_vector))
  val s2_btb_resp_valid = if (usingBTB) Reg(Bool()) else false.B
  val s2_btb_resp_bits = Reg(new BTBResp)
  val s2_btb_taken = s2_btb_resp_valid && s2_btb_resp_bits.taken
  val s2_tlb_resp = Reg(tlb.io.resp)
  val s2_xcpt = s2_tlb_resp.ae.inst || s2_tlb_resp.pf.inst
  val s2_speculative = Reg(init=Bool(false))
  val s2_partial_insn_valid = RegInit(false.B)
  val s2_partial_insn = Reg(UInt(width = coreInstBits))
  val wrong_path = Reg(Bool())

  val s1_base_pc = ~(~s1_pc | (fetchBytes - 1))
  val ntpc = s1_base_pc + fetchBytes.U
  val predicted_npc = Wire(init = ntpc)
  val predicted_taken = Wire(init = Bool(false))

  val s2_replay = Wire(Bool())
  s2_replay := (s2_valid && !fq.io.enq.fire()) || RegNext(s2_replay && !s0_valid, true.B)
  val npc = Mux(s2_replay, s2_pc, predicted_npc)

  s1_pc := io.cpu.npc
  // consider RVC fetches across blocks to be non-speculative if the first
  // part was non-speculative
  val s0_speculative =
    if (usingCompressed) s1_speculative || s2_valid && !s2_speculative || predicted_taken
    else Bool(true)
  s1_speculative := Mux(io.cpu.req.valid, io.cpu.req.bits.speculative, Mux(s2_replay, s2_speculative, s0_speculative))

  val s2_redirect = Wire(init = io.cpu.req.valid)
  s2_valid := false
  when (!s2_replay) {
    s2_valid := !s2_redirect
    s2_pc := s1_pc
    s2_speculative := s1_speculative
    s2_tlb_resp := tlb.io.resp
  }

  io.ptw <> tlb.io.ptw
  tlb.io.req.valid := !s2_replay
  tlb.io.req.bits.vaddr := s1_pc
  tlb.io.req.bits.passthrough := Bool(false)
  tlb.io.req.bits.sfence := io.cpu.sfence
  tlb.io.req.bits.size := log2Ceil(coreInstBytes*fetchWidth)

  icache.io.hartid := io.hartid
  icache.io.req.valid := s0_valid
  icache.io.req.bits.addr := io.cpu.npc
  icache.io.invalidate := io.cpu.flush_icache
  icache.io.s1_paddr := tlb.io.resp.paddr
  icache.io.s2_vaddr := s2_pc
  icache.io.s1_kill := s2_redirect || tlb.io.resp.miss || s2_replay
  icache.io.s2_kill := s2_speculative && !s2_tlb_resp.cacheable || s2_xcpt
  icache.io.s2_prefetch := s2_tlb_resp.prefetchable

  fq.io.enq.valid := RegNext(s1_valid) && s2_valid && (icache.io.resp.valid || !s2_tlb_resp.miss && icache.io.s2_kill)
  fq.io.enq.bits.pc := s2_pc
  io.cpu.npc := alignPC(Mux(io.cpu.req.valid, io.cpu.req.bits.pc, npc))

  fq.io.enq.bits.data := icache.io.resp.bits.data
  fq.io.enq.bits.mask := UInt((1 << fetchWidth)-1) << s2_pc.extract(log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))
  fq.io.enq.bits.replay := icache.io.resp.bits.replay || icache.io.s2_kill && !icache.io.resp.valid && !s2_xcpt
  fq.io.enq.bits.btb := s2_btb_resp_bits
  fq.io.enq.bits.btb.taken := s2_btb_taken
  fq.io.enq.bits.xcpt := s2_tlb_resp
  when (icache.io.resp.valid && icache.io.resp.bits.ae) { fq.io.enq.bits.xcpt.ae.inst := true }

//  io.cpu.btb_req.req.valid := false.B
 io.cpu.btb_req.req.valid := s0_valid
 io.cpu.btb_req.req.bits.addr := io.cpu.npc
 io.cpu.btb_req.fqenq_valid := fq.io.enq.valid
 io.cpu.btb_req.debug_fqenq_pc := fq.io.enq.bits.pc
 io.cpu.btb_req.debug_fqenq_ready := fq.io.enq.ready
 io.cpu.btb_req.s2_replay := s2_replay

//  when (!s2_replay) {
//    io.cpu.btb_req.req.valid := true.B //!s2_redirect
//    s2_btb_resp_valid := btb.io.resp.valid
//    s2_btb_resp_bits := btb.io.resp.bits
//  }
//  printf("I$ %c %c [ %d %d %d ] %x %x %x imemresp: (%c %x)\n", 
//   Mux(io.cpu.req.valid, Str("V"), Str(" ")),
//   Mux(s2_replay, Str("R"), Str(" ")),
//   s0_valid, s1_valid, s2_valid,
//   npc(15,0), s1_pc(15,0), s2_pc(15,0),
//   Mux(io.cpu.resp.valid, Str("V"), Str(" ")),
//   io.cpu.resp.bits.pc(15,0)
//   )


  io.cpu.resp <> fq.io.deq

  // performance events
  io.cpu.perf := icache.io.perf
  io.cpu.perf.tlbMiss := io.ptw.req.fire()
  io.errors := icache.io.errors

  def alignPC(pc: UInt) = ~(~pc | (coreInstBytes - 1))

  def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    cover(cond, s"FRONTEND_$label", "Rocket;;" + desc)
}

/** Mix-ins for constructing tiles that have an ICache-based pipeline frontend */
trait HasBoomICacheFrontend extends CanHavePTW { this: BaseTile =>
  val module: HasBoomICacheFrontendModule
  val frontend = LazyModule(new BoomFrontend(tileParams.icache.get, hartId))
  tlMasterXbar.node := frontend.masterNode
  connectTLSlave(frontend.slaveNode, tileParams.core.fetchBytes)
  nPTWPorts += 1
  nPTWPorts += 1 // boom -- needs an extra PTW port for its LSU.
}

trait HasBoomICacheFrontendModule extends CanHavePTWModule {
  val outer: HasBoomICacheFrontend
  ptwPorts += outer.frontend.module.io.ptw
}



///** Mix-ins for constructing tiles that have an ICache-based pipeline frontend */
//trait HasBoomICacheFrontend extends HasICacheFrontend { this: BaseTile =>
//  val module: HasBoomICacheFrontendModule
//  nPTWPorts += 1 // boom -- needs an extra PTW port for its LSU.
//}
//
//trait HasBoomICacheFrontendModule extends HasICacheFrontendModule {
//  val outer: HasBoomICacheFrontend
//}
