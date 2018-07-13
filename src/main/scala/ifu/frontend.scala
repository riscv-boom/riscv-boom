// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package boom.ifu


import chisel3._
import chisel3.util.{DecoupledIO, log2Ceil, Cat, Valid, Fill, PriorityMux, PriorityEncoderOH, PopCount, OHToUInt}
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import boom.bpu._
import boom.common._
import boom.exu.{BranchUnitResp, FlushSignals}
import boom.lsu.{CanHaveBoomPTW, CanHaveBoomPTWModule}

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

trait HasL1ICacheBankedParameters extends HasL1ICacheParameters with HasBoomCoreParameters
{
  // Use a bank interleaved I$ if our fetch width is wide enough.
  val icIsBanked = fetchBytes > 8 // TODO RVC
  // How many bytes wide is a bank?
  val bankBytes = if (icIsBanked) fetchBytes/2 else fetchBytes
  // How many "chunks"/interleavings make up a cache line?
  def numChunks = cacheParams.blockBytes / bankBytes

  // Which bank is the address pointing to?
  def bank(addr: UInt) = addr(log2Ceil(bankBytes))
  def inLastChunk(addr: UInt) = addr(blockOffBits-1, log2Ceil(bankBytes)) === (numChunks-1).U

  // Round address down to the nearest fetch boundary.
  def alignToFetchBoundary(addr: UInt) =
  {
    ~(~addr | (fetchBytes-1).U)
  }

  // Round address down to the nearest fetch boundary.
  def alignToFetchBoundary1(addr: UInt) =
  {
    if (icIsBanked) ~(~addr | (bankBytes-1).U)
    else ~(~addr | (fetchBytes-1).U)
  }

  // Input: an ALIGNED pc.
  // For the fall-through next PC, where does the next fetch pc start?
  // This is complicated by cache-line wraparound.
  def nextFetchStart(addr: UInt) =
  {
    if (icIsBanked)
    {
      addr + Mux(bank(addr), bankBytes.U, fetchBytes.U)
    } else {
      addr + fetchBytes.U
    }
  }

  // For a given fetch address, what is the mask of validly fetched instructions.
  def cacheFetchMask(addr: UInt) =
  {
    // where is the first instruction, aligned to a log(fetchWidth) boundary?
    val idx = addr.extract(log2Ceil(rvcFetchWidth)+log2Ceil(minCoreInstBytes) - 1, log2Ceil(minCoreInstBytes))
    if (icIsBanked) {
      // shave off the msb of idx since we are aligned to half-fetchWidth boundaries.
      val shamt = idx.extract(log2Ceil(rvcFetchWidth)-1, 0)
      val end_mask = Mux(bank(addr), Cat(Fill(fetchWidth, 1.U), Fill(fetchWidth, 0.U)), Fill(rvcFetchWidth, 1.U))
      ((1 << rvcFetchWidth)-1).U << shamt & end_mask
    } else {
      ((1 << rvcFetchWidth)-1).U << idx
    }
  }

  // For a given fetch address, what is the mask of validly fetched instructions.
  def norvcFetchMask(addr: UInt) =
  {
    // where is the first instruction, aligned to a log(fetchWidth) boundary?
    val idx = addr.extract(log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))
    if (icIsBanked) {
      // shave off the msb of idx since we are aligned to half-fetchWidth boundaries.
      val shamt = idx.extract(log2Ceil(fetchWidth)-1, 0)
      val end_mask = Mux(bank(addr), Cat(Fill(fetchWidth/2, 1.U), Fill(fetchWidth/2, 0.U)), Fill(fetchWidth, 1.U))
      ((1 << fetchWidth)-1).U << shamt & end_mask
    } else {
      ((1 << fetchWidth)-1).U << idx
    }
  }
}


class BoomFrontendIO(implicit p: Parameters) extends BoomBundle()(p)
{
   // Give the backend a packet of instructions.
   val fetchpacket       = Flipped(new DecoupledIO(new FetchBufferResp))

   val br_unit           = Output(new BranchUnitResp())
   val get_pc            = Flipped(new GetPCFromFtqIO())

   val sfence            = Valid(new SFenceReq)

   // sfence needs to steal the TLB CAM part.
   // TODO redudcant with above sfenceReq
   val sfence_take_pc    = Output(Bool())
   val sfence_addr       = Output(UInt((vaddrBits+1).W))


   val commit            = Valid(UInt(log2Ceil(ftqSz).W))
   val flush_info        = Valid(new FlushSignals())
   val flush_take_pc     = Output(Bool())
   val flush_pc          = Output(UInt((vaddrBits+1).W)) // TODO rename; no longer catch-all flush_pc
   val flush_icache      = Output(Bool())

   val com_ftq_idx       = Output(UInt(log2Ceil(ftqSz).W)) // ROB tells us the commit pointer so we can read out the PC.
   val com_fetch_pc      = Input(UInt(vaddrBitsExtended.W)) // tell CSRFile the fetch-pc at the FTQ head.

   // bpd pipeline
   // should I take in the entire rob.io.flush?
   val flush             = Output(Bool()) // pipeline flush from ROB TODO CODEREVIEW (redudant with fe_clear?)
   val clear_fetchbuffer = Output(Bool()) // pipeline redirect (rob-flush, sfence request, branch mispredict)

   val status_prv        = Output(UInt(freechips.rocketchip.rocket.PRV.SZ.W))
   val status_debug      = Output(Bool())

   val perf              = Input(new FrontendPerfEvents())
   val tsc_reg           = Output(UInt(xLen.W))
}

class BoomFrontend(val icacheParams: ICacheParams, hartid: Int)(implicit p: Parameters) extends LazyModule {
  lazy val module = new BoomFrontendModule(this)
  val icache = LazyModule(new boom.ifu.ICache(icacheParams, hartid))
  val masterNode = icache.masterNode
  val slaveNode = icache.slaveNode
}

class BoomFrontendBundle(val outer: BoomFrontend) extends CoreBundle()(outer.p)
    with HasExternallyDrivenTileConstants {
  val cpu = Flipped(new BoomFrontendIO())
  val ptw = new TLBPTWIO()
  val errors = new ICacheErrors
}

class BoomFrontendModule(outer: BoomFrontend) extends LazyModuleImp(outer)
  with HasCoreParameters
  with HasL1ICacheParameters
  with HasL1ICacheBankedParameters
  with HasBoomCoreParameters
{
  val io = IO(new BoomFrontendBundle(outer))
  implicit val edge = outer.masterNode.edges.out(0)
  require(fetchWidth*coreInstBytes == outer.icacheParams.fetchBytes)

  val icache = outer.icache.module
  val tlb = Module(new TLB(true, log2Ceil(fetchBytes), nTLBEntries))
  tlb.io := DontCare
  val fetch_controller = Module(new FetchControlUnit(fetchWidth))
  fetch_controller.io := DontCare
  val bpdpipeline = Module(new BranchPredictionStage)

  override def toString: String = bpdpipeline.toString + "\n" + icache.toString

  val s0_pc = Wire(UInt(vaddrBitsExtended.W))
  val s0_valid = fetch_controller.io.imem_req.valid || fetch_controller.io.imem_resp.ready
  val s1_valid = RegNext(s0_valid)
  val s1_pc = Reg(UInt(vaddrBitsExtended.W))
  val s1_speculative = Reg(Bool())
  val s2_valid = RegInit(false.B)
  val s2_pc = RegInit(t = UInt(vaddrBitsExtended.W), alignPC(io.reset_vector))
  val s2_tlb_miss = Reg(Bool())
  val s2_tlb_prefetchable = Reg(Bool())
  val s2_tlb_cacheable = Reg(Bool())
  val s2_tlb_xcpt = Reg(new FrontendExceptions)
  val s2_xcpt = s2_tlb_xcpt.ae.inst || s2_tlb_xcpt.pf.inst
  val s2_speculative = RegInit(false.B)
  val rvcexist = Wire(Bool())
  val s2_irespvalid = Wire(Bool())
  val sendtos3 = Wire(Bool())
  val respready = Wire(Bool())
  respready := fetch_controller.io.imem_resp.ready

  val s1_base_pc = alignToFetchBoundary1(s1_pc)
  val ntpc = nextFetchStart(s1_base_pc)
  val predicted_npc = WireInit(ntpc)
  val predicted_taken = WireInit(false.B)

   val s3_valid = RegInit(false.B)
   val s4_valid = RegInit(false.B)
   val inst32across = RegInit(false.B)
   val s2_replay = Wire(Bool())
   s2_replay := (s2_valid && !fetch_controller.io.imem_resp.fire() && !(sendtos3 && icache.io.resp.valid)) || RegNext(s2_replay && !s0_valid, true.B) ||
                  (s3_valid && sendtos3 && icache.io.resp.valid && s2_valid && !respready) || (s3_valid && s4_valid) || ((s3_valid || s4_valid) && !sendtos3 && s2_irespvalid) ||
                  (s2_valid && (!RegNext(s1_valid) || !icache.io.resp.valid))
  val npc = Mux(s2_replay, s2_pc, predicted_npc)

  s1_pc := s0_pc
  // consider RVC fetches across blocks to be non-speculative if the first
  // part was non-speculative
  val s0_speculative =
    if (usingCompressed) s1_speculative || s2_valid && !s2_speculative || predicted_taken
    else true.B
  s1_speculative := Mux(fetch_controller.io.imem_req.valid, fetch_controller.io.imem_req.bits.speculative, Mux(s2_replay, s2_speculative, s0_speculative))

  val s2_redirect = WireInit(fetch_controller.io.imem_req.valid)
  s2_valid := false.B
   when (!s2_replay) {
      s2_valid := !s2_redirect
      s2_pc := s1_pc
      s2_speculative := s1_speculative
      s2_tlb_xcpt.ae.inst := tlb.io.resp.ae.inst
      s2_tlb_xcpt.pf.inst := tlb.io.resp.pf.inst
      s2_tlb_miss := tlb.io.resp.miss
      s2_tlb_prefetchable := tlb.io.resp.prefetchable
      s2_tlb_cacheable := tlb.io.resp.cacheable
   }

  io.ptw <> tlb.io.ptw
  tlb.io.req.valid := !s2_replay
  tlb.io.req.bits.vaddr := s1_pc
  tlb.io.req.bits.passthrough := false.B
  tlb.io.sfence := io.cpu.sfence
  tlb.io.req.bits.size := log2Ceil(4*fetchWidth).U

  icache.io.hartid := io.hartid
  icache.io.req.valid := s0_valid
  icache.io.req.bits.addr := s0_pc
  icache.io.invalidate := io.cpu.flush_icache
  icache.io.s1_vaddr := s1_pc
  icache.io.s1_paddr := tlb.io.resp.paddr
  icache.io.s2_vaddr := s2_pc
  icache.io.s1_kill := s2_redirect || tlb.io.resp.miss || s2_replay 
  icache.io.s2_kill := s2_speculative && !s2_tlb_cacheable || s2_xcpt
  icache.io.s2_prefetch := s2_tlb_prefetchable

  s0_pc := alignPC(Mux(fetch_controller.io.imem_req.valid, fetch_controller.io.imem_req.bits.pc, npc))
  
  val s2_respdata = icache.io.resp.bits.data
  val s2_mask = cacheFetchMask(s2_pc)
  s2_irespvalid := RegNext(s1_valid) && s2_valid && icache.io.resp.valid
  var rvcvec : Seq[Bool] = Seq() 
  var overridenext = inst32across
  for (i <- 0 until rvcFetchWidth) {
    val inst16 = !s2_respdata(16*i+1, 16*i).andR && !overridenext && s2_mask(i)
    overridenext = !inst16 && !overridenext && s2_mask(i)
    rvcvec ++= Seq(inst16) 
  }
  val rvc = VecInit(rvcvec).asUInt
  when (s2_irespvalid && !s2_replay) {
      inst32across := overridenext
      /*if (icIsBanked) { inst32across := overridenext || (!rvc(fetchWidth-1) && s2_mask(fetchWidth-1) && !s2_mask(fetchWidth)) }
      else {   inst32across := overridenext  }*/
  }
  rvcexist := rvc.orR
  sendtos3 := rvcexist || inst32across || overridenext

  fetch_controller.io.imem_resp.valid := RegNext(s1_valid) && s2_valid && ((!sendtos3 && icache.io.resp.valid) || (!s2_tlb_miss && icache.io.s2_kill && !icache.io.resp.valid)) 
  fetch_controller.io.imem_resp.bits.pc := s2_pc

  fetch_controller.io.imem_resp.bits.data := icache.io.resp.bits.data
  fetch_controller.io.imem_resp.bits.mask := norvcFetchMask(s2_pc)
  fetch_controller.io.imem_resp.bits.rvc_mask := 0.U
  fetch_controller.io.imem_resp.bits.fidx := VecInit(Seq.tabulate(fetchWidth) ( i => (Cat(0.U, s2_pc(1).asUInt) + (i << 1).U(log2Ceil(rvcFetchWidth).W))(log2Ceil(fetchWidth), 0)  ))
  fetch_controller.io.imem_resp.bits.instacross := inst32across && s2_xcpt
  fetch_controller.io.imem_resp.bits.replay := icache.io.resp.bits.replay || icache.io.s2_kill && !icache.io.resp.valid && !s2_xcpt
  fetch_controller.io.imem_resp.bits.xcpt := s2_tlb_xcpt
  fetch_controller.io.imem_resp.bits.debug_raw.map { inst => inst := 0.U }
  fetch_controller.io.imem_resp.bits.split := false.B
  fetch_controller.io.f2_split := !(bpdpipeline.io.f2_btb_resp.bits.mask & s2_mask).orR
  when (icache.io.resp.valid && icache.io.resp.bits.ae) { fetch_controller.io.imem_resp.bits.xcpt.ae.inst := true.B }


   val s3_mask = Reg(UInt(rvcFetchWidth.W))
   val s3_respdata = Reg(UInt((fetchWidth*32).W)) 
   val s3_btb_resp_bits = Reg(new BoomBTBResp)
   val s3_btb_resp_valid = Reg(Bool())
   val s3_pc = Reg(UInt(vaddrBitsExtended.W))
   val s3_prevhalf = Reg(UInt(16.W))
   val s3_rvc = Reg(UInt(rvcFetchWidth.W))
   val prev_inst32across = RegInit(false.B)
   val s3_datawire = WireInit(VecInit(Seq.fill(fetchWidth)(0.U(32.W))))
   val expunit = Seq.fill(fetchWidth)(Module(new freechips.rocketchip.rocket.RVCExpander))
   val expunitrvc = WireInit(VecInit(Seq.fill(fetchWidth)(false.B)))
   val s3_tlb_xcpt = Reg(new FrontendExceptions)
   val expunitout = WireInit(VecInit(Seq.fill(fetchWidth)(0.U(32.W))))
   val outalloted = WireInit(VecInit(Seq.fill(fetchWidth)(false.B)))

   val s4_mask = Reg(UInt(rvcFetchWidth.W))
   val s4_btb_resp_bits = Reg(new BoomBTBResp)
   val s4_btb_resp_valid = Reg(Bool())
   val s4_rvc = Reg(UInt(rvcFetchWidth.W))
   val s4_respdata = Reg(UInt((fetchWidth*16).W)) 
   val s4_pc = Reg(UInt(vaddrBitsExtended.W))
   val s4_datawire = WireInit(VecInit(Seq.fill(fetchWidth)(0.U(32.W))))
   val s4_tlb_xcpt = Reg(new FrontendExceptions)
   val s4_notallotedOH = Reg(UInt(rvcFetchWidth.W))
   val s4_outalloted = WireInit(VecInit(Seq.fill(fetchWidth)(false.B)))
   val allotedins3 = Wire(UInt(rvcFetchWidth.W))
   val addnpc = WireInit(0.U)

   when (sendtos3 && s2_irespvalid && (respready || !s3_valid) && !(s4_valid && s3_valid)) {
      s3_valid := true.B
      s3_respdata := icache.io.resp.bits.data
      s3_mask := s2_mask
      s3_pc := s2_pc
      s3_rvc := rvc
      prev_inst32across := inst32across
      s3_tlb_xcpt := s2_tlb_xcpt
      s3_btb_resp_bits := bpdpipeline.io.f2_btb_resp.bits
      s3_btb_resp_valid := bpdpipeline.io.extstageval1 && (s2_mask & bpdpipeline.io.f2_btb_resp.bits.mask).orR
   }

   /////////////////////
   // S3
   ////////////////////
   var canallot : Seq[Bool] = Seq(s3_mask(0) && (prev_inst32across || s3_rvc(0) || s3_mask(1))) 
   var canallotinst : Seq[UInt] = Seq(Mux(prev_inst32across, Cat(s3_respdata(15,0), s3_prevhalf), s3_respdata(31,0)))
   var allotedidx : Seq[UInt] = Seq()
   var nextalloted = s3_mask(1,0).andR && !(s3_rvc(0) || prev_inst32across)

   for (i <- 1 until (rvcFetchWidth-1)) {
      canallot ++= Seq(s3_mask(i) && (s3_rvc(i) || (!nextalloted && s3_mask(i+1))))
      canallotinst ++= Seq(s3_respdata(16*i+31,16*i))
      nextalloted = !nextalloted && !s3_rvc(i) && s3_mask(i)    
   }
   canallot ++= Seq(s3_mask(rvcFetchWidth-1) && s3_rvc(rvcFetchWidth-1))
   canallotinst ++= Seq(Cat(0.U, s3_respdata(16*(rvcFetchWidth-1)+15, 16*(rvcFetchWidth-1))))
   s3_datawire(0) := PriorityMux(canallot, canallotinst)
   outalloted(0) := canallot.reduce(_ | _)

   var prev_canallot = canallot
   for (j <- 1 until fetchWidth) {
      var canallot1 : Seq[Bool] = Seq()
      var canallotinst1 : Seq[UInt] = Seq()
      val allotedoh = PriorityEncoderOH(prev_canallot)
      if (j!=1) { allotedidx ++= Seq(OHToUInt(Seq.fill(j-1)(false.B) ++ allotedoh)) }
      else {   allotedidx ++= Seq(OHToUInt(allotedoh))   }
      for (i <- 1 until (rvcFetchWidth-j+1)) {
         canallot1 ++= Seq(prev_canallot(i) && !allotedoh(i))
         canallotinst1 ++= Seq(canallotinst(i+j-1))
      }
      outalloted(j) := canallot1.reduce(_ | _)
      s3_datawire(j) := PriorityMux(canallot1, canallotinst1)
      prev_canallot = canallot1
   }
    
   var canallot1 : Seq[Bool] = Seq()
   val allotedoh = PriorityEncoderOH(prev_canallot)
   allotedidx ++= Seq(OHToUInt(Seq.fill(fetchWidth-1)(false.B) ++ allotedoh))
   for (i <- 1 until (fetchWidth+1)) {   canallot1 ++= Seq(prev_canallot(i) && !allotedoh(i))   }
   prev_canallot = Seq.fill(fetchWidth)(false.B) ++ canallot1

   require(fetchWidth >= 2) 
   when (s3_valid && respready && !s4_valid) {
      fetch_controller.io.imem_resp.valid := outalloted(0)
      fetch_controller.io.imem_resp.bits.pc := s3_pc
      fetch_controller.io.imem_resp.bits.data := expunitout.asUInt
      fetch_controller.io.imem_resp.bits.mask := outalloted.asUInt
      fetch_controller.io.imem_resp.bits.rvc_mask := expunitrvc.asUInt
      fetch_controller.io.imem_resp.bits.fidx := VecInit(allotedidx)
      fetch_controller.io.imem_resp.bits.xcpt := s3_tlb_xcpt
      fetch_controller.io.imem_resp.bits.instacross := prev_inst32across
      fetch_controller.io.imem_resp.bits.debug_raw := s3_datawire
      fetch_controller.io.imem_resp.bits.split := canallot1.reduce(_ | _)
      fetch_controller.io.f2_split := canallot1.reduce(_ | _)

      when (inst32across) { // incorporate banking i.e. if fetchaddr is inLastChunk
         s3_prevhalf := s3_respdata(32*fetchWidth-1, 16*(2*fetchWidth-1))
         /*if (icIsBanked) { s3_prevhalf := Mux(s3_mask(fetchWidth-1) && !s3_mask(fetchWidth), s3_respdata(16*fetchWidth-1, 16*(fetchWidth-1)), s3_respdata(32*fetchWidth-1, 16*(2*fetchWidth-1)))   }
         else {   s3_prevhalf := s3_respdata(32*fetchWidth-1, 16*(2*fetchWidth-1))  }*/
      }
  
      s4_rvc := s3_rvc 
      s4_mask := s3_mask 
      s4_respdata := s3_respdata >> (16*fetchWidth).U
      s4_pc := s3_pc
      s4_notallotedOH := VecInit(prev_canallot).asUInt
      s4_tlb_xcpt := s3_tlb_xcpt
      s3_valid := sendtos3 && s2_irespvalid
      s4_valid := canallot1.reduce(_ | _)
      s4_btb_resp_bits := s3_btb_resp_bits
      s4_btb_resp_valid := s3_btb_resp_valid
   }
   ////////////////////


   /////////////////////
   // S4
   ////////////////////

   allotedins3 := s4_mask & ~(s4_notallotedOH | (s4_notallotedOH << 1))
   addnpc := PopCount(allotedins3.toBools) << 1

   val smask = s4_mask >> fetchWidth.U
   val srvc = s4_rvc >> fetchWidth.U
   val snotallot = s4_notallotedOH >> fetchWidth.U
   var s4_canallot : Seq[Bool] = Seq() 
   var s4_canallotinst : Seq[UInt] = Seq()
   var s4_allotedidx : Seq[UInt] = Seq()
   for (i <- 0 until (fetchWidth-1)) {
      s4_canallot ++= Seq(snotallot(i))
      s4_canallotinst ++= Seq(s4_respdata(16*i+31,16*i))
   }
   s4_canallot ++= Seq(snotallot(fetchWidth-1))
   s4_canallotinst ++= Seq(Cat(0.U, s4_respdata(16*(fetchWidth-1)+15, 16*(fetchWidth-1))))
   s4_datawire(0) := PriorityMux(s4_canallot, s4_canallotinst)
   s4_outalloted(0) := s4_canallot.reduce(_ | _)

   var s4_prev_canallot = s4_canallot
   for (j <- 1 until fetchWidth) {
      var canallot1 : Seq[Bool] = Seq()
      var canallotinst1 : Seq[UInt] = Seq()
      val allotedoh = PriorityEncoderOH(s4_prev_canallot)
      s4_allotedidx ++= Seq(OHToUInt(Seq.fill(j-1+fetchWidth)(false.B) ++ allotedoh))
      for (i <- 1 until (fetchWidth-j+1)) {
         canallot1 ++= Seq(s4_prev_canallot(i) && !allotedoh(i))
         canallotinst1 ++= Seq(s4_canallotinst(i+j-1))
      }
      s4_outalloted(j) := canallot1.reduce(_ | _)
      s4_datawire(j) := PriorityMux(canallot1, canallotinst1)
      s4_prev_canallot = canallot1
   }
   s4_allotedidx ++= Seq(Mux(s4_prev_canallot(0), (rvcFetchWidth-1).U, 0.U))

   when (s4_valid && respready) {
      fetch_controller.io.imem_resp.valid := true.B
      fetch_controller.io.imem_resp.bits.pc := s4_pc + addnpc
      fetch_controller.io.imem_resp.bits.data := expunitout.asUInt
      fetch_controller.io.imem_resp.bits.mask := s4_outalloted.asUInt 
      fetch_controller.io.imem_resp.bits.rvc_mask := expunitrvc.asUInt
      fetch_controller.io.imem_resp.bits.fidx := VecInit(s4_allotedidx)
      fetch_controller.io.imem_resp.bits.xcpt := s4_tlb_xcpt
      fetch_controller.io.imem_resp.bits.instacross := false.B
      fetch_controller.io.imem_resp.bits.split := false.B
      fetch_controller.io.imem_resp.bits.debug_raw := s4_datawire
      fetch_controller.io.f2_split := false.B
      s4_valid := false.B
   }

   when (s2_redirect) {
      s4_valid := false.B
      s3_valid := false.B
      inst32across := false.B
   }
  ////////////////////


   ////////////////////
   //  S3/S4
   ////////////////////
   for ((exp, i) <- expunit.zipWithIndex) {
      exp.io.in := Mux(s4_valid, s4_datawire(i), s3_datawire(i))
      expunitout(i) := exp.io.out.bits 
      expunitrvc(i) := exp.io.rvc
   }
  ////////////////////



  //-------------------------------------------------------------
  // **** Fetch Controller ****
  //-------------------------------------------------------------

   fetch_controller.io.br_unit           := io.cpu.br_unit
   fetch_controller.io.tsc_reg           := io.cpu.tsc_reg

   fetch_controller.io.f2_btb_resp       := bpdpipeline.io.f2_btb_resp 
   fetch_controller.io.f2_btb_resp.valid := !(sendtos3 || s2_replay) && bpdpipeline.io.f2_btb_resp.valid && (s2_mask & bpdpipeline.io.f2_btb_resp.bits.mask).orR
   fetch_controller.io.f2_btb_resp.bits.mask := VecInit(Seq.tabulate(fetchWidth)( i => bpdpipeline.io.f2_btb_resp.bits.mask(2*i+1, 2*i).orR)).asUInt
   when (s3_valid) {
      fetch_controller.io.f2_btb_resp.bits    := s3_btb_resp_bits  
      fetch_controller.io.f2_btb_resp.valid   := s3_btb_resp_valid && respready && !s4_valid
      fetch_controller.io.f2_btb_resp.bits.mask := VecInit(Seq.tabulate(fetchWidth)( i => (s3_btb_resp_bits.mask(allotedidx(i)) && outalloted(i)) )).asUInt
   }
   when (s4_valid) {
      fetch_controller.io.f2_btb_resp.bits    := s4_btb_resp_bits
      fetch_controller.io.f2_btb_resp.valid   := s4_btb_resp_valid && respready
      fetch_controller.io.f2_btb_resp.bits.mask := VecInit(Seq.tabulate(fetchWidth)( i => (s4_btb_resp_bits.mask(s4_allotedidx(i)) && s4_outalloted(i)) )).asUInt
   }
   fetch_controller.io.f3_bpd_resp       := bpdpipeline.io.f3_bpd_resp

   fetch_controller.io.clear_fetchbuffer := io.cpu.clear_fetchbuffer

   fetch_controller.io.sfence_take_pc    := io.cpu.sfence_take_pc
   fetch_controller.io.sfence_addr       := io.cpu.sfence_addr

   fetch_controller.io.flush_take_pc     := io.cpu.flush_take_pc
   fetch_controller.io.flush_pc          := io.cpu.flush_pc
   fetch_controller.io.com_ftq_idx       := io.cpu.com_ftq_idx

   fetch_controller.io.flush_info        := io.cpu.flush_info
   fetch_controller.io.commit            := io.cpu.commit

   io.cpu.get_pc <> fetch_controller.io.get_pc

   io.cpu.com_fetch_pc := fetch_controller.io.com_fetch_pc

   io.cpu.fetchpacket <> fetch_controller.io.fetchpacket

   //-------------------------------------------------------------
   // **** Branch Prediction ****
   //-------------------------------------------------------------

   bpdpipeline.io.s0_req.valid := s0_valid
   bpdpipeline.io.s0_req.bits.addr := s0_pc


   bpdpipeline.io.f2_replay := s2_replay
   bpdpipeline.io.extstageval := s3_valid && sendtos3 && s2_irespvalid && !s4_valid && respready
   bpdpipeline.io.s4_valid := s4_valid
   bpdpipeline.io.s3_valid := s3_valid
   bpdpipeline.io.respready := respready
   bpdpipeline.io.capture := sendtos3 && s2_irespvalid && (respready || !s3_valid) && !(s3_valid && s4_valid)
   bpdpipeline.io.split := fetch_controller.io.imem_resp.bits.split
   bpdpipeline.io.f2_stall := !fetch_controller.io.imem_resp.ready  
   bpdpipeline.io.f3_stall := fetch_controller.io.f3_stall 
   bpdpipeline.io.f3_is_br := fetch_controller.io.f3_is_br
   bpdpipeline.io.debug_imemresp_pc := fetch_controller.io.imem_resp.bits.pc

   bpdpipeline.io.br_unit := io.cpu.br_unit
   bpdpipeline.io.ftq_restore := fetch_controller.io.ftq_restore_history
   bpdpipeline.io.redirect := fetch_controller.io.imem_req.valid

   bpdpipeline.io.flush := io.cpu.flush

   bpdpipeline.io.f2_valid := fetch_controller.io.imem_resp.fire()
   bpdpipeline.io.f2_redirect := fetch_controller.io.f2_redirect
   bpdpipeline.io.f4_redirect := fetch_controller.io.f4_redirect
   bpdpipeline.io.f4_taken := fetch_controller.io.f4_taken
   bpdpipeline.io.fe_clear := fetch_controller.io.clear_fetchbuffer

   bpdpipeline.io.f3_ras_update := fetch_controller.io.f3_ras_update
   bpdpipeline.io.f3_btb_update := fetch_controller.io.f3_btb_update
   bpdpipeline.io.bim_update    := fetch_controller.io.bim_update
   bpdpipeline.io.bpd_update    := fetch_controller.io.bpd_update



   bpdpipeline.io.status_prv    := io.cpu.status_prv
   bpdpipeline.io.status_debug  := io.cpu.status_debug

  //-------------------------------------------------------------
  // performance events
  io.cpu.perf := icache.io.perf
  io.cpu.perf.tlbMiss := io.ptw.req.fire()
  io.errors := icache.io.errors


  def alignPC(pc: UInt) = ~(~pc | (minCoreInstBytes - 1).U)
}

/** Mix-ins for constructing tiles that have an ICache-based pipeline frontend */
trait HasBoomICacheFrontend extends CanHaveBoomPTW { this: BaseTile =>
  val module: HasBoomICacheFrontendModule
  val frontend = LazyModule(new BoomFrontend(tileParams.icache.get, hartId))
  tlMasterXbar.node := frontend.masterNode
  connectTLSlave(frontend.slaveNode, tileParams.core.fetchBytes)
  nPTWPorts += 1
  nPTWPorts += 1 // boom -- needs an extra PTW port for its LSU.
}

trait HasBoomICacheFrontendModule extends CanHaveBoomPTWModule {
  val outer: HasBoomICacheFrontend
  ptwPorts += outer.frontend.module.io.ptw
}

