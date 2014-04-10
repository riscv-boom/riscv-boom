package BOOM

import Chisel._
import Node._
import uncore._

import rocket.ICacheConfig
import rocket.ICache
import rocket.Util._

class FrontendReq(implicit conf: ICacheConfig) extends Bundle {
  val pc = UInt(width = conf.as.vaddrBits+1)
  val mispredict = Bool()
  val taken = Bool()
  val currentpc = UInt(width = conf.as.vaddrBits+1)
  val btb_correct_target= UInt(width = conf.as.vaddrBits+1)
  override def clone = new FrontendReq().asInstanceOf[this.type]
}

class FrontendResp(implicit conf: ICacheConfig) extends Bundle {
  val mask = Bits(width = (conf.ibytes/4)) // mark which words are valid instructions
  val pc = UInt(width = conf.as.vaddrBits+1)  // ID stage PC
  val data = Bits(width = conf.ibytes*8)
  val taken = Bool() // the BTB took the branch
  val taken_idx = UInt() // the BTB took the branch, which inst in the packet had the branch
  val debug_taken_pc = UInt() // debug -- remember the target from the BTB
  val xcpt_ma = Bool()
  val xcpt_if = Bool()
  val bht_pc = UInt(width = conf.as.vaddrBits+1) // IF stage PC

  override def clone = new FrontendResp().asInstanceOf[this.type]
}
 
class CPUFrontendIO(implicit conf: ICacheConfig) extends Bundle {
  val req = Valid(new FrontendReq)
  val resp = Decoupled(new FrontendResp).flip
  val ptw = new rocket.TLBPTWIO()(conf.as).flip
  val invalidate = Bool(OUTPUT)
}

class Frontend(implicit c: ICacheConfig) extends Module
{
  implicit val (tl, as) = (c.tl, c.as)
  val io = new Bundle {
    val cpu = new CPUFrontendIO()(c).flip
    val mem = new UncachedTileLinkIO
  }
  
  val btb = Module(new BTB(FETCH_WIDTH)(c.btb))
  val icache = Module(new ICache)
  val tlb = Module(new rocket.TLB(c.ntlb))

  val s1_pc_ = Reg(UInt())
  val s1_pc = s1_pc_ & SInt(-2) // discard LSB of PC (throughout the pipeline)
  val s1_same_block = Reg(Bool())
  val s2_valid = Reg(init=Bool(true))
  val s2_pc = Reg(init=UInt(START_ADDR))
  val s2_btb_hit = Reg(init=Bool(false))
  val s2_btb_hit_idx = Reg(UInt())
  val s2_xcpt_if = Reg(init=Bool(false))
  val s2_debug_taken_pc = Reg(UInt())
  
  // for now, hardhacks for 4 bytes and 8 byte fetches
  require (c.ibytes == 4 || c.ibytes == 8)
  val msb = c.as.vaddrBits-1
  val lsb = log2Up(c.ibytes)
  val btbTarget = Cat(btb.io.target(c.as.vaddrBits-1), btb.io.target)
  // round to the nearest "ibyte"-th word
  // i love that this is called "pcp4", even though it's parameterizable 
  val pcp4_0 = s1_pc + UInt(c.ibytes)
  val pcp4 = Cat(s1_pc(msb) & pcp4_0(msb), pcp4_0(msb,lsb), Bits(0,lsb))     
  val icmiss = s2_valid && !icache.io.resp.valid
  debug(icmiss)
  val predicted_npc = Mux(btb.io.hit, btbTarget, pcp4)
  val npc = Mux(icmiss, s2_pc, predicted_npc).toUInt
  val s0_same_block = !icmiss && !io.cpu.req.valid && (predicted_npc >> log2Up(c.rowbytes)) === (s1_pc >> log2Up(c.rowbytes))

  val stall = io.cpu.resp.valid && !io.cpu.resp.ready
  when (!stall) {
    s1_same_block := s0_same_block && !tlb.io.resp.miss
    s1_pc_ := npc
    s2_valid := !icmiss
    when (!icmiss) {
      s2_pc := s1_pc
      s2_btb_hit := btb.io.hit
      s2_btb_hit_idx := btb.io.hit_idx
      s2_xcpt_if := tlb.io.resp.xcpt_if
      s2_debug_taken_pc := btbTarget
    }
  }
  when (io.cpu.req.valid) {
    s1_same_block := Bool(false)
    s1_pc_ := io.cpu.req.bits.pc
    s2_valid := Bool(false)
  }

  btb.io.current_pc := s1_pc
  btb.io.wen := io.cpu.req.bits.mispredict
  btb.io.clr := !io.cpu.req.bits.taken
  btb.io.correct_pc := io.cpu.req.bits.currentpc
//  btb.io.correct_target := io.cpu.req.bits.pc
  btb.io.correct_target := io.cpu.req.bits.btb_correct_target // celio was here
  btb.io.invalidate := io.cpu.invalidate || io.cpu.ptw.invalidate

  tlb.io.ptw <> io.cpu.ptw
  tlb.io.req.valid := !stall && !icmiss
  tlb.io.req.bits.vpn := s1_pc >> UInt(c.as.pgIdxBits)
  tlb.io.req.bits.asid := UInt(0)
  tlb.io.req.bits.passthrough := Bool(false)
  tlb.io.req.bits.instruction := Bool(true)

  icache.io.mem <> io.mem
  icache.io.req.valid := !stall && !s0_same_block
  icache.io.req.bits.idx := Mux(io.cpu.req.valid, io.cpu.req.bits.pc, npc)
  icache.io.invalidate := io.cpu.invalidate
  icache.io.req.bits.ppn := tlb.io.resp.ppn
  icache.io.req.bits.kill := io.cpu.req.valid || tlb.io.resp.miss || icmiss
  icache.io.resp.ready := !stall && !s1_same_block

  io.cpu.resp.valid := s2_valid && (s2_xcpt_if || icache.io.resp.valid)
  if (c.ibytes == 4)
  {
     io.cpu.resp.bits.mask := Bits(1)
  }
  else
  {
     io.cpu.resp.bits.mask := Mux(s2_pc(2), Bits(2), Bits(3))
  }
  io.cpu.resp.bits.pc := s2_pc
  io.cpu.resp.bits.data := icache.io.resp.bits.datablock >> (s2_pc(log2Up(c.rowbytes)-1,log2Up(c.ibytes)) << log2Up(c.ibytes*8))
  io.cpu.resp.bits.taken := s2_btb_hit
  io.cpu.resp.bits.taken_idx := s2_btb_hit_idx
  io.cpu.resp.bits.debug_taken_pc := s2_debug_taken_pc
  io.cpu.resp.bits.xcpt_ma := s2_pc(log2Up(c.ibytes)-1,0) != UInt(0)
  io.cpu.resp.bits.xcpt_if := s2_xcpt_if
  io.cpu.resp.bits.bht_pc := s1_pc
}
