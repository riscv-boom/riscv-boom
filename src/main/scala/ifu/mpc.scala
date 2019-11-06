package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch
import chisel3.internal.sourceinfo.{SourceInfo}


import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._


import boom.common._
import boom.exu.{CommitExceptionSignals, BranchDecode, BrUpdateInfo}
import boom.util.{BoomCoreStringPrefix, MaskLower, MaskUpper}

class MispredictCache(implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFrontendParameters
{
  val io = IO(new Bundle {
    val flush = Input(Bool())

    // // Requested when the CPU redirects
    val redirect_val = Input(Bool())
    val redirect_pc  = Input(UInt(vaddrBitsExtended.W))
    val redirect_ghist = Input(new GlobalHistory)

    val resp = Decoupled(new FetchBundle)

    val mpc_redirect_val   = Output(Bool())
    val mpc_redirect_pc    = Output(UInt(vaddrBitsExtended.W))
    val mpc_redirect_ghist = Output(new GlobalHistory)

    val set_prev_half      = Output(Valid(UInt(16.W)))

    // Enqueue a stream of non-cfi insructions
    val enq           = Flipped(Valid(new FetchBundle))

  })
  val nStreams = 4
  val nPackets = 3

  def createContinuousFetchBundle(bundle: FetchBundle) = {
    val out = WireInit(bundle)
    val seen_cfis = bundle.mask & (bundle.br_mask.asUInt |
      (UIntToOH(bundle.cfi_idx.bits).asUInt & Fill(fetchWidth, bundle.cfi_idx.valid)))
    val first_cfi = PriorityEncoder(seen_cfis)
    val first_cfi_sub2 = if (nBanks == 2) {
      first_cfi === bankWidth.U && bundle.edge_inst(1)
    } else {
      false.B
    }
    val first_cfi_pc = (bankAlign(bundle.pc)
      + (first_cfi << 1)
      - Mux(first_cfi_sub2, 2.U, 0.U))
    val include_first_cfi = (bundle.cfi_idx.valid &&
      bundle.cfi_idx.bits === first_cfi &&
      bundle.cfi_type === CFI_JAL)

    out.mask := ((bundle.mask & ~MaskUpper(seen_cfis)) |
      Mux(include_first_cfi, UIntToOH(bundle.cfi_idx.bits), 0.U))
    out.next_pc := Mux(include_first_cfi, bundle.next_pc,
                   Mux(seen_cfis =/= 0.U, first_cfi_pc,
                                          bundle.next_pc))
    out.cfi_idx.valid := include_first_cfi
    out.end_half.valid := bundle.end_half.valid && seen_cfis === 0.U
    out
  }
  val bundle = createContinuousFetchBundle(io.enq.bits)

  val start_addrs  = Reg(Vec(nStreams, UInt(vaddrBitsExtended.W)))
  val end_addrs    = Reg(Vec(nStreams, UInt(vaddrBitsExtended.W)))
  val end_half     = Reg(Vec(nStreams, Valid(UInt(16.W))))
  val bundles      = Reg(Vec(nStreams, Vec(3, new FetchBundle)))
  val counts       = Reg(Vec(nStreams, UInt(2.W)))

  val plru = new PseudoLRU(nStreams)

  // Enq new streams
  val end_addr_hits = VecInit((0 until nStreams).map{ i =>
    counts(i) =/= 0.U && end_addrs(i) === bundle.pc
  })
  val enq_idx = PriorityEncoder(end_addr_hits)
  when (io.enq.valid) {
    when (end_addr_hits.reduce(_||_) && bundle.mask =/= 0.U) {
      when (counts(enq_idx) < 3.U) {
        end_addrs(enq_idx)                  := bundle.next_pc
        counts   (enq_idx)                  := counts(enq_idx) + 1.U
        bundles  (enq_idx)(counts(enq_idx)) := bundle
        end_half (enq_idx)                  := bundle.end_half
      }

    } .elsewhen (bundle.tsrc === BSRC_C && bundle.mask =/= 0.U) {
      val enq_new_idx = plru.replace
      start_addrs(enq_new_idx)    := bundle.pc
      end_addrs  (enq_new_idx)    := bundle.next_pc
      counts     (enq_new_idx)    := 1.U
      bundles    (enq_new_idx)(0) := bundle
      end_half   (enq_new_idx)    := bundle.end_half
      plru.access(enq_new_idx)
    }
  }

  val req_addr_hits = VecInit((0 until nStreams).map{ i =>
    counts(i) =/= 0.U && start_addrs(i) === io.redirect_pc
  })
  val req_hit     = req_addr_hits.reduce(_||_)
  val req_hit_idx = PriorityEncoder(req_addr_hits)
  val do_deq      = RegInit(false.B)
  val do_deq_idx  = Reg(UInt(log2Ceil(nStreams).W))
  val do_deq_count  = RegInit(0.U(2.W))
  val mpc_redirect  = Wire(Valid(UInt(vaddrBitsExtended.W)))
  val set_prev_half = Wire(Valid(UInt(16.W)))
  set_prev_half.valid := false.B
  set_prev_half.bits  := DontCare
  mpc_redirect.valid := false.B
  mpc_redirect.bits  := DontCare

  io.mpc_redirect_val   := RegNext(mpc_redirect.valid)
  io.mpc_redirect_pc    := RegNext(mpc_redirect.bits)
  io.mpc_redirect_ghist := RegNext(io.redirect_ghist)
  io.set_prev_half      := RegNext(set_prev_half)

  io.resp.valid := false.B
  io.resp.bits  := DontCare

  when (io.redirect_val && req_hit) {
    do_deq     := true.B
    do_deq_idx := req_hit_idx
    do_deq_count := 0.U
    mpc_redirect.valid   := true.B
    mpc_redirect.bits    := end_addrs(req_hit_idx)
    set_prev_half.valid  := end_half(req_hit_idx).valid
    set_prev_half.bits   := end_half(req_hit_idx).bits
    plru.access(req_hit_idx)
  } .elsewhen (do_deq && !io.flush) {
    when (do_deq_count < counts(do_deq_idx) && !io.redirect_val) {
      io.resp.valid := true.B
      io.resp.bits  := bundles(do_deq_idx)(do_deq_count)
      when (io.resp.fire()) {
        do_deq_count := do_deq_count + 1.U
      }
    } .otherwise {
      do_deq := false.B
    }
  }

  when (reset.asBool || io.flush) {
    counts   := VecInit(Seq.fill(nStreams) { 0.U })
    do_deq   := false.B
  }
}

