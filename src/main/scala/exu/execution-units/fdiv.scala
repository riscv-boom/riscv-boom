//******************************************************************************
// Copyright (c) 2016 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// FDiv/FSqrt Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.FPConstants._
import freechips.rocketchip.tile
import boom.common._
import boom.util._
import freechips.rocketchip.tile.HasFPUParameters
import freechips.rocketchip.util.uintToBitPat

/**
 * Decoder for FPU divide and square root signals
 */
class UOPCodeFDivDecoder(implicit p: Parameters) extends BoomModule
  with HasFPUParameters
{
  val io = IO(new Bundle {
    val uopc = Input(Bits(UOPC_SZ.W))
    val sigs = Output(new tile.FPInput)
  })

  val N = BitPat("b0")
  val Y = BitPat("b1")
  val X = BitPat("b?")

  val decoder = freechips.rocketchip.rocket.DecodeLogic(io.uopc,
    // Note: not all of these signals are used or necessary, but we're
    // constrained by the need to fit the rocket.FPU units' ctrl signals.
    //                                      swap12         fma
    //                                      | swap32       | div
    //                                      | | typeTagIn  | | sqrt
    //                           ldst       | | | typeTagOut | | wflags
    //                           | wen      | | | | from_int | | |
    //                           | | ren1   | | | | | to_int | | |
    //                           | | | ren2 | | | | | | fast | | |
    //                           | | | | ren3 | | | | | |  | | | |
    //                           | | | | |  | | | | | | |  | | | |
    /* Default */           List(X,X,X,X,X, X,X,X,X,X,X,X, X,X,X,X),
    Array(
      BitPat(uopFDIV_S)  -> List(X,X,Y,Y,X, X,X,S,S,X,X,X, X,Y,N,Y),
      BitPat(uopFDIV_D)  -> List(X,X,Y,Y,X, X,X,D,D,X,X,X, X,Y,N,Y),
      BitPat(uopFSQRT_S) -> List(X,X,Y,N,X, X,X,S,S,X,X,X, X,N,Y,Y),
      BitPat(uopFSQRT_D) -> List(X,X,Y,N,X, X,X,D,D,X,X,X, X,N,Y,Y)
    ): Array[(BitPat, List[BitPat])])

  val s = io.sigs
  io.sigs := DontCare
  val sigs = Seq(s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                 s.swap23, s.typeTagIn, s.typeTagOut, s.fromint, s.toint, s.fastpipe, s.fma,
                 s.div, s.sqrt, s.wflags)
  sigs zip decoder map {case(s,d) => s := d}
}

/**
  * Modern implementation that doesn't rely on upconverting, compared to original FDivSqrtUnit
  */
class FDivSqrtUnit2(implicit p: Parameters)
  extends FunctionalUnit(
    dataWidth = 65,
    needsFcsr = true)
  with tile.HasFPUParameters
{
  val divSqrt_inFlight = WireInit(false.B)

  val r_req = Reg(Valid(new FuncUnitReq(dataWidth=65)))
  val r_sigs = Reg(new tile.FPUCtrlSigs)
  val r_rm = Reg(UInt())
  val r_out_valid = Reg(Bool())
  val r_out_flags = Reg(UInt())
  val r_out_wdata = Reg(UInt())
  val kill = IsKilledByBranch(io.brupdate, io.kill, r_req)
  io.req.ready := !r_req.valid

  val fdiv_decoder = Module(new UOPCodeFDivDecoder)
  fdiv_decoder.io.uopc := io.req.bits.uop.uopc

  r_req := UpdateBrMask(io.brupdate, io.kill, r_req)
  r_out_valid := r_out_valid && !kill
  io.req.ready := !r_req.valid && !divSqrt_inFlight && !r_out_valid

  val fpiu = Module(new tile.FPToInt)
  fpiu.io.in.valid := io.req.valid
  fpiu.io.in.bits  := fdiv_decoder.io.sigs
  fpiu.io.in.bits.rm := Mux(io.req.bits.uop.fp_rm === 7.U, io.fcsr_rm, io.req.bits.uop.fp_rm)
  fpiu.io.in.bits.in1 := unbox(io.req.bits.rs1_data, fdiv_decoder.io.sigs.typeTagIn, None)
  fpiu.io.in.bits.in2 := unbox(io.req.bits.rs2_data, fdiv_decoder.io.sigs.typeTagIn, None)
  fpiu.io.in.bits.in3 := DontCare
  fpiu.io.in.bits.typ := io.req.bits.uop.fp_typ
  fpiu.io.in.bits.fmaCmd := 0.U

  when (io.req.fire()) {
    r_req.valid := !IsKilledByBranch(io.brupdate, io.kill, io.req.bits.uop.br_mask)
    r_req.bits  := UpdateBrMask(io.brupdate, io.req.bits)
    r_sigs := fdiv_decoder.io.sigs
    r_rm := io.fcsr_rm
  }
  for (t <- floatTypes) {
    val tag = r_sigs.typeTagOut
    val divSqrt = Module(new hardfloat.DivSqrtRecFN_small(t.exp, t.sig, 0))
    divSqrt.io.inValid := r_req.valid && (tag === typeTag(t).U) && (r_sigs.div || r_sigs.sqrt) && !divSqrt_inFlight
    divSqrt.io.sqrtOp := r_sigs.sqrt
    divSqrt.io.a := maxType.unsafeConvert(fpiu.io.out.bits.in.in1, t)
    divSqrt.io.b := maxType.unsafeConvert(fpiu.io.out.bits.in.in2, t)
    divSqrt.io.roundingMode := fpiu.io.out.bits.in.rm
    divSqrt.io.detectTininess := hardfloat.consts.tininess_afterRounding
    when (!divSqrt.io.inReady) { divSqrt_inFlight := true.B }
    when (divSqrt.io.outValid_div || divSqrt.io.outValid_sqrt) {
      r_out_valid := r_req.valid && !kill
      r_out_flags := divSqrt.io.exceptionFlags
      r_out_wdata := box(sanitizeNaN(divSqrt.io.out, t), tag)
    }
  }

  io.resp.valid := r_out_valid && r_req.valid
  io.resp.bits.uop := r_req.bits.uop
  io.resp.bits.data := r_out_wdata
  io.resp.bits.fflags.valid := io.resp.valid
  io.resp.bits.fflags.bits  := r_out_flags
  when (io.resp.fire() || reset.asBool) {
    r_req.valid := false.B
    r_out_valid := false.B
  }
}
