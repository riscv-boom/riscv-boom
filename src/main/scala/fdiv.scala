//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// FDiv/FSqrt Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2016 Feb 5


package boom

import Chisel._
import cde.Parameters

import rocket.FPConstants._


class UOPCodeFDivDecoder extends Module
{
  val io = new Bundle {
    val uopc = Bits(INPUT, UOPC_SZ)
    val sigs = new rocket.FPUCtrlSigs().asOutput
  }

   val N = BitPat("b0")
   val Y = BitPat("b1")
   val X = BitPat("b?")

   val decoder = rocket.DecodeLogic(io.uopc,
      // Note: not all of these signals are used or necessary, but we're
      // constrained by the need to fit the rocket.FPU units' ctrl signals.
      //                                                    swap12         div
      //                                                    | swap32       | sqrt
      //                            cmd                     | | single     | | round
      //                            |            ldst       | | | fromint  | | | wflags
      //                            |            | wen      | | | | toint  | | | |
      //                            |            | | ren1   | | | | | fastpipe | |
      //                            |            | | | ren2 | | | | | | fma| | | |
      //                            |            | | | | ren3 | | | | | |  | | | |
      //                            |            | | | | |  | | | | | | |  | | | |
      /* Default */            List(FCMD_X,      X,X,X,X,X, X,X,X,X,X,X,X, X,X,X,X),
      Array(
         BitPat(uopFDIV_S)  -> List(FCMD_DIV,    X,X,Y,Y,X, X,X,Y,X,X,X,X, Y,N,Y,Y),
         BitPat(uopFDIV_D)  -> List(FCMD_DIV,    X,X,Y,Y,X, X,X,N,X,X,X,X, Y,N,Y,Y),
         BitPat(uopFSQRT_S) -> List(FCMD_SQRT,   X,X,Y,N,X, X,X,Y,X,X,X,X, N,Y,Y,Y),
         BitPat(uopFSQRT_D) -> List(FCMD_SQRT,   X,X,Y,N,X, X,X,N,X,X,X,X, N,Y,Y,Y)
      ))

   val s = io.sigs
   val sigs = Seq(s.cmd, s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                  s.swap23, s.single, s.fromint, s.toint, s.fastpipe, s.fma,
                  s.div, s.sqrt, s.round, s.wflags)
   sigs zip decoder map {case(s,d) => s := d}
}


// fdiv/fsqrt is douple-precision. Must upconvert inputs and downconvert outputs
// as necessary.  Must wait till killed uop finishes before we're ready again.
// fdiv/fsqrt unit uses an unstable FIFO interface, and thus we must spend a
// cycle buffering up an uop to provide slack between the issue queue and the
// fdiv/fsqrt unit.  FDivUnit inherents directly from FunctionalUnit, because
// UnpipelinedFunctionalUnit can only handle 1 inflight uop, whereas FDivUnit
// contains up to 2 inflight uops due to the need to buffer the input as the
// fdiv unit uses an unstable FIFO interface.
// TODO extend UnpipelinedFunctionalUnit to handle a >1 uops inflight.
class FDivSqrtUnit(implicit p: Parameters) extends FunctionalUnit(is_pipelined = false
                                                                 , num_stages = 1
                                                                 , num_bypass_stages = 0
                                                                 , data_width = 65)(p)
{
   //--------------------------------------
   // buffer inputs and upconvert as needed

   // provide a one-entry queue to store incoming uops while waiting for the fdiv/fsqrt unit to become available.
   val r_buffer_val = Reg(init = Bool(false))
   val r_buffer_req = Reg(new FuncUnitReq(data_width=65))
   val r_buffer_fin = Reg(new rocket.FPInput)

   val fdiv_decoder = Module(new UOPCodeFDivDecoder)
   fdiv_decoder.io.uopc := io.req.bits.uop.uopc

   // handle branch kill on queued entry
   r_buffer_val := !IsKilledByBranch(io.brinfo, r_buffer_req.uop) && !io.req.bits.kill && r_buffer_val
   r_buffer_req.uop.br_mask := GetNewBrMask(io.brinfo, r_buffer_req.uop)

   // handle incoming uop, including upconversion as needed, and push back if our input queue is already occupied
   io.req.ready := !r_buffer_val

   def upconvert(x: UInt) =
   {
      val s2d = Module(new hardfloat.RecFNToRecFN(inExpWidth = 8, inSigWidth = 24, outExpWidth = 11, outSigWidth = 53))
      s2d.io.in := x
      s2d.io.roundingMode := UInt(0)
      s2d.io.out
   }
   val in1_upconvert = upconvert(io.req.bits.rs1_data)
   val in2_upconvert = upconvert(io.req.bits.rs2_data)

   when (io.req.valid && !IsKilledByBranch(io.brinfo, io.req.bits.uop) && !io.req.bits.kill)
   {
      r_buffer_val := Bool(true)
      r_buffer_req := io.req.bits
      r_buffer_req.uop.br_mask := GetNewBrMask(io.brinfo, io.req.bits.uop)
      r_buffer_fin := fdiv_decoder.io.sigs
      r_buffer_fin.rm := io.fcsr_rm
      r_buffer_fin.typ := UInt(0) // unused for fdivsqrt
      r_buffer_fin.in1 := io.req.bits.rs1_data
      r_buffer_fin.in2 := io.req.bits.rs2_data
      when (fdiv_decoder.io.sigs.single)
      {
         r_buffer_fin.in1 := in1_upconvert
         r_buffer_fin.in2 := in2_upconvert
      }
   }

   assert (!(r_buffer_val && io.req.valid), "[fdiv] a request is incoming while the buffer is already full.")

   //-----------
   // fdiv/fsqrt

   val divsqrt = Module(new hardfloat.DivSqrtRecF64)

   val r_divsqrt_val = Reg(init = Bool(false))  // inflight uop?
   val r_divsqrt_killed = Reg(Bool())           // has inflight uop been killed?
   val r_divsqrt_fin = Reg(new rocket.FPInput)
   val r_divsqrt_uop = Reg(new MicroOp)

   val output_buffer_available = Wire(Bool())

   val divsqrt_ready = Mux(divsqrt.io.sqrtOp, divsqrt.io.inReady_sqrt, divsqrt.io.inReady_div)
   divsqrt.io.inValid := r_buffer_val && (r_buffer_fin.div || r_buffer_fin.sqrt) && !r_divsqrt_val
   divsqrt.io.sqrtOp := r_buffer_fin.sqrt
   divsqrt.io.a := r_buffer_fin.in1
   divsqrt.io.b := Mux(divsqrt.io.sqrtOp, r_buffer_fin.in1, r_buffer_fin.in2)
   divsqrt.io.roundingMode := r_buffer_fin.rm

   r_divsqrt_killed := r_divsqrt_killed || IsKilledByBranch(io.brinfo, r_divsqrt_uop) || io.req.bits.kill
   r_divsqrt_uop.br_mask := GetNewBrMask(io.brinfo, r_divsqrt_uop)

   when (divsqrt.io.inValid &&
         divsqrt_ready &&
         !IsKilledByBranch(io.brinfo, r_buffer_req.uop) &&
         !io.req.bits.kill &&
         output_buffer_available
         )
   {
      r_buffer_val := Bool(false) // remove the entry from the buffer
      r_divsqrt_val := Bool(true)
      r_divsqrt_killed := Bool(false)
      r_divsqrt_fin := r_buffer_fin
      r_divsqrt_uop := r_buffer_req.uop
      r_divsqrt_uop.br_mask := GetNewBrMask(io.brinfo, r_buffer_req.uop)
   }

   //-----------------------------------------
   // buffer output and down-convert as needed

   val r_out_val = Reg(init=Bool(false))
   val r_out_uop = Reg(new MicroOp)
   val r_out_flags_double = Reg(Bits())
   val r_out_wdata_double = Reg(Bits())

   output_buffer_available := !r_out_val

   r_out_uop.br_mask := GetNewBrMask(io.brinfo, r_out_uop)

   when (io.resp.ready || IsKilledByBranch(io.brinfo, r_out_uop) || io.req.bits.kill)
   {
      r_out_val := Bool(false)
   }
   when (divsqrt.io.outValid_div || divsqrt.io.outValid_sqrt)
   {
      r_divsqrt_val := Bool(false)

      r_out_val := !r_divsqrt_killed && !IsKilledByBranch(io.brinfo, r_divsqrt_uop) && !io.req.bits.kill
      r_out_uop := r_divsqrt_uop
      r_out_uop.br_mask := GetNewBrMask(io.brinfo, r_divsqrt_uop)
      r_out_wdata_double := divsqrt.io.out
      r_out_flags_double := divsqrt.io.exceptionFlags
   }

   assert (!(r_out_val && (divsqrt.io.outValid_div || divsqrt.io.outValid_sqrt)),
      "[fdiv] Buffered output being overwritten by another output from the fdiv/fsqrt unit.")

   val downvert_d2s = Module(new hardfloat.RecFNToRecFN(
      inExpWidth = 11, inSigWidth = 53, outExpWidth = 8, outSigWidth = 24))
   downvert_d2s.io.in := r_out_wdata_double
   downvert_d2s.io.roundingMode := r_divsqrt_fin.rm
   val out_flags = r_out_flags_double | Mux(r_divsqrt_fin.single, downvert_d2s.io.exceptionFlags, Bits(0))

   io.resp.valid := r_out_val && !IsKilledByBranch(io.brinfo, r_out_uop)
   io.resp.bits.uop := r_out_uop
   io.resp.bits.data := Mux(r_divsqrt_fin.single, downvert_d2s.io.out, r_out_wdata_double)
   io.resp.bits.fflags.valid := io.resp.valid
   io.resp.bits.fflags.bits.uop := r_out_uop
   io.resp.bits.fflags.bits.uop.br_mask := GetNewBrMask(io.brinfo, r_out_uop)
   io.resp.bits.fflags.bits.flags := out_flags
}

