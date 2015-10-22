//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom
{
   // Note: fdiv, fsqrt unsupported.
   // Note: (this FPU currently only supports fixed latency ops)

import Chisel._
import Node._
import cde.Parameters

import rocket.FPConstants._
import rocket.FPUCtrlSigs

import rocket.SFMALatency
import rocket.DFMALatency


// TODO get rid of this decoder and move into the Decode stage? Or the RRd stage?
// most of these signals are already created, just need to be translated
// to the Rocket FPU-speak
class UOPCodeFPUDecoder extends Module
{
  val io = new Bundle {
    val uopc = Bits(INPUT, UOPC_SZ)
    val sigs = new FPUCtrlSigs().asOutput
  }

   val N = Bool(false)
   val Y = Bool(true)
   val X = Bool(false)

   val default: List[BitPat] = List(FCMD_X,    X,X,X,X,X, X,X,X,X,X,X,X, X,X,X,X)

   val table: Array[(BitPat, List[BitPat])] =
      // Note: not all of these signals are used or necessary, but we're
      // constrained by the need to fit the rocket.FPU units' ctrl signals.
      //                                          swap12         div
      //                                          | swap32       | sqrt
      //                  cmd                     | | single     | | round
      //                  |            ldst       | | | fromint  | | | wflags
      //                  |            | wen      | | | | toint  | | | |
      //                  |            | | ren1   | | | | | fastpipe | |
      //                  |            | | | ren2 | | | | | | fma| | | |
      //                  |            | | | | ren3 | | | | | |  | | | |
      //                  |            | | | | |  | | | | | | |  | | | |
      Array(
      BitPat(uopFCLASS_S) -> List(FCMD_MV_XF,  X,X,Y,N,N, N,X,Y,N,Y,N,N, N,N,Y,N),
      BitPat(uopFCLASS_D) -> List(FCMD_MV_XF,  X,X,Y,N,N, N,X,N,N,Y,N,N, N,N,Y,N),
      BitPat(uopFMV_S_X)  -> List(FCMD_MV_FX,  X,X,N,N,N, X,X,Y,Y,N,N,N, N,N,Y,N),
      BitPat(uopFMV_D_X)  -> List(FCMD_MV_FX,  X,X,N,N,N, X,X,N,Y,N,N,N, N,N,Y,N),
      BitPat(uopFMV_X_S)  -> List(FCMD_MV_XF,  X,X,Y,N,N, N,X,Y,N,Y,N,N, N,N,Y,N),
      BitPat(uopFMV_X_D)  -> List(FCMD_MV_XF,  X,X,Y,N,N, N,X,N,N,Y,N,N, N,N,Y,N),
      BitPat(uopFCVT_S_D) -> List(FCMD_CVT_FF, X,X,Y,N,N, N,X,Y,N,N,Y,N, N,N,Y,Y),
      BitPat(uopFCVT_D_S) -> List(FCMD_CVT_FF, X,X,Y,N,N, N,X,N,N,N,Y,N, N,N,Y,Y),

      BitPat(uopFCVT_S_W) -> List(FCMD_CVT_FI, X,X,N,N,N, X,X,Y,Y,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_S_WU)-> List(FCMD_CVT_FI, X,X,N,N,N, X,X,Y,Y,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_S_L) -> List(FCMD_CVT_FI, X,X,N,N,N, X,X,Y,Y,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_S_LU)-> List(FCMD_CVT_FI, X,X,N,N,N, X,X,Y,Y,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_D_W) -> List(FCMD_CVT_FI, X,X,N,N,N, X,X,N,Y,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_D_WU)-> List(FCMD_CVT_FI, X,X,N,N,N, X,X,N,Y,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_D_L) -> List(FCMD_CVT_FI, X,X,N,N,N, X,X,N,Y,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_D_LU)-> List(FCMD_CVT_FI, X,X,N,N,N, X,X,N,Y,N,N,N, N,N,Y,Y),

      BitPat(uopFCVT_W_S) -> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,Y,N,Y,N,N, N,N,Y,Y),
      BitPat(uopFCVT_WU_S)-> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,Y,N,Y,N,N, N,N,Y,Y),
      BitPat(uopFCVT_L_S) -> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,Y,N,Y,N,N, N,N,Y,Y),
      BitPat(uopFCVT_LU_S)-> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,Y,N,Y,N,N, N,N,Y,Y),
      BitPat(uopFCVT_W_D) -> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,N,Y,N,N, N,N,Y,Y),
      BitPat(uopFCVT_WU_D)-> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,N,Y,N,N, N,N,Y,Y),
      BitPat(uopFCVT_L_D) -> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,N,Y,N,N, N,N,Y,Y),
      BitPat(uopFCVT_LU_D)-> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,N,Y,N,N, N,N,Y,Y),

      BitPat(uopFEQ_S)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,Y,N,Y,N,N, N,N,N,Y),
      BitPat(uopFLT_S)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,Y,N,Y,N,N, N,N,N,Y),
      BitPat(uopFLE_S)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,Y,N,Y,N,N, N,N,N,Y),
      BitPat(uopFEQ_D)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,N,N,Y,N,N, N,N,N,Y),
      BitPat(uopFLT_D)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,N,N,Y,N,N, N,N,N,Y),
      BitPat(uopFLE_D)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,N,N,Y,N,N, N,N,N,Y),

      BitPat(uopFSGNJ_S)  -> List(FCMD_SGNJ,   X,X,Y,Y,N, N,N,Y,N,N,Y,N, N,N,N,N),
      BitPat(uopFSGNJ_D)  -> List(FCMD_SGNJ,   X,X,Y,Y,N, N,N,N,N,N,Y,N, N,N,N,N),

      BitPat(uopFMIN_S)   -> List(FCMD_MINMAX, X,X,Y,Y,N, N,N,Y,N,N,Y,N, N,N,N,Y),
      BitPat(uopFMAX_S)   -> List(FCMD_MINMAX, X,X,Y,Y,N, N,N,Y,N,N,Y,N, N,N,N,Y),
      BitPat(uopFMIN_D)   -> List(FCMD_MINMAX, X,X,Y,Y,N, N,N,N,N,N,Y,N, N,N,N,Y),
      BitPat(uopFMAX_D)   -> List(FCMD_MINMAX, X,X,Y,Y,N, N,N,N,N,N,Y,N, N,N,N,Y),

      BitPat(uopFADD_S)   -> List(FCMD_ADD,    X,X,Y,Y,N, N,Y,Y,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFSUB_S)   -> List(FCMD_SUB,    X,X,Y,Y,N, N,Y,Y,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFMUL_S)   -> List(FCMD_MUL,    X,X,Y,Y,N, N,N,Y,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFADD_D)   -> List(FCMD_ADD,    X,X,Y,Y,N, N,Y,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFSUB_D)   -> List(FCMD_SUB,    X,X,Y,Y,N, N,Y,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFMUL_D)   -> List(FCMD_MUL,    X,X,Y,Y,N, N,N,N,N,N,N,Y, N,N,Y,Y),

      BitPat(uopFMADD_S)  -> List(FCMD_MADD,   X,X,Y,Y,Y, N,N,Y,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFMSUB_S)  -> List(FCMD_MSUB,   X,X,Y,Y,Y, N,N,Y,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFNMADD_S) -> List(FCMD_NMADD,  X,X,Y,Y,Y, N,N,Y,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFNMSUB_S) -> List(FCMD_NMSUB,  X,X,Y,Y,Y, N,N,Y,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFMADD_D)  -> List(FCMD_MADD,   X,X,Y,Y,Y, N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFMSUB_D)  -> List(FCMD_MSUB,   X,X,Y,Y,Y, N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFNMADD_D) -> List(FCMD_NMADD,  X,X,Y,Y,Y, N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFNMSUB_D) -> List(FCMD_NMSUB,  X,X,Y,Y,Y, N,N,N,N,N,N,Y, N,N,Y,Y)

// currently unsupported (requires variable latency)
//      uopFDIV_S   -> List(FCMD_DIV,    X,X,Y,Y,N, N,N,Y,N,N,N,N, Y,N,Y,Y),
//      uopFDIV_D   -> List(FCMD_DIV,    X,X,Y,Y,N, N,N,N,N,N,N,N, Y,N,Y,Y),
//      uopFSQRT_S  -> List(FCMD_SQRT,   X,X,Y,N,N, Y,X,Y,N,N,N,N, N,Y,Y,Y),
//      uopFSQRT_D  -> List(FCMD_SQRT,   X,X,Y,N,N, Y,X,N,N,N,N,N, N,Y,Y,Y)
          )

   val decoder = rocket.DecodeLogic(io.uopc, default, table)

   val s = io.sigs
   val sigs = Seq(s.cmd, s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                  s.swap23, s.single, s.fromint, s.toint, s.fastpipe, s.fma,
                  s.div, s.sqrt, s.round, s.wflags)
   sigs zip decoder map {case(s,d) => s := d}
}


class FPU(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new Bundle
   {
      val req = new ValidIO(new Bundle
         {
            val uop      = new MicroOp()
            val rs1_data = Bits(width = 65)
            val rs2_data = Bits(width = 65)
            val rs3_data = Bits(width = 65)
            val fcsr_rm  = Bits(width = rocket.FPConstants.RM_SZ)
         }).flip
      val resp = new ValidIO(new ExeUnitResp(65))
   }

   // all FP units are padded out to the same latency for easy scheduling of
   // the write port
//   val test = params(DFMALatency) TODO BUG why is this returning "Nothing"?
   val fpu_latency = 3
   val io_req = io.req.bits

   val fp_decoder = Module(new UOPCodeFPUDecoder)
   fp_decoder.io.uopc:= io_req.uop.uopc
   val fp_ctrl = fp_decoder.io.sigs
   val fp_rm = Mux(ImmGenRm(io_req.uop.imm_packed) === Bits(7), io_req.fcsr_rm, ImmGenRm(io_req.uop.imm_packed))

   val req = Wire(new rocket.FPInput)
   req := fp_ctrl
   req.rm := fp_rm
   req.in1 := io_req.rs1_data
   req.in2 := io_req.rs2_data
   req.in3 := io_req.rs3_data
   when (fp_ctrl.swap23) { req.in3 := io_req.rs2_data }

   req.typ := ImmGenTyp(io_req.uop.imm_packed)


   val dfma = Module(new rocket.FPUFMAPipe(fpu_latency, 52, 12))
   dfma.io.in.valid := io.req.valid && fp_ctrl.fma && !fp_ctrl.single
   dfma.io.in.bits := req


   val sfma = Module(new rocket.FPUFMAPipe(fpu_latency, 23, 9))
   sfma.io.in.valid := io.req.valid && fp_ctrl.fma && fp_ctrl.single
   sfma.io.in.bits := req


   val ifpu = Module(new rocket.IntToFP(fpu_latency)) // 3 for rocket
   ifpu.io.in.valid := io.req.valid && fp_ctrl.fromint
   ifpu.io.in.bits := req
   assert (!(io.req.valid && fp_ctrl.fromint && req.in1(64).toBool),
            "IntToFP integer input has 65th high-order bit set!")


   val fpiu = Module(new rocket.FPToInt)
   fpiu.io.in.valid := io.req.valid && (fp_ctrl.toint || fp_ctrl.cmd === FCMD_MINMAX)
   fpiu.io.in.bits := req
   val fpiu_out = Pipe(Reg(next=fpiu.io.in.valid && !fp_ctrl.fastpipe),
                       fpiu.io.out.bits, fpu_latency-1)

   val fpiu_result  = Wire(new rocket.FPResult)
   fpiu_result.data := fpiu_out.bits.toint
   fpiu_result.exc  := fpiu_out.bits.exc


   val fpmu = Module(new rocket.FPToFP(fpu_latency)) // latency 2 for rocket
   fpmu.io.in.valid := io.req.valid && fp_ctrl.fastpipe
   fpmu.io.in.bits := req
   fpmu.io.lt := fpiu.io.out.bits.lt

   // Response (all FP units have been padded out to the same latency)
   io.resp.valid := ifpu.io.out.valid ||
                    fpiu_out.valid ||
                    fpmu.io.out.valid ||
                    sfma.io.out.valid ||
                    dfma.io.out.valid
   val fpu_out   = Mux(dfma.io.out.valid, dfma.io.out.bits,
                   Mux(sfma.io.out.valid, sfma.io.out.bits,
                   Mux(ifpu.io.out.valid, ifpu.io.out.bits,
                   Mux(fpiu_out.valid,    fpiu_result,
                                          fpmu.io.out.bits))))

   io.resp.bits.data              := fpu_out.data
   io.resp.bits.fflags.valid      := io.resp.valid
   io.resp.bits.fflags.bits.flags := fpu_out.exc

// TODO why is this assertion failing?
//   assert (PopCount(Vec(ifpu.io.out, fpiu_out, fpmu.io.out, sfma.io.out, dfma.io.out).map(_.valid)) <= UInt(1),
//      "Multiple FPU units are firing requests.")
}

}
