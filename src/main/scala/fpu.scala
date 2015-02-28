package BOOM
{

import Chisel._
import Node._

import rocket.FPConstants._
import rocket.FPUCtrlSigs

import rocket.SFMALatency
import rocket.DFMALatency


// TODO get rid of this decoder and move into the Decode stage? Or the RRd stage?
// most of these signals are already created, just need to be translated
// to the ROcket FPU-speak
// move rm from inst into the uop
class UOPCodeFPUDecoder extends Module
{
  val io = new Bundle {
    val uopc = Bits(INPUT, UOPC_SZ)
    val sigs = new FPUCtrlSigs().asOutput
  }

   val N = Bool(false)
   val Y = Bool(true)
   val X = Bool(false)
   val decoder = rocket.DecodeLogic(io.uopc,
      // TODO get rid of some of these signals
      // such as ldst, swap32, wen?
      // remove wen -> it refers to the FP register file
      //                                          swap32
      //                  cmd                     | single
      //                  |            ldst       | | fromint
      //                  |            | wen      | | | toint
      //                  |            | | ren1   | | | | fastpipe
      //                  |            | | | ren2 | | | | | fma
      //                  |            | | | | ren3 | | | | | round
      //                  |            | | | | |  | | | | | | |
      List               (FCMD_X,      X,X,X,X,X, X,X,X,X,X,X,X),
      Array(
       //FLW      -> List(FCMD_X,      Y,Y,N,N,N, X,Y,N,N,N,N,N),
       //FLD      -> List(FCMD_X,      Y,Y,N,N,N, X,N,N,N,N,N,N),
       //FSW      -> List(FCMD_MV_XF,  Y,N,N,Y,N, X,Y,N,Y,N,N,N),
       //FSD      -> List(FCMD_MV_XF,  Y,N,N,Y,N, X,N,N,Y,N,N,N),
      uopFCLASS_S -> List(FCMD_MV_XF,  N,N,Y,N,N, X,Y,N,Y,N,N,Y),
      uopFCLASS_D -> List(FCMD_MV_XF,  N,N,Y,N,N, X,N,N,Y,N,N,Y),
      uopFMV_S_X  -> List(FCMD_MV_FX,  N,Y,N,N,N, X,Y,Y,N,N,N,Y),
      uopFMV_D_X  -> List(FCMD_MV_FX,  N,Y,N,N,N, X,N,Y,N,N,N,Y),
      uopFMV_X_S  -> List(FCMD_MV_XF,  N,N,Y,N,N, X,Y,N,Y,N,N,Y),
      uopFMV_X_D  -> List(FCMD_MV_XF,  N,N,Y,N,N, X,N,N,Y,N,N,Y),

      uopFCVT_S_D -> List(FCMD_CVT_FF, N,Y,Y,N,N, X,Y,N,N,Y,N,Y),
      uopFCVT_D_S -> List(FCMD_CVT_FF, N,Y,Y,N,N, X,N,N,N,Y,N,Y),

      uopFCVT_S_W -> List(FCMD_CVT_FI, N,Y,N,N,N, X,Y,Y,N,N,N,Y),
      uopFCVT_S_WU-> List(FCMD_CVT_FI, N,Y,N,N,N, X,Y,Y,N,N,N,Y),
      uopFCVT_S_L -> List(FCMD_CVT_FI, N,Y,N,N,N, X,Y,Y,N,N,N,Y),
      uopFCVT_S_LU-> List(FCMD_CVT_FI, N,Y,N,N,N, X,Y,Y,N,N,N,Y),
      uopFCVT_D_W -> List(FCMD_CVT_FI, N,Y,N,N,N, X,N,Y,N,N,N,Y),
      uopFCVT_D_WU-> List(FCMD_CVT_FI, N,Y,N,N,N, X,N,Y,N,N,N,Y),
      uopFCVT_D_L -> List(FCMD_CVT_FI, N,Y,N,N,N, X,N,Y,N,N,N,Y),
      uopFCVT_D_LU-> List(FCMD_CVT_FI, N,Y,N,N,N, X,N,Y,N,N,N,Y),
      uopFCVT_W_S -> List(FCMD_CVT_IF, N,N,Y,N,N, X,Y,N,Y,N,N,Y),
      uopFCVT_WU_S-> List(FCMD_CVT_IF, N,N,Y,N,N, X,Y,N,Y,N,N,Y),
      uopFCVT_L_S -> List(FCMD_CVT_IF, N,N,Y,N,N, X,Y,N,Y,N,N,Y),
      uopFCVT_LU_S-> List(FCMD_CVT_IF, N,N,Y,N,N, X,Y,N,Y,N,N,Y),
      uopFCVT_W_D -> List(FCMD_CVT_IF, N,N,Y,N,N, X,N,N,Y,N,N,Y),
      uopFCVT_WU_D-> List(FCMD_CVT_IF, N,N,Y,N,N, X,N,N,Y,N,N,Y),
      uopFCVT_L_D -> List(FCMD_CVT_IF, N,N,Y,N,N, X,N,N,Y,N,N,Y),
      uopFCVT_LU_D-> List(FCMD_CVT_IF, N,N,Y,N,N, X,N,N,Y,N,N,Y),

      uopFEQ_S    -> List(FCMD_CMP,    N,N,Y,Y,N, N,Y,N,Y,N,N,N),
      uopFLT_S    -> List(FCMD_CMP,    N,N,Y,Y,N, N,Y,N,Y,N,N,N),
      uopFLE_S    -> List(FCMD_CMP,    N,N,Y,Y,N, N,Y,N,Y,N,N,N),
      uopFEQ_D    -> List(FCMD_CMP,    N,N,Y,Y,N, N,N,N,Y,N,N,N),
      uopFLT_D    -> List(FCMD_CMP,    N,N,Y,Y,N, N,N,N,Y,N,N,N),
      uopFLE_D    -> List(FCMD_CMP,    N,N,Y,Y,N, N,N,N,Y,N,N,N),

      uopFSGNJ_S  -> List(FCMD_SGNJ,   N,Y,Y,Y,N, N,Y,N,N,Y,N,N),
      uopFSGNJ_D  -> List(FCMD_SGNJ,   N,Y,Y,Y,N, N,N,N,N,Y,N,N),

      uopFMIN_S   -> List(FCMD_MINMAX, N,Y,Y,Y,N, N,Y,N,N,Y,N,N),
      uopFMAX_S   -> List(FCMD_MINMAX, N,Y,Y,Y,N, N,Y,N,N,Y,N,N),
      uopFMIN_D   -> List(FCMD_MINMAX, N,Y,Y,Y,N, N,N,N,N,Y,N,N),
      uopFMAX_D   -> List(FCMD_MINMAX, N,Y,Y,Y,N, N,N,N,N,Y,N,N),

      uopFADD_S   -> List(FCMD_ADD,    N,Y,Y,Y,N, Y,Y,N,N,N,Y,Y),
      uopFSUB_S   -> List(FCMD_SUB,    N,Y,Y,Y,N, Y,Y,N,N,N,Y,Y),
      uopFMUL_S   -> List(FCMD_MUL,    N,Y,Y,Y,N, N,Y,N,N,N,Y,Y),
      uopFADD_D   -> List(FCMD_ADD,    N,Y,Y,Y,N, Y,N,N,N,N,Y,Y),
      uopFSUB_D   -> List(FCMD_SUB,    N,Y,Y,Y,N, Y,N,N,N,N,Y,Y),
      uopFMUL_D   -> List(FCMD_MUL,    N,Y,Y,Y,N, N,N,N,N,N,Y,Y),

      uopFMADD_S  -> List(FCMD_MADD,   N,Y,Y,Y,Y, N,Y,N,N,N,Y,Y),
      uopFMSUB_S  -> List(FCMD_MSUB,   N,Y,Y,Y,Y, N,Y,N,N,N,Y,Y),
      uopFNMADD_S -> List(FCMD_NMADD,  N,Y,Y,Y,Y, N,Y,N,N,N,Y,Y),
      uopFNMSUB_S -> List(FCMD_NMSUB,  N,Y,Y,Y,Y, N,Y,N,N,N,Y,Y),
      uopFMADD_D  -> List(FCMD_MADD,   N,Y,Y,Y,Y, N,N,N,N,N,Y,Y),
      uopFMSUB_D  -> List(FCMD_MSUB,   N,Y,Y,Y,Y, N,N,N,N,N,Y,Y),
      uopFNMADD_D -> List(FCMD_NMADD,  N,Y,Y,Y,Y, N,N,N,N,N,Y,Y),
      uopFNMSUB_D -> List(FCMD_NMSUB,  N,Y,Y,Y,Y, N,N,N,N,N,Y,Y)
          ))
  val s = io.sigs
  Vec(s.cmd, s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap23, s.single, s.fromint,
      s.toint, s.fastpipe, s.fma, s.round) := decoder
}


class FPU extends Module with BOOMCoreParameters
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

   val fp_rm = Mux(io_req.uop.inst(14,12) === Bits(7), io_req.fcsr_rm, io_req.uop.inst(14,12)) // TODO FIXME XXX put information elsewhere in uop, this is the rm (founding mode)

   val req = new rocket.FPInput
   req := fp_ctrl
   req.rm := fp_rm
   req.in1 := io_req.rs1_data
   req.in2 := io_req.rs2_data
   req.in3 := io_req.rs3_data
   when (fp_ctrl.swap23) { req.in3 := io_req.rs2_data } // TODO move this elsewhere? this feels expensive

   req.typ := io_req.uop.inst(21,20) // TODO FIXME XXX put typ elsewhere in uop


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
   
   val fpiu_result  = new rocket.FPResult
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
   io.resp.bits := Mux(dfma.io.out.valid, dfma.io.out.bits,
                   Mux(sfma.io.out.valid, sfma.io.out.bits,
                   Mux(ifpu.io.out.valid, ifpu.io.out.bits,
                   Mux(fpiu_out.valid,    fpiu_result,
                                          fpmu.io.out.bits))))
}

}
