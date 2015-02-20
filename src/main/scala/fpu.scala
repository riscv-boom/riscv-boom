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
      // such as ldst, swap32, fastpipe?, wen?
      // remove wen -> it refers to the FP register file
      //                                         swap32
      //                  cmd                    | single
      //                  |            ldst      | | fromint
      //                  |            | wen     | | | toint
      //                  |            | | ren1  | | | | fastpipe
      //                  |            | | | ren2| | | | | fma
      //                  |            | | | | ren3| | | | | round
      //                  |            | | | | | | | | | | | |
      List               (FCMD_X,      X,X,X,X,X,X,X,X,X,X,X,X),
      Array(
       //FLW      -> List(FCMD_X,      Y,Y,N,N,N,X,Y,N,N,N,N,N),
       //FLD      -> List(FCMD_X,      Y,Y,N,N,N,X,N,N,N,N,N,N),
       //FSW      -> List(FCMD_MV_XF,  Y,N,N,Y,N,X,Y,N,Y,N,N,N),
       //FSD      -> List(FCMD_MV_XF,  Y,N,N,Y,N,X,N,N,Y,N,N,N),
      uopFMV_S_X  -> List(FCMD_MV_FX,  N,Y,N,N,N,X,Y,Y,N,N,N,Y),
      uopFMV_D_X  -> List(FCMD_MV_FX,  N,Y,N,N,N,X,N,Y,N,N,N,Y),
       //FCVT_S_W -> List(FCMD_CVT_FI, N,Y,N,N,N,X,Y,Y,N,N,N,Y),
       //FCVT_S_WU-> List(FCMD_CVT_FI, N,Y,N,N,N,X,Y,Y,N,N,N,Y),
       //FCVT_S_L -> List(FCMD_CVT_FI, N,Y,N,N,N,X,Y,Y,N,N,N,Y),
       //FCVT_S_LU-> List(FCMD_CVT_FI, N,Y,N,N,N,X,Y,Y,N,N,N,Y),
       //FCVT_D_W -> List(FCMD_CVT_FI, N,Y,N,N,N,X,N,Y,N,N,N,Y),
       //FCVT_D_WU-> List(FCMD_CVT_FI, N,Y,N,N,N,X,N,Y,N,N,N,Y),
       //FCVT_D_L -> List(FCMD_CVT_FI, N,Y,N,N,N,X,N,Y,N,N,N,Y),
       //FCVT_D_LU-> List(FCMD_CVT_FI, N,Y,N,N,N,X,N,Y,N,N,N,Y),
      uopFMV_X_S  -> List(FCMD_MV_XF,  N,N,Y,N,N,X,Y,N,Y,N,N,Y),
      uopFMV_X_D  -> List(FCMD_MV_XF,  N,N,Y,N,N,X,N,N,Y,N,N,Y),
       //FCLASS_S -> List(FCMD_MV_XF,  N,N,Y,N,N,X,Y,N,Y,N,N,Y),
       //FCLASS_D -> List(FCMD_MV_XF,  N,N,Y,N,N,X,N,N,Y,N,N,Y),
       //FCVT_W_S -> List(FCMD_CVT_IF, N,N,Y,N,N,X,Y,N,Y,N,N,Y),
       //FCVT_WU_S-> List(FCMD_CVT_IF, N,N,Y,N,N,X,Y,N,Y,N,N,Y),
       //FCVT_L_S -> List(FCMD_CVT_IF, N,N,Y,N,N,X,Y,N,Y,N,N,Y),
       //FCVT_LU_S-> List(FCMD_CVT_IF, N,N,Y,N,N,X,Y,N,Y,N,N,Y),
       //FCVT_W_D -> List(FCMD_CVT_IF, N,N,Y,N,N,X,N,N,Y,N,N,Y),
       //FCVT_WU_D-> List(FCMD_CVT_IF, N,N,Y,N,N,X,N,N,Y,N,N,Y),
       //FCVT_L_D -> List(FCMD_CVT_IF, N,N,Y,N,N,X,N,N,Y,N,N,Y),
       //FCVT_LU_D-> List(FCMD_CVT_IF, N,N,Y,N,N,X,N,N,Y,N,N,Y),
       //FCVT_S_D -> List(FCMD_CVT_FF, N,Y,Y,N,N,X,Y,N,N,Y,N,Y),
       //FCVT_D_S -> List(FCMD_CVT_FF, N,Y,Y,N,N,X,N,N,N,Y,N,Y),
       //FEQ_S    -> List(FCMD_CMP,    N,N,Y,Y,N,N,Y,N,Y,N,N,N),
       //FLT_S    -> List(FCMD_CMP,    N,N,Y,Y,N,N,Y,N,Y,N,N,N),
       //FLE_S    -> List(FCMD_CMP,    N,N,Y,Y,N,N,Y,N,Y,N,N,N),
       //FEQ_D    -> List(FCMD_CMP,    N,N,Y,Y,N,N,N,N,Y,N,N,N),
       //FLT_D    -> List(FCMD_CMP,    N,N,Y,Y,N,N,N,N,Y,N,N,N),
       //FLE_D    -> List(FCMD_CMP,    N,N,Y,Y,N,N,N,N,Y,N,N,N),
       uopFSGNJ_S -> List(FCMD_SGNJ,   N,Y,Y,Y,N,N,Y,N,N,Y,N,N),
//       uopFSGNJN_S-> List(FCMD_SGNJ,   N,Y,Y,Y,N,N,Y,N,N,Y,N,N),
//       uopFSGNJX_S-> List(FCMD_SGNJ,   N,Y,Y,Y,N,N,Y,N,N,Y,N,N),

       uopFSGNJ_D -> List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,N,N,Y,N,N)
//       uopFSGNJN_D-> List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,N,N,Y,N,N),
//       uopFSGNJX_D-> List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,N,N,Y,N,N)

       //FMIN_S   -> List(FCMD_MINMAX, N,Y,Y,Y,N,N,Y,N,N,Y,N,N),
       //FMAX_S   -> List(FCMD_MINMAX, N,Y,Y,Y,N,N,Y,N,N,Y,N,N),
       //FMIN_D   -> List(FCMD_MINMAX, N,Y,Y,Y,N,N,N,N,N,Y,N,N),
       //FMAX_D   -> List(FCMD_MINMAX, N,Y,Y,Y,N,N,N,N,N,Y,N,N),
       //FADD_S   -> List(FCMD_ADD,    N,Y,Y,Y,N,Y,Y,N,N,N,Y,Y),
       //FSUB_S   -> List(FCMD_SUB,    N,Y,Y,Y,N,Y,Y,N,N,N,Y,Y),
       //FMUL_S   -> List(FCMD_MUL,    N,Y,Y,Y,N,N,Y,N,N,N,Y,Y),
       //FADD_D   -> List(FCMD_ADD,    N,Y,Y,Y,N,Y,N,N,N,N,Y,Y),
       //FSUB_D   -> List(FCMD_SUB,    N,Y,Y,Y,N,Y,N,N,N,N,Y,Y),
       //FMUL_D   -> List(FCMD_MUL,    N,Y,Y,Y,N,N,N,N,N,N,Y,Y),
       //FMADD_S  -> List(FCMD_MADD,   N,Y,Y,Y,Y,N,Y,N,N,N,Y,Y),
       //FMSUB_S  -> List(FCMD_MSUB,   N,Y,Y,Y,Y,N,Y,N,N,N,Y,Y),
       //FNMADD_S -> List(FCMD_NMADD,  N,Y,Y,Y,Y,N,Y,N,N,N,Y,Y),
       //FNMSUB_S -> List(FCMD_NMSUB,  N,Y,Y,Y,Y,N,Y,N,N,N,Y,Y),
       //FMADD_D  -> List(FCMD_MADD,   N,Y,Y,Y,Y,N,N,N,N,N,Y,Y),
       //FMSUB_D  -> List(FCMD_MSUB,   N,Y,Y,Y,Y,N,N,N,N,N,Y,Y),
       //FNMADD_D -> List(FCMD_NMADD,  N,Y,Y,Y,Y,N,N,N,N,N,Y,Y),
       //FNMSUB_D -> List(FCMD_NMSUB,  N,Y,Y,Y,Y,N,N,N,N,N,Y,Y)
          ))
  val s = io.sigs
  Vec(s.cmd, s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap23, s.single, s.fromint,
      s.toint, s.fastpipe, s.fma, s.round) := decoder
}


//TODO
// fromint_data -> which register does that come in form?
// fcsr_flags -> how to get that data to the ROB
//                and then get the ROB to write it at commit time
// stages -> figure out how many stages "3" latency corresponds to, get the registers in the correct place and the FPUUnit to understand it, and then pad out the ALU to the correct length

class FPU extends Module with BOOMCoreParameters
{
   val io = new Bundle
   {
//      val dpath = (new DpathFPUIO).flip
      val valid   = Bool(INPUT)
      val in1     = Bits(INPUT,  width = 65)
      val in2     = Bits(INPUT,  width = 65)
      val in3     = Bits(INPUT,  width = 65)
      val uop     = new MicroOp().asInput
      val fcsr_rm = Bits(INPUT, rocket.FPConstants.RM_SZ)

      val resp = new ValidIO(new ExeUnitResp(65))
      val fcsr_flags = Valid(Bits(width = rocket.FPConstants.FLAGS_SZ))
   }

   // all FP units are padded out to the same latency for easy scheduling of
   // the write port
   val fpu_latency = 3

   val fp_decoder = Module(new UOPCodeFPUDecoder)
   fp_decoder.io.uopc:= io.uop.uopc

   val fp_ctrl = fp_decoder.io.sigs

   val fp_rm = Mux(io.uop.inst(14,12) === Bits(7), io.fcsr_rm, io.uop.inst(14,12)) // TODO FIXME XXX put information elsewhere in uop, this is the rm (founding mode)

   val req = new rocket.FPInput
   req := fp_ctrl
   req.rm := fp_rm
   req.in1 := io.in1
   req.in2 := io.in2
   req.in3 := io.in3
   req.typ := io.uop.inst(21,20) // TODO FIXME XXX put type elsewhere in uop


   val ifpu = Module(new rocket.IntToFP(fpu_latency)) // 3 for rocket
   ifpu.io.in.valid := io.valid && fp_ctrl.fromint
   ifpu.io.in.bits := req
   //ifpu.io.in.bits.in1 := io.dpath.fromint_data
   assert (!(io.valid && fp_ctrl.fromint && req.in1(64).toBool),
            "IntToFP integer input has 65th high-order bit set!")


   val fpiu = Module(new rocket.FPToInt)
   fpiu.io.in.valid := io.valid && (fp_ctrl.toint || fp_ctrl.cmd === FCMD_MINMAX)
   fpiu.io.in.bits := req
   val fpiu_out = Pipe(Reg(next=fpiu.io.in.valid), fpiu.io.out.bits, fpu_latency-1)
   val fpiu_result  = new rocket.FPResult
   fpiu_result.data := fpiu_out.bits.toint
   fpiu_result.exc  := fpiu_out.bits.exc

   val fpmu = Module(new rocket.FPToFP(fpu_latency)) // latency 2 for rocket
   fpmu.io.in.valid := io.valid && fp_ctrl.fastpipe
   fpmu.io.in.bits := req
   fpmu.io.lt := fpiu.io.out.bits.lt



   // Response (all FP units have been padded out to the same latency)
   io.resp.valid := ifpu.io.out.valid ||
                    fpiu_out.valid ||
                    fpmu.io.out.valid
   io.resp.bits := Mux(ifpu.io.out.valid, ifpu.io.out.bits,
                   Mux(fpiu_out.valid,    fpiu_result,
                                          fpmu.io.out.bits))
}

}
