//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom.exu
import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.FPConstants
import freechips.rocketchip.tile.FPUCtrlSigs
import freechips.rocketchip.tile
import freechips.rocketchip.rocket
import freechips.rocketchip.util.uintToBitPat
import boom.common._
import boom.util.{ImmGenRm, ImmGenTyp}



class UOPCodeVFPUDecoder extends Module
{
  val io = IO(new Bundle {
    val uopc = Bits(INPUT, UOPC_SZ)
    val sigs = new FPUCtrlSigs().asOutput
  })
   val N = Bool(false)
   val Y = Bool(true)
   val X = Bool(false)

   val default: List[BitPat] = List(X,X,X,X,X, X,X,X,X,X,X,X, X,X,X,X)

   // TODO_vec: Add half_precision table

   val f_table: Array[(BitPat, List[BitPat])] =
      // Note: not all of these signals are used or necessary, but we're
      // constrained by the need to fit the rocket.FPU units' ctrl signals.
      //                                     swap12         fma
      //                                     | swap32       | div
      //                                     | | singleIn   | | sqrt
      //                          ldst       | | | singleOut| | | wflags
      //                          | wen      | | | | fromint| | | |
      //                          | | ren1   | | | | | toint| | | |
      //                          | | | ren2 | | | | | | fastpipe |
      //                          | | | | ren3 | | | | | |  | | | |
      //                          | | | | |  | | | | | | |  | | | |
      Array(
      BitPat(uopVADD)     -> List(X,X,Y,Y,N, N,Y,Y,Y,N,N,N, Y,N,N,Y)
      ) // TODO_vec: Add all other uops, make these uops specify precision

   val d_table: Array[(BitPat, List[BitPat])] =
      // Note: not all of these signals are used or necessary, but we're
      // constrained by the need to fit the rocket.FPU units' ctrl signals.
      //                                     swap12         fma
      //                                     | swap32       | div
      //                                     | | singleIn   | | sqrt
      //                          ldst       | | | singleOut| | | wflags
      //                          | wen      | | | | fromint| | | |
      //                          | | ren1   | | | | | toint| | | |
      //                          | | | ren2 | | | | | | fastpipe |
      //                          | | | | ren3 | | | | | |  | | | |
      //                          | | | | |  | | | | | | |  | | | |

      Array(
      )

	val insns = f_table ++ d_table
   val decoder = rocket.DecodeLogic(io.uopc, default, insns)

   val s = io.sigs
   val sigs = Seq(s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                  s.swap23, s.singleIn, s.singleOut, s.fromint, s.toint, s.fastpipe, s.fma,
                  s.div, s.sqrt, s.wflags)
   sigs zip decoder map {case(s,d) => s := d}
}

class VFpuReq()(implicit p: Parameters) extends BoomBundle()(p)
{ // TODO_Vec: Figure out width. 128? 130? 
   val uop      = new MicroOp()
   val rs1_data = Bits(width = 128)
   val rs2_data = Bits(width = 128)
   val rs3_data = Bits(width = 128)
   val fcsr_rm  = Bits(width = tile.FPConstants.RM_SZ) // TODO_Vec: Is rounding mode still controlled by fp csrs?
}
class VFPU(implicit p: Parameters) extends BoomModule()(p) with tile.HasFPUParameters
{
   val io = IO(new Bundle
      {
         val req = new ValidIO(new VFpuReq).flip
         val resp = new ValidIO(new ExeUnitResp(128))
      })
   val vfpu_latency = dfmaLatency
   val io_req = io.req.bits

   val vfp_decoder = Module(new UOPCodeVFPUDecoder)
   vfp_decoder.io.uopc := io_req.uop.uopc
   val vfp_ctrl = vfp_decoder.io.sigs
   val vfp_rm = Mux(ImmGenRm(io_req.uop.imm_packed) === Bits(7), io_req.fcsr_rm, ImmGenRm(io_req.uop.imm_packed))
   // TODO: Make this unpack all elements, add argument specifying which element
   def vfuInput(minT: Option[tile.FType]): tile.FPInput = {
      val req = Wire(new tile.FPInput)
      val tag = !vfp_ctrl.singleIn
      req := vfp_ctrl
      req.rm := vfp_rm
      // TODO_vec: Ugh why is hardfloat weird
      req.in1 := unbox(io_req.rs1_data(64, 0), tag, minT)
      req.in2 := unbox(io_req.rs2_data(64, 0), tag, minT)
      req.in3 := unbox(io_req.rs3_data(64, 0), tag, minT)
      when (vfp_ctrl.swap23) {req.in3 := req.in2 }
      req.typ := ImmGenTyp(io_req.uop.imm_packed)
      val fma_decoder = Module(new FMADecoder)
      fma_decoder.io.uopc := io_req.uop.uopc
      req.fmaCmd := fma_decoder.io.cmd
      req
   }

   // TODO_Vec: Add more of these to do the packed computation
   // TODO_Vec: Add half prec
   // TODO_Vec: Use Hwacha's 128-wide unit instead here
   val dfma = Module(new tile.FPUFMAPipe(latency=vfpu_latency, t=tile.FType.D))
   dfma.io.in.valid := io.req.valid && vfp_ctrl.fma && !vfp_ctrl.singleOut
   dfma.io.in.bits := vfuInput(Some(dfma.t))

   val sfma = Module(new tile.FPUFMAPipe(latency=vfpu_latency, t=tile.FType.S))
   sfma.io.in.valid := io.req.valid && vfp_ctrl.fma && vfp_ctrl.singleOut
   sfma.io.in.bits := vfuInput(Some(sfma.t))

   // TODO_Vec: Fptoint?

   // TODO_Vec: fpmu?

   // Response (all vec units have same latency)
   // TODO_Vec: Do we actually want this?

   io.resp.valid := sfma.io.out.valid || dfma.io.out.valid

   val fpu_out_data = Mux(dfma.io.out.valid, box(dfma.io.out.bits.data, true.B),
                          box(sfma.io.out.bits.data, false.B))
   val fpu_out_exc = Mux(dfma.io.out.valid, dfma.io.out.bits.exc, sfma.io.out.bits.exc)
   io.resp.bits.data := fpu_out_data
   io.resp.bits.fflags.valid := io.resp.valid
   io.resp.bits.fflags.bits.flags := fpu_out_exc
}
