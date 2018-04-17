//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom.exu

import Chisel._
import freechips.rocketchip.config.Parameters

import freechips.rocketchip.tile.FPConstants._
import freechips.rocketchip.tile.FPUCtrlSigs
import freechips.rocketchip.tile
import freechips.rocketchip.rocket
import freechips.rocketchip.util.uintToBitPat
import boom.common._
import boom.util.{ImmGenRm, ImmGenTyp}


// TODO get rid of this decoder and move into the Decode stage? Or the RRd stage?
// most of these signals are already created, just need to be translated
// to the Rocket FPU-speak
class UOPCodeFPUDecoder extends Module
{
  val io = IO(new Bundle {
    val uopc = Bits(INPUT, UOPC_SZ)
    val sigs = new FPUCtrlSigs().asOutput
  })

   // TODO change N,Y,X to BitPat("b1"), BitPat("b0"), and BitPat("b?")
   val N = Bool(false)
   val Y = Bool(true)
   val X = Bool(false)

   val default: List[BitPat] = List(X,X,X,X,X, X,X,X,X,X,X,X, X,X,X,X)

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
      BitPat(uopFCLASS_S) -> List(X,X,Y,N,N, N,X,Y,Y,N,Y,N, N,N,N,N),
      BitPat(uopFMV_S_X)  -> List(X,X,N,N,N, X,X,Y,N,Y,N,N, N,N,N,N),
      BitPat(uopFMV_X_S)  -> List(X,X,Y,N,N, N,X,N,Y,N,Y,N, N,N,N,N),

      BitPat(uopFCVT_S_W) -> List(X,X,N,N,N, X,X,Y,Y,Y,N,N, N,N,N,Y),
      BitPat(uopFCVT_S_WU)-> List(X,X,N,N,N, X,X,Y,Y,Y,N,N, N,N,N,Y),
      BitPat(uopFCVT_S_L) -> List(X,X,N,N,N, X,X,Y,Y,Y,N,N, N,N,N,Y),
      BitPat(uopFCVT_S_LU)-> List(X,X,N,N,N, X,X,Y,Y,Y,N,N, N,N,N,Y),

      BitPat(uopFCVT_W_S) -> List(X,X,Y,N,N, N,X,Y,Y,N,Y,N, N,N,N,Y),
      BitPat(uopFCVT_WU_S)-> List(X,X,Y,N,N, N,X,Y,Y,N,Y,N, N,N,N,Y),
      BitPat(uopFCVT_L_S) -> List(X,X,Y,N,N, N,X,Y,Y,N,Y,N, N,N,N,Y),
      BitPat(uopFCVT_LU_S)-> List(X,X,Y,N,N, N,X,Y,Y,N,Y,N, N,N,N,Y),

      BitPat(uopFEQ_S)    -> List(X,X,Y,Y,N, N,N,Y,Y,N,Y,N, N,N,N,Y),
      BitPat(uopFLT_S)    -> List(X,X,Y,Y,N, N,N,Y,Y,N,Y,N, N,N,N,Y),
      BitPat(uopFLE_S)    -> List(X,X,Y,Y,N, N,N,Y,Y,N,Y,N, N,N,N,Y),

      BitPat(uopFSGNJ_S)  -> List(X,X,Y,Y,N, N,N,Y,Y,N,N,Y, N,N,N,N),

      BitPat(uopFMIN_S)   -> List(X,X,Y,Y,N, N,N,Y,Y,N,N,Y, N,N,N,Y),
      BitPat(uopFMAX_S)   -> List(X,X,Y,Y,N, N,N,Y,Y,N,N,Y, N,N,N,Y),

      BitPat(uopFADD_S)   -> List(X,X,Y,Y,N, N,Y,Y,Y,N,N,N, Y,N,N,Y),
      BitPat(uopFSUB_S)   -> List(X,X,Y,Y,N, N,Y,Y,Y,N,N,N, Y,N,N,Y),
      BitPat(uopFMUL_S)   -> List(X,X,Y,Y,N, N,N,Y,Y,N,N,N, Y,N,N,Y),
      BitPat(uopFMADD_S)  -> List(X,X,Y,Y,Y, N,N,Y,Y,N,N,N, Y,N,N,Y),
      BitPat(uopFMSUB_S)  -> List(X,X,Y,Y,Y, N,N,Y,Y,N,N,N, Y,N,N,Y),
      BitPat(uopFNMADD_S) -> List(X,X,Y,Y,Y, N,N,Y,Y,N,N,N, Y,N,N,Y),
      BitPat(uopFNMSUB_S) -> List(X,X,Y,Y,Y, N,N,Y,Y,N,N,N, Y,N,N,Y)
      )

   val d_table: Array[(BitPat, List[BitPat])] =
      Array(
      BitPat(uopFCLASS_D) -> List(X,X,Y,N,N, N,X,N,N,N,Y,N, N,N,N,N),
      BitPat(uopFMV_D_X)  -> List(X,X,N,N,N, X,X,N,N,Y,N,N, N,N,N,N),
      BitPat(uopFMV_X_D)  -> List(X,X,Y,N,N, N,X,N,N,N,Y,N, N,N,N,N),
      BitPat(uopFCVT_S_D) -> List(X,X,Y,N,N, N,X,N,Y,N,N,Y, N,N,N,Y),
      BitPat(uopFCVT_D_S) -> List(X,X,Y,N,N, N,X,Y,N,N,N,Y, N,N,N,Y),

      BitPat(uopFCVT_D_W) -> List(X,X,N,N,N, X,X,N,N,Y,N,N, N,N,N,Y),
      BitPat(uopFCVT_D_WU)-> List(X,X,N,N,N, X,X,N,N,Y,N,N, N,N,N,Y),
      BitPat(uopFCVT_D_L) -> List(X,X,N,N,N, X,X,N,N,Y,N,N, N,N,N,Y),
      BitPat(uopFCVT_D_LU)-> List(X,X,N,N,N, X,X,N,N,Y,N,N, N,N,N,Y),

      BitPat(uopFCVT_W_D) -> List(X,X,Y,N,N, N,X,N,N,N,Y,N, N,N,N,Y),
      BitPat(uopFCVT_WU_D)-> List(X,X,Y,N,N, N,X,N,N,N,Y,N, N,N,N,Y),
      BitPat(uopFCVT_L_D) -> List(X,X,Y,N,N, N,X,N,N,N,Y,N, N,N,N,Y),
      BitPat(uopFCVT_LU_D)-> List(X,X,Y,N,N, N,X,N,N,N,Y,N, N,N,N,Y),

      BitPat(uopFEQ_D)    -> List(X,X,Y,Y,N, N,N,N,N,N,Y,N, N,N,N,Y),
      BitPat(uopFLT_D)    -> List(X,X,Y,Y,N, N,N,N,N,N,Y,N, N,N,N,Y),
      BitPat(uopFLE_D)    -> List(X,X,Y,Y,N, N,N,N,N,N,Y,N, N,N,N,Y),

      BitPat(uopFSGNJ_D)  -> List(X,X,Y,Y,N, N,N,N,N,N,N,Y, N,N,N,N),

      BitPat(uopFMIN_D)   -> List(X,X,Y,Y,N, N,N,N,N,N,N,Y, N,N,N,Y),
      BitPat(uopFMAX_D)   -> List(X,X,Y,Y,N, N,N,N,N,N,N,Y, N,N,N,Y),

      BitPat(uopFADD_D)   -> List(X,X,Y,Y,N, N,Y,N,N,N,N,N, Y,N,N,Y),
      BitPat(uopFSUB_D)   -> List(X,X,Y,Y,N, N,Y,N,N,N,N,N, Y,N,N,Y),
      BitPat(uopFMUL_D)   -> List(X,X,Y,Y,N, N,N,N,N,N,N,N, Y,N,N,Y),

      BitPat(uopFMADD_D)  -> List(X,X,Y,Y,Y, N,N,N,N,N,N,N, Y,N,N,Y),
      BitPat(uopFMSUB_D)  -> List(X,X,Y,Y,Y, N,N,N,N,N,N,N, Y,N,N,Y),
      BitPat(uopFNMADD_D) -> List(X,X,Y,Y,Y, N,N,N,N,N,N,N, Y,N,N,Y),
      BitPat(uopFNMSUB_D) -> List(X,X,Y,Y,Y, N,N,N,N,N,N,N, Y,N,N,Y)
      )

//   val insns = fLen match {
//      case 32 => f_table
//      case 64 => f_table ++ d_table
//   }
	val insns = f_table ++ d_table
   val decoder = rocket.DecodeLogic(io.uopc, default, insns)

   val s = io.sigs
   val sigs = Seq(s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                  s.swap23, s.singleIn, s.singleOut, s.fromint, s.toint, s.fastpipe, s.fma,
                  s.div, s.sqrt, s.wflags)
   sigs zip decoder map {case(s,d) => s := d}
}

class FMADecoder extends Module
{
   val io = IO(new Bundle
   {
      val uopc = UInt(INPUT, UOPC_SZ)
      val cmd = UInt(OUTPUT, 2)
   })

   val default: List[BitPat] = List(BitPat("b??"))
   val table: Array[(BitPat, List[BitPat])] =
   Array(
      BitPat(uopFADD_S)   -> List(BitPat("b00")),
      BitPat(uopFSUB_S)   -> List(BitPat("b01")),
      BitPat(uopFMUL_S)   -> List(BitPat("b00")),
      BitPat(uopFMADD_S)  -> List(BitPat("b00")),
      BitPat(uopFMSUB_S)  -> List(BitPat("b01")),
      BitPat(uopFNMADD_S) -> List(BitPat("b11")),
      BitPat(uopFNMSUB_S) -> List(BitPat("b10")),
      BitPat(uopFADD_D)   -> List(BitPat("b00")),
      BitPat(uopFSUB_D)   -> List(BitPat("b01")),
      BitPat(uopFMUL_D)   -> List(BitPat("b00")),
      BitPat(uopFMADD_D)  -> List(BitPat("b00")),
      BitPat(uopFMSUB_D)  -> List(BitPat("b01")),
      BitPat(uopFNMADD_D) -> List(BitPat("b11")),
      BitPat(uopFNMSUB_D) -> List(BitPat("b10"))
      )

   val decoder = rocket.DecodeLogic(io.uopc, default, table)

   val (cmd: UInt) :: Nil = decoder
   io.cmd := cmd
}

class FpuReq()(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop      = new MicroOp()
   val rs1_data = Bits(width = 65)
   val rs2_data = Bits(width = 65)
   val rs3_data = Bits(width = 65)
   val fcsr_rm  = Bits(width = tile.FPConstants.RM_SZ)
}

class FPU(implicit p: Parameters) extends BoomModule()(p) with tile.HasFPUParameters
{
   val io = IO(new Bundle
   {
      val req = new ValidIO(new FpuReq).flip
      val resp = new ValidIO(new ExeUnitResp(65))
   })

   // all FP units are padded out to the same latency for easy scheduling of the write port
   val fpu_latency = dfmaLatency
   val io_req = io.req.bits

   val fp_decoder = Module(new UOPCodeFPUDecoder)
   fp_decoder.io.uopc := io_req.uop.uopc
   val fp_ctrl = fp_decoder.io.sigs
   val fp_rm = Mux(ImmGenRm(io_req.uop.imm_packed) === Bits(7), io_req.fcsr_rm, ImmGenRm(io_req.uop.imm_packed))

	def fuInput(minT: Option[tile.FType]): tile.FPInput = {
		val req = Wire(new tile.FPInput)
		val tag = !fp_ctrl.singleIn
		req := fp_ctrl
		req.rm := fp_rm
		req.in1 := unbox(io_req.rs1_data, tag, minT)
		req.in2 := unbox(io_req.rs2_data, tag, minT)
		req.in3 := unbox(io_req.rs3_data, tag, minT)
   	when (fp_ctrl.swap23) { req.in3 := req.in2 }
   	req.typ := ImmGenTyp(io_req.uop.imm_packed)

      val fma_decoder = Module(new FMADecoder)
      fma_decoder.io.uopc := io_req.uop.uopc
		req.fmaCmd := fma_decoder.io.cmd // ex_reg_inst(3,2) | (!fp_ctrl.ren3 && ex_reg_inst(27))
    	req
  	}


   val dfma = Module(new tile.FPUFMAPipe(latency = fpu_latency, t = tile.FType.D))
   dfma.io.in.valid := io.req.valid && fp_ctrl.fma && !fp_ctrl.singleOut
   dfma.io.in.bits := fuInput(Some(dfma.t))


   val sfma = Module(new tile.FPUFMAPipe(latency = fpu_latency, t = tile.FType.S))
   sfma.io.in.valid := io.req.valid && fp_ctrl.fma && fp_ctrl.singleOut
   sfma.io.in.bits := fuInput(Some(sfma.t))


   val fpiu = Module(new tile.FPToInt)
	fpiu.io.in.valid := io.req.valid && (fp_ctrl.toint || (fp_ctrl.fastpipe && fp_ctrl.wflags))
   fpiu.io.in.bits := fuInput(None)
   val fpiu_out = Pipe(Reg(next=fpiu.io.in.valid && !fp_ctrl.fastpipe),
                          fpiu.io.out.bits, fpu_latency-1)
   val fpiu_result  = Wire(new tile.FPResult)
   fpiu_result.data := fpiu_out.bits.toint
   fpiu_result.exc  := fpiu_out.bits.exc


   val fpmu = Module(new tile.FPToFP(fpu_latency)) // latency 2 for rocket
   fpmu.io.in.valid := io.req.valid && fp_ctrl.fastpipe
   fpmu.io.in.bits := fpiu.io.in.bits
   fpmu.io.lt := fpiu.io.out.bits.lt
   val fpmu_double = Pipe(io.req.valid && fp_ctrl.fastpipe, !fp_ctrl.singleOut, fpu_latency).bits


   // Response (all FP units have been padded out to the same latency)
   io.resp.valid := fpiu_out.valid ||
                    fpmu.io.out.valid ||
                    sfma.io.out.valid ||
                    dfma.io.out.valid
   val fpu_out_data =
      Mux(dfma.io.out.valid, box(dfma.io.out.bits.data, true.B),
      Mux(sfma.io.out.valid, box(sfma.io.out.bits.data, false.B),
      Mux(fpiu_out.valid,    fpiu_result.data,
         box(fpmu.io.out.bits.data, fpmu_double))))

   val fpu_out_exc =
      Mux(dfma.io.out.valid, dfma.io.out.bits.exc,
      Mux(sfma.io.out.valid, sfma.io.out.bits.exc,
      Mux(fpiu_out.valid,    fpiu_result.exc,
         fpmu.io.out.bits.exc)))


   io.resp.bits.data              := fpu_out_data
   io.resp.bits.fflags.valid      := io.resp.valid
   io.resp.bits.fflags.bits.flags := fpu_out_exc
}

