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
import boom.util.{ImmGenRm, ImmGenTyp, Packing}

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



   val table: Array[(BitPat, List[BitPat])] =
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
      BitPat(uopVADD)     -> List(X,X,Y,Y,N, N,Y,N,N,N,N,N, Y,N,N,Y),
      BitPat(uopVSUB)     -> List(X,X,Y,Y,N, N,Y,N,N,N,N,N, Y,N,N,Y),
      BitPat(uopVMUL)     -> List(X,X,Y,Y,N, N,N,N,N,N,N,N, Y,N,N,Y),
      BitPat(uopVMADD)    -> List(X,X,Y,Y,Y, N,N,N,N,N,N,N, Y,N,N,Y),
      BitPat(uopVMSUB)    -> List(X,X,Y,Y,Y, N,N,N,N,N,N,N, Y,N,N,Y),
      BitPat(uopVNMADD)   -> List(X,X,Y,Y,Y, N,N,N,N,N,N,N, Y,N,N,Y),
      BitPat(uopVNMSUB)   -> List(X,X,Y,Y,Y, N,N,N,N,N,N,N, Y,N,N,Y)
      )

   val insns = table
   val decoder = rocket.DecodeLogic(io.uopc, default, insns)

   val s = io.sigs
   val sigs = Seq(s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                  s.swap23, s.singleIn, s.singleOut, s.fromint, s.toint, s.fastpipe, s.fma,
                  s.div, s.sqrt, s.wflags)
   sigs zip decoder map {case(s,d) => s := d}
}

class VFMADecoder extends Module
{
   val io = IO(new Bundle
   {
      val uopc = UInt(INPUT, UOPC_SZ)
      val cmd = UInt(OUTPUT, 2)
   })

   val default: List[BitPat] = List(BitPat("b??"))
   val table: Array[(BitPat, List[BitPat])] =
   Array(
      BitPat(uopVADD)     -> List(BitPat("b00")), // TODO : Add bindings for vector uops
      BitPat(uopVSUB)     -> List(BitPat("b01")),
      BitPat(uopVMUL)     -> List(BitPat("b00")),
      BitPat(uopVMADD)    -> List(BitPat("b00")),
      BitPat(uopVMSUB)    -> List(BitPat("b01")),
      BitPat(uopVNMADD)   -> List(BitPat("b11")),
      BitPat(uopVNMSUB)   -> List(BitPat("b10"))
      )

   val decoder = rocket.DecodeLogic(io.uopc, default, table)

   val (cmd: UInt) :: Nil = decoder
   io.cmd := cmd
}


class VFpuReq()(implicit p: Parameters) extends BoomBundle()(p)
{ // TODO_Vec: Figure out width. 128? 130?
   val uop      = new MicroOp()
   val rs1_data = Bits(width = 128)
   val rs2_data = Bits(width = 128)
   val rs3_data = Bits(width = 128)
   val fcsr_rm  = Bits(width = tile.FPConstants.RM_SZ) // TODO_Vec: Is rounding mode still controlled by fp csrs?
}
class VFPU(implicit p: Parameters) extends BoomModule()(p) with tile.HasFPUParameters with Packing
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
   def vfpuInput(minT: Option[tile.FType], width: Int, idx: Int,
      recode_fn: Bits=>Bits,
      unpack_fn: (Bits, Int)=>UInt): tile.FPInput = {
      val req = Wire(new tile.FPInput)

      req := vfp_ctrl
      req.rm := vfp_rm
      // TODO_vec: Ugh why is hardfloat weird
      req.in1 := recode_fn(unpack_fn(io_req.rs1_data, idx))
      req.in2 := recode_fn(unpack_fn(io_req.rs2_data, idx))
      req.in3 := recode_fn(unpack_fn(io_req.rs3_data, idx))
      when (vfp_ctrl.swap23) {req.in3 := req.in2 }
      req.typ := ImmGenTyp(io_req.uop.imm_packed)
      val fma_decoder = Module(new VFMADecoder)
      fma_decoder.io.uopc := io_req.uop.uopc
      req.fmaCmd := fma_decoder.io.cmd
      req
   }



   when (io.req.valid)
   {
      assert(io.req.bits.uop.dst_rtype === RT_VEC && io.req.bits.uop.rd_vshape === VSHAPE_VECTOR,
         "Desination must be vector reg\n")
      when (io.req.bits.uop.lrs1_rtype =/= RT_X)
      {
         assert(io.req.bits.uop.rs1_vew === io.req.bits.uop.rd_vew,
            "Element width of rs1 does not match")
         assert(io.req.bits.uop.rs1_verep === io.req.bits.uop.rd_verep,
            "Element rep of rs1 does not match")
      }
      when (io.req.bits.uop.lrs2_rtype =/= RT_X)
      {
         assert(io.req.bits.uop.rs2_vew === io.req.bits.uop.rd_vew,
            "Element width of rs2 does not match")
         assert(io.req.bits.uop.rs2_verep === io.req.bits.uop.rd_verep,
            "Element rep of rs2 does not match")
      }
      when (io.req.bits.uop.lrs3_rtype =/= RT_X)
      {
         assert(io.req.bits.uop.rs3_vew === io.req.bits.uop.rd_vew,
            "Element width of rs3 does not match")
         assert(io.req.bits.uop.rs3_verep === io.req.bits.uop.rd_verep,
            "Element rep of rs3 does not match")
      }
   }

   val results =
      List((SZ_D, VEW_64, recode_dp _, unpack_d _, ieee_dp _, repack_d _, expand_float_d _, (11, 53)),
           (SZ_W, VEW_32, recode_sp _, unpack_w _, ieee_sp _, repack_w _, expand_float_s _, (8, 24)),
           (SZ_H, VEW_16, recode_hp _, unpack_h _, ieee_hp _, repack_h _, expand_float_h _, (5, 11))) map {
         case (sz, ew, recode, unpack, ieee, repack, expand, (exp, sig)) => {
            val n = 128 / sz
            val fp_val = io.req.valid && io.req.bits.uop.rd_vew === ew
            val results = for (i <- (0 until n)) yield {
               val fma = Module(new tile.FPUFMAPipe(latency=vfpu_latency, t=tile.FType(exp=exp, sig=sig)))
               // TODO_vec add predication here
               val valid = fp_val
               fma.io.in.valid := valid
               fma.io.in.bits := vfpuInput(Some(fma.t), sz, i, recode, unpack)

               val out_data = ieee(fma.io.out.bits.data)
               val out_exc  = fma.io.out.bits.exc
               val out_val  = fma.io.out.valid
               (out_val, out_data, out_exc)
            }
            val result_val = results.map(_._1).reduce(_||_)
            assert ( result_val === results.map(_._1).reduce(_&&_),
               "VFPU slice not all responding valid at the same time!")
            val result_out = repack(results.map(_._2))
            val result_exc = results.map(_._3).reduce(_|_)
            (result_val, result_out, result_exc)
         }
      }

   val fpmatch = results.map(_._1)
   io.resp.valid := fpmatch.reduce(_||_)
   io.resp.bits.data := Mux1H(fpmatch, results.map(_._2))
   io.resp.bits.fflags.valid := io.resp.valid
   io.resp.bits.fflags.bits.flags := Mux1H(fpmatch, results.map(_._3))

}
