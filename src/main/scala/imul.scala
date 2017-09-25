//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom
import Chisel._

import rocket.ALU._
// TODO can repurpose the FN_* ctrl signals,
// as we don't overlap with the ALU

class IMul(imul_stages: Int) extends Module
{
   val io = IO(new Bundle {
      val valid = Bool(INPUT)
      val fn  = UInt(INPUT, SZ_ALU_FN)
      val dw  = UInt(INPUT, SZ_DW)
      val in0 = UInt(INPUT,  64)
      val in1 = UInt(INPUT,  64)
      val out = UInt(OUTPUT, 64)
   })

   def FN(dw: UInt, fn: UInt) = io.dw === dw && io.fn === fn
   val sxl64 = FN(DW_64, FN_MULH) | FN(DW_64, FN_MULHSU)
   val sxr64 = FN(DW_64, FN_MULH)
   val zxl32 = FN(DW_32, FN_MULHU)
   val zxr32 = FN(DW_32, FN_MULHU) | FN(DW_32, FN_MULHSU)
   val sxl32 = FN(DW_32, FN_MULH) | FN(DW_32, FN_MULHSU)
   val sxr32 = FN(DW_32, FN_MULH)

   val lhs = Cat(
      io.in0(63) & sxl64,
      Fill(32, ~zxl32)&io.in0(63,32) | Fill(32, sxl32&io.in0(31)),
      io.in0(31,0)) //TODO: 65 bits
   val rhs = Cat(
      io.in1(63) & sxr64,
      Fill(32, ~zxr32)&io.in1(63,32) | Fill(32, sxr32&io.in1(31)),
      io.in1(31,0)) //TODO: 65 bits

   val mul_result = lhs.asSInt * rhs.asSInt //TODO:130 bits

   val mul_output_mux = MuxCase(
      UInt(0, 64), Array(
         FN(DW_64, FN_MUL)    -> mul_result(63,0),
         FN(DW_64, FN_MULH)   -> mul_result(127,64),
         FN(DW_64, FN_MULHU)  -> mul_result(127,64),
         FN(DW_64, FN_MULHSU) -> mul_result(127,64),
         FN(DW_32, FN_MUL)    -> Cat(Fill(32, mul_result(31)), mul_result(31,0)),
         FN(DW_32, FN_MULH)   -> Cat(Fill(32, mul_result(63)), mul_result(63,32)),
         FN(DW_32, FN_MULHU)  -> Cat(Fill(32, mul_result(63)), mul_result(63,32)),
         FN(DW_32, FN_MULHSU) -> Cat(Fill(32, mul_result(63)), mul_result(63,32))
   ))

   io.out := Pipe(io.valid, mul_output_mux, imul_stages).bits
}

