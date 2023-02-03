//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.uintToBitPat
import freechips.rocketchip.rocket.CSR

import boom.common._

class RRDDecode(implicit p: Parameters) extends BoomModule {
  val aluFn = new freechips.rocketchip.rocket.ALUFN()
  val io = IO(new Bundle {
    val in = Input(new MicroOp)
    val out = Output(new MicroOp)
  })


           //
           //                                            op1 sel   op2 sel
           //                                            |         |         csr_cmd
           //                                            |         |         |
           //                   alu fcn          wd/word?|         |         |
           //                   |                |       |         |         |
  val default: List[BitPat] = //|                |       |         |         |
    List[BitPat](               aluFn.FN_ADD   , DW_X  , OP1_X   , OP2_X   , CSR.N)
  val table: Array[(BitPat, List[BitPat])] =
    Array[(BitPat, List[BitPat])](
      BitPat(uopLUI)    -> List(aluFn.FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMM , CSR.N),

      BitPat(uopADDI)   -> List(aluFn.FN_ADD   , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopANDI)   -> List(aluFn.FN_AND   , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopORI)    -> List(aluFn.FN_OR    , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopXORI)   -> List(aluFn.FN_XOR   , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSLTI)   -> List(aluFn.FN_SLT   , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSLTIU)  -> List(aluFn.FN_SLTU  , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSLLI)   -> List(aluFn.FN_SL    , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSRAI)   -> List(aluFn.FN_SRA   , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSRLI)   -> List(aluFn.FN_SR    , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),

      BitPat(uopADDIW)  -> List(aluFn.FN_ADD   , DW_32 , OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSLLIW)  -> List(aluFn.FN_SL    , DW_32 , OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSRAIW)  -> List(aluFn.FN_SRA   , DW_32 , OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSRLIW)  -> List(aluFn.FN_SR    , DW_32 , OP1_RS1 , OP2_IMM , CSR.N),

      BitPat(uopADD)    -> List(aluFn.FN_ADD   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSLL)    -> List(aluFn.FN_SL    , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSUB)    -> List(aluFn.FN_SUB   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSLT)    -> List(aluFn.FN_SLT   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSLTU)   -> List(aluFn.FN_SLTU  , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopAND)    -> List(aluFn.FN_AND   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopOR)     -> List(aluFn.FN_OR    , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopXOR)    -> List(aluFn.FN_XOR   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSRA)    -> List(aluFn.FN_SRA   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSRL)    -> List(aluFn.FN_SR    , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),

      // Special case Mov
      BitPat(uopMOV)    -> List(aluFn.FN_ADD   , DW_X  , OP1_X   , OP2_X   , CSR.N),

      BitPat(uopADDW)   -> List(aluFn.FN_ADD   , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSUBW)   -> List(aluFn.FN_SUB   , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSLLW)   -> List(aluFn.FN_SL    , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSRAW)   -> List(aluFn.FN_SRA   , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSRLW)   -> List(aluFn.FN_SR    , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),

      BitPat(uopBEQ)    -> List(aluFn.FN_SUB   , DW_XPR, OP1_X   , OP2_X   , CSR.N),
      BitPat(uopBNE)    -> List(aluFn.FN_SUB   , DW_XPR, OP1_X   , OP2_X   , CSR.N),
      BitPat(uopBGE)    -> List(aluFn.FN_SLT   , DW_XPR, OP1_X   , OP2_X   , CSR.N),
      BitPat(uopBGEU)   -> List(aluFn.FN_SLTU  , DW_XPR, OP1_X   , OP2_X   , CSR.N),
      BitPat(uopBLT)    -> List(aluFn.FN_SLT   , DW_XPR, OP1_X   , OP2_X   , CSR.N),
      BitPat(uopBLTU)   -> List(aluFn.FN_SLTU  , DW_XPR, OP1_X   , OP2_X   , CSR.N),

      BitPat(uopJAL)    -> List(aluFn.FN_ADD   , DW_XPR, OP1_PC  , OP2_NEXT, CSR.N),
      BitPat(uopJALR)   -> List(aluFn.FN_ADD   , DW_XPR, OP1_PC  , OP2_NEXT, CSR.N),
      BitPat(uopAUIPC)  -> List(aluFn.FN_ADD   , DW_XPR, OP1_PC  , OP2_IMM , CSR.N),

      BitPat(uopMUL)    -> List(aluFn.FN_MUL   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopMULH)   -> List(aluFn.FN_MULH  , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopMULHU)  -> List(aluFn.FN_MULHU , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopMULHSU) -> List(aluFn.FN_MULHSU, DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopMULW)   -> List(aluFn.FN_MUL   , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),

      BitPat(uopDIV)    -> List(aluFn.FN_DIV   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopDIVU)   -> List(aluFn.FN_DIVU  , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopREM)    -> List(aluFn.FN_REM   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopREMU)   -> List(aluFn.FN_REMU  , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopDIVW)   -> List(aluFn.FN_DIV   , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopDIVUW)  -> List(aluFn.FN_DIVU  , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopREMW)   -> List(aluFn.FN_REM   , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopREMUW)  -> List(aluFn.FN_REMU  , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),

      BitPat(uopCSRRW)  -> List(aluFn.FN_ADD   , DW_XPR, OP1_RS1 , OP2_ZERO, CSR.W),
      BitPat(uopCSRRS)  -> List(aluFn.FN_ADD   , DW_XPR, OP1_RS1 , OP2_ZERO, CSR.S),
      BitPat(uopCSRRC)  -> List(aluFn.FN_ADD   , DW_XPR, OP1_RS1 , OP2_ZERO, CSR.C),

      BitPat(uopCSRRWI) -> List(aluFn.FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.W),
      BitPat(uopCSRRSI) -> List(aluFn.FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.S),
      BitPat(uopCSRRCI) -> List(aluFn.FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.C),

      BitPat(uopSFENCE) -> List(aluFn.FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.R),
      BitPat(uopWFI)    -> List(aluFn.FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.I),
      BitPat(uopSCALL)  -> List(aluFn.FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.I),
      BitPat(uopSBREAK) -> List(aluFn.FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.I),
      BitPat(uopERET)   -> List(aluFn.FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.I))



  val uop = WireInit(io.in)
  io.out := uop
  val decoder = freechips.rocketchip.rocket.DecodeLogic(io.in.uopc, default, table)
  Seq(uop.fcn_op, uop.fcn_dw, uop.op1_sel, uop.op2_sel, uop.csr_cmd) zip decoder map {
    case (s,d) => s := d
  }
  io.out.br_type := Seq(
    (uopBEQ , B_EQ ),
    (uopBNE , B_NE ),
    (uopBGE , B_GE ),
    (uopBGEU, B_GEU),
    (uopBLT , B_LT ),
    (uopBLTU, B_LTU),
    (uopJAL , B_J  ),
    (uopJALR, B_JR )
  ) .map { case (c, b) => Mux(io.in.uopc === c, b, 0.U) } .reduce(_|_)
}

object RRDDecode {
  def apply(uop: MicroOp)(implicit p: Parameters): MicroOp = {
    val decoder = Module(new RRDDecode)
    decoder.io.in := uop
    val out = WireInit(uop)
    out.fcn_op := decoder.io.out.fcn_op
    out.fcn_dw := decoder.io.out.fcn_dw
    out.op1_sel := decoder.io.out.op1_sel
    out.op2_sel := decoder.io.out.op2_sel
    out.csr_cmd := decoder.io.out.csr_cmd
    out.br_type := decoder.io.out.br_type
    out
  }
}
