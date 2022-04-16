//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.ALU._
import freechips.rocketchip.util.uintToBitPat
import freechips.rocketchip.rocket.CSR
import boom.exu.BITMANIP._

import boom.common._

class RRDDecode(implicit p: Parameters) extends BoomModule {
  val io = IO(new Bundle {
    val in = Input(new MicroOp)
    val out = Output(new MicroOp)
  })


           //
           //                                      op1 sel   op2 sel
           //                                      |         |         csr_cmd
           //                                      |         |         |
           //                   alu fcn    wd/word?|         |         |
           //                   |          |       |         |         |
  val default: List[BitPat] = //|          |       |         |         |
    List[BitPat](               FN_ADD   , DW_X  , OP1_X   , OP2_X   , CSR.N)
  val table: Array[(BitPat, List[BitPat])] =
    Array[(BitPat, List[BitPat])](
      BitPat(uopLUI)    -> List(FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMM , CSR.N),

      BitPat(uopADDI)   -> List(FN_ADD   , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopANDI)   -> List(FN_AND   , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopORI)    -> List(FN_OR    , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopXORI)   -> List(FN_XOR   , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSLTI)   -> List(FN_SLT   , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSLTIU)  -> List(FN_SLTU  , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSLLI)   -> List(FN_SL    , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSRAI)   -> List(FN_SRA   , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSRLI)   -> List(FN_SR    , DW_XPR, OP1_RS1 , OP2_IMM , CSR.N),

      BitPat(uopADDIW)  -> List(FN_ADD   , DW_32 , OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSLLIW)  -> List(FN_SL    , DW_32 , OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSRAIW)  -> List(FN_SRA   , DW_32 , OP1_RS1 , OP2_IMM , CSR.N),
      BitPat(uopSRLIW)  -> List(FN_SR    , DW_32 , OP1_RS1 , OP2_IMM , CSR.N),

      BitPat(uopADD)    -> List(FN_ADD   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSLL)    -> List(FN_SL    , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSUB)    -> List(FN_SUB   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSLT)    -> List(FN_SLT   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSLTU)   -> List(FN_SLTU  , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopAND)    -> List(FN_AND   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopOR)     -> List(FN_OR    , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopXOR)    -> List(FN_XOR   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSRA)    -> List(FN_SRA   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSRL)    -> List(FN_SR    , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),

      // Special case Mov
      BitPat(uopMOV)    -> List(FN_ADD   , DW_X  , OP1_X   , OP2_X   , CSR.N),

      BitPat(uopADDW)   -> List(FN_ADD   , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSUBW)   -> List(FN_SUB   , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSLLW)   -> List(FN_SL    , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSRAW)   -> List(FN_SRA   , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopSRLW)   -> List(FN_SR    , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),

      BitPat(uopBEQ)    -> List(FN_SUB   , DW_XPR, OP1_X   , OP2_X   , CSR.N),
      BitPat(uopBNE)    -> List(FN_SUB   , DW_XPR, OP1_X   , OP2_X   , CSR.N),
      BitPat(uopBGE)    -> List(FN_SLT   , DW_XPR, OP1_X   , OP2_X   , CSR.N),
      BitPat(uopBGEU)   -> List(FN_SLTU  , DW_XPR, OP1_X   , OP2_X   , CSR.N),
      BitPat(uopBLT)    -> List(FN_SLT   , DW_XPR, OP1_X   , OP2_X   , CSR.N),
      BitPat(uopBLTU)   -> List(FN_SLTU  , DW_XPR, OP1_X   , OP2_X   , CSR.N),

      BitPat(uopJAL)    -> List(FN_ADD   , DW_XPR, OP1_PC  , OP2_NEXT, CSR.N),
      BitPat(uopJALR)   -> List(FN_ADD   , DW_XPR, OP1_PC  , OP2_NEXT, CSR.N),
      BitPat(uopAUIPC)  -> List(FN_ADD   , DW_XPR, OP1_PC  , OP2_IMM , CSR.N),

      BitPat(uopMUL)    -> List(FN_MUL   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopMULH)   -> List(FN_MULH  , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopMULHU)  -> List(FN_MULHU , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopMULHSU) -> List(FN_MULHSU, DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopMULW)   -> List(FN_MUL   , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),

      BitPat(uopDIV)    -> List(FN_DIV   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopDIVU)   -> List(FN_DIVU  , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopREM)    -> List(FN_REM   , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopREMU)   -> List(FN_REMU  , DW_XPR, OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopDIVW)   -> List(FN_DIV   , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopDIVUW)  -> List(FN_DIVU  , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopREMW)   -> List(FN_REM   , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),
      BitPat(uopREMUW)  -> List(FN_REMU  , DW_32 , OP1_RS1 , OP2_RS2 , CSR.N),

      BitPat(uopCSRRW)  -> List(FN_ADD   , DW_XPR, OP1_RS1 , OP2_ZERO, CSR.W),
      BitPat(uopCSRRS)  -> List(FN_ADD   , DW_XPR, OP1_RS1 , OP2_ZERO, CSR.S),
      BitPat(uopCSRRC)  -> List(FN_ADD   , DW_XPR, OP1_RS1 , OP2_ZERO, CSR.C),

      BitPat(uopCSRRWI) -> List(FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.W),
      BitPat(uopCSRRSI) -> List(FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.S),
      BitPat(uopCSRRCI) -> List(FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.C),

      BitPat(uopSFENCE) -> List(FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.R),
      BitPat(uopWFI)    -> List(FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.I),
      BitPat(uopSCALL)  -> List(FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.I),
      BitPat(uopSBREAK) -> List(FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.I),
      BitPat(uopERET)   -> List(FN_ADD   , DW_XPR, OP1_ZERO, OP2_IMMC, CSR.I),
         //
           //                                       op1 sel   op2 sel
           //                                       |         |         csr_cmd
           //                                       |         |         |
           //              bitmanip fcn     wd/word?|         |         |
           //                   |           |       |         |         |
      BitPat(uopANDN)   -> List(FN_ANDN   , DW_XPR, OP1_RS1, OP2_RS2 , CSR.N),
      BitPat(uopORN)    -> List(FN_ORN    , DW_XPR, OP1_RS1, OP2_RS2 , CSR.N),
      BitPat(uopXNOR)   -> List(FN_XNOR   , DW_XPR, OP1_RS1, OP2_RS2 , CSR.N),

      BitPat(uopCLZ)    -> List(FN_CLZ    , DW_XPR, OP1_RS1, OP2_X   , CSR.N),
      BitPat(uopCLZW)   -> List(FN_CLZ    , DW_32 , OP1_RS1, OP2_X   , CSR.N),
      BitPat(uopCTZ)    -> List(FN_CTZ    , DW_XPR, OP1_RS1, OP2_X   , CSR.N),
      BitPat(uopCTZW)   -> List(FN_CTZ    , DW_32 , OP1_RS1, OP2_X   , CSR.N),

      BitPat(uopPCNT)   -> List(FN_PCNT   , DW_XPR, OP1_RS1, OP2_X   , CSR.N),
      BitPat(uopPCNTW)  -> List(FN_PCNT   , DW_32 , OP1_RS1, OP2_X   , CSR.N),

      BitPat(uopMAX)    -> List(FN_MAX    , DW_XPR, OP1_RS1, OP2_RS2 , CSR.N),
      BitPat(uopMAXU)   -> List(FN_MAXU   , DW_XPR, OP1_RS1, OP2_RS2 , CSR.N),
      BitPat(uopMIN)    -> List(FN_MIN    , DW_XPR, OP1_RS1, OP2_RS2 , CSR.N),
      BitPat(uopMINU)   -> List(FN_MINU   , DW_XPR, OP1_RS1, OP2_RS2 , CSR.N),

      BitPat(uopSEXTB) -> List(FN_SEXTB   , DW_XPR, OP1_RS1, OP2_X   , CSR.N),
      BitPat(uopSEXTH) -> List(FN_SEXTH   , DW_XPR, OP1_RS1, OP2_X   , CSR.N),
      BitPat(uopZEXTH) -> List(FN_ZEXTH   , DW_XPR, OP1_RS1, OP2_X   , CSR.N),

      BitPat(uopROL)   -> List(FN_ROL     , DW_XPR, OP1_RS1, OP2_RS2 , CSR.N),
      BitPat(uopROLW)  -> List(FN_ROL     , DW_32 , OP1_RS1, OP2_RS2 , CSR.N),
      BitPat(uopROR)   -> List(FN_ROR     , DW_XPR, OP1_RS1, OP2_RS2 , CSR.N),
      BitPat(uopRORI)  -> List(FN_ROR     , DW_XPR, OP1_RS1, OP2_IMM , CSR.N),
      BitPat(uopRORIW) -> List(FN_ROR     , DW_32 , OP1_RS1, OP2_IMM , CSR.N),
      BitPat(uopRORW)  -> List(FN_ROR     , DW_32 , OP1_RS1, OP2_RS2 , CSR.N),

      BitPat(uopORCB)  -> List(FN_ORCB    , DW_XPR, OP1_RS1, OP2_X   , CSR.N),

      BitPat(uopREV8)  -> List(FN_REV8    , DW_XPR, OP1_RS1, OP2_X   , CSR.N),
      BitPat(uopPACK)  -> List(FN_PACK    , DW_XPR, OP1_RS1, OP2_RS2 , CSR.N))      


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
    out.fcn_op := decoder.io.out.fcn_op //change this field to 5
    out.fcn_dw := decoder.io.out.fcn_dw
    out.op1_sel := decoder.io.out.op1_sel
    out.op2_sel := decoder.io.out.op2_sel
    out.csr_cmd := decoder.io.out.csr_cmd
    out.br_type := decoder.io.out.br_type
    out
  }
}
