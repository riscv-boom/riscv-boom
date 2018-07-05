//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Functional Unit Decode
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// Generate the functional unit control signals from the micro-op opcodes.

package boom.exu

import Chisel._
import freechips.rocketchip.config.Parameters

import freechips.rocketchip.rocket.ALU._
import freechips.rocketchip.util.uintToBitPat
import freechips.rocketchip.rocket.CSR
import boom.common._

class RRdCtrlSigs(implicit p: Parameters) extends BoomBundle()(p)
{
   val br_type          = UInt(width = BR_N.getWidth)
   val use_alupipe      = Bool()
   val use_muldivpipe   = Bool()
   val use_mempipe      = Bool()
   val op_fcn      = Bits(width = SZ_ALU_FN)
   val fcn_dw      = Bool()
   val op1_sel     = UInt(width = OP1_X.getWidth)
   val op2_sel     = UInt(width = OP2_X.getWidth)
   val imm_sel     = UInt(width = IS_X.getWidth)
   val rf_wen      = Bool()
   val csr_cmd     = Bits(width = CSR.SZ)

   def decode(uopc: UInt, table: Iterable[(BitPat, List[BitPat])]) =
   {
      val decoder = freechips.rocketchip.rocket.DecodeLogic(uopc, AluRRdDecode.default, table)
      val sigs = Seq(br_type, use_alupipe, use_muldivpipe, use_mempipe, op_fcn,
                     fcn_dw, op1_sel, op2_sel, imm_sel, rf_wen, csr_cmd)
      sigs zip decoder map {case(s,d) => s := d}
      this
   }
}

abstract trait RRdDecodeConstants
{
   val default: List[BitPat] =
                     List[BitPat](BR_N , Y, N, N, FN_ADD , DW_X  , OP1_X   , OP2_X   , IS_X, REN_0, CSR.N)
   val table: Array[(BitPat, List[BitPat])]
}

object AluRRdDecode extends RRdDecodeConstants
{
   val table: Array[(BitPat, List[BitPat])] =
              Array[(BitPat, List[BitPat])](
                               // br type
                               // |      use alu pipe              op1 sel   op2 sel
                               // |      |  use muldiv pipe        |         |         immsel       csr_cmd
                               // |      |  |  use mem pipe        |         |         |     rf wen |
                               // |      |  |  |  alu fcn  wd/word?|         |         |     |      |
                               // |      |  |  |  |        |       |         |         |     |      |
         BitPat(uopLUI)   -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMM , IS_U, REN_1, CSR.N),

         BitPat(uopADDI)  -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
         BitPat(uopANDI)  -> List(BR_N , Y, N, N, FN_AND , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
         BitPat(uopORI)   -> List(BR_N , Y, N, N, FN_OR  , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
         BitPat(uopXORI)  -> List(BR_N , Y, N, N, FN_XOR , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
         BitPat(uopSLTI)  -> List(BR_N , Y, N, N, FN_SLT , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
         BitPat(uopSLTIU) -> List(BR_N , Y, N, N, FN_SLTU, DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
         BitPat(uopSLLI)  -> List(BR_N , Y, N, N, FN_SL  , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
         BitPat(uopSRAI)  -> List(BR_N , Y, N, N, FN_SRA , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
         BitPat(uopSRLI)  -> List(BR_N , Y, N, N, FN_SR  , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),

         BitPat(uopADDIW) -> List(BR_N , Y, N, N, FN_ADD , DW_32 , OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
         BitPat(uopSLLIW) -> List(BR_N , Y, N, N, FN_SL  , DW_32 , OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
         BitPat(uopSRAIW) -> List(BR_N , Y, N, N, FN_SRA , DW_32 , OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
         BitPat(uopSRLIW) -> List(BR_N , Y, N, N, FN_SR  , DW_32 , OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),

         BitPat(uopADD)   -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopSLL)   -> List(BR_N , Y, N, N, FN_SL  , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopSUB)   -> List(BR_N , Y, N, N, FN_SUB , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopSLT)   -> List(BR_N , Y, N, N, FN_SLT , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopSLTU)  -> List(BR_N , Y, N, N, FN_SLTU, DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopAND)   -> List(BR_N , Y, N, N, FN_AND , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopOR)    -> List(BR_N , Y, N, N, FN_OR  , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopXOR)   -> List(BR_N , Y, N, N, FN_XOR , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopSRA)   -> List(BR_N , Y, N, N, FN_SRA , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopSRL)   -> List(BR_N , Y, N, N, FN_SR  , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),

         BitPat(uopADDW)  -> List(BR_N , Y, N, N, FN_ADD , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopSUBW)  -> List(BR_N , Y, N, N, FN_SUB , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopSLLW)  -> List(BR_N , Y, N, N, FN_SL  , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopSRAW)  -> List(BR_N , Y, N, N, FN_SRA , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopSRLW)  -> List(BR_N , Y, N, N, FN_SR  , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N))
}

object BruRRdDecode extends RRdDecodeConstants
{
   val table: Array[(BitPat, List[BitPat])] =
              Array[(BitPat, List[BitPat])](
                               // br type
                               // |      use alu pipe              op1 sel   op2 sel
                               // |      |  use muldiv pipe        |         |         immsel       csr_cmd
                               // |      |  |  use mem pipe        |         |         |     rf wen |
                               // |      |  |  |  alu fcn  wd/word?|         |         |     |      |
                               // |      |  |  |  |        |       |         |         |     |      |
         BitPat(uopBEQ)   -> List(BR_EQ ,Y, N, N, FN_SUB , DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, CSR.N),
         BitPat(uopBNE)   -> List(BR_NE ,Y, N, N, FN_SUB , DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, CSR.N),
         BitPat(uopBGE)   -> List(BR_GE ,Y, N, N, FN_SLT , DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, CSR.N),
         BitPat(uopBGEU)  -> List(BR_GEU,Y, N, N, FN_SLTU, DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, CSR.N),
         BitPat(uopBLT)   -> List(BR_LT ,Y, N, N, FN_SLT , DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, CSR.N),
         BitPat(uopBLTU)  -> List(BR_LTU,Y, N, N, FN_SLTU, DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, CSR.N),

         BitPat(uopJAL)   -> List(BR_J , Y, N, N, FN_ADD , DW_XPR, OP1_PC  , OP2_FOUR, IS_J, REN_1, CSR.N),
         BitPat(uopJALR)  -> List(BR_JR, Y, N, N, FN_ADD , DW_XPR, OP1_PC  , OP2_FOUR, IS_I, REN_1, CSR.N),
         BitPat(uopAUIPC) -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_PC  , OP2_IMM , IS_U, REN_1, CSR.N))
}

object MulDivRRdDecode extends RRdDecodeConstants
{
   val table: Array[(BitPat, List[BitPat])] =
              Array[(BitPat, List[BitPat])](
                               // br type
                               // |      use alu pipe              op1 sel   op2 sel
                               // |      |  use muldiv pipe        |         |         immsel       csr_cmd
                               // |      |  |  use mem pipe        |         |         |     rf wen |
                               // |      |  |  |  alu fcn  wd/word?|         |         |     |      |
                               // |      |  |  |  |        |       |         |         |     |      |
         BitPat(uopMUL)   -> List(BR_N , N, Y, N, FN_MUL,   DW_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,CSR.N),
         BitPat(uopMULH)  -> List(BR_N , N, Y, N, FN_MULH,  DW_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,CSR.N),
         BitPat(uopMULHU) -> List(BR_N , N, Y, N, FN_MULHU, DW_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,CSR.N),
         BitPat(uopMULHSU)-> List(BR_N , N, Y, N, FN_MULHSU,DW_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,CSR.N),
         BitPat(uopMULW)  -> List(BR_N , N, Y, N, FN_MUL,   DW_32 ,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,CSR.N),

         BitPat(uopDIV)   -> List(BR_N , N, Y, N, FN_DIV , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopDIVU)  -> List(BR_N , N, Y, N, FN_DIVU, DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopREM)   -> List(BR_N , N, Y, N, FN_REM , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopREMU)  -> List(BR_N , N, Y, N, FN_REMU, DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopDIVW)  -> List(BR_N , N, Y, N, FN_DIV , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopDIVUW) -> List(BR_N , N, Y, N, FN_DIVU, DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopREMW)  -> List(BR_N , N, Y, N, FN_REM , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
         BitPat(uopREMUW) -> List(BR_N , N, Y, N, FN_REMU, DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N))
}

object MemRRdDecode extends RRdDecodeConstants
{
   val table: Array[(BitPat, List[BitPat])] =
              Array[(BitPat, List[BitPat])](
                               // br type
                               // |      use alu pipe              op1 sel   op2 sel
                               // |      |  use muldiv pipe        |         |         immsel       csr_cmd
                               // |      |  |  use mem pipe        |         |         |     rf wen |
                               // |      |  |  |  alu fcn  wd/word?|         |         |     |      |
                               // |      |  |  |  |        |       |         |         |     |      |
         BitPat(uopLD)    -> List(BR_N , N, N, Y, FN_ADD , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_0, CSR.N),
         BitPat(uopSTA)   -> List(BR_N , N, N, Y, FN_ADD , DW_XPR, OP1_RS1 , OP2_IMM , IS_S, REN_0, CSR.N),
         BitPat(uopSTD)   -> List(BR_N , N, N, Y, FN_X   , DW_X  , OP1_RS1 , OP2_RS2 , IS_X, REN_0, CSR.N),
         BitPat(uopSFENCE)-> List(BR_N , N, N, Y, FN_X   , DW_X  , OP1_RS1 , OP2_RS2 , IS_X, REN_0, CSR.N),

         BitPat(uopAMO_AG)-> List(BR_N , N, N, Y, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_X, REN_0, CSR.N))
}

object CsrRRdDecode extends RRdDecodeConstants
{
   val table: Array[(BitPat, List[BitPat])] =
              Array[(BitPat, List[BitPat])](
                               // br type
                               // |      use alu pipe              op1 sel   op2 sel
                               // |      |  use muldiv pipe        |         |         immsel       csr_cmd
                               // |      |  |  use mem pipe        |         |         |     rf wen |
                               // |      |  |  |  alu fcn  wd/word?|         |         |     |      |
                               // |      |  |  |  |        |       |         |         |     |      |
         BitPat(uopCSRRW) -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_I, REN_1, CSR.W),
         BitPat(uopCSRRS) -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_I, REN_1, CSR.S),
         BitPat(uopCSRRC) -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_I, REN_1, CSR.C),

         BitPat(uopCSRRWI)-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_1, CSR.W),
         BitPat(uopCSRRSI)-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_1, CSR.S),
         BitPat(uopCSRRCI)-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_1, CSR.C),

         BitPat(uopSYSTEM)-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_0, CSR.I))
}

object FpuRRdDecode extends RRdDecodeConstants
{
   val table: Array[(BitPat, List[BitPat])] =
              Array[(BitPat, List[BitPat])](
                               // br type
                               // |      use alu pipe              op1 sel   op2 sel
                               // |      |  use muldiv pipe        |         |         immsel       csr_cmd
                               // |      |  |  use mem pipe        |         |         |     rf wen |
                               // |      |  |  |  alu fcn  wd/word?|         |         |     |      |
                               // |      |  |  |  |        |       |         |         |     |      |
         BitPat(uopFCLASS_S)->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCLASS_D)->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),

//         BitPat(uopFMV_S_X)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
//         BitPat(uopFMV_D_X)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFMV_X_S)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFMV_X_D)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFSGNJ_S)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFSGNJ_D)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),

         BitPat(uopFCVT_S_D) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_D_S) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),

// TODO comment out I2F instructions.
         BitPat(uopFCVT_S_W) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_S_WU)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_S_L) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_S_LU)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_D_W) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_D_WU)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_D_L) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_D_LU)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),

         BitPat(uopFCVT_W_S) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_WU_S)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_L_S) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_LU_S)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_W_D) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_WU_D)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_L_D) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_LU_D)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),

         BitPat(uopFEQ_S)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFLT_S)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFLE_S)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFEQ_D)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFLT_D)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFLE_D)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),

         BitPat(uopFMIN_S)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFMAX_S)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFMIN_D)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFMAX_D)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),

         BitPat(uopFADD_S)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFSUB_S)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFMUL_S)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFADD_D)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFSUB_D)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFMUL_D)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),

         BitPat(uopFMADD_S) ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFMSUB_S) ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFNMADD_S)->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFNMSUB_S)->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFMADD_D) ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFMSUB_D) ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFNMADD_D)->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFNMSUB_D)->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N))
}
 

object IfmvRRdDecode extends RRdDecodeConstants
{
   val table: Array[(BitPat, List[BitPat])] =
              Array[(BitPat, List[BitPat])](
                               // br type
                               // |      use alu pipe              op1 sel   op2 sel
                               // |      |  use muldiv pipe        |         |         immsel       csr_cmd
                               // |      |  |  use mem pipe        |         |         |     rf wen |
                               // |      |  |  |  alu fcn  wd/word?|         |         |     |      |
                               // |      |  |  |  |        |       |         |         |     |      |
         BitPat(uopFMV_S_X)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFMV_D_X)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),

         BitPat(uopFCVT_S_W) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_S_WU)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_S_L) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_S_LU)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_D_W) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_D_WU)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_D_L) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFCVT_D_LU)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N))
}

 

object FDivRRdDecode extends RRdDecodeConstants
{
   val table: Array[(BitPat, List[BitPat])] =
              Array[(BitPat, List[BitPat])](
                               // br type
                               // |      use alu pipe              op1 sel   op2 sel
                               // |      |  use muldiv pipe        |         |         immsel       csr_cmd
                               // |      |  |  use mem pipe        |         |         |     rf wen |
                               // |      |  |  |  alu fcn  wd/word?|         |         |     |      |
                               // |      |  |  |  |        |       |         |         |     |      |
         BitPat(uopFDIV_S)  ->List(BR_N, N, Y, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFDIV_D)  ->List(BR_N, N, Y, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFSQRT_S) ->List(BR_N, N, Y, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopFSQRT_D) ->List(BR_N, N, Y, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N))
}


// Do we really need these tables? Literally the only thing they do is set rf_wen to 1
object VFPURRdDecode extends RRdDecodeConstants
{
   val table: Array[(BitPat, List[BitPat])] =
              Array[(BitPat, List[BitPat])](
                               // br type
                               // |      use alu pipe              op1 sel   op2 sel
                               // |      |  use muldiv pipe        |         |         immsel       csr_cmd
                               // |      |  |  use mem pipe        |         |         |     rf wen |
                               // |      |  |  |  alu fcn  wd/word?|         |         |     |      |
                               // |      |  |  |  |        |       |         |         |     |      |
         BitPat(uopVADD)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopVSUB)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopVMUL)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopVMADD)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopVMSUB)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopVNMADD) ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopVNMSUB) ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N),
         BitPat(uopVINSERT)->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, CSR.N)
              )
}

object VALURRdDecode extends RRdDecodeConstants
{
   val table: Array[(BitPat, List[BitPat])] =
              Array[(BitPat, List[BitPat])](
                               // br type
                               // |      use alu pipe              op1 sel   op2 sel
                               // |      |  use muldiv pipe        |         |         immsel       csr_cmd
                               // |      |  |  use mem pipe        |         |         |     rf wen |
                               // |      |  |  |  alu fcn  wd/word?|         |         |     |      |
                               // |      |  |  |  |        |       |         |         |     |      |

              )
} 



class RegisterReadDecode(supported_units: SupportedFuncUnits)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new BoomBundle()(p)
   {
      val iss_valid = Bool(INPUT)
      val iss_uop   = new MicroOp().asInput

      val rrd_valid = Bool(OUTPUT)
      val rrd_uop   = new MicroOp().asOutput
   })

   // Issued Instruction
   val rrd_valid = io.iss_valid
   io.rrd_uop   := io.iss_uop

   var dec_table = AluRRdDecode.table
   if (supported_units.bru) dec_table ++= BruRRdDecode.table
   if (supported_units.mem) dec_table ++= MemRRdDecode.table
   if (supported_units.muld) dec_table ++= MulDivRRdDecode.table
   if (supported_units.csr) dec_table ++= CsrRRdDecode.table
   if (supported_units.fpu) dec_table ++= FpuRRdDecode.table
   if (supported_units.fdiv) dec_table ++= FDivRRdDecode.table
   if (supported_units.ifpu) dec_table ++= IfmvRRdDecode.table
   if (supported_units.vfpu) dec_table ++= VFPURRdDecode.table
   if (supported_units.valu) dec_table ++= VALURRdDecode.table
   val rrd_cs = Wire(new RRdCtrlSigs()).decode(io.rrd_uop.uopc, dec_table)

   // rrd_use_alupipe is unused
   io.rrd_uop.ctrl.br_type := rrd_cs.br_type
   io.rrd_uop.ctrl.rf_wen  := rrd_cs.rf_wen
   io.rrd_uop.ctrl.op1_sel := rrd_cs.op1_sel
   io.rrd_uop.ctrl.op2_sel := rrd_cs.op2_sel
   io.rrd_uop.ctrl.imm_sel := rrd_cs.imm_sel
   io.rrd_uop.ctrl.op_fcn  := rrd_cs.op_fcn.asUInt
   io.rrd_uop.ctrl.fcn_dw  := rrd_cs.fcn_dw.toBool
   io.rrd_uop.ctrl.is_load := io.rrd_uop.is_load
   io.rrd_uop.ctrl.is_sta  := io.rrd_uop.uopc === uopSTA || io.rrd_uop.uopc === uopAMO_AG || io.rrd_uop.uopc === uopVST
   io.rrd_uop.ctrl.is_std  := io.rrd_uop.uopc === uopSTD || (io.rrd_uop.ctrl.is_sta && io.rrd_uop.lrs2_rtype === RT_FIX)

   when (io.rrd_uop.uopc === uopAMO_AG)
   {
      io.rrd_uop.imm_packed := 0.U
   }

   val raddr1 = io.rrd_uop.pop1 // although renamed, it'll stay 0 if lrs1 = 0
   val csr_ren = (rrd_cs.csr_cmd === CSR.S || rrd_cs.csr_cmd === CSR.C) && raddr1 === 0.U
   io.rrd_uop.ctrl.csr_cmd := Mux(csr_ren, CSR.R, rrd_cs.csr_cmd)


   require (rrd_cs.op_fcn.getWidth == FN_SRA.getWidth)

   //-------------------------------------------------------------
   // set outputs

   io.rrd_valid := rrd_valid

}
