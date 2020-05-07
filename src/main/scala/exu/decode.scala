//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.rocket.ALU._
import freechips.rocketchip.rocket.RVCExpander
import freechips.rocketchip.rocket.{CSR, Causes, DecodeLogic}
import freechips.rocketchip.util.{uintToBitPat,UIntIsOneOf}

import FUConstants._
import boom.common._
import boom.util._

// scalastyle:off
/**
 * Abstract trait giving defaults and other relevant values to different Decode constants/
 */
object DecodeTables
  extends freechips.rocketchip.rocket.constants.ScalarOpConstants
  with freechips.rocketchip.rocket.constants.MemoryOpConstants
{
  val xpr64 = Y // TODO inform this from xLen
  val DC2 = BitPat.dontCare(2) // Makes the listing below more readable
  def decode_default: List[BitPat] =
              //            xs                                                              frs3_en
              //               is val inst?                                               |  imm sel                  bypassable (aka, known/fixed latency)
              //               |  is fp inst?                                             |  |     uses_ldq           |  is_br
              //               |  |                                       rs1 regtype     |  |     |  uses_stq        |  |  is unique? (clear pipeline for it)
              //               |  |  micro-code                           |       rs2 type|  |     |  |  is_amo       |  |  |  flush on commit
              //               |  |  |          iq-type  func unit        |       |       |  |     |  |  |            |  |  |  |  csr cmd
              //               |  |  |          |        |                |       |       |  |     |  |  |            |  |  |  |  |      fcn_dw
              //               |  |  |          |        |        dst     |       |       |  |     |  |  |  mem       |  |  |  |  |      |       fcn_op
              //               |  |  |          |        |        regtype |       |       |  |     |  |  |  cmd       |  |  |  |  |      |       |
              //               |  |  |          |        |        |       |       |       |  |     |  |  |  |         |  |  |  |  |      |       |
              //               |  |  |          |        |        |       |       |       |  |     |  |  |  |         |  |  |  |  |      |       |
                          List(N, N, uopX     , IQT_INT, FU_X   , RT_X  , DC2    ,DC2    ,X, IS_X, X, X, X, M_X,      X, X, N, X, CSR.X, DW_X  , FN_X  )

  val X32_table: Array[(BitPat, List[BitPat])] = Array(
    SLLI_RV32          -> List(Y, N, uopSLLI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SL  ),
    SRLI_RV32          -> List(Y, N, uopSRLI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SR  ),
    SRAI_RV32          -> List(Y, N, uopSRAI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SRA )
  )
  val X64_table: Array[(BitPat, List[BitPat])] = Array(
    LD                 -> List(Y, X, uopLD    , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, M_XRD   , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    LWU                -> List(Y, X, uopLD    , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, M_XRD   , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    SD                 -> List(Y, X, uopSTA   , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, M_XWR   , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    SLLI               -> List(Y, X, uopSLLI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SL  ),
    SRLI               -> List(Y, X, uopSRLI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SR  ),
    SRAI               -> List(Y, X, uopSRAI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SRA ),

    ADDIW              -> List(Y, X, uopADDIW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_32 , FN_ADD ),
    SLLIW              -> List(Y, X, uopSLLIW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_32 , FN_SL  ),
    SRAIW              -> List(Y, X, uopSRAIW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_32 , FN_SRA ),
    SRLIW              -> List(Y, X, uopSRLIW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_32 , FN_SR  ),

    ADDW               -> List(Y, X, uopADDW  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_32 , FN_ADD ),
    SUBW               -> List(Y, X, uopSUBW  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_32 , FN_SUB ),
    SLLW               -> List(Y, X, uopSLLW  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_32 , FN_SL  ),
    SRAW               -> List(Y, X, uopSRAW  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_32 , FN_SRA ),
    SRLW               -> List(Y, X, uopSRLW  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_32 , FN_SR  )
  )
  val X_table: Array[(BitPat, List[BitPat])] = Array(
    LW                 -> List(Y, N, uopLD    , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, M_XRD   , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    LH                 -> List(Y, N, uopLD    , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, M_XRD   , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    LHU                -> List(Y, N, uopLD    , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, M_XRD   , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    LB                 -> List(Y, N, uopLD    , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, M_XRD   , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    LBU                -> List(Y, N, uopLD    , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, M_XRD   , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    SW                 -> List(Y, N, uopSTA   , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, M_XWR   , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    SH                 -> List(Y, N, uopSTA   , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, M_XWR   , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    SB                 -> List(Y, N, uopSTA   , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, M_XWR   , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    LUI                -> List(Y, N, uopLUI   , IQT_INT, FU_ALU , RT_FIX, RT_X  , RT_X  , N, IS_U, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_ADD ),

    ADDI               -> List(Y, N, uopADDI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_ADD ),
    ANDI               -> List(Y, N, uopANDI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_AND ),
    ORI                -> List(Y, N, uopORI   , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_OR  ),
    XORI               -> List(Y, N, uopXORI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_XOR ),
    SLTI               -> List(Y, N, uopSLTI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SLT ),
    SLTIU              -> List(Y, N, uopSLTIU , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SLTU),

    SLL                -> List(Y, N, uopSLL   , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SL  ),
    ADD                -> List(Y, N, uopADD   , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_ADD ),
    SUB                -> List(Y, N, uopSUB   , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SUB ),
    SLT                -> List(Y, N, uopSLT   , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SLT ),
    SLTU               -> List(Y, N, uopSLTU  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SLTU),
    AND                -> List(Y, N, uopAND   , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_AND ),
    OR                 -> List(Y, N, uopOR    , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_OR  ),
    XOR                -> List(Y, N, uopXOR   , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_XOR ),
    SRA                -> List(Y, N, uopSRA   , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SRA ),
    SRL                -> List(Y, N, uopSRL   , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , Y, N, N, N, CSR.N, DW_XPR, FN_SR  ),

    MUL                -> List(Y, N, uopMUL   , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , N, N, N, N, CSR.N, DW_XPR, FN_MUL ),
    MULH               -> List(Y, N, uopMULH  , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , N, N, N, N, CSR.N, DW_XPR, FN_MULH),
    MULHU              -> List(Y, N, uopMULHU , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , N, N, N, N, CSR.N, DW_XPR, FN_MULHU),
    MULHSU             -> List(Y, N, uopMULHSU, IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , N, N, N, N, CSR.N, DW_XPR, FN_MULHSU),
    MULW               -> List(Y, N, uopMULW  , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , N, N, N, N, CSR.N, DW_32 , FN_MUL ),

    DIV                -> List(Y, N, uopDIV   , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , N, N, N, N, CSR.N, DW_XPR, FN_DIV ),
    DIVU               -> List(Y, N, uopDIVU  , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , N, N, N, N, CSR.N, DW_XPR, FN_DIVU),
    REM                -> List(Y, N, uopREM   , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , N, N, N, N, CSR.N, DW_XPR, FN_REM ),
    REMU               -> List(Y, N, uopREMU  , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , N, N, N, N, CSR.N, DW_XPR, FN_REMU),
    DIVW               -> List(Y, N, uopDIVW  , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , N, N, N, N, CSR.N, DW_32 , FN_DIV ),
    DIVUW              -> List(Y, N, uopDIVUW , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , N, N, N, N, CSR.N, DW_32 , FN_DIVU),
    REMW               -> List(Y, N, uopREMW  , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , N, N, N, N, CSR.N, DW_32 , FN_REM ),
    REMUW              -> List(Y, N, uopREMUW , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_N, N, N, N, M_X     , N, N, N, N, CSR.N, DW_32 , FN_REMU),

    AUIPC              -> List(Y, N, uopAUIPC , IQT_INT, FU_JMP , RT_FIX, RT_X  , RT_X  , N, IS_U, N, N, N, M_X     , N, N, N, N, CSR.N, DW_XPR, FN_ADD ), // use BRU for the PC read
    JAL                -> List(Y, N, uopJAL   , IQT_INT, FU_JMP , RT_FIX, RT_X  , RT_X  , N, IS_J, N, N, N, M_X     , N, N, N, N, CSR.N, DW_XPR, FN_ADD ),
    JALR               -> List(Y, N, uopJALR  , IQT_INT, FU_JMP , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_XPR, FN_ADD ),
    BEQ                -> List(Y, N, uopBEQ   , IQT_INT, FU_ALU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, M_X     , N, Y, N, N, CSR.N, DW_XPR, FN_SUB ),
    BNE                -> List(Y, N, uopBNE   , IQT_INT, FU_ALU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, M_X     , N, Y, N, N, CSR.N, DW_XPR, FN_SUB ),
    BGE                -> List(Y, N, uopBGE   , IQT_INT, FU_ALU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, M_X     , N, Y, N, N, CSR.N, DW_XPR, FN_SLT ),
    BGEU               -> List(Y, N, uopBGEU  , IQT_INT, FU_ALU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, M_X     , N, Y, N, N, CSR.N, DW_XPR, FN_SLTU),
    BLT                -> List(Y, N, uopBLT   , IQT_INT, FU_ALU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, M_X     , N, Y, N, N, CSR.N, DW_XPR, FN_SLT ),
    BLTU               -> List(Y, N, uopBLTU  , IQT_INT, FU_ALU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, M_X     , N, Y, N, N, CSR.N, DW_XPR, FN_SLTU),

    // I-type, the immedia2 holds the CSR register.
    CSRRW              -> List(Y, N, uopCSRRW , IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, Y, Y, CSR.W, DW_XPR, FN_ADD ),
    CSRRS              -> List(Y, N, uopCSRRS , IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, Y, Y, CSR.S, DW_XPR, FN_ADD ),
    CSRRC              -> List(Y, N, uopCSRRC , IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, Y, Y, CSR.C, DW_XPR, FN_ADD ),

    CSRRWI             -> List(Y, N, uopCSRRWI, IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , N, IS_I, N, N, N, M_X     , N, N, Y, Y, CSR.W, DW_XPR, FN_ADD ),
    CSRRSI             -> List(Y, N, uopCSRRSI, IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , N, IS_I, N, N, N, M_X     , N, N, Y, Y, CSR.S, DW_XPR, FN_ADD ),
    CSRRCI             -> List(Y, N, uopCSRRCI, IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , N, IS_I, N, N, N, M_X     , N, N, Y, Y, CSR.C, DW_XPR, FN_ADD ),

    SFENCE_VMA          ->List(Y, N, uopSFENCE, IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_X, N, N, N,M_SFENCE , N, N, Y, Y, CSR.N, DW_XPR, FN_ADD ),
    SCALL              -> List(Y, N, uopSCALL , IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, M_X     , N, N, Y, Y, CSR.I, DW_XPR, FN_ADD ),
    SBREAK             -> List(Y, N, uopSBREAK, IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, M_X     , N, N, Y, Y, CSR.I, DW_XPR, FN_ADD ),
    SRET               -> List(Y, N, uopSRET  , IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, M_X     , N, N, Y, Y, CSR.I, DW_XPR, FN_ADD ),
    MRET               -> List(Y, N, uopMRET  , IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, M_X     , N, N, Y, Y, CSR.I, DW_XPR, FN_ADD ),
    DRET               -> List(Y, N, uopDRET  , IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, M_X     , N, N, Y, Y, CSR.I, DW_XPR, FN_ADD ),

    WFI                -> List(Y, N, uopWFI   , IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, M_X     , N, N, Y, Y, CSR.I, DW_XPR, FN_ADD ),

    FENCE_I            -> List(Y, N, uopFENCEI, IQT_INT, FU_X   , RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, M_X     , N, N, Y, Y, CSR.N, DW_XPR, FN_ADD ),
    FENCE              -> List(Y, N, uopFENCE , IQT_INT, FU_MEM , RT_X  , RT_X  , RT_X  , N, IS_X, N, Y, N, M_X     , N, N, Y, Y, CSR.N, DW_XPR, FN_ADD ), // TODO PERF make fence higher performance
                                                                                                                                                  // currently serializes pipeline
    // A-type
    AMOADD_W           -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_ADD, N, N, Y, Y, CSR.N, DW_X  , FN_X   ), // TODO make AMOs higherperformance
    AMOXOR_W           -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_XOR, N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOSWAP_W          -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_SWAP,N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOAND_W           -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_AND, N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOOR_W            -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_OR,  N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOMIN_W           -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_MIN, N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOMINU_W          -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_MINU,N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOMAX_W           -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_MAX, N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOMAXU_W          -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_MAXU,N, N, Y, Y, CSR.N, DW_X  , FN_X   ),

    AMOADD_D           -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_ADD, N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOXOR_D           -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_XOR, N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOSWAP_D          -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_SWAP,N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOAND_D           -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_AND, N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOOR_D            -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_OR,  N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOMIN_D           -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_MIN, N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOMINU_D          -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_MINU,N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOMAX_D           -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_MAX, N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    AMOMAXU_D          -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XA_MAXU,N, N, Y, Y, CSR.N, DW_X  , FN_X   ),

    LR_W               -> List(Y, N, uopLD     , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_X  , N, IS_X, Y, N, N, M_XLR   , N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    LR_D               -> List(Y, N, uopLD     , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_X  , N, IS_X, Y, N, N, M_XLR   , N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    SC_W               -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XSC   , N, N, Y, Y, CSR.N, DW_X  , FN_X   ),
    SC_D               -> List(Y, N, uopAMO_AG , IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, M_XSC   , N, N, Y, Y, CSR.N, DW_X  , FN_X    )
  )
  val F_table: Array[(BitPat, List[BitPat])] = Array(
    FLW                -> List(Y, Y, uopLD     , IQT_MEM, FU_MEM, RT_FLT, RT_FIX, RT_X  , N, IS_I, Y, N, N, M_XRD   , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FLD                -> List(Y, Y, uopLD     , IQT_MEM, FU_MEM, RT_FLT, RT_FIX, RT_X  , N, IS_I, Y, N, N, M_XRD   , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FSW                -> List(Y, Y, uopSTA    , IQT_MFP,FU_F2IMEM,RT_X , RT_FIX, RT_FLT, N, IS_S, N, Y, N, M_XWR   , N, N, N, N, CSR.N, DW_X  , FN_X   ), // sort of a lie; broken into two micro-ops
    FSD                -> List(Y, Y, uopSTA    , IQT_MFP,FU_F2IMEM,RT_X , RT_FIX, RT_FLT, N, IS_S, N, Y, N, M_XWR   , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    FCLASS_S           -> List(Y, Y, uopFCLASS_S,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCLASS_D           -> List(Y, Y, uopFCLASS_D,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    FMV_S_X            -> List(Y, Y, uopFMV_S_X, IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FMV_D_X            -> List(Y, Y, uopFMV_D_X, IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FMV_X_S            -> List(Y, Y, uopFMV_X_S, IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FMV_X_D            -> List(Y, Y, uopFMV_X_D, IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    FSGNJ_S            -> List(Y, Y, uopFSGNJ_S, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FSGNJ_D            -> List(Y, Y, uopFSGNJ_D, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FSGNJX_S           -> List(Y, Y, uopFSGNJ_S, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FSGNJX_D           -> List(Y, Y, uopFSGNJ_D, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FSGNJN_S           -> List(Y, Y, uopFSGNJ_S, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FSGNJN_D           -> List(Y, Y, uopFSGNJ_D, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    // FP to FP
    FCVT_S_D           -> List(Y, Y, uopFCVT_S_D,IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCVT_D_S           -> List(Y, Y, uopFCVT_D_S,IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    // Int to FP
    FCVT_S_W           -> List(Y, Y, uopFCVT_S_X,IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCVT_S_WU          -> List(Y, Y, uopFCVT_S_X,IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCVT_S_L           -> List(Y, Y, uopFCVT_S_X,IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCVT_S_LU          -> List(Y, Y, uopFCVT_S_X,IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    FCVT_D_W           -> List(Y, Y, uopFCVT_D_X,IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCVT_D_WU          -> List(Y, Y, uopFCVT_D_X,IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCVT_D_L           -> List(Y, Y, uopFCVT_D_X,IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCVT_D_LU          -> List(Y, Y, uopFCVT_D_X,IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    // FP to Int
    FCVT_W_S           -> List(Y, Y, uopFCVT_X_S,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCVT_WU_S          -> List(Y, Y, uopFCVT_X_S,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCVT_L_S           -> List(Y, Y, uopFCVT_X_S,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCVT_LU_S          -> List(Y, Y, uopFCVT_X_S,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    FCVT_W_D           -> List(Y, Y, uopFCVT_X_D,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCVT_WU_D          -> List(Y, Y, uopFCVT_X_D,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCVT_L_D           -> List(Y, Y, uopFCVT_X_D,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FCVT_LU_D          -> List(Y, Y, uopFCVT_X_D,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),


    FEQ_S              -> List(Y, Y, uopCMPR_S , IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FLT_S              -> List(Y, Y, uopCMPR_S , IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FLE_S              -> List(Y, Y, uopCMPR_S , IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    FEQ_D              -> List(Y, Y, uopCMPR_D , IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FLT_D              -> List(Y, Y, uopCMPR_D , IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FLE_D              -> List(Y, Y, uopCMPR_D , IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    FMIN_S             -> List(Y, Y,uopFMINMAX_S,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FMAX_S             -> List(Y, Y,uopFMINMAX_S,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FMIN_D             -> List(Y, Y,uopFMINMAX_D,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FMAX_D             -> List(Y, Y,uopFMINMAX_D,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    FADD_S             -> List(Y, Y, uopFADD_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FSUB_S             -> List(Y, Y, uopFSUB_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FMUL_S             -> List(Y, Y, uopFMUL_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FADD_D             -> List(Y, Y, uopFADD_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FSUB_D             -> List(Y, Y, uopFSUB_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FMUL_D             -> List(Y, Y, uopFMUL_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),

    FMADD_S            -> List(Y, Y, uopFMADD_S, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FMSUB_S            -> List(Y, Y, uopFMSUB_S, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FNMADD_S           -> List(Y, Y, uopFNMADD_S,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FNMSUB_S           -> List(Y, Y, uopFNMSUB_S,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FMADD_D            -> List(Y, Y, uopFMADD_D, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FMSUB_D            -> List(Y, Y, uopFMSUB_D, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FNMADD_D           -> List(Y, Y, uopFNMADD_D,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FNMSUB_D           -> List(Y, Y, uopFNMSUB_D,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   )
  )
  val FDivSqrt_table: Array[(BitPat, List[BitPat])] = Array(
    FDIV_S             -> List(Y, Y, uopFDIV_S , IQT_FP,  FU_FDV, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FDIV_D             -> List(Y, Y, uopFDIV_D , IQT_FP,  FU_FDV, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FSQRT_S            -> List(Y, Y, uopFSQRT_S, IQT_FP,  FU_FDV, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    FSQRT_D            -> List(Y, Y, uopFSQRT_D, IQT_FP,  FU_FDV, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   )
  )
  val RoCC_table: Array[(BitPat, List[BitPat])] = Array(
  // Note: We use FU_CSR since CSR instructions cannot co-execute with RoCC instructions
    CUSTOM0            -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM0_RS1        -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM0_RS1_RS2    -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM0_RD         -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_X  , RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM0_RD_RS1     -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM0_RD_RS1_RS2 -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM1            -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM1_RS1        -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM1_RS1_RS2    -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM1_RD         -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_X  , RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM1_RD_RS1     -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM1_RD_RS1_RS2 -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM2            -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM2_RS1        -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM2_RS1_RS2    -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM2_RD         -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_X  , RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM2_RD_RS1     -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM2_RD_RS1_RS2 -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM3            -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM3_RS1        -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM3_RS1_RS2    -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM3_RD         -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_X  , RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM3_RD_RS1     -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   ),
    CUSTOM3_RD_RS1_RS2 -> List(Y, N, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, M_X     , N, N, N, N, CSR.N, DW_X  , FN_X   )
  )
}
// scalastyle:on



/**
 * Decoded control signals
 */
class CtrlSigs extends Bundle
{
  val legal           = Bool()
  val fp_val          = Bool()
  val uopc            = UInt(UOPC_SZ.W)
  val iq_type         = UInt(IQT_SZ.W)
  val fu_code         = UInt(FUC_SZ.W)
  val dst_type        = UInt(2.W)
  val rs1_type        = UInt(2.W)
  val rs2_type        = UInt(2.W)
  val frs3_en         = Bool()
  val imm_sel         = UInt(IS_X.getWidth.W)
  val uses_ldq        = Bool()
  val uses_stq        = Bool()
  val is_amo          = Bool()
  val mem_cmd         = UInt(freechips.rocketchip.rocket.M_SZ.W)
  val bypassable      = Bool()
  val is_br           = Bool()
  val inst_unique     = Bool()
  val flush_on_commit = Bool()
  val csr_cmd         = UInt(freechips.rocketchip.rocket.CSR.SZ.W)
  val fcn_dw          = Bool()
  val fcn_op          = UInt(freechips.rocketchip.rocket.ALU.SZ_ALU_FN.W)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, DecodeTables.decode_default, table)
    val sigs =
      Seq(legal, fp_val, uopc, iq_type, fu_code, dst_type, rs1_type,
          rs2_type, frs3_en, imm_sel, uses_ldq, uses_stq, is_amo,
          mem_cmd, bypassable,
          is_br, inst_unique, flush_on_commit, csr_cmd,
          fcn_dw, fcn_op
      )
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}





/**
 * IO bundle for the Decode unit
 */
class DecodeUnitIo(implicit p: Parameters) extends BoomBundle
{
  val enq = new Bundle { val uop = Input(new MicroOp()) }
  val deq = new Bundle { val uop = Output(new MicroOp()) }

  // from CSRFile
  val status = Input(new freechips.rocketchip.rocket.MStatus())
  val csr_decode = Flipped(new freechips.rocketchip.rocket.CSRDecodeIO)
  val interrupt = Input(Bool())
  val interrupt_cause = Input(UInt(xLen.W))
}

/**
 * Decode unit that takes in a single instruction and generates a MicroOp.
 */
class DecodeUnit(implicit p: Parameters) extends BoomModule
  with freechips.rocketchip.rocket.constants.MemoryOpConstants
  with freechips.rocketchip.rocket.constants.ScalarOpConstants
{
  val io = IO(new DecodeUnitIo)

  val uop = Wire(new MicroOp())
  uop := io.enq.uop

  var decode_table = DecodeTables.X_table
  if (usingFPU) decode_table ++= DecodeTables.F_table
  if (usingFPU && usingFDivSqrt) decode_table ++= DecodeTables.FDivSqrt_table
  if (usingRoCC) decode_table ++= DecodeTables.RoCC_table
  decode_table ++= (if (xLen == 64) DecodeTables.X64_table else DecodeTables.X64_table)

  val inst = uop.inst

  val cs = Wire(new CtrlSigs()).decode(inst, decode_table)

  // Exception Handling
  io.csr_decode.csr := inst(31,20)
  val csr_en = cs.csr_cmd.isOneOf(CSR.S, CSR.C, CSR.W)
  val csr_ren = cs.csr_cmd.isOneOf(CSR.S, CSR.C) && uop.lrs1 === 0.U
  val system_insn = cs.csr_cmd === CSR.I
  val sfence = inst === SFENCE_VMA

  val cs_legal = cs.legal
//   dontTouch(cs_legal)

  require (fLen >= 64)
  val id_illegal_insn = !cs_legal ||
    cs.fp_val && io.csr_decode.fp_illegal || // TODO check for illegal rm mode: (io.fpu.illegal_rm)
    cs.uopc === uopROCC && io.csr_decode.rocc_illegal ||
    cs.is_amo && !io.status.isa('a'-'a')  ||
    csr_en && (io.csr_decode.read_illegal || !csr_ren && io.csr_decode.write_illegal) ||
    ((sfence || system_insn) && io.csr_decode.system_illegal)

//     cs.div && !csr.io.status.isa('m'-'a') || TODO check for illegal div instructions

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))

  val (xcpt_valid, xcpt_cause) = checkExceptions(List(
    (io.interrupt && !io.enq.uop.is_sfb, io.interrupt_cause),  // Disallow interrupts while we are handling a SFB
    (uop.bp_debug_if,                    (CSR.debugTriggerCause).U),
    (uop.bp_xcpt_if,                     (Causes.breakpoint).U),
    (uop.xcpt_pf_if,                     (Causes.fetch_page_fault).U),
    (uop.xcpt_ae_if,                     (Causes.fetch_access).U),
    (id_illegal_insn,                    (Causes.illegal_instruction).U)))

  uop.exception := xcpt_valid
  uop.exc_cause := xcpt_cause

  //-------------------------------------------------------------

  uop.uopc       := cs.uopc
  uop.iq_type    := cs.iq_type
  uop.fu_code    := cs.fu_code

  // x-registers placed in 0-31, f-registers placed in 32-63.
  // This allows us to straight-up compare register specifiers and not need to
  // verify the rtypes (e.g., bypassing in rename).
  val LDST = inst(RD_MSB,RD_LSB)
  val LRS1 = inst(RS1_MSB,RS1_LSB)
  val LRS2 = inst(RS2_MSB,RS2_LSB)
  val LRS3 = inst(RS3_MSB,RS3_LSB)

  uop.ldst       := LDST
  uop.lrs1       := LRS1
  uop.lrs2       := LRS2
  uop.lrs3       := LRS3

  uop.ldst_val   := cs.dst_type =/= RT_X && !(uop.ldst === 0.U && uop.dst_rtype === RT_FIX)
  uop.dst_rtype  := cs.dst_type
  uop.lrs1_rtype := cs.rs1_type
  uop.lrs2_rtype := cs.rs2_type
  uop.frs3_en    := cs.frs3_en

  uop.ldst_is_rs1 := uop.is_sfb_shadow
  // SFB optimization
  uop.is_mov      := false.B
  when (uop.is_sfb_shadow && cs.rs2_type === RT_X) {
    uop.lrs2_rtype  := RT_FIX
    uop.lrs2        := LDST
    uop.ldst_is_rs1 := false.B
  } .elsewhen (uop.is_sfb_shadow && cs.uopc === uopADD && LRS1 === 0.U) {
    uop.uopc        := uopMOV
    uop.is_mov      := true.B
    uop.lrs1        := LDST
    uop.ldst_is_rs1 := true.B
  }
  when (uop.is_sfb_br) {
    uop.fu_code := FU_JMP
  }


  uop.fp_val     := cs.fp_val

  uop.mem_cmd    := cs.mem_cmd
  uop.mem_size   := Mux(cs.mem_cmd.isOneOf(M_SFENCE, M_FLUSH_ALL), Cat(LRS2 =/= 0.U, LRS1 =/= 0.U), inst(13,12))
  uop.mem_signed := !inst(14)
  uop.uses_ldq   := cs.uses_ldq
  uop.uses_stq   := cs.uses_stq
  uop.is_amo     := cs.is_amo
  uop.is_fence   := inst === FENCE
  uop.is_fencei  := inst === FENCE_I
  uop.is_sys_pc2epc := inst === SBREAK || inst === SCALL
  uop.is_eret    := inst === SCALL || inst === SBREAK || inst === SRET || inst === MRET || inst === DRET
  uop.is_unique  := cs.inst_unique
  uop.is_rocc    := cs.uopc === uopROCC
  uop.flush_on_commit := cs.flush_on_commit || (csr_en && !csr_ren && io.csr_decode.write_flush)

  uop.bypassable   := cs.bypassable

  //-------------------------------------------------------------
  // immediates

  // repackage the immediate, and then pass the fewest number of bits around
  val di24_20 = Mux(cs.imm_sel === IS_B || cs.imm_sel === IS_S, inst(11,7), inst(24,20))
  uop.imm_packed := Cat(inst(31,25), di24_20, inst(19,12))
  when (cs.uopc === uopAMO_AG || (cs.uopc === uopLD && cs.mem_cmd === M_XLR)) {
    uop.imm_packed := 0.U
  }
  uop.imm_sel := cs.imm_sel

  //-------------------------------------------------------------

  uop.is_br          := cs.is_br
  uop.is_jal         := inst === JAL
  uop.is_jalr        := inst === JALR
  // uop.is_jump        := cs.is_jal || (uop.uopc === uopJALR)
  // uop.is_ret         := (uop.uopc === uopJALR) &&
  //                       (uop.ldst === X0) &&
  //                       (uop.lrs1 === RA)
  // uop.is_call        := (uop.uopc === uopJALR || uop.uopc === uopJAL) &&
  //                       (uop.ldst === RA)

  //-------------------------------------------------------------


  uop.csr_cmd := cs.csr_cmd
  when ((cs.csr_cmd === CSR.S || cs.csr_cmd === CSR.C) && LRS1 === 0.U) {
    uop.csr_cmd := CSR.R
  }

  uop.fcn_dw := cs.fcn_dw
  uop.fcn_op := cs.fcn_op

  uop.op1_sel := OP1_RS1
  when (inst === LUI || inst === CSRRWI || inst === CSRRSI || inst === CSRRCI ||
    inst === WFI || inst === SRET || inst === MRET || inst === DRET) {
    uop.op1_sel := OP1_ZERO
  } .elsewhen (inst === JAL || inst === JALR || inst === AUIPC) {
    uop.op1_sel := OP1_PC
  }

  uop.op2_sel := OP2_RS2
  when (cs.is_amo || inst === CSRRW || inst === CSRRS || inst === CSRRC) {
    uop.op2_sel := OP2_ZERO
  } .elsewhen (inst === CSRRWI || inst === CSRRSI || inst === CSRRCI ||
    inst === WFI || inst === SRET || inst === DRET || inst === MRET) {
    uop.op2_sel := OP2_IMMC
  } .elsewhen (inst === JAL || inst === JALR) {
    uop.op2_sel := OP2_NEXT
  } .elsewhen (cs.imm_sel === IS_U || cs.imm_sel === IS_I || cs.imm_sel === IS_S) {
    uop.op2_sel := OP2_IMM
  }

  uop.br_type := Mux(inst === BEQ , B_EQ,
                 Mux(inst === BNE , B_NE,
                 Mux(inst === BGE , B_GE,
                 Mux(inst === BGEU, B_GEU,
                 Mux(inst === BLT , B_LT,
                 Mux(inst === BLTU, B_LTU,
                 Mux(inst === JAL , B_J,
                 Mux(inst === JALR, B_JR,
                                    DontCare))))))))


  io.deq.uop := uop
}

/**
 * Smaller Decode unit for the Frontend to decode different
 * branches.
 * Accepts EXPANDED RVC instructions
  */

class BranchDecodeSignals(implicit p: Parameters) extends BoomBundle
{
  val is_ret   = Bool()
  val is_call  = Bool()
  val target   = UInt(vaddrBitsExtended.W)
  val cfi_type = UInt(CFI_SZ.W)


  // Is this branch a short forwards jump?
  val sfb_offset = Valid(UInt(log2Ceil(icBlockBytes).W))
  // Is this instruction allowed to be inside a sfb?
  val shadowable = Bool()
}

class BranchDecode(implicit p: Parameters) extends BoomModule
{
  val io = IO(new Bundle {
    val inst    = Input(UInt(32.W))
    val pc      = Input(UInt(vaddrBitsExtended.W))

    val out = Output(new BranchDecodeSignals)
  })

  val bpd_csignals =
    freechips.rocketchip.rocket.DecodeLogic(io.inst,
                  List[BitPat](N, N, N, N, X),
////                               is br?
////                               |  is jal?
////                               |  |  is jalr?
////                               |  |  |
////                               |  |  |  shadowable
////                               |  |  |  |  has_rs2
////                               |  |  |  |  |
            Array[(BitPat, List[BitPat])](
               JAL         -> List(N, Y, N, N, X),
               JALR        -> List(N, N, Y, N, X),
               BEQ         -> List(Y, N, N, N, X),
               BNE         -> List(Y, N, N, N, X),
               BGE         -> List(Y, N, N, N, X),
               BGEU        -> List(Y, N, N, N, X),
               BLT         -> List(Y, N, N, N, X),
               BLTU        -> List(Y, N, N, N, X),

               SLLI        -> List(N, N, N, Y, N),
               SRLI        -> List(N, N, N, Y, N),
               SRAI        -> List(N, N, N, Y, N),

               ADDIW       -> List(N, N, N, Y, N),
               SLLIW       -> List(N, N, N, Y, N),
               SRAIW       -> List(N, N, N, Y, N),
               SRLIW       -> List(N, N, N, Y, N),

               ADDW        -> List(N, N, N, Y, Y),
               SUBW        -> List(N, N, N, Y, Y),
               SLLW        -> List(N, N, N, Y, Y),
               SRAW        -> List(N, N, N, Y, Y),
               SRLW        -> List(N, N, N, Y, Y),

               LUI         -> List(N, N, N, Y, N),

               ADDI        -> List(N, N, N, Y, N),
               ANDI        -> List(N, N, N, Y, N),
               ORI         -> List(N, N, N, Y, N),
               XORI        -> List(N, N, N, Y, N),
               SLTI        -> List(N, N, N, Y, N),
               SLTIU       -> List(N, N, N, Y, N),

               SLL         -> List(N, N, N, Y, Y),
               ADD         -> List(N, N, N, Y, Y),
               SUB         -> List(N, N, N, Y, Y),
               SLT         -> List(N, N, N, Y, Y),
               SLTU        -> List(N, N, N, Y, Y),
               AND         -> List(N, N, N, Y, Y),
               OR          -> List(N, N, N, Y, Y),
               XOR         -> List(N, N, N, Y, Y),
               SRA         -> List(N, N, N, Y, Y),
               SRL         -> List(N, N, N, Y, Y)
            ))

  val (cs_is_br: Bool) :: (cs_is_jal: Bool) :: (cs_is_jalr:Bool) :: (cs_is_shadowable:Bool) :: (cs_has_rs2) :: Nil = bpd_csignals

  io.out.is_call := (cs_is_jal || cs_is_jalr) && GetRd(io.inst) === RA
  io.out.is_ret  := cs_is_jalr && GetRs1(io.inst) === BitPat("b00?01") && GetRd(io.inst) === X0

  io.out.target := Mux(cs_is_br, ComputeBranchTarget(io.pc, io.inst, xLen),
                                 ComputeJALTarget(io.pc, io.inst, xLen))
  io.out.cfi_type :=
    Mux(cs_is_jalr,
      CFI_JALR,
    Mux(cs_is_jal,
      CFI_JAL,
    Mux(cs_is_br,
      CFI_BR,
      CFI_X)))

  val br_offset = Cat(io.inst(7), io.inst(30,25), io.inst(11,8), 0.U(1.W))
  // Is a sfb if it points forwards (offset is positive)
  io.out.sfb_offset.valid := cs_is_br && !io.inst(31) && br_offset =/= 0.U && (br_offset >> log2Ceil(icBlockBytes)) === 0.U
  io.out.sfb_offset.bits  := br_offset
  io.out.shadowable := cs_is_shadowable && (
    !cs_has_rs2 ||
    (GetRs1(io.inst) === GetRd(io.inst)) ||
    (io.inst === ADD && GetRs1(io.inst) === X0)
  )
}

/**
 * Track the current "branch mask", and give out the branch mask to each micro-op in Decode
 * (each micro-op in the machine has a branch mask which says which branches it
 * is being speculated under).
 *
 * @param pl_width pipeline width for the processor
 */
class BranchMaskGenerationLogic(val pl_width: Int)(implicit p: Parameters) extends BoomModule
{
  val io = IO(new Bundle {
    // guess if the uop is a branch (we'll catch this later)
    val is_branch = Input(Vec(pl_width, Bool()))
    // lock in that it's actually a branch and will fire, so we update
    // the branch_masks.
    val will_fire = Input(Vec(pl_width, Bool()))

    // give out tag immediately (needed in rename)
    // mask can come later in the cycle
    val br_tag    = Output(Vec(pl_width, UInt(brTagSz.W)))
    val br_mask   = Output(Vec(pl_width, UInt(maxBrCount.W)))

     // tell decoders the branch mask has filled up, but on the granularity
     // of an individual micro-op (so some micro-ops can go through)
    val is_full   = Output(Vec(pl_width, Bool()))

    val brupdate         = Input(new BrUpdateInfo())
    val flush_pipeline = Input(Bool())

    val debug_branch_mask = Output(UInt(maxBrCount.W))
  })

  val branch_mask = RegInit(0.U(maxBrCount.W))

  //-------------------------------------------------------------
  // Give out the branch tag to each branch micro-op

  var allocate_mask = branch_mask
  val tag_masks = Wire(Vec(pl_width, UInt(maxBrCount.W)))

  for (w <- 0 until pl_width) {
    // TODO this is a loss of performance as we're blocking branches based on potentially fake branches
    io.is_full(w) := (allocate_mask === ~(0.U(maxBrCount.W))) && io.is_branch(w)

    // find br_tag and compute next br_mask
    val new_br_tag = Wire(UInt(brTagSz.W))
    new_br_tag := 0.U
    tag_masks(w) := 0.U

    for (i <- maxBrCount-1 to 0 by -1) {
      when (~allocate_mask(i)) {
        new_br_tag := i.U
        tag_masks(w) := (1.U << i.U)
      }
    }

    io.br_tag(w) := new_br_tag
    allocate_mask = Mux(io.is_branch(w), tag_masks(w) | allocate_mask, allocate_mask)
  }

  //-------------------------------------------------------------
  // Give out the branch mask to each micro-op
  // (kill off the bits that corresponded to branches that aren't going to fire)

  var curr_mask = branch_mask
  for (w <- 0 until pl_width) {
    io.br_mask(w) := GetNewBrMask(io.brupdate, curr_mask)
    curr_mask = Mux(io.will_fire(w), tag_masks(w) | curr_mask, curr_mask)
  }

  //-------------------------------------------------------------
  // Update the current branch_mask

  when (io.flush_pipeline) {
    branch_mask := 0.U
  } .otherwise {
    val mask = Mux(io.brupdate.b2.mispredict,
      io.brupdate.b2.uop.br_mask,
      ~(0.U(maxBrCount.W)))
    branch_mask := GetNewBrMask(io.brupdate, curr_mask) & mask
  }

  io.debug_branch_mask := branch_mask
}
