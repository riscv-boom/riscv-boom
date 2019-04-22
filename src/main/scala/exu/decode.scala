//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.rocket.RVCExpander
import freechips.rocketchip.rocket.{CSR,Causes}
import freechips.rocketchip.util.{uintToBitPat,UIntIsOneOf}

import FUConstants._
import boom.common._
import boom.util._

// scalastyle:off
/**
 * Abstract trait giving defaults and other relevant values to different Decode constants/
 */
abstract trait DecodeConstants
  extends freechips.rocketchip.rocket.constants.ScalarOpConstants
  with freechips.rocketchip.rocket.constants.MemoryOpConstants
{
  val xpr64 = Y // TODO inform this from xLen
  val DC2 = BitPat.dontCare(2) // Makes the listing below more readable
  def decode_default: List[BitPat] =
            //                                                                  frs3_en                               wakeup_delay
            //     is val inst?                                                 |  imm sel                            |    bypassable (aka, known/fixed latency)
            //     |  is fp inst?                                               |  |     is_load                      |    |  br/jmp
            //     |  |  is single-prec?                        rs1 regtype     |  |     |  is_store                  |    |  |  is jal
            //     |  |  |  micro-code                          |       rs2 type|  |     |  |  is_amo                 |    |  |  |  allocate_brtag
            //     |  |  |  |         iq-type  func unit        |       |       |  |     |  |  |  is_fence            |    |  |  |  |
            //     |  |  |  |         |        |                |       |       |  |     |  |  |  |  is_fencei        |    |  |  |  |  is breakpoint or ecall?
            //     |  |  |  |         |        |        dst     |       |       |  |     |  |  |  |  |  mem    mem    |    |  |  |  |  |  is unique? (clear pipeline for it)
            //     |  |  |  |         |        |        regtype |       |       |  |     |  |  |  |  |  cmd    msk    |    |  |  |  |  |  |  flush on commit
            //     |  |  |  |         |        |        |       |       |       |  |     |  |  |  |  |  |      |      |    |  |  |  |  |  |  |  csr cmd
            //     |  |  |  |         |        |        |       |       |       |  |     |  |  |  |  |  |      |      |    |  |  |  |  |  |  |  |
              List(N, N, X, uopX    , IQT_INT, FU_X   , RT_X  , DC2    ,DC2    ,X, IS_X, X, X, X, X, N, M_X,   MT_X,  DC2, X, X, X, X, N, N, X, CSR.X)

  val table: Array[(BitPat, List[BitPat])]
}
// scalastyle:on

/**
 * Decoded control signals
 */
class CtrlSigs extends Bundle
{
  val legal           = Bool()
  val fp_val          = Bool()
  val fp_single       = Bool()
  val uopc            = UInt(UOPC_SZ.W)
  val iq_type         = UInt(IQT_SZ.W)
  val fu_code         = UInt(FUC_SZ.W)
  val dst_type        = UInt(2.W)
  val rs1_type        = UInt(2.W)
  val rs2_type        = UInt(2.W)
  val frs3_en         = Bool()
  val imm_sel         = UInt(IS_X.getWidth.W)
  val is_load         = Bool()
  val is_store        = Bool()
  val is_amo          = Bool()
  val is_fence        = Bool()
  val is_fencei       = Bool()
  val mem_cmd         = UInt(freechips.rocketchip.rocket.M_SZ.W)
  val mem_typ         = UInt(freechips.rocketchip.rocket.MT_SZ.W)
  val wakeup_delay    = UInt(2.W)
  val bypassable      = Bool()
  val br_or_jmp       = Bool()
  val is_jal          = Bool()
  val allocate_brtag  = Bool()
  val is_sys_pc2epc   = Bool()
  val inst_unique     = Bool()
  val flush_on_commit = Bool()
  val csr_cmd         = UInt(freechips.rocketchip.rocket.CSR.SZ.W)
  val rocc            = Bool()

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, XDecode.decode_default, table)
    val sigs =
      Seq(legal, fp_val, fp_single, uopc, iq_type, fu_code, dst_type, rs1_type,
          rs2_type, frs3_en, imm_sel, is_load, is_store, is_amo,
          is_fence, is_fencei, mem_cmd, mem_typ, wakeup_delay, bypassable,
          br_or_jmp, is_jal, allocate_brtag, is_sys_pc2epc, inst_unique, flush_on_commit, csr_cmd)
      sigs zip decoder map {case(s,d) => s := d}
      rocc := false.B
      this
  }
}

// scalastyle:off
/**
 * Decode constants for RV32
 */
object X32Decode extends DecodeConstants
{
            //                                                                  frs3_en                               wakeup_delay
            //     is val inst?                                                 |  imm sel                            |    bypassable (aka, known/fixed latency)
            //     |  is fp inst?                                               |  |     is_load                      |    |  br/jmp
            //     |  |  is single-prec?                        rs1 regtype     |  |     |  is_store                  |    |  |  is jal
            //     |  |  |  micro-code                          |       rs2 type|  |     |  |  is_amo                 |    |  |  |  allocate_brtag
            //     |  |  |  |         iq-type  func unit        |       |       |  |     |  |  |  is_fence            |    |  |  |  |
            //     |  |  |  |         |        |                |       |       |  |     |  |  |  |  is_fencei        |    |  |  |  |  is breakpoint or ecall?
            //     |  |  |  |         |        |        dst     |       |       |  |     |  |  |  |  |  mem    mem    |    |  |  |  |  |  is unique? (clear pipeline for it)
            //     |  |  |  |         |        |        regtype |       |       |  |     |  |  |  |  |  cmd    msk    |    |  |  |  |  |  |  flush on commit
            //     |  |  |  |         |        |        |       |       |       |  |     |  |  |  |  |  |      |      |    |  |  |  |  |  |  |  csr cmd
  val table: Array[(BitPat, List[BitPat])] = Array(//   |       |       |       |  |     |  |  |  |  |  |      |      |    |  |  |  |  |  |  |  |
  SLLI_RV32-> List(Y, N, X, uopSLLI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SRLI_RV32-> List(Y, N, X, uopSRLI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SRAI_RV32-> List(Y, N, X, uopSRAI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N)
  )
}

/**
 * Decode constants for RV64
 */
object X64Decode extends DecodeConstants
{
           //                                                                  frs3_en                               wakeup_delay
           //     is val inst?                                                 |  imm sel                            |    bypassable (aka, known/fixed latency)
           //     |  is fp inst?                                               |  |     is_load                      |    |  br/jmp
           //     |  |  is single-prec?                        rs1 regtype     |  |     |  is_store                  |    |  |  is jal
           //     |  |  |  micro-code                          |       rs2 type|  |     |  |  is_amo                 |    |  |  |  allocate_brtag
           //     |  |  |  |         iq-type  func unit        |       |       |  |     |  |  |  is_fence            |    |  |  |  |
           //     |  |  |  |         |        |                |       |       |  |     |  |  |  |  is_fencei        |    |  |  |  |  is breakpoint or ecall?
           //     |  |  |  |         |        |        dst     |       |       |  |     |  |  |  |  |  mem    mem    |    |  |  |  |  |  is unique? (clear pipeline for it)
           //     |  |  |  |         |        |        regtype |       |       |  |     |  |  |  |  |  cmd    msk    |    |  |  |  |  |  |  flush on commit
           //     |  |  |  |         |        |        |       |       |       |  |     |  |  |  |  |  |      |      |    |  |  |  |  |  |  |  csr cmd
  val table: Array[(BitPat, List[BitPat])] = Array(//  |       |       |       |  |     |  |  |  |  |  |      |      |    |  |  |  |  |  |  |  |
  LD      -> List(Y, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_D , 3.U, N, N, N, N, N, N, N, CSR.N),
  LWU     -> List(Y, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_WU, 3.U, N, N, N, N, N, N, N, CSR.N),
  SD      -> List(Y, N, X, uopSTA  , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, N, N, M_XWR, MT_D , 0.U, N, N, N, N, N, N, N, CSR.N),

  SLLI    -> List(Y, N, X, uopSLLI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SRLI    -> List(Y, N, X, uopSRLI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SRAI    -> List(Y, N, X, uopSRAI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),

  ADDIW   -> List(Y, N, X, uopADDIW, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SLLIW   -> List(Y, N, X, uopSLLIW, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SRAIW   -> List(Y, N, X, uopSRAIW, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SRLIW   -> List(Y, N, X, uopSRLIW, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),

  ADDW    -> List(Y, N, X, uopADDW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SUBW    -> List(Y, N, X, uopSUBW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SLLW    -> List(Y, N, X, uopSLLW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SRAW    -> List(Y, N, X, uopSRAW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SRLW    -> List(Y, N, X, uopSRLW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N)
  )
}

/**
 * Overall Decode constants
 */
object XDecode extends DecodeConstants
{
           //                                                                  frs3_en                               wakeup_delay
           //     is val inst?                                                 |  imm sel                            |    bypassable (aka, known/fixed latency)
           //     |  is fp inst?                                               |  |     is_load                      |    |  br/jmp
           //     |  |  is single-prec?                        rs1 regtype     |  |     |  is_store                  |    |  |  is jal
           //     |  |  |  micro-code                          |       rs2 type|  |     |  |  is_amo                 |    |  |  |  allocate_brtag
           //     |  |  |  |         iq-type  func unit        |       |       |  |     |  |  |  is_fence            |    |  |  |  |
           //     |  |  |  |         |        |                |       |       |  |     |  |  |  |  is_fencei        |    |  |  |  |  is breakpoint or ecall?
           //     |  |  |  |         |        |        dst     |       |       |  |     |  |  |  |  |  mem    mem    |    |  |  |  |  |  is unique? (clear pipeline for it)
           //     |  |  |  |         |        |        regtype |       |       |  |     |  |  |  |  |  cmd    msk    |    |  |  |  |  |  |  flush on commit
           //     |  |  |  |         |        |        |       |       |       |  |     |  |  |  |  |  |      |      |    |  |  |  |  |  |  |  csr cmd
  val table: Array[(BitPat, List[BitPat])] = Array(//  |       |       |       |  |     |  |  |  |  |  |      |      |    |  |  |  |  |  |  |  |
  LW      -> List(Y, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_W , 3.U, N, N, N, N, N, N, N, CSR.N),
  LH      -> List(Y, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_H , 3.U, N, N, N, N, N, N, N, CSR.N),
  LHU     -> List(Y, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_HU, 3.U, N, N, N, N, N, N, N, CSR.N),
  LB      -> List(Y, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_B , 3.U, N, N, N, N, N, N, N, CSR.N),
  LBU     -> List(Y, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_BU, 3.U, N, N, N, N, N, N, N, CSR.N),

  SW      -> List(Y, N, X, uopSTA  , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, N, N, M_XWR, MT_W , 0.U, N, N, N, N, N, N, N, CSR.N),
  SH      -> List(Y, N, X, uopSTA  , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, N, N, M_XWR, MT_H , 0.U, N, N, N, N, N, N, N, CSR.N),
  SB      -> List(Y, N, X, uopSTA  , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, N, N, M_XWR, MT_B , 0.U, N, N, N, N, N, N, N, CSR.N),

  LUI     -> List(Y, N, X, uopLUI  , IQT_INT, FU_ALU , RT_FIX, RT_X  , RT_X  , N, IS_U, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),

  ADDI    -> List(Y, N, X, uopADDI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  ANDI    -> List(Y, N, X, uopANDI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  ORI     -> List(Y, N, X, uopORI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  XORI    -> List(Y, N, X, uopXORI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SLTI    -> List(Y, N, X, uopSLTI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SLTIU   -> List(Y, N, X, uopSLTIU, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),

  SLL     -> List(Y, N, X, uopSLL  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  ADD     -> List(Y, N, X, uopADD  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SUB     -> List(Y, N, X, uopSUB  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SLT     -> List(Y, N, X, uopSLT  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SLTU    -> List(Y, N, X, uopSLTU , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  AND     -> List(Y, N, X, uopAND  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  OR      -> List(Y, N, X, uopOR   , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  XOR     -> List(Y, N, X, uopXOR  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SRA     -> List(Y, N, X, uopSRA  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),
  SRL     -> List(Y, N, X, uopSRL  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 1.U, Y, N, N, N, N, N, N, CSR.N),

  MUL     -> List(Y, N, X, uopMUL  , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  MULH    -> List(Y, N, X, uopMULH , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  MULHU   -> List(Y, N, X, uopMULHU, IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  MULHSU  -> List(Y, N, X, uopMULHSU,IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  MULW    -> List(Y, N, X, uopMULW , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  DIV     -> List(Y, N, X, uopDIV  , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  DIVU    -> List(Y, N, X, uopDIVU , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  REM     -> List(Y, N, X, uopREM  , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  REMU    -> List(Y, N, X, uopREMU , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  DIVW    -> List(Y, N, X, uopDIVW , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  DIVUW   -> List(Y, N, X, uopDIVUW, IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  REMW    -> List(Y, N, X, uopREMW , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  REMUW   -> List(Y, N, X, uopREMUW, IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  AUIPC   -> List(Y, N, X, uopAUIPC, IQT_INT, FU_BRU , RT_FIX, RT_X  , RT_X  , N, IS_U, N, N, N, N, N, M_X  , MT_X , 1.U, N, N, N, N, N, N, N, CSR.N), // use BRU for the PC read
  JAL     -> List(Y, N, X, uopJAL  , IQT_INT, FU_BRU , RT_FIX, RT_X  , RT_X  , N, IS_J, N, N, N, N, N, M_X  , MT_X , 1.U, N, Y, Y, N, N, N, N, CSR.N),
  JALR    -> List(Y, N, X, uopJALR , IQT_INT, FU_BRU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 1.U, N, Y, N, Y, N, N, N, CSR.N),
  BEQ     -> List(Y, N, X, uopBEQ  , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MT_X , 0.U, N, Y, N, Y, N, N, N, CSR.N),
  BNE     -> List(Y, N, X, uopBNE  , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MT_X , 0.U, N, Y, N, Y, N, N, N, CSR.N),
  BGE     -> List(Y, N, X, uopBGE  , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MT_X , 0.U, N, Y, N, Y, N, N, N, CSR.N),
  BGEU    -> List(Y, N, X, uopBGEU , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MT_X , 0.U, N, Y, N, Y, N, N, N, CSR.N),
  BLT     -> List(Y, N, X, uopBLT  , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MT_X , 0.U, N, Y, N, Y, N, N, N, CSR.N),
  BLTU    -> List(Y, N, X, uopBLTU , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MT_X , 0.U, N, Y, N, Y, N, N, N, CSR.N),

  // I-type, the immediate12 holds the CSR register.
  CSRRW   -> List(Y, N, X, uopCSRRW, IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, Y, Y, CSR.W),
  CSRRS   -> List(Y, N, X, uopCSRRS, IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, Y, Y, CSR.S),
  CSRRC   -> List(Y, N, X, uopCSRRC, IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, Y, Y, CSR.C),

  CSRRWI  -> List(Y, N, X, uopCSRRWI,IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, Y, Y, CSR.W),
  CSRRSI  -> List(Y, N, X, uopCSRRSI,IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, Y, Y, CSR.S),
  CSRRCI  -> List(Y, N, X, uopCSRRCI,IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, Y, Y, CSR.C),

  SFENCE_VMA->List(Y,N, X, uopSFENCE,IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_SFENCE,MT_X,0.U, N, N, N, N, N, Y, Y, CSR.N),
  SCALL   -> List(Y, N, X, uopERET  ,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, Y, Y, Y, CSR.I),
  SBREAK  -> List(Y, N, X, uopERET  ,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, Y, Y, Y, CSR.I),
  SRET    -> List(Y, N, X, uopERET  ,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, Y, Y, CSR.I),
  MRET    -> List(Y, N, X, uopERET  ,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, Y, Y, CSR.I),
  DRET    -> List(Y, N, X, uopERET  ,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, Y, Y, CSR.I),

  WFI     -> List(Y, N, X, uopWFI   ,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, Y, Y, CSR.I),

  FENCE_I -> List(Y, N, X, uopNOP  , IQT_INT, FU_X   , RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, Y, M_X  , MT_X , 0.U, N, N, N, N, N, Y, Y, CSR.N),
  FENCE   -> List(Y, N, X, uopFENCE, IQT_INT, FU_MEM , RT_X  , RT_X  , RT_X  , N, IS_X, N, Y, N, Y, N, M_X  , MT_X , 0.U, N, N, N, N, N, Y, Y, CSR.N), // TODO PERF make fence higher performance
                                                                                                                                                       // currently serializes pipeline
  // A-type
  AMOADD_W-> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_ADD, MT_W,0.U,N, N, N, N, N, Y, Y, CSR.N), // TODO make AMOs higherperformance
  AMOXOR_W-> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_XOR, MT_W,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOSWAP_W->List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_SWAP,MT_W,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOAND_W-> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_AND, MT_W,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOOR_W -> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_OR,  MT_W,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOMIN_W-> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MIN, MT_W,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOMINU_W->List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MINU,MT_W,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOMAX_W-> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MAX, MT_W,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOMAXU_W->List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MAXU,MT_W,0.U,N, N, N, N, N, Y, Y, CSR.N),

  AMOADD_D-> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_ADD, MT_D,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOXOR_D-> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_XOR, MT_D,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOSWAP_D->List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_SWAP,MT_D,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOAND_D-> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_AND, MT_D,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOOR_D -> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_OR,  MT_D,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOMIN_D-> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MIN, MT_D,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOMINU_D->List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MINU,MT_D,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOMAX_D-> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MAX, MT_D,0.U,N, N, N, N, N, Y, Y, CSR.N),
  AMOMAXU_D->List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MAXU,MT_D,0.U,N, N, N, N, N, Y, Y, CSR.N),

  LR_W    -> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XLR   , MT_W,0.U,N, N, N, N, N, Y, Y, CSR.N), // TODO optimize LR, SC
  LR_D    -> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XLR   , MT_D,0.U,N, N, N, N, N, Y, Y, CSR.N), // note LR generates 2 micro-ops,
  SC_W    -> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XSC   , MT_W,0.U,N, N, N, N, N, Y, Y, CSR.N), // one which isn't needed
  SC_D    -> List(Y, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XSC   , MT_D,0.U,N, N, N, N, N, Y, Y, CSR.N)
  )
}

/**
 * FP Decode constants
 */
object FDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
            //                                                                  frs3_en                               wakeup_delay
            //                                                                  |  imm sel                            |    bypassable (aka, known/fixed latency)
            //                                                                  |  |     is_load                      |    |  br/jmp
            //    is val inst?                                  rs1 regtype     |  |     |  is_store                  |    |  |  is jal
            //    |  is fp inst?                                |       rs2 type|  |     |  |  is_amo                 |    |  |  |  allocate_brtag
            //    |  |  is dst single-prec?                     |       |       |  |     |  |  |  is_fence            |    |  |  |  |
            //    |  |  |  micro-opcode                         |       |       |  |     |  |  |  |  is_fencei        |    |  |  |  |  is breakpoint or ecall
            //    |  |  |  |           iq_type  func    dst     |       |       |  |     |  |  |  |  |  mem    mem    |    |  |  |  |  |  is unique? (clear pipeline for it)
            //    |  |  |  |           |        unit    regtype |       |       |  |     |  |  |  |  |  cmd    msk    |    |  |  |  |  |  |  flush on commit
            //    |  |  |  |           |        |       |       |       |       |  |     |  |  |  |  |  |      |      |    |  |  |  |  |  |  |  csr cmd
  FLW     -> List(Y, Y, Y, uopLD     , IQT_MEM, FU_MEM, RT_FLT, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_W , 0.U, N, N, N, N, N, N, N, CSR.N),
  FLD     -> List(Y, Y, N, uopLD     , IQT_MEM, FU_MEM, RT_FLT, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_D , 0.U, N, N, N, N, N, N, N, CSR.N),
  FSW     -> List(Y, Y, Y, uopSTA    , IQT_MFP,FU_F2IMEM,RT_X , RT_FIX, RT_FLT, N, IS_S, N, Y, N, N, N, M_XWR, MT_W , 0.U, N, N, N, N, N, N, N, CSR.N), // sort of a lie; broken into two micro-ops
  FSD     -> List(Y, Y, N, uopSTA    , IQT_MFP,FU_F2IMEM,RT_X , RT_FIX, RT_FLT, N, IS_S, N, Y, N, N, N, M_XWR, MT_D , 0.U, N, N, N, N, N, N, N, CSR.N),

  FCLASS_S-> List(Y, Y, Y, uopFCLASS_S,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCLASS_D-> List(Y, Y, N, uopFCLASS_D,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  FMV_S_X -> List(Y, Y, Y, uopFMV_S_X, IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FMV_D_X -> List(Y, Y, N, uopFMV_D_X, IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FMV_X_S -> List(Y, Y, Y, uopFMV_X_S, IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FMV_X_D -> List(Y, Y, N, uopFMV_X_D, IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  FSGNJ_S -> List(Y, Y, Y, uopFSGNJ_S, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FSGNJ_D -> List(Y, Y, N, uopFSGNJ_D, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FSGNJX_S-> List(Y, Y, Y, uopFSGNJ_S, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FSGNJX_D-> List(Y, Y, N, uopFSGNJ_D, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FSGNJN_S-> List(Y, Y, Y, uopFSGNJ_S, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FSGNJN_D-> List(Y, Y, N, uopFSGNJ_D, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  // FP to FP
  FCVT_S_D-> List(Y, Y, Y, uopFCVT_S_D,IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCVT_D_S-> List(Y, Y, N, uopFCVT_D_S,IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  // Int to FP
  FCVT_S_W-> List(Y, Y, Y, uopFCVT_S_X, IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCVT_S_WU->List(Y, Y, Y, uopFCVT_S_X, IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCVT_S_L-> List(Y, Y, Y, uopFCVT_S_X, IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCVT_S_LU->List(Y, Y, Y, uopFCVT_S_X, IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  FCVT_D_W-> List(Y, Y, N, uopFCVT_D_X, IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCVT_D_WU->List(Y, Y, N, uopFCVT_D_X, IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCVT_D_L-> List(Y, Y, N, uopFCVT_D_X, IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCVT_D_LU->List(Y, Y, N, uopFCVT_D_X, IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  // FP to Int
  FCVT_W_S-> List(Y, Y, Y, uopFCVT_X_S, IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCVT_WU_S->List(Y, Y, Y, uopFCVT_X_S, IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCVT_L_S-> List(Y, Y, Y, uopFCVT_X_S, IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCVT_LU_S->List(Y, Y, Y, uopFCVT_X_S, IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  FCVT_W_D-> List(Y, Y, N, uopFCVT_X_D, IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCVT_WU_D->List(Y, Y, N, uopFCVT_X_D, IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCVT_L_D-> List(Y, Y, N, uopFCVT_X_D, IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FCVT_LU_D->List(Y, Y, N, uopFCVT_X_D, IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  // "fp_single" is used for wb_data formatting (and debugging)
  FEQ_S    ->List(Y, Y, Y, uopCMPR_S , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FLT_S    ->List(Y, Y, Y, uopCMPR_S , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FLE_S    ->List(Y, Y, Y, uopCMPR_S , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  FEQ_D    ->List(Y, Y, N, uopCMPR_D , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FLT_D    ->List(Y, Y, N, uopCMPR_D , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FLE_D    ->List(Y, Y, N, uopCMPR_D , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  FMIN_S   ->List(Y, Y, Y,uopFMINMAX_S,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FMAX_S   ->List(Y, Y, Y,uopFMINMAX_S,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FMIN_D   ->List(Y, Y, N,uopFMINMAX_D,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FMAX_D   ->List(Y, Y, N,uopFMINMAX_D,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  FADD_S   ->List(Y, Y, Y, uopFADD_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FSUB_S   ->List(Y, Y, Y, uopFSUB_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FMUL_S   ->List(Y, Y, Y, uopFMUL_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FADD_D   ->List(Y, Y, N, uopFADD_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FSUB_D   ->List(Y, Y, N, uopFSUB_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FMUL_D   ->List(Y, Y, N, uopFMUL_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),

  FMADD_S  ->List(Y, Y, Y, uopFMADD_S, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FMSUB_S  ->List(Y, Y, Y, uopFMSUB_S, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FNMADD_S ->List(Y, Y, Y, uopFNMADD_S,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FNMSUB_S ->List(Y, Y, Y, uopFNMSUB_S,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FMADD_D  ->List(Y, Y, N, uopFMADD_D, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FMSUB_D  ->List(Y, Y, N, uopFMSUB_D, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FNMADD_D ->List(Y, Y, N, uopFNMADD_D,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FNMSUB_D ->List(Y, Y, N, uopFNMSUB_D,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N)
  )
}

/**
 * FP Divide SquareRoot Constants
 */
object FDivSqrtDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
            //                                                                  frs3_en                               wakeup_delay
            //                                                                  |  imm sel                            |    bypassable (aka, known/fixed latency)
            //                                                                  |  |     is_load                      |    |  br/jmp
            //     is val inst?                                 rs1 regtype     |  |     |  is_store                  |    |  |  is jal
            //     |  is fp inst?                               |       rs2 type|  |     |  |  is_amo                 |    |  |  |  allocate_brtag
            //     |  |  is dst single-prec?                    |       |       |  |     |  |  |  is_fence            |    |  |  |  |
            //     |  |  |  micro-opcode                        |       |       |  |     |  |  |  |  is_fencei        |    |  |  |  |  is breakpoint or ecall
            //     |  |  |  |           iq-type func    dst     |       |       |  |     |  |  |  |  |  mem    mem    |    |  |  |  |  |  is unique? (clear pipeline for it)
            //     |  |  |  |           |       unit    regtype |       |       |  |     |  |  |  |  |  cmd    msk    |    |  |  |  |  |  |  flush on commit
            //     |  |  |  |           |       |       |       |       |       |  |     |  |  |  |  |  |      |      |    |  |  |  |  |  |  |  csr cmd
  FDIV_S    ->List(Y, Y, Y, uopFDIV_S , IQT_FP, FU_FDV, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FDIV_D    ->List(Y, Y, N, uopFDIV_D , IQT_FP, FU_FDV, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FSQRT_S   ->List(Y, Y, Y, uopFSQRT_S, IQT_FP, FU_FDV, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
  FSQRT_D   ->List(Y, Y, N, uopFSQRT_D, IQT_FP, FU_FDV, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N)
  )
}
//scalastyle:on

/**
 * RoCC initial decode
 */
object RoCCDecode extends DecodeConstants
{
  // Note: We use FU_CSR since CSR instructions cannot co-execute with RoCC instructions
                       //                                                                   frs3_en                               wakeup_delay
                       //     is val inst?                                                  |  imm sel                            |    bypassable (aka, known/fixed latency)
                       //     |  is fp inst?                                                |  |     is_load                      |    |  br/jmp
                       //     |  |  is single-prec                          rs1 regtype     |  |     |  is_store                  |    |  |  is jal
                       //     |  |  |                                       |       rs2 type|  |     |  |  is_amo                 |    |  |  |  allocate_brtag
                       //     |  |  |  micro-code           func unit       |       |       |  |     |  |  |  is_fence            |    |  |  |  |
                       //     |  |  |  |           iq-type  |               |       |       |  |     |  |  |  |  is_fencei        |    |  |  |  |  is breakpoint or ecall?
                       //     |  |  |  |           |        |       dst     |       |       |  |     |  |  |  |  |  mem    mem    |    |  |  |  |  |  is unique? (clear pipeline for it)
                       //     |  |  |  |           |        |       regtype |       |       |  |     |  |  |  |  |  cmd    msk    |    |  |  |  |  |  |  flush on commit
                       //     |  |  |  |           |        |       |       |       |       |  |     |  |  |  |  |  |      |      |    |  |  |  |  |  |  |  csr cmd
                       //     |  |  |  |           |        |       |       |       |       |  |     |  |  |  |  |  |      |      |    |  |  |  |  |  |  |  |
  val table: Array[(BitPat, List[BitPat])] = Array(//        |       |       |       |       |  |     |  |  |  |  |  |      |      |    |  |  |  |  |  |  |  |
    CUSTOM0            ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM0_RS1        ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM0_RS1_RS2    ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM0_RD         ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM0_RD_RS1     ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM0_RD_RS1_RS2 ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM1            ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM1_RS1        ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM1_RS1_RS2    ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM1_RD         ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM1_RD_RS1     ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM1_RD_RS1_RS2 ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM2            ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM2_RS1        ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM2_RS1_RS2    ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM2_RD         ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM2_RD_RS1     ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM2_RD_RS1_RS2 ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM3            ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM3_RS1        ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM3_RS1_RS2    ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_X  , RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM3_RD         ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM3_RD_RS1     ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N),
    CUSTOM3_RD_RS1_RS2 ->List(Y, N, X, uopROCC   , IQT_INT, FU_CSR, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MT_X , 0.U, N, N, N, N, N, N, N, CSR.N)
  )
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
{
  val io = IO(new DecodeUnitIo)

  val uop = Wire(new MicroOp())
  uop := io.enq.uop

  var decode_table = XDecode.table
  if (usingFPU) decode_table ++= FDecode.table
  if (usingFPU && usingFDivSqrt) decode_table ++= FDivSqrtDecode.table
  if (usingRoCC) decode_table ++= RoCCDecode.table
  decode_table ++= (if (xLen == 64) X64Decode.table else X32Decode.table)

  val rvc_exp    = Module(new RVCExpander)
  rvc_exp.io.in := io.enq.uop.debug_inst
  uop.is_rvc    := rvc_exp.io.rvc
  val inst       = Mux(rvc_exp.io.rvc, rvc_exp.io.out.bits, io.enq.uop.debug_inst)

  val cs = Wire(new CtrlSigs()).decode(inst, decode_table)

  // Exception Handling
  io.csr_decode.csr := inst(31,20)
  val csr_en = cs.csr_cmd.isOneOf(CSR.S, CSR.C, CSR.W)
  val csr_ren = cs.csr_cmd.isOneOf(CSR.S, CSR.C) && uop.lrs1 === 0.U
  val system_insn = cs.csr_cmd === CSR.I
  val sfence = cs.uopc === uopSFENCE

  val cs_legal = cs.legal
//   dontTouch(cs_legal)

  val id_illegal_insn = !cs_legal ||
    cs.fp_val && io.csr_decode.fp_illegal || // TODO check for illegal rm mode: (io.fpu.illegal_rm)
    cs.rocc && io.csr_decode.rocc_illegal ||
    cs.is_amo && !io.status.isa('a'-'a')  ||
    (cs.fp_val && !cs.fp_single) && !io.status.isa('d'-'a') ||
    csr_en && (io.csr_decode.read_illegal || !csr_ren && io.csr_decode.write_illegal) ||
    ((sfence || system_insn) && io.csr_decode.system_illegal)

//     cs.div && !csr.io.status.isa('m'-'a') || TODO check for illegal div instructions

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))

  val (xcpt_valid, xcpt_cause) = checkExceptions(List(
    (io.interrupt,     io.interrupt_cause),
    (uop.replay_if,    MINI_EXCEPTION_REPLAY),
    (uop.xcpt_pf_if,   (Causes.fetch_page_fault).U),
    (uop.xcpt_ae_if,   (Causes.fetch_access).U),
    (uop.xcpt_ma_if,   (Causes.misaligned_fetch).U),
    (id_illegal_insn,  (Causes.illegal_instruction).U)))

  uop.exception := xcpt_valid
  uop.exc_cause := xcpt_cause

  //-------------------------------------------------------------

  uop.uopc       := cs.uopc
  uop.iq_type    := cs.iq_type
  if (usingUnifiedMemIntIQs) {
    when (cs.iq_type === IQT_MEM) { uop.iq_type := IQT_INT }
  }
  uop.fu_code    := cs.fu_code

  // x-registers placed in 0-31, f-registers placed in 32-63.
  // This allows us to straight-up compare register specifiers and not need to
  // verify the rtypes (e.g., bypassing in rename).
  uop.ldst       := inst(RD_MSB,RD_LSB)
  uop.lrs1       := inst(RS1_MSB,RS1_LSB)
  uop.lrs2       := inst(RS2_MSB,RS2_LSB)
  uop.lrs3       := inst(RS3_MSB,RS3_LSB)

  uop.ldst_val   := cs.dst_type =/= RT_X && !(uop.ldst === 0.U && uop.dst_rtype === RT_FIX)
  uop.dst_rtype  := cs.dst_type
  uop.lrs1_rtype := cs.rs1_type
  uop.lrs2_rtype := cs.rs2_type
  uop.frs3_en    := cs.frs3_en

  uop.fp_val     := cs.fp_val
  uop.fp_single  := cs.fp_single // TODO use this signal instead of the FPU decode's table signal?

  uop.mem_cmd    := cs.mem_cmd
  uop.mem_typ    := Mux(!sfence, cs.mem_typ, Cat(uop.lrs2 =/= 0.U, uop.lrs1 =/= 0.U))
  uop.is_load    := cs.is_load
  uop.is_store   := cs.is_store
  uop.is_amo     := cs.is_amo
  uop.is_fence   := cs.is_fence
  uop.is_fencei  := cs.is_fencei
  uop.is_sys_pc2epc   := cs.is_sys_pc2epc
  uop.is_unique  := cs.inst_unique
  uop.flush_on_commit := cs.flush_on_commit || (csr_en && !csr_ren && io.csr_decode.write_flush)

  uop.bypassable   := cs.bypassable

  //-------------------------------------------------------------
  // immediates

  // repackage the immediate, and then pass the fewest number of bits around
  val di24_20 = Mux(cs.imm_sel === IS_B || cs.imm_sel === IS_S, inst(11,7), inst(24,20))
  uop.imm_packed := Cat(inst(31,25), di24_20, inst(19,12))

  //-------------------------------------------------------------

  uop.allocate_brtag := cs.allocate_brtag
  uop.is_br_or_jmp   := cs.br_or_jmp
  uop.is_jal         := cs.is_jal
  uop.is_jump        := cs.is_jal || (uop.uopc === uopJALR)
  uop.is_ret         := (uop.uopc === uopJALR) &&
                        (uop.ldst === X0) &&
                        (uop.lrs1 === RA)
  uop.is_call        := (uop.uopc === uopJALR || uop.uopc === uopJAL) &&
                        (uop.ldst === RA)

  //-------------------------------------------------------------

  io.deq.uop := uop
}

/**
 * Smaller Decode unit for the Frontend to decode different
 * branches.
 * Accepts EXPANDED RVC instructions
 */

class BranchDecode(implicit p: Parameters) extends BoomModule
{
  val io = IO(new Bundle {
    val inst    = Input(UInt(32.W))
    val pc      = Input(UInt(vaddrBitsExtended.W))
    val is_br   = Output(Bool())
    val is_jal  = Output(Bool())
    val is_jalr = Output(Bool())
    val is_ret  = Output(Bool())
    val is_call = Output(Bool())
    val target = Output(UInt(vaddrBitsExtended.W))
    val cfi_type = Output(UInt(CfiType.SZ.W))
  })

  val bpd_csignals =
    freechips.rocketchip.rocket.DecodeLogic(io.inst,
                  List[BitPat](N, N, N, IS_X),
////                      //   is br?
////                      //   |  is jal?
////                      //   |  |  is jalr?
////                      //   |  |  |  br type
////                      //   |  |  |  |
            Array[(BitPat, List[BitPat])](
               JAL     -> List(N, Y, N, IS_J),
               JALR    -> List(N, N, Y, IS_I),
               BEQ     -> List(Y, N, N, IS_B),
               BNE     -> List(Y, N, N, IS_B),
               BGE     -> List(Y, N, N, IS_B),
               BGEU    -> List(Y, N, N, IS_B),
               BLT     -> List(Y, N, N, IS_B),
               BLTU    -> List(Y, N, N, IS_B)
            ))

  val (cs_is_br: Bool) :: (cs_is_jal: Bool) :: (cs_is_jalr:Bool) :: imm_sel_ :: Nil = bpd_csignals

  io.is_br   := cs_is_br
  io.is_jal  := cs_is_jal
  io.is_jalr := cs_is_jalr
  io.is_call := (cs_is_jal || cs_is_jalr) && GetRd(io.inst) === RA
  io.is_ret  := cs_is_jalr && GetRs1(io.inst) === BitPat("b00?01")

  io.target := Mux(cs_is_br, ComputeBranchTarget(io.pc, io.inst, xLen),
                              ComputeJALTarget(io.pc, io.inst, xLen))
  io.cfi_type :=
    Mux(cs_is_jalr,
      CfiType.jalr,
    Mux(cs_is_jal,
      CfiType.jal,
    Mux(cs_is_br,
      CfiType.branch,
      CfiType.none)))
}

/**
 * IO bundle for getting the branch mask
 */
class DebugBranchMaskGenerationLogicIO(implicit p: Parameters) extends BoomBundle
{
  val branch_mask = UInt(MAX_BR_COUNT.W)
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
    val br_tag    = Output(Vec(pl_width, UInt(BR_TAG_SZ.W)))
    val br_mask   = Output(Vec(pl_width, UInt(MAX_BR_COUNT.W)))

     // tell decoders the branch mask has filled up, but on the granularity
     // of an individual micro-op (so some micro-ops can go through)
    val is_full   = Output(Vec(pl_width, Bool()))

    val brinfo         = Input(new BrResolutionInfo())
    val flush_pipeline = Input(Bool())

    val debug = Output(new DebugBranchMaskGenerationLogicIO())
  })

  val branch_mask = RegInit(0.U(MAX_BR_COUNT.W))

  //-------------------------------------------------------------
  // Give out the branch tag to each branch micro-op

  var allocate_mask = branch_mask
  val tag_masks = Wire(Vec(pl_width, UInt(MAX_BR_COUNT.W)))

  for (w <- 0 until pl_width) {
    // TODO this is a loss of performance as we're blocking branches based on potentially fake branches
    io.is_full(w) := (allocate_mask === ~(0.U(MAX_BR_COUNT.W))) && io.is_branch(w)

    // find br_tag and compute next br_mask
    val new_br_tag = Wire(UInt(BR_TAG_SZ.W))
    new_br_tag := 0.U
    tag_masks(w) := 0.U

    for (i <- MAX_BR_COUNT-1 to 0 by -1) {
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
    io.br_mask(w) := GetNewBrMask(io.brinfo, curr_mask)
    curr_mask = Mux(io.will_fire(w), tag_masks(w) | curr_mask, curr_mask)
  }

  //-------------------------------------------------------------
  // Update the current branch_mask

  when (io.flush_pipeline) {
    branch_mask := 0.U
  } .elsewhen (io.brinfo.valid && io.brinfo.mispredict) {
    branch_mask := io.brinfo.exe_mask
  } .otherwise {
    branch_mask := GetNewBrMask(io.brinfo, curr_mask)
  }

  io.debug.branch_mask := branch_mask
}
