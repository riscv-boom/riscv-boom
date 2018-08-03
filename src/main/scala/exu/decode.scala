//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom.exu

import Chisel._
import freechips.rocketchip.config.Parameters

import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.rocket.{CSR,Causes}
import freechips.rocketchip.util.{uintToBitPat,UIntIsOneOf}
import FUConstants._
import boom.common._
import boom.util._


abstract trait DecodeConstants
   extends freechips.rocketchip.rocket.constants.ScalarOpConstants
   with freechips.rocketchip.rocket.constants.MemoryOpConstants
{
// scalastyle:off
  val xpr64 = Y // TODO inform this from xLen

  def decode_default: List[BitPat] =
            //                                                                            frs3_en                              wakeup_delay
            //     is val inst?                                                           |  imm sel                           |                    bypassable (aka, known/fixed latency)
            //     |  is fp inst?                                                         |  |     is_load                     |                    |  br/jmp
            //     |  |  is vec inst?                            rs1 regtype              |  |     |  is_store                 |                    |  |  is jal
            //     |  |  |  is single-pr                          |       rs2 type        |  |     |  |  is_amo                |                    |  |  |  allocate_brtag
            //     |  |  |  |  micro-code        func unit        |       |    rs3_type   |  |     |  |  |  is_fence           |                    |  |  |  |
            //     |  |  |  |  |                 |                |       |    |          |  |     |  |  |  |  is_fencei       |                    |  |  |  |  is breakpoint or ecall
            //     |  |  |  |  |                 |        dst     |       |    |          |  |     |  |  |  |  |  mem    mem   |                    |  |  |  |  |  is unique? (clear pipeline for it)
            //     |  |  |  |  |                 |        regtype |       |    |          |  |     |  |  |  |  |  cmd    msk   |                    |  |  |  |  |  |  flush on commit
            //     |  |  |  |  |                 |        |       |       |    |          |  |     |  |  |  |  |  |      |     |                    |  |  |  |  |  |  |  csr cmd
              List(N, N, N, X, uopX   , IQT_INT, FU_X   ,RT_X,BitPat.dontCare(2),BitPat.dontCare(2),BitPat.dontCare(2),X,IS_X,X,X,X,X,N, M_X,   MT_X, BitPat.dontCare(2),        X, X, X, X, N, N, X, CSR.X)

  val table: Array[(BitPat, List[BitPat])]
// scalastyle:on
}

class CtrlSigs extends Bundle
{
   val legal           = Bool()
   val fp_val          = Bool()
   val vec_val         = Bool()
   val fp_single       = Bool()
   val uopc            = UInt(width = UOPC_SZ)
   val iqtype          = UInt(width = IQT_SZ)
   val fu_code         = UInt(width = FUC_SZ)
   val dst_type        = UInt(width=RT_SZ)
   val rs1_type        = UInt(width=RT_SZ)
   val rs2_type        = UInt(width=RT_SZ)
   val rs3_type        = UInt(width=RT_SZ)
   val frs3_en         = Bool()
   val imm_sel         = UInt(width = IS_X.getWidth)
   val is_load         = Bool()
   val is_store        = Bool()
   val is_amo          = Bool()
   val is_fence        = Bool()
   val is_fencei       = Bool()
   val mem_cmd         = UInt(width = freechips.rocketchip.rocket.M_SZ)
   val mem_typ         = UInt(width = freechips.rocketchip.rocket.MT_SZ)
   val wakeup_delay    = UInt(width = 2)
   val bypassable      = Bool()
   val br_or_jmp       = Bool()
   val is_jal          = Bool()
   val allocate_brtag  = Bool()
   val is_sys_pc2epc   = Bool()
   val inst_unique     = Bool()
   val flush_on_commit = Bool()
   val csr_cmd         = UInt(width = freechips.rocketchip.rocket.CSR.SZ)
   val rocc            = Bool()


   def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
      val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, XDecode.decode_default, table)
      val sigs =
         Seq(legal, fp_val, vec_val, fp_single, uopc, iqtype, fu_code, dst_type, rs1_type
         , rs2_type, rs3_type, frs3_en, imm_sel, is_load, is_store, is_amo
         , is_fence, is_fencei, mem_cmd, mem_typ, wakeup_delay, bypassable
         , br_or_jmp, is_jal, allocate_brtag, is_sys_pc2epc, inst_unique, flush_on_commit, csr_cmd)
      sigs zip decoder map {case(s,d) => s := d}
      rocc := false.B
      this
   }
}


object XDecode extends DecodeConstants
{
// scalastyle:off
            //                                                                             frs3_en                               wakeup_delay
            //     is val inst?                                                            |  imm sel                            |      bypassable (aka, known/fixed latency)
            //     |  is fp inst?                                                          |  |     is_load                      |        |  br/jmp
            //     |  |  is vec inst?                             rs1 regtype              |  |     |  is_store                  |        |  |  is jal
            //     |  |  |  is single-pr                           |       rs2 type        |  |     |  |  is_amo                 |        |  |  |  allocate_brtag
            //     |  |  |  |  micro-code         func unit        |       |       rs3_type|  |     |  |  |  is_fence            |        |  |  |  |
            //     |  |  |  |  |                  |                |       |       |       |  |     |  |  |  |  is_fencei        |        |  |  |  |  is breakpoint or ecall
            //     |  |  |  |  |                  |        dst     |       |       |       |  |     |  |  |  |  |  mem    mem    |        |  |  |  |  |  is unique? (clear pipeline for it)
            //     |  |  |  |  |                  |        regtype |       |       |       |  |     |  |  |  |  |  cmd    msk    |        |  |  |  |  |  |  flush on commit
            //     |  |  |  |  |                  |        |       |       |       |       |  |     |  |  |  |  |  |      |      |        |  |  |  |  |  |  |  csr cmd
   val table: Array[(BitPat, List[BitPat])] = Array(//     |       |       |       |       |  |     |  |  |  |  |  |      |      |        |  |  |  |  |  |  |  |
   LD      -> List(Y, N, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_D , UInt(3), N, N, N, N, N, N, N, CSR.N),
   LW      -> List(Y, N, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_W , UInt(3), N, N, N, N, N, N, N, CSR.N),
   LWU     -> List(Y, N, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_WU, UInt(3), N, N, N, N, N, N, N, CSR.N),
   LH      -> List(Y, N, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_H , UInt(3), N, N, N, N, N, N, N, CSR.N),
   LHU     -> List(Y, N, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_HU, UInt(3), N, N, N, N, N, N, N, CSR.N),
   LB      -> List(Y, N, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_B , UInt(3), N, N, N, N, N, N, N, CSR.N),
   LBU     -> List(Y, N, N, X, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_BU, UInt(3), N, N, N, N, N, N, N, CSR.N),

   SD      -> List(Y, N, N, X, uopSTA  , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, RT_X  , N, IS_S, N, Y, N, N, N, M_XWR, MT_D , UInt(0), N, N, N, N, N, N, N, CSR.N),
   SW      -> List(Y, N, N, X, uopSTA  , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, RT_X  , N, IS_S, N, Y, N, N, N, M_XWR, MT_W , UInt(0), N, N, N, N, N, N, N, CSR.N),
   SH      -> List(Y, N, N, X, uopSTA  , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, RT_X  , N, IS_S, N, Y, N, N, N, M_XWR, MT_H , UInt(0), N, N, N, N, N, N, N, CSR.N),
   SB      -> List(Y, N, N, X, uopSTA  , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, RT_X  , N, IS_S, N, Y, N, N, N, M_XWR, MT_B , UInt(0), N, N, N, N, N, N, N, CSR.N),

   LUI     -> List(Y, N, N, X, uopLUI  , IQT_INT, FU_ALU , RT_FIX, RT_X  , RT_X  , RT_X  , N, IS_U, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),

   ADDI    -> List(Y, N, N, X, uopADDI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   ANDI    -> List(Y, N, N, X, uopANDI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   ORI     -> List(Y, N, N, X, uopORI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   XORI    -> List(Y, N, N, X, uopXORI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SLTI    -> List(Y, N, N, X, uopSLTI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SLTIU   -> List(Y, N, N, X, uopSLTIU, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SLLI    -> List(Y, N, N, X, uopSLLI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SRAI    -> List(Y, N, N, X, uopSRAI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SRLI    -> List(Y, N, N, X, uopSRLI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),

   ADDIW   -> List(Y, N, N, X, uopADDIW, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SLLIW   -> List(Y, N, N, X, uopSLLIW, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SRAIW   -> List(Y, N, N, X, uopSRAIW, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SRLIW   -> List(Y, N, N, X, uopSRLIW, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),

   SLL     -> List(Y, N, N, X, uopSLL  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   ADD     -> List(Y, N, N, X, uopADD  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SUB     -> List(Y, N, N, X, uopSUB  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SLT     -> List(Y, N, N, X, uopSLT  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SLTU    -> List(Y, N, N, X, uopSLTU , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   AND     -> List(Y, N, N, X, uopAND  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   OR      -> List(Y, N, N, X, uopOR   , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   XOR     -> List(Y, N, N, X, uopXOR  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SRA     -> List(Y, N, N, X, uopSRA  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SRL     -> List(Y, N, N, X, uopSRL  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),

   ADDW    -> List(Y, N, N, X, uopADDW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SUBW    -> List(Y, N, N, X, uopSUBW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SLLW    -> List(Y, N, N, X, uopSLLW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SRAW    -> List(Y, N, N, X, uopSRAW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
   SRLW    -> List(Y, N, N, X, uopSRLW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),

   MUL     -> List(Y, N, N, X, uopMUL  , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   MULH    -> List(Y, N, N, X, uopMULH , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   MULHU   -> List(Y, N, N, X, uopMULHU, IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   MULHSU  -> List(Y, N, N, X, uopMULHSU,IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   MULW    -> List(Y, N, N, X, uopMULW , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   DIV     -> List(Y, N, N, X, uopDIV  , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   DIVU    -> List(Y, N, N, X, uopDIVU , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   REM     -> List(Y, N, N, X, uopREM  , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   REMU    -> List(Y, N, N, X, uopREMU , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   DIVW    -> List(Y, N, N, X, uopDIVW , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   DIVUW   -> List(Y, N, N, X, uopDIVUW, IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   REMW    -> List(Y, N, N, X, uopREMW , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   REMUW   -> List(Y, N, N, X, uopREMUW, IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   AUIPC   -> List(Y, N, N, X, uopAUIPC, IQT_INT, FU_BRU , RT_FIX, RT_X  , RT_X  , RT_X  , N, IS_U, N, N, N, N, N, M_X  , MT_X , UInt(1), N, N, N, N, N, N, N, CSR.N), // use BRU for the PC read
   JAL     -> List(Y, N, N, X, uopJAL  , IQT_INT, FU_BRU , RT_FIX, RT_X  , RT_X  , RT_X  , N, IS_J, N, N, N, N, N, M_X  , MT_X , UInt(1), N, Y, Y, N, N, N, N, CSR.N),
   JALR    -> List(Y, N, N, X, uopJALR , IQT_INT, FU_BRU , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(1), N, Y, N, Y, N, N, N, CSR.N),
   BEQ     -> List(Y, N, N, X, uopBEQ  , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, RT_X  , N, IS_B, N, N, N, N, N, M_X  , MT_X , UInt(0), N, Y, N, Y, N, N, N, CSR.N),
   BNE     -> List(Y, N, N, X, uopBNE  , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, RT_X  , N, IS_B, N, N, N, N, N, M_X  , MT_X , UInt(0), N, Y, N, Y, N, N, N, CSR.N),
   BGE     -> List(Y, N, N, X, uopBGE  , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, RT_X  , N, IS_B, N, N, N, N, N, M_X  , MT_X , UInt(0), N, Y, N, Y, N, N, N, CSR.N),
   BGEU    -> List(Y, N, N, X, uopBGEU , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, RT_X  , N, IS_B, N, N, N, N, N, M_X  , MT_X , UInt(0), N, Y, N, Y, N, N, N, CSR.N),
   BLT     -> List(Y, N, N, X, uopBLT  , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, RT_X  , N, IS_B, N, N, N, N, N, M_X  , MT_X , UInt(0), N, Y, N, Y, N, N, N, CSR.N),
   BLTU    -> List(Y, N, N, X, uopBLTU , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, RT_X  , N, IS_B, N, N, N, N, N, M_X  , MT_X , UInt(0), N, Y, N, Y, N, N, N, CSR.N),

   // I-type, the immediate12 holds the CSR register.
   CSRRW   -> List(Y, N, N, X, uopCSRRW, IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, Y, Y, CSR.W),
   CSRRS   -> List(Y, N, N, X, uopCSRRS, IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, Y, Y, CSR.S),
   CSRRC   -> List(Y, N, N, X, uopCSRRC, IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, Y, Y, CSR.C),

   CSRRWI  -> List(Y, N, N, X, uopCSRRWI,IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, Y, Y, CSR.W),
   CSRRSI  -> List(Y, N, N, X, uopCSRRSI,IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, Y, Y, CSR.S),
   CSRRCI  -> List(Y, N, N, X, uopCSRRCI,IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, Y, Y, CSR.C),

   SFENCE_VMA->List(Y,N, N, X, uopSFENCE,IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_SFENCE,MT_X,UInt(0), N, N, N, N, N, Y, Y, CSR.N),
   SCALL   -> List(Y, N, N, X, uopSYSTEM,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, Y, Y, N, CSR.I),
   SBREAK  -> List(Y, N, N, X, uopSYSTEM,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, Y, Y, N, CSR.I),
   SRET    -> List(Y, N, N, X, uopSYSTEM,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, Y, N, CSR.I),
   MRET    -> List(Y, N, N, X, uopSYSTEM,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, Y, N, CSR.I),
   DRET    -> List(Y, N, N, X, uopSYSTEM,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, Y, N, CSR.I),

   WFI     -> List(Y, N, N, X, uopSYSTEM,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, Y, Y, CSR.I),

   FENCE_I -> List(Y, N, N, X, uopNOP  , IQT_INT, FU_X   , RT_X  , RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, Y, M_X  , MT_X , UInt(0), N, N, N, N, N, Y, Y, CSR.N),
   FENCE   -> List(Y, N, N, X, uopFENCE, IQT_INT, FU_MEM , RT_X  , RT_X  , RT_X  , RT_X  , N, IS_X, N, Y, N, Y, N, M_X  , MT_X , UInt(0), N, N, N, N, N, Y, Y, CSR.N), // TODO PERF make fence higher performance
                                                                                                                                                    // currently serializes pipeline
   // A-type
   AMOADD_W-> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_ADD, MT_W,UInt(0),N, N, N, N, N, Y, Y, CSR.N), // TODO make AMOs higherperformance
   AMOXOR_W-> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_XOR, MT_W,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOSWAP_W->List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_SWAP,MT_W,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOAND_W-> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_AND, MT_W,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOOR_W -> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_OR,  MT_W,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOMIN_W-> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_MIN, MT_W,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOMINU_W->List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_MINU,MT_W,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOMAX_W-> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_MAX, MT_W,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOMAXU_W->List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_MAXU,MT_W,UInt(0),N, N, N, N, N, Y, Y, CSR.N),

   AMOADD_D-> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_ADD, MT_D,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOXOR_D-> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_XOR, MT_D,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOSWAP_D->List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_SWAP,MT_D,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOAND_D-> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_AND, MT_D,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOOR_D -> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_OR,  MT_D,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOMIN_D-> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_MIN, MT_D,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOMINU_D->List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_MINU,MT_D,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOMAX_D-> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_MAX, MT_D,UInt(0),N, N, N, N, N, Y, Y, CSR.N),
   AMOMAXU_D->List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XA_MAXU,MT_D,UInt(0),N, N, N, N, N, Y, Y, CSR.N),

   LR_W    -> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XLR   , MT_W,UInt(0),N, N, N, N, N, Y, Y, CSR.N), // TODO optimize LR, SC
   LR_D    -> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XLR   , MT_D,UInt(0),N, N, N, N, N, Y, Y, CSR.N), // note LR generates 2 micro-ops,
   SC_W    -> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XSC   , MT_W,UInt(0),N, N, N, N, N, Y, Y, CSR.N), // one which isn't needed
   SC_D    -> List(Y, N, N, X, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_X  , N, IS_X, N, Y, Y, N, N, M_XSC   , MT_D,UInt(0),N, N, N, N, N, Y, Y, CSR.N)
   )
// scalastyle:on
}

object FDecode extends DecodeConstants
{
// scalastyle:off
  val table: Array[(BitPat, List[BitPat])] = Array(
            //                                                                              frs3_en                               wakeup_delay
            //     is val inst?                                                             |  imm sel                            |      bypassable (aka, known/fixed latency)
            //     |  is fp inst?                                                           |  |     is_load                      |        |  br/jmp
            //     |  |  is vec inst?                              rs1 regtype              |  |     |  is_store                  |        |  |  is jal
            //     |  |  |  is single-pr                            |       rs2 type        |  |     |  |  is_amo                 |        |  |  |  allocate_brtag
            //     |  |  |  |  micro-code           func unit       |       |       rs3 type|  |     |  |  |  is_fence            |        |  |  |  |
            //     |  |  |  |  |                    |               |       |       |       |  |     |  |  |  |  is_fencei        |        |  |  |  |  is breakpoint or ecall
            //     |  |  |  |  |                    |       dst     |       |       |       |  |     |  |  |  |  |  mem    mem    |        |  |  |  |  |  is unique? (clear pipeline for it)
            //     |  |  |  |  |                    |       regtype |       |       |       |  |     |  |  |  |  |  cmd    msk    |        |  |  |  |  |  |  flush on commit
            //     |  |  |  |  |                    |       |       |       |       |       |  |     |  |  |  |  |  |      |      |        |  |  |  |  |  |  |  csr cmd
   FLW     -> List(Y, Y, N, Y, uopLD     , IQT_MEM, FU_MEM, RT_FLT, RT_FIX, RT_X  , RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_W , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FLD     -> List(Y, Y, N, N, uopLD     , IQT_MEM, FU_MEM, RT_FLT, RT_FIX, RT_X  , RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MT_D , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FSW     -> List(Y, Y, N, Y, uopSTA    , IQT_MEM, FU_MEM, RT_X  , RT_FIX, RT_FLT, RT_X  , N, IS_S, N, Y, N, N, N, M_XWR, MT_W , UInt(0), N, N, N, N, N, N, N, CSR.N), // sort of a lie; broken into two micro-ops
   FSD     -> List(Y, Y, N, N, uopSTA    , IQT_MEM, FU_MEM, RT_X  , RT_FIX, RT_FLT, RT_X  , N, IS_S, N, Y, N, N, N, M_XWR, MT_D , UInt(0), N, N, N, N, N, N, N, CSR.N),

   FCLASS_S-> List(Y, Y, N, Y, uopFCLASS_S,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCLASS_D-> List(Y, Y, N, N, uopFCLASS_D,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   FMV_S_X -> List(Y, Y, N, Y, uopFMV_S_X, IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FMV_D_X -> List(Y, Y, N, N, uopFMV_D_X, IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FMV_X_S -> List(Y, Y, N, Y, uopFMV_X_S, IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FMV_X_D -> List(Y, Y, N, N, uopFMV_X_D, IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   FSGNJ_S -> List(Y, Y, N, Y, uopFSGNJ_S, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FSGNJ_D -> List(Y, Y, N, N, uopFSGNJ_D, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FSGNJX_S-> List(Y, Y, N, Y, uopFSGNJ_S, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FSGNJX_D-> List(Y, Y, N, N, uopFSGNJ_D, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FSGNJN_S-> List(Y, Y, N, Y, uopFSGNJ_S, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FSGNJN_D-> List(Y, Y, N, N, uopFSGNJ_D, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   // FP to FP
   FCVT_S_D-> List(Y, Y, N, Y, uopFCVT_S_D,IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCVT_D_S-> List(Y, Y, N, N, uopFCVT_D_S,IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   // Int to FP
   FCVT_S_W-> List(Y, Y, N, Y, uopFCVT_S_W ,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCVT_S_WU->List(Y, Y, N, Y, uopFCVT_S_WU,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCVT_S_L-> List(Y, Y, N, Y, uopFCVT_S_L ,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCVT_S_LU->List(Y, Y, N, Y, uopFCVT_S_LU,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   FCVT_D_W-> List(Y, Y, N, N, uopFCVT_D_W ,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCVT_D_WU->List(Y, Y, N, N, uopFCVT_D_WU,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCVT_D_L-> List(Y, Y, N, N, uopFCVT_D_L ,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCVT_D_LU->List(Y, Y, N, N, uopFCVT_D_LU,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   // FP to Int
   FCVT_W_S-> List(Y, Y, N, Y, uopFCVT_W_S ,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCVT_WU_S->List(Y, Y, N, Y, uopFCVT_WU_S,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCVT_L_S-> List(Y, Y, N, Y, uopFCVT_L_S ,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCVT_LU_S->List(Y, Y, N, Y, uopFCVT_LU_S,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   FCVT_W_D-> List(Y, Y, N, N, uopFCVT_W_D ,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCVT_WU_D->List(Y, Y, N, N, uopFCVT_WU_D,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCVT_L_D-> List(Y, Y, N, N, uopFCVT_L_D ,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FCVT_LU_D->List(Y, Y, N, N, uopFCVT_LU_D,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   // "fp_single" is used for wb_data formatting (and debugging)
   FEQ_S    ->List(Y, Y, N, Y, uopFEQ_S  , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FLT_S    ->List(Y, Y, N, Y, uopFLT_S  , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FLE_S    ->List(Y, Y, N, Y, uopFLE_S  , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   FEQ_D    ->List(Y, Y, N, N, uopFEQ_D  , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FLT_D    ->List(Y, Y, N, N, uopFLT_D  , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FLE_D    ->List(Y, Y, N, N, uopFLE_D  , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   FMIN_S   ->List(Y, Y, N, Y, uopFMIN_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FMAX_S   ->List(Y, Y, N, Y, uopFMAX_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FMIN_D   ->List(Y, Y, N, N, uopFMIN_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FMAX_D   ->List(Y, Y, N, N, uopFMAX_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   FADD_S   ->List(Y, Y, N, Y, uopFADD_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FSUB_S   ->List(Y, Y, N, Y, uopFSUB_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FMUL_S   ->List(Y, Y, N, Y, uopFMUL_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FADD_D   ->List(Y, Y, N, N, uopFADD_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FSUB_D   ->List(Y, Y, N, N, uopFSUB_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FMUL_D   ->List(Y, Y, N, N, uopFMUL_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

   FMADD_S  ->List(Y, Y, N, Y, uopFMADD_S, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FMSUB_S  ->List(Y, Y, N, Y, uopFMSUB_S, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FNMADD_S ->List(Y, Y, N, Y, uopFNMADD_S,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FNMSUB_S ->List(Y, Y, N, Y, uopFNMSUB_S,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FMADD_D  ->List(Y, Y, N, N, uopFMADD_D, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FMSUB_D  ->List(Y, Y, N, N, uopFMSUB_D, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FNMADD_D ->List(Y, Y, N, N, uopFNMADD_D,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FNMSUB_D ->List(Y, Y, N, N, uopFNMSUB_D,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N)
   )

// scalastyle:on
}

object FDivSqrtDecode extends DecodeConstants
{
// scalastyle:off
  val table: Array[(BitPat, List[BitPat])] = Array(
             //                                                                             frs3_en                               wakeup_delay
             //     is val inst?                                                            |  imm sel                            |      bypassable (aka, known/fixed latency)
             //     |  is fp inst?                                                          |  |     is_load                      |        |  br/jmp
             //     |  |  is vec inst?                             rs1 regtype              |  |     |  is_store                  |        |  |  is jal
             //     |  |  |  is single-pr                           |       rs2 type        |  |     |  |  is_amo                 |        |  |  |  allocate_brtag
             //     |  |  |  |  micro-code          func unit       |       |       rs3 type|  |     |  |  |  is_fence            |        |  |  |  |
             //     |  |  |  |  |                   |               |       |       |       |  |     |  |  |  |  is_fencei        |        |  |  |  |  is breakpoint or ecall
             //     |  |  |  |  |                   |       dst     |       |       |       |  |     |  |  |  |  |  mem    mem    |        |  |  |  |  |  is unique? (clear pipeline for it)
             //     |  |  |  |  |                   |       regtype |       |       |       |  |     |  |  |  |  |  cmd    msk    |        |  |  |  |  |  |  flush on commit
             //     |  |  |  |  |                   |       |       |       |       |       |  |     |  |  |  |  |  |      |      |        |  |  |  |  |  |  |  csr cmd
   FDIV_S    ->List(Y, Y, N, Y, uopFDIV_S , IQT_FP, FU_FDV, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FDIV_D    ->List(Y, Y, N, N, uopFDIV_D , IQT_FP, FU_FDV, RT_FLT, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FSQRT_S   ->List(Y, Y, N, Y, uopFSQRT_S, IQT_FP, FU_FDV, RT_FLT, RT_FLT, RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   FSQRT_D   ->List(Y, Y, N, N, uopFSQRT_D, IQT_FP, FU_FDV, RT_FLT, RT_FLT, RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N)
   )
// scalastyle:on
}
object VecDecode extends DecodeConstants
{
// scalastyle:off
  val table: Array[(BitPat, List[BitPat])] = Array(
             //                                                                                 frs3_en                               wakeup_delay
             //     is val inst?                                                                |  imm sel                            |      bypassable (aka, known/fixed latency)
             //     |  is fp inst?                                                              |  |     is_load                      |        |  br/jmp
             //     |  |  is vec inst?                              rs1 regtype                 |  |     |  is_store                  |        |  |  is jal
             //     |  |  |  is single-pr                            |        rs2 type          |  |     |  |  is_amo                 |        |  |  |  allocate_brtag
             //     |  |  |  |  micro-code          func unit        |        |        rs3 type |  |     |  |  |  is_fence            |        |  |  |  |
             //     |  |  |  |  |                   |                |        |        |        |  |     |  |  |  |  is_fencei        |        |  |  |  |  is breakpoint or ecall
             //     |  |  |  |  |                   |       dst      |        |        |        |  |     |  |  |  |  |  mem    mem    |        |  |  |  |  |  is unique? (clear pipeline for it)
             //     |  |  |  |  |                   |       regtype  |        |        |        |  |     |  |  |  |  |  cmd    msk    |        |  |  |  |  |  |  flush on commit
             //     |  |  |  |  |                   |       |        |        |        |        |  |     |  |  |  |  |  |      |      |        |  |  |  |  |  |  |  csr cmd
   VADD      ->List(Y, N, Y, N, uopVADD  ,  IQT_VEC,FU_POLY,RT_VEC , RT_POLY, RT_POLY, RT_X   , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VSUB      ->List(Y, N, Y, N, uopVSUB  ,  IQT_VEC,FU_POLY,RT_VEC , RT_POLY, RT_POLY, RT_X   , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VMUL      ->List(Y, N, Y, N, uopVMUL  ,  IQT_VEC,FU_POLY,RT_VEC , RT_POLY, RT_POLY, RT_X   , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VMADD     ->List(Y, N, Y, N, uopVMADD ,  IQT_VEC,FU_POLY,RT_VEC , RT_POLY, RT_POLY, RT_POLY, N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VMSUB     ->List(Y, N, Y, N, uopVMSUB ,  IQT_VEC,FU_POLY,RT_VEC , RT_POLY, RT_POLY, RT_POLY, N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VNMADD    ->List(Y, N, Y, N, uopVNMADD,  IQT_VEC,FU_POLY,RT_VEC , RT_POLY, RT_POLY, RT_POLY, N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VNMSUB    ->List(Y, N, Y, N, uopVNMSUB,  IQT_VEC,FU_POLY,RT_VEC , RT_POLY, RT_POLY, RT_POLY, N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VLD       ->List(Y, N, Y, N, uopVLD   ,  IQT_MEM,FU_MEM ,RT_VEC , RT_FIX , RT_X   , RT_X   , N, IS_I, Y, N, N, N, N, M_XRD, MT_D , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VLDS      ->List(Y, N, Y, N, uopVLDS  ,  IQT_MEM,FU_MEM ,RT_VEC , RT_FIX , RT_FIX , RT_X   , N, IS_I, Y, N, N, N, N, M_XRD, MT_D , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VLDX      ->List(Y, N, Y, N, uopVLDX  ,  IQT_VEC,FU_MEM ,RT_VEC , RT_FIX , RT_VEC , RT_X   , N, IS_I, Y, N, N, N, N, M_XRD, MT_D , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VST       ->List(Y, N, Y, N, uopVST   ,  IQT_MEM,FU_MEM ,RT_X   , RT_FIX , RT_X   , RT_VEC , N, IS_S, N, Y, N, N, N, M_XWR, MT_D , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VSTS      ->List(Y, N, Y, N, uopVSTS  ,  IQT_MEM,FU_MEM ,RT_X   , RT_FIX , RT_FIX , RT_VEC , N, IS_S, N, Y, N, N, N, M_XWR, MT_D , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VSTX      ->List(Y, N, Y, N, uopVSTX  ,  IQT_VEC,FU_MEM ,RT_X   , RT_FIX , RT_VEC , RT_VEC , N, IS_S, N, Y, N, N, N, M_XWR, MT_D , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VINSERT   ->List(Y, N, Y, N, uopVINSERT, IQT_VEC,FU_VALU,RT_POLY, RT_FIX , RT_FIX , RT_VEC , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VEXTRACT  ->List(Y, N, Y, N, uopVEXTRACT,IQT_VEC,FU_VALU,RT_FIX , RT_POLY, RT_FIX , RT_X   , N, IS_X, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
   VADDI     ->List(Y, N, Y, N, uopVADDI ,  IQT_VEC,FU_VALU,RT_VEC , RT_POLY, RT_X   , RT_X   , N, IS_V, N, N, N, N, N, M_X  , MT_X , UInt(0), N, N, N, N, N, N, N, CSR.N)
     // TODO_Vec: VINSV needs to go to both int and v iqs
      //           This should default to FU_I2V



      //uopVST will go to both vector iq and mem iq. Also for strided stuff
  ) // TODO_VEC Add all other instructions, decide correct uop for polymorphism
// scalastyle:on
}


class DecodeUnitIo(implicit p: Parameters) extends BoomBundle()(p)
{
   val enq = new Bundle { val uop = new MicroOp().asInput }
   val deq = new Bundle { val uop = new MicroOp().asOutput }

   // from CSRFile
   val status = new freechips.rocketchip.rocket.MStatus().asInput
   val csr_decode = Flipped(new freechips.rocketchip.rocket.CSRDecodeIO)
   val interrupt = Bool(INPUT)
   val interrupt_cause = UInt(INPUT, xLen)
   val vecstatus = new freechips.rocketchip.rocket.VecStatus().asInput

   override def cloneType: this.type = new DecodeUnitIo()(p).asInstanceOf[this.type]
}

// Takes in a single instruction, generates a MicroOp.
class DecodeUnit(implicit p: Parameters) extends BoomModule()(p) with freechips.rocketchip.rocket.constants.ScalarOpConstants
{
   val io = IO(new DecodeUnitIo)

   val uop = Wire(new MicroOp())
   uop := io.enq.uop

   var decode_table = XDecode.table
   if (usingFPU) decode_table ++= FDecode.table
   if (usingFPU && usingFDivSqrt) decode_table ++= FDivSqrtDecode.table
   if (usingVec) decode_table ++= VecDecode.table
   val cs = Wire(new CtrlSigs()).decode(uop.inst, decode_table)


   // Exception Handling
   io.csr_decode.csr := uop.inst(31,20)
	val csr_en = cs.csr_cmd.isOneOf(CSR.S, CSR.C, CSR.W)
	val csr_ren = cs.csr_cmd.isOneOf(CSR.S, CSR.C) && uop.lrs1 === 0.U
	val system_insn = cs.csr_cmd >= CSR.I
	val sfence = cs.uopc === uopSFENCE

   val cs_legal = cs.legal
//   dontTOuch(cs_legal)

   val id_illegal_insn = !cs_legal ||
      cs.fp_val && io.csr_decode.fp_illegal || // TODO check for illegal rm mode: (io.fpu.illegal_rm)
      cs.rocc && io.csr_decode.rocc_illegal ||
      cs.is_amo && !io.status.isa('a'-'a') ||
      (cs.fp_val && !cs.fp_single) && !io.status.isa('d'-'a') ||
     csr_en && (io.csr_decode.read_illegal || !csr_ren && io.csr_decode.write_illegal) ||
     ((sfence || system_insn) && io.csr_decode.system_illegal)

//     cs.div && !csr.io.status.isa('m'-'a') || TODO check for illegal div instructions


   def checkExceptions(x: Seq[(Bool, UInt)]) =
      (x.map(_._1).reduce(_||_), PriorityMux(x))

   val (xcpt_valid, xcpt_cause) = checkExceptions(List(
      (io.interrupt,     io.interrupt_cause),
      (uop.replay_if,    MINI_EXCEPTION_REPLAY),
      (uop.xcpt_pf_if,   UInt(Causes.fetch_page_fault)),
      (uop.xcpt_ae_if,   UInt(Causes.fetch_access)),
      (uop.xcpt_ma_if,   UInt(Causes.misaligned_fetch)),
      (id_illegal_insn,  UInt(Causes.illegal_instruction))))

   uop.exception := xcpt_valid
   uop.exc_cause := xcpt_cause

   //-------------------------------------------------------------

   uop.uopc       := cs.uopc
   uop.iqtype     := cs.iqtype
   when (cs.fu_code === FU_POLY && usingVec.B) {
      uop.fu_code := Mux(io.vecstatus.vereps(uop.inst(RD_MSB, RD_LSB)) === VEREP_FP, FU_VFPU, FU_VALU)
   } .otherwise {
      uop.fu_code    := cs.fu_code
   }

   val cs_rd  = uop.inst(RD_MSB , RD_LSB)
   val cs_rs1 = uop.inst(RS1_MSB, RS1_LSB)
   val cs_rs2 = uop.inst(RS2_MSB, RS2_LSB)
   val cs_rs3 = uop.inst(RS3_MSB, RS3_LSB)

   List(
      (cs_rd , cs.dst_type, uop.ldst, uop.dst_rtype),
      (cs_rs1, cs.rs1_type, uop.lrs1, uop.lrs1_rtype),
      (cs_rs2, cs.rs2_type, uop.lrs2, uop.lrs2_rtype),
      (cs_rs3, cs.rs3_type, uop.lrs3, uop.lrs3_rtype)) map {
      case (cs_reg, cs_rtype, uop_reg, uop_regtype) => {
         uop_regtype := cs_rtype
         uop_reg := cs_reg
         when (cs_rtype === RT_POLY && usingVec.B) {
            when (io.vecstatus.vshapes(cs_reg) === VSHAPE_SCALAR) {
               uop_regtype := RT_FLT
               uop_reg := "b100000".U | cs_reg
            } .otherwise {
               uop_regtype := RT_VEC
            }
         }
      }
   }

   uop.frs3_en    := cs.rs3_type =/= RT_X

   uop.ldst_val   := cs.dst_type =/= RT_X && !(cs_rd === UInt(0) && uop.dst_rtype === RT_FIX)


   uop.rs1_vew    := io.vecstatus.vews(cs_rs1)
   uop.rs2_vew    := io.vecstatus.vews(cs_rs2)
   uop.rs3_vew    := io.vecstatus.vews(cs_rs3)
   uop.rd_vew     := io.vecstatus.vews(cs_rd)

   uop.rs1_vshape := io.vecstatus.vshapes(cs_rs1)
   uop.rs2_vshape := io.vecstatus.vshapes(cs_rs2)
   uop.rs3_vshape := io.vecstatus.vshapes(cs_rs3)
   uop.rd_vshape  := io.vecstatus.vshapes(cs_rd)

   uop.rs1_verep  := io.vecstatus.vereps(cs_rs1)
   uop.rs2_verep  := io.vecstatus.vereps(cs_rs2)
   uop.rs3_verep  := io.vecstatus.vereps(cs_rs3)
   uop.rd_verep   := io.vecstatus.vereps(cs_rd)

   uop.rate       := MuxLookup(io.vecstatus.vews(cs_rd), VEW_DISABLE, Array(
      VEW_8  -> UInt(16),
      VEW_16 -> UInt(8),// TODO_vec: this needs to lookup when dst is not vec
      VEW_32 -> UInt(4),
      VEW_64 -> UInt(2)))
   uop.eidx       := UInt(0)

   uop.fp_val     := cs.fp_val
   uop.vec_val    := cs.vec_val
   uop.fp_single  := cs.fp_single // TODO use this signal instead of the FPU decode's table signal?

   uop.mem_cmd    := cs.mem_cmd
   uop.mem_typ    := Mux(!sfence, cs.mem_typ, Cat(cs_rs2 =/= 0.U, cs_rs1 =/= 0.U))
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
   // Special cases for polymorphic vector instructions
   if (usingVec) {
      when (cs.vec_val && cs.is_load) {
         uop.rate       := UInt(1)
         uop.mem_typ    := MuxLookup(io.vecstatus.vews(cs_rd), VEW_8, Array(VEW_8 -> MT_B, VEW_16 -> MT_H, VEW_32 -> MT_W, VEW_64 -> MT_D))
      } .elsewhen (cs.vec_val && cs.is_store) {
         uop.rate       := UInt(1)
         uop.mem_typ    := MuxLookup(io.vecstatus.vews(cs_rs3), VEW_8, Array(VEW_8 -> MT_B, VEW_16 -> MT_H, VEW_32 -> MT_W, VEW_64 -> MT_D))
      } .elsewhen (cs.uopc === uopVINSERT) {
         when (uop.dst_rtype === RT_FLT) {
            uop.iqtype     := IQT_INT
            uop.fp_val     := true.B
            uop.vec_val    := false.B
            uop.uopc       := uopFMV_D_X
            uop.fu_code    := FU_I2F
            uop.lrs2_rtype := RT_X
         } .otherwise {
            uop.lrs3       := uop.ldst
            uop.rs3_vew    := uop.rd_vew
            uop.rs3_vshape := uop.rd_vshape
            uop.rs3_verep  := uop.rd_verep
         }
      } .elsewhen (cs.uopc === uopVEXTRACT) {
         when (uop.lrs1_rtype === RT_FLT) {
            uop.iqtype     := IQT_FP
            uop.vec_val    := false.B
            uop.uopc       := uopFMV_X_D
            uop.fu_code    := FU_F2I
            uop.lrs2_rtype := RT_X
         }
      }
      uop.use_vscopb       := (uop.iqtype === IQT_VEC || (cs.vec_val && cs.is_store)) &&
                              ((uop.lrs1_rtype === RT_FIX || uop.lrs1_rtype === RT_FLT) ||
                               (uop.lrs2_rtype === RT_FIX || uop.lrs2_rtype === RT_FLT) ||
                               (uop.lrs3_rtype === RT_FIX || uop.lrs3_rtype === RT_FLT))
   }

   //-------------------------------------------------------------
   // immediates

   // repackage the immediate, and then pass the fewest number of bits around
   val di24_20 = Mux(cs.imm_sel === IS_B || cs.imm_sel === IS_S, uop.inst(11,7), uop.inst(24,20))
   val di31_25 = Mux(cs.vec_val && cs.is_store, Bits(0, 6), uop.inst(31,25))
   uop.imm_packed := Cat(di31_25, di24_20, uop.inst(19,12))

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

   //-------------------------------------------------------------

   override val compileOptions = chisel3.core.ExplicitCompileOptions.NotStrict.copy(explicitInvalidate = true)
}


class BranchDecode extends Module
{
   val io = IO(new Bundle
   {
      val inst    = UInt(INPUT, 32)
      val is_br   = Bool(OUTPUT)
      val is_jal  = Bool(OUTPUT)
      val is_jalr = Bool(OUTPUT)
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
}


// track the current "branch mask", and give out the branch mask to each micro-op in Decode
// (each micro-op in the machine has a branch mask which says which branches it
// is being speculated under).

class DebugBranchMaskGenerationLogicIO(implicit p: Parameters) extends BoomBundle()(p)
{
   val branch_mask = UInt(width = MAX_BR_COUNT)
}

class BranchMaskGenerationLogic(val pl_width: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new Bundle
   {
      // guess if the uop is a branch (we'll catch this later)
      val is_branch = Vec(pl_width, Bool()).asInput
      // lock in that it's actually a branch and will fire, so we update
      // the branch_masks.
      val will_fire = Vec(pl_width, Bool()).asInput

      // give out tag immediately (needed in rename)
      // mask can come later in the cycle
      val br_tag    = Vec(pl_width, UInt(width=BR_TAG_SZ)).asOutput
      val br_mask   = Vec(pl_width, UInt(width=MAX_BR_COUNT)).asOutput

       // tell decoders the branch mask has filled up, but on the granularity
       // of an individual micro-op (so some micro-ops can go through)
      val is_full   = Vec(pl_width, Bool()).asOutput

      val brinfo         = new BrResolutionInfo().asInput
      val flush_pipeline = Bool(INPUT)

      val debug = new DebugBranchMaskGenerationLogicIO().asOutput
   })

   val branch_mask = Reg(init = UInt(0, MAX_BR_COUNT))

   //-------------------------------------------------------------
   // Give out the branch tag to each branch micro-op

   var allocate_mask = branch_mask
   val tag_masks = Wire(Vec(pl_width, UInt(width=MAX_BR_COUNT)))

   for (w <- 0 until pl_width)
   {
      // TODO this is a loss of performance as we're blocking branches based on potentially fake branches
      io.is_full(w) := (allocate_mask === ~(UInt(0,MAX_BR_COUNT))) && io.is_branch(w)

      // find br_tag and compute next br_mask
      val new_br_tag = Wire(UInt(width = BR_TAG_SZ))
      new_br_tag := UInt(0)
      tag_masks(w) := UInt(0)

      for (i <- MAX_BR_COUNT-1 to 0 by -1)
      {
         when (~allocate_mask(i))
         {
            new_br_tag := UInt(i)
            tag_masks(w) := (UInt(1) << UInt(i))
         }
      }

      io.br_tag(w) := new_br_tag
      allocate_mask = Mux(io.is_branch(w), tag_masks(w) | allocate_mask, allocate_mask)
   }

   //-------------------------------------------------------------
   // Give out the branch mask to each micro-op
   // (kill off the bits that corresponded to branches that aren't going to fire)

   var curr_mask = branch_mask
   for (w <- 0 until pl_width)
   {
      io.br_mask(w) := GetNewBrMask(io.brinfo, curr_mask)
      curr_mask = Mux(io.will_fire(w), tag_masks(w) | curr_mask, curr_mask)
   }

   //-------------------------------------------------------------
   // Update the current branch_mask

   when (io.flush_pipeline)
   {
      branch_mask := UInt(0)
   }
   .elsewhen (io.brinfo.valid && io.brinfo.mispredict)
   {
      branch_mask := io.brinfo.exe_mask
   }
   .otherwise
   {
      branch_mask := GetNewBrMask(io.brinfo, curr_mask)
   }

   io.debug.branch_mask := branch_mask

   override val compileOptions = chisel3.core.ExplicitCompileOptions.NotStrict.copy(explicitInvalidate = true)
}
