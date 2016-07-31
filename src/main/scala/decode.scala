//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom
{

import Chisel._
import cde.Parameters

import rocket.Instructions._
import rocket.{CSR,Causes}
import rocket.Util.uintToBitPat
import FUConstants._
import uncore.constants.MemoryOpConstants._


abstract trait DecodeConstants
{
// scalastyle:off
  val xpr64 = Y // TODO inform this from xLen

  def decode_default: List[BitPat] =
            //                                                         frs3_en                                wakeup_delay
            //     is val inst?                                        |  imm sel                             |                    bypassable (aka, known/fixed latency)
            //     |  is fp inst?                                      |  |     is_load                       |                    |  br/jmp
            //     |  |  is single-prec?               rs1 regtype     |  |     |  is_store                   |                    |  |  is jal
            //     |  |  |  micro-code                 |       rs2 type|  |     |  |  is_amo                  |                    |  |  |  allocate_brtag
            //     |  |  |  |         func unit        |       |       |  |     |  |  |  is_fence             |                    |  |  |  |
            //     |  |  |  |         |                |       |       |  |     |  |  |  |  is_fencei         |                    |  |  |  |
            //     |  |  |  |         |        dst     |       |       |  |     |  |  |  |  |  mem    mem     |                    |  |  |  |  is unique? (clear pipeline for it)
            //     |  |  |  |         |        regtype |       |       |  |     |  |  |  |  |  cmd    msk     |                    |  |  |  |  |  flush on commit
            //     |  |  |  |         |        |       |       |       |  |     |  |  |  |  |  |      |       |                    |  |  |  |  |  |  csr cmd
              List(N, N, X, uopX    , FU_X   ,RT_X,BitPat.DC(2),BitPat.DC(2),X,IS_X,X,X,X,X,N, M_X,   MSK_X,  BitPat.DC(2),        X, X, X, X, N, X, CSR.X)

  val table: Array[(BitPat, List[BitPat])]
// scalastyle:on
}

class CtrlSigs extends Bundle
{
   val legal           = Bool()
   val fp_val          = Bool()
   val fp_single       = Bool()
   val uopc            = UInt(width = UOPC_SZ)
   val fu_code         = UInt(width = FUC_SZ)
   val dst_type        = UInt(width=2)
   val rs1_type        = UInt(width=2)
   val rs2_type        = UInt(width=2)
   val frs3_en         = Bool()
   val imm_sel         = UInt(width = IS_X.getWidth)
   val is_load         = Bool()
   val is_store        = Bool()
   val is_amo          = Bool()
   val is_fence        = Bool()
   val is_fencei       = Bool()
   val mem_cmd         = UInt(width = M_SZ)
   val mem_typ         = UInt(width = MT_SZ)
   val wakeup_delay    = UInt(width = 2)
   val bypassable      = Bool()
   val br_or_jmp       = Bool()
   val is_jal          = Bool()
   val allocate_brtag  = Bool()
   val inst_unique     = Bool()
   val flush_on_commit = Bool()
   val csr_cmd         = UInt(width = rocket.CSR.SZ)
   val rocc            = Bool()

   def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
      val decoder = rocket.DecodeLogic(inst, XDecode.decode_default, table)
      val sigs =
         Seq(legal, fp_val, fp_single, uopc, fu_code, dst_type, rs1_type
         , rs2_type, frs3_en, imm_sel, is_load, is_store, is_amo
         , is_fence, is_fencei, mem_cmd, mem_typ, wakeup_delay, bypassable
         , br_or_jmp, is_jal, allocate_brtag, inst_unique, flush_on_commit, csr_cmd)
      sigs zip decoder map {case(s,d) => s := d}
      rocc := Bool(false)
      this
   }
}


object XDecode extends DecodeConstants
{
// scalastyle:off
            //                                                         frs3_en                                wakeup_delay
            //     is val inst?                                        |  imm sel                             |        bypassable (aka, known/fixed latency)
            //     |  is fp inst?                                      |  |     is_load                       |        |  br/jmp
            //     |  |  is single-prec?               rs1 regtype     |  |     |  is_store                   |        |  |  is jal
            //     |  |  |  micro-code                 |       rs2 type|  |     |  |  is_amo                  |        |  |  |  allocate_brtag
            //     |  |  |  |         func unit        |       |       |  |     |  |  |  is_fence             |        |  |  |  |
            //     |  |  |  |         |                |       |       |  |     |  |  |  |  is_fencei         |        |  |  |  |
            //     |  |  |  |         |        dst     |       |       |  |     |  |  |  |  |  mem    mem     |        |  |  |  |  is unique? (clear pipeline for it)
            //     |  |  |  |         |        regtype |       |       |  |     |  |  |  |  |  cmd    msk     |        |  |  |  |  |  flush on commit
            //     |  |  |  |         |        |       |       |       |  |     |  |  |  |  |  |      |       |        |  |  |  |  |  |  csr cmd
   val table: Array[(BitPat, List[BitPat])] = Array(// |       |       |  |     |  |  |  |  |  |      |       |        |  |  |  |  |  |  |
   LD      -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_D , UInt(3), N, N, N, N, N, N, CSR.N),
   LW      -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_W , UInt(3), N, N, N, N, N, N, CSR.N),
   LWU     -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_WU, UInt(3), N, N, N, N, N, N, CSR.N),
   LH      -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_H , UInt(3), N, N, N, N, N, N, CSR.N),
   LHU     -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_HU, UInt(3), N, N, N, N, N, N, CSR.N),
   LB      -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_B , UInt(3), N, N, N, N, N, N, CSR.N),
   LBU     -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_BU, UInt(3), N, N, N, N, N, N, CSR.N),

   SD      -> List(Y, N, X, uopSTA  , FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, N, N, M_XWR, MSK_D , UInt(0), N, N, N, N, N, N, CSR.N),
   SW      -> List(Y, N, X, uopSTA  , FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, N, N, M_XWR, MSK_W , UInt(0), N, N, N, N, N, N, CSR.N),
   SH      -> List(Y, N, X, uopSTA  , FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, N, N, M_XWR, MSK_H , UInt(0), N, N, N, N, N, N, CSR.N),
   SB      -> List(Y, N, X, uopSTA  , FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, N, N, M_XWR, MSK_B , UInt(0), N, N, N, N, N, N, CSR.N),

   LUI     -> List(Y, N, X, uopLUI  , FU_ALU , RT_FIX, RT_X  , RT_X  , N, IS_U, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),

   ADDI    -> List(Y, N, X, uopADDI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   ANDI    -> List(Y, N, X, uopANDI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   ORI     -> List(Y, N, X, uopORI  , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   XORI    -> List(Y, N, X, uopXORI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLTI    -> List(Y, N, X, uopSLTI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLTIU   -> List(Y, N, X, uopSLTIU, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLLI    -> List(Y, N, X, uopSLLI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRAI    -> List(Y, N, X, uopSRAI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRLI    -> List(Y, N, X, uopSRLI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),

   ADDIW   -> List(Y, N, X, uopADDIW, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLLIW   -> List(Y, N, X, uopSLLIW, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRAIW   -> List(Y, N, X, uopSRAIW, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRLIW   -> List(Y, N, X, uopSRLIW, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),

   SLL     -> List(Y, N, X, uopSLL  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   ADD     -> List(Y, N, X, uopADD  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SUB     -> List(Y, N, X, uopSUB  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLT     -> List(Y, N, X, uopSLT  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLTU    -> List(Y, N, X, uopSLTU , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   AND     -> List(Y, N, X, uopAND  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   OR      -> List(Y, N, X, uopOR   , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   XOR     -> List(Y, N, X, uopXOR  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRA     -> List(Y, N, X, uopSRA  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRL     -> List(Y, N, X, uopSRL  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),

   ADDW    -> List(Y, N, X, uopADDW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SUBW    -> List(Y, N, X, uopSUBW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLLW    -> List(Y, N, X, uopSLLW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRAW    -> List(Y, N, X, uopSRAW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRLW    -> List(Y, N, X, uopSRLW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),

   MUL     -> List(Y, N, X, uopMUL  , FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   MULH    -> List(Y, N, X, uopMULH , FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   MULHU   -> List(Y, N, X, uopMULHU, FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   MULHSU  -> List(Y, N, X, uopMULHSU,FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   MULW    -> List(Y, N, X, uopMULW , FU_MUL , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   DIV     -> List(Y, N, X, uopDIV  , FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   DIVU    -> List(Y, N, X, uopDIVU , FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   REM     -> List(Y, N, X, uopREM  , FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   REMU    -> List(Y, N, X, uopREMU , FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   DIVW    -> List(Y, N, X, uopDIVW , FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   DIVUW   -> List(Y, N, X, uopDIVUW, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   REMW    -> List(Y, N, X, uopREMW , FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   REMUW   -> List(Y, N, X, uopREMUW, FU_DIV , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   AUIPC   -> List(Y, N, X, uopAUIPC, FU_BRU , RT_FIX, RT_X  , RT_X  , N, IS_U, N, N, N, N, N, M_X  , MSK_X , UInt(1), N, N, N, N, N, N, CSR.N), // use BRU for the PC read
   JAL     -> List(Y, N, X, uopJAL  , FU_BRU , RT_FIX, RT_X  , RT_X  , N, IS_J, N, N, N, N, N, M_X  , MSK_X , UInt(1), N, Y, Y, N, N, N, CSR.N),
   JALR    -> List(Y, N, X, uopJALR , FU_BRU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), N, Y, N, Y, N, N, CSR.N),
   BEQ     -> List(Y, N, X, uopBEQ  , FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, Y, N, N, CSR.N),
   BNE     -> List(Y, N, X, uopBNE  , FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, Y, N, N, CSR.N),
   BGE     -> List(Y, N, X, uopBGE  , FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, Y, N, N, CSR.N),
   BGEU    -> List(Y, N, X, uopBGEU , FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, Y, N, N, CSR.N),
   BLT     -> List(Y, N, X, uopBLT  , FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, Y, N, N, CSR.N),
   BLTU    -> List(Y, N, X, uopBLTU , FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, Y, N, N, CSR.N),

   // I-type, the immediate12 holds the CSR register.
   CSRRW   -> List(Y, N, X, uopCSRRW, FU_CSR , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.W),
   CSRRS   -> List(Y, N, X, uopCSRRS, FU_CSR , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.S),
   CSRRC   -> List(Y, N, X, uopCSRRC, FU_CSR , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.C),

   CSRRWI  -> List(Y, N, X, uopCSRRWI,FU_CSR , RT_FIX, RT_PAS, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.W),
   CSRRSI  -> List(Y, N, X, uopCSRRSI,FU_CSR , RT_FIX, RT_PAS, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.S),
   CSRRCI  -> List(Y, N, X, uopCSRRCI,FU_CSR , RT_FIX, RT_PAS, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.C),

   SFENCE_VM->List(Y, N, X, uopSYSTEM,FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, N, CSR.I),
   SCALL   -> List(Y, N, X, uopSYSTEM,FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, N, CSR.I),
   SBREAK  -> List(Y, N, X, uopSYSTEM,FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, N, CSR.I),
   SRET    -> List(Y, N, X, uopSYSTEM,FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, N, CSR.I),
   MRET    -> List(Y, N, X, uopSYSTEM,FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, N, CSR.I),
   DRET    -> List(Y, N, X, uopSYSTEM,FU_CSR , RT_X  , RT_X  , RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, N, CSR.I),

   WFI     -> List(Y, N, X, uopNOP   ,FU_X   , RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.N), // implemented as a NOP; TODO

   FENCE_I -> List(Y, N, X, uopNOP  , FU_X   , RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, Y, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.N),
   FENCE   -> List(Y, N, X, uopFENCE, FU_MEM , RT_X  , RT_X  , RT_X  , N, IS_X, N, Y, N, Y, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.N), // TODO PERF make fence higher performance
                                                                                                                                                 // currently serializes pipeline
   // A-type
   AMOADD_W-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_ADD, MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N), // TODO make AMOs higherperformance
   AMOXOR_W-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_XOR, MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOSWAP_W->List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_SWAP,MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOAND_W-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_AND, MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOOR_W -> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_OR,  MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMIN_W-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MIN, MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMINU_W->List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MINU,MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMAX_W-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MAX, MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMAXU_W->List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MAXU,MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),

   AMOADD_D-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_ADD, MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOXOR_D-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_XOR, MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOSWAP_D->List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_SWAP,MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOAND_D-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_AND, MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOOR_D -> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_OR,  MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMIN_D-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MIN, MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMINU_D->List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MINU,MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMAX_D-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MAX, MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMAXU_D->List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MAXU,MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),

   LR_W    -> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XLR   , MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N), // TODO optimize LR, SC
   LR_D    -> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XLR   , MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N), // note LR generates 2 micro-ops,
   SC_W    -> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XSC   , MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N), // one which isn't needed
   SC_D    -> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XSC   , MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N)
   )
// scalastyle:on
}

object FDecode extends DecodeConstants
{
// scalastyle:off
  val table: Array[(BitPat, List[BitPat])] = Array(
             //                                                          frs3_en                                wakeup_delay
             //                                                          |  imm sel                             |        bypassable (aka, known/fixed latency)
             //                                                          |  |     is_load                       |        |  br/jmp
             //     is val inst?                         rs1 regtype     |  |     |  is_store                   |        |  |  is jal
             //     |  is fp inst?                       |       rs2 type|  |     |  |  is_amo                  |        |  |  |  allocate_brtag
             //     |  |  is dst single-prec?            |       |       |  |     |  |  |  is_fence             |        |  |  |  |
             //     |  |  |  micro-opcode                |       |       |  |     |  |  |  |  is_fencei         |        |  |  |  |
             //     |  |  |  |           func    dst     |       |       |  |     |  |  |  |  |  mem    mem     |        |  |  |  |  is unique? (clear pipeline for it)
             //     |  |  |  |           unit    regtype |       |       |  |     |  |  |  |  |  cmd    msk     |        |  |  |  |  |  flush on commit
             //     |  |  |  |           |       |       |       |       |  |     |  |  |  |  |  |      |       |        |  |  |  |  |  |  csr cmd
   FLW      -> List(Y, Y, Y, uopLD     , FU_MEM, RT_FLT, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_W , UInt(0), N, N, N, N, N, N, CSR.N),
   FLD      -> List(Y, Y, N, uopLD     , FU_MEM, RT_FLT, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_D , UInt(0), N, N, N, N, N, N, CSR.N),
   FSW      -> List(Y, Y, Y, uopSTA    , FU_MEM, RT_X  , RT_FIX, RT_FLT, N, IS_S, N, Y, N, N, N, M_XWR, MSK_W , UInt(0), N, N, N, N, N, N, CSR.N),
   FSD      -> List(Y, Y, N, uopSTA    , FU_MEM, RT_X  , RT_FIX, RT_FLT, N, IS_S, N, Y, N, N, N, M_XWR, MSK_D , UInt(0), N, N, N, N, N, N, CSR.N),

   FCLASS_S-> List(Y, Y, Y, uopFCLASS_S,FU_FPU, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCLASS_D-> List(Y, Y, N, uopFCLASS_D,FU_FPU, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FMV_S_X -> List(Y, Y, Y, uopFMV_S_X, FU_FPU, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMV_D_X -> List(Y, Y, N, uopFMV_D_X, FU_FPU, RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMV_X_S -> List(Y, Y, Y, uopFMV_X_S, FU_FPU, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMV_X_D -> List(Y, Y, N, uopFMV_X_D, FU_FPU, RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FSGNJ_S -> List(Y, Y, Y, uopFSGNJ_S, FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSGNJ_D -> List(Y, Y, N, uopFSGNJ_D, FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSGNJX_S-> List(Y, Y, Y, uopFSGNJ_S, FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSGNJX_D-> List(Y, Y, N, uopFSGNJ_D, FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSGNJN_S-> List(Y, Y, Y, uopFSGNJ_S, FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSGNJN_D-> List(Y, Y, N, uopFSGNJ_D, FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   // FP to FP
   FCVT_S_D-> List(Y, Y, Y, uopFCVT_S_D,FU_FPU, RT_FLT, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_D_S-> List(Y, Y, N, uopFCVT_D_S,FU_FPU, RT_FLT, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   // Int to FP
   FCVT_S_W-> List(Y, Y, Y, uopFCVT_S_W ,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_S_WU->List(Y, Y, Y, uopFCVT_S_WU,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_S_L-> List(Y, Y, Y, uopFCVT_S_L ,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_S_LU->List(Y, Y, Y, uopFCVT_S_LU,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FCVT_D_W-> List(Y, Y, N, uopFCVT_D_W ,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_D_WU->List(Y, Y, N, uopFCVT_D_WU,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_D_L-> List(Y, Y, N, uopFCVT_D_L ,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_D_LU->List(Y, Y, N, uopFCVT_D_LU,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   // FP to Int
   FCVT_W_S-> List(Y, Y, Y, uopFCVT_W_S ,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_WU_S->List(Y, Y, Y, uopFCVT_WU_S,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_L_S-> List(Y, Y, Y, uopFCVT_L_S ,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_LU_S->List(Y, Y, Y, uopFCVT_LU_S,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FCVT_W_D-> List(Y, Y, N, uopFCVT_W_D ,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_WU_D->List(Y, Y, N, uopFCVT_WU_D,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_L_D-> List(Y, Y, N, uopFCVT_L_D ,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_LU_D->List(Y, Y, N, uopFCVT_LU_D,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   // "fp_single" is used for wb_data formatting (and debugging)
   FEQ_S    ->List(Y, Y, Y, uopFEQ_S  , FU_FPU, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FLT_S    ->List(Y, Y, Y, uopFLT_S  , FU_FPU, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FLE_S    ->List(Y, Y, Y, uopFLE_S  , FU_FPU, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FEQ_D    ->List(Y, Y, N, uopFEQ_D  , FU_FPU, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FLT_D    ->List(Y, Y, N, uopFLT_D  , FU_FPU, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FLE_D    ->List(Y, Y, N, uopFLE_D  , FU_FPU, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FMIN_S   ->List(Y, Y, Y, uopFMIN_S , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMAX_S   ->List(Y, Y, Y, uopFMAX_S , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMIN_D   ->List(Y, Y, N, uopFMIN_D , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMAX_D   ->List(Y, Y, N, uopFMAX_D , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FADD_S   ->List(Y, Y, Y, uopFADD_S , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSUB_S   ->List(Y, Y, Y, uopFSUB_S , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMUL_S   ->List(Y, Y, Y, uopFMUL_S , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FADD_D   ->List(Y, Y, N, uopFADD_D , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSUB_D   ->List(Y, Y, N, uopFSUB_D , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMUL_D   ->List(Y, Y, N, uopFMUL_D , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FMADD_S  ->List(Y, Y, Y, uopFMADD_S, FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMSUB_S  ->List(Y, Y, Y, uopFMSUB_S, FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FNMADD_S ->List(Y, Y, Y, uopFNMADD_S,FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FNMSUB_S ->List(Y, Y, Y, uopFNMSUB_S,FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMADD_D  ->List(Y, Y, N, uopFMADD_D, FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMSUB_D  ->List(Y, Y, N, uopFMSUB_D, FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FNMADD_D ->List(Y, Y, N, uopFNMADD_D,FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FNMSUB_D ->List(Y, Y, N, uopFNMSUB_D,FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N)
   )

// scalastyle:on
}

object FDivSqrtDecode extends DecodeConstants
{
// scalastyle:off
  val table: Array[(BitPat, List[BitPat])] = Array(
             //                                                          frs3_en                                wakeup_delay
             //                                                          |  imm sel                             |        bypassable (aka, known/fixed latency)
             //                                                          |  |     is_load                       |        |  br/jmp
             //     is val inst?                         rs1 regtype     |  |     |  is_store                   |        |  |  is jal
             //     |  is fp inst?                       |       rs2 type|  |     |  |  is_amo                  |        |  |  |  allocate_brtag
             //     |  |  is dst single-prec?            |       |       |  |     |  |  |  is_fence             |        |  |  |  |
             //     |  |  |  micro-opcode                |       |       |  |     |  |  |  |  is_fencei         |        |  |  |  |
             //     |  |  |  |           func    dst     |       |       |  |     |  |  |  |  |  mem    mem     |        |  |  |  |  is unique? (clear pipeline for it)
             //     |  |  |  |           unit    regtype |       |       |  |     |  |  |  |  |  cmd    msk     |        |  |  |  |  |  flush on commit
             //     |  |  |  |           |       |       |       |       |  |     |  |  |  |  |  |      |       |        |  |  |  |  |  |  csr cmd
   FDIV_S    ->List(Y, Y, Y, uopFDIV_S , FU_FDV, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FDIV_D    ->List(Y, Y, N, uopFDIV_D , FU_FDV, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSQRT_S   ->List(Y, Y, Y, uopFSQRT_S, FU_FDV, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSQRT_D   ->List(Y, Y, N, uopFSQRT_D, FU_FDV, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N)
   )
// scalastyle:on
}


class DecodeUnitIo(implicit p: Parameters) extends BoomBundle()(p)
{
   val enq = new Bundle { val uop = new MicroOp().asInput }
   val deq = new Bundle { val uop = new MicroOp().asOutput }

   // from CSRFile
   val status = new rocket.MStatus().asInput
   val csr_xcpt = Bool(INPUT)
   val interrupt = Bool(INPUT)
   val interrupt_cause = UInt(INPUT, xLen)
   
   override def cloneType: this.type = new DecodeUnitIo()(p).asInstanceOf[this.type]
}

// Takes in a single instruction, generates a MicroOp (or multiply micro-ops over x cycles)
class DecodeUnit(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new DecodeUnitIo

   val uop = Wire(new MicroOp())
   uop := io.enq.uop

   var decode_table = XDecode.table
   if (usingFPU) decode_table ++= FDecode.table
   if (usingFPU && usingFDivSqrt) decode_table ++= FDivSqrtDecode.table

   val cs = Wire(new CtrlSigs()).decode(uop.inst, decode_table)

   // Exception Handling
   val id_illegal_insn = !cs.legal ||
      cs.fp_val && !io.status.fs.orR ||
      cs.rocc && !io.status.xs.orR

   def checkExceptions(x: Seq[(Bool, UInt)]) =
      (x.map(_._1).reduce(_||_), PriorityMux(x))

   val (xcpt_valid, xcpt_cause) = checkExceptions(List(
      (io.interrupt,     io.interrupt_cause),
      (uop.replay_if,    MINI_EXCEPTION_REPLAY),
      (uop.xcpt_if,      UInt(Causes.fault_fetch)),
      (id_illegal_insn,  UInt(Causes.illegal_instruction))))

   uop.exception := xcpt_valid
   uop.exc_cause := xcpt_cause

   //-------------------------------------------------------------

   uop.uopc       := cs.uopc
   uop.fu_code    := cs.fu_code

   // x-registers placed in 0-31, f-registers placed in 32-63.
   // This allows us to straight-up compare register specifiers and not need to
   // verify the rtypes (e.g., bypassing in rename).
   uop.ldst       := Cat(cs.dst_type === RT_FLT, uop.inst(RD_MSB,RD_LSB))
   uop.lrs1       := Cat(cs.rs1_type === RT_FLT, uop.inst(RS1_MSB,RS1_LSB))
   uop.lrs2       := Cat(cs.rs2_type === RT_FLT, uop.inst(RS2_MSB,RS2_LSB))
   uop.lrs3       := Cat(Bool(true),             uop.inst(RS3_MSB,RS3_LSB))
   // TODO do I need to remove (uop.lrs3) for integer-only? Or do synthesis tools properly remove it?

   uop.ldst_val   := (cs.dst_type =/= RT_X && uop.ldst =/= UInt(0))
   uop.dst_rtype  := cs.dst_type
   uop.lrs1_rtype := cs.rs1_type
   uop.lrs2_rtype := cs.rs2_type
   uop.frs3_en    := cs.frs3_en

   uop.fp_val     := cs.fp_val
   uop.fp_single  := cs.fp_single // TODO use this signal instead of the FPU decode's table signal?

   uop.mem_cmd    := cs.mem_cmd
   uop.mem_typ    := cs.mem_typ
   uop.is_load    := cs.is_load
   uop.is_store   := cs.is_store
   uop.is_amo     := cs.is_amo
   uop.is_fence   := cs.is_fence
   uop.is_fencei  := cs.is_fencei
   uop.is_unique  := cs.inst_unique
   uop.flush_on_commit := cs.flush_on_commit

   uop.wakeup_delay := cs.wakeup_delay
   uop.bypassable   := cs.bypassable

   //-------------------------------------------------------------
   // immediates

   // repackage the immediate, and then pass the fewest number of bits around
   val di24_20 = Mux(cs.imm_sel === IS_B || cs.imm_sel === IS_S, uop.inst(11,7), uop.inst(24,20))
   uop.imm_packed := Cat(uop.inst(31,25), di24_20, uop.inst(19,12))

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
}


class BranchDecode extends Module
{
   val io = new Bundle
   {
      val inst    = UInt(INPUT, 32)
      val is_br   = Bool(OUTPUT)
      val is_jal  = Bool(OUTPUT)
      val is_jalr = Bool(OUTPUT)
   }

   val bpd_csignals =
      rocket.DecodeLogic(io.inst,
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


class FetchSerializerResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val uops = Vec(DECODE_WIDTH, new MicroOp())
   val pred_resp = new BranchPredictionResp()
}
class FetchSerializerIO(implicit p: Parameters) extends BoomBundle()(p)
{
   val enq = new DecoupledIO(new FetchBundle()).flip
   val deq = new DecoupledIO(new FetchSerializerResp)
   val kill = Bool(INPUT)
}


// TODO horrific hodgepodge, needs refactoring
// connect a N-word wide Fetch Buffer with a M-word decode
// currently only works for 2 wide fetch to 1 wide decode, OR N:N fetch/decode
// TODO instead of counter, clear mask bits as instructions are finished?
class FetchSerializerNtoM(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new FetchSerializerIO

   val counter = Reg(init = UInt(0, log2Up(FETCH_WIDTH)))
   val inst_idx = Wire(UInt())
   inst_idx := UInt(0)

   //-------------------------------------------------------------
   // Compute index for where to get the instruction
   when (counter === UInt(1))
   {
      inst_idx := UInt(1)
   }
   .otherwise
   {
      inst_idx := Mux(io.enq.bits.mask === UInt(2), UInt(1), UInt(0))
   }

   //-------------------------------------------------------------
   // Compute Enqueue Ready (get the next bundle)
   io.enq.ready := io.deq.ready &&
                     (io.enq.bits.mask =/= UInt(3) || (counter === UInt(1)))


   //-------------------------------------------------------------
   // Compute Counter
   when (io.kill || io.enq.ready)
   {
      // reset counter on every new bundle
      counter := UInt(0)
   }
   .elsewhen (io.deq.valid && io.deq.ready)
   {
      counter := counter + UInt(1)
   }


   //-------------------------------------------------------------
   // override all the above logic for FW==1
   if (FETCH_WIDTH == 1)
   {
      inst_idx := UInt(0)
      io.enq.ready := io.deq.ready
   }

   io.deq.bits.uops(0).pc             := io.enq.bits.pc
   io.deq.bits.uops(0).fetch_pc_lob   := io.enq.bits.pc
   io.deq.bits.uops(0).inst           := io.enq.bits.insts(inst_idx)
   io.deq.bits.uops(0).br_prediction  := io.enq.bits.predictions(inst_idx)
   io.deq.bits.uops(0).valid          := io.enq.bits.mask(inst_idx)
   io.deq.bits.uops(0).xcpt_if        := io.enq.bits.xcpt_if
   io.deq.bits.uops(0).replay_if        := io.enq.bits.replay_if
   io.deq.bits.uops(0).debug_events   := io.enq.bits.debug_events(inst_idx)

   //-------------------------------------------------------------
   // override all the above logic for DW>1
   // assume FW is also DW, and pass everything through
   if ((DECODE_WIDTH == FETCH_WIDTH) && (FETCH_WIDTH > 1))
   {
      // 1:1, so pass everything straight through!
      for (i <- 0 until DECODE_WIDTH)
      {
         io.deq.bits.uops(i).valid          := io.enq.bits.mask(i)
         io.deq.bits.uops(i).pc             := (io.enq.bits.pc.toSInt & SInt(-(FETCH_WIDTH*coreInstBytes))).toUInt + UInt(i << 2)
         io.deq.bits.uops(i).fetch_pc_lob   := io.enq.bits.pc
         io.deq.bits.uops(i).inst           := io.enq.bits.insts(i)
         io.deq.bits.uops(i).xcpt_if        := io.enq.bits.xcpt_if
         io.deq.bits.uops(i).replay_if        := io.enq.bits.replay_if
         io.deq.bits.uops(i).br_prediction  := io.enq.bits.predictions(i)
         io.deq.bits.uops(i).debug_events   := io.enq.bits.debug_events(i)
      }
      io.enq.ready := io.deq.ready
   }

   // Pipe valid straight through, since conceptually,
   // we are just an extension of the Fetch Buffer
   io.deq.valid := io.enq.valid
   io.deq.bits.pred_resp := io.enq.bits.pred_resp

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
   val io = new Bundle
   {
      // guess if the uop is a branch (we'll catch this later)
      val is_branch = Vec(pl_width, Bool(INPUT))
      // lock in that it's actually a branch and will fire, so we update
      // the branch_masks.
      val will_fire = Vec(pl_width, Bool(INPUT))

      // give out tag immediately (needed in rename)
      // mask can come later in the cycle
      val br_tag    = Vec(pl_width, UInt(OUTPUT, BR_TAG_SZ))
      val br_mask   = Vec(pl_width, UInt(OUTPUT, MAX_BR_COUNT))

       // tell decoders the branch mask has filled up, but on the granularity
       // of an individual micro-op (so some micro-ops can go through)
      val is_full   = Vec(pl_width, Bool(OUTPUT))

      val brinfo         = new BrResolutionInfo().asInput
      val flush_pipeline = Bool(INPUT)

      val debug = new DebugBranchMaskGenerationLogicIO().asOutput
   }

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
}

}
