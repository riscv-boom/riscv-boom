package BOOM
{

import Chisel._
import Node._

import rocket.Instructions._
import rocket._
import FUCode._
import uncore.constants.MemoryOpConstants._

object XDecode
{
   val default = List(N, N, X, uopX    , FU_X   ,UInt("b??",2),UInt("b??",2),UInt("b??",2),X, IS_X, X,X,X,X,N,M_X, MSK_X,UInt("b??"), X, X, X, X, X, X, N, X, CSR.X)
            //                                                         frs3_en                               wakeup_delay
            //     is val inst?                                        | imm sel                             |        bypassable (aka, known/fixed latency)
            //     |  is fp inst?                                      | |     is_load                       |        |  br/jmp
            //     |  |  is single-prec?               rs1 regtype     | |     |  is_store                   |        |  |  is jal
            //     |  |  |  micro-code                 |       rs2 type| |     |  |  is_amo                  |        |  |  |  is sret
            //     |  |  |  |         func unit        |       |       | |     |  |  |  is_fence             |        |  |  |  |  is syscall
            //     |  |  |  |         |                |       |       | |     |  |  |  |  is_fencei         |        |  |  |  |  |  is sbreak
            //     |  |  |  |         |        dst     |       |       | |     |  |  |  |  |  mem    mem     |        |  |  |  |  |  |  is unique? (clear pipeline for it)
            //     |  |  |  |         |        regtype |       |       | |     |  |  |  |  |  cmd    msk     |        |  |  |  |  |  |  |  flush on commit
            //     |  |  |  |         |        |       |       |       | |     |  |  |  |  |  |      |       |        |  |  |  |  |  |  |  |  csr cmd
   val table = Array(//  |  |         |        |       |       |       | |     |  |  |  |  |  |      |       |        |  |  |  |  |  |  |  |  |
   LD      -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_D , UInt(3), N, N, N, N, N, N, N, N, CSR.N),
   LW      -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_W , UInt(3), N, N, N, N, N, N, N, N, CSR.N),
   LWU     -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_WU, UInt(3), N, N, N, N, N, N, N, N, CSR.N),
   LH      -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_H , UInt(3), N, N, N, N, N, N, N, N, CSR.N),
   LHU     -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_HU, UInt(3), N, N, N, N, N, N, N, N, CSR.N),
   LB      -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_B , UInt(3), N, N, N, N, N, N, N, N, CSR.N),
   LBU     -> List(Y, N, X, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_BU, UInt(3), N, N, N, N, N, N, N, N, CSR.N),

   SD      -> List(Y, N, X, uopSTA  , FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, N, N, M_XWR, MSK_D , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   SW      -> List(Y, N, X, uopSTA  , FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, N, N, M_XWR, MSK_W , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   SH      -> List(Y, N, X, uopSTA  , FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, N, N, M_XWR, MSK_H , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   SB      -> List(Y, N, X, uopSTA  , FU_MEM , RT_X  , RT_FIX, RT_FIX, N, IS_S, N, Y, N, N, N, M_XWR, MSK_B , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   LUI     -> List(Y, N, X, uopLUI  , FU_ALU , RT_FIX, RT_X  , RT_X  , N, IS_U, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),

   ADDI    -> List(Y, N, X, uopADDI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   ANDI    -> List(Y, N, X, uopANDI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   ORI     -> List(Y, N, X, uopORI  , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   XORI    -> List(Y, N, X, uopXORI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SLTI    -> List(Y, N, X, uopSLTI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SLTIU   -> List(Y, N, X, uopSLTIU, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SLLI    -> List(Y, N, X, uopSLLI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SRAI    -> List(Y, N, X, uopSRAI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SRLI    -> List(Y, N, X, uopSRLI , FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),

   ADDIW   -> List(Y, N, X, uopADDIW, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SLLIW   -> List(Y, N, X, uopSLLIW, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SRAIW   -> List(Y, N, X, uopSRAIW, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SRLIW   -> List(Y, N, X, uopSRLIW, FU_ALU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),

   SLL     -> List(Y, N, X, uopSLL  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   ADD     -> List(Y, N, X, uopADD  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SUB     -> List(Y, N, X, uopSUB  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SLT     -> List(Y, N, X, uopSLT  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SLTU    -> List(Y, N, X, uopSLTU , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   AND     -> List(Y, N, X, uopAND  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   OR      -> List(Y, N, X, uopOR   , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   XOR     -> List(Y, N, X, uopXOR  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SRA     -> List(Y, N, X, uopSRA  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SRL     -> List(Y, N, X, uopSRL  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),

   ADDW    -> List(Y, N, X, uopADDW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SUBW    -> List(Y, N, X, uopSUBW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SLLW    -> List(Y, N, X, uopSLLW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SRAW    -> List(Y, N, X, uopSRAW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),
   SRLW    -> List(Y, N, X, uopSRLW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, N, CSR.N),

   MUL     -> List(Y, N, X, uopMUL  , FU_MULD, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   MULH    -> List(Y, N, X, uopMULH , FU_MULD, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   MULHU   -> List(Y, N, X, uopMULHU, FU_MULD, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   MULHSU  -> List(Y, N, X, uopMULHSU,FU_MULD, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   MULW    -> List(Y, N, X, uopMULW , FU_MULD, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   DIV     -> List(Y, N, X, uopDIV  , FU_MULD, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   DIVU    -> List(Y, N, X, uopDIVU , FU_MULD, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   REM     -> List(Y, N, X, uopREM  , FU_MULD, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   REMU    -> List(Y, N, X, uopREMU , FU_MULD, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   DIVW    -> List(Y, N, X, uopDIVW , FU_MULD, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   DIVUW   -> List(Y, N, X, uopDIVUW, FU_MULD, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   REMW    -> List(Y, N, X, uopREMW , FU_MULD, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   REMUW   -> List(Y, N, X, uopREMUW, FU_MULD, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   AUIPC   -> List(Y, N, X, uopAUIPC, FU_BRU , RT_FIX, RT_X  , RT_X  , N, IS_U, N, N, N, N, N, M_X  , MSK_X , UInt(1), N, N, N, N, N, N, N, N, CSR.N), // use BRU for the PC read
   JAL     -> List(Y, N, X, uopJAL  , FU_BRU , RT_FIX, RT_X  , RT_X  , N, IS_J, N, N, N, N, N, M_X  , MSK_X , UInt(1), N, Y, Y, N, N, N, N, N, CSR.N),
   JALR    -> List(Y, N, X, uopJALR , FU_BRU , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), N, Y, N, N, N, N, N, N, CSR.N),
   BEQ     -> List(Y, N, X, uopBEQ  , FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, N, N, N, N, N, CSR.N),
   BNE     -> List(Y, N, X, uopBNE  , FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, N, N, N, N, N, CSR.N),
   BGE     -> List(Y, N, X, uopBGE  , FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, N, N, N, N, N, CSR.N),
   BGEU    -> List(Y, N, X, uopBGEU , FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, N, N, N, N, N, CSR.N),
   BLT     -> List(Y, N, X, uopBLT  , FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, N, N, N, N, N, CSR.N),
   BLTU    -> List(Y, N, X, uopBLTU , FU_BRU , RT_X  , RT_FIX, RT_FIX, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, N, N, N, N, N, CSR.N),

   // I-type, the immediate12 holds the CSR register.
   CSRRW   -> List(Y, N, X, uopCSRRW, FU_PCR , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, Y, Y, CSR.W),
   CSRRS   -> List(Y, N, X, uopCSRRS, FU_PCR , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, Y, Y, CSR.S),
   CSRRC   -> List(Y, N, X, uopCSRRC, FU_PCR , RT_FIX, RT_FIX, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, Y, Y, CSR.C),

   CSRRWI  -> List(Y, N, X, uopCSRRWI,FU_PCR , RT_FIX, RT_PAS, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, Y, Y, CSR.W),
   CSRRSI  -> List(Y, N, X, uopCSRRSI,FU_PCR , RT_FIX, RT_PAS, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, Y, Y, CSR.S),
   CSRRCI  -> List(Y, N, X, uopCSRRCI,FU_PCR , RT_FIX, RT_PAS, RT_X  , N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, Y, Y, CSR.C),

   SCALL   -> List(Y, N, X, uopNOP  , FU_ALU , RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, N, Y, N, CSR.N),
   SRET    -> List(Y, N, X, uopSRET , FU_ALU , RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, Y, N, N, Y, N, CSR.N),
   SBREAK  -> List(Y, N, X, uopNOP  , FU_ALU , RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, Y, Y, N, CSR.N),

   FENCE_I -> List(Y, N, X, uopNOP  , FU_X   , RT_X  , RT_X  , RT_X  , N, IS_X, N, N, N, N, Y, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, Y, Y, CSR.N),
   FENCE   -> List(Y, N, X, uopMEMSPECIAL,FU_MEM, RT_X, RT_X, RT_X   , N, IS_X, N, Y, N, Y, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, Y, Y, CSR.N), // TODO PERF make fence higher performance

   // A-type
   AMOADD_W-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_ADD, MSK_W,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N), //TODO make AMOs higherperformance
   AMOXOR_W-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_XOR, MSK_W,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N), //TODO should I mark "Is_store"?
   AMOSWAP_W->List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_SWAP,MSK_W,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOAND_W-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_AND, MSK_W,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOOR_W -> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_OR,  MSK_W,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOMIN_W-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MIN, MSK_W,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOMINU_W->List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MINU,MSK_W,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOMAX_W-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MAX, MSK_W,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOMAXU_W->List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MAXU,MSK_W,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),

   AMOADD_D-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_ADD, MSK_D,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOXOR_D-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_XOR, MSK_D,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOSWAP_D->List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_SWAP,MSK_D,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOAND_D-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_AND, MSK_D,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOOR_D -> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_OR,  MSK_D,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOMIN_D-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MIN, MSK_D,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOMINU_D->List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MINU,MSK_D,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOMAX_D-> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MAX, MSK_D,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),
   AMOMAXU_D->List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XA_MAXU,MSK_D,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N),

   LR_W    -> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XLR   , MSK_W,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N), // TODO optimize LR, SC
   LR_D    -> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XLR   , MSK_D,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N), // note LR generates 2 micro-ops,
   SC_W    -> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XSC   , MSK_W,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N), // one which isn't needed
   SC_D    -> List(Y, N, X, uopAMO_AG, FU_MEM, RT_FIX, RT_FIX, RT_FIX, N, IS_X, N, Y, Y, N, N, M_XSC   , MSK_D,UInt(0),N, N, N, N, N, N, Y, Y, CSR.N)
   )


}

object FDecode extends DecodeConstants
{
  val table = Array(
            //                                                          frs3_en                             wakeup_delay
            //                                                          | imm sel                             |        bypassable (aka, known/fixed latency)
            //                                                          | |     is_load                       |        |  br/jmp
            //     is val inst?                         rs1 regtype     | |     |  is_store                   |        |  |  is jal
            //     |  is fp inst?                       |       rs2 type| |     |  |  is_amo                  |        |  |  |  is sret
            //     |  |  is dst single-prec?            |       |       | |     |  |  |  is_fence             |        |  |  |  |  is syscall
            //     |  |  |  micro-code                  |       |       | |     |  |  |  |  is_fencei         |        |  |  |  |  |  is sbreak
            //     |  |  |  |           func    dst     |       |       | |     |  |  |  |  |  mem    mem     |        |  |  |  |  |  |  is unique? (clear pipeline for it)
            //     |  |  |  |           unit    regtype |       |       | |     |  |  |  |  |  cmd    msk     |        |  |  |  |  |  |  |  flush on commit
            //     |  |  |  |           |       |       |       |       | |     |  |  |  |  |  |      |       |        |  |  |  |  |  |  |  |  csr cmd
   FLW     -> List(Y, Y, Y, uopLD     , FU_MEM, RT_FLT, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_W , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FLD     -> List(Y, Y, N, uopLD     , FU_MEM, RT_FLT, RT_FIX, RT_X  , N, IS_I, Y, N, N, N, N, M_XRD, MSK_D , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FSW     -> List(Y, Y, Y, uopSTA    , FU_MEM, RT_X  , RT_FIX, RT_FLT, N, IS_S, N, Y, N, N, N, M_XWR, MSK_W , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FSD     -> List(Y, Y, N, uopSTA    , FU_MEM, RT_X  , RT_FIX, RT_FLT, N, IS_S, N, Y, N, N, N, M_XWR, MSK_D , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   // TODO consolidate ctrl signals? tons of opc's here
   // not convinced "single-prec" is being used for the fmv
   FCLASS_S-> List(Y, Y, Y, uopFCLASS_S,FU_FPU, RT_FIX, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCLASS_D-> List(Y, Y, N, uopFCLASS_D,FU_FPU, RT_FIX, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),


   FMV_S_X -> List(Y, Y, Y, uopFMV_S_X, FU_FPU, RT_FLT, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FMV_D_X -> List(Y, Y, N, uopFMV_D_X, FU_FPU, RT_FLT, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FMV_X_S -> List(Y, Y, Y, uopFMV_X_S, FU_FPU, RT_FIX, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FMV_X_D -> List(Y, Y, N, uopFMV_X_D, FU_FPU, RT_FIX, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   FSGNJ_S -> List(Y, Y, Y, uopFSGNJ_S, FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FSGNJ_D -> List(Y, Y, Y, uopFSGNJ_D, FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FSGNJX_S-> List(Y, Y, Y, uopFSGNJ_S, FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FSGNJX_D-> List(Y, Y, N, uopFSGNJ_D, FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FSGNJN_S-> List(Y, Y, N, uopFSGNJ_S, FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FSGNJN_D-> List(Y, Y, N, uopFSGNJ_D, FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   // FP to FP
   FCVT_S_D-> List(Y, Y, Y, uopFCVT_S_D,FU_FPU, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCVT_D_S-> List(Y, Y, N, uopFCVT_D_S,FU_FPU, RT_FLT, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   //// Int to FP
   FCVT_S_W-> List(Y, Y, Y, uopFCVT_S_W ,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCVT_S_WU->List(Y, Y, Y, uopFCVT_S_WU,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCVT_S_L-> List(Y, Y, Y, uopFCVT_S_L ,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCVT_S_LU->List(Y, Y, Y, uopFCVT_S_LU,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   FCVT_D_W-> List(Y, Y, N, uopFCVT_D_W ,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCVT_D_WU->List(Y, Y, N, uopFCVT_D_WU,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCVT_D_L-> List(Y, Y, N, uopFCVT_D_L ,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCVT_D_LU->List(Y, Y, N, uopFCVT_D_LU,FU_FPU,RT_FLT, RT_FIX, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   // FP to Int
   FCVT_W_S-> List(Y, Y, Y, uopFCVT_W_S ,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCVT_WU_S->List(Y, Y, Y, uopFCVT_WU_S,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCVT_L_S-> List(Y, Y, Y, uopFCVT_L_S ,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCVT_LU_S->List(Y, Y, Y, uopFCVT_LU_S,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   FCVT_W_D-> List(Y, Y, N, uopFCVT_W_D ,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCVT_WU_D->List(Y, Y, N, uopFCVT_WU_D,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCVT_L_D-> List(Y, Y, N, uopFCVT_L_D ,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FCVT_LU_D->List(Y, Y, N, uopFCVT_LU_D,FU_FPU,RT_FIX, RT_FLT, RT_X  , N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   // "fp_single" is used for wb_data formatting
   FEQ_S    ->List(Y, Y, Y, uopFEQ_S  , FU_FPU, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FLT_S    ->List(Y, Y, Y, uopFLT_S  , FU_FPU, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FLE_S    ->List(Y, Y, Y, uopFLE_S  , FU_FPU, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FEQ_D    ->List(Y, Y, N, uopFEQ_D  , FU_FPU, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FLT_D    ->List(Y, Y, N, uopFLT_D  , FU_FPU, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FLE_D    ->List(Y, Y, N, uopFLE_D  , FU_FPU, RT_FIX, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   FMIN_S   ->List(Y, Y, Y, uopFMIN_S , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FMAX_S   ->List(Y, Y, Y, uopFMAX_S , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FMIN_D   ->List(Y, Y, N, uopFMIN_D , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FMAX_D   ->List(Y, Y, N, uopFMAX_D , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   FADD_S   ->List(Y, Y, Y, uopFADD_S , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FSUB_S   ->List(Y, Y, Y, uopFSUB_S , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FMUL_S   ->List(Y, Y, Y, uopFMUL_S , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FADD_D   ->List(Y, Y, N, uopFADD_D , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FSUB_D   ->List(Y, Y, N, uopFSUB_D , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FMUL_D   ->List(Y, Y, N, uopFMUL_D , FU_FPU, RT_FLT, RT_FLT, RT_FLT, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),

   FMADD_S  ->List(Y, Y, Y, uopFMADD_S, FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FMSUB_S  ->List(Y, Y, Y, uopFMSUB_S, FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FNMADD_S ->List(Y, Y, Y, uopFNMADD_S,FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FNMSUB_S ->List(Y, Y, Y, uopFNMSUB_S,FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FMADD_D  ->List(Y, Y, N, uopFMADD_D, FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FMSUB_D  ->List(Y, Y, N, uopFMSUB_D, FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FNMADD_D ->List(Y, Y, N, uopFNMADD_D,FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N),
   FNMSUB_D ->List(Y, Y, N, uopFNMSUB_D,FU_FPU, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, N, CSR.N)

   )
}


class DecodeUnitIo extends BOOMCoreBundle
{
   val enq = new Bundle
   {
      val uop = new MicroOp
   }.asInput

   val deq = new Bundle
   {
      val uop = new MicroOp
   }.asOutput

   val status = new rocket.Status().asInput
}

// Takes in a single instruction, generates a MicroOp (or multiply micro-ops over x cycles)
class DecodeUnit() extends Module
{
   val io = new DecodeUnitIo

   val uop = new MicroOp()
   uop := io.enq.uop


   var decode_table = XDecode.table
   if (!params(BuildFPU).isEmpty) decode_table ++= FDecode.table

   val dec_csignals = rocket.DecodeLogic(uop.inst,
                                 XDecode.default,
                                 decode_table)

   val (cs_inst_val: Bool) :: (cs_fp_val: Bool)     :: (cs_fp_single: Bool) :: cs_uopc :: cs_fu_code :: cs_dst_type :: cs_rs1_type :: cs_rs2_type :: cs_frs3_en :: cs_imm_sel :: dec_cs0 = dec_csignals
   val (cs_is_load: Bool)  :: (cs_is_store: Bool)   :: (cs_is_amo: Bool)    :: (cs_is_fence: Bool)    :: (cs_is_fencei: Bool)   :: cs_mem_cmd :: cs_mem_typ :: dec_cs1 = dec_cs0
   val cs_wakeup_delay     :: (cs_bypassable: Bool) :: (cs_br_or_jmp: Bool) :: (cs_is_jal: Bool)      :: dec_cs2 = dec_cs1
   val (cs_sret: Bool)     :: (cs_syscall: Bool)    :: (cs_sbreak: Bool)    :: (cs_inst_unique: Bool) :: (cs_flush_on_commit: Bool) :: cs_csr_cmd :: Nil = dec_cs2


   // Exception Handling
   val exc_illegal    = !cs_inst_val

   var exc_interrupts = (0 until io.status.ip.getWidth).map(i => (io.status.im(i) && io.status.ip(i), UInt(BigInt(1) << (params(XprLen)-1) | i)))
   val (exc_interrupt_unmasked, exc_interrupt_cause) = checkExceptions(exc_interrupts)
   val exc_interrupt = io.status.ei && exc_interrupt_unmasked

   def checkExceptions(x: Seq[(Bool, UInt)]) =
      (x.map(_._1).reduce(_||_), PriorityMux(x))


   val fp_csrs = rocket.CSRs.fcsr :: rocket.CSRs.frm :: rocket.CSRs.fflags :: Nil
   val legal_csrs = if (!params(BuildFPU).isEmpty) rocket.CSRs.all.toSet else rocket.CSRs.all.toSet -- fp_csrs

   val raddr1         = uop.inst(RS1_MSB,RS1_LSB)
   val csr_addr       = uop.inst(CSR_ADDR_MSB, CSR_ADDR_LSB)
   val csr_en         = cs_csr_cmd != CSR.N
   val csr_wen        = raddr1 != UInt(0) || !Vec(CSR.S, CSR.C).contains(cs_csr_cmd)
//   val csr_fp         = Bool(!params(BuildFPU).isEmpty) && csr_en && rocket.DecodeLogic(csr_addr, fp_csrs, rocket.CSRs.all.toSet -- fp_csrs) TODO XXX need to throw error if FP is disabled
   val csr_fp         = Bool(false)
   val exc_csr_privileged = csr_en &&
                        (csr_addr(11,10) === UInt(3) && csr_wen ||
                         csr_addr(11,10) === UInt(2) ||
                         csr_addr(11,10) === UInt(1) && !io.status.s ||
                         csr_addr(9,8) >= UInt(2) ||
                         csr_addr(9,8) === UInt(1) && !io.status.s && csr_wen)

   val csr_invalid    = csr_en && !Vec(legal_csrs.map(UInt(_))).contains(csr_addr)

   // flush pipeline on CSR writes that may have side effects
   //val id_csr_flush = {
   //  val safe_csrs = CSRs.sup0 :: CSRs.sup1 :: CSRs.epc :: Nil
   //  cs_csr_en && id_csr_wen && DecodeLogic(id_csr_addr, legal_csrs -- safe_csrs, safe_csrs)
   //}
   val exc_privileged = exc_csr_privileged || (cs_sret.toBool && !(io.status.s))

   val cs_inst_is_fp = cs_fp_val
   val exc_fp_disabled = (cs_inst_is_fp || csr_fp) && !io.status.ef

   uop.sret      := cs_sret.toBool

   uop.exception := cs_syscall.toBool   ||
                       cs_sbreak.toBool ||
                       exc_illegal      ||
                       csr_invalid      ||
                       exc_privileged   ||
                       uop.xcpt_ma      ||
                       uop.xcpt_if      ||
                       exc_interrupt    ||
                       exc_fp_disabled

   // note: priority here is very important
   uop.exc_cause := Mux(exc_interrupt,              exc_interrupt_cause,
                    Mux(uop.xcpt_ma,                UInt(rocket.Causes.misaligned_fetch),
                    Mux(uop.xcpt_if,                UInt(rocket.Causes.fault_fetch),
                    Mux(exc_illegal || csr_invalid, UInt(rocket.Causes.illegal_instruction),
                    Mux(exc_privileged,             UInt(rocket.Causes.privileged_instruction),


//                    ((id_ctrl.fp || id_csr_fp) && !io.dpath.status.ef,UInt(Causes.fp_disabled)),
                    Mux(exc_fp_disabled,            UInt(rocket.Causes.fp_disabled),
                    Mux(cs_syscall.toBool,          UInt(rocket.Causes.syscall),
                    Mux(cs_sbreak.toBool,           UInt(rocket.Causes.breakpoint),
                                                    UInt(0,5)))))))))

   uop.debug_ei_enabled := io.status.ei

   //-------------------------------------------------------------

   uop.uopc       := cs_uopc
   uop.fu_code    := cs_fu_code

   // x-registers placed in 0-31, f-registers placed in 32-63.
   // This allows us to straight-up compare register specifiers and not need to
   // verify the rtypes (e.g., bypassing in rename).
   uop.ldst       := Cat(cs_dst_type === RT_FLT, uop.inst(RD_MSB,RD_LSB))
   uop.lrs1       := Cat(cs_rs1_type === RT_FLT, uop.inst(RS1_MSB,RS1_LSB))
   uop.lrs2       := Cat(cs_rs2_type === RT_FLT, uop.inst(RS2_MSB,RS2_LSB))
   uop.lrs3       := Cat(Bool(true),             uop.inst(RS3_MSB,RS3_LSB)) // TODO do I need to remove this for integer-only?

   uop.ldst_val   := (cs_dst_type != RT_X && (uop.ldst != UInt(0)))
   uop.dst_rtype  := cs_dst_type
   uop.lrs1_rtype := cs_rs1_type
   uop.lrs2_rtype := cs_rs2_type
   uop.frs3_en    := cs_frs3_en

   uop.fp_val     := cs_fp_val
   uop.fp_single  := cs_fp_single

   uop.mem_cmd    := cs_mem_cmd.toUInt
   uop.mem_typ    := cs_mem_typ
   uop.is_load    := cs_is_load
   uop.is_store   := cs_is_store
   uop.is_amo     := cs_is_amo
   uop.is_fence   := cs_is_fence
   uop.is_fencei  := cs_is_fencei
   uop.is_unique  := cs_inst_unique
   uop.flush_on_commit := cs_flush_on_commit

   uop.wakeup_delay := cs_wakeup_delay
   uop.bypassable   := cs_bypassable.toBool

   //-------------------------------------------------------------
   // immediates

   // repackage the immediate, and then pass the fewest number of bits around
   val di24_20 = Mux(cs_imm_sel === IS_B || cs_imm_sel === IS_S, uop.inst(11,7), uop.inst(24,20))
   uop.imm_packed := Cat(uop.inst(31,25), di24_20, uop.inst(19,12))

   //-------------------------------------------------------------

   uop.is_br_or_jmp := cs_br_or_jmp
   uop.is_jal       := cs_is_jal
   uop.is_jump      := cs_is_jal || (uop.uopc === uopJALR)
   uop.is_ret       := (uop.uopc === uopJALR) &&
                       (uop.ldst === X0) &&
                       (uop.lrs1 === RA)
   uop.is_call      := (uop.uopc === uopJALR || uop.uopc === uopJAL) &&
                       (uop.ldst === RA)


   //-------------------------------------------------------------

   io.deq.uop := uop

   //-------------------------------------------------------------

}


class BranchDecode extends Module
{
   val io = new Bundle
   {
      val inst    = Bits(INPUT, 32)
      val is_br   = Bool(OUTPUT)
      val is_jal  = Bool(OUTPUT)
      val is_jalr = Bool(OUTPUT)
   }                      //   is br?
                          //   |  is jal?
                          //   |  |  is jalr?
   val bpd_csignals =     //   |  |  |  br type
      rocket.DecodeLogic(io.inst,//  |  |
                          List(N, N, N, IS_X),
            Array(
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


class FetchSerializerIO() extends BOOMCoreBundle
{
   val enq = new DecoupledIO(new FetchBundle()).flip
   val deq = new DecoupledIO(Vec.fill(DECODE_WIDTH){new MicroOp()})

   val kill = Bool(INPUT)

  override def clone = new FetchSerializerIO().asInstanceOf[this.type]
}



// TODO horrific hodgepodge, needs refactoring
// connect a N-word wide Fetch Buffer with a M-word decode
// currently only works for 2 wide fetch to 1 wide decode, OR N:N fetch/decode
// TODO instead of counter, clear mask bits as instructions are finished?
class FetchSerializerNtoM() extends Module with BOOMCoreParameters
{
   val io = new FetchSerializerIO

   val counter = Reg(init = UInt(0, log2Up(FETCH_WIDTH)))
   val inst_idx = UInt()
   inst_idx := UInt(0)

   //-------------------------------------------------------------
   // Compute index for where to get the instruction
   when (counter === UInt(1))
   {
      inst_idx := UInt(1)
   }
   .otherwise
   {
      inst_idx := Mux(io.enq.bits.mask === Bits(2), UInt(1), UInt(0))
   }

   //-------------------------------------------------------------
   // Compute Enqueue Ready (get the next bundle)
   io.enq.ready := io.deq.ready &&
                     (io.enq.bits.mask != Bits(3) || (counter === UInt(1)))


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

   io.deq.bits(0).pc             := io.enq.bits.pc
   io.deq.bits(0).fetch_pc_lob   := io.enq.bits.pc
   io.deq.bits(0).inst           := io.enq.bits.insts(inst_idx)
   io.deq.bits(0).btb_resp_valid := io.enq.bits.btb_resp_valid
   io.deq.bits(0).btb_hit        := io.enq.bits.btb_resp_valid
   io.deq.bits(0).btb_resp       := io.enq.bits.btb_resp
   io.deq.bits(0).valid          := io.enq.bits.mask(0)
   io.deq.bits(0).xcpt_ma        := io.enq.bits.xcpt_ma(inst_idx)
   io.deq.bits(0).xcpt_if        := io.enq.bits.xcpt_if(inst_idx)



   //-------------------------------------------------------------
   // override all the above logic for DW>1
   // assume FW is also DW, and pass everything through
   if ((DECODE_WIDTH == FETCH_WIDTH) && (FETCH_WIDTH > 1))
   {
      // 1:1, so pass everything straight through!
      for (i <- 0 until DECODE_WIDTH)
      {
         io.deq.bits(i).valid          := io.enq.bits.mask(i)
         io.deq.bits(i).pc             := (io.enq.bits.pc & SInt(-(FETCH_WIDTH*coreInstBytes))) + UInt(i << 2)
         io.deq.bits(i).fetch_pc_lob   := io.enq.bits.pc
         io.deq.bits(i).inst           := io.enq.bits.insts(i)
         io.deq.bits(i).btb_resp_valid := io.enq.bits.btb_resp_valid
         io.deq.bits(i).btb_hit        := Mux(io.enq.bits.btb_pred_taken_idx === UInt(i),
                                                             io.enq.bits.btb_resp_valid,
                                                             Bool(false))
         io.deq.bits(i).btb_resp       := io.enq.bits.btb_resp
         when (io.enq.bits.btb_pred_taken_idx != UInt(i))
         {
            io.deq.bits(i).btb_resp.taken := Bool(false)
         }
         io.deq.bits(i).xcpt_ma := io.enq.bits.xcpt_ma
         io.deq.bits(i).xcpt_if := io.enq.bits.xcpt_if
      }

      io.enq.ready := io.deq.ready
   }

   // Pipe valid straight through, since conceptually,
   // we are just an extension of the Fetch Buffer
   io.deq.valid := io.enq.valid

}


// track the current "branch mask", and give out the branch mask to each micro-op in Decode
// (each micro-op in the machine has a branch mask which says which branches it
// is being speculated under.
class BranchMaskGenerationLogic(val pl_width: Int) extends Module with BOOMCoreParameters
{
   val io = new Bundle
   {
      val will_fire = Vec.fill(pl_width) { Bool(INPUT) }
      val is_branch = Vec.fill(pl_width) { Bool(INPUT) }

      val br_tag    = Vec.fill(pl_width) { UInt(OUTPUT, BR_TAG_SZ) }
      val br_mask   = Vec.fill(pl_width) { Bits(OUTPUT, MAX_BR_COUNT) }


      val is_full   = Vec.fill(pl_width) { Bool(OUTPUT) } // tell decoders the branch
                                                          // mask has filled up, but on
                                                          // the granularity of an
                                                          // individual micro-op (so
                                                          // some micro-ops can go
                                                          // through)

      val brinfo         = new BrResolutionInfo().asInput
      val flush_pipeline = Bool(INPUT)

      val debug = new Bundle {
         val branch_mask = Bits(width = MAX_BR_COUNT)
      }.asOutput()
   }

   val branch_mask       = Reg(init = Bits(0, MAX_BR_COUNT))

   //-------------------------------------------------------------
   // Give out the branch mask and branch tag to each micro-op

   var curr_br_mask = branch_mask

   for (w <- 0 until pl_width)
   {
      io.is_full(w) := (curr_br_mask === ~(Bits(0,MAX_BR_COUNT))) && io.is_branch(w)
      io.br_mask(w) := GetNewBrMask(io.brinfo, curr_br_mask)


      // find br_tag and compute next br_mask
      val new_br_mask = Bits(width = MAX_BR_COUNT)
      new_br_mask := curr_br_mask
      val new_br_tag = UInt(width = BR_TAG_SZ)
      new_br_tag := UInt(0)

      for (i <- MAX_BR_COUNT-1 to 0 by -1)
      {
         when (~curr_br_mask(i))
         {
            new_br_mask := (UInt(1) << UInt(i)) | curr_br_mask
            new_br_tag  := UInt(i)
         }
      }

      io.br_tag(w)  := new_br_tag

      curr_br_mask = Mux(io.is_branch(w) && io.will_fire(w), new_br_mask
                                                           , curr_br_mask)
   }

   //-------------------------------------------------------------
   // Update the current branch_mask

   when (io.flush_pipeline)
   {
      branch_mask := Bits(0)
   }
   .elsewhen (io.brinfo.valid && io.brinfo.mispredict)
   {
      branch_mask := io.brinfo.exe_mask
   }
   .otherwise
   {
      branch_mask := GetNewBrMask(io.brinfo, curr_br_mask)
   }

   io.debug.branch_mask := branch_mask

}

}

