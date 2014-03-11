package BOOM
{

import Chisel._
import Node._

import rocket.Instructions._
import rocket._
import FUCode._
import uncore.constants.MemoryOpConstants._

object Decode
{
   val default =          List(N, uopX    , FU_X   ,UInt("b??",2),UInt("b??",2),UInt("b??",2), IS_X, X,X,X,M_X, MSK_X,UInt("b??"), X, X, X, X, X, N, X, CSR.X)
                        //                                                                                  wakeup_delay
                        //                                                                                  |       bypassable (aka, known/fixed latency)
                        //                                                   imm sel                        |       |  br/jmp
                        //     is val inst?                  rs1 regtype     |     is_load                  |       |  |  is jal
                        //     |  micro-opcode               |       rs2 type|     |  is_store              |       |  |  | is sret
                        //     |  |         func unit        |       |       |     |  |  is_fence           |       |  |  | |  is syscall
                        //     |  |         |        dst     |       |       |     |  |  |  mem    mem      |       |  |  | |  |  is unique? (clear pipeline for it)
                        //     |  |         |        regtype |       |       |     |  |  |  cmd    msk      |       |  |  | |  |  |  flush on commit
                        //     |  |         |        |       |       |       |     |  |  |  |      |        |       |  |  | |  |  |  |  csr cmd
   val table = Array(        
               LD      -> List(Y, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , IS_I, Y, N, N, M_XRD, MSK_D , UInt(3), N, N, N, N, N, N, N, CSR.N),
               LW      -> List(Y, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , IS_I, Y, N, N, M_XRD, MSK_W , UInt(3), N, N, N, N, N, N, N, CSR.N),
               LWU     -> List(Y, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , IS_I, Y, N, N, M_XRD, MSK_WU, UInt(3), N, N, N, N, N, N, N, CSR.N),
               LH      -> List(Y, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , IS_I, Y, N, N, M_XRD, MSK_H , UInt(3), N, N, N, N, N, N, N, CSR.N),
               LHU     -> List(Y, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , IS_I, Y, N, N, M_XRD, MSK_HU, UInt(3), N, N, N, N, N, N, N, CSR.N),
               LB      -> List(Y, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , IS_I, Y, N, N, M_XRD, MSK_B , UInt(3), N, N, N, N, N, N, N, CSR.N),
               LBU     -> List(Y, uopLD   , FU_MEM , RT_FIX, RT_FIX, RT_X  , IS_I, Y, N, N, M_XRD, MSK_BU, UInt(3), N, N, N, N, N, N, N, CSR.N),
               
               SD      -> List(Y, uopSTA  , FU_MEM , RT_X  , RT_FIX, RT_FIX, IS_S, N, Y, N, M_XWR, MSK_D , UInt(0), N, N, N, N, N, N, N, CSR.N),
               SW      -> List(Y, uopSTA  , FU_MEM , RT_X  , RT_FIX, RT_FIX, IS_S, N, Y, N, M_XWR, MSK_W , UInt(0), N, N, N, N, N, N, N, CSR.N),
               SH      -> List(Y, uopSTA  , FU_MEM , RT_X  , RT_FIX, RT_FIX, IS_S, N, Y, N, M_XWR, MSK_H , UInt(0), N, N, N, N, N, N, N, CSR.N),
               SB      -> List(Y, uopSTA  , FU_MEM , RT_X  , RT_FIX, RT_FIX, IS_S, N, Y, N, M_XWR, MSK_B , UInt(0), N, N, N, N, N, N, N, CSR.N),
               
               LUI     -> List(Y, uopLUI  , FU_ALU , RT_FIX, RT_X  , RT_X  , IS_U, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),

               ADDI    -> List(Y, uopADDI , FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               ANDI    -> List(Y, uopANDI , FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               ORI     -> List(Y, uopORI  , FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               XORI    -> List(Y, uopXORI , FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               SLTI    -> List(Y, uopSLTI , FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               SLTIU   -> List(Y, uopSLTIU, FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               SLLI    -> List(Y, uopSLLI , FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               SRAI    -> List(Y, uopSRAI , FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               SRLI    -> List(Y, uopSRLI , FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               
               ADDIW   -> List(Y, uopADDIW, FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               SLLIW   -> List(Y, uopSLLIW, FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               SRAIW   -> List(Y, uopSRAIW, FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               SRLIW   -> List(Y, uopSRLIW, FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),

               SLL     -> List(Y, uopSLL  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N), 
               ADD     -> List(Y, uopADD  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N), 
               SUB     -> List(Y, uopSUB  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N), 
               SLT     -> List(Y, uopSLT  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N), 
               SLTU    -> List(Y, uopSLTU , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N), 
               AND     -> List(Y, uopAND  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N), 
               OR      -> List(Y, uopOR   , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N), 
               XOR     -> List(Y, uopXOR  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N), 
               SRA     -> List(Y, uopSRA  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N), 
               SRL     -> List(Y, uopSRL  , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               
               ADDW    -> List(Y, uopADDW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N), 
               SUBW    -> List(Y, uopSUBW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N), 
               SLLW    -> List(Y, uopSLLW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N), 
               SRAW    -> List(Y, uopSRAW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N), 
               SRLW    -> List(Y, uopSRLW , FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, N, CSR.N),
               
               MUL     -> List(Y, uopMUL  , FU_MULD, RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
               MULH    -> List(Y, uopMULH , FU_MULD, RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
               MULHU   -> List(Y, uopMULHU, FU_MULD, RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
               MULHSU  -> List(Y, uopMULHSU,FU_MULD, RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
               MULW    -> List(Y, uopMULW , FU_MULD, RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

               DIV     -> List(Y, uopDIV  , FU_MULD, RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
               DIVU    -> List(Y, uopDIVU , FU_MULD, RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
               REM     -> List(Y, uopREM  , FU_MULD, RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
               REMU    -> List(Y, uopREMU , FU_MULD, RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
               DIVW    -> List(Y, uopDIVW , FU_MULD, RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
               DIVUW   -> List(Y, uopDIVUW, FU_MULD, RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
               REMW    -> List(Y, uopREMW , FU_MULD, RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, CSR.N),
               REMUW   -> List(Y, uopREMUW, FU_MULD, RT_FIX, RT_FIX, RT_FIX, IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, N, CSR.N),

               AUIPC   -> List(Y, uopAUIPC, FU_BRU , RT_FIX, RT_X  , RT_X  , IS_U, N, N, N, M_X  , MSK_X , UInt(1), N, N, N, N, N, N, N, CSR.N), // use BRU for the PC read
               JAL     -> List(Y, uopJAL  , FU_BRU , RT_FIX, RT_X  , RT_X  , IS_J, N, N, N, M_X  , MSK_X , UInt(1), N, Y, Y, N, N, N, N, CSR.N),
               JALR    -> List(Y, uopJALR , FU_BRU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(1), N, Y, N, N, N, N, N, CSR.N),
               BEQ     -> List(Y, uopBEQ  , FU_BRU , RT_X  , RT_FIX, RT_FIX, IS_B, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, N, N, N, N, CSR.N),
               BNE     -> List(Y, uopBNE  , FU_BRU , RT_X  , RT_FIX, RT_FIX, IS_B, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, N, N, N, N, CSR.N),
               BGE     -> List(Y, uopBGE  , FU_BRU , RT_X  , RT_FIX, RT_FIX, IS_B, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, N, N, N, N, CSR.N),
               BGEU    -> List(Y, uopBGEU , FU_BRU , RT_X  , RT_FIX, RT_FIX, IS_B, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, N, N, N, N, CSR.N),
               BLT     -> List(Y, uopBLT  , FU_BRU , RT_X  , RT_FIX, RT_FIX, IS_B, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, N, N, N, N, CSR.N),
               BLTU    -> List(Y, uopBLTU , FU_BRU , RT_X  , RT_FIX, RT_FIX, IS_B, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, N, N, N, N, CSR.N),
               
               // I-type, the immediate12 holds the CSR register. 
               CSRRW   -> List(Y, uopCSRRW, FU_PCR , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, Y, Y, CSR.W), 
               CSRRS   -> List(Y, uopCSRRS, FU_PCR , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, Y, Y, CSR.S), 
               CSRRC   -> List(Y, uopCSRRC, FU_PCR , RT_FIX, RT_FIX, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, Y, Y, CSR.C), 
               
               CSRRWI  -> List(Y, uopCSRRWI,FU_PCR , RT_FIX, RT_PAS, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, Y, Y, CSR.W), 
               CSRRSI  -> List(Y, uopCSRRSI,FU_PCR , RT_FIX, RT_PAS, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, Y, Y, CSR.S), 
               CSRRCI  -> List(Y, uopCSRRCI,FU_PCR , RT_FIX, RT_PAS, RT_X  , IS_I, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, Y, Y, CSR.C), 

               SCALL   -> List(Y, uopNOP  , FU_ALU , RT_X  , RT_X  , RT_X  , IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, N, CSR.N), 
               SRET    -> List(Y, uopSRET , FU_ALU , RT_X  , RT_X  , RT_X  , IS_X, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, Y, N, Y, N, CSR.N), 

               FENCE_I -> List(Y, uopFENCEI    ,FU_MEM, RT_X, RT_X, RT_X   , IS_X, N, Y, Y, M_X  , MSK_X , UInt(0), N, N, N, N, N, Y, Y, CSR.N), 
               FENCE   -> List(Y, uopMEMSPECIAL,FU_MEM, RT_X, RT_X, RT_X   , IS_X, N, Y, Y, M_X  , MSK_X , UInt(0), N, N, N, N, N, Y, Y, CSR.N) // TODO PERF make fence higher performance
               )
                 

}


class DecodeUnitIo extends Bundle
{
   val enq = new Bundle
   {
      val inst  = Bits(width = XPRLEN)
   }.asInput

   val deq = new Bundle
   {
      val valid = Bool() // not valid if we are getting stalled
      val uop   = new MicroOp()
      val ready = Bool() // we may be busy writing out multiple micro-ops per macro-inst or waiting on ROB to empty
   }.asOutput

   val status    = new rocket.Status().asInput
}

// Takes in a single instruction, generates a MicroOp (or multiply micro-ops over x cycles)
class DecodeUnit(implicit conf: BOOMConfiguration) extends Module
{
   val io = new DecodeUnitIo

   val uop = new MicroOp()
   uop.inst := io.enq.inst

   val dec_csignals = rocket.DecodeLogic(uop.inst, 
                                 Decode.default,
                                 Decode.table)
                                   
   val cs_inst_val :: cs_uopc :: cs_fu_code :: cs_dst_type :: cs_rs1_type :: cs_rs2_type :: cs_imm_sel :: cs_is_load :: cs_is_store :: cs_is_fence :: cs_mem_cmd :: cs_mem_typ :: cs_wakeup_delay :: cs_bypassable :: cs_br_or_jmp :: cs_is_jal :: cs_sret :: cs_syscall :: cs_inst_unique :: cs_flush_on_commit :: cs_csr_cmd :: Nil = dec_csignals
   

   // Exception Handling
   val exc_illegal    = !cs_inst_val 

   val fp_csrs = rocket.CSRs.fcsr :: rocket.CSRs.frm :: rocket.CSRs.fflags :: Nil
   val legal_csrs = if (conf.rc.fpu) rocket.CSRs.all.toSet else rocket.CSRs.all.toSet -- fp_csrs

   val raddr1         = uop.inst(RS1_MSB,RS1_LSB)
   val csr_addr       = uop.inst(CSR_ADDR_MSB, CSR_ADDR_LSB)
   val csr_en         = cs_csr_cmd != CSR.N
   val csr_wen        = raddr1 != UInt(0) || !Vec(CSR.S, CSR.C).contains(cs_csr_cmd)
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

   uop.sret      := cs_sret.toBool
   uop.syscall   := cs_syscall.toBool
   
   uop.exception := cs_syscall.toBool  ||
                       exc_illegal ||
                       exc_privileged
                                             
   uop.exc_cause := Mux(exc_illegal,           UInt(rocket.Causes.illegal_instruction),
                       Mux(exc_privileged,     UInt(rocket.Causes.privileged_instruction),
                       Mux(cs_syscall.toBool,  UInt(rocket.Causes.syscall),
                                               UInt(0,5))))
   
   //-------------------------------------------------------------
    
   uop.uopc       := cs_uopc
   uop.fu_code    := cs_fu_code
     
   uop.ldst       := uop.inst(RD_MSB,RD_LSB).toUInt
   uop.lrs1       := uop.inst(RS1_MSB,RS1_LSB).toUInt
   uop.lrs2       := uop.inst(RS2_MSB,RS2_LSB).toUInt

   uop.ldst_val   := (cs_dst_type != RT_X && (uop.ldst != UInt(0)))
   uop.ldst_rtype := cs_dst_type
   uop.lrs1_rtype := cs_rs1_type
   uop.lrs2_rtype := cs_rs2_type

   uop.mem_cmd    := cs_mem_cmd.toUInt
   uop.mem_typ    := cs_mem_typ
   uop.is_load    := cs_is_load.toBool
   uop.is_store   := cs_is_store.toBool  // fences are considered stores that write to all addresses
   uop.is_fence   := cs_is_fence.toBool
   uop.is_unique  := cs_inst_unique.toBool
   uop.flush_on_commit := cs_flush_on_commit.toBool
   
   uop.wakeup_delay := cs_wakeup_delay
   uop.bypassable   := cs_bypassable.toBool
  
   //-------------------------------------------------------------
   // immediates

   // repackage the immediate, and then pass the fewest number of bits around
   val di24_20 = Mux(cs_imm_sel === IS_B || cs_imm_sel === IS_S, uop.inst(11,7), uop.inst(24,20))
   uop.imm_packed := Cat(uop.inst(31,25), di24_20, uop.inst(19,12))

   //-------------------------------------------------------------
   
   uop.is_br_or_jmp := cs_br_or_jmp.toBool
   
   uop.is_jal := cs_is_jal.toBool
   uop.is_jump:= (uop.uopc === uopJAL) ||
                 (uop.uopc === uopJALR) 
   uop.is_ret := (uop.uopc === uopJALR) &&
                 (uop.ldst === X0) &&
                 (uop.lrs1 === RA)


   //-------------------------------------------------------------

   io.deq.uop   := uop

   //-------------------------------------------------------------

}


class BranchDecode extends Module
{
   val io = new Bundle
   {
      val inst    = Bits(INPUT, 32)

      val is_br   = Bool(OUTPUT)
      val is_jal  = Bool(OUTPUT)
      val brtype  = Bits(OUTPUT, UOPC_SZ)
      val imm_sel = UInt(OUTPUT, IS_X.getWidth)
   }
                          //            is br?
                          //            |  is jal?
   val bpd_csignals =     //            |  |  br type
      rocket.DecodeLogic(io.inst, //    |  |  | 
                          List(uopNOP , N, N, IS_X),
            Array(
               JAL     -> List(uopJAL , N, Y, IS_J),
               JALR    -> List(uopJALR, N, N, IS_I),
               BEQ     -> List(uopBEQ , Y, N, IS_B),
               BNE     -> List(uopBNE , Y, N, IS_B),
               BGE     -> List(uopBGE , Y, N, IS_B),
               BGEU    -> List(uopBGEU, Y, N, IS_B),
               BLT     -> List(uopBLT , Y, N, IS_B),
               BLTU    -> List(uopBLTU, Y, N, IS_B)
            ))

   val brtype_ :: is_br_ :: is_jal_ :: imm_sel_ :: Nil = bpd_csignals

   io.is_br   := is_br_.toBool
   io.is_jal  := is_jal_.toBool
   io.brtype  := brtype_.toBits
   io.imm_sel := imm_sel_
}


class FetchSerializerIO(implicit conf: BOOMConfiguration) extends Bundle
{
   val enq = new DecoupledIO(new FetchBundle()).flip
   val deq = new DecoupledIO(Vec.fill(DECODE_WIDTH){new MicroOp()}) 
      
//   val stall = Bits(INPUT, DECODE_WIDTH)

   val kill = Bool(INPUT) 

  override def clone = new FetchSerializerIO().asInstanceOf[this.type]
}



// TODO horrific hodgepodge, needs refactoring
// connect a N-word wide Fetch Buffer with a M-word decode
// currently only works for 2 wide fetch to 1 wide decode, OR N:N fetch/decode
// TODO instead of counter, clear mask bits as instructions are finished?
class FetchSerializerNtoM(implicit conf: BOOMConfiguration) extends Module
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

   io.deq.bits(0).pc             := io.enq.bits.pc + Mux(inst_idx.orR,UInt(4),UInt(0))
   io.deq.bits(0).inst           := io.enq.bits.insts(inst_idx)
   io.deq.bits(0).br_prediction  := io.enq.bits.br_predictions(inst_idx)
   io.deq.bits(0).btb_pred_taken := io.enq.bits.btb_pred_taken
   io.deq.bits(0).valid          := io.enq.bits.mask(0)



   //-------------------------------------------------------------
   // override all the above logic for DW>1
   // assume FW is also DW, and pass everything through
   if ((DECODE_WIDTH == FETCH_WIDTH) && (FETCH_WIDTH > 1))
   {
      // 1:1, so pass everything straight through!
      for (i <- 0 until DECODE_WIDTH)
      {
         io.deq.bits(i).pc := io.enq.bits.pc + UInt(i << 2)
         io.deq.bits(i).inst := io.enq.bits.insts(i)
         io.deq.bits(i).br_prediction  := io.enq.bits.br_predictions(i)
         io.deq.bits(i).btb_pred_taken := Mux(io.enq.bits.btb_pred_taken_idx === UInt(i), 
                                                             io.enq.bits.btb_pred_taken, 
                                                             Bool(false))
         io.deq.bits(i).valid := io.enq.bits.mask(i)
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
class BranchMaskGenerationLogic(val pl_width: Int) extends Module
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

}

}

