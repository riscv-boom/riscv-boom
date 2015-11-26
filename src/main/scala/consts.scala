//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Constants
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2011 May 28

package boom
package constants
{

import Chisel._
import Node._

trait BOOMDebugConstants
{
   val DEBUG_PRINTF        = false // use the Chisel printf functionality
   val DEBUG_ENABLE_COLOR  = true  // provide color to print outs? requires a VIM plugin to work properly :(
   val COMMIT_LOG_PRINTF   = false // dump commit state, for comparision against ISA sim

   // turn off stuff to dramatically reduce Chisel node count
   val DEBUG_PRINTF_LSU    = true && DEBUG_PRINTF
   val DEBUG_PRINTF_ROB    = true && DEBUG_PRINTF


   // color codes for output files
   // if you use VIM to view, you'll need the AnsiEsc plugin.
   // 1 is bold, 2 is background, 4 is underlined
   val blk   = if (DEBUG_ENABLE_COLOR) "\u001b[1;30m" else " "
   val red   = if (DEBUG_ENABLE_COLOR) "\u001b[1;31m" else " "
   val grn   = if (DEBUG_ENABLE_COLOR) "\u001b[1;32m" else " "
   val ylw   = if (DEBUG_ENABLE_COLOR) "\u001b[1;33m" else " "
   val blu   = if (DEBUG_ENABLE_COLOR) "\u001b[1;34m" else " "
   val mgt   = if (DEBUG_ENABLE_COLOR) "\u001b[1;35m" else " "
   val cyn   = if (DEBUG_ENABLE_COLOR) "\u001b[1;36m" else " "
   val wht   = if (DEBUG_ENABLE_COLOR) "\u001b[1;37m" else " "
   val end   = if (DEBUG_ENABLE_COLOR) "\u001b[0m"    else " "

   val b_blk = if (DEBUG_ENABLE_COLOR) "\u001b[2;30m" else " "
   val b_red = if (DEBUG_ENABLE_COLOR) "\u001b[2;31m" else " "
   val b_grn = if (DEBUG_ENABLE_COLOR) "\u001b[2;32m" else " "
   val b_ylw = if (DEBUG_ENABLE_COLOR) "\u001b[2;33m" else " "
   val b_blu = if (DEBUG_ENABLE_COLOR) "\u001b[2;34m" else " "
   val b_mgt = if (DEBUG_ENABLE_COLOR) "\u001b[2;35m" else " "
   val b_cyn = if (DEBUG_ENABLE_COLOR) "\u001b[2;36m" else " "
   val b_wht = if (DEBUG_ENABLE_COLOR) "\u001b[2;37m" else " "

   val u_blk = if (DEBUG_ENABLE_COLOR) "\u001b[4;30m" else " "
   val u_red = if (DEBUG_ENABLE_COLOR) "\u001b[4;31m" else " "
   val u_grn = if (DEBUG_ENABLE_COLOR) "\u001b[4;32m" else " "
   val u_ylw = if (DEBUG_ENABLE_COLOR) "\u001b[4;33m" else " "
   val u_blu = if (DEBUG_ENABLE_COLOR) "\u001b[4;34m" else " "
   val u_mgt = if (DEBUG_ENABLE_COLOR) "\u001b[4;35m" else " "
   val u_cyn = if (DEBUG_ENABLE_COLOR) "\u001b[4;36m" else " "
   val u_wht = if (DEBUG_ENABLE_COLOR) "\u001b[4;37m" else " "
}

trait BrPredConstants
{
   val NOT_TAKEN = Bool(false)
   val TAKEN = Bool(true)
}

trait ScalarOpConstants
{
   val X = BitPat("b?")
   val Y = Bool(true)
   val N = Bool(false)

   //************************************
   // Extra Constants
   val WATCHDOG_ERR_NO = 0xffff // tohost error number


   //************************************
   // Control Signals

   val s_invalid :: s_valid_1 :: s_valid_2 :: Nil = Enum(UInt(),3)

   // PC Select Signal
   val PC_PLUS4 = Bits(0, 2)  // PC + 4
   val PC_BRJMP = Bits(1, 2)  // brjmp_target
   val PC_JALR  = Bits(2, 2)  // jump_reg_target

   // Branch Type
   val BR_N   = UInt(0, 4)  // Next
   val BR_NE  = UInt(1, 4)  // Branch on NotEqual
   val BR_EQ  = UInt(2, 4)  // Branch on Equal
   val BR_GE  = UInt(3, 4)  // Branch on Greater/Equal
   val BR_GEU = UInt(4, 4)  // Branch on Greater/Equal Unsigned
   val BR_LT  = UInt(5, 4)  // Branch on Less Than
   val BR_LTU = UInt(6, 4)  // Branch on Less Than Unsigned
   val BR_J   = UInt(7, 4)  // Jump
   val BR_JR  = UInt(8, 4)  // Jump Register

   // RS1 Operand Select Signal
   val OP1_RS1 = UInt(0, 2) // Register Source #1
   val OP1_ZERO= UInt(1, 2)
   val OP1_PC  = UInt(2, 2)
   val OP1_X   = BitPat("b??")

   // RS2 Operand Select Signal
   val OP2_RS2 = UInt(0, 3) // Register Source #2
   val OP2_IMM = UInt(1, 3) // immediate
   val OP2_ZERO= UInt(2, 3) // constant 0
   val OP2_FOUR= UInt(3, 3) // constant 4 (for PC+4)
   val OP2_IMMC= UInt(4, 3) // for CSR imm found in RS1
   val OP2_X   = BitPat("b???")

   // Register File Write Enable Signal
   val REN_0   = Bool(false)
   val REN_1   = Bool(true)

   // Is 32b Word or 64b Doubldword?
   val SZ_DW = 1
   val DW_X   = Bool(true) // Bool(xLen==64)
   val DW_32  = Bool(false)
   val DW_64  = Bool(true)
   val DW_XPR = Bool(true) // Bool(xLen==64)

   // Memory Enable Signal
   val MEN_0   = Bool(false)
   val MEN_1   = Bool(true)
   val MEN_X   = Bool(false)

   // Immediate Extend Select
   val IS_I   = UInt(0, 3)  // I-Type  (LD,ALU)
   val IS_S   = UInt(1, 3)  // S-Type  (ST)
   val IS_B   = UInt(2, 3)  // SB-Type (BR)
   val IS_U   = UInt(3, 3)  // U-Type  (LUI/AUIPC)
   val IS_J   = UInt(4, 3)  // UJ-Type (J/JAL)
   val IS_X   = BitPat("b???")


   // Decode Stage Control Signals
   val RT_FIX   = UInt(0, 2)
   val RT_FLT   = UInt(1, 2)
   val RT_PAS   = UInt(3, 2) // pass-through (pop1 := lrs1, etc)
   val RT_X     = UInt(2, 2) // not-a-register (but shouldn't get a busy-bit, etc.)
                             // TODO rename RT_NAR

   // Micro-op opcodes
   // TODO use an enum
   val UOPC_SZ = 9
   val uopX    = BitPat.DC(UOPC_SZ)
   val uopNOP  = Bits( 0, UOPC_SZ)
   val uopLD   = Bits( 1, UOPC_SZ)
   val uopSTA  = Bits( 2, UOPC_SZ)  // store address generation
   val uopSTD  = Bits( 3, UOPC_SZ)  // store data generation
   val uopLUI  = Bits( 4, UOPC_SZ)

   val uopADDI = Bits( 5, UOPC_SZ)
   val uopANDI = Bits( 6, UOPC_SZ)
   val uopORI  = Bits( 7, UOPC_SZ)
   val uopXORI = Bits( 8, UOPC_SZ)
   val uopSLTI = Bits( 9, UOPC_SZ)
   val uopSLTIU= Bits(10, UOPC_SZ)
   val uopSLLI = Bits(11, UOPC_SZ)
   val uopSRAI = Bits(12, UOPC_SZ)
   val uopSRLI = Bits(13, UOPC_SZ)

   val uopSLL  = Bits(14, UOPC_SZ)
   val uopADD  = Bits(15, UOPC_SZ)
   val uopSUB  = Bits(16, UOPC_SZ)
   val uopSLT  = Bits(17, UOPC_SZ)
   val uopSLTU = Bits(18, UOPC_SZ)
   val uopAND  = Bits(19, UOPC_SZ)
   val uopOR   = Bits(20, UOPC_SZ)
   val uopXOR  = Bits(21, UOPC_SZ)
   val uopSRA  = Bits(22, UOPC_SZ)
   val uopSRL  = Bits(23, UOPC_SZ)

   val uopBEQ  = Bits(24, UOPC_SZ)
   val uopBNE  = Bits(25, UOPC_SZ)
   val uopBGE  = Bits(26, UOPC_SZ)
   val uopBGEU = Bits(27, UOPC_SZ)
   val uopBLT  = Bits(28, UOPC_SZ)
   val uopBLTU = Bits(29, UOPC_SZ)
   val uopCSRRW= Bits(30, UOPC_SZ)
   val uopCSRRS= Bits(31, UOPC_SZ)
   val uopCSRRC= Bits(32, UOPC_SZ)
   val uopCSRRWI=Bits(33, UOPC_SZ)
   val uopCSRRSI=Bits(34, UOPC_SZ)
   val uopCSRRCI=Bits(35, UOPC_SZ)

   val uopJ    = Bits(36, UOPC_SZ)
   val uopJAL  = Bits(37, UOPC_SZ)
   val uopJALR = Bits(38, UOPC_SZ)
   val uopAUIPC= Bits(39, UOPC_SZ)

//   val uopSRET = Bits(40, UOPC_SZ)
   val uopCFLSH= Bits(41, UOPC_SZ)
   val uopFENCE= Bits(42, UOPC_SZ)

   val uopADDIW= Bits(43, UOPC_SZ)
   val uopADDW = Bits(44, UOPC_SZ)
   val uopSUBW = Bits(45, UOPC_SZ)
   val uopSLLIW= Bits(46, UOPC_SZ)
   val uopSLLW = Bits(47, UOPC_SZ)
   val uopSRAIW= Bits(48, UOPC_SZ)
   val uopSRAW = Bits(49, UOPC_SZ)
   val uopSRLIW= Bits(50, UOPC_SZ)
   val uopSRLW = Bits(51, UOPC_SZ)
   val uopMUL  = Bits(52, UOPC_SZ)
   val uopMULH = Bits(53, UOPC_SZ)
   val uopMULHU= Bits(54, UOPC_SZ)
   val uopMULHSU=Bits(55, UOPC_SZ)
   val uopMULW = Bits(56, UOPC_SZ)
   val uopDIV  = Bits(57, UOPC_SZ)
   val uopDIVU = Bits(58, UOPC_SZ)
   val uopREM  = Bits(59, UOPC_SZ)
   val uopREMU = Bits(60, UOPC_SZ)
   val uopDIVW = Bits(61, UOPC_SZ)
   val uopDIVUW= Bits(62, UOPC_SZ)
   val uopREMW = Bits(63, UOPC_SZ)
   val uopREMUW= Bits(64, UOPC_SZ)

   val uopFENCEI    = Bits(65, UOPC_SZ)
   //               = Bits(66, UOPC_SZ)
   val uopAMO_AG    = Bits(67, UOPC_SZ) // AMO-address gen (use normal STD for datagen)

   val uopFMV_S_X   = Bits(68, UOPC_SZ)
   val uopFMV_D_X   = Bits(69, UOPC_SZ)
   val uopFMV_X_S   = Bits(70, UOPC_SZ)
   val uopFMV_X_D   = Bits(71, UOPC_SZ)

   val uopFSGNJ_S   = Bits(72, UOPC_SZ)
   val uopFSGNJ_D   = Bits(73, UOPC_SZ)

   val uopFCVT_S_D  = Bits(74, UOPC_SZ)
   val uopFCVT_D_S  = Bits(75, UOPC_SZ)

   val uopFCVT_S_W  = Bits(76, UOPC_SZ)
   val uopFCVT_S_WU = Bits(77, UOPC_SZ)
   val uopFCVT_S_L  = Bits(78, UOPC_SZ)
   val uopFCVT_S_LU = Bits(79, UOPC_SZ)
   val uopFCVT_D_W  = Bits(80, UOPC_SZ)
   val uopFCVT_D_WU = Bits(81, UOPC_SZ)
   val uopFCVT_D_L  = Bits(82, UOPC_SZ)
   val uopFCVT_D_LU = Bits(83, UOPC_SZ)


   val uopFCVT_W_S  = Bits(84, UOPC_SZ)
   val uopFCVT_WU_S = Bits(85, UOPC_SZ)
   val uopFCVT_L_S  = Bits(86, UOPC_SZ)
   val uopFCVT_LU_S = Bits(87, UOPC_SZ)
   val uopFCVT_W_D  = Bits(88, UOPC_SZ)
   val uopFCVT_WU_D = Bits(89, UOPC_SZ)
   val uopFCVT_L_D  = Bits(90, UOPC_SZ)
   val uopFCVT_LU_D = Bits(91, UOPC_SZ)

   val uopFEQ_S     = Bits(92, UOPC_SZ)
   val uopFLT_S     = Bits(93, UOPC_SZ)
   val uopFLE_S     = Bits(94, UOPC_SZ)
   val uopFEQ_D     = Bits(95, UOPC_SZ)
   val uopFLT_D     = Bits(96, UOPC_SZ)
   val uopFLE_D     = Bits(97, UOPC_SZ)

   val uopFCLASS_S  = Bits(98, UOPC_SZ)
   val uopFCLASS_D  = Bits(99, UOPC_SZ)

   val uopFMIN_S    = Bits(100,UOPC_SZ)
   val uopFMAX_S    = Bits(101,UOPC_SZ)
   val uopFMIN_D    = Bits(102,UOPC_SZ)
   val uopFMAX_D    = Bits(103,UOPC_SZ)

   val uopFADD_S    = Bits(104,UOPC_SZ)
   val uopFSUB_S    = Bits(105,UOPC_SZ)
   val uopFMUL_S    = Bits(106,UOPC_SZ)
   val uopFADD_D    = Bits(107,UOPC_SZ)
   val uopFSUB_D    = Bits(108,UOPC_SZ)
   val uopFMUL_D    = Bits(109,UOPC_SZ)

   val uopFMADD_S   = Bits(110,UOPC_SZ)
   val uopFMSUB_S   = Bits(111,UOPC_SZ)
   val uopFNMADD_S  = Bits(112,UOPC_SZ)
   val uopFNMSUB_S  = Bits(113,UOPC_SZ)
   val uopFMADD_D   = Bits(114,UOPC_SZ)
   val uopFMSUB_D   = Bits(115,UOPC_SZ)
   val uopFNMADD_D  = Bits(116,UOPC_SZ)
   val uopFNMSUB_D  = Bits(117,UOPC_SZ)

   val uopFDIV_S    = Bits(117,UOPC_SZ)
   val uopFDIV_D    = Bits(118,UOPC_SZ)
   val uopFSQRT_S   = Bits(119,UOPC_SZ)
   val uopFSQRT_D   = Bits(120,UOPC_SZ)

   val uopSYSTEM    = Bits(121, UOPC_SZ) // pass uop down the CSR pipeline and let it handle it

   // Memory Mask Type Signal
   val MSK_X   = BitPat("b???")
   val MSK_B   = UInt(0, 3)
   val MSK_H   = UInt(1, 3)
   val MSK_W   = UInt(2, 3)
   val MSK_D   = UInt(3, 3)
   val MSK_BU  = UInt(4, 3)
   val MSK_HU  = UInt(5, 3)
   val MSK_WU  = UInt(6, 3)

   // The Bubble Instruction (Machine generated NOP)
   // Insert (XOR x0,x0,x0) which is different from software compiler
   // generated NOPs which are (ADDI x0, x0, 0).
   // Reasoning for this is to let visualizers and stat-trackers differentiate
   // between software NOPs and machine-generated Bubbles in the pipeline.
   val BUBBLE  = Bits(0x4033, 32)


   def NullMicroOp(): MicroOp =
   {
      val uop = Wire(new MicroOp())
      uop.uopc       := uopNOP // maybe not required, but helps on asserts that try to catch spurious behavior
      uop.bypassable := Bool(false)
      uop.fp_val     := Bool(false)
      uop.is_store   := Bool(false)
      uop.is_load    := Bool(false)
      uop.pdst       := UInt(0)
      uop.dst_rtype  := RT_X
      uop.valid      := Bool(false)
      // TODO these unnecessary? used in regread stage?
      uop.is_br_or_jmp := Bool(false)

      val cs = Wire(new CtrlSignals())
      cs.br_type     := BR_N
      cs.rf_wen      := Bool(false)
      cs.csr_cmd     := rocket.CSR.N
      cs.is_load     := Bool(false)
      cs.is_sta      := Bool(false)
      cs.is_std      := Bool(false)

      uop.ctrl := cs
      uop
   }

}

trait RISCVConstants
{
   // abstract out instruction decode magic numbers
   val RD_MSB  = 11
   val RD_LSB  = 7
   val RS1_MSB = 19
   val RS1_LSB = 15
   val RS2_MSB = 24
   val RS2_LSB = 20
   val RS3_MSB = 31
   val RS3_LSB = 27

   val CSR_ADDR_MSB = 31
   val CSR_ADDR_LSB = 20
   val CSR_ADDR_SZ = 12

   // location of the fifth bit in the shamt (for checking for illegal ops for SRAIW,etc.)
   val SHAMT_5_BIT = 25
   val LONGEST_IMM_SZ = 20
   val X0 = UInt(0)
   val RA = UInt(1) // return address register

   val jal_opc = UInt(0x6f)
   val jalr_opc = UInt(0x67)
   def GetUop(inst: Bits): Bits = inst(6,0)
   def GetRd (inst: Bits): UInt = inst(RD_MSB,RD_LSB)
   def GetRs1(inst: Bits): UInt = inst(RS1_MSB,RS1_LSB)
   def IsCall(inst: Bits): Bool = (inst === rocket.Instructions.JAL || inst === rocket.Instructions.JALR) && GetRd(inst) === RA
   def IsReturn(inst: Bits): Bool = GetUop(inst) === jalr_opc && GetRd(inst) === X0 && GetRs1(inst) === RA

   def ComputeBranchTarget(pc: UInt, inst: Bits, xlen: Int, coreInstBytes: Int): UInt =
   {
      val b_imm32 = Cat(Fill(inst(31),20), inst(7), inst(30,25), inst(11,8), Bits(0,1))
      (pc + Sext(b_imm32, xlen)).toBits & SInt(-coreInstBytes)
   }
   def ComputeJALTarget(pc: UInt, inst: Bits, xlen: Int, coreInstBytes: Int): UInt =
   {
      val j_imm32 = Cat(Fill(inst(31),12), inst(19,12), inst(20), inst(30,25), inst(24,21), Bits(0,1))
      (pc + Sext(j_imm32, xlen)).toBits & SInt(-coreInstBytes)
   }
}

trait ExcCauseConstants
{
   val MINI_EXCEPTION_MEM_ORDERING = UInt(13)
   require (!rocket.Causes.all.contains(13))
}

}

