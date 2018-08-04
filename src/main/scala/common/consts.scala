
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

package boom.common
package constants
{

import Chisel._
import freechips.rocketchip.config.Parameters

import freechips.rocketchip.util.Str

trait BOOMDebugConstants
{
   val DEBUG_PRINTF        = true // use the Chisel printf functionality
   val COMMIT_LOG_PRINTF   = true // dump commit state, for comparision against ISA sim
   val O3PIPEVIEW_PRINTF   = false // dump trace for O3PipeView from gem5
   val O3_CYCLE_TIME       = (1000)// "cycle" time expected by o3pipeview.py

   // When enabling DEBUG_PRINTF, the vertical whitespace can be padded out
   // such that viewing the *.out file in vim can line up veritically to
   // enable ctrl+f/ctrl+b to advance the *.out file one cycle without
   // moving the structures.
   val debugScreenheight  = 81

   // turn off stuff to dramatically reduce Chisel node count
   val DEBUG_PRINTF_LSU    = true  && DEBUG_PRINTF
   val DEBUG_PRINTF_ROB    = true  && DEBUG_PRINTF
   val DEBUG_PRINTF_TAGE   = false && DEBUG_PRINTF
   val DEBUG_PRINTF_FTQ    = false && DEBUG_PRINTF

   if (O3PIPEVIEW_PRINTF) require (!DEBUG_PRINTF && !COMMIT_LOG_PRINTF)
}

trait BrPredConstants
{
   val NOT_TAKEN = Bool(false)
   val TAKEN = Bool(true)
}

trait IQType
{
   val IQT_SZ  = 2
   val IQT_INT = UInt(0, IQT_SZ)
   val IQT_MEM = UInt(1, IQT_SZ)
   val IQT_FP  = UInt(2, IQT_SZ)
   val IQT_VEC = UInt(3, IQT_SZ)
}

trait ScalarOpConstants
{
   val X = BitPat("b?")
   val Y = BitPat("b1")
   val N = BitPat("b0")

   //************************************
   // Extra Constants


   //************************************
   // Control Signals

   // PC Select Signal
   val PC_PLUS4 = UInt(0, 2)  // PC + 4
   val PC_BRJMP = UInt(1, 2)  // brjmp_target
   val PC_JALR  = UInt(2, 2)  // jump_reg_target

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
   val IS_V   = UInt(5, 3)  // V-Type  (VADDI, etc.)
   val IS_X   = BitPat("b???")


   // Decode Stage Control Signals
   val RT_SZ = 3
   val RT_FIX   = UInt(0, RT_SZ)
   val RT_FLT   = UInt(1, RT_SZ)
   val RT_VEC   = UInt(4, RT_SZ)
   val RT_POLY  = UInt(5, RT_SZ) // Used only in decode to decode vector polymorphic instructions
   val RT_VPRED = UInt(6, RT_SZ) // Used only has a hack for rename hardware, this should never be the value in MicroOp
   val RT_PAS   = UInt(3, RT_SZ) // pass-through (pop1 := lrs1, etc)
   val RT_X     = UInt(2, RT_SZ) // not-a-register (but shouldn't get a busy-bit, etc.)
                             // TODO rename RT_NAR

   // Micro-op opcodes
   // TODO change micro-op opcodes into using enum
   val UOPC_SZ = 9
   val uopX    = BitPat.dontCare(UOPC_SZ)
   val uopNOP  = UInt( 0, UOPC_SZ)
   val uopLD   = UInt( 1, UOPC_SZ)
   val uopSTA  = UInt( 2, UOPC_SZ)  // store address generation
   val uopSTD  = UInt( 3, UOPC_SZ)  // store data generation
   val uopLUI  = UInt( 4, UOPC_SZ)

   val uopADDI = UInt( 5, UOPC_SZ)
   val uopANDI = UInt( 6, UOPC_SZ)
   val uopORI  = UInt( 7, UOPC_SZ)
   val uopXORI = UInt( 8, UOPC_SZ)
   val uopSLTI = UInt( 9, UOPC_SZ)
   val uopSLTIU= UInt(10, UOPC_SZ)
   val uopSLLI = UInt(11, UOPC_SZ)
   val uopSRAI = UInt(12, UOPC_SZ)
   val uopSRLI = UInt(13, UOPC_SZ)

   val uopSLL  = UInt(14, UOPC_SZ)
   val uopADD  = UInt(15, UOPC_SZ)
   val uopSUB  = UInt(16, UOPC_SZ)
   val uopSLT  = UInt(17, UOPC_SZ)
   val uopSLTU = UInt(18, UOPC_SZ)
   val uopAND  = UInt(19, UOPC_SZ)
   val uopOR   = UInt(20, UOPC_SZ)
   val uopXOR  = UInt(21, UOPC_SZ)
   val uopSRA  = UInt(22, UOPC_SZ)
   val uopSRL  = UInt(23, UOPC_SZ)

   val uopBEQ  = UInt(24, UOPC_SZ)
   val uopBNE  = UInt(25, UOPC_SZ)
   val uopBGE  = UInt(26, UOPC_SZ)
   val uopBGEU = UInt(27, UOPC_SZ)
   val uopBLT  = UInt(28, UOPC_SZ)
   val uopBLTU = UInt(29, UOPC_SZ)
   val uopCSRRW= UInt(30, UOPC_SZ)
   val uopCSRRS= UInt(31, UOPC_SZ)
   val uopCSRRC= UInt(32, UOPC_SZ)
   val uopCSRRWI=UInt(33, UOPC_SZ)
   val uopCSRRSI=UInt(34, UOPC_SZ)
   val uopCSRRCI=UInt(35, UOPC_SZ)

   val uopJ    = UInt(36, UOPC_SZ)
   val uopJAL  = UInt(37, UOPC_SZ)
   val uopJALR = UInt(38, UOPC_SZ)
   val uopAUIPC= UInt(39, UOPC_SZ)

//   val uopSRET = UInt(40, UOPC_SZ)
   val uopCFLSH= UInt(41, UOPC_SZ)
   val uopFENCE= UInt(42, UOPC_SZ)

   val uopADDIW= UInt(43, UOPC_SZ)
   val uopADDW = UInt(44, UOPC_SZ)
   val uopSUBW = UInt(45, UOPC_SZ)
   val uopSLLIW= UInt(46, UOPC_SZ)
   val uopSLLW = UInt(47, UOPC_SZ)
   val uopSRAIW= UInt(48, UOPC_SZ)
   val uopSRAW = UInt(49, UOPC_SZ)
   val uopSRLIW= UInt(50, UOPC_SZ)
   val uopSRLW = UInt(51, UOPC_SZ)
   val uopMUL  = UInt(52, UOPC_SZ)
   val uopMULH = UInt(53, UOPC_SZ)
   val uopMULHU= UInt(54, UOPC_SZ)
   val uopMULHSU=UInt(55, UOPC_SZ)
   val uopMULW = UInt(56, UOPC_SZ)
   val uopDIV  = UInt(57, UOPC_SZ)
   val uopDIVU = UInt(58, UOPC_SZ)
   val uopREM  = UInt(59, UOPC_SZ)
   val uopREMU = UInt(60, UOPC_SZ)
   val uopDIVW = UInt(61, UOPC_SZ)
   val uopDIVUW= UInt(62, UOPC_SZ)
   val uopREMW = UInt(63, UOPC_SZ)
   val uopREMUW= UInt(64, UOPC_SZ)

   val uopFENCEI    = UInt(65, UOPC_SZ)
   //               = UInt(66, UOPC_SZ)
   val uopAMO_AG    = UInt(67, UOPC_SZ) // AMO-address gen (use normal STD for datagen)

   val uopFMV_S_X   = UInt(68, UOPC_SZ)
   val uopFMV_D_X   = UInt(69, UOPC_SZ)
   val uopFMV_X_S   = UInt(70, UOPC_SZ)
   val uopFMV_X_D   = UInt(71, UOPC_SZ)

   val uopFSGNJ_S   = UInt(72, UOPC_SZ)
   val uopFSGNJ_D   = UInt(73, UOPC_SZ)

   val uopFCVT_S_D  = UInt(74, UOPC_SZ)
   val uopFCVT_D_S  = UInt(75, UOPC_SZ)

   val uopFCVT_S_W  = UInt(76, UOPC_SZ)
   val uopFCVT_S_WU = UInt(77, UOPC_SZ)
   val uopFCVT_S_L  = UInt(78, UOPC_SZ)
   val uopFCVT_S_LU = UInt(79, UOPC_SZ)
   val uopFCVT_D_W  = UInt(80, UOPC_SZ)
   val uopFCVT_D_WU = UInt(81, UOPC_SZ)
   val uopFCVT_D_L  = UInt(82, UOPC_SZ)
   val uopFCVT_D_LU = UInt(83, UOPC_SZ)


   val uopFCVT_W_S  = UInt(84, UOPC_SZ)
   val uopFCVT_WU_S = UInt(85, UOPC_SZ)
   val uopFCVT_L_S  = UInt(86, UOPC_SZ)
   val uopFCVT_LU_S = UInt(87, UOPC_SZ)
   val uopFCVT_W_D  = UInt(88, UOPC_SZ)
   val uopFCVT_WU_D = UInt(89, UOPC_SZ)
   val uopFCVT_L_D  = UInt(90, UOPC_SZ)
   val uopFCVT_LU_D = UInt(91, UOPC_SZ)

   val uopFEQ_S     = UInt(92, UOPC_SZ)
   val uopFLT_S     = UInt(93, UOPC_SZ)
   val uopFLE_S     = UInt(94, UOPC_SZ)
   val uopFEQ_D     = UInt(95, UOPC_SZ)
   val uopFLT_D     = UInt(96, UOPC_SZ)
   val uopFLE_D     = UInt(97, UOPC_SZ)

   val uopFCLASS_S  = UInt(98, UOPC_SZ)
   val uopFCLASS_D  = UInt(99, UOPC_SZ)

   val uopFMIN_S    = UInt(100,UOPC_SZ)
   val uopFMAX_S    = UInt(101,UOPC_SZ)
   val uopFMIN_D    = UInt(102,UOPC_SZ)
   val uopFMAX_D    = UInt(103,UOPC_SZ)

   val uopFADD_S    = UInt(104,UOPC_SZ)
   val uopFSUB_S    = UInt(105,UOPC_SZ)
   val uopFMUL_S    = UInt(106,UOPC_SZ)
   val uopFADD_D    = UInt(107,UOPC_SZ)
   val uopFSUB_D    = UInt(108,UOPC_SZ)
   val uopFMUL_D    = UInt(109,UOPC_SZ)

   val uopFMADD_S   = UInt(110,UOPC_SZ)
   val uopFMSUB_S   = UInt(111,UOPC_SZ)
   val uopFNMADD_S  = UInt(112,UOPC_SZ)
   val uopFNMSUB_S  = UInt(113,UOPC_SZ)
   val uopFMADD_D   = UInt(114,UOPC_SZ)
   val uopFMSUB_D   = UInt(115,UOPC_SZ)
   val uopFNMADD_D  = UInt(116,UOPC_SZ)
   val uopFNMSUB_D  = UInt(117,UOPC_SZ)

   val uopFDIV_S    = UInt(118,UOPC_SZ)
   val uopFDIV_D    = UInt(119,UOPC_SZ)
   val uopFSQRT_S   = UInt(120,UOPC_SZ)
   val uopFSQRT_D   = UInt(121,UOPC_SZ)

   val uopSYSTEM    = UInt(122, UOPC_SZ) // pass uop down the CSR pipeline and let it handle it
   val uopSFENCE    = UInt(123, UOPC_SZ)


   val uopVADD      = UInt(124, UOPC_SZ)
   val uopVSUB      = UInt(125, UOPC_SZ)
   val uopVMUL      = UInt(126, UOPC_SZ)
   val uopVMADD     = UInt(127, UOPC_SZ)
   val uopVMSUB     = UInt(128, UOPC_SZ)
   val uopVNMADD    = UInt(129, UOPC_SZ)
   val uopVNMSUB    = UInt(130, UOPC_SZ)

   val uopVSLL      = UInt(131, UOPC_SZ)
   val uopVSRL      = UInt(132, UOPC_SZ)
   val uopVSRA      = UInt(133, UOPC_SZ)
   val uopVSLT      = UInt(134, UOPC_SZ)
   val uopVSLTU     = UInt(135, UOPC_SZ)
   val uopVAND      = UInt(136, UOPC_SZ)
   val uopVXOR      = UInt(137, UOPC_SZ)
   val uopVOR       = UInt(138, UOPC_SZ)
   val uopVFSJ      = UInt(139, UOPC_SZ)
   val uopVFSJN     = UInt(140, UOPC_SZ)
   val uopVFSJX     = UInt(141, UOPC_SZ)

   val uopVLD       = UInt(142, UOPC_SZ)
   val uopVLDS      = UInt(143, UOPC_SZ)
   val uopVLDX      = UInt(144, UOPC_SZ)
   val uopVST       = UInt(145, UOPC_SZ)
   val uopVSTS      = UInt(146, UOPC_SZ)
   val uopVSTX      = UInt(147, UOPC_SZ)

   val uopVINSERT   = UInt(148, UOPC_SZ) // When vinserting into vector reg
                                        // Vinsert into scalar reg aliases to FMV
   val uopVEXTRACT  = UInt(149, UOPC_SZ)

   val uopVADDI     = UInt(150, UOPC_SZ)

   val uopTOVEC     = UInt(160, UOPC_SZ) // Move scalar vector operand to vector issue


   // The Bubble Instruction (Machine generated NOP)
   // Insert (XOR x0,x0,x0) which is different from software compiler
   // generated NOPs which are (ADDI x0, x0, 0).
   // Reasoning for this is to let visualizers and stat-trackers differentiate
   // between software NOPs and machine-generated Bubbles in the pipeline.
   val BUBBLE  = UInt(0x4033, 32)


   def NullMicroOp()(implicit p: Parameters): MicroOp =
   {
      val uop = Wire(new MicroOp()(p))
      uop.uopc       := uopNOP // maybe not required, but helps on asserts that try to catch spurious behavior
      uop.bypassable := Bool(false)
      uop.fp_val     := Bool(false)
      uop.vec_val    := Bool(false)
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
      cs.csr_cmd     := freechips.rocketchip.rocket.CSR.N
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

   // memory consistency model
   // The C/C++ atomics MCM requires that two loads to the same address maintain program order.
   // The Cortex A9 does NOT enforce load/load ordering (which leads to buggy behavior).
   val MCM_ORDER_DEPENDENT_LOADS = false

   val jal_opc = UInt(0x6f)
   val jalr_opc = UInt(0x67)
   def GetUop(inst: UInt): UInt = inst(6,0)
   def GetRd (inst: UInt): UInt = inst(RD_MSB,RD_LSB)
   def GetRs1(inst: UInt): UInt = inst(RS1_MSB,RS1_LSB)
   def IsCall(inst: UInt): Bool = (inst === freechips.rocketchip.rocket.Instructions.JAL ||
                                  inst === freechips.rocketchip.rocket.Instructions.JALR) && GetRd(inst) === RA
   def IsReturn(inst: UInt): Bool = GetUop(inst) === jalr_opc && GetRs1(inst) === BitPat("b00?01")

   def ComputeBranchTarget(pc: UInt, inst: UInt, xlen: Int): UInt =
   {
      val b_imm32 = Cat(Fill(20,inst(31)), inst(7), inst(30,25), inst(11,8), UInt(0,1))
      ((pc.asSInt + b_imm32.asSInt).asSInt & SInt(-2)).asUInt
   }
   def ComputeJALTarget(pc: UInt, inst: UInt, xlen: Int): UInt =
   {
      val j_imm32 = Cat(Fill(12,inst(31)), inst(19,12), inst(20), inst(30,25), inst(24,21), UInt(0,1))
      ((pc.asSInt + j_imm32.asSInt).asSInt & SInt(-2)).asUInt
   }

   def GetCfiType(inst: UInt): UInt =
   {
      import freechips.rocketchip.util.uintToBitPat
      val bpd_csignals =
         freechips.rocketchip.rocket.DecodeLogic(inst,
                     List[BitPat](N, N, N, IS_X),
                                                 //   is br?
                                                 //   |  is jal?
                                                 //   |  |  is jalr?
                                                 //   |  |  |  br type
                                                 //   |  |  |  |
                  Array[(BitPat, List[BitPat])](
                  freechips.rocketchip.rocket.Instructions.JAL     -> List(N, Y, N, IS_J),
                  freechips.rocketchip.rocket.Instructions.JALR    -> List(N, N, Y, IS_I),
                  freechips.rocketchip.rocket.Instructions.BEQ     -> List(Y, N, N, IS_B),
                  freechips.rocketchip.rocket.Instructions.BNE     -> List(Y, N, N, IS_B),
                  freechips.rocketchip.rocket.Instructions.BGE     -> List(Y, N, N, IS_B),
                  freechips.rocketchip.rocket.Instructions.BGEU    -> List(Y, N, N, IS_B),
                  freechips.rocketchip.rocket.Instructions.BLT     -> List(Y, N, N, IS_B),
                  freechips.rocketchip.rocket.Instructions.BLTU    -> List(Y, N, N, IS_B)
               ))

      val (cs_is_br: Bool) :: (cs_is_jal: Bool) :: (cs_is_jalr:Bool) :: imm_sel_ :: Nil = bpd_csignals

      val ret =
         Mux(cs_is_jalr,
            CfiType.jalr,
         Mux(cs_is_jal,
            CfiType.jal,
         Mux(cs_is_br,
            CfiType.branch,
            CfiType.none)))
      ret
   }


}

trait ExcCauseConstants
{
   // a memory disambigious misspeculation occurred
   val MINI_EXCEPTION_MEM_ORDERING = 16.U
   // an instruction needs to be replayed (e.g., I$ asks for a replay)
   val MINI_EXCEPTION_REPLAY = 17.U
   require (!freechips.rocketchip.rocket.Causes.all.contains(16))
   require (!freechips.rocketchip.rocket.Causes.all.contains(17))
}

trait VecConstants
{
   val VFP_SZ = 2
   val VFP_X  = UInt(0, VFP_SZ)
   val VFP_H  = UInt(1, VFP_SZ)
   val VFP_S  = UInt(2, VFP_SZ)
   val VFP_D  = UInt(3, VFP_SZ)

   val VL_SZ = 10
   val VRATE_SZ = 5
}

trait Sizes
{
   val SZ_D = 64
   val SZ_W = 32
   val SZ_H = 16
   val SZ_B = 8
}
}
