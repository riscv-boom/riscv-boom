//**************************************************************************
// RISCV Processor Constants
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2011 May 28

package BOOM
package constants
{

import Chisel._
import Node._
   
trait BOOMProcConstants
{
   //************************************
   // Debug Support
   val COMMIT_LOG_PRINTF = false // dump commit state, for comparision against ISA sim 
   val DEBUG_PRINTF = true // use the Chisel printf functionality 
   val DEBUG_FETCHBUFFER = false // print out the fetch buffer
   val DEBUG_BTB = false
   
   
   //************************************
   // Machine Parameters
   val XPRLEN = 64           // native width of machine
                             // (i.e., the width of a register in 
                             // the general-purpose register file)
   require(XPRLEN == 64)     // additional work required to make rv32 available


   //************************************
   // Superscalar Widths

   // number of words we can fetch every cycle
   val FETCH_WIDTH      = 2; require(FETCH_WIDTH == 1 || FETCH_WIDTH == 2)

   val DECODE_WIDTH     = FETCH_WIDTH ; require(DECODE_WIDTH <= FETCH_WIDTH)
   val DISPATCH_WIDTH   = DECODE_WIDTH 
   val COMMIT_WIDTH     = DISPATCH_WIDTH

   val ISSUE_WIDTH      = 2; require (ISSUE_WIDTH <= 3)

   
   //************************************
   // Pipelining 
   
   val ENABLE_FETCH_BUFFER_FLOW_THROUGH = true


   //************************************
   // Extra Knobs and Features

   val ENABLE_PREFETCHING   = false
   val ENABLE_BTB           = true
   val ENABLE_ALU_BYPASSING = true
   val ENABLE_REGFILE_BYPASSING = true    // bypass regfile write ports to read ports
 
   val BTB_NUM_ENTRIES = 8
 
   val IC_NUM_SETS = 128
   val IC_NUM_WAYS = 2
   val DC_NUM_SETS = 128
   val DC_NUM_WAYS = 2
   val DC_NUM_MSHR = 2    // secondary miss handler

   val INTEGER_ISSUE_SLOT_COUNT = 12
   val NUM_ROB_ENTRIES          = 16*DECODE_WIDTH // number of ROB entries (32 entries for R10k)
   require (isPow2(NUM_ROB_ENTRIES))
   val NUM_LSU_ENTRIES          = 8   // number of LD/ST entries
   require (isPow2(NUM_LSU_ENTRIES))
   val ROB_ADDR_SZ = log2Up(NUM_ROB_ENTRIES) 
   val MEM_ADDR_SZ = log2Up(NUM_LSU_ENTRIES)

   val MAX_WAKEUP_DELAY = 3 // unused

   // size of the unified, physical register file
   val PHYS_REG_COUNT = 64; require(PHYS_REG_COUNT > 32)

   val BR_TAG_SZ   = 2   // log number of branches we can speculate simultaneously
   require(BR_TAG_SZ >=1)
   
   val FETCH_BUFFER_SZ = 4 // number of instructions that can be stored between fetch + decode
   

   // Implicitly calculated constants
   val LOGICAL_REG_COUNT = 32
   val LREG_SZ           = log2Up(LOGICAL_REG_COUNT)
   val PREG_SZ           = log2Up(PHYS_REG_COUNT)   
   val MAX_ST_COUNT      = (1 << MEM_ADDR_SZ)
   val MAX_LD_COUNT      = (1 << MEM_ADDR_SZ)
   val MAX_BR_COUNT      = (1 << (BR_TAG_SZ)) 

   // if pipeline goes idle, throw error
   // otherwise, reset pipeline and restart TODO on this feature
   val ON_IDLE_THROW_ERROR = true
                                                      
}

trait LoadStoreUnitConstants
{
   val ENABLE_STOREDATA_FORWARDING = true // allow stores to forward data to depending loads 
   require (ENABLE_STOREDATA_FORWARDING == true) // required to for younger loads to read out of the committed store buffer

   val ENABLE_SPECULATE_LOADS = true // allow loads to speculate - otherwise execute at commit (EaC is currently broken...)
}
    
trait BrPredConstants
{
   val NOT_TAKEN = Bool(false)
   val TAKEN = Bool(true)
                     
   // Uses a History Table of n-bit counters
   val USE_BRANCH_PREDICTOR = true
   val BPRED_DESIGN    = "BP_R10K"
//   val BPRED_DESIGN    = "BP_21264"
//   val BPRED_DESIGN    = "BP_GSHARE"
//   val BPRED_DESIGN    = "BP_GLOBAL"
   val NUM_BHT_ENTRIES = 128  
   val BHT_COUNTER_SZ = 2  
   val NUM_LHIST_ENTRIES = 128  
}
 

trait ScalarOpConstants
{
   val Y = Bool(true)
   val N = Bool(false)

    
   //************************************
   // Control Signals 
                
   // PC Select Signal
   val PC_PLUS4 = Bits(0, 2)  // PC + 4
   val PC_BRJMP = Bits(1, 2)  // brjmp_target 
   val PC_JALR  = Bits(2, 2)  // jump_reg_target
   
   // PC Select Signal (for the oracle)
   val PC_4   = UInt(0, 3)  // PC + 4
   val PC_BR  = UInt(1, 3)  // branch_target
   val PC_J   = UInt(2, 3)  // jump_target
   val PC_JR  = UInt(3, 3)  // jump_reg_target
      
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
   val OP1_RS1 = UInt(0, 1) // Register Source #1
   val OP1_LUI = UInt(1, 1) // Load Upper Immediate
   val OP1_X   = UInt(0, 1)
   
   // RS2 Operand Select Signal
   // TODO this is confusing Immediate select, ALU OP2, and RRd OP2
   val OP2_RS2 = UInt(0, 3) // Register Source #2
   val OP2_IMM = UInt(1, 3) // immediate, I-type
   val OP2_IMI = UInt(1, 3) // immediate, I-type
   val OP2_IMB = UInt(2, 3) // immediate, B-type
   val OP2_12  = UInt(3, 3) // literal 12 (for LUI shift)
   val OP2_TSC = UInt(4, 3) // time stamp counter
   val OP2_IRT = UInt(5, 3) // retired inst count
   val OP2_X   = UInt(0, 3)
                      
   // Register File Write Enable Signal
   val REN_0   = Bool(false)
   val REN_1   = Bool(true)
   val REN_X   = Bool(false)
           
   // Is 32b Word or 64b Doubldword?
   val SZ_DW = 1 
   val DW_X   = Bool(true) //Bool(XPRLEN==64)
   val DW_32  = Bool(false)
   val DW_64  = Bool(true)
   val DW_XPR = Bool(true) //Bool(XPRLEN==64)
   
   // Writeback Select Signal
   val WB_ALU  = UInt(0, 2)
   val WB_PC4  = UInt(1, 2)
   val WB_PCR  = UInt(2, 2)
   val WB_X    = UInt(0, 2)
   
   // Memory Function Type (Read,Write,Fence) Signal
                          
   // Memory Enable Signal
   val MEN_0   = Bool(false)
   val MEN_1   = Bool(true)
   val MEN_X   = Bool(false)
                     
   // Immediate Extend Select
   val IS_I   = UInt(0, 3)  //I-Type  (LD,ALU) 
   val IS_S   = UInt(1, 3)  //S-Type  (ST)
   val IS_B   = UInt(2, 3)  //SB-Type (BR)
   val IS_U   = UInt(3, 3)  //U-Type  (LUI/AUIPC)     
   val IS_J   = UInt(4, 3)  //UJ-Type (J/JAL)   
   val IS_X   = UInt(0, 3)  


   // Decode Stage Control Signals
   val RT_FIX   = UInt(0, 2)
   val RT_PCR   = UInt(1, 2)
   val RT_FLT   = UInt(2, 2)
   val RT_X     = UInt(3, 2)
   
   // Micro-op opcodes
   // TODO use an enum
   val UOPC_SZ = 9
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
   // missing 33
   val uopJ    = Bits(34, UOPC_SZ)
   val uopJAL  = Bits(35, UOPC_SZ)
   val uopJALR = Bits(36, UOPC_SZ)
   val uopAUIPC= Bits(37, UOPC_SZ)
   
   val uopSRET = Bits(38, UOPC_SZ)
   val uopCFLSH= Bits(39, UOPC_SZ)
   val uopFENCE= Bits(40, UOPC_SZ)
   
   val uopRDC  = Bits(41, UOPC_SZ)
   val uopRDI  = Bits(42, UOPC_SZ)
   
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
   
   val uopFENCEI= Bits(65, UOPC_SZ)
   val uopMEMSPECIAL= Bits(66, UOPC_SZ)

   // Enable Co-processor Register Signal (ToHost Register, etc.)
   val PCR_N   = UInt(0,3)    // do nothing
   val PCR_F   = UInt(1,3)    // mfpcr
   val PCR_T   = UInt(2,3)    // mtpcr
   val PCR_C   = UInt(3,3)    // clear pcr
   val PCR_S   = UInt(4,3)    // set pcr
        
   // Memory Mask Type Signal
   val MSK_X   = UInt(4, 3)
   val MSK_B   = UInt(0, 3)
   val MSK_H   = UInt(1, 3)
   val MSK_W   = UInt(2, 3)
   val MSK_D   = UInt(3, 3)
   val MSK_BU  = UInt(4, 3)
   val MSK_HU  = UInt(5, 3)
   val MSK_WU  = UInt(6, 3)

 
   // Cache Flushes & Sync Primitives 
   val M_N      = Bits("b000",4)

   // The Bubble Instruction (Machine generated NOP)
   // Insert (XOR x0,x0,x0) which is different from software compiler 
   // generated NOPs which are (ADDI x0, x0, 0).
   // Reasoning for this is to let visualizers and stat-trackers differentiate
   // between software NOPs and machine-generated Bubbles in the pipeline.
   val BUBBLE  = Bits(0x4033, 32)


   //val nullCtrlSignals = new CtrlSignals()
   //nullCtrlSignals.br_type     := BR_N
   //nullCtrlSignals.rf_wen      := Bool(false)
   //nullCtrlSignals.pcr_fcn     := PCR_N
   //nullCtrlSignals.is_load     := Bool(false)
   //nullCtrlSignals.is_sta      := Bool(false)
   //nullCtrlSignals.is_std      := Bool(false)

//   val nullUop = new MicroOp()
//   nullUop.ctrl := nullCtrlSignals
}

trait InterruptConstants 
{
   val CAUSE_INTERRUPT = 32
}
  
//abstract trait RocketDcacheConstants extends uncore.constants.CacheConstants with uncore.constants.AddressConstants {
//   require(OFFSET_BITS == log2Up(uncore.Constants.CACHE_DATA_SIZE_IN_BYTES))
//   require(OFFSET_BITS <= uncore.Constants.ACQUIRE_WRITE_MASK_BITS)
//   require(log2Up(OFFSET_BITS) <= uncore.Constants.ACQUIRE_SUBWORD_ADDR_BITS)
//}




}

