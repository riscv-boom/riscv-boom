//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// MicroOp
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.common

import Chisel._
import freechips.rocketchip.config.Parameters
import boom.bpu.BranchPredInfo
import boom.exu.FUConstants

class MicroOp(implicit p: Parameters) extends BoomBundle()(p)
   with freechips.rocketchip.rocket.constants.MemoryOpConstants
   with freechips.rocketchip.rocket.constants.ScalarOpConstants
{
   val valid            = Bool()                      // is this uop valid? or has it been masked out,
                                                      // used by fetch buffer and Decode stage
   val iw_state         = UInt(width = 2)             // what is the next state of this uop in the issue window? useful
                                                      // for the compacting queue? TODO or is this not really belong
                                                      // here?

   val uopc             = UInt(width = UOPC_SZ)       // micro-op code
   val inst             = UInt(width = 32)
   val pc               = UInt(width = coreMaxAddrBits) // TODO remove -- use FTQ to get PC. Change to debug_pc.
   val iqtype           = UInt(width = IQT_SZ)        // which issue unit do we use?
   val fu_code          = UInt(width = FUConstants.FUC_SZ) // which functional unit do we use?
   val ctrl             = new CtrlSignals

   val allocate_brtag   = Bool()                      // does this allocate a branch tag? (is branch or JR but not JAL)
   val is_br_or_jmp     = Bool()                      // is this micro-op a (branch or jump) vs a regular PC+4 inst?
   val is_jump          = Bool()                      // is this a jump? (jal or jalr)
   val is_jal           = Bool()                      // is this a JAL (doesn't include JR)? used for branch unit
   val is_ret           = Bool()                      // is jalr with rd=x0, rs1=x1? (i.e., a return)
   val is_call          = Bool()                      //
   val br_mask          = UInt(width = MAX_BR_COUNT)  // which branches are we being speculated under?
   val br_tag           = UInt(width = BR_TAG_SZ)

   val br_prediction    = new BranchPredInfo

   // stat tracking of committed instructions
   val stat_brjmp_mispredicted = Bool()                 // number of mispredicted branches/jmps
   val stat_btb_made_pred      = Bool()                 // the BTB made a prediction (even if BPD overrided it)
   val stat_btb_mispredicted   = Bool()                 //
   val stat_bpd_made_pred      = Bool()                 // the BPD made the prediction
   val stat_bpd_mispredicted   = Bool()                 // denominator: all committed branches

   // Index into FTQ to figure out our fetch PC.
   val ftq_idx          = UInt(width = log2Ceil(ftqSz))
   // Low-order bits of our own PC. Combine with ftq[ftq_idx] to get PC.
   // Aligned to a cache-line size, as that is the greater fetch granularity.
   val pc_lob           = UInt(width = log2Ceil(icBlockBytes).W)


   val imm_packed       = UInt(width = LONGEST_IMM_SZ) // densely pack the imm in decode...
                                                       // then translate and sign-extend in execute
   val csr_addr         = UInt(width = CSR_ADDR_SZ)    // only used for critical path reasons in Exe
   val rob_idx          = UInt(width = ROB_ADDR_SZ)
   val ldq_idx          = UInt(width = MEM_ADDR_SZ)
   val stq_idx          = UInt(width = MEM_ADDR_SZ)
   val pdst             = UInt(width = PREG_SZ)
   val pop1             = UInt(width = PREG_SZ)
   val pop2             = UInt(width = PREG_SZ)
   val pop3             = UInt(width = PREG_SZ)

   val prs1_busy        = Bool()
   val prs2_busy        = Bool()
   val prs3_busy        = Bool()

   val prs1_eidx        = UInt(width = VL_SZ)
   val prs2_eidx        = UInt(width = VL_SZ)
   val prs3_eidx        = UInt(width = VL_SZ)

   val stale_pdst       = UInt(width = PREG_SZ)
   val exception        = Bool()
   val exc_cause        = UInt(width = xLen)          // TODO compress this down, xlen is insanity
   val bypassable       = Bool()                      // can we bypass ALU results? (doesn't include loads, csr, etc...)
   val mem_cmd          = UInt(width = M_SZ)          // sync primitives/cache flushes
   val mem_typ          = UInt(width = MT_SZ)         // memory mask type for loads/stores
   val is_fence         = Bool()
   val is_fencei        = Bool()
   val is_store         = Bool()                      // anything that goes into the STQ, including fences and AMOs
   val is_amo           = Bool()
   val is_load          = Bool()
   val is_sys_pc2epc    = Bool()                      // Is a ECall or Breakpoint -- both set EPC to PC.
   val is_unique        = Bool()                      // only allow this instruction in the pipeline, wait for STQ to
                                                      // drain, clear fetcha fter it (tell ROB to un-ready until empty)
   val flush_on_commit  = Bool()                      // some instructions need to flush the pipeline behind them

   // logical specifiers (only used in Decode->Rename), except rollback (ldst)
   val ldst             = UInt(width=LREG_SZ)
   val lrs1             = UInt(width=LREG_SZ)
   val lrs2             = UInt(width=LREG_SZ)
   val lrs3             = UInt(width=LREG_SZ)
   val ldst_val         = Bool()              // is there a destination? invalid for stores, rd==x0, etc.
   val dst_rtype        = UInt(width=RT_SZ)
   val lrs1_rtype       = UInt(width=RT_SZ)
   val lrs2_rtype       = UInt(width=RT_SZ)
   val lrs3_rtype       = UInt(width=RT_SZ)
   val frs3_en          = Bool()


   // TODO vec: These shouldn't be the full vcfg specifiers
   val rs1_vew          = UInt(width=VEW_SZ)
   val rs2_vew          = UInt(width=VEW_SZ)
   val rs3_vew          = UInt(width=VEW_SZ)
   val rd_vew           = UInt(width=VEW_SZ)

   val rs1_vshape       = UInt(width=VSHAPE_SZ)
   val rs2_vshape       = UInt(width=VSHAPE_SZ)
   val rs3_vshape       = UInt(width=VSHAPE_SZ)
   val rd_vshape        = UInt(width=VSHAPE_SZ)

   val rs1_verep        = UInt(width=VEREP_SZ)
   val rs2_verep        = UInt(width=VEREP_SZ)
   val rs3_verep        = UInt(width=VEREP_SZ)
   val rd_verep         = UInt(width=VEREP_SZ)

   val rate             = UInt(width=VRATE_SZ)
   val eidx             = UInt(width=VL_SZ)

   // TODO_Vec: These are temporarily where scalar vector operands are stored
   //           In future store these in operand buffer in vector pipeline
   val rs1_data         = UInt(width=xLen)
   val rs2_data         = UInt(width=xLen)
   val rs3_data         = UInt(width=xLen)


   // floating point information
   val fp_val           = Bool()             // is a floating-point instruction (F- or D-extension)?
                                             // If it's non-ld/st it will write back exception bits to the fcsr.
   val fp_single        = Bool()             // single-precision floating point instruction (F-extension)

   val vec_val          = Bool()

   // exception information
   val xcpt_pf_if       = Bool()             // I-TLB page fault.
   val xcpt_ae_if       = Bool()             // I$ access exception.
   val replay_if        = Bool()             // I$ wants us to replay our ifetch request
   val xcpt_ma_if       = Bool()             // Misaligned fetch (jal/brjumping to misaligned addr).

   // purely debug information
   val debug_wdata      = UInt(width=xLen)
   val debug_events     = new DebugStageEvents

   def fu_code_is(_fu: UInt) = fu_code === _fu
}

// NOTE: I can't promise these signals get killed/cleared on a mispredict,
// so I should listen to the corresponding valid bit
// For example, on a bypassing, we listen to rf_wen to see if bypass is valid,
// but we "could" be bypassing to a branch which kills us (a false positive combinational loop),
// so we have to keep the rf_wen enabled, and not dependent on a branch kill signal
// TODO REFACTOR this, as this should no longer be true, as bypass occurs in stage before branch resolution
class CtrlSignals extends Bundle()
{
   val br_type     = UInt(width = BR_N.getWidth)
   val op1_sel     = UInt(width = OP1_X.getWidth)
   val op2_sel     = UInt(width = OP2_X.getWidth)
   val imm_sel     = UInt(width = IS_X.getWidth)
   val op_fcn      = UInt(width = freechips.rocketchip.rocket.ALU.SZ_ALU_FN)
   val fcn_dw      = Bool()
   val rf_wen      = Bool()
   val csr_cmd     = UInt(width = freechips.rocketchip.rocket.CSR.SZ)
   val is_load     = Bool()   // will invoke TLB address lookup
   val is_sta      = Bool()   // will invoke TLB address lookup
   val is_std      = Bool()
}

class DebugStageEvents extends Bundle()
{
   // Track the sequence number of each instruction fetched.
   val fetch_seq        = UInt(width = 32)
}

// What type of Control-Flow Instruction is it?
object CfiType
{
   def SZ = 3
   def apply() = UInt(width = SZ)
   def none = 0.U
   def branch = 1.U
   def jal = 2.U
   def jalr = 3.U
}

class MicroOpWithData(data_sz: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop = new MicroOp()
   val data = UInt(width = data_sz)
   override def cloneType = new MicroOpWithData(data_sz)(p).asInstanceOf[this.type]
}
