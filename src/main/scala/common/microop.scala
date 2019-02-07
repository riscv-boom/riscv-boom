//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// MicroOp
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.common

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.bpu.BranchPredInfo
import boom.exu.FUConstants

abstract trait HasBoomUOP extends BoomBundle 
{
  val uop = new MicroOp()
}

class MicroOp(implicit p: Parameters) extends BoomBundle()(p)
   with freechips.rocketchip.rocket.constants.MemoryOpConstants
   with freechips.rocketchip.rocket.constants.ScalarOpConstants
{
   // Is this uop valid? or has it been masked out,
   // Used by fetch buffer and Decode stage.
   val valid            = Bool()

   val uopc             = UInt(UOPC_SZ.W)       // micro-op code
   val inst             = UInt(32.W)
   val is_rvc           = Bool()
   val pc               = UInt(coreMaxAddrBits.W) // TODO remove -- use FTQ to get PC. Change to debug_pc.
   val iqtype           = UInt(IQT_SZ.W)        // which issue unit do we use?
   val fu_code          = UInt(FUConstants.FUC_SZ.W) // which functional unit do we use?
   val ctrl             = new CtrlSignals

   // What is the next state of this uop in the issue window? useful
   // for the compacting queue.
   val iw_state         = UInt(2.W)
   // Has operand 1 or 2 been waken speculatively by a load?
   // Only integer operands are speculaively woken up,
   // so we can ignore p3.
   val iw_p1_poisoned   = Bool()
   val iw_p2_poisoned   = Bool()

   val allocate_brtag   = Bool()                      // does this allocate a branch tag? (is branch or JR but not JAL)
   val is_br_or_jmp     = Bool()                      // is this micro-op a (branch or jump) vs a regular PC+4 inst?
   val is_jump          = Bool()                      // is this a jump? (jal or jalr)
   val is_jal           = Bool()                      // is this a JAL (doesn't include JR)? used for branch unit
   val is_ret           = Bool()                      // is jalr with rd=x0, rs1=x1? (i.e., a return)
   val is_call          = Bool()                      //
   val br_mask          = UInt(MAX_BR_COUNT.W)  // which branches are we being speculated under?
   val br_tag           = UInt(BR_TAG_SZ.W)

   val br_prediction    = new BranchPredInfo

   // stat tracking of committed instructions
   val stat_brjmp_mispredicted = Bool()                 // number of mispredicted branches/jmps
   val stat_btb_made_pred      = Bool()                 // the BTB made a prediction (even if BPD overrided it)
   val stat_btb_mispredicted   = Bool()                 //
   val stat_bpd_made_pred      = Bool()                 // the BPD made the prediction
   val stat_bpd_mispredicted   = Bool()                 // denominator: all committed branches

   // Index into FTQ to figure out our fetch PC.
   val ftq_idx          = UInt(log2Ceil(ftqSz).W)
   // This inst straddles two fetch packets
   val edge_inst        = Bool()
   // Low-order bits of our own PC. Combine with ftq[ftq_idx] to get PC.
   // Aligned to a cache-line size, as that is the greater fetch granularity.
   val pc_lob           = UInt(log2Ceil(icBlockBytes).W)

   val imm_packed       = UInt(LONGEST_IMM_SZ.W) // densely pack the imm in decode...
                                               // then translate and sign-extend in execute
   val csr_addr         = UInt(CSR_ADDR_SZ.W)    // only used for critical path reasons in Exe
   val rob_idx          = UInt(ROB_ADDR_SZ.W)
   val ldq_idx          = UInt(MEM_ADDR_SZ.W)
   val stq_idx          = UInt(MEM_ADDR_SZ.W)
   val pdst             = UInt(PREG_SZ.W)
   val pop1             = UInt(PREG_SZ.W)
   val pop2             = UInt(PREG_SZ.W)
   val pop3             = UInt(PREG_SZ.W)

   val prs1_busy        = Bool()
   val prs2_busy        = Bool()
   val prs3_busy        = Bool()
   val stale_pdst       = UInt(PREG_SZ.W)
   val exception        = Bool()
   val exc_cause        = UInt(xLen.W)          // TODO compress this down, xlen is insanity
   val bypassable       = Bool()                      // can we bypass ALU results? (doesn't include loads, csr, etc...)
   val mem_cmd          = UInt(M_SZ.W)          // sync primitives/cache flushes
   val mem_typ          = UInt(MT_SZ.W)         // memory mask type for loads/stores
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
   val ldst             = UInt(LREG_SZ.W)
   val lrs1             = UInt(LREG_SZ.W)
   val lrs2             = UInt(LREG_SZ.W)
   val lrs3             = UInt(LREG_SZ.W)
   val ldst_val         = Bool()              // is there a destination? invalid for stores, rd==x0, etc.
   val dst_rtype        = UInt(2.W)
   val lrs1_rtype       = UInt(2.W)
   val lrs2_rtype       = UInt(2.W)
   val frs3_en          = Bool()

   // floating point information
   val fp_val           = Bool()             // is a floating-point instruction (F- or D-extension)?
                                             // If it's non-ld/st it will write back exception bits to the fcsr.
   val fp_single        = Bool()             // single-precision floating point instruction (F-extension)

   // exception information
   val xcpt_pf_if       = Bool()             // I-TLB page fault.
   val xcpt_ae_if       = Bool()             // I$ access exception.
   val replay_if        = Bool()             // I$ wants us to replay our ifetch request
   val xcpt_ma_if       = Bool()             // Misaligned fetch (jal/brjumping to misaligned addr).

   // purely debug information
   val debug_wdata      = UInt(xLen.W)
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
   val br_type     = UInt(BR_N.getWidth.W)
   val op1_sel     = UInt(OP1_X.getWidth.W)
   val op2_sel     = UInt(OP2_X.getWidth.W)
   val imm_sel     = UInt(IS_X.getWidth.W)
   val op_fcn      = UInt(freechips.rocketchip.rocket.ALU.SZ_ALU_FN.W)
   val fcn_dw      = Bool()
   val rf_wen      = Bool()
   val csr_cmd     = UInt(freechips.rocketchip.rocket.CSR.SZ.W)
   val is_load     = Bool()   // will invoke TLB address lookup
   val is_sta      = Bool()   // will invoke TLB address lookup
   val is_std      = Bool()
}

class DebugStageEvents extends Bundle()
{
   // Track the sequence number of each instruction fetched.
   val fetch_seq        = UInt(32.W)
}

// What type of Control-Flow Instruction is it?
object CfiType
{
   def SZ = 3
   def apply() = UInt(SZ.W)
   def none = 0.U
   def branch = 1.U
   def jal = 2.U
   def jalr = 3.U
}

class MicroOpWithData(val data_sz: Int)(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomUOP
{
   val data = UInt(data_sz.W)
}
