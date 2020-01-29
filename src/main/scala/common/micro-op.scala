//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
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

/**
 * Extension to BoomBundle to add a MicroOp
 */
abstract trait HasBoomUOP extends BoomBundle
{
  val uop = new MicroOp()
}

/**
 * MicroOp passing through the pipeline
 */
class MicroOp(implicit p: Parameters) extends BoomBundle
  with freechips.rocketchip.rocket.constants.MemoryOpConstants
  with freechips.rocketchip.rocket.constants.ScalarOpConstants
{
  val uopc             = UInt(UOPC_SZ.W)       // micro-op code
  val inst             = UInt(32.W)
  val debug_inst       = UInt(32.W)
  val is_rvc           = Bool()
  val debug_pc         = UInt(coreMaxAddrBits.W)
  val iq_type          = UInt(IQT_SZ.W)        // which issue unit do we use?
  val fu_code          = UInt(FUConstants.FUC_SZ.W) // which functional unit do we use?
  val eu_code          = UInt(4.W) // Just hard-code this for now
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
  val br_mask          = UInt(maxBrCount.W)  // which branches are we being speculated under?
  val br_tag           = UInt(brTagSz.W)

  val br_prediction    = new BranchPredInfo


  // stat tracking of committed instructions
  val stat_brjmp_mispredicted = Bool()                 // number of mispredicted branches/jmps
  val stat_btb_made_pred      = Bool()                 // the BTB made a prediction (even if BPD overrided it)
  val stat_btb_mispredicted   = Bool()                 //
  val stat_bpd_made_pred      = Bool()                 // the BPD made the prediction
  val stat_bpd_mispredicted   = Bool()                 // denominator: all committed branches

  // Index into FTQ to figure out our fetch PC.
  val ftq_idx          = UInt(log2Ceil(ftqSz).W)
  // Index in fetch bundle.
  val cfi_idx          = UInt(log2Ceil(fetchWidth).W)
  // This inst straddles two fetch packets
  val edge_inst        = Bool()
  // Low-order bits of our own PC. Combine with ftq[ftq_idx] to get PC.
  // Aligned to a cache-line size, as that is the greater fetch granularity.
  val pc_lob           = UInt(log2Ceil(icBlockBytes).W)

  val imm_packed       = UInt(LONGEST_IMM_SZ.W) // densely pack the imm in decode...
                                              // then translate and sign-extend in execute
  val csr_addr         = UInt(CSR_ADDR_SZ.W)    // only used for critical path reasons in Exe
  val rob_idx          = UInt(robAddrSz.W)
  val ldq_idx          = UInt(ldqAddrSz.W)
  val stq_idx          = UInt(stqAddrSz.W)
  val rxq_idx          = UInt(log2Ceil(numRxqEntries).W)
  val pdst             = UInt(maxPregSz.W)
  val prs1             = UInt(maxPregSz.W)
  val prs2             = UInt(maxPregSz.W)
  val prs3             = UInt(maxPregSz.W)
  val stale_pdst       = UInt(maxPregSz.W)

  val fast_prs_sel     = Bool() // Which prs was used to pick a dst column? Can receive a bypassed operand.
                                // 0 = prs1, 1 = prs2
  def GetFastOperand   = Mux(fast_prs_sel, prs2, prs1)

  val prs1_busy        = Bool()
  val prs2_busy        = Bool()
  val prs3_busy        = Bool()

  val prs1_bypass      = Bool() // Was the uop immediately scheduled after prs1 matched a fast wakeup?
  val prs2_bypass      = Bool() // Ditto

  val exception        = Bool()
  val exc_cause        = UInt(xLen.W)          // TODO compress this down, xlen is insanity
  val bypassable       = Bool()                      // can we bypass ALU results? (doesn't include loads, csr, etc...)
  val mem_cmd          = UInt(M_SZ.W)          // sync primitives/cache flushes
  val mem_size         = UInt(2.W)
  val mem_signed       = Bool()
  val is_fence         = Bool()
  val is_fencei        = Bool()
  val is_amo           = Bool()
  val uses_ldq         = Bool()
  val uses_stq         = Bool()
  val is_sys_pc2epc    = Bool()                      // Is a ECall or Breakpoint -- both set EPC to PC.
  val is_unique        = Bool()                      // only allow this instruction in the pipeline, wait for STQ to
                                                     // drain, clear fetcha fter it (tell ROB to un-ready until empty)
  val flush_on_commit  = Bool()                      // some instructions need to flush the pipeline behind them

  // logical specifiers (only used in Decode->Rename), except rollback (ldst)
  val ldst             = UInt(lregSz.W)
  val lrs1             = UInt(lregSz.W)
  val lrs2             = UInt(lregSz.W)
  val lrs3             = UInt(lregSz.W)
  val ldst_val         = Bool()              // is there a destination? invalid for stores, rd==x0, etc.
  val dst_rtype        = UInt(2.W)
  val lrs1_rtype       = UInt(2.W)
  val lrs2_rtype       = UInt(2.W)
  val frs3_en          = Bool()

  // floating point information
  val fp_val           = Bool()             // is a floating-point instruction (F- or D-extension)?
                                            // If it's non-ld/st it will write back exception bits to the fcsr.
  val fp_single        = Bool()             // single-precision floating point instruction (F-extension)

  // frontend exception information
  val xcpt_pf_if       = Bool()             // I-TLB page fault.
  val xcpt_ae_if       = Bool()             // I$ access exception.
  val replay_if        = Bool()             // I$ wants us to replay our ifetch request
  val xcpt_ma_if       = Bool()             // Misaligned fetch (jal/brjumping to misaligned addr).
  val bp_debug_if      = Bool()             // Breakpoint
  val bp_xcpt_if       = Bool()             // Breakpoint

  // purely debug information
  val debug_wdata      = UInt(xLen.W)
  val debug_events     = new DebugStageEvents


  // Does this register write-back
  def rf_wen           = dst_rtype =/= RT_X

  // Is it possible for this uop to misspeculate, preventing the commit of subsequent uops?
  def unsafe           = uses_ldq || (uses_stq && !is_fence) || (is_br_or_jmp && !is_jal)

  def fu_code_is(_fu: UInt) = (fu_code & _fu) =/= 0.U

  // In which column does a physical register reside?
  def ColIdx(in: UInt) = {
    val pregSz = in.getWidth
    val colSz = log2Ceil(coreWidth)
    in(pregSz-1, pregSz-colSz)
  }

  // Physical register's specifier within a column.
  def ColSpec(in: UInt) = {
    val pregSz = in.getWidth
    val colSz = log2Ceil(coreWidth)
    in(pregSz-colSz-1, 0)
  }

  // Get the columns of the uop's physical registers
  def dst_col   = UIntToOH(ColIdx(pdst))
  def op1_col   = UIntToOH(ColIdx(pop1))
  def op2_col   = UIntToOH(ColIdx(pop2))
  def stale_col = UIntToOH(ColIdx(stale_pdst))

  // Getters that help with arbitration during scheduling
  def prs1_do_read   = lrs1_rtype === RT_FIX && !prs1_bypass
  def prs2_do_read   = lrs2_rtype === RT_FIX && !prs2_bypass
  def shared_eu_code = eu_code(4,1)
  def exe_wb_latency = (fu_code(2) | fu_code(3)) << 3 | fu_code(0)
}

/**
 * Control signals within a MicroOp
 *
 * TODO REFACTOR this, as this should no longer be true, as bypass occurs in stage before branch resolution
 */
class CtrlSignals extends Bundle()
{
  val br_type     = UInt(BR_N.getWidth.W)
  val op1_sel     = UInt(OP1_X.getWidth.W)
  val op2_sel     = UInt(OP2_X.getWidth.W)
  val imm_sel     = UInt(IS_X.getWidth.W)
  val op_fcn      = UInt(freechips.rocketchip.rocket.ALU.SZ_ALU_FN.W)
  val fcn_dw      = Bool()
  val csr_cmd     = UInt(freechips.rocketchip.rocket.CSR.SZ.W)
  val is_load     = Bool()   // will invoke TLB address lookup
  val is_sta      = Bool()   // will invoke TLB address lookup
  val is_std      = Bool()
}

/**
 * Debug stage events for Fetch stage
 */
class DebugStageEvents extends Bundle()
{
  // Track the sequence number of each instruction fetched.
  val fetch_seq        = UInt(32.W)
}


