//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Re-order Buffer
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Bank the ROB, such that each "dispatch" group gets its own row of the ROB,
// and each instruction in the dispatch group goes to a different bank.
// We can compress out the PC by only saving the high-order bits!
//
// ASSUMPTIONS:
//    - dispatch groups are aligned to the PC.
//
// NOTES:
//    - Currently we do not compress out bubbles in the ROB.
//    - commit_width is tied directly to the dispatch_width.
//    - Exceptions are only taken when at the head of the commit bundle --
//      this helps deal with loads, stores, and refetch instructions.

package boom.exu

import scala.math.ceil

import chisel3._
import chisel3.util._
import chisel3.experimental.chiselName

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import boom.common._
import boom.util._

/**
 * IO bundle to interact with the ROB
 *
 * @param machine_width dispatch and commit width
 * @param num_wakeup_ports number of wakeup ports to the rob
 * @param num_fpu_ports number of fpu ports that will write back fflags
 */
class RobIo(
   val machine_width: Int,
   val num_wakeup_ports: Int,
   val num_fpu_ports: Int
   )(implicit p: Parameters)  extends BoomBundle()(p)
{
   // Decode Stage
   // (Allocate, write instruction to ROB).
   val enq_valids       = Input(Vec(machine_width, Bool()))
   val enq_uops         = Input(Vec(machine_width, new MicroOp()))
   val enq_partial_stall= Input(Bool()) // we're dispatching only a partial packet,
                                        // and stalling on the rest of it (don't
                                      // advance the tail ptr)
   val enq_new_packet    = Input(Bool()) // we're dispatching the first (and perhaps only) part of a dispatch packet.

   val curr_rob_tail_idx = Output(UInt(ROB_ADDR_SZ.W))
   // For the PNR we send the idx instead of the row addr
   // because otherwise partial rows stall the PNR
   val curr_rob_pnr_idx  = Output(UInt(ROB_ADDR_SZ.W))

   // Handle Branch Misspeculations
   val brinfo = Input(new BrResolutionInfo())

   // Write-back Stage
   // (Update of ROB)
   // Instruction is no longer busy and can be committed
   val wb_resps = Flipped(Vec(num_wakeup_ports, Valid(new ExeUnitResp(xLen max fLen+1))))

   val lsu_clr_bsy_valid      = Input(Vec(2, Bool()))
   val lsu_clr_bsy_rob_idx    = Input(Vec(2, UInt(ROB_ADDR_SZ.W)))
   val lsu_mem_success         = Input(Bool())
   val lsu_mem_success_rob_idx = Input(UInt(ROB_ADDR_SZ.W))

   // Track side-effects for debug purposes.
   // Also need to know when loads write back, whereas we don't need loads to unbusy.
   val debug_wb_valids = Input(Vec(num_wakeup_ports, Bool()))
   val debug_wb_wdata  = Input(Vec(num_wakeup_ports, Bits(xLen.W)))

   val fflags = Flipped(Vec(num_fpu_ports, new ValidIO(new FFlagsResp())))
   val lxcpt = Flipped(new ValidIO(new Exception())) // LSU
   val bxcpt = Flipped(new ValidIO(new Exception())) // BRU

   // Commit stage (free resources; also used for rollback).
   val commit = Output(new CommitSignals())

   // tell the LSU that the head of the ROB is a load
   // (some loads can only execute once they are at the head of the ROB).
   val com_load_is_at_rob_head = Output(Bool())

   // Communicate exceptions to the CSRFile
   val com_xcpt = Valid(new CommitExceptionSignals())

   // Let the CSRFile stall us (e.g., wfi).
   val csr_stall = Input(Bool())

   // Flush signals (including exceptions, pipeline replays, and memory ordering failures)
   // to send to the frontend for redirection.
   val flush = Valid(new CommitExceptionSignals)

   // Stall Decode as appropriate
   val empty = Output(Bool())
   val ready = Output(Bool()) // ROB is busy unrolling rename state...

   // pass out debug information to high-level printf
   val debug = Output(new DebugRobSignals())

   val debug_tsc = Input(UInt(xLen.W))
}

/**
 * Bundle to send commit signals across processor
 */
class CommitSignals(implicit p: Parameters) extends BoomBundle()(p)
{
   val valids     = Vec(retireWidth, Bool())
   val uops       = Vec(retireWidth, new MicroOp())
   val fflags     = Valid(UInt(5.W))

   // Perform rollback of rename state (in conjuction with commit.uops).
   val rbk_valids = Vec(retireWidth, Bool())

   // tell the LSU how many stores and loads are being committed
   val st_mask    = Vec(retireWidth, Bool())
   val ld_mask    = Vec(retireWidth, Bool())
}

/**
 * Bundle to communicate exceptions to CSRFile
 *
 * TODO combine FlushSignals and ExceptionSignals (currently timed to different cycles).
 */
class CommitExceptionSignals(implicit p: Parameters) extends BoomBundle()(p)
{
   val ftq_idx    = UInt(log2Ceil(ftqSz).W)
   val edge_inst  = Bool()
   val is_rvc     = Bool()
   val pc_lob     = UInt(log2Ceil(icBlockBytes).W)
   val cause      = UInt(xLen.W)
   val badvaddr   = UInt(xLen.W)
// The ROB needs to tell the FTQ if there's a pipeline flush (and what type)
// so the FTQ can drive the frontend with the correct redirected PC.
   val flush_typ  = FlushTypes()
}

/**
 * Tell the frontend the type of flush so it can set up the next PC properly.
 */
object FlushTypes
{
   def SZ = 3
   def apply() = UInt(SZ.W)
   def none = 0.U
   def xcpt = 1.U // An exception occurred.
   def eret = (2+1).U // Execute an environment return instruction.
   def refetch = 2.U // Flush and refetch the head instruction.
   def next = 4.U // Flush and fetch the next instruction.

   def useCsrEvec(typ: UInt): Bool = typ(0) // typ === xcpt.U || typ === eret.U
   def useSamePC(typ: UInt): Bool = typ === refetch
   def usePCplus4(typ: UInt): Bool = typ === next

   def getType(valid: Bool, i_xcpt: Bool, i_eret: Bool, i_refetch: Bool): UInt =
   {
      val ret =
         Mux(!valid, none,
         Mux(i_eret, eret,
         Mux(i_xcpt, xcpt,
         Mux(i_refetch, refetch,
            next))))
      ret
   }
}

/**
 * Bundle of signals indicating that an exception occurred
 */
class Exception(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop = new MicroOp()
   val cause = Bits(log2Ceil(freechips.rocketchip.rocket.Causes.all.max+2).W)
   val badvaddr = UInt(coreMaxAddrBits.W)
}

/**
 * Bundle for debug ROB signals
 * These should not be synthesized!
 */
class DebugRobSignals(implicit p: Parameters) extends BoomBundle()(p)
{
   val state = UInt()
   val rob_head = UInt(ROB_ADDR_SZ.W)
   val rob_pnr = UInt(ROB_ADDR_SZ.W)
   val xcpt_val = Bool()
   val xcpt_uop = new MicroOp()
   val xcpt_badvaddr = UInt(xLen.W)
}

/**
 * Reorder Buffer to keep track of dependencies and inflight instructions
 *
 * @param width the dispatch and commit width of the processor
 * @param num_wakeup_ports number of wakeup ports to the ROB
 * @param num_fpu_ports number of FPU units that will write back fflags
 */
@chiselName
class Rob(
   width: Int,
   num_rob_entries: Int,
   val num_wakeup_ports: Int,
   val num_fpu_ports: Int
   )(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new RobIo(width, num_wakeup_ports, num_fpu_ports))

   require (num_rob_entries % width == 0)

   require (isPow2(width))

   // ROB Finite State Machine
   val s_reset :: s_normal :: s_rollback :: s_wait_till_empty :: Nil = Enum(4)
   val rob_state = RegInit(s_reset)

   //commit entries at the head, and unwind exceptions from the tail
   val rob_head     = RegInit(0.U(log2Ceil(NUM_ROB_ROWS).W))
   val rob_tail     = RegInit(0.U(log2Ceil(NUM_ROB_ROWS).W))
   val rob_tail_lsb = RegInit(0.U(log2Ceil(width).W))
   val rob_tail_idx = if (width == 1) rob_tail else Cat(rob_tail, rob_tail_lsb)
   val rob_pnr      = RegInit(0.U(log2Ceil(NUM_ROB_ROWS).W))
   val rob_pnr_lsb  = RegInit(0.U(log2Ceil(width).W))
   val rob_pnr_idx  = if (width == 1) rob_pnr  else Cat(rob_pnr , rob_pnr_lsb)


   val will_commit         = Wire(Vec(width, Bool()))
   val can_commit          = Wire(Vec(width, Bool()))
   val can_throw_exception = Wire(Vec(width, Bool()))
   val rob_pnr_safe        = Wire(Vec(width, Bool())) // are the instructions at the pnr head safe?
   val rob_head_vals       = Wire(Vec(width, Bool())) // are the instructions at the head valid?
   val rob_head_is_store   = Wire(Vec(width, Bool()))
   val rob_head_is_load    = Wire(Vec(width, Bool()))
   val rob_head_fflags     = Wire(Vec(width, UInt(freechips.rocketchip.tile.FPConstants.FLAGS_SZ.W)))

   val exception_thrown = Wire(Bool())

   // exception info
   // TODO compress xcpt cause size. Most bits in the middle are zero.
   val r_xcpt_val       = RegInit(false.B)
   val r_xcpt_uop       = Reg(new MicroOp())
   val r_xcpt_badvaddr  = Reg(UInt(xLen.W))

   //--------------------------------------------------
   // Utility

   def GetRowIdx(rob_idx: UInt): UInt =
   {
      if (width == 1) return rob_idx
      else return rob_idx >> log2Ceil(width).U
   }
   def GetBankIdx(rob_idx: UInt): UInt =
   {
      if(width == 1) { return 0.U }
      else           { return rob_idx(log2Ceil(width)-1, 0).asUInt }
   }

   // **************************************************************************
   // Debug

   class DebugRobBundle extends BoomBundle()(p)
   {
      val valid      = Bool()
      val busy       = Bool()
      val safe       = Bool()
      val uop        = new MicroOp()
      val exception  = Bool()
   }
   val debug_entry = Wire(Vec(NUM_ROB_ENTRIES, new DebugRobBundle))
   debug_entry := DontCare // override in statements below

   // **************************************************************************
   // --------------------------------------------------------------------------
   // **************************************************************************

   for (w <- 0 until width)
   {
      def MatchBank(bank_idx: UInt): Bool = (bank_idx === w.U)

      // one bank
      val rob_val       = RegInit(VecInit(Seq.fill(NUM_ROB_ROWS){false.B}))
      val rob_bsy       = Mem(NUM_ROB_ROWS, Bool())
      val rob_safe      = Mem(NUM_ROB_ROWS, Bool())
      val rob_uop       = Reg(Vec(NUM_ROB_ROWS, new MicroOp()))
      val rob_exception = Mem(NUM_ROB_ROWS, Bool())
      val rob_fflags    = Mem(NUM_ROB_ROWS, Bits(freechips.rocketchip.tile.FPConstants.FLAGS_SZ.W))

      //-----------------------------------------------
      // Dispatch: Add Entry to ROB

      when (io.enq_valids(w))
      {
         rob_val(rob_tail)       := true.B
         rob_bsy(rob_tail)       := !io.enq_uops(w).is_fence &&
                                    !(io.enq_uops(w).is_fencei)
         rob_safe(rob_tail)      := !io.enq_uops(w).may_xcpt && !io.enq_uops(w).exception
         rob_uop(rob_tail)       := io.enq_uops(w)
         rob_exception(rob_tail) := io.enq_uops(w).exception
         rob_fflags(rob_tail)    := 0.U
         rob_uop(rob_tail).stat_brjmp_mispredicted := false.B

         assert (rob_val(rob_tail) === false.B, "[rob] overwriting a valid entry.")
         assert ((io.enq_uops(w).rob_idx >> log2Ceil(width)) === rob_tail)
      }
      .elsewhen (io.enq_valids.reduce(_|_) && !rob_val(rob_tail))
      {
         rob_uop(rob_tail).inst := BUBBLE // just for debug purposes
      }

      //-----------------------------------------------
      // Writeback

      for (i <- 0 until num_wakeup_ports)
      {
         val wb_resp = io.wb_resps(i)
         val wb_uop = wb_resp.bits.uop
         val row_idx = GetRowIdx(wb_uop.rob_idx)
         when (wb_resp.valid && MatchBank(GetBankIdx(wb_uop.rob_idx)))
         {
            rob_bsy(row_idx)    := false.B
            rob_safe(row_idx)   := true.B
            if (O3PIPEVIEW_PRINTF)
            {
               printf("%d; O3PipeView:complete:%d\n",
                  rob_uop(row_idx).debug_events.fetch_seq,
                  io.debug_tsc)
            }
         }
         // TODO check that fflags aren't overwritten
         // TODO check that the wb is to a valid ROB entry, give it a time stamp
//         assert (!(wb_resp.valid && MatchBank(GetBankIdx(wb_uop.rob_idx)) &&
//                  wb_uop.fp_val && !(wb_uop.is_load || wb_uop.is_store) &&
//                  rob_exc_cause(row_idx) =/= 0.U),
//                  "FP instruction writing back exc bits is overriding an existing exception.")
      }

      // Stores have a separate method to clear busy bits
      for ((clr_valid, clr_rob_idx) <- io.lsu_clr_bsy_valid zip io.lsu_clr_bsy_rob_idx)
      {
         when (clr_valid && MatchBank(GetBankIdx(clr_rob_idx)))
         {
            val cidx = GetRowIdx(clr_rob_idx)
            rob_bsy(cidx)    := false.B
            rob_safe(cidx)   := true.B
            assert (rob_val(cidx) === true.B, "[rob] store writing back to invalid entry.")
            assert (rob_bsy(cidx) === true.B, "[rob] store writing back to a not-busy entry.")

            if (O3PIPEVIEW_PRINTF)
            {
               printf("%d; O3PipeView:complete:%d\n",
                  rob_uop(GetRowIdx(clr_rob_idx)).debug_events.fetch_seq, io.debug_tsc)
            }
         }
      }

      when (io.lsu_mem_success && MatchBank(GetBankIdx(io.lsu_mem_success_rob_idx)))
      {
         val cidx = GetRowIdx(io.lsu_mem_success_rob_idx)
         rob_safe(cidx) := true.B
      }

      when (io.brinfo.valid && MatchBank(GetBankIdx(io.brinfo.rob_idx)))
      {
         rob_uop(GetRowIdx(io.brinfo.rob_idx)).stat_brjmp_mispredicted := io.brinfo.mispredict
         rob_uop(GetRowIdx(io.brinfo.rob_idx)).stat_btb_mispredicted   := io.brinfo.btb_mispredict
         rob_uop(GetRowIdx(io.brinfo.rob_idx)).stat_btb_made_pred      := io.brinfo.btb_made_pred
         rob_uop(GetRowIdx(io.brinfo.rob_idx)).stat_bpd_mispredicted   := io.brinfo.bpd_mispredict
         rob_uop(GetRowIdx(io.brinfo.rob_idx)).stat_bpd_made_pred      := io.brinfo.bpd_made_pred
      }

      //-----------------------------------------------
      // Accruing fflags
      for (i <- 0 until num_fpu_ports)
      {
         val fflag_uop = io.fflags(i).bits.uop
         when (io.fflags(i).valid && MatchBank(GetBankIdx(fflag_uop.rob_idx)))
         {
            rob_fflags(GetRowIdx(fflag_uop.rob_idx)) := io.fflags(i).bits.flags
         }
      }

      //-----------------------------------------------------
      // Exceptions
      // (the cause bits are compressed and stored elsewhere)

      when (io.lxcpt.valid && MatchBank(GetBankIdx(io.lxcpt.bits.uop.rob_idx)))
      {
         rob_exception(GetRowIdx(io.lxcpt.bits.uop.rob_idx)) := true.B
         when (io.lxcpt.bits.cause =/= MINI_EXCEPTION_MEM_ORDERING)
         {
            // For mem-ordering failures the failing load will have been marked safe already
            assert(!rob_safe(GetRowIdx(io.lxcpt.bits.uop.rob_idx)),
               "An instruction marked as safe is causing an exception")
         }
      }
      when (io.bxcpt.valid && MatchBank(GetBankIdx(io.bxcpt.bits.uop.rob_idx)))
      {
         rob_exception(GetRowIdx(io.bxcpt.bits.uop.rob_idx)) := true.B
         assert(!rob_safe(GetRowIdx(io.bxcpt.bits.uop.rob_idx)),
            "An instruction marked as safe is causing an exception")

      }
      can_throw_exception(w) := rob_val(rob_head) && rob_exception(rob_head)

      //-----------------------------------------------
      // Commit or Rollback

      // Can this instruction commit? (the check for exceptions/rob_state happens later).
      can_commit(w)   := rob_val(rob_head) && !(rob_bsy(rob_head)) && !io.csr_stall

      // If the PNR is at the tail, we could be in a partially enqueued row,
      //  so block the pointer from moving past invalid entries when enq_stall
      // If the PNR is at the head, we could be in a partially committing row,
      //  so move the pointer ahead past invalid entries
      // Handle both cases here
      rob_pnr_safe(w) := Mux((rob_pnr === rob_tail) && io.enq_partial_stall,
         rob_val(rob_pnr) && rob_safe(rob_pnr) && !rob_exception(rob_pnr),
         !rob_val(rob_pnr) || (rob_safe(rob_pnr) && !rob_exception(rob_pnr)))

      val com_idx = Wire(UInt())
      com_idx := rob_head
      when (rob_state === s_rollback)
      {
         com_idx := rob_tail
      }

      // use the same "com_uop" for both rollback AND commit
      // Perform Commit
      io.commit.valids(w)     := will_commit(w)
      io.commit.uops(w)       := rob_uop(com_idx)

      io.commit.rbk_valids(w) :=
                              (rob_state === s_rollback) &&
                              rob_val(com_idx) &&
                              (rob_uop(com_idx).dst_rtype === RT_FIX || rob_uop(com_idx).dst_rtype === RT_FLT) &&
                              (!(ENABLE_COMMIT_MAP_TABLE.B))

      when (rob_state === s_rollback)
      {
         rob_val(com_idx)       := false.B
         rob_exception(com_idx) := false.B
      }

      if (ENABLE_COMMIT_MAP_TABLE)
      {
         when (RegNext(exception_thrown))
         {
            for (i <- 0 until NUM_ROB_ROWS)
            {
               rob_val(i)      := false.B
               rob_bsy(i)      := false.B
               rob_uop(i).inst := BUBBLE
            }
         }
      }

      // -----------------------------------------------
      // Kill speculated entries on branch mispredict
      for (i <- 0 until NUM_ROB_ROWS)
      {
         val br_mask = rob_uop(i).br_mask
         val entry_match = rob_val(i) && maskMatch(io.brinfo.mask, br_mask)

         //kill instruction if mispredict & br mask match
         when (io.brinfo.valid && io.brinfo.mispredict && entry_match)
         {
            rob_val(i) := false.B
            rob_uop(i.U).inst := BUBBLE
         }
         .elsewhen (io.brinfo.valid && !io.brinfo.mispredict && entry_match)
         {
            // clear speculation bit even on correct speculation
            rob_uop(i).br_mask := (br_mask & ~io.brinfo.mask)
         }
      }

      // -----------------------------------------------
      // Commit
      when (will_commit(w))
      {
         rob_val(rob_head) := false.B
      }

      // -----------------------------------------------
      // Outputs
      rob_head_vals(w)     := rob_val(rob_head)
      rob_head_fflags(w)   := rob_fflags(rob_head)
      rob_head_is_store(w) := rob_uop(rob_head).is_store
      rob_head_is_load(w)  := rob_uop(rob_head).is_load

      // -----------------------------------------------
      // debugging write ports that should not be synthesized
      when (will_commit(w))
      {
         rob_uop(rob_head).inst := BUBBLE
      }
      .elsewhen (rob_state === s_rollback)
      {
         rob_uop(rob_tail).inst := BUBBLE
      }

      //--------------------------------------------------
      // Debug: for debug purposes, track side-effects to all register destinations

      for (i <- 0 until num_wakeup_ports)
      {
         val rob_idx = io.wb_resps(i).bits.uop.rob_idx
         when (io.debug_wb_valids(i) && MatchBank(GetBankIdx(rob_idx)))
         {
            rob_uop(GetRowIdx(rob_idx)).debug_wdata := io.debug_wb_wdata(i)
         }
         val temp_uop = rob_uop(GetRowIdx(rob_idx))

         assert (!(io.wb_resps(i).valid && MatchBank(GetBankIdx(rob_idx)) &&
                     !rob_val(GetRowIdx(rob_idx))),
                  "[rob] writeback (" + i + ") occurred to an invalid ROB entry.")
         assert (!(io.wb_resps(i).valid && MatchBank(GetBankIdx(rob_idx)) &&
                     !rob_bsy(GetRowIdx(rob_idx))),
                  "[rob] writeback (" + i + ") occurred to a not-busy ROB entry.")
         assert (!(io.wb_resps(i).valid && MatchBank(GetBankIdx(rob_idx)) &&
                  temp_uop.ldst_val && temp_uop.pdst =/= io.wb_resps(i).bits.uop.pdst),
                  "[rob] writeback (" + i + ") occurred to the wrong pdst.")
      }
      io.commit.uops(w).debug_wdata := rob_uop(rob_head).debug_wdata

      //--------------------------------------------------
      // Debug: handle passing out signals to printf in dpath

      if (DEBUG_PRINTF_ROB)
      {
         for (i <- 0 until NUM_ROB_ROWS)
         {
            debug_entry(w + i*width).valid     := rob_val(i)
            debug_entry(w + i*width).busy      := rob_bsy(i.U)
            debug_entry(w + i*width).safe      := rob_safe(i.U)
            debug_entry(w + i*width).uop       := rob_uop(i.U)
            debug_entry(w + i*width).exception := rob_exception(i.U)
         }
      }

   } //for (w <- 0 until width)

   // **************************************************************************
   // --------------------------------------------------------------------------
   // **************************************************************************

   // -----------------------------------------------
   // Commit Logic
   // need to take a "can_commit" array, and let the first can_commits commit
   // previous instructions may block the commit of younger instructions in the commit bundle
   // e.g., exception, or (valid && busy).
   // Finally, don't throw an exception if there are instructions in front of
   // it that want to commit (only throw exception when head of the bundle).

   var block_commit = (rob_state =/= s_normal) && (rob_state =/= s_wait_till_empty)
   var will_throw_exception = false.B
   var block_xcpt   = false.B

   for (w <- 0 until width)
   {
      will_throw_exception = (can_throw_exception(w) && !block_commit && !block_xcpt) || will_throw_exception

      will_commit(w)       := can_commit(w) && !can_throw_exception(w) && !block_commit
      block_commit         = (rob_head_vals(w) &&
                             (!can_commit(w) || can_throw_exception(w))) | block_commit
      block_xcpt           = will_commit(w)
   }

   // Note: exception must be in the commit bundle.
   // Note: exception must be the first valid instruction in the commit bundle.
   exception_thrown    := will_throw_exception
   val is_mini_exception = io.com_xcpt.bits.cause === MINI_EXCEPTION_MEM_ORDERING ||
                           io.com_xcpt.bits.cause === MINI_EXCEPTION_REPLAY
   io.com_xcpt.valid := exception_thrown && !is_mini_exception
   io.com_xcpt.bits.cause := r_xcpt_uop.exc_cause

   io.com_xcpt.bits.badvaddr := Sext(r_xcpt_badvaddr, xLen)
   val insn_sys_pc2epc =
      rob_head_vals.reduce(_|_) && PriorityMux(rob_head_vals, io.commit.uops.map{u => u.is_sys_pc2epc})

   val refetch_inst = exception_thrown || insn_sys_pc2epc
   val com_xcpt_uop = PriorityMux(rob_head_vals, io.commit.uops)
   io.com_xcpt.bits.ftq_idx   := com_xcpt_uop.ftq_idx
   io.com_xcpt.bits.edge_inst := com_xcpt_uop.edge_inst
   io.com_xcpt.bits.is_rvc    := com_xcpt_uop.is_rvc
   io.com_xcpt.bits.pc_lob    := com_xcpt_uop.pc_lob

   val flush_commit_mask = Range(0,width).map{i => io.commit.valids(i) && io.commit.uops(i).flush_on_commit}
   val flush_commit = flush_commit_mask.reduce(_|_)
   val flush_val = exception_thrown || flush_commit

   assert(!(PopCount(flush_commit_mask) > 1.U),
      "[rob] Can't commit multiple flush_on_commit instructions on one cycle")

   val flush_uop = Mux(exception_thrown, com_xcpt_uop, Mux1H(flush_commit_mask, io.commit.uops))

   // delay a cycle for critical path considerations
   io.flush.valid          := RegNext(flush_val, init=false.B)
   io.flush.bits.ftq_idx   := RegNext(flush_uop.ftq_idx)
   io.flush.bits.pc_lob    := RegNext(flush_uop.pc_lob)
   io.flush.bits.edge_inst := RegNext(flush_uop.edge_inst)
   io.flush.bits.is_rvc    := RegNext(flush_uop.is_rvc)
   io.flush.bits.flush_typ := RegNext(FlushTypes.getType(flush_val,
                                                         exception_thrown && !is_mini_exception,
                                                         flush_commit && flush_uop.uopc === uopERET,
                                                         refetch_inst))

   val com_lsu_misspec = RegNext(exception_thrown && io.com_xcpt.bits.cause === MINI_EXCEPTION_MEM_ORDERING)
   assert (!(com_lsu_misspec && !io.flush.valid), "[rob] pipeline flush not be exercised during a LSU misspeculation")

   // -----------------------------------------------
   // FP Exceptions
   // send fflags bits to the CSRFile to accrue

   val fflags_val = Wire(Vec(width, Bool()))
   val fflags     = Wire(Vec(width, UInt(freechips.rocketchip.tile.FPConstants.FLAGS_SZ.W)))

   for (w <- 0 until width)
   {
      fflags_val(w) :=
         io.commit.valids(w) &&
         io.commit.uops(w).fp_val &&
         !(io.commit.uops(w).is_load || io.commit.uops(w).is_store)

      fflags(w) := Mux(fflags_val(w), rob_head_fflags(w), 0.U)

      assert (!(io.commit.valids(w) &&
               !io.commit.uops(w).fp_val &&
               rob_head_fflags(w) =/= 0.U),
               "Committed non-FP instruction has non-zero fflag bits.")
      assert (!(io.commit.valids(w) &&
               io.commit.uops(w).fp_val &&
               (io.commit.uops(w).is_load || io.commit.uops(w).is_store) &&
               rob_head_fflags(w) =/= 0.U),
               "Committed FP load or store has non-zero fflag bits.")
   }
   io.commit.fflags.valid := fflags_val.reduce(_|_)
   io.commit.fflags.bits  := fflags.reduce(_|_)

   // -----------------------------------------------
   // Exception Tracking Logic
   // only store the oldest exception, since only one can happen!

   // is i0 older than i1? (closest to zero). Provide the tail_ptr to the
   // queue. This is Cat(i1 <= tail, i1) because the rob_tail can point to a
   // valid (partially dispatched) row.
   def IsOlder(i0: UInt, i1: UInt, tail: UInt) = (Cat(i0 <= tail, i0) < Cat(i1 <= tail, i1))
   val next_xcpt_uop = Wire(new MicroOp())
   next_xcpt_uop := r_xcpt_uop
   val enq_xcpts = Wire(Vec(width, Bool()))
   for (i <- 0 until width)
   {
      enq_xcpts(i) := io.enq_valids(i) && io.enq_uops(i).exception
   }

   when (!(io.flush.valid || exception_thrown) && rob_state =/= s_rollback)
   {
      when (io.lxcpt.valid || io.bxcpt.valid)
      {
         val load_is_older =
            (io.lxcpt.valid && !io.bxcpt.valid) ||
            (io.lxcpt.valid && io.bxcpt.valid &&
            IsOlder(io.lxcpt.bits.uop.rob_idx, io.bxcpt.bits.uop.rob_idx, rob_tail_idx))
         val new_xcpt_uop = Mux(load_is_older, io.lxcpt.bits.uop, io.bxcpt.bits.uop)

         when (!r_xcpt_val || IsOlder(new_xcpt_uop.rob_idx, r_xcpt_uop.rob_idx, rob_tail_idx))
         {
            r_xcpt_val              := true.B
            next_xcpt_uop           := new_xcpt_uop
            next_xcpt_uop.exc_cause := Mux(io.lxcpt.valid, io.lxcpt.bits.cause, io.bxcpt.bits.cause)
            r_xcpt_badvaddr         := Mux(io.lxcpt.valid, io.lxcpt.bits.badvaddr, io.bxcpt.bits.badvaddr)
         }
      }
      .elsewhen (!r_xcpt_val && enq_xcpts.reduce(_|_))
      {
         val idx = enq_xcpts.indexWhere{i: Bool => i}

         // if no exception yet, dispatch exception wins
         r_xcpt_val      := true.B
         next_xcpt_uop   := io.enq_uops(idx)
         r_xcpt_badvaddr := io.enq_uops(idx).pc + Mux(io.enq_uops(idx).edge_inst, 2.U, 0.U)
         // TODO XXX REMOVE THIS. Temporary hack to fix ma-fetch tests.
         // The problem is we shouldn't have access to pc and inst in the ROB.
         // This should be handled by the front-end.
         when ((io.enq_uops(idx).uopc === uopJAL) && !io.enq_uops(idx).exc_cause.orR)
         {
            r_xcpt_badvaddr := ComputeJALTarget(io.enq_uops(idx).pc, ExpandRVC(io.enq_uops(idx).inst), xLen)
         }
      }
   }

   r_xcpt_uop         := next_xcpt_uop
   r_xcpt_uop.br_mask := GetNewBrMask(io.brinfo, next_xcpt_uop)
   when (io.flush.valid || IsKilledByBranch(io.brinfo, next_xcpt_uop))
   {
      r_xcpt_val := false.B
   }

   assert (!(exception_thrown && !r_xcpt_val),
      "ROB trying to throw an exception, but it doesn't have a valid xcpt_cause")

   assert (!(io.empty && r_xcpt_val),
      "ROB is empty, but believes it has an outstanding exception.")

   assert (!(will_throw_exception && (GetRowIdx(r_xcpt_uop.rob_idx) =/= rob_head)),
      "ROB is throwing an exception, but the stored exception information's " +
      "rob_idx does not match the rob_head")

   // -----------------------------------------------
   // ROB Head Logic

   // remember if we're still waiting on the rest of the dispatch packet, and prevent
   // the rob_head from advancing if it commits a partial parket before we
   // dispatch the rest of it.
   // update when committed ALL valid instructions in commit_bundle

   val r_partial_row = RegInit(false.B)

   when (io.enq_valids.reduce(_|_))
   {
      r_partial_row := io.enq_partial_stall
   }

   val finished_committing_row =
      (io.commit.valids.asUInt =/= 0.U) &&
      ((will_commit.asUInt ^ rob_head_vals.asUInt) === 0.U) &&
      !(r_partial_row && rob_head === rob_tail)

   when (finished_committing_row)
   {
      rob_head := WrapInc(rob_head, NUM_ROB_ROWS)
   }
   assert(!(rob_head === WrapInc(rob_pnr, NUM_ROB_ROWS) && rob_pnr =/= rob_tail),
      "ROB head overran the PNR head!")
   // -----------------------------------------------
   // ROB PNR Head Logic

   val pnr_safe_to_advance_row =
      rob_pnr =/= rob_tail &&
      rob_pnr_safe.reduce(_&&_) &&
      (rob_state === s_normal || rob_state === s_wait_till_empty) &&
      !exception_thrown
   val pnr_safe_to_advance_entry =
      rob_pnr_safe(rob_pnr_lsb) &&
      (rob_state === s_normal || rob_state === s_wait_till_empty) &&
      !exception_thrown
   // We advance the PNR by a row
   when (pnr_safe_to_advance_row)
   {
      rob_pnr     := WrapInc(rob_pnr, NUM_ROB_ROWS)
      rob_pnr_lsb := 0.U
   }
      // If the ROB row is partially valid, we advance the LSB
      .elsewhen (pnr_safe_to_advance_entry)
   {
      rob_pnr_lsb := rob_pnr_lsb + 1.U
   }

   // -----------------------------------------------
   // ROB Tail Logic

   when (rob_state === s_rollback && rob_tail =/= rob_head)
   {
      rob_tail     := WrapDec(rob_tail, NUM_ROB_ROWS)
      rob_tail_lsb := 0.U
   }
   .elsewhen (io.brinfo.valid && io.brinfo.mispredict)
   {
      rob_tail     := WrapInc(GetRowIdx(io.brinfo.rob_idx), NUM_ROB_ROWS)
      rob_tail_lsb := 0.U
   }
   .elsewhen (io.enq_valids.asUInt =/= 0.U && !io.enq_partial_stall)
   {
      rob_tail     := WrapInc(rob_tail, NUM_ROB_ROWS)
      rob_tail_lsb := 0.U
   }
   .elsewhen (io.enq_valids.asUInt =/= 0.U && io.enq_partial_stall)
   {
      rob_tail_lsb := OHToUInt(io.enq_valids) + 1.U
   }

   if (ENABLE_COMMIT_MAP_TABLE)
   {
      when (RegNext(exception_thrown))
      {
         rob_tail     := 0.U
         rob_tail_lsb := 0.U
         rob_head     := 0.U
         rob_pnr      := 0.U
         rob_pnr_lsb  := 0.U
      }
   }

   // -----------------------------------------------
   // Full Logic

   // TODO can we let the ROB fill up completely?
   // can we track "maybe_full"?
   // maybe full is reset on branch mispredict
   // ALSO must handle xcpt tail/age logic if we do this!
   // also must handle rob_pc valid logic.
   val full = WrapInc(rob_tail, NUM_ROB_ROWS) === rob_head

   io.empty := (rob_head === rob_tail) && (rob_head_vals.asUInt === 0.U)

   io.curr_rob_tail_idx := rob_tail_idx
   io.curr_rob_pnr_idx  := rob_pnr_idx
   io.ready := (rob_state === s_normal) && !full

   //-----------------------------------------------
   //-----------------------------------------------
   //-----------------------------------------------

   // ROB FSM
   if (!ENABLE_COMMIT_MAP_TABLE)
   {
       switch (rob_state)
      {
         is (s_reset)
         {
            rob_state := s_normal
         }
         is (s_normal)
         {
            when (exception_thrown)
            {
               rob_state := s_rollback
            }
            .otherwise
            {
               for (w <- 0 until width)
               {
                  when (io.enq_valids(w) && io.enq_uops(w).is_unique)
                  {
                     rob_state := s_wait_till_empty
                  }
               }
            }
         }
         is (s_rollback)
         {
            when (io.empty)
            {
               rob_state := s_normal
            }
         }
         is (s_wait_till_empty)
         {
            when (exception_thrown)
            {
               rob_state := s_rollback
            }
            .elsewhen (io.empty)
            {
               rob_state := s_normal
            }
         }
      }
   }
   else
   {
      switch (rob_state)
      {
         is (s_reset)
         {
            rob_state := s_normal
         }
         is (s_normal)
         {
            when (exception_thrown)
            {
               ; //rob_state := s_rollback
            }
            .otherwise
            {
               for (w <- 0 until width)
               {
                  when (io.enq_valids(w) && io.enq_uops(w).is_unique)
                  {
                     rob_state := s_wait_till_empty
                  }
               }
            }
         }
         is (s_rollback)
         {
            when (rob_tail  === rob_head)
            {
               rob_state := s_normal
            }
         }
         is (s_wait_till_empty)
         {
            when (exception_thrown)
            {
               ; //rob_state := s_rollback
            }
            .elsewhen (rob_tail === rob_head)
            {
               rob_state := s_normal
            }
         }
      }
   }

   // -----------------------------------------------
   // Outputs

   for (w <- 0 until width)
   {
      // tell LSU it is ready to its stores and loads
      io.commit.st_mask(w) := io.commit.valids(w) && rob_head_is_store(w)
      io.commit.ld_mask(w) := io.commit.valids(w) && rob_head_is_load(w)
   }

   io.com_load_is_at_rob_head := rob_head_is_load(PriorityEncoder(rob_head_vals.asUInt))

   //--------------------------------------------------
   // Handle passing out signals to printf in dpath

   io.debug.state    := rob_state
   io.debug.rob_head := rob_head
   io.debug.rob_pnr := rob_pnr
   io.debug.xcpt_val := r_xcpt_val
   io.debug.xcpt_uop := r_xcpt_uop
   io.debug.xcpt_badvaddr := r_xcpt_badvaddr

   if (DEBUG_PRINTF_ROB)
   {
      printf("  RobXcpt[%c%x r:%d b:%x bva:0x%x]\n",
         Mux(r_xcpt_val, Str("E"),Str("-")),
         io.debug.xcpt_uop.exc_cause,
         io.debug.xcpt_uop.rob_idx,
         io.debug.xcpt_uop.br_mask,
         io.debug.xcpt_badvaddr
         )

      var r_idx = 0
      // scalastyle:off
      for (i <- 0 until (NUM_ROB_ENTRIES/COMMIT_WIDTH))
      {
//            rob[ 0]           (  )(  ) 0x00002000 [ -                       ][unknown                  ]    ,   (d:X p 1, bm:0 - sdt: 0) (d:- p 3, bm:f - sdt:60)
//            rob[ 1]           (  )(B ) 0xc71cb68e [flw     fa3, -961(s11)   ][ -                       ] E31,   (d:- p22, bm:e T sdt:57) (d:- p 0, bm:0 - sdt: 0)
//            rob[ 2] HEAD ---> (vv)( b) 0x00002008 [lui     ra, 0x2          ][addi    ra, ra, 704      ]    ,   (d:x p 2, bm:1 - sdt: 0) (d:x p 3, bm:1 - sdt: 2)
//            rob[ 3]           (vv)(bb) 0x00002010 [lw      s1, 0(ra)        ][lui     t3, 0xff0        ]    ,   (d:x p 4, bm:0 - sdt: 0) (d:x p 5, bm:0 - sdt: 0)
//            rob[ 4]      TL-> (v )(b ) 0x00002015 - 2018 [addiw   t3, t3, 255      ][li      t2, 2            ]    ,   (d:x p 6, bm:0 - sdt: 5) (d:x p 7, bm:0 - sdt: 0)

         val row = if (COMMIT_WIDTH == 1) r_idx else (r_idx >> log2Ceil(COMMIT_WIDTH))
         val r_head = rob_head
         val r_tail = rob_tail

         printf("    rob[%d] %c %c (",
            row.U(ROB_ADDR_SZ.W),
            Mux(r_head === row.U && r_tail === row.U, Str("B"),
              Mux(r_head === row.U, Str("H"),
              Mux(r_tail === row.U, Str("T"),
                                        Str(" ")))),
            Mux(rob_pnr === row.U, Str("P"), Str(" "))
          )

         if (COMMIT_WIDTH == 1)
         {
            printf("(%c)(%c)(%c) 0x%x [DASM(%x)] %c ",
                   Mux(debug_entry(r_idx+0).valid, Str("V"), Str(" ")),
                   Mux(debug_entry(r_idx+0).busy, Str("B"),  Str(" ")),
                   Mux(debug_entry(r_idx+0).safe, Str("S"),  Str(" ")),
                   debug_entry(r_idx+0).uop.pc(31,0),
                   debug_entry(r_idx+0).uop.inst,
                   Mux(debug_entry(r_idx+0).exception, Str("E"), Str("-"))
                   )
         }
         else if (COMMIT_WIDTH == 2)
         {
            val row_is_val = debug_entry(r_idx+0).valid || debug_entry(r_idx+1).valid
            printf("(%c%c)(%c%c)(%c%c) 0x%x %x [DASM(%x)][DASM(%x)" + "] %c,%c %d,%d ",
                   Mux(debug_entry(r_idx+0).valid, Str("V"), Str(" ")),
                   Mux(debug_entry(r_idx+1).valid, Str("V"), Str(" ")),
                   Mux(debug_entry(r_idx+0).busy,  Str("B"), Str(" ")),
                   Mux(debug_entry(r_idx+1).busy,  Str("B"), Str(" ")),
                   Mux(debug_entry(r_idx+0).safe,  Str("S"), Str(" ")),
                   Mux(debug_entry(r_idx+1).safe,  Str("S"), Str(" ")),
                   debug_entry(r_idx+0).uop.pc(31,0),
                   debug_entry(r_idx+1).uop.pc(15,0),
                   debug_entry(r_idx+0).uop.inst,
                   debug_entry(r_idx+1).uop.inst,
                   Mux(debug_entry(r_idx+0).exception, Str("E"), Str("-")),
                   Mux(debug_entry(r_idx+1).exception, Str("E"), Str("-")),
                   debug_entry(r_idx+0).uop.ftq_idx,
                   debug_entry(r_idx+1).uop.ftq_idx
                   )
         }
         else if (COMMIT_WIDTH == 4)
         {
            val row_is_val = debug_entry(r_idx+0).valid || debug_entry(r_idx+1).valid || debug_entry(r_idx+2).valid || debug_entry(r_idx+3).valid
            printf("(%c%c%c%c)(%c%c%c%c)(%c%c%c%c) 0x%x %x %x %x [DASM(%x)][DASM(%x)][DASM(%x)][DASM(%x)" + "]%c%c%c%c",
                   Mux(debug_entry(r_idx+0).valid, Str("V"), Str(" ")),
                   Mux(debug_entry(r_idx+1).valid, Str("V"), Str(" ")),
                   Mux(debug_entry(r_idx+2).valid, Str("V"), Str(" ")),
                   Mux(debug_entry(r_idx+3).valid, Str("V"), Str(" ")),
                   Mux(debug_entry(r_idx+0).busy,  Str("B"), Str(" ")),
                   Mux(debug_entry(r_idx+1).busy,  Str("B"), Str(" ")),
                   Mux(debug_entry(r_idx+2).busy,  Str("B"), Str(" ")),
                   Mux(debug_entry(r_idx+3).busy,  Str("B"), Str(" ")),
                   Mux(debug_entry(r_idx+0).safe,  Str("S"), Str(" ")),
                   Mux(debug_entry(r_idx+1).safe,  Str("S"), Str(" ")),
                   Mux(debug_entry(r_idx+2).safe,  Str("S"), Str(" ")),
                   Mux(debug_entry(r_idx+3).safe,  Str("S"), Str(" ")),
                   debug_entry(r_idx+0).uop.pc(23,0),
                   debug_entry(r_idx+1).uop.pc(15,0),
                   debug_entry(r_idx+2).uop.pc(15,0),
                   debug_entry(r_idx+3).uop.pc(15,0),
                   debug_entry(r_idx+0).uop.inst,
                   debug_entry(r_idx+1).uop.inst,
                   debug_entry(r_idx+2).uop.inst,
                   debug_entry(r_idx+3).uop.inst,
                   Mux(debug_entry(r_idx+0).exception, Str("E"), Str("-")),
                   Mux(debug_entry(r_idx+1).exception, Str("E"), Str("-")),
                   Mux(debug_entry(r_idx+2).exception, Str("E"), Str("-")),
                   Mux(debug_entry(r_idx+3).exception, Str("E"), Str("-"))
                   )
         }
         else
         {
            println("  BOOM's Chisel printf does not support commit_width >= " + COMMIT_WIDTH)
         }

         var temp_idx = r_idx
         for (w <- 0 until COMMIT_WIDTH)
         {
            printf("(d:%c p%d, bm:%x sdt:%d) ",
                   Mux(debug_entry(temp_idx).uop.dst_rtype === RT_FIX, Str("X"),
                   Mux(debug_entry(temp_idx).uop.dst_rtype === RT_PAS, Str("C"),
                   Mux(debug_entry(temp_idx).uop.dst_rtype === RT_FLT, Str("f"),
                   Mux(debug_entry(temp_idx).uop.dst_rtype === RT_X, Str("-"), Str("?"))))),
                   debug_entry    (temp_idx).uop.pdst,
                   debug_entry    (temp_idx).uop.br_mask,
                   debug_entry    (temp_idx).uop.stale_pdst
            )
            temp_idx = temp_idx + 1
         }

         r_idx = r_idx + COMMIT_WIDTH

         printf("\n")
      }
      // scalastyle:off
   }

   override def toString: String =
      "\n   ==ROB==" +
      "\n   Machine Width  : " + width +
      "\n   Rob Entries    : " + num_rob_entries +
      "\n   Rob Rows       : " + NUM_ROB_ROWS +
      "\n   Rob Row size   : " + log2Ceil(NUM_ROB_ROWS) +
      "\n   log2Ceil(width): " + log2Ceil(width) +
      "\n   FPU FFlag Ports: " + num_fpu_ports
}
