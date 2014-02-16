//**************************************************************************
// RISCV Re-order Buffer
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2013 Oct 18
//

// Bank the ROB, such that each "dispatch" group gets its own row of the ROB,
// and each instruction in the dispatch group goes to a different bank.
// We can compress out the PC by only saving the high-order bits.
//
// ASSUMPTION: dispatch groups are aligned to the PC.
//
// NOTES: 
//    - Currently we do not compress out bubbles in the ROB. 
//    - commit_width is tied directly to the dispatch_width.
//    - Exceptions are only taken when at the head of the commit bundle. 
//      This helps deal with loads, stores, and refetch instructions.
//

package BOOM

import Chisel._
import Node._
import scala.math.ceil


class RobIo(machine_width: Int, num_wakeup_ports: Int)  extends Bundle()
{
   // Dispatch Stage                                                 
   // (Write Instruction to ROB from Dispatch Stage)                 
   val dis_mask         = Vec.fill(machine_width) { Bool(INPUT) }
   val dis_uops         = Vec.fill(machine_width) { new MicroOp().asInput() }

   val curr_rob_tail    = UInt(OUTPUT, ROB_ADDR_SZ) 

   // Write-back Stage
   // (Update of ROB)
   // Instruction is no longer busy and can be committed
   // currently all supported exceptions are detected in Decode (except load-ordering failures)
   val wb_valids        = Vec.fill(num_wakeup_ports) { Bool(INPUT) }
   val wb_rob_idxs      = Vec.fill(num_wakeup_ports) { UInt(INPUT, ROB_ADDR_SZ) }
   
   // track side-effects for debug purposes.
   // Also need to know when loads write back, whereas we don't need loads to unbusy.
   val debug_wb_valids  = Vec.fill(num_wakeup_ports) { Bool(INPUT) }
   val debug_wb_wdata   = Vec.fill(num_wakeup_ports) { Bits(INPUT, XPRLEN) }

   val mem_xcpt_val     = Bool(INPUT)
   val mem_xcpt_uop     = new MicroOp().asInput
   val mem_xcpt         = (new rocket.HellaCacheExceptions).asInput

   // load-ordering failure - treat as exception, even though it's just a rollback/pipeline restart
   // TODO consolidate exception ports?
   val ldo_xcpt_val     = Bool(INPUT)
   val ldo_xcpt_uop     = new MicroOp().asInput
   
   // Commit Stage
   // (Free no-longer used register).
   // Also used for rollback.
   val com_valids       = Vec.fill(machine_width) {Bool(OUTPUT)}
   val com_uops         = Vec.fill(machine_width) {new MicroOp().asOutput()}

   // tell the LSU how many stores and loads are being committed
   val com_st_mask      = Vec.fill(machine_width) {Bool(OUTPUT)}
   val com_ld_mask      = Vec.fill(machine_width) {Bool(OUTPUT)} 

   val lsu_clr_bsy_valid = Bool(INPUT)
   val lsu_clr_bsy_rob_idx = UInt(INPUT, ROB_ADDR_SZ)

   // Handle Exceptions/ROB Rollback
   val com_exception    = Bool(OUTPUT)
   val com_exc_cause    = UInt(OUTPUT, EXC_CAUSE_SZ) 
   val com_handling_exc = Bool(OUTPUT)
   val com_rbk_valids   = Vec.fill(machine_width) {Bool(OUTPUT)}
   
   // Handle Branch Misspeculations
   val br_unit          = new BranchUnitResp().asInput()
        
   // Let the Branch Unit read out an instruction's PC
   val get_pc = new Bundle
   {
      val rob_idx  = UInt(INPUT, ROB_ADDR_SZ) 
      val curr_pc  = UInt(OUTPUT, XPRLEN)
      val next_val = Bool(OUTPUT)             // the next_pc may not be valid (stalled or still being fetched)
      val next_pc  = UInt(OUTPUT, XPRLEN)
   }

   // Handle Additional Misspeculations (LSU)
   // tell the LSU a misspec occurred
   val lsu_misspec      = Bool(OUTPUT)

   // When flushing pipeline, need to reset to PC+4 relative to the head of the ROB
   // but because we're doing superscalar commit, the actual flush pc may not be the rob_head pc+4, but rather the last committed instruction in the commit group.
   val flush_val        = Bool(OUTPUT)
   val flush_pc         = UInt(OUTPUT, XPRLEN)
   
   // Stall Decode as appropriate
   val empty            = Bool(OUTPUT)
   val ready            = Bool(OUTPUT) // busy unrolling...


   // pass out debug information to high-level printf
   val debug = new Bundle
   {
      val state = UInt()
      val rob_head = UInt(width = ROB_ADDR_SZ)
      val entry = Vec.fill(1 << ROB_ADDR_SZ) { new Bundle {
         val valid = Bool()
         val busy = Bool()
         val uop = new MicroOp()
         val exception = Bool()
         val eflags = UInt() 
      }}
   }.asOutput
}
 

// width = the dispatch and commit width of the processor
// num_wakeup_ports = self-explanatory
class Rob(width: Int, num_rob_entries: Int, num_wakeup_ports: Int) extends Module 
{
   val io = new RobIo(width, num_wakeup_ports)

   val num_rob_rows = num_rob_entries / width

   println("    Machine Width: " + width)
   println("    Rob Entries  : " + num_rob_entries)
   println("    Rob Rows     : " + num_rob_rows)
   println("    Rob Row size : " + log2Up(num_rob_rows))
   println("    log2UP(width): " + log2Up(width))

   val s_reset :: s_normal :: s_rollback :: s_wait_till_empty :: Nil = Enum(UInt(),4)
   val rob_state = Reg(init = s_reset)
                   
                    
   //commit entries at the head, and unwind exceptions from the tail 
   val rob_head = Reg(init = UInt(0, log2Up(num_rob_rows)))
   val rob_tail = Reg(init = UInt(0, log2Up(num_rob_rows)))
                                     
   val will_commit         = Vec.fill(width) {Bool()}
   val can_commit          = Vec.fill(width) {Bool()}
   val can_throw_exception = Vec.fill(width) {Bool()}
   val rob_head_vals       = Vec.fill(width) {Bool()} // are the instructions at the head valid?
   val rob_head_is_store   = Vec.fill(width) {Bool()}  
   val rob_head_is_load    = Vec.fill(width) {Bool()}  
   val rob_head_eflags     = Vec.fill(width) {UInt()} 
   
   // valid bits at the branch target
   // the br_unit needs to verify the target PC, but it must read out the valid bits
   // for that row
   val rob_brt_vals        = Vec.fill(width) {Bool()} 
   
   val exception_thrown = Bool() 
   

   //--------------------------------------------------
   // Utility

   def GetRowIdx(rob_idx: UInt): UInt = 
   {
      if (width == 1) return rob_idx
      else return rob_idx >> UInt(log2Up(width)) 
   }
   def GetBankIdx(rob_idx: UInt): UInt = 
   {
      if(width == 1) { return UInt(0) } 
      else           { return rob_idx(log2Up(width)-1, 0).toUInt }
   }
                                          

   // **************************************************************************
   // --------------------------------------------------------------------------
   // **************************************************************************
   //
   // PCs
   // store the high-order-bits of the PC only once per ROB row, allowing us to
   // compress out most of the expensive PC information.
   // NOTE: This works since we only write consecutive, aligned instructions
   // into the ROB row.
   //
   // The ROB PC information is written by dispatch at rob_tail.
   // The ROB PC information is read 
   //       - commit for flush PC at rob_head.
   //       - execute by the Br Unit for target calculation at idx X.
   //       - execute by the Br Unit to get actual target at idx X+1.

   val rob_pc_hob = new RobPCs(width)

   when (io.dis_mask.reduce(_|_))
   {
      rob_pc_hob.write(rob_tail, io.dis_uops(0).pc)
   }
   
   // the br unit needs to read out two consecutive ROBs
   val (curr_row_pc, next_row_pc) = rob_pc_hob.read2(GetRowIdx(io.get_pc.rob_idx))

   io.get_pc.curr_pc := curr_row_pc + Cat(GetBankIdx(io.get_pc.rob_idx), Bits(0,2))
   
   val next_bank_idx = if (width == 1) UInt(0) else PriorityEncoder(rob_brt_vals.toBits)
   // Chisel barfing, so let's write it ourselves
//   val next_bank_idx = UInt()
//   next_bank_idx := UInt(0)
//   for (w <- width-1 to 0 by -1)
//   {
//      when (rob_brt_vals(w))
//      {
//         next_bank_idx := UInt(w)
//      }
//   }

   io.get_pc.next_pc := next_row_pc + Cat(next_bank_idx, Bits(0,2))
   io.get_pc.next_val := (GetRowIdx(io.get_pc.rob_idx)+UInt(1)) != rob_tail
 
   
   // **************************************************************************
   // --------------------------------------------------------------------------
   // **************************************************************************

   for (w <- 0 until width)
   {
      def MatchBank(bank_idx: UInt): Bool = (bank_idx === UInt(w))
     
      // one bank 
      val rob_val       = Vec.fill(num_rob_rows) {Reg(init = Bool(false))}
      val rob_bsy       = Mem(Bool(), num_rob_rows)
      val rob_uop       = Vec.fill(num_rob_rows) {Reg(new MicroOp())} // one write port - dispatch
                                                           // fake write ports - clearing on commit,
                                                           // rollback, branch_kill
      val rob_exception = Mem(Bool(), num_rob_rows)        // TODO consolidate into the com_uop? what's the best for Chisel?
      val rob_exc_cause = Mem(UInt(width=EXC_CAUSE_SZ), num_rob_rows)

      //-----------------------------------------------
      // Dispatch: Add Entry to ROB

      when (io.dis_mask(w))
      {
         rob_val(rob_tail)       := Bool(true)
         rob_bsy(rob_tail)       := io.dis_uops(w).uopc != uopSRET &&  // TODO do I need to do this for eret? or should I treat it like it's an exception
                                    io.dis_uops(w).uopc != uopMEMSPECIAL &&
                                    io.dis_uops(w).uopc != uopFENCEI
         rob_uop(rob_tail)       := io.dis_uops(w)
         rob_exception(rob_tail) := io.dis_uops(w).exception 
         rob_exc_cause(rob_tail) := io.dis_uops(w).exc_cause
         rob_uop(rob_tail).br_was_taken := Bool(false) // remove me SYNTH? 
      }
      .elsewhen (io.dis_mask.reduce(_|_))
      {
         rob_uop(rob_tail).inst := BUBBLE // just for debug purposes
      }

       
      //-----------------------------------------------
      // Writeback

      for (i <- 0 until num_wakeup_ports)
      {
         when (io.wb_valids(i) && MatchBank(GetBankIdx(io.wb_rob_idxs(i))))
         {
            rob_bsy(GetRowIdx(io.wb_rob_idxs(i))) := Bool(false)
         }
      }

      // TODO HACK: Loads and Stores have a separate method to clear busy bits
      when (io.lsu_clr_bsy_valid && MatchBank(GetBankIdx(io.lsu_clr_bsy_rob_idx)))
      {
         rob_bsy(GetRowIdx(io.lsu_clr_bsy_rob_idx)) := Bool(false)
      }

      when (io.br_unit.brinfo.valid && MatchBank(GetBankIdx(io.br_unit.brinfo.rob_idx)))
      {
         rob_uop(GetRowIdx(io.br_unit.brinfo.rob_idx)).br_was_taken := io.br_unit.taken
         rob_uop(GetRowIdx(io.br_unit.brinfo.rob_idx)).btb_mispredicted := io.br_unit.btb_mispredict
      }


      //-----------------------------------------------
      // Exceptions
      // only support execute-time exceptions from memory     

      when (io.mem_xcpt_val && MatchBank(GetBankIdx(io.mem_xcpt_uop.rob_idx))) 
      {
         rob_exception(GetRowIdx(io.mem_xcpt_uop.rob_idx)) := Bool(true)
         rob_exc_cause(GetRowIdx(io.mem_xcpt_uop.rob_idx)) := Mux(io.mem_xcpt.ma.ld, UInt(rocket.Causes.misaligned_load), UInt(rocket.Causes.misaligned_store))
      }

      when (io.ldo_xcpt_val && MatchBank(GetBankIdx(io.ldo_xcpt_uop.rob_idx)))
      {
         rob_exception(GetRowIdx(io.ldo_xcpt_uop.rob_idx)) := Bool(true)
         rob_exc_cause(GetRowIdx(io.ldo_xcpt_uop.rob_idx)) := MINI_EXCEPTION_MEM_ORDERING
      }
 
      can_throw_exception(w) := rob_val(rob_head) && rob_exception(rob_head) 

      
      //-----------------------------------------------
      // Commit or Rollback

      // can this instruction commit? (the check for exceptions/rob_state happens later) 
      can_commit(w) := rob_val(rob_head) && !(rob_bsy(rob_head)) 

      val com_idx = UInt() 
      com_idx := rob_head
      when (rob_state === s_rollback)
      {
         com_idx := rob_tail
      }
      
      // use the same "com_uop" for both rollback AND commit
      // Perform Commit
      io.com_valids(w)     := will_commit(w)
      io.com_rbk_valids(w) := (rob_state === s_rollback) &&
                              rob_val(com_idx) &&
                              rob_uop(com_idx).pdst_rtype === RT_FIX
      io.com_uops(w)       := rob_uop(com_idx) 

      when (rob_state === s_rollback)
      {
         rob_val(com_idx)       := Bool(false)
         rob_exception(com_idx) := Bool(false)
      }
      
      // -----------------------------------------------
      // Kill speculated entries on branch mispredict
      for (i <- 0 until num_rob_rows)
      {
         val br_mask = rob_uop(i).br_mask
         val entry_match = rob_val(i) && maskMatch(io.br_unit.brinfo.mask, br_mask)

         //kill instruction if mispredict & br mask match 
         when (io.br_unit.brinfo.valid && io.br_unit.brinfo.mispredict && entry_match)
         {
            rob_val(i) := Bool(false)
            rob_uop(UInt(i)).inst := BUBBLE
         }
         .elsewhen (io.br_unit.brinfo.valid && !io.br_unit.brinfo.mispredict && entry_match)
         {
            // clear speculation bit even on correct speculation
            rob_uop(i).br_mask := (br_mask & ~io.br_unit.brinfo.mask)
         }
      }

      // -----------------------------------------------
      // Commit 
      when (will_commit(w))
      {
         rob_val(rob_head) := Bool(false)
      }

      // -----------------------------------------------
      // Outputs
      rob_head_vals(w)     := rob_val(rob_head)
      rob_head_eflags(w)   := rob_exc_cause(rob_head)
      rob_head_is_store(w) := rob_uop(rob_head).is_store
      rob_head_is_load(w)  := rob_uop(rob_head).is_load
      rob_brt_vals(w)      := rob_val(GetRowIdx(io.get_pc.rob_idx) + UInt(1))
       
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
         val rob_idx = io.wb_rob_idxs(i)
         when (io.debug_wb_valids(i) && MatchBank(GetBankIdx(rob_idx)))
         {
            rob_uop(GetRowIdx(rob_idx)).debug_wdata := io.debug_wb_wdata(i)
         }
      }
      io.com_uops(w).debug_wdata := rob_uop(rob_head).debug_wdata
       
      //--------------------------------------------------
      // Debug: handle passing out signals to printf in dpath
      
      for (i <- 0 until num_rob_rows)
      {
         io.debug.entry(w + i*width).valid := rob_val(i)
         io.debug.entry(w + i*width).busy := rob_bsy(UInt(i))
         io.debug.entry(w + i*width).uop := rob_uop(UInt(i))
         io.debug.entry(w + i*width).uop.pc := rob_pc_hob.read(UInt(i,log2Up(num_rob_rows))) + UInt(w << 2)
         io.debug.entry(w + i*width).exception := rob_exception(UInt(i))
         io.debug.entry(w + i*width).eflags := rob_exc_cause(UInt(i))
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
   
   var block_commit = (rob_state != s_normal) && (rob_state != s_wait_till_empty)
   var will_throw_exception = Bool(false)
   var block_xcpt   = Bool(false) // TODO we can relax this constraint, so long
                                  // as we handle committing stores in
                                  // conjuction with exceptions, and exceptions
                                  // with flush_on_commit (I think).

   for (w <- 0 until width)
   {
      will_throw_exception = (can_throw_exception(w) && !block_commit && !block_xcpt) || will_throw_exception
//      will_throw_exception = (can_throw_exception(w) && !block_commit) || will_throw_exception

      will_commit(w)       := can_commit(w) && !can_throw_exception(w) && !block_commit 
      block_commit         = (rob_head_vals(w) && 
                             (!can_commit(w) || can_throw_exception(w))) | block_commit
      block_xcpt           = will_commit(w)
   }
 
   // exception must be in the commit bundle
   exception_thrown    := will_throw_exception
   val is_mini_exception = io.com_exc_cause === MINI_EXCEPTION_MEM_ORDERING
//   io.com_exception    := (rob_state === s_rollback && (rob_tail === rob_head)) && !is_mini_exception delete me, i throw exception at the end of rob rollback
   io.com_exception    := exception_thrown && !is_mini_exception
   io.com_exc_cause    := PriorityMux(can_throw_exception, rob_head_eflags)
   io.com_handling_exc := exception_thrown  // TODO get rid of com_handling_exc? used to handle loads coming back from the $ probbaly unnecessary

   io.lsu_misspec := exception_thrown && io.com_exc_cause === MINI_EXCEPTION_MEM_ORDERING

   // TODO BUG XXX what if eret and inst_valid excp in same bundle and bad load?
   // flush PC, say an instruction needs to flush the pipeline and refetch either itself or PC+4
   // Note: exception must be the first valid instruction in the commit bundle
   val refetch_inst = exception_thrown //is_mini_exception
   io.flush_pc  := rob_pc_hob.read(rob_head) + 
                   PriorityMux(rob_head_vals, Range(0,width).map(w => UInt(w << 2))) + 
                   Mux(refetch_inst, UInt(0), UInt(4))
   io.flush_val := exception_thrown ||
                     (Range(0,width).map{i => io.com_valids(i) && io.com_uops(i).flush_on_commit}).reduce(_|_)

   // -----------------------------------------------
   // ROB Head Logic
   
   // update when committed ALL valid instructions in commit_bundle
   when ((io.com_valids.toBits != Bits(0)) && ((will_commit.toBits ^ rob_head_vals.toBits) === Bits(0))) 
   {
      rob_head := rob_head + UInt(1, ROB_ADDR_SZ)
   }
                  
   // -----------------------------------------------
   // ROB Tail Logic

   when (rob_state === s_rollback && rob_tail != rob_head) 
   {
      rob_tail := rob_tail - UInt(1)
   }
   .elsewhen (io.br_unit.brinfo.valid && io.br_unit.brinfo.mispredict)
   {
      rob_tail := GetRowIdx(io.br_unit.brinfo.rob_idx) + UInt(1)
   }
   .elsewhen (io.dis_mask.toBits != Bits(0))
   {
      rob_tail := rob_tail + UInt(1)
   }

   // -----------------------------------------------
   // Full Logic
   
   // TODO can we let the ROB fill up completely?
   // can we track "maybe_full"? 
   // maybe full is reset on branch mispredict
   val full = (rob_tail + UInt(1) === rob_head) 

   io.empty := rob_head === rob_tail

   io.curr_rob_tail := rob_tail


   io.ready := (rob_state === s_normal) && !full
                                     
   //-----------------------------------------------
   //-----------------------------------------------
   //-----------------------------------------------
   
   // ROB FSM
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
               when (io.dis_mask(w) && io.dis_uops(w).is_unique)
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
            rob_state := s_rollback
         }
         .elsewhen (rob_tail === rob_head)
         {
            rob_state := s_normal
         }
      }
   }
       
   
   // -----------------------------------------------
   // Outputs

   for (w <- 0 until width)
   {
      // tell LSU it is ready to its stores and loads
      io.com_st_mask(w) := io.com_valids(w) && rob_head_is_store(w)
      io.com_ld_mask(w) := io.com_valids(w) && rob_head_is_load(w) 
   }
          
   //--------------------------------------------------
   // Counters
   
   val rob_full_count = Reg(init = UInt(0, XPRLEN))
   when (full) { rob_full_count := rob_full_count + UInt(1) }
   debug(rob_full_count)
   // SYNTH make wide-counter
   //val rob_full_count = WideCounter(width=XPRLEN, inc=full)
   //debug(rob_full_count.value)
    
   //--------------------------------------------------
   // Handle passing out signals to printf in dpath
   
   io.debug.state    := rob_state
   io.debug.rob_head := rob_head
   
 
   // this object holds the high-order bits of the PC of each ROB row
   class RobPCs(width: Int)
   {
      val pc_shift = if (width == 1) 2 else (log2Up(width) + 2)
      val pc_hob_width = XPRLEN - pc_shift

      // bank this so we only need 1 read port to handle branches, which read
      // row X and row X+1
      val bank0 = Mem(UInt(width=pc_hob_width), ceil(num_rob_rows/2).toInt)
      val bank1 = Mem(UInt(width=pc_hob_width), ceil(num_rob_rows/2).toInt) 

      // takes rob_row_idx, returns PC (with low-order bits zeroed out)
      def  read (row_idx: UInt) = 
      {
         val rdata = Bits(width=XPRLEN)
         rdata := bank0(row_idx >> UInt(1)) << UInt(pc_shift)
         when (row_idx(0))
         {
            rdata := bank1(row_idx >> UInt(1)) << UInt(pc_shift)
         }
         // damn chisel demands a "default"
         //.otherwise
         //{
         //   rdata := bank1(row_idx >> UInt(1)) << UInt(pc_shift)
         //}
         rdata
      }

      // returns the row_idx and row_idx+1 PC (lob zeroed out)
      def read2 (row_idx: UInt) =
      {
         val addr0 = Mux(row_idx(0), row_idx + UInt(2), row_idx)
         
         val data0 = bank0(addr0 >> UInt(1)) << UInt(pc_shift)
         val data1 = bank1(row_idx >> UInt(1)) << UInt(pc_shift)
         
         val ret0 = UInt(width = XPRLEN)
         val ret1 = UInt(width = XPRLEN)
         ret0 := Mux(row_idx(0), data1, data0)
         ret1 := Mux(row_idx(0), data0, data1)

         (ret0, ret1)
      }

      // takes rob_row_idx, write in PC (with low-order bits zeroed out)
      def write (waddr: UInt, data: UInt) = 
      {
         when (waddr(0))
         {
            bank1(waddr >> UInt(1)) := data >> UInt(pc_shift)
         }
         .otherwise
         {
            bank0(waddr >> UInt(1)) := data >> UInt(pc_shift)
         }
      }
   }   
}
