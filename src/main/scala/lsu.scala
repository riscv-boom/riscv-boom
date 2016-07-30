//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Out-of-Order Load/Store Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2012 Jun 3
//
// Load/Store Unit is made up of the Load-Address Queue, the Store-Address
// Queue, and the Store-Data queue (LAQ, SAQ, and SDQ).
//
// Stores are sent to memory at (well, after) commit, loads are executed
// optimstically ASAP.  If a misspeculation was discovered, the pipeline is
// cleared. Loads put to sleep are retried.  If a LoadAddr and StoreAddr match,
// the Load can receive its data by forwarding data out of the Store-Data
// Queue.

// Currently, loads are sent to memory immediately, and in parallel do an
// associative search of the SAQ, on entering the LSU. If a hit on the SAQ
// search, the memory request is killed on the next cycle, and if the SDQ entry
// is valid, the store data is forwarded to the load (delayed to match the
// load-use delay to delay with the write-port structural hazard). If the store
// data is not present, or it's only a partial match (SB->LH), the load is put
// to sleep in the LAQ.

// Memory ordering violations are detected by stores at their addr-gen time by
// associatively searching the LAQ for newer loads that have been issued to
// memory.

// The store queue contains both speculated and committed stores.

// Only one port to memory... loads and stores have to fight for it, West Side
// Story style.

// TODO:
//    - Add predicting structure for ordering failures
//    - currently won't STD forward if DMEM is busy
//    - ability to turn off things if VM is disabled
//    - reconsider port count of the wakeup, retry stuff


package boom
{

import Chisel._
import cde.Parameters

import rocket.Str
import uncore.constants.MemoryOpConstants._
import junctions.PgIdxBits

class LoadStoreUnitIO(pl_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   // Decode Stage
   // Track which stores are "alive" in the pipeline
   // allows us to know which stores get killed by branch mispeculation
   val dec_st_vals        = Vec(pl_width,  Bool(INPUT))
   val dec_ld_vals        = Vec(pl_width,  Bool(INPUT))
   val dec_uops           = Vec(pl_width, new MicroOp()).asInput

   val new_ldq_idx        = UInt(OUTPUT, MEM_ADDR_SZ)
   val new_stq_idx        = UInt(OUTPUT, MEM_ADDR_SZ)

   // Execute Stage
   val exe_resp           = (new ValidIO(new FuncUnitResp(xLen))).flip

   // Commit Stage
   val commit_store_mask  = Vec(pl_width, Bool(INPUT))
   val commit_load_mask   = Vec(pl_width, Bool(INPUT))
   val commit_load_at_rob_head = Bool(INPUT)

   // Send out Memory Request
   val memreq_val         = Bool(OUTPUT)
   val memreq_addr        = UInt(OUTPUT, corePAddrBits)
   val memreq_wdata       = UInt(OUTPUT, xLen)
   val memreq_uop         = new MicroOp().asOutput

   val memreq_kill        = Bool(OUTPUT) // kill request sent out last cycle

   // Forward Store Data to Register File
   // TODO turn into forward bundle
   val forward_val        = Bool(OUTPUT)
   val forward_data       = UInt(OUTPUT, xLen)
   val forward_uop        = new MicroOp().asOutput // the load microop (for its pdst)

   // Receive Memory Response
   val memresp            = new ValidIO(new MicroOp()).flip

   // Handle Branch Misspeculations
   val brinfo             = new BrResolutionInfo().asInput

   // Stall Decode as appropriate
   val laq_full           = Bool(OUTPUT)
   val stq_full           = Bool(OUTPUT)

   val exception          = Bool(INPUT)
   val lsu_clr_bsy_valid  = Bool(OUTPUT) // HACK: let the stores clear out the busy bit in the ROB
   val lsu_clr_bsy_rob_idx= UInt(OUTPUT, width=ROB_ADDR_SZ)
   val lsu_fencei_rdy     = Bool(OUTPUT)

   val xcpt = new ValidIO(new Exception)

   // cache nacks
   val nack               = new NackInfo().asInput

// causing stuff to dissapear
//   val dmem = new DCMemPortIO().flip()
   val dmem_is_ordered = Bool(INPUT)
   val dmem_req_ready = Bool(INPUT)    // arbiter can back-pressure us (or MSHRs can fill up).
                                       // although this is also turned into a
                                       // nack two cycles later in the cache
                                       // wrapper, we can prevent spurious
                                       // retries as well as some load ordering
                                       // failures.

   val ptw = new rocket.TLBPTWIO

   val counters = new Bundle
   {
      val ld_valid      = Bool(OUTPUT) // a load address micro-op has entered the LSU
      val ld_forwarded  = Bool(OUTPUT)
      val ld_sleep      = Bool(OUTPUT)
      val ld_killed     = Bool(OUTPUT)
      val ld_order_fail = Bool(OUTPUT)
   }

   val debug_tsc = UInt(INPUT, xLen)     // time stamp counter
}


class LoadStoreUnit(pl_width: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new LoadStoreUnitIO(pl_width)

   val num_ld_entries = NUM_LSU_ENTRIES
   val num_st_entries = NUM_LSU_ENTRIES


   // Load-Address Queue
   val laq_addr_val       = Reg(Vec(num_ld_entries, Bool()))
   val laq_addr           = Mem(num_ld_entries, UInt(width=coreMaxAddrBits))

   val laq_allocated      = Reg(Vec(num_ld_entries, Bool())) // entry has been allocated
   val laq_is_virtual     = Reg(Vec(num_ld_entries, Bool())) // address in LAQ is a virtual address. There was a tlb_miss and a retry is required.
   val laq_is_uncacheable = Reg(Vec(num_ld_entries, Bool())) // address in LAQ is an uncacheable address. Can only execute once it's the head of the ROB
   val laq_executed       = Reg(Vec(num_ld_entries, Bool())) // load has been issued to memory (immediately set this bit)
   val laq_succeeded      = Reg(Vec(num_ld_entries, Bool())) // load has returned from memory, but may still have an ordering failure
   val laq_failure        = Reg(init = Vec.fill(num_ld_entries) { Bool(false) })  // ordering fail, must retry (at commit time, which requires a rollback)
   val laq_uop            = Reg(Vec(num_ld_entries, new MicroOp()))
   //laq_uop.stq_idx between oldest and youngest (dep_mask can't establish age :( ), "aka store coloring" if you're Intel
//   val laq_request   = Vec.fill(num_ld_entries) { Reg(resetVal = Bool(false)) } // TODO sleeper load requesting issue to memory (perhaps stores broadcast, sees its store-set finished up)


   // track window of stores we depend on
   val laq_st_dep_mask        = Reg(Vec(num_ld_entries, UInt(width = num_st_entries))) // list of stores we might depend (cleared when a store commits)
   val laq_forwarded_std_val  = Reg(Vec(num_ld_entries, Bool()))
   val laq_forwarded_stq_idx  = Reg(Vec(num_ld_entries, UInt(width = MEM_ADDR_SZ)))    // which store did get store-load forwarded data from? compare later to see I got things correct
   val debug_laq_put_to_sleep = Reg(Vec(num_ld_entries, Bool()))                       // did a load get put to sleep at least once?
//   val laq_st_wait_mask = Vec.fill(num_ld_entries) { Reg() { Bits(width = num_st_entries) } }// TODO list of stores we might depend on whose addresses are not yet computed
//   val laq_block_val    = Vec.fill(num_ld_entries) { Reg() { Bool() } }                     // TODO something is blocking us from executing
//   val laq_block_id     = Vec.fill(num_ld_entries) { Reg() { UInt(width = MEM_ADDR_SZ) } }  // TODO something is blocking us from executing, listen for this ID to wakeup

   // Store-Address Queue
   val saq_val       = Reg(Vec(num_st_entries, Bool()))
   val saq_is_virtual= Reg(Vec(num_st_entries, Bool())) // address in SAQ is a virtual address. There was a tlb_miss and a retry is required.
   val saq_addr      = Mem(num_st_entries, UInt(width=coreMaxAddrBits))

   // Store-Data Queue
   val sdq_val       = Reg(Vec(num_st_entries, Bool()))
   val sdq_data      = Reg(Vec(num_st_entries, UInt(width = xLen)))

   // Shared Store Queue Information
   val stq_uop       = Reg(Vec(num_st_entries, new MicroOp()))
   // TODO not convinced I actually need stq_entry_val; I think other ctrl signals gate this off
   val stq_entry_val = Reg(Vec(num_st_entries, Bool())) // this may be valid, but not TRUE (on exceptions, this doesn't get cleared but STQ_TAIL gets moved)
   val stq_executed  = Reg(Vec(num_st_entries, Bool())) // sent to mem
   val stq_succeeded = Reg(Vec(num_st_entries, Bool())) // returned TODO is this needed, or can we just advance the stq_head?
   val stq_committed = Reg(Vec(num_st_entries, Bool())) // the ROB has committed us, so we can now send our store to memory


   val laq_head = Reg(UInt())
   val laq_tail = Reg(UInt())
   val stq_head = Reg(UInt()) // point to next store to clear from STQ (i.e., send to memory)
   val stq_tail = Reg(UInt()) // point to next available, open entry
   val stq_commit_head = Reg(UInt()) // point to next store to commit
   val stq_execute_head = Reg(UInt()) // point to next store to execute

   val clear_store = Wire(Bool())
   clear_store := Bool(false)

   val live_store_mask = Reg(init = UInt(0, num_st_entries))
   var next_live_store_mask = Mux(clear_store, live_store_mask & ~(UInt(1) << stq_head),
                                                live_store_mask)

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // Enqueue new entries
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // put this earlier than Enqueue, since this is lower priority to laq_st_dep_mask
   for (i <- 0 until num_ld_entries)
   {
      when (clear_store)
      {
         laq_st_dep_mask(i) := laq_st_dep_mask(i) & ~(UInt(1) << stq_head)
      }
   }

   // Decode stage ----------------------------

   var ld_enq_idx = laq_tail
   var st_enq_idx = stq_tail

   for (w <- 0 until pl_width)
   {
      when (io.dec_ld_vals(w))
      {
         // TODO is it better to read out ld_idx?
         // val ld_enq_idx = io.dec_uops(w).ldq_idx
         laq_uop(ld_enq_idx)          := io.dec_uops(w)
         laq_st_dep_mask(ld_enq_idx)  := next_live_store_mask

         laq_allocated(ld_enq_idx)    := Bool(true)
         laq_addr_val (ld_enq_idx)    := Bool(false)
         laq_executed (ld_enq_idx)    := Bool(false)
         laq_succeeded(ld_enq_idx)    := Bool(false)
         laq_failure  (ld_enq_idx)    := Bool(false)
         laq_forwarded_std_val(ld_enq_idx)  := Bool(false)
         debug_laq_put_to_sleep(ld_enq_idx) := Bool(false)
      }
      ld_enq_idx = Mux(io.dec_ld_vals(w), WrapInc(ld_enq_idx, num_ld_entries),
                                          ld_enq_idx)

      when (io.dec_st_vals(w))
      {
         stq_uop(st_enq_idx)       := io.dec_uops(w)

         stq_entry_val(st_enq_idx) := Bool(true)
         saq_val      (st_enq_idx) := Bool(false)
         sdq_val      (st_enq_idx) := Bool(false)
         stq_executed (st_enq_idx) := Bool(false)
         stq_succeeded(st_enq_idx) := Bool(false)
         stq_committed(st_enq_idx) := Bool(false)
      }
      next_live_store_mask = Mux(io.dec_st_vals(w), next_live_store_mask | (UInt(1) << st_enq_idx),
                                                    next_live_store_mask)

      st_enq_idx = Mux(io.dec_st_vals(w), WrapInc(st_enq_idx, num_st_entries),
                                          st_enq_idx)

   }

   laq_tail := ld_enq_idx
   stq_tail := st_enq_idx


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // Execute stage (access TLB, send requests to Memory)
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   //--------------------------------------------
   // Controller Logic (arbitrate TLB, D$ access)
   //
   // There are a couple some-what coupled datapaths here and 7 potential users.
   // AddrGen -> TLB -> LAQ/D$ (for loads) or SAQ/ROB (for stores)
   // LAQ     -> optionally TLB -> D$
   // SAQ     -> TLB -> ROB
   // And uopSTAs and uopSTDs fight over the ROB unbusy port.

   val will_fire_load_incoming = Wire(Bool()) // uses TLB, D$, SAQ-search
   val will_fire_sta_incoming  = Wire(Bool()) // uses TLB,     LAQ-search, ROB
   val will_fire_std_incoming  = Wire(Bool()) // uses                      ROB
   val will_fire_sta_retry     = Wire(Bool()) // uses TLB,                 ROB
   val will_fire_load_retry    = Wire(Bool()) // uses TLB, D$, SAQ-search
   val will_fire_store_commit  = Wire(Bool()) // uses      D$
   val will_fire_load_wakeup   = Wire(Bool()) // uses      D$, SAQ-search

   val can_fire_sta_retry      = Wire(Bool())
   val can_fire_load_retry     = Wire(Bool())
   val can_fire_store_commit   = Wire(Bool())
   val can_fire_load_wakeup    = Wire(Bool())

   // defaults
   will_fire_load_incoming := Bool(false)
   will_fire_sta_incoming  := Bool(false)
   will_fire_std_incoming  := Bool(false)
   will_fire_sta_retry     := Bool(false)
   will_fire_load_retry    := Bool(false)
   will_fire_store_commit  := Bool(false)
   will_fire_load_wakeup   := Bool(false)

   val dc_avail = Wire(Bool()); dc_avail := Bool(true)
   val tlb_avail= Wire(Bool()); tlb_avail:= Bool(true)
   val rob_avail= Wire(Bool()); rob_avail:= Bool(true)

   // give first priority to incoming uops
   when (io.exe_resp.valid)
   {
      when (io.exe_resp.bits.uop.ctrl.is_load)
      {
         will_fire_load_incoming := Bool(true)
         dc_avail  := Bool(false)
         tlb_avail := Bool(false)
      }
      when (io.exe_resp.bits.uop.ctrl.is_sta)
      {
         will_fire_sta_incoming := Bool(true)
         tlb_avail := Bool(false)
         rob_avail := Bool(false)
      }
      when (io.exe_resp.bits.uop.ctrl.is_std)
      {
         will_fire_std_incoming := Bool(true)
         rob_avail := Bool(false)
      }
   }

   when (tlb_avail)
   {
      when (can_fire_sta_retry && rob_avail)
      {
         will_fire_sta_retry := Bool(true)
      }
      .elsewhen (can_fire_load_retry)
      {
         will_fire_load_retry := Bool(true)
         dc_avail := Bool(false)
      }
   }

   when (dc_avail)
   {
      // TODO allow dyanmic priority here
      will_fire_store_commit := can_fire_store_commit
      will_fire_load_wakeup  := !can_fire_store_commit && can_fire_load_wakeup
   }

   //--------------------------------------------
   // TLB Access

   val stq_retry_idx = Wire(UInt())
   val laq_retry_idx = Wire(UInt())


   // micro-op going through the TLB generate paddr's. If this is a load, it will continue
   // to the D$ and search the SAQ. uopSTD also uses this uop.
   val exe_tlb_uop = Mux(will_fire_sta_retry,  stq_uop(stq_retry_idx),
                     Mux(will_fire_load_retry, laq_uop(laq_retry_idx),
                                               io.exe_resp.bits.uop))

   val exe_vaddr   = Mux(will_fire_sta_retry,  saq_addr(stq_retry_idx),
                     Mux(will_fire_load_retry, laq_addr(laq_retry_idx),
                                               io.exe_resp.bits.addr.toBits))

   val dtlb = Module(new rocket.TLB()(p.alterPartial({case uncore.agents.CacheName => "L1D"})))
   io.ptw <> dtlb.io.ptw
   dtlb.io.req.valid := will_fire_load_incoming ||
                        will_fire_sta_incoming ||
                        will_fire_sta_retry ||
                        will_fire_load_retry
   dtlb.io.req.bits.passthrough := Bool(false) // lets status.vm decide
   dtlb.io.req.bits.vpn := exe_vaddr >> UInt(corePgIdxBits)
   dtlb.io.req.bits.instruction := Bool(false)
   dtlb.io.req.bits.store := will_fire_sta_incoming || will_fire_sta_retry

   // exceptions
   val ma_ld = io.exe_resp.valid && io.exe_resp.bits.mxcpt.valid && exe_tlb_uop.is_load
   val pf_ld = dtlb.io.req.valid && dtlb.io.resp.xcpt_ld && exe_tlb_uop.is_load
   val pf_st = dtlb.io.req.valid && dtlb.io.resp.xcpt_st && exe_tlb_uop.is_store
   val mem_xcpt_valid = Reg(next=((dtlb.io.req.valid && (pf_ld || pf_st)) ||
                                 (io.exe_resp.valid && io.exe_resp.bits.mxcpt.valid)) &&
                                 !io.exception &&
                                 !IsKilledByBranch(io.brinfo, exe_tlb_uop),
                            init=Bool(false))
   val mem_xcpt_cause = Reg(next=(Mux(io.exe_resp.valid &&
                                      io.exe_resp.bits.mxcpt.valid, io.exe_resp.bits.mxcpt.bits,
                                  Mux(exe_tlb_uop.is_load,         UInt(rocket.Causes.fault_load),
                                                                   UInt(rocket.Causes.fault_store)))))
   io.xcpt.bits.badvaddr := Reg(next=exe_vaddr) // TODO is there another register we can use instead?

   assert (!(dtlb.io.req.valid && exe_tlb_uop.is_fence), "Fence is pretending to talk to the TLB")
   assert (!(io.exe_resp.bits.mxcpt.valid && io.exe_resp.valid &&
            !(io.exe_resp.bits.uop.ctrl.is_load || io.exe_resp.bits.uop.ctrl.is_sta))
            , "A uop that's not a load or store-address is throwing a memory exception.")

   val tlb_miss = dtlb.io.req.valid && (dtlb.io.resp.miss || !dtlb.io.req.ready)



   // output
   val exe_tlb_paddr = Cat(dtlb.io.resp.ppn, exe_vaddr(corePgIdxBits-1,0))

   // check if a load is uncacheable - must stop it from executing speculatively,
   // as it might have side-effects!
   val tlb_addr_uncacheable = !(addrMap.isCacheable(exe_tlb_paddr))

   //-------------------------------------
   // Can-fire Logic & Wakeup/Retry Select

   // *** Wakeup Load from LAQ ***
   can_fire_load_wakeup := Bool(false)

   // TODO make option to only wakeup load at the head (to compare to old behavior)
   val exe_ld_idx_wakeup =
      AgePriorityEncoder((0 until num_ld_entries).map(i => laq_addr_val(i) & ~laq_executed(i)), laq_head)


   when (laq_addr_val       (exe_ld_idx_wakeup) &&
         !laq_is_virtual    (exe_ld_idx_wakeup) &&
         laq_allocated      (exe_ld_idx_wakeup) &&
         !laq_executed      (exe_ld_idx_wakeup) &&
         !laq_failure       (exe_ld_idx_wakeup) &&
         (!laq_is_uncacheable(exe_ld_idx_wakeup) || (io.commit_load_at_rob_head && laq_head === exe_ld_idx_wakeup))
         )
   {
      can_fire_load_wakeup := Bool(true)
   }

   // *** Retry Load TLB-lookup from LAQ ***

   can_fire_load_retry := Bool(false)
   laq_retry_idx := exe_ld_idx_wakeup

   when (laq_allocated (laq_retry_idx) &&
         laq_addr_val  (laq_retry_idx) &&
         laq_is_virtual(laq_retry_idx) &&
         !laq_executed (laq_retry_idx) && // perf lose, but simplifies control
         !laq_failure  (laq_retry_idx) &&
         Reg(next=dtlb.io.req.ready))
   {
      can_fire_load_retry := Bool(true)
   }


   // *** STORES ***

   can_fire_store_commit := Bool(false)

   when (stq_entry_val(stq_execute_head) &&
         (stq_committed(stq_execute_head) ||
            (stq_uop(stq_execute_head).is_amo &&
            saq_val(stq_execute_head) &&
            !saq_is_virtual(stq_execute_head) &&
            sdq_val(stq_execute_head)
            )) &&
         !stq_executed(stq_execute_head) &&
         !(stq_uop(stq_execute_head).is_fence))
   {
      can_fire_store_commit := Bool(true)
   }

   can_fire_sta_retry := Bool(false)
   stq_retry_idx := stq_commit_head

   when (stq_entry_val (stq_retry_idx) &&
         saq_val       (stq_retry_idx) &&
         saq_is_virtual(stq_retry_idx) &&
         Reg(next=dtlb.io.req.ready))
   {
      can_fire_sta_retry := Bool(true)
   }


   assert (!(can_fire_store_commit && saq_is_virtual(stq_execute_head)),
            "a committed store is trying to fire to memory that has a bad paddr.")

   assert (stq_entry_val(stq_execute_head) ||
            stq_head === stq_execute_head || stq_tail === stq_execute_head,
            "stq_execute_head got off track.")

   //-------------------------
   // Issue Someting to Memory
   //
   // Three locations a memory op can come from.
   // 1. Incoming load   ("Fast")
   // 2. Sleeper Load    ("from the LAQ")
   // 3. Store at Commit ("from SAQ")

   val exe_ld_addr = Mux(will_fire_load_incoming || will_fire_load_retry, exe_tlb_paddr, laq_addr(exe_ld_idx_wakeup))
   val exe_ld_uop  = Mux(will_fire_load_incoming || will_fire_load_retry, exe_tlb_uop,   laq_uop(exe_ld_idx_wakeup))

   // defaults
   io.memreq_val     := Bool(false)
   io.memreq_addr    := exe_ld_addr
   io.memreq_wdata   := sdq_data(stq_execute_head)
   io.memreq_uop     := exe_ld_uop

   val mem_fired_st = Reg(init = Bool(false))
   mem_fired_st := Bool(false)
   when (will_fire_store_commit)
   {
      io.memreq_addr  := saq_addr(stq_execute_head)
      io.memreq_uop   := stq_uop (stq_execute_head)

      // prevent this store going out if an earlier store just got nacked!
      when (!(io.nack.valid && !io.nack.isload))
      {
         io.memreq_val   := Bool(true)
         stq_executed(stq_execute_head) := Bool(true)
         stq_execute_head := WrapInc(stq_execute_head, num_st_entries)
         mem_fired_st := Bool(true)
      }
   }
   .elsewhen (will_fire_load_incoming || will_fire_load_retry || will_fire_load_wakeup)
   {
      io.memreq_val   := Bool(true)
      io.memreq_addr  := exe_ld_addr
      io.memreq_uop   := exe_ld_uop

      laq_executed(exe_ld_uop.ldq_idx) := Bool(true)
      laq_failure (exe_ld_uop.ldq_idx) := (will_fire_load_incoming && (ma_ld || pf_ld)) ||
                                          (will_fire_load_retry && pf_ld)
   }

   assert (PopCount(Vec(will_fire_store_commit, will_fire_load_incoming, will_fire_load_retry, will_fire_load_wakeup))
      <= UInt(1), "Multiple requestors firing to the data cache.")




   //-------------------------------------------------------------
   // Write Addr into the LAQ/SAQ

   when (will_fire_load_incoming || will_fire_load_retry)
   {
      laq_addr_val      (exe_tlb_uop.ldq_idx)      := Bool(true)
      laq_addr          (exe_tlb_uop.ldq_idx)      := Mux(tlb_miss, exe_vaddr, exe_tlb_paddr)
      laq_uop           (exe_tlb_uop.ldq_idx).pdst := exe_tlb_uop.pdst
      laq_is_virtual    (exe_tlb_uop.ldq_idx)      := tlb_miss
      laq_is_uncacheable(exe_tlb_uop.ldq_idx)      := tlb_addr_uncacheable && !tlb_miss
   }

   when (will_fire_sta_incoming || will_fire_sta_retry)
   {
      saq_val       (exe_tlb_uop.stq_idx)      := !pf_st // prevent AMOs from executing!
      saq_addr      (exe_tlb_uop.stq_idx)      := Mux(tlb_miss, exe_vaddr, exe_tlb_paddr)
      stq_uop       (exe_tlb_uop.stq_idx).pdst := exe_tlb_uop.pdst // needed for amo's TODO this is expensive, can we get around this?
      saq_is_virtual(exe_tlb_uop.stq_idx)      := tlb_miss
   }

   when (will_fire_std_incoming)
   {
      sdq_val (io.exe_resp.bits.uop.stq_idx) := Bool(true)
      sdq_data(io.exe_resp.bits.uop.stq_idx) := io.exe_resp.bits.data.toBits
   }


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // Cache Access Cycle (Mem)
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // search SAQ/LAQ for matches

   val mem_tlb_paddr    = Reg(next=exe_tlb_paddr)
   val mem_tlb_uop      = Reg(next=exe_tlb_uop) // not valid for std_incoming!
   mem_tlb_uop.br_mask := GetNewBrMask(io.brinfo, exe_tlb_uop)
   val mem_tlb_miss     = Reg(next=tlb_miss, init=Bool(false))
   val mem_tlb_uncacheable = Reg(next=tlb_addr_uncacheable, init=Bool(false))
   val mem_ld_used_tlb  = RegNext(will_fire_load_incoming || will_fire_load_retry)

   // the load address that will search the SAQ (either a fast load or a retry load)
   val mem_ld_addr = Mux(Reg(next=will_fire_load_wakeup), Reg(next=laq_addr(exe_ld_idx_wakeup)), mem_tlb_paddr)
   val mem_ld_uop  = Reg(next=exe_ld_uop)
   mem_ld_uop.br_mask := GetNewBrMask(io.brinfo, exe_ld_uop)
   val mem_ld_killed = Wire(Bool()) // was a load killed in execute

   val mem_fired_ld = Reg(next=(will_fire_load_incoming ||
                                    will_fire_load_retry ||
                                    will_fire_load_wakeup))
   val mem_fired_sta = Reg(next=(will_fire_sta_incoming || will_fire_sta_retry), init=Bool(false))
   val mem_fired_std = Reg(next=will_fire_std_incoming, init=Bool(false))

   mem_ld_killed := Bool(false)
   when (Reg(next=
         (IsKilledByBranch(io.brinfo, exe_ld_uop) ||
         io.exception ||
         (tlb_addr_uncacheable && dtlb.io.req.valid))) ||
      io.exception)
   {
      mem_ld_killed := Bool(true) && mem_fired_ld
   }


   // tell the ROB to clear the busy bit on the incoming store

   io.lsu_clr_bsy_valid := Bool(false)
   io.lsu_clr_bsy_rob_idx := mem_tlb_uop.rob_idx
   when (mem_fired_sta && !mem_tlb_miss && mem_fired_std)
   {
      io.lsu_clr_bsy_valid := !mem_tlb_uop.is_amo
      io.lsu_clr_bsy_rob_idx := mem_tlb_uop.rob_idx
   }
   .elsewhen (mem_fired_sta && !mem_tlb_miss)
   {
      io.lsu_clr_bsy_valid := sdq_val(mem_tlb_uop.stq_idx) &&
                              !mem_tlb_uop.is_amo
      io.lsu_clr_bsy_rob_idx := mem_tlb_uop.rob_idx
   }
   .elsewhen (mem_fired_std)
   {
      val mem_std_uop = Reg(next=io.exe_resp.bits.uop)
      io.lsu_clr_bsy_valid := saq_val(mem_std_uop.stq_idx) &&
                              !saq_is_virtual(mem_std_uop.stq_idx) &&
                              !mem_std_uop.is_amo
   io.lsu_clr_bsy_rob_idx := mem_std_uop.rob_idx
   }

   //-------------------------------------------------------------
   // Load Issue Datapath (ALL loads need to use this path,
   //    to handle forwarding from the STORE QUEUE, etc.)
   // search entire STORE QUEUE for match on load
   //-------------------------------------------------------------
   // does the incoming load match any store addresses?
   // NOTE: these are fully translated physical addresses, as
   // forwarding requires a full address check.

   val read_mask = GenByteMask(mem_ld_addr, mem_ld_uop.mem_typ)
   val st_dep_mask = laq_st_dep_mask(Reg(next=exe_ld_uop.ldq_idx))

   // do the double-word addr match? (doesn't necessarily mean a conflict or forward)
   val dword_addr_matches = Wire(Vec(num_st_entries, Bool()))
   // if there is some overlap on the bytes, you may need to put to sleep the load
   // (either data not ready, or not a perfect match between addr and type)
   val addr_conflicts     = Wire(Vec(num_st_entries, Bool()))
   // a full address match
   val forwarding_matches  = Wire(Vec(num_st_entries, Bool()))

   val force_ld_to_sleep = Wire(Bool())
   force_ld_to_sleep := Bool(false)

   // do the load and store memory types match (aka, B == BU, H == HU, W == WU)
   def MemTypesMatch(typ_1: UInt, typ_2: UInt) = typ_1(1,0) === typ_2(1,0)


   // TODO totally refactor how conflict/forwarding logic is generated
   for (i <- 0 until num_st_entries)
   {
      val s_addr = saq_addr(i)

      dword_addr_matches(i) := Bool(false)

      when (stq_entry_val(i) &&
            st_dep_mask(i) &&
            saq_val(i) &&
            !saq_is_virtual(i) &&
            (s_addr(corePAddrBits-1,3) === mem_ld_addr(corePAddrBits-1,3)))
      {
         dword_addr_matches(i) := Bool(true)
      }

      // check the lower-order bits for overlap/conflicts and matches
      addr_conflicts(i) := Bool(false)
      val write_mask = GenByteMask(s_addr, stq_uop(i).mem_typ)

      // if overlap on bytes and dword matches, the address conflicts!
      when (((read_mask & write_mask) =/= UInt(0)) && dword_addr_matches(i))
      {
         addr_conflicts(i) := Bool(true)
      }
      // fences/flushes are treated as stores that touch all addresses
      .elsewhen (stq_entry_val(i) &&
                  st_dep_mask(i) &&
                  stq_uop(i).is_fence)
      {
         addr_conflicts(i) := Bool(true)
      }

      // exact match on masks? we can forward the data, if data is also present!
      // TODO PERF we can be fancier perhaps, like (r_mask & w_mask === r_mask)
      forwarding_matches(i) := Bool(false)
      when ((read_mask === write_mask) &&
            !(stq_uop(i).is_fence) &&
            dword_addr_matches(i))
      {
         forwarding_matches(i) := Bool(true)
      }

      // did a load see a conflicting store (sb->lw) or a fence/AMO? if so, put the load to sleep
      // TODO this shuts down all loads so long as there is a store live in the dependent mask
      when ((stq_entry_val(i) &&
               st_dep_mask(i) &&
               (stq_uop(i).is_fence || stq_uop(i).is_amo)) ||
            (dword_addr_matches(i) &&
//               (mem_ld_uop.mem_typ =/= stq_uop(i).mem_typ) &&
               (!MemTypesMatch(mem_ld_uop.mem_typ, stq_uop(i).mem_typ)) &&
               ((read_mask & write_mask) =/= UInt(0))))
      {
         force_ld_to_sleep := Bool(true)
      }
   }


   val forwarding_age_logic = Module(new ForwardingAgeLogic(num_st_entries))
   forwarding_age_logic.io.addr_matches    := forwarding_matches.toBits()
   forwarding_age_logic.io.youngest_st_idx := laq_uop(Reg(next=exe_ld_uop.ldq_idx)).stq_idx

   when (mem_fired_ld && forwarding_age_logic.io.forwarding_val && !tlb_miss)
   {
      laq_forwarded_std_val(mem_ld_uop.ldq_idx) := Bool(true)
      laq_forwarded_stq_idx(mem_ld_uop.ldq_idx) := forwarding_age_logic.io.forwarding_idx
   }

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // Writeback Cycle (St->Ld Forwarding Path)
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   val wb_forward_std_val = Reg(init = Bool(false))
   val wb_forward_std_idx = Reg(UInt())
   val wb_uop             = Reg(next=mem_ld_uop)
   wb_uop.br_mask        := GetNewBrMask(io.brinfo, mem_ld_uop)


   // kill load request to mem if address matches (we will either sleep load, or forward data) or TLB miss
   io.memreq_kill     := (mem_ld_used_tlb && (mem_tlb_miss || Reg(next=pf_ld || ma_ld))) ||
                         (mem_fired_ld && addr_conflicts.toBits =/= UInt(0)) ||
                         mem_ld_killed ||
                         (mem_fired_st && io.nack.valid && !io.nack.isload)
   wb_forward_std_idx := forwarding_age_logic.io.forwarding_idx

   // kill forwarding if branch mispredict
   when (IsKilledByBranch(io.brinfo, mem_ld_uop))
   {
      wb_forward_std_val := Bool(false)
   }
   .otherwise
   {
      wb_forward_std_val := mem_fired_ld && forwarding_age_logic.io.forwarding_val &&
                           !force_ld_to_sleep && !(mem_tlb_miss && mem_ld_used_tlb) && !mem_ld_killed && !io.exception
   }

   // Notes:
   //    - Time the forwarding of the data to coincide with what would be a HIT
   //       from the cache (to only use one port).

   io.forward_val := Bool(false)
   when (IsKilledByBranch(io.brinfo, wb_uop))
   {
      io.forward_val := Bool(false)
   }
   .otherwise
   {
      io.forward_val := wb_forward_std_val &&
                        sdq_val(wb_forward_std_idx) &&
                        !(io.nack.valid && io.nack.cache_nack)
   }
   io.forward_data := LoadDataGenerator(sdq_data(wb_forward_std_idx).toBits, wb_uop.mem_typ)
   io.forward_uop  := wb_uop


   //------------------------
   // Handle Memory Responses
   //------------------------

   when (io.memresp.valid)
   {
      when (io.memresp.bits.is_load)
      {
         laq_succeeded(io.memresp.bits.ldq_idx) := Bool(true)
      }
      .otherwise
      {
         stq_succeeded(io.memresp.bits.stq_idx) := Bool(true)

         if (O3PIPEVIEW_PRINTF)
         {
            printf("%d; store-comp: %d\n", io.memresp.bits.debug_events.fetch_seq, io.debug_tsc)
         }
      }
   }

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // Search LAQ for misspeculated loads (when a store is executed)
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // When to check for memory ordering failure?
   // 1) Could check at Commit: any loads here are by definition younger
   //    (except not really true once going superscalar). Other problem is the
   //    search bandwidth must match the commit bandwidth, which is way over
   //    provisioned and very expensive..
   // 2) Check at Execute: need to ignore loads with which we don't match their
   //    dependence matrix, but only need to search for as many stores as we let
   //    perform address generation. Other big factor is we can figure out loads
   //    failed before commit time, solving the issue of trying to commit a bundle
   //    of stores and loads (where the loads could be marked failures by the
   //    stores).

   // TODO check Chisel performance on pushing invariants inside of for loops

   // At Store Execute (address generation)...
   //    Check the incoming store address against younger loads that have
   //    executed, looking for memory ordering failures. This check occurs the
   //    cycle after address generation and TLB lookup.
   val st_addr     = mem_tlb_paddr
   val st_mask     = GenByteMask(st_addr, mem_tlb_uop.mem_typ)
   val st_is_fence = mem_tlb_uop.is_fence
   val stq_idx     = mem_tlb_uop.stq_idx
   val failed_loads = Wire(Vec(num_ld_entries, Bool()))

   for (i <- 0 until num_ld_entries)
   {
      val l_addr = laq_addr(i)
      val l_mask = GenByteMask(l_addr, laq_uop(i).mem_typ)
      failed_loads(i) := Bool(false)

      when (Reg(next=(will_fire_sta_incoming || will_fire_sta_retry),init=Bool(false)))
      {
         // does the load depend on this store?
         // TODO CODE REVIEW what's the best way to perform this bit extract?
         when ((laq_st_dep_mask(i) & (UInt(1) << stq_idx)) =/= UInt(0))
         {
            when (st_is_fence &&
                  laq_allocated(i) &&
                  laq_addr_val(i) &&
                  !laq_is_virtual(i) &&
                  laq_executed(i))
            {
               // fences, flushes are like stores that hit all addresses
               laq_executed(i)   := Bool(false)
               laq_failure(i)    := Bool(true)
               laq_succeeded(i)  := Bool(false)
               failed_loads(i)   := Bool(true)
            }
            // NOTE: this address check doesn't necessarily have to be across all address bits
            .elsewhen ((st_addr(corePAddrBits-1,3) === l_addr(corePAddrBits-1,3)) &&
                  laq_allocated(i) &&
                  laq_addr_val(i) &&
                  !laq_is_virtual(i) &&
                  laq_executed(i)
                  )
            {
               val yid = laq_uop(i).stq_idx
               val fid = laq_forwarded_stq_idx(i)
               // double-words match, now check for conflict of byte masks,
               // then check if it was forwarded from us,
               // and if not, then fail OR
               // if it was forwarded but not us, was the forwarded store older than me
               // head < forwarded < youngest?
               when (((st_mask & l_mask) =/= UInt(0)) &&
                    (!laq_forwarded_std_val(i) ||
                      ((fid =/= stq_idx) && (Cat(stq_idx < yid, stq_idx) > Cat(fid < yid, fid)))))
               {
                  laq_executed(i)   := Bool(false)
                  laq_failure(i)    := Bool(true)
                  laq_succeeded(i)  := Bool(false)
                  failed_loads(i)   := Bool(true)
               }
            }
         }
      }
   }

   // detect which loads get marked as failures, but broadcast to the ROB the oldest failing load
   // TODO encapsulate this in an age-based  priority-encoder
//   val l_idx = AgePriorityEncoder((Vec(Vec.tabulate(num_ld_entries)(i => failed_loads(i) && UInt(i) >= laq_head) ++ failed_loads)).toBits)
   val temp_bits = (Vec(Vec.tabulate(num_ld_entries)(i => 
      failed_loads(i) && UInt(i) >= laq_head) ++ failed_loads)).toBits
   val l_idx = PriorityEncoder(temp_bits)

   // TODO always pad out the input to PECircular() to pow2
   // convert it to vec[bool], then in.padTo(1 << log2Up(in.size), Bool(false))

   // one exception port, but multiple causes!
   // - 1) the incoming store-address finds a faulting load (it is by definition younger)
   // - 2) the incoming load or store address is excepting. It must be older and thus takes precedent.
   io.xcpt.valid := failed_loads.reduce(_|_) || mem_xcpt_valid
   io.xcpt.bits.uop := Mux(mem_xcpt_valid, mem_tlb_uop, laq_uop(Mux(l_idx >= UInt(num_ld_entries), l_idx - UInt(num_ld_entries), l_idx)))
   io.xcpt.bits.cause := Mux(mem_xcpt_valid, mem_xcpt_cause, MINI_EXCEPTION_MEM_ORDERING)


   //-------------------------------------------------------------
   // Kill speculated entries on branch mispredict
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   val st_brkilled_mask = Wire(Vec(num_st_entries, Bool()))
   for (i <- 0 until num_st_entries)
   {
      st_brkilled_mask(i) := Bool(false)

      when (stq_entry_val(i))
      {
         stq_uop(i).br_mask := GetNewBrMask(io.brinfo, stq_uop(i))

         when (IsKilledByBranch(io.brinfo, stq_uop(i)))
         {
            stq_entry_val(i)   := Bool(false)
            saq_val(i)         := Bool(false)
            sdq_val(i)         := Bool(false)
            stq_uop(i).br_mask := UInt(0)
            st_brkilled_mask(i):= Bool(true)
         }
      }

      assert (!(IsKilledByBranch(io.brinfo, stq_uop(i)) && stq_entry_val(i) && stq_committed(i)),
         "Branch is trying to clear a committed store.")
   }

   //-------------------------------------------------------------
   // Kill speculated entries on branch mispredict
   for (i <- 0 until num_ld_entries)
   {
      when(laq_allocated(i))
      {
         laq_uop(i).br_mask := GetNewBrMask(io.brinfo, laq_uop(i))
         when (IsKilledByBranch(io.brinfo, laq_uop(i)))
         {
            laq_allocated(i)   := Bool(false)
            laq_addr_val(i)    := Bool(false)
         }
      }
   }

   //-------------------------------------------------------------
   when (io.brinfo.valid && io.brinfo.mispredict && !io.exception)
   {
      stq_tail := io.brinfo.stq_idx
      laq_tail := io.brinfo.ldq_idx
   }


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // dequeue old entries on commit
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   var temp_stq_commit_head = stq_commit_head
   for (w <- 0 until pl_width)
   {
      when (io.commit_store_mask(w))
      {
         stq_committed(temp_stq_commit_head) := Bool(true)
      }

      temp_stq_commit_head = Mux(io.commit_store_mask(w), WrapInc(temp_stq_commit_head, num_st_entries), temp_stq_commit_head)
   }

   stq_commit_head := temp_stq_commit_head

   // store has been committed AND successfully sent data to memory
   when (stq_entry_val(stq_head) && stq_committed(stq_head))
   {
      clear_store := Mux(stq_uop(stq_head).is_fence, io.dmem_is_ordered,
                                                     stq_succeeded(stq_head))
   }

   when (clear_store)
   {
      stq_entry_val(stq_head)   := Bool(false)
      saq_val(stq_head)         := Bool(false)
      sdq_val(stq_head)         := Bool(false)
      stq_executed(stq_head)    := Bool(false)
      stq_succeeded(stq_head)   := Bool(false)
      stq_committed(stq_head)   := Bool(false)

      stq_head := WrapInc(stq_head, num_st_entries)
      when (stq_uop(stq_head).is_fence)
      {
         stq_execute_head := WrapInc(stq_execute_head, num_st_entries)
      }
   }


   var temp_laq_head = laq_head
   for (w <- 0 until pl_width)
   {
      val idx = temp_laq_head
      when (io.commit_load_mask(w))
      {
         assert (laq_allocated(idx), "[lsu] trying to commit an un-allocated load entry.")
         assert (laq_executed(idx), "[lsu] trying to commit an un-executed load entry.")
         assert (laq_succeeded(idx), "[lsu] trying to commit an un-succeeded load entry.")

         laq_allocated(idx)         := Bool(false)
         laq_addr_val (idx)         := Bool(false)
         laq_executed (idx)         := Bool(false)
         laq_succeeded(idx)         := Bool(false)
         laq_failure  (idx)         := Bool(false)
         laq_forwarded_std_val(idx) := Bool(false)
      }

      temp_laq_head = Mux(io.commit_load_mask(w), WrapInc(temp_laq_head, num_ld_entries), temp_laq_head)
   }
   laq_head := temp_laq_head



   //-------------------------------------------------------------
   // Handle Nacks
   // the data cache may nack our requests, requiring us to resend our request,
   // the forwarding logic (from the STD) may be "nacking" us, in which case,
   // we ignore the nack (the nack is for the D$, not the LSU).

   val clr_ld = Wire(Bool())
   clr_ld := Bool(false)

   // did the load execute, but was then killed/nacked (will overcount)?
   val ld_was_killed       = Wire(Bool())
   // did the load execute, but was then killed/nacked (only high once per load)?
   val ld_was_put_to_sleep = Wire(Bool())
   ld_was_killed           := Bool(false)
   ld_was_put_to_sleep     := Bool(false)

   def IsOlder(i0: UInt, i1: UInt, tail: UInt) = (Cat(i0 <= tail, i0) < Cat(i1 <= tail, i1))
   when (io.nack.valid)
   {
      // the cache nacked our store
      when (!io.nack.isload)
      {
         stq_executed(io.nack.lsu_idx) := Bool(false)
         when (IsOlder(io.nack.lsu_idx, stq_execute_head, stq_tail))
         {
            stq_execute_head := io.nack.lsu_idx
         }
      }
      // the nackee is a load
      .otherwise
      {
         // we're trying to forward a load from the STD
         when (wb_forward_std_val)
         {
            // handle case where sdq_val is no longer true (store was
            // committed) or was never valid
            when (!(sdq_val(wb_forward_std_idx)) || (io.nack.valid && io.nack.cache_nack))
            {
               clr_ld := Bool(true)
            }
         }
         .otherwise
         {
            clr_ld := Bool(true)
         }

         when (clr_ld)
         {
            laq_executed(io.nack.lsu_idx) := Bool(false)
            debug_laq_put_to_sleep(io.nack.lsu_idx) := Bool(true)
            ld_was_killed := Bool(true)
            ld_was_put_to_sleep := !debug_laq_put_to_sleep(io.nack.lsu_idx)
            laq_forwarded_std_val(io.nack.lsu_idx) := Bool(false)
         }
      }
   }


   //-------------------------------------------------------------
   // Exception / Reset

   // for the live_store_mask, need to kill stores that haven't been committed
   val st_exc_killed_mask = Wire(Vec(num_st_entries, Bool()))
   (0 until num_st_entries).map(i => st_exc_killed_mask(i) := Bool(false))

   val null_uop = NullMicroOp

   when (reset.toBool || io.exception)
   {
      laq_head := UInt(0, MEM_ADDR_SZ)
      laq_tail := UInt(0, MEM_ADDR_SZ)

      when (reset.toBool)
      {
         stq_head := UInt(0, MEM_ADDR_SZ)
         stq_tail := UInt(0, MEM_ADDR_SZ)
         stq_commit_head := UInt(0, MEM_ADDR_SZ)
         stq_execute_head := UInt(0, MEM_ADDR_SZ)

         for (i <- 0 until num_st_entries)
         {
            saq_val(i)         := Bool(false)
            sdq_val(i)         := Bool(false)
            stq_entry_val(i)   := Bool(false)
         }
         for (i <- 0 until num_st_entries)
         {
            stq_uop(i) := null_uop
         }
      }
      .otherwise // exception
      {
         stq_tail := stq_commit_head

         for (i <- 0 until num_st_entries)
         {
            when (!stq_committed(i))
            {
               saq_val(i)            := Bool(false)
               sdq_val(i)            := Bool(false)
               stq_entry_val(i)      := Bool(false)
               st_exc_killed_mask(i) := Bool(true)
            }
         }
      }

      for (i <- 0 until num_ld_entries)
      {
         laq_addr_val(i)    := Bool(false)
         laq_allocated(i)   := Bool(false)
         laq_executed(i)    := Bool(false)
      }

   }

   //-------------------------------------------------------------
   // Live Store Mask
   // track a bit-array of stores that are alive
   // (could maybe be re-produced from the stq_head/stq_tail, but need to know include spec_killed entries)

   // TODO is this the most efficient way to compute the live store mask?
   live_store_mask := next_live_store_mask &
                        ~(st_brkilled_mask.toBits) &
                        ~(st_exc_killed_mask.toBits)

   //-------------------------------------------------------------

   val laq_maybe_full = (laq_allocated.toBits =/= UInt(0))
   val stq_maybe_full = (stq_entry_val.toBits =/= UInt(0))

   var laq_is_full = Bool(false)
   var stq_is_full = Bool(false)

   // TODO refactor this logic
   for (w <- 0 until DECODE_WIDTH)
   {
      val l_temp = laq_tail + UInt(w)
      laq_is_full = ((l_temp === laq_head || l_temp === (laq_head + UInt(num_ld_entries))) && laq_maybe_full) | laq_is_full
      val s_temp = stq_tail + UInt(w+1)
      stq_is_full = (s_temp === stq_head || s_temp === (stq_head + UInt(num_st_entries))) | stq_is_full
   }

   io.laq_full  := laq_is_full
   io.stq_full  := stq_is_full
   val stq_empty = stq_tail === stq_head //&& !stq_maybe_full

   io.new_ldq_idx := laq_tail
   io.new_stq_idx := stq_tail

   io.lsu_fencei_rdy := stq_empty && io.dmem_is_ordered

   //-------------------------------------------------------------
   // Debug & Counter outputs

   io.counters.ld_valid      := io.exe_resp.valid && io.exe_resp.bits.uop.is_load
   io.counters.ld_forwarded  := io.forward_val
   io.counters.ld_sleep      := ld_was_put_to_sleep
   io.counters.ld_killed     := ld_was_killed
   io.counters.ld_order_fail := failed_loads.reduce(_|_)

   if (DEBUG_PRINTF_LSU)
   {
      printf("wakeup_idx: %d, ld is head of ROB:%d\n", exe_ld_idx_wakeup, io.commit_load_at_rob_head)
      for (i <- 0 until NUM_LSU_ENTRIES)
      {
         val t_laddr = laq_addr(i)
         val t_saddr = saq_addr(i)
         printf("         ldq[%d]=(%c%c%c%c%c%c%c%d) st_dep(%d,m=%x) 0x%x %c %c   saq[%d]=(%c%c%c%c%c%c%c) b:%x 0x%x -> 0x%x %c %c %c %c\n"
            , UInt(i, MEM_ADDR_SZ)
            , Mux(laq_allocated(i), Str("V"), Str("-"))
            , Mux(laq_addr_val(i), Str("A"), Str("-"))
            , Mux(laq_executed(i), Str("E"), Str("-"))
            , Mux(laq_succeeded(i), Str("S"), Str("-"))
            , Mux(laq_failure(i), Str("F"), Str("_"))
            , Mux(laq_is_uncacheable(i), Str("U"), Str("_"))
            , Mux(laq_forwarded_std_val(i), Str("X"), Str("_"))
            , laq_forwarded_stq_idx(i)
            , laq_uop(i).stq_idx // youngest dep-store
            , laq_st_dep_mask(i)
            , t_laddr(19,0)

            , Mux(laq_head === UInt(i), Str("H"), Str(" "))
            , Mux(laq_tail=== UInt(i), Str("T"), Str(" "))

            , UInt(i, MEM_ADDR_SZ)
            , Mux(stq_entry_val(i), Str("V"), Str("-"))
            , Mux(saq_val(i), Str("A"), Str("-"))
            , Mux(sdq_val(i), Str("D"), Str("-"))
            , Mux(stq_committed(i), Str("C"), Str("-"))
            , Mux(stq_executed(i), Str("E"), Str("-"))
            , Mux(stq_succeeded(i), Str("S"), Str("-"))
            , Mux(saq_is_virtual(i), Str("T"), Str("-"))
            , stq_uop(i).br_mask
            , t_saddr(19,0)
            , sdq_data(i)

            , Mux(stq_head === UInt(i), Str("H"), Str(" "))
            , Mux(stq_execute_head === UInt(i), Str("E"), Str(" "))
            , Mux(stq_commit_head === UInt(i), Str("C"), Str(" "))
            , Mux(stq_tail=== UInt(i), Str("T"), Str(" "))
         )
      }}
}


// take an address and generate an 8-bit mask of which bytes within a double-word are touched
object GenByteMask
{
   def apply(addr: UInt, typ: UInt): UInt =
   {
      val mask = Wire(UInt(width = 8))
      mask := MuxCase(UInt(255,8), Array(
                   (typ === MT_B || typ === MT_BU) -> (UInt(1, 8) << addr(2,0)),
                   (typ === MT_H || typ === MT_HU) -> (UInt(3, 8) << (addr(2,1) << UInt(1))),
                   (typ === MT_W || typ === MT_WU) -> Mux(addr(2), UInt(240, 8), UInt(15, 8)),
                   (typ === MT_D)                  -> UInt(255, 8)))
      mask
   }
}


// TODO currently assumes w_addr and r_addr are identical, so no shifting
// store data is already aligned (since its the value straight from the register
// but the load data may need to be re-aligned...
object LoadDataGenerator
{
   def apply(data: UInt, mem_type: UInt): UInt =
   {
     val sext  = (mem_type === MT_B) || (mem_type === MT_H) ||
                 (mem_type === MT_W) || (mem_type === MT_D)
     val word  = (mem_type === MT_W) || (mem_type === MT_WU)
     val half  = (mem_type === MT_H) || (mem_type === MT_HU)
     val byte_ = (mem_type === MT_B) || (mem_type === MT_BU)
     val dword = (mem_type === MT_D)

      val out = Mux (dword, data,
                Mux (word , Cat(Fill(32, sext & data(31)), data(31, 0)),
                Mux (half , Cat(Fill(48, sext & data(15)), data(15, 0)),
                Mux (byte_, Cat(Fill(56, sext & data( 7)), data( 7, 0)),
                            data))))
      out // return
   }
}

class ForwardingAgeLogic(num_entries: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new Bundle
   {
      val addr_matches    = UInt(INPUT, num_entries) // bit vector of addresses that match between the load and the SAQ
      val youngest_st_idx = UInt(INPUT, MEM_ADDR_SZ) // needed to get "age"

      val forwarding_val  = Bool(OUTPUT)
      val forwarding_idx  = UInt(OUTPUT, MEM_ADDR_SZ)
   }

   // generating mask that zeroes out anything younger than tail
   val age_mask = Wire(Vec(num_entries, Bool()))
   for (i <- 0 until num_entries)
   {
      age_mask(i) := Bool(true)
      when (UInt(i) >= io.youngest_st_idx) // currently the tail points PAST last store, so use >=
      {
         age_mask(i) := Bool(false)
      }
   }

   // Priority encoder with moving tail: double length
   val matches = Wire(UInt(width = 2*num_entries))
   matches := Cat(io.addr_matches & age_mask.toBits,
                  io.addr_matches)


   val found_match = Wire(Bool())
   found_match       := Bool(false)
   io.forwarding_idx := UInt(0)

   // look for youngest, approach from the oldest side, let the last one found stick
   for (i <- 0 until (2*num_entries))
   {
      when (matches(i))
      {
         found_match := Bool(true)
         io.forwarding_idx := UInt(i % num_entries)
      }
   }


   io.forwarding_val := found_match
}

}

