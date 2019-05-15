//******************************************************************************
// Copyright (c) 2012 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Out-of-Order Load/Store Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Load/Store Unit is made up of the Load-Address Queue, the Store-Address
// Queue, and the Store-Data queue (LAQ, SAQ, and SDQ).
//
// Stores are sent to memory at (well, after) commit, loads are executed
// optimstically ASAP.  If a misspeculation was discovered, the pipeline is
// cleared. Loads put to sleep are retried.  If a LoadAddr and StoreAddr match,
// the Load can receive its data by forwarding data out of the Store-Data
// Queue.
//
// Currently, loads are sent to memory immediately, and in parallel do an
// associative search of the SAQ, on entering the LSU. If a hit on the SAQ
// search, the memory request is killed on the next cycle, and if the SDQ entry
// is valid, the store data is forwarded to the load (delayed to match the
// load-use delay to delay with the write-port structural hazard). If the store
// data is not present, or it's only a partial match (SB->LH), the load is put
// to sleep in the LAQ.
//
// Memory ordering violations are detected by stores at their addr-gen time by
// associatively searching the LAQ for newer loads that have been issued to
// memory.
//
// The store queue contains both speculated and committed stores.
//
// Only one port to memory... loads and stores have to fight for it, West Side
// Story style.
//
// TODO:
//    - Add predicting structure for ordering failures
//    - currently won't STD forward if DMEM is busy
//    - ability to turn off things if VM is disabled
//    - reconsider port count of the wakeup, retry stuff

package boom.lsu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket
import freechips.rocketchip.util.Str

import boom.common._
import boom.exu.{BrResolutionInfo, Exception, FuncUnitResp}
import boom.util.{BoolToChar, AgePriorityEncoder, IsKilledByBranch, GetNewBrMask, WrapInc, IsOlder}

class LSUExeIO(implicit p: Parameters) extends BoomBundle()(p)
{
  // The "resp" of the maddrcalc is really a "req" to the LSU
  val req       = Flipped(new ValidIO(new FuncUnitResp(xLen)))

  // Send load data to regfiles
  val iresp    = new DecoupledIO(new boom.exu.ExeUnitResp(xLen))
  val fresp    = new DecoupledIO(new boom.exu.ExeUnitResp(xLen+1)) // TODO: Should this be fLen?
}

class BoomDCacheReq(implicit p: Parameters) extends BoomBundle()(p)
{
  val uop   = new MicroOp
  val vaddr = UInt(coreMaxAddrBits.W)
  val paddr = Valid(UInt(corePAddrBits.W))
  val data  = Bits(coreDataBits.W)
}

class BoomDCacheResp(implicit p: Parameters) extends BoomBundle()(p)
{
  val data = Bits(coreDataBits.W)
  val uop  = new MicroOp
  val size = UInt(2.W)
  val nack = Bool()
}

class LSUDMemIO(implicit p: Parameters) extends BoomBundle()(p)
{
  val req         = new DecoupledIO(new BoomDCacheReq)
  val resp        = Flipped(new ValidIO(new BoomDCacheResp))

  val brinfo      = Output(new BrResolutionInfo)
}

class LSUCoreIO(implicit p: Parameters) extends BoomBundle()(p)
{
  val exe = new LSUExeIO

  val dec_uops    = Flipped(Vec(coreWidth, Valid(new MicroOp)))
  val dec_ldq_idx = Output(Vec(coreWidth, UInt(LDQ_ADDR_SZ.W)))
  val dec_stq_idx = Output(Vec(coreWidth, UInt(STQ_ADDR_SZ.W)))

  val ldq_full    = Output(Vec(coreWidth, Bool()))
  val stq_full    = Output(Vec(coreWidth, Bool()))

  val fp_stdata   = Flipped(Valid(new MicroOpWithData(fLen)))

  val ld_issued   = Valid(UInt(PREG_SZ.W))
  val ld_miss     = Output(Bool())

  val commit_store_mask = Input(Vec(coreWidth, Bool()))
  val commit_load_mask  = Input(Vec(coreWidth, Bool()))
  val commit_load_at_rob_head = Input(Bool())

  val brinfo      = Input(new BrResolutionInfo)
  val exception   = Input(Bool())
}

class LSUIO(implicit p: Parameters) extends BoomBundle()(p)
{
  val ptw  = new rocket.TLBPTWIO
  val core = new LSUCoreIO
  val dmem = new LSUDMemIO
}

class LDQEntry(implicit p: Parameters) extends BoomBundle()(p)
{
  val uop                 = new MicroOp

  val addr                = Valid(UInt(coreMaxAddrBits.W))
  val addr_is_virtual     = Bool() // Virtual address, we got a TLB miss
  val addr_is_uncacheable = Bool() // Uncacheable, wait until head of ROB to execute

  val executed            = Bool() // load sent to memory
  val succeeded           = Bool() // load returned from memory (might still have order failure)
  val order_fail          = Bool()

  val st_dep_mask         = UInt(NUM_STQ_ENTRIES.W) // list of stores we might
                                                    // depend (cleared when a
                                                    // store commits)

  val forward_std_val     = Bool()
  val forward_stq_idx     = UInt(STQ_ADDR_SZ.W) // Which store did we get the store-load forward from?
}

class STQEntry(implicit p: Parameters) extends BoomBundle()(p)
{
  val uop                 = new MicroOp

  val addr                = Valid(UInt(coreMaxAddrBits.W))
  val addr_is_virtual     = Bool() // Virtual address, we got a TLB miss
  val data                = Valid(UInt(xLen.W))

  val executed            = Bool() // sent to memory
  val committed           = Bool() // committed by ROB
}

class LSU(implicit p: Parameters, edge: freechips.rocketchip.tilelink.TLEdgeOut) extends BoomModule()(p)
{
  val io = IO(new LSUIO)


  val ldq = Reg(Vec(NUM_LDQ_ENTRIES, Valid(new LDQEntry)))
  val stq = Reg(Vec(NUM_STQ_ENTRIES, Valid(new STQEntry)))

  val ldq_head         = Reg(UInt(LDQ_ADDR_SZ.W))
  val ldq_tail         = Reg(UInt(LDQ_ADDR_SZ.W))
  val stq_head         = Reg(UInt(STQ_ADDR_SZ.W)) // point to next store to clear from STQ (i.e., send to memory)
  val stq_tail         = Reg(UInt(STQ_ADDR_SZ.W))
  val stq_commit_head  = Reg(UInt(STQ_ADDR_SZ.W)) // point to next store to commit
  val stq_execute_head = Reg(UInt(STQ_ADDR_SZ.W)) // point to next store to execute

  dontTouch(io)
  dontTouch(ldq)
  dontTouch(stq)

  val clear_store     = WireInit(false.B)
  val live_store_mask = RegInit(0.U(NUM_STQ_ENTRIES.W))
  var next_live_store_mask = Mux(clear_store, live_store_mask & ~(1.U << stq_head),
                                              live_store_mask)


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Enqueue new entries
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // This is a newer store than existing loads, so clear the bit in all the store dependency masks
  for (i <- 0 until NUM_LDQ_ENTRIES)
  {
    when (clear_store)
    {
      ldq(i).bits.st_dep_mask := ldq(i).bits.st_dep_mask & ~(1.U << stq_head)
    }
  }

  // Decode stage
  var ld_enq_idx = ldq_tail
  var st_enq_idx = stq_tail

  val ldq_nonempty = VecInit(ldq.map(_.valid)).asUInt =/= 0.U
  val stq_nonempty = VecInit(stq.map(_.valid)).asUInt =/= 0.U

  var ldq_full = Bool()
  var stq_full = Bool()

  for (w <- 0 until coreWidth)
  {
    ldq_full = ld_enq_idx === ldq_head && ldq_nonempty
    io.core.ldq_full(w)    := ldq_full
    io.core.dec_ldq_idx(w) := ld_enq_idx

    val dec_ld_val = io.core.dec_uops(w).valid && io.core.dec_uops(w).bits.is_load

    when (dec_ld_val)
    {
      ldq(ld_enq_idx).valid            := true.B
      ldq(ld_enq_idx).bits.uop         := io.core.dec_uops(w).bits
      ldq(ld_enq_idx).bits.st_dep_mask := next_live_store_mask

      ldq(ld_enq_idx).bits.addr.valid  := false.B
      ldq(ld_enq_idx).bits.executed    := false.B
      ldq(ld_enq_idx).bits.succeeded   := false.B
      ldq(ld_enq_idx).bits.order_fail  := false.B
      ldq(ld_enq_idx).bits.forward_std_val := false.B

      assert (ld_enq_idx === io.core.dec_uops(w).bits.ldq_idx, "[lsu] mismatch enq load tag.")
    }
    ld_enq_idx = Mux(dec_ld_val, WrapInc(ld_enq_idx, NUM_LDQ_ENTRIES),
                                 ld_enq_idx)

    stq_full = st_enq_idx === stq_head && stq_nonempty
    io.core.stq_full(w)    := stq_full
    io.core.dec_stq_idx(w) := st_enq_idx

    val dec_st_val = io.core.dec_uops(w).valid && io.core.dec_uops(w).bits.is_store
    when (dec_st_val)
    {
      stq(st_enq_idx).valid           := true.B
      stq(st_enq_idx).bits.uop        := io.core.dec_uops(w).bits
      stq(st_enq_idx).bits.addr.valid := false.B
      stq(st_enq_idx).bits.data.valid := false.B
      stq(st_enq_idx).bits.executed   := false.B
      stq(st_enq_idx).bits.committed  := false.B

      assert (st_enq_idx === io.core.dec_uops(w).bits.stq_idx, "[lsu] mismatch enq store tag.")
    }
    next_live_store_mask = Mux(dec_st_val, next_live_store_mask | (1.U << st_enq_idx),
                                           next_live_store_mask)
    st_enq_idx = Mux(dec_st_val, WrapInc(st_enq_idx, NUM_STQ_ENTRIES),
                                 st_enq_idx)
   }

   ldq_tail := ld_enq_idx
   stq_tail := st_enq_idx

//   io.lsu_fencei_rdy := !stq_nonempty && io.dmem_is_ordered

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
  // uopSTAs and uopSTDs fight over the ROB unbusy port.
  // And loads and store-addresses must search the LAQ (lcam) for ordering failures (or to prevent ordering failures).

  val will_fire_load_incoming = WireInit(false.B) // uses TLB, D$, SAQ-search, LAQ-search
  val will_fire_sta_incoming  = WireInit(false.B) // uses TLB,                 LAQ-search, ROB
  val will_fire_std_incoming  = WireInit(false.B) // uses                                  ROB
  val will_fire_sta_retry     = WireInit(false.B) // uses TLB,                             ROB
  val will_fire_load_retry    = WireInit(false.B) // uses TLB, D$, SAQ-search, LAQ-search
  val will_fire_store_commit  = WireInit(false.B) // uses      D$
  val will_fire_load_wakeup   = WireInit(false.B) // uses      D$, SAQ-search, LAQ-search
  val will_fire_sfence        = WireInit(false.B) // uses TLB                            , ROB

  val can_fire_sta_retry      = WireInit(false.B)
  val can_fire_load_retry     = WireInit(false.B)
  val can_fire_store_commit   = WireInit(false.B)
  val can_fire_load_wakeup    = WireInit(false.B)

  val dc_avail  = WireInit(true.B)
  val tlb_avail = WireInit(true.B)
  val rob_avail = WireInit(true.B)
  val lcam_avail= WireInit(true.B)

  val exe_req = io.core.exe.req
  when (exe_req.valid) {
    when (exe_req.bits.sfence.valid) {
      will_fire_sfence := true.B
      dc_avail   := false.B
      tlb_avail  := false.B
      lcam_avail := false.B
    }
    when (exe_req.bits.uop.ctrl.is_load) {
      will_fire_load_incoming := true.B
      dc_avail   := false.B
      tlb_avail  := false.B
      lcam_avail := false.B
    }
    when (exe_req.bits.uop.ctrl.is_sta) {
      will_fire_sta_incoming := true.B
      tlb_avail  := false.B
      rob_avail  := false.B
      lcam_avail := false.B
    }
    when (exe_req.bits.uop.ctrl.is_std) {
      will_fire_std_incoming := true.B
      rob_avail := false.B
    }
  }

  when (tlb_avail) {
    when (can_fire_sta_retry && rob_avail) {
      will_fire_sta_retry := true.B
      lcam_avail := false.B
    } .elsewhen (can_fire_load_retry) {
      will_fire_load_retry := true.B
      dc_avail := false.B
      lcam_avail := false.B
    }
  }

  when (dc_avail) {
    // TODO allow dyanmic priority here
    will_fire_store_commit := can_fire_store_commit
    will_fire_load_wakeup  := !can_fire_store_commit && can_fire_load_wakeup && lcam_avail
  }

  //--------------------------------------------
  // TLB Access

  val stq_retry_idx = WireInit(0.U(STQ_ADDR_SZ.W))
  val ldq_retry_idx = WireInit(0.U(LDQ_ADDR_SZ.W))

  // micro-op going through the TLB generate paddr's. If this is a load, it will continue
  // to the D$ and search the SAQ. uopSTD also uses this uop.
  val exe_tlb_uop = Mux(will_fire_sta_retry,  stq(stq_retry_idx).bits.uop,
                    Mux(will_fire_load_retry, ldq(ldq_retry_idx).bits.uop,
                                              exe_req.bits.uop))

  val exe_vaddr   = Mux(will_fire_sta_retry,  stq(stq_retry_idx).bits.addr.bits,
                    Mux(will_fire_load_retry, ldq(ldq_retry_idx).bits.addr.bits,
                    Mux(will_fire_sfence,     exe_req.bits.sfence.bits.addr,
                                              exe_req.bits.addr)))

  assert(!(will_fire_sta_retry && !stq(stq_retry_idx).bits.addr.valid),
    "Can't fire sta retry with no address in the stq!")
  assert(!(will_fire_sta_retry && !stq(stq_retry_idx).bits.addr_is_virtual),
    "Can't fire sta retry if the address is not virtual!")
  assert(!(will_fire_load_retry && !ldq(ldq_retry_idx).bits.addr.valid),
    "Can't fire load retry with no address in the ldq!")
  assert(!(will_fire_load_retry && !ldq(ldq_retry_idx).bits.addr_is_virtual),
    "Can't fire load retry if the address is not virtual!")

  val dtlb = Module(new rocket.TLB(
    instruction = false, lgMaxSize = log2Ceil(coreDataBytes), rocket.TLBConfig(dcacheParams.nTLBEntries)))
  dontTouch(dtlb.io)

  io.ptw <> dtlb.io.ptw
  dtlb.io.req.valid := will_fire_load_incoming ||
                       will_fire_sta_incoming ||
                       will_fire_sta_retry ||
                       will_fire_load_retry ||
                       will_fire_sfence
  dtlb.io.req.bits.vaddr := exe_vaddr
  dtlb.io.req.bits.size  := exe_tlb_uop.mem_size
  dtlb.io.req.bits.cmd   := exe_tlb_uop.mem_cmd
  dtlb.io.req.bits.passthrough := false.B // let status.vm decide
  dtlb.io.sfence         := exe_req.bits.sfence
  dtlb.io.kill           := false.B

  // exceptions
  val ma_ld = will_fire_load_incoming && exe_req.bits.mxcpt.valid // We get ma_ld in memaddrcalc
  val ma_st = will_fire_sta_incoming && exe_req.bits.mxcpt.valid // We get ma_ld in memaddrcalc
  val pf_ld = dtlb.io.req.valid && dtlb.io.resp.pf.ld && (exe_tlb_uop.is_load || exe_tlb_uop.is_amo)
  val pf_st = dtlb.io.req.valid && dtlb.io.resp.pf.st && exe_tlb_uop.is_store
  val ae_ld = dtlb.io.req.valid && dtlb.io.resp.ae.ld && (exe_tlb_uop.is_load || exe_tlb_uop.is_amo)
  val ae_st = dtlb.io.req.valid && dtlb.io.resp.ae.st && exe_tlb_uop.is_store
  // TODO check for xcpt_if and verify that never happens on non-speculative instructions.
  val mem_xcpt_valid = RegNext((pf_ld || pf_st || ae_ld || ae_st || ma_ld || ma_st) &&
                                !io.core.exception &&
                                !IsKilledByBranch(io.core.brinfo, exe_tlb_uop),
                                init=false.B)

  val xcpt_uop       = RegNext(exe_tlb_uop)

  when (mem_xcpt_valid)
  {
    // Technically only faulting AMOs need this
    assert(xcpt_uop.is_load ^ xcpt_uop.is_store)
    when (xcpt_uop.is_load)
    {
      ldq(xcpt_uop.ldq_idx).bits.uop.exception := true.B
    }
      .otherwise
    {
      stq(xcpt_uop.stq_idx).bits.uop.exception := true.B
    }
  }

  val mem_xcpt_cause = RegNext(
    Mux(ma_ld, rocket.Causes.misaligned_load.U,
    Mux(ma_st, rocket.Causes.misaligned_store.U,
    Mux(pf_ld, rocket.Causes.load_page_fault.U,
    Mux(pf_st, rocket.Causes.store_page_fault.U,
    Mux(ae_ld, rocket.Causes.load_access.U,
               rocket.Causes.store_access.U))))))


  assert (!(dtlb.io.req.valid && exe_tlb_uop.is_fence), "Fence is pretending to talk to the TLB")
  assert (!(exe_req.bits.mxcpt.valid && dtlb.io.req.valid &&
          !(exe_tlb_uop.ctrl.is_load || exe_tlb_uop.ctrl.is_sta)),
          "A uop that's not a load or store-address is throwing a memory exception.")

  val tlb_miss = dtlb.io.req.valid && (dtlb.io.resp.miss || !dtlb.io.req.ready)

  // output
  val exe_tlb_paddr = Cat(dtlb.io.resp.paddr(paddrBits-1,corePgIdxBits), exe_vaddr(corePgIdxBits-1,0))
  assert (exe_tlb_paddr === dtlb.io.resp.paddr || exe_req.bits.sfence.valid, "[lsu] paddrs should match.")

  // check if a load is uncacheable - must stop it from executing speculatively,
  // as it might have side-effects!
  val tlb_addr_uncacheable = !(dtlb.io.resp.cacheable)

  //-------------------------------------
  // Can-fire Logic & Wakeup/Retry Select

  // *** Wakeup Load from LAQ ***
  // TODO make option to only wakeup load at the head (to compare to old behavior)
  // Compute this for the next cycle to remove can_fire, ld_idx_wakeup off critical path.
  val exe_ld_idx_wakeup = RegNext(
    AgePriorityEncoder(ldq.map(e => e.bits.addr.valid && ~e.bits.executed), ldq_head))

  when ( ldq(exe_ld_idx_wakeup).valid                &&
         ldq(exe_ld_idx_wakeup).bits.addr.valid      &&
        !ldq(exe_ld_idx_wakeup).bits.addr_is_virtual &&
        !ldq(exe_ld_idx_wakeup).bits.executed        &&
        !ldq(exe_ld_idx_wakeup).bits.order_fail      &&
       (!ldq(exe_ld_idx_wakeup).bits.addr_is_uncacheable || (io.core.commit_load_at_rob_head && ldq_head === exe_ld_idx_wakeup)))
  {
    can_fire_load_wakeup := true.B
  }

  // *** Retry Load TLB-lookup from LAQ ***
  ldq_retry_idx := exe_ld_idx_wakeup

  when ( ldq(ldq_retry_idx).valid                &&
         ldq(ldq_retry_idx).bits.addr.valid      &&
         ldq(ldq_retry_idx).bits.addr_is_virtual &&
        !ldq(ldq_retry_idx).bits.executed        && // perf lose, but simplifies control
        !ldq(ldq_retry_idx).bits.order_fail      &&
         RegNext(dtlb.io.req.ready)) // TODO: Why RegNext here?
  {
    can_fire_load_retry := true.B
  }

  // *** STORES ***
  when ( stq(stq_execute_head).valid              &&
        !stq(stq_execute_head).bits.executed      &&
        !stq(stq_execute_head).bits.uop.is_fence  &&
        !mem_xcpt_valid                           &&
        !stq(stq_execute_head).bits.uop.exception &&
        (stq(stq_execute_head).bits.committed || ( stq(stq_execute_head).bits.uop.is_amo      &&
                                                   stq(stq_execute_head).bits.addr.valid      &&
                                                  !stq(stq_execute_head).bits.addr_is_virtual &&
                                                   stq(stq_execute_head).bits.data.valid)))
  {
    can_fire_store_commit := true.B
  }

  stq_retry_idx := stq_commit_head
  when (stq(stq_retry_idx).valid                &&
        stq(stq_retry_idx).bits.addr.valid      &&
        stq(stq_retry_idx).bits.addr_is_virtual &&
        RegNext(dtlb.io.req.ready))
  {
    can_fire_sta_retry := true.B
  }

  assert (!(can_fire_store_commit && stq(stq_execute_head).bits.addr_is_virtual),
            "a committed store is trying to fire to memory that has a bad paddr.")

  assert (stq(stq_execute_head).valid ||
          stq_head === stq_execute_head || stq_tail === stq_execute_head,
            "stq_execute_head got off track.")

  //-------------------------
  // Issue Someting to Memory
  //
  // Three locations a memory op can come from.
  // 1. Incoming load   ("Fast")
  // 2. Sleeper Load    ("from the LAQ")
  // 3. Store at Commit ("from SAQ")

  val exe_ldq_idx = Mux(will_fire_load_incoming || will_fire_load_retry, exe_tlb_uop.ldq_idx, exe_ld_idx_wakeup)


  // defaults
  io.dmem.req.valid      := false.B
  io.dmem.req.bits.uop   := NullMicroOp
  io.dmem.req.bits.vaddr := 0.U
  io.dmem.req.bits.data  := 0.U

  io.dmem.req.bits.paddr.valid := false.B // Send this on the next cycle
  io.dmem.req.bits.paddr.bits  := 0.U

  val mem_fired_st  = RegInit(false.B)
  mem_fired_st := false.B
  when (will_fire_store_commit) {
    io.dmem.req.valid      := true.B
    io.dmem.req.bits.vaddr := stq(stq_execute_head).bits.addr.bits
    io.dmem.req.bits.data  := stq(stq_execute_head).bits.data.bits
    io.dmem.req.bits.uop   := stq(stq_execute_head).bits.uop

    io.dmem.req.bits.paddr.valid := true.B
    io.dmem.req.bits.paddr.bits  := stq(stq_execute_head).bits.addr.bits // This has already been translated

    // TODO Nacks
    stq(stq_execute_head).bits.executed := true.B
    stq_execute_head                    := WrapInc(stq_execute_head, NUM_STQ_ENTRIES)
    mem_fired_st                        := true.B
  }
    .elsewhen (will_fire_load_incoming || will_fire_load_retry || will_fire_load_wakeup)
  {
    io.dmem.req.valid      := true.B
    io.dmem.req.bits.vaddr := exe_vaddr
    io.dmem.req.bits.uop   := exe_tlb_uop

    io.dmem.req.bits.paddr.valid := !tlb_miss
    io.dmem.req.bits.paddr.bits  := exe_tlb_paddr

    ldq(exe_ldq_idx).bits.executed  := true.B
  }

  assert (PopCount(VecInit(will_fire_store_commit, will_fire_load_incoming,
                           will_fire_load_retry, will_fire_load_wakeup))
      <= 1.U, "Multiple requestors firing to the data cache.")


  //-------------------------------------------------------------
  // Kill speculated entries on branch mispredict
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Kill stores
  val st_brkilled_mask = Wire(Vec(NUM_STQ_ENTRIES, Bool()))
  for (i <- 0 until NUM_STQ_ENTRIES)
  {
    st_brkilled_mask(i) := false.B

    when (stq(i).valid)
    {
      stq(i).bits.uop.br_mask := GetNewBrMask(io.core.brinfo, stq(i).bits.uop.br_mask)

      when (IsKilledByBranch(io.core.brinfo, stq(i).bits.uop))
      {
        stq(i).valid           := false.B
        stq(i).bits.addr.valid := false.B
        stq(i).bits.data.valid := false.B
        st_brkilled_mask(i)    := true.B
      }
    }

    assert (!(IsKilledByBranch(io.core.brinfo, stq(i).bits.uop) && stq(i).valid && stq(i).bits.committed),
      "Branch is trying to clear a committed store.")
  }

  // Kill loads
  for (i <- 0 until NUM_LDQ_ENTRIES)
  {
    when (ldq(i).valid)
    {
      ldq(i).bits.uop.br_mask := GetNewBrMask(io.core.brinfo, ldq(i).bits.uop.br_mask)
      when (IsKilledByBranch(io.core.brinfo, ldq(i).bits.uop))
      {
        ldq(i).valid           := false.B
        ldq(i).bits.addr.valid := false.B
      }
    }
  }

  //-------------------------------------------------------------
  when (io.core.brinfo.valid && io.core.brinfo.mispredict && !io.core.exception)
  {
    stq_tail := io.core.brinfo.stq_idx
    ldq_tail := io.core.brinfo.ldq_idx
  }

  //-------------------------------------------------------------
  // Exception / Reset

  // for the live_store_mask, need to kill stores that haven't been committed
  val st_exc_killed_mask = WireInit(VecInit((0 until NUM_STQ_ENTRIES).map(x=>false.B)))

  when (reset.asBool || io.core.exception)
  {
    ldq_head := 0.U
    ldq_tail := 0.U

    when (reset.asBool)
    {
      stq_head := 0.U
      stq_tail := 0.U
      stq_commit_head  := 0.U
      stq_execute_head := 0.U

      for (i <- 0 until NUM_STQ_ENTRIES)
      {
        stq(i).valid           := false.B
        stq(i).bits.addr.valid := false.B
        stq(i).bits.data.valid := false.B
        stq(i).bits.uop        := NullMicroOp
      }
    }
      .otherwise // exception
    {
      stq_tail := stq_commit_head

      for (i <- 0 until NUM_STQ_ENTRIES)
      {
        when (!stq(i).bits.committed)
        {
          stq(i).valid           := false.B
          stq(i).bits.addr.valid := false.B
          stq(i).bits.data.valid := false.B
          st_exc_killed_mask(i)  := true.B
        }
      }
    }

    for (i <- 0 until NUM_LDQ_ENTRIES)
    {
      ldq(i).valid           := false.B
      ldq(i).bits.addr.valid := false.B
      ldq(i).bits.executed   := false.B
    }
  }

  //-------------------------------------------------------------
  // Live Store Mask
  // track a bit-array of stores that are alive
  // (could maybe be re-produced from the stq_head/stq_tail, but need to know include spec_killed entries)

  // TODO is this the most efficient way to compute the live store mask?
  live_store_mask := next_live_store_mask &
                    ~(st_brkilled_mask.asUInt) &
                    ~(st_exc_killed_mask.asUInt)

}

/**
 * IO bundle representing the different signals to interact with the backend
 * (dcache, dcache shim, etc) 2memory system.
 *
 * @param pl_width pipeline width of the processor
 */
class LoadStoreUnitIO(val pl_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   // Decode Stage
   // Track which stores are "alive" in the pipeline
   // allows us to know which stores get killed by branch mispeculation
   val dec_st_vals        = Input(Vec(pl_width,  Bool()))
   val dec_ld_vals        = Input(Vec(pl_width,  Bool()))
   val dec_uops           = Input(Vec(pl_width, new MicroOp()))

   val new_ldq_idx        = Output(Vec(pl_width, UInt(LDQ_ADDR_SZ.W)))
   val new_stq_idx        = Output(Vec(pl_width, UInt(STQ_ADDR_SZ.W)))

   // Execute Stage
   val exe_resp           = Flipped(new ValidIO(new FuncUnitResp(xLen)))
   val fp_stdata          = Flipped(Valid(new MicroOpWithData(fLen)))

   // Send out Memory Request
   val memreq_val         = Output(Bool())
   val memreq_addr        = Output(UInt(corePAddrBits.W))
   val memreq_wdata       = Output(UInt(xLen.W))
   val memreq_uop         = Output(new MicroOp())

   // Memory Stage
   val memreq_kill        = Output(Bool()) // kill request sent out last cycle
   val mem_ldSpecWakeup   = Valid(UInt(PREG_SZ.W)) // do NOT send out FP loads.

   // Forward Store Data to Register File
   // TODO turn into forward bundle
   val forward_val        = Output(Bool())
   val forward_data       = Output(UInt(xLen.W))
   val forward_uop        = Output(new MicroOp()) // the load microop (for its pdst)

   // Receive Memory Response
   val memresp            = Flipped(new ValidIO(new MicroOp()))

   // Commit Stage
   val commit_store_mask  = Input(Vec(pl_width, Bool()))
   val commit_load_mask   = Input(Vec(pl_width, Bool()))
   val commit_load_at_rob_head = Input(Bool())

   // Handle Release Probes
   val release            = Flipped(Valid(new ReleaseInfo))

   // Handle Branch Misspeculations
   val brinfo             = Input(new BrResolutionInfo())

   // Stall Decode as appropriate
   val laq_full           = Output(Vec(pl_width, Bool()))
   val stq_full           = Output(Vec(pl_width, Bool()))

   val exception          = Input(Bool())
   // Let the stores clear out the busy bit in the ROB.
   // Two ports, one for integer and the other for FP.
   // Otherwise, we must back-pressure incoming FP store-data micro-ops.
   val clr_bsy_valid      = Output(Vec(2, Bool()))
   val clr_bsy_rob_idx    = Output(Vec(2, UInt(robAddrSz.W)))

   // LSU can mark its instructions as speculatively safe in the ROB.
   val clr_unsafe_valid   = Output(Bool())
   val clr_unsafe_rob_idx = Output(UInt(robAddrSz.W))

   val lsu_fencei_rdy     = Output(Bool())

   val xcpt = new ValidIO(new Exception)

   // cache nacks
   val nack               = Input(new NackInfo())

// causing stuff to dissapear
//   val dmem = new DCMemPortIO().flip()
   val dmem_is_ordered = Input(Bool())
   val dmem_req_ready = Input(Bool())    // arbiter can back-pressure us (or MSHRs can fill up).
                                       // although this is also turned into a
                                       // nack two cycles later in the cache
                                       // wrapper, we can prevent spurious
                                       // retries as well as some load ordering
                                       // failures.

   val ptw = new rocket.TLBPTWIO
   val sfence = new rocket.SFenceReq

   val counters = new Bundle
   {
      val ld_valid        = Output(Bool()) // a load address micro-op has entered the LSU
      val ld_forwarded    = Output(Bool())
      val ld_sleep        = Output(Bool())
      val ld_killed       = Output(Bool())
      val stld_order_fail = Output(Bool())
      val ldld_order_fail = Output(Bool())
   }

   val debug_tsc = Input(UInt(xLen.W))     // time stamp counter

}

/**
 * Load store unit. Holds SAQ, LAQ, SDQ.
 *
 * @param pl_width pipeline width of the processor
 */
class LoadStoreUnit(pl_width: Int)(implicit p: Parameters,
                                   edge: freechips.rocketchip.tilelink.TLEdgeOut) extends BoomModule()(p)
   with freechips.rocketchip.rocket.constants.MemoryOpConstants
{
   val io = IO(new LoadStoreUnitIO(pl_width))

   // Load-Address Queue
   val laq_addr_val       = Reg(Vec(NUM_LDQ_ENTRIES, Bool()))
   val laq_addr           = Mem(NUM_LDQ_ENTRIES, UInt(coreMaxAddrBits.W))

   val laq_allocated      = Reg(Vec(NUM_LDQ_ENTRIES, Bool())) // entry has been allocated
   val laq_is_virtual     = Reg(Vec(NUM_LDQ_ENTRIES, Bool())) // address in LAQ is a virtual address.
                                                             // There was a tlb_miss and a retry is required.
   val laq_is_uncacheable = Reg(Vec(NUM_LDQ_ENTRIES, Bool())) // address in LAQ is an uncacheable address.
                                                             // Can only execute once it's the head of the ROB
   val laq_executed       = Reg(Vec(NUM_LDQ_ENTRIES, Bool())) // load has been issued to memory
                                                             // (immediately set this bit)
   val laq_succeeded      = Reg(Vec(NUM_LDQ_ENTRIES, Bool())) // load has returned from memory,
                                                             // but may still have an ordering failure
   val laq_failure        = RegInit(VecInit(Seq.fill(NUM_LDQ_ENTRIES) { false.B }))  // ordering fail, must retry
                                                                                    // (at commit time,
                                                                                    // which requires a rollback)
   val laq_uop            = Reg(Vec(NUM_LDQ_ENTRIES, new MicroOp()))
   //laq_uop.stq_idx between oldest and youngest (dep_mask can't establish age :( ),
   // "aka store coloring" if you're Intel
   //val laq_request   = Vec.fill(NUM_LDQ_ENTRIES) { Reg(resetVal = false.B) } // TODO sleeper load requesting
                                                                              // issue to memory (perhaps stores
                                                                              // broadcast, sees its store-set
                                                                              // finished up)

   // track window of stores we depend on
   val laq_st_dep_mask        = Reg(Vec(NUM_LDQ_ENTRIES, UInt(NUM_STQ_ENTRIES.W))) // list of stores we might
                                                                                 // depend (cleared when a
                                                                                 // store commits)
   val laq_forwarded_std_val  = Reg(Vec(NUM_LDQ_ENTRIES, Bool()))
   val laq_forwarded_stq_idx  = Reg(Vec(NUM_LDQ_ENTRIES, UInt(LDQ_ADDR_SZ.W)))    // which store did get
                                                                                 // store-load forwarded
                                                                                 // data from? compare later
                                                                                 // to see I got things correct
   val debug_laq_put_to_sleep = Reg(Vec(NUM_LDQ_ENTRIES, Bool())) // did a load get put to sleep at least once?
   //val laq_st_wait_mask = Vec.fill(NUM_LDQ_ENTRIES) { Reg() { Bits(width = NUM_STQ_ENTRIES) } } // TODO list of stores
                                                                                                // we might depend on
                                                                                                // whose addresses are
                                                                                                // not yet computed
   //val laq_block_val    = Vec.fill(NUM_LDQ_ENTRIES) { Reg() { Bool() } }                       // TODO something is
                                                                                                //blocking us from
                                                                                                //executing
   //val laq_block_id     = Vec.fill(NUM_LDQ_ENTRIES) { Reg() { UInt(width = MEM_ADDR_SZ) } }    // TODO something is
                                                                                                // blocking us from
                                                                                                // executing, listen
                                                                                                // for this ID to wakeup

   // Store-Address Queue
   val saq_val       = Reg(Vec(NUM_STQ_ENTRIES, Bool()))
   val saq_is_virtual= Reg(Vec(NUM_STQ_ENTRIES, Bool())) // address in SAQ is a virtual address.
                                                         // There was a tlb_miss and a retry is required.
   val saq_addr      = Mem(NUM_STQ_ENTRIES, UInt(coreMaxAddrBits.W))

   // Store-Data Queue
   val sdq_val       = Reg(Vec(NUM_STQ_ENTRIES, Bool()))
   val sdq_data      = Reg(Vec(NUM_STQ_ENTRIES, UInt(xLen.W)))

   // Shared Store Queue Information
   val stq_uop       = Reg(Vec(NUM_STQ_ENTRIES, new MicroOp()))
   // TODO not convinced I actually need stq_allocated; I think other ctrl signals gate this off
   val stq_allocated = Reg(Vec(NUM_STQ_ENTRIES, Bool())) // this may be valid, but not TRUE (on exceptions, this doesn't
                                                        // get cleared but STQ_TAIL gets moved)
   val stq_executed  = Reg(Vec(NUM_STQ_ENTRIES, Bool())) // sent to mem
   val stq_succeeded = Reg(Vec(NUM_STQ_ENTRIES, Bool())) // returned TODO is this needed, or can we just advance the
                                                        // stq_head?
   val stq_committed = Reg(Vec(NUM_STQ_ENTRIES, Bool())) // the ROB has committed us, so we can now send our store
                                                        // to memory

   val laq_head = Reg(UInt())
   val laq_tail = Reg(UInt()) // point to next available (or if full, the laq_head entry).
   val stq_head = Reg(UInt()) // point to next store to clear from STQ (i.e., send to memory)
   val stq_tail = Reg(UInt()) // point to next available, open entry
   val stq_commit_head = Reg(UInt()) // point to next store to commit
   val stq_execute_head = Reg(UInt()) // point to next store to execute

   val clear_store = Wire(Bool())
   clear_store := false.B

   val live_store_mask = RegInit(0.U(NUM_STQ_ENTRIES.W))
   var next_live_store_mask = Mux(clear_store, live_store_mask & ~(1.U << stq_head),
                                                live_store_mask)

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // Enqueue new entries
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // put this earlier than Enqueue, since this is lower priority to laq_st_dep_mask
   for (i <- 0 until NUM_LDQ_ENTRIES)
   {
      when (clear_store)
      {
         laq_st_dep_mask(i) := laq_st_dep_mask(i) & ~(1.U << stq_head)
      }
   }

   // Decode stage ----------------------------

   var ld_enq_idx = laq_tail
   var st_enq_idx = stq_tail

   val laq_nonempty = laq_allocated.asUInt =/= 0.U
   val stq_nonempty = stq_allocated.asUInt =/= 0.U

   var laq_full = Bool()
   var stq_full = Bool()

   for (w <- 0 until pl_width)
   {
      laq_full = ld_enq_idx === laq_head && laq_nonempty
      io.laq_full(w) := laq_full
      io.new_ldq_idx(w) := ld_enq_idx

      when (io.dec_ld_vals(w))
      {
         laq_uop(ld_enq_idx)          := io.dec_uops(w)
         laq_st_dep_mask(ld_enq_idx)  := next_live_store_mask

         laq_allocated(ld_enq_idx)    := true.B
         laq_addr_val (ld_enq_idx)    := false.B
         laq_executed (ld_enq_idx)    := false.B
         laq_succeeded(ld_enq_idx)    := false.B
         laq_failure  (ld_enq_idx)    := false.B
         laq_forwarded_std_val(ld_enq_idx)  := false.B
         debug_laq_put_to_sleep(ld_enq_idx) := false.B

         assert (ld_enq_idx === io.dec_uops(w).ldq_idx, "[lsu] mismatch enq load tag.")
      }
      ld_enq_idx = Mux(io.dec_ld_vals(w), WrapInc(ld_enq_idx, NUM_LDQ_ENTRIES),
                                          ld_enq_idx)

      stq_full = st_enq_idx === stq_head && stq_nonempty
      io.stq_full(w) := stq_full
      io.new_stq_idx(w) := st_enq_idx

      when (io.dec_st_vals(w))
      {
         stq_uop(st_enq_idx)       := io.dec_uops(w)

         stq_allocated(st_enq_idx) := true.B
         saq_val      (st_enq_idx) := false.B
         sdq_val      (st_enq_idx) := false.B
         stq_executed (st_enq_idx) := false.B
         stq_succeeded(st_enq_idx) := false.B
         stq_committed(st_enq_idx) := false.B

         assert (st_enq_idx === io.dec_uops(w).stq_idx, "[lsu] mismatch enq store tag.")
      }
      next_live_store_mask = Mux(io.dec_st_vals(w), next_live_store_mask | (1.U << st_enq_idx),
                                                    next_live_store_mask)
      st_enq_idx = Mux(io.dec_st_vals(w), WrapInc(st_enq_idx, NUM_STQ_ENTRIES),
                                          st_enq_idx)
   }

   laq_tail := ld_enq_idx
   stq_tail := st_enq_idx

   io.lsu_fencei_rdy := !stq_nonempty && io.dmem_is_ordered

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
   // uopSTAs and uopSTDs fight over the ROB unbusy port.
   // And loads and store-addresses must search the LAQ (lcam) for ordering failures (or to prevent ordering failures).

   val will_fire_load_incoming = WireInit(false.B) // uses TLB, D$, SAQ-search, LAQ-search
   val will_fire_sta_incoming  = WireInit(false.B) // uses TLB,                 LAQ-search, ROB
   val will_fire_std_incoming  = WireInit(false.B) // uses                                  ROB
   val will_fire_sta_retry     = WireInit(false.B) // uses TLB,                             ROB
   val will_fire_load_retry    = WireInit(false.B) // uses TLB, D$, SAQ-search, LAQ-search
   val will_fire_store_commit  = WireInit(false.B) // uses      D$
   val will_fire_load_wakeup   = WireInit(false.B) // uses      D$, SAQ-search, LAQ-search
   val will_fire_sfence        = WireInit(false.B) // uses TLB                            , ROB

   val can_fire_sta_retry      = WireInit(false.B)
   val can_fire_load_retry     = WireInit(false.B)
   val can_fire_store_commit   = WireInit(false.B)
   val can_fire_load_wakeup    = WireInit(false.B)

   val dc_avail  = WireInit(true.B)
   val tlb_avail = WireInit(true.B)
   val rob_avail = WireInit(true.B)
   val lcam_avail= WireInit(true.B)

   // give first priority to incoming uops
   when (io.exe_resp.valid)
   {
      when (io.exe_resp.bits.sfence.valid)
      {
         will_fire_sfence := true.B
         dc_avail   := false.B
         tlb_avail  := false.B
         lcam_avail := false.B
      }
      when (io.exe_resp.bits.uop.ctrl.is_load)
      {
         will_fire_load_incoming := true.B
         dc_avail   := false.B
         tlb_avail  := false.B
         lcam_avail := false.B
      }
      when (io.exe_resp.bits.uop.ctrl.is_sta)
      {
         will_fire_sta_incoming := true.B
         tlb_avail  := false.B
         rob_avail  := false.B
         lcam_avail := false.B
      }
      when (io.exe_resp.bits.uop.ctrl.is_std)
      {
         will_fire_std_incoming := true.B
         rob_avail := false.B
      }
   }

   when (tlb_avail)
   {
      when (can_fire_sta_retry && rob_avail)
      {
         will_fire_sta_retry := true.B
         lcam_avail := false.B
      }
      .elsewhen (can_fire_load_retry)
      {
         will_fire_load_retry := true.B
         dc_avail := false.B
         lcam_avail := false.B
      }
   }

   when (dc_avail)
   {
      // TODO allow dyanmic priority here
      will_fire_store_commit := can_fire_store_commit
      will_fire_load_wakeup  := !can_fire_store_commit && can_fire_load_wakeup && lcam_avail
   }

   //--------------------------------------------
   // TLB Access

   val stq_retry_idx = Wire(UInt(STQ_ADDR_SZ.W))
   val laq_retry_idx = Wire(UInt(LDQ_ADDR_SZ.W))

   // micro-op going through the TLB generate paddr's. If this is a load, it will continue
   // to the D$ and search the SAQ. uopSTD also uses this uop.
   val exe_tlb_uop = Mux(will_fire_sta_retry,  stq_uop(stq_retry_idx),
                     Mux(will_fire_load_retry, laq_uop(laq_retry_idx),
                                               io.exe_resp.bits.uop))

   val exe_vaddr   = Mux(will_fire_sta_retry,  saq_addr(stq_retry_idx),
                     Mux(will_fire_load_retry, laq_addr(laq_retry_idx),
                                               io.exe_resp.bits.addr.asUInt))

   val dtlb = Module(new rocket.TLB(
      instruction = false, lgMaxSize = log2Ceil(coreDataBytes), rocket.TLBConfig(dcacheParams.nTLBEntries)))

   io.ptw <> dtlb.io.ptw
   dtlb.io.req.valid := will_fire_load_incoming ||
                        will_fire_sta_incoming ||
                        will_fire_sta_retry ||
                        will_fire_load_retry ||
                        will_fire_sfence
   dtlb.io.req.bits.vaddr := Mux(io.exe_resp.bits.sfence.valid, io.exe_resp.bits.sfence.bits.addr, exe_vaddr)
   dtlb.io.req.bits.size := exe_tlb_uop.mem_size
   dtlb.io.req.bits.cmd  := exe_tlb_uop.mem_cmd
   dtlb.io.req.bits.passthrough := false.B // let status.vm decide
   dtlb.io.sfence := io.exe_resp.bits.sfence
   dtlb.io.kill := DontCare

   // exceptions
   val ma_ld = io.exe_resp.valid && io.exe_resp.bits.mxcpt.valid && exe_tlb_uop.is_load
   val pf_ld = dtlb.io.req.valid && dtlb.io.resp.pf.ld && (exe_tlb_uop.is_load || exe_tlb_uop.is_amo)
   val pf_st = dtlb.io.req.valid && dtlb.io.resp.pf.st && exe_tlb_uop.is_store
   val ae_ld = dtlb.io.req.valid && dtlb.io.resp.ae.ld && (exe_tlb_uop.is_load || exe_tlb_uop.is_amo)
   val ae_st = dtlb.io.req.valid && dtlb.io.resp.ae.st && exe_tlb_uop.is_store
   // TODO check for xcpt_if and verify that never happens on non-speculative instructions.
   val mem_xcpt_valid = RegNext(((pf_ld || pf_st || ae_ld || ae_st) ||
                                 (io.exe_resp.valid && io.exe_resp.bits.mxcpt.valid)) &&
                                 !io.exception &&
                                 !IsKilledByBranch(io.brinfo, exe_tlb_uop),
                            init=false.B)

   val xcpt_uop       = RegNext(Mux(ma_ld, io.exe_resp.bits.uop, exe_tlb_uop))


   when (mem_xcpt_valid)
   {
      // Technically only faulting AMOs need this
      assert(xcpt_uop.is_load ^ xcpt_uop.is_store)
      when (xcpt_uop.is_load)
      {
         laq_uop(xcpt_uop.ldq_idx).exception := true.B
      }
      .otherwise
      {
         stq_uop(xcpt_uop.stq_idx).exception := true.B
      }
   }

   val mem_xcpt_cause = RegNext(
      Mux(io.exe_resp.valid && io.exe_resp.bits.mxcpt.valid,
         io.exe_resp.bits.mxcpt.bits,
      Mux(pf_ld,
         rocket.Causes.load_page_fault.U,
      Mux(pf_st,
         rocket.Causes.store_page_fault.U,
      Mux(ae_ld,
         rocket.Causes.load_access.U,
         rocket.Causes.store_access.U
      )))))

   assert (!(dtlb.io.req.valid && exe_tlb_uop.is_fence), "Fence is pretending to talk to the TLB")
   assert (!(io.exe_resp.bits.mxcpt.valid && io.exe_resp.valid &&
           !(io.exe_resp.bits.uop.ctrl.is_load || io.exe_resp.bits.uop.ctrl.is_sta)),
           "A uop that's not a load or store-address is throwing a memory exception.")

   val tlb_miss = dtlb.io.req.valid && (dtlb.io.resp.miss || !dtlb.io.req.ready)

   // output
   val exe_tlb_paddr = Cat(dtlb.io.resp.paddr(paddrBits-1,corePgIdxBits), exe_vaddr(corePgIdxBits-1,0))
   assert (exe_tlb_paddr === dtlb.io.resp.paddr || io.exe_resp.bits.sfence.valid, "[lsu] paddrs should match.")

   // check if a load is uncacheable - must stop it from executing speculatively,
   // as it might have side-effects!
   val tlb_addr_uncacheable = !(dtlb.io.resp.cacheable)

   //-------------------------------------
   // Can-fire Logic & Wakeup/Retry Select

   // *** Wakeup Load from LAQ ***

   // TODO make option to only wakeup load at the head (to compare to old behavior)
   // Compute this for the next cycle to remove can_fire, ld_idx_wakeup off critical path.
   val exe_ld_idx_wakeup = RegNext(
      AgePriorityEncoder((0 until NUM_LDQ_ENTRIES).map(i => laq_addr_val(i) & ~laq_executed(i)), laq_head))

   when (laq_addr_val       (exe_ld_idx_wakeup) &&
         !laq_is_virtual    (exe_ld_idx_wakeup) &&
         laq_allocated      (exe_ld_idx_wakeup) &&
         !laq_executed      (exe_ld_idx_wakeup) &&
         !laq_failure       (exe_ld_idx_wakeup) &&
         (!laq_is_uncacheable(exe_ld_idx_wakeup) || (io.commit_load_at_rob_head && laq_head === exe_ld_idx_wakeup))
         )
   {
      can_fire_load_wakeup := true.B
   }

   // *** Retry Load TLB-lookup from LAQ ***

   laq_retry_idx := exe_ld_idx_wakeup

   when (laq_allocated (laq_retry_idx) &&
         laq_addr_val  (laq_retry_idx) &&
         laq_is_virtual(laq_retry_idx) &&
         !laq_executed (laq_retry_idx) && // perf lose, but simplifies control
         !laq_failure  (laq_retry_idx) &&
         RegNext(dtlb.io.req.ready))
   {
      can_fire_load_retry := true.B
   }

   // *** STORES ***

   when (stq_allocated(stq_execute_head) &&
         (stq_committed(stq_execute_head) ||
            (stq_uop(stq_execute_head).is_amo &&
            saq_val(stq_execute_head) &&
            !saq_is_virtual(stq_execute_head) &&
            sdq_val(stq_execute_head)
            )) &&
         !stq_executed(stq_execute_head) &&
         !stq_uop(stq_execute_head).is_fence &&
         !mem_xcpt_valid &&
         !stq_uop(stq_execute_head).exception
   )
   {
      can_fire_store_commit := true.B
   }

   stq_retry_idx := stq_commit_head

   when (stq_allocated (stq_retry_idx) &&
         saq_val       (stq_retry_idx) &&
         saq_is_virtual(stq_retry_idx) &&
         RegNext(dtlb.io.req.ready))
   {
      can_fire_sta_retry := true.B
   }

   assert (!(can_fire_store_commit && saq_is_virtual(stq_execute_head)),
            "a committed store is trying to fire to memory that has a bad paddr.")

   assert (stq_allocated(stq_execute_head) ||
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
   io.memreq_val     := false.B
   io.memreq_addr    := exe_ld_addr
   io.memreq_wdata   := sdq_data(stq_execute_head)
   io.memreq_uop     := exe_ld_uop

   val mem_fired_st = RegInit(false.B)
   mem_fired_st := false.B
   when (will_fire_store_commit)
   {
      io.memreq_addr  := saq_addr(stq_execute_head)
      io.memreq_uop   := stq_uop (stq_execute_head)

      // prevent this store going out if an earlier store just got nacked!
      when (!(io.nack.valid && !io.nack.isload))
      {
         io.memreq_val   := true.B
         stq_executed(stq_execute_head) := true.B
         stq_execute_head := WrapInc(stq_execute_head, NUM_STQ_ENTRIES)
         mem_fired_st := true.B
      }
   }
   .elsewhen (will_fire_load_incoming || will_fire_load_retry || will_fire_load_wakeup)
   {
      io.memreq_val   := true.B
      io.memreq_addr  := exe_ld_addr
      io.memreq_uop   := exe_ld_uop

      laq_executed(exe_ld_uop.ldq_idx) := true.B
      laq_failure (exe_ld_uop.ldq_idx) := (will_fire_load_incoming && (ma_ld || pf_ld)) ||
                                          (will_fire_load_retry && pf_ld)

   }

   assert (PopCount(VecInit(will_fire_store_commit, will_fire_load_incoming,
                            will_fire_load_retry, will_fire_load_wakeup))
      <= 1.U, "Multiple requestors firing to the data cache.")



   //-------------------------------------------------------------
   // Write Addr into the LAQ/SAQ

   when (will_fire_load_incoming || will_fire_load_retry)
   {
      laq_addr_val      (exe_tlb_uop.ldq_idx)      := true.B
      laq_addr          (exe_tlb_uop.ldq_idx)      := Mux(tlb_miss, exe_vaddr, exe_tlb_paddr)
      laq_uop           (exe_tlb_uop.ldq_idx).pdst := exe_tlb_uop.pdst
      laq_is_virtual    (exe_tlb_uop.ldq_idx)      := tlb_miss
      laq_is_uncacheable(exe_tlb_uop.ldq_idx)      := tlb_addr_uncacheable && !tlb_miss

      assert(!(will_fire_load_incoming && laq_addr_val(exe_tlb_uop.ldq_idx)),
         "[lsu] incoming load is overwriting a valid address.")
   }

   when (will_fire_sta_incoming || will_fire_sta_retry)
   {
      saq_val       (exe_tlb_uop.stq_idx)      := !pf_st // prevent AMOs from executing!
      saq_addr      (exe_tlb_uop.stq_idx)      := Mux(tlb_miss, exe_vaddr, exe_tlb_paddr)
      stq_uop       (exe_tlb_uop.stq_idx).pdst := exe_tlb_uop.pdst // needed for amo's TODO this is expensive,
                                                                   // can we get around this?
      saq_is_virtual(exe_tlb_uop.stq_idx)      := tlb_miss

      assert(!(will_fire_sta_incoming && saq_val(exe_tlb_uop.stq_idx)),
         "[lsu] incoming store is overwriting a valid address.")
   }

   // use two ports on STD to handle Integer and FP store data.
   when (will_fire_std_incoming)
   {
      val sidx = io.exe_resp.bits.uop.stq_idx
      sdq_val (sidx) := true.B
      sdq_data(sidx) := io.exe_resp.bits.data.asUInt

      assert(!(will_fire_std_incoming && sdq_val(sidx)),
         "[lsu] incoming store is overwriting a valid data entry.")
   }

   //--------------------------------------------
   // FP Data
   // Store Data Generation MicroOps come in directly from the FP registerfile,
   // and not through the exe_resp datapath.

   when (io.fp_stdata.valid)
   {
      val sidx = io.fp_stdata.bits.uop.stq_idx
      sdq_val (sidx) := true.B
      sdq_data(sidx) := io.fp_stdata.bits.data.asUInt
   }
   assert(!(io.fp_stdata.valid && io.exe_resp.valid && io.exe_resp.bits.uop.ctrl.is_std &&
      io.fp_stdata.bits.uop.stq_idx === io.exe_resp.bits.uop.stq_idx),
      "[lsu] FP and INT data is fighting over the same sdq entry.")

   require (xLen >= fLen) // otherwise the SDQ is missized.

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // Cache Access Cycle (Mem)
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // search SAQ for matches

   val mem_tlb_paddr    = RegNext(exe_tlb_paddr)
   // TODO can we/should we use mem_tlb_uop from the LAQ/SAQ and avoid the exe_resp.uop (move need for mem_typ, etc.).
   val mem_tlb_uop      = RegNext(exe_tlb_uop) // not valid for std_incoming!
   mem_tlb_uop.br_mask := GetNewBrMask(io.brinfo, exe_tlb_uop)
   val mem_tlb_miss     = RegNext(tlb_miss, init=false.B)
   val mem_tlb_uncacheable = RegNext(tlb_addr_uncacheable, init=false.B)
   val mem_ld_used_tlb  = RegNext(will_fire_load_incoming || will_fire_load_retry)

   // the load address that will search the SAQ (either a fast load or a retry load)
   val mem_ld_addr = Mux(RegNext(will_fire_load_wakeup), RegNext(laq_addr(exe_ld_idx_wakeup)), mem_tlb_paddr)
   val mem_ld_uncacheable = Mux(RegNext(will_fire_load_wakeup), RegNext(laq_is_uncacheable(exe_ld_idx_wakeup)), mem_tlb_uncacheable)
   val mem_ld_uop  = RegNext(exe_ld_uop)
   mem_ld_uop.br_mask := GetNewBrMask(io.brinfo, exe_ld_uop)
   val mem_ld_killed = Wire(Bool()) // was a load killed in execute

   val mem_fired_ld = RegNext((will_fire_load_incoming ||
                                    will_fire_load_retry ||
                                    will_fire_load_wakeup))
   val mem_fired_sta = RegNext((will_fire_sta_incoming || will_fire_sta_retry), init=false.B)
   val mem_fired_stdi = RegNext(will_fire_std_incoming, init=false.B)
   val mem_fired_stdf = RegNext(io.fp_stdata.valid, init=false.B)
   val mem_fired_sfence = RegNext(will_fire_sfence, init=false.B)

   // Mark instructions as safe after successful address translation.
   // Need to delay to same cycle as exception broadcast into ROB to avoid
   // the PNR 'jumping the gun' over a misspeculated ordering.
   io.clr_unsafe_valid := RegNext(!mem_tlb_miss && (mem_ld_used_tlb || mem_fired_sta))
   io.clr_unsafe_rob_idx := RegNext(mem_tlb_uop.rob_idx)

   mem_ld_killed := false.B
   when (RegNext(
         (IsKilledByBranch(io.brinfo, exe_ld_uop) ||
         io.exception ||
         (tlb_addr_uncacheable && dtlb.io.req.valid))) ||
      io.exception)
   {
      mem_ld_killed := true.B && mem_fired_ld
   }

   io.mem_ldSpecWakeup.valid := RegNext(will_fire_load_incoming
                                     && !io.exe_resp.bits.uop.fp_val
                                     && io.exe_resp.bits.uop.pdst =/= 0.U, init=false.B)
   io.mem_ldSpecWakeup.bits := mem_ld_uop.pdst

   // tell the ROB to clear the busy bit on the incoming store
   val clr_bsy_valid = RegInit(false.B)
   val clr_bsy_robidx = Reg(UInt(robAddrSz.W))
   val clr_bsy_brmask = Reg(UInt(MAX_BR_COUNT.W))
   clr_bsy_valid  := false.B
   clr_bsy_robidx := mem_tlb_uop.rob_idx
   clr_bsy_brmask := GetNewBrMask(io.brinfo, mem_tlb_uop)

   when (mem_fired_sta && !mem_tlb_miss && mem_fired_stdi)
   {
      clr_bsy_valid := !mem_tlb_uop.is_amo && !IsKilledByBranch(io.brinfo, mem_tlb_uop) &&
                       !io.exception && !RegNext(io.exception)
      clr_bsy_robidx := mem_tlb_uop.rob_idx
      clr_bsy_brmask := GetNewBrMask(io.brinfo, mem_tlb_uop)
   }
   .elsewhen (mem_fired_sta && !mem_tlb_miss)
   {
      clr_bsy_valid := sdq_val(mem_tlb_uop.stq_idx) &&
                       !mem_tlb_uop.is_amo &&
                       !IsKilledByBranch(io.brinfo, mem_tlb_uop) &&
                       !io.exception && !RegNext(io.exception)
      clr_bsy_robidx := mem_tlb_uop.rob_idx
      clr_bsy_brmask := GetNewBrMask(io.brinfo, mem_tlb_uop)
   }
   .elsewhen (mem_fired_stdi)
   {
      val mem_std_uop = RegNext(io.exe_resp.bits.uop)
      clr_bsy_valid := saq_val(mem_std_uop.stq_idx) &&
                              !saq_is_virtual(mem_std_uop.stq_idx) &&
                              !mem_std_uop.is_amo &&
                              !IsKilledByBranch(io.brinfo, mem_std_uop) &&
                              !io.exception && !RegNext(io.exception)
      clr_bsy_robidx := mem_std_uop.rob_idx
      clr_bsy_brmask := GetNewBrMask(io.brinfo, mem_std_uop)
   }
   .elsewhen (mem_fired_sfence)
   {
      clr_bsy_valid := true.B
      clr_bsy_robidx := mem_tlb_uop.rob_idx
      clr_bsy_brmask := GetNewBrMask(io.brinfo, mem_tlb_uop)
   }

   val mem_uop_stdf = RegNext(io.fp_stdata.bits.uop)
   val stdf_clr_bsy_valid = RegNext(mem_fired_stdf &&
                           saq_val(mem_uop_stdf.stq_idx) &&
                           !saq_is_virtual(mem_uop_stdf.stq_idx) &&
                           !mem_uop_stdf.is_amo &&
                           !IsKilledByBranch(io.brinfo, mem_uop_stdf)) &&
                           !io.exception && !RegNext(io.exception)
   val stdf_clr_bsy_robidx = RegEnable(mem_uop_stdf.rob_idx, mem_fired_stdf)
   val stdf_clr_bsy_brmask = RegEnable(GetNewBrMask(io.brinfo, mem_uop_stdf), mem_fired_stdf)

   io.clr_bsy_valid(0)   := clr_bsy_valid && !io.exception && !IsKilledByBranch(io.brinfo, clr_bsy_brmask)
   io.clr_bsy_rob_idx(0) := clr_bsy_robidx
   io.clr_bsy_valid(1)   := stdf_clr_bsy_valid && !io.exception && !IsKilledByBranch(io.brinfo, stdf_clr_bsy_brmask)
   io.clr_bsy_rob_idx(1) := stdf_clr_bsy_robidx

   //-------------------------------------------------------------
   // Load Issue Datapath (ALL loads need to use this path,
   //    to handle forwarding from the STORE QUEUE, etc.)
   // search entire STORE QUEUE for match on load
   //-------------------------------------------------------------
   // does the incoming load match any store addresses?
   // NOTE: these are fully translated physical addresses, as
   // forwarding requires a full address check.

   val read_mask = GenByteMask(mem_ld_addr, mem_ld_uop.mem_size)
   val st_dep_mask = laq_st_dep_mask(RegNext(exe_ld_uop.ldq_idx))

   // do the double-word addr match? (doesn't necessarily mean a conflict or forward)
   val dword_addr_matches = Wire(Vec(NUM_STQ_ENTRIES, Bool()))
   // if there is some overlap on the bytes, you may need to put to sleep the load
   // (either data not ready, or not a perfect match between addr and type)
   val ldst_addr_conflicts   = Wire(Vec(NUM_STQ_ENTRIES, Bool()))
   // a full address match
   val forwarding_matches  = Wire(Vec(NUM_STQ_ENTRIES, Bool()))

   val force_ld_to_sleep = Wire(Bool())
   force_ld_to_sleep := false.B

   // do the load and store memory types match (aka, B == BU, H == HU, W == WU)
   def MemTypesMatch(typ_1: UInt, typ_2: UInt) = typ_1(1,0) === typ_2(1,0)

   // TODO totally refactor how conflict/forwarding logic is generated
   for (i <- 0 until NUM_STQ_ENTRIES)
   {
      val s_addr = saq_addr(i)

      dword_addr_matches(i) := false.B

      when (stq_allocated(i) &&
            st_dep_mask(i) &&
            saq_val(i) &&
            !mem_ld_uncacheable &&
            !saq_is_virtual(i) &&
            (s_addr(corePAddrBits-1,3) === mem_ld_addr(corePAddrBits-1,3)))
      {
         dword_addr_matches(i) := true.B
      }

      // check the lower-order bits for overlap/conflicts and matches
      ldst_addr_conflicts(i) := false.B
      val write_mask = GenByteMask(s_addr, stq_uop(i).mem_size)

      // if overlap on bytes and dword matches, the address conflicts!
      when (((read_mask & write_mask) =/= 0.U) && dword_addr_matches(i))
      {
         ldst_addr_conflicts(i) := true.B
      }
      // fences/flushes are treated as stores that touch all addresses
      .elsewhen (stq_allocated(i) &&
                  st_dep_mask(i) &&
                  stq_uop(i).is_fence)
      {
         ldst_addr_conflicts(i) := true.B
      }

      // exact match on masks? we can forward the data, if data is also present!
      // TODO PERF we can be fancier perhaps, like (r_mask & w_mask === r_mask)
      forwarding_matches(i) := false.B
      when ((read_mask === write_mask) &&
            !(stq_uop(i).is_fence) &&
            dword_addr_matches(i))
      {
         forwarding_matches(i) := true.B
      }

      // did a load see a conflicting store (sb->lw) or a fence/AMO? if so, put the load to sleep
      // TODO this shuts down all loads so long as there is a store live in the dependent mask
      when ((stq_allocated(i) &&
               st_dep_mask(i) &&
               (stq_uop(i).is_fence || stq_uop(i).is_amo)) ||
            (dword_addr_matches(i) &&
               (mem_ld_uop.mem_size =/= stq_uop(i).mem_size) &&
               ((read_mask & write_mask) =/= 0.U)))
      {
         force_ld_to_sleep := true.B
      }
   }

   val forwarding_age_logic = Module(new ForwardingAgeLogic(NUM_STQ_ENTRIES))
   forwarding_age_logic.io.addr_matches    := forwarding_matches.asUInt()
   forwarding_age_logic.io.youngest_st_idx := laq_uop(RegNext(exe_ld_uop.ldq_idx)).stq_idx

   when (mem_fired_ld && forwarding_age_logic.io.forwarding_val && !tlb_miss)
   {
      laq_forwarded_std_val(mem_ld_uop.ldq_idx) := true.B
      laq_forwarded_stq_idx(mem_ld_uop.ldq_idx) := forwarding_age_logic.io.forwarding_idx
   }

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // Writeback Cycle (St->Ld Forwarding Path)
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   val wb_forward_std_val = RegInit(false.B)
   val wb_forward_std_idx = Reg(UInt())
   val wb_uop             = RegNext(mem_ld_uop)
   wb_uop.br_mask        := GetNewBrMask(io.brinfo, mem_ld_uop)

   val ldld_addr_conflict= WireInit(false.B)

   // Kill load request to mem if address matches (we will either sleep load, or forward data) or TLB miss.
   // Also kill load request if load address matches an older, unexecuted load.
   io.memreq_kill     := (mem_ld_used_tlb && (mem_tlb_miss || RegNext(pf_ld || ma_ld))) ||
                         (mem_fired_ld && ldst_addr_conflicts.asUInt=/= 0.U) ||
                         (mem_fired_ld && ldld_addr_conflict) ||
                         mem_ld_killed ||
                         (mem_fired_st && io.nack.valid && !io.nack.isload)
   wb_forward_std_idx := forwarding_age_logic.io.forwarding_idx

   // kill forwarding if branch mispredict
   when (IsKilledByBranch(io.brinfo, mem_ld_uop))
   {
      wb_forward_std_val := false.B
   }
   .otherwise
   {
      wb_forward_std_val := mem_fired_ld && forwarding_age_logic.io.forwarding_val &&
                           !force_ld_to_sleep && !(mem_tlb_miss && mem_ld_used_tlb) && !mem_ld_killed && !io.exception
   }

   // Notes:
   //    - Time the forwarding of the data to coincide with what would be a HIT
   //       from the cache (to only use one port).

   io.forward_val := false.B
   when (IsKilledByBranch(io.brinfo, wb_uop))
   {
      io.forward_val := false.B
   }
   .otherwise
   {
      io.forward_val := wb_forward_std_val &&
                        sdq_val(wb_forward_std_idx) &&
                        !(io.nack.valid && io.nack.cache_nack)
   }
   io.forward_data := LoadDataGenerator(sdq_data(wb_forward_std_idx).asUInt, wb_uop.mem_size, wb_uop.mem_signed)
   io.forward_uop  := wb_uop

   //------------------------
   // Handle Memory Responses
   //------------------------

   when (io.memresp.valid)
   {
      when (io.memresp.bits.is_load)
      {
         laq_succeeded(io.memresp.bits.ldq_idx) := true.B
      }
      .otherwise
      {
         stq_succeeded(io.memresp.bits.stq_idx) := true.B

         if (O3PIPEVIEW_PRINTF)
         {
            // TODO supress printing out a store-comp for lr instructions.
            printf("%d; store-comp: %d\n", io.memresp.bits.debug_events.fetch_seq, io.debug_tsc)
         }
      }
   }

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // Search LAQ for misspeculated load orderings
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // At Store (or Load) Execute (address generation)...
   //    Check the incoming store/load address against younger loads that have
   //    executed, looking for memory ordering failures. This check occurs the
   //    cycle after address generation and TLB lookup.

   // load queue CAM search
   val lcam_addr       = Mux(RegNext(will_fire_load_wakeup), mem_ld_addr, mem_tlb_paddr)
   val lcam_uop        = Mux(RegNext(will_fire_load_wakeup), RegNext(laq_uop(exe_ld_idx_wakeup)), mem_tlb_uop)
   val lcam_mask       = GenByteMask(lcam_addr, lcam_uop.mem_size)
   val lcam_is_fence   = lcam_uop.is_fence
   val lcam_ldq_idx    = lcam_uop.ldq_idx
   val stq_idx         = lcam_uop.stq_idx
   val failed_loads    = Wire(Vec(NUM_LDQ_ENTRIES, Bool()))
   val stld_order_fail = WireInit(false.B)
   val ldld_order_fail = WireInit(false.B)

   val do_stld_search = RegNext((will_fire_sta_incoming ||
                                    will_fire_sta_retry),
                            init = false.B)
   val do_ldld_search = RegNext((will_fire_load_incoming ||
                                   will_fire_load_retry ||
                                   will_fire_load_wakeup),
                            init = false.B) && MCM_ORDER_DEPENDENT_LOADS.B
   assert (!(do_stld_search && do_ldld_search), "[lsu]: contention on LAQ CAM search.")

   dontTouch(io.release)

   for (i <- 0 until NUM_LDQ_ENTRIES)
   {
      val l_addr      = laq_addr(i)
      val l_mask      = GenByteMask(l_addr, laq_uop(i).mem_size)
      val l_allocated = laq_allocated(i)
      val l_addr_val  = laq_addr_val(i)
      val l_is_virtual= laq_is_virtual(i)
      val l_executed  = laq_executed(i)
      failed_loads(i) := false.B

      when (do_stld_search)
      {
         // does the load depend on this store?
         // TODO CODE REVIEW what's the best way to perform this bit extract?
         when ((laq_st_dep_mask(i) & (1.U << stq_idx)) =/= 0.U)
         {
            when (lcam_is_fence &&
                  l_allocated &&
                  l_addr_val &&
                  !l_is_virtual &&
                  l_executed)
            {
               // fences, flushes are like stores that hit all addresses
               laq_executed(i)   := false.B
               laq_failure(i)    := true.B
               laq_succeeded(i)  := false.B
               failed_loads(i)   := true.B
               stld_order_fail   := true.B
            }
            // NOTE: this address check doesn't necessarily have to be across all address bits
            .elsewhen ((lcam_addr(corePAddrBits-1,3) === l_addr(corePAddrBits-1,3)) &&
                  l_allocated &&
                  l_addr_val &&
                  !l_is_virtual &&
                  l_executed
                  )
            {
               val yid = laq_uop(i).stq_idx
               val fid = laq_forwarded_stq_idx(i)
               // double-words match, now check for conflict of byte masks,
               // then check if it was forwarded from us,
               // and if not, then fail OR
               // if it was forwarded but not us, was the forwarded store older than me
               // head < forwarded < youngest?
               when (((lcam_mask & l_mask) =/= 0.U) &&
                    (!laq_forwarded_std_val(i) ||
                      ((fid =/= stq_idx) && (Cat(stq_idx < yid, stq_idx) > Cat(fid < yid, fid)))))
               {
                  laq_executed(i)   := false.B
                  laq_failure(i)    := true.B
                  laq_succeeded(i)  := false.B
                  failed_loads(i)   := true.B
                  stld_order_fail   := true.B
               }
            }
         }
   }
      .elsewhen (do_ldld_search)
      {
         val searcher_is_older = IsOlder(lcam_ldq_idx, i.U, laq_head)

         // Does the load entry depend on the searching load?
         // Aka, is the searching load older than the load entry?
         // If yes, search for ordering failures.
         when (searcher_is_older)
         {
            when ((lcam_addr(corePAddrBits-1,3) === l_addr(corePAddrBits-1,3)) &&
               l_allocated &&
               l_addr_val &&
               !l_is_virtual &&
               l_executed &&
               ((lcam_mask & l_mask) =/= 0.U))
            {
               laq_executed(i)  := false.B
               laq_failure(i)   := true.B
               laq_succeeded(i) := false.B
               failed_loads(i)  := true.B
               ldld_order_fail  := true.B
            }
         }
         .elsewhen (lcam_ldq_idx =/= i.U)
         {
            // Searcher is newer and not itself, should the searching load be
            // put to sleep because of a potential ordering failure?
            when ((lcam_addr(corePAddrBits-1,3) === l_addr(corePAddrBits-1,3)) &&
               l_allocated &&
               l_addr_val &&
               !l_is_virtual &&
               !l_executed &&
               ((lcam_mask & l_mask) =/= 0.U))
            {
               // Put younger load to sleep -- otherwise an order failure will occur.
               ldld_addr_conflict := true.B
            }
         }
      }
   }

   // detect which loads get marked as failures, but broadcast to the ROB the oldest failing load
   // TODO encapsulate this in an age-based  priority-encoder
//   val l_idx = AgePriorityEncoder((Vec(Vec.tabulate(NUM_LDQ_ENTRIES)(i => failed_loads(i) && i.U >= laq_head)
//   ++ failed_loads)).asUInt)
   val temp_bits = (VecInit(VecInit.tabulate(NUM_LDQ_ENTRIES)(i =>
      failed_loads(i) && i.U >= laq_head) ++ failed_loads)).asUInt
   val l_idx = PriorityEncoder(temp_bits)

   // TODO always pad out the input to PECircular() to pow2
   // convert it to vec[bool], then in.padTo(1 << log2Ceil(in.size), false.B)

   // one exception port, but multiple causes!
   // - 1) the incoming store-address finds a faulting load (it is by definition younger)
   // - 2) the incoming load or store address is excepting. It must be older and thus takes precedent.
   val r_xcpt_valid = RegInit(false.B)
   val r_xcpt = Reg(new Exception)

   val mem_xcpt_uop = Mux(mem_xcpt_valid,
                        mem_tlb_uop,
                        laq_uop(Mux(l_idx >= NUM_LDQ_ENTRIES.U, l_idx - NUM_LDQ_ENTRIES.U, l_idx)))
   r_xcpt_valid := (failed_loads.reduce(_|_) || mem_xcpt_valid) &&
                   !io.exception &&
                   !IsKilledByBranch(io.brinfo, mem_xcpt_uop)
   r_xcpt.uop := mem_xcpt_uop
   r_xcpt.uop.br_mask := GetNewBrMask(io.brinfo, mem_xcpt_uop)
   r_xcpt.cause := Mux(mem_xcpt_valid, mem_xcpt_cause, MINI_EXCEPTION_MEM_ORDERING)
   r_xcpt.badvaddr := RegNext(exe_vaddr) // TODO is there another register we can use instead?

   io.xcpt.valid := r_xcpt_valid && !io.exception && !IsKilledByBranch(io.brinfo, r_xcpt.uop)
   io.xcpt.bits := r_xcpt

   //-------------------------------------------------------------
   // Kill speculated entries on branch mispredict
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   val st_brkilled_mask = Wire(Vec(NUM_STQ_ENTRIES, Bool()))
   for (i <- 0 until NUM_STQ_ENTRIES)
   {
      st_brkilled_mask(i) := false.B

      when (stq_allocated(i))
      {
         stq_uop(i).br_mask := GetNewBrMask(io.brinfo, stq_uop(i))

         when (IsKilledByBranch(io.brinfo, stq_uop(i)))
         {
            stq_allocated(i)   := false.B
            saq_val(i)         := false.B
            sdq_val(i)         := false.B
            stq_uop(i).br_mask := 0.U
            st_brkilled_mask(i):= true.B
         }
      }

      assert (!(IsKilledByBranch(io.brinfo, stq_uop(i)) && stq_allocated(i) && stq_committed(i)),
         "Branch is trying to clear a committed store.")
   }

   //-------------------------------------------------------------
   // Kill speculated entries on branch mispredict
   for (i <- 0 until NUM_LDQ_ENTRIES)
   {
      when(laq_allocated(i))
      {
         laq_uop(i).br_mask := GetNewBrMask(io.brinfo, laq_uop(i))
         when (IsKilledByBranch(io.brinfo, laq_uop(i)))
         {
            laq_allocated(i)   := false.B
            laq_addr_val(i)    := false.B
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
         stq_committed(temp_stq_commit_head) := true.B
      }

      temp_stq_commit_head = Mux(io.commit_store_mask(w),
                                 WrapInc(temp_stq_commit_head, NUM_STQ_ENTRIES),
                                 temp_stq_commit_head)
   }

   stq_commit_head := temp_stq_commit_head

   // store has been committed AND successfully sent data to memory
   when (stq_allocated(stq_head) && stq_committed(stq_head))
   {
      clear_store := Mux(stq_uop(stq_head).is_fence, io.dmem_is_ordered,
                                                     stq_succeeded(stq_head))
   }

   when (clear_store)
   {
      stq_allocated(stq_head)   := false.B
      saq_val(stq_head)         := false.B
      sdq_val(stq_head)         := false.B
      stq_executed(stq_head)    := false.B
      stq_succeeded(stq_head)   := false.B
      stq_committed(stq_head)   := false.B

      stq_head := WrapInc(stq_head, NUM_STQ_ENTRIES)
      when (stq_uop(stq_head).is_fence)
      {
         stq_execute_head := WrapInc(stq_execute_head, NUM_STQ_ENTRIES)
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

         laq_allocated(idx)         := false.B
         laq_addr_val (idx)         := false.B
         laq_executed (idx)         := false.B
         laq_succeeded(idx)         := false.B
         laq_failure  (idx)         := false.B
         laq_forwarded_std_val(idx) := false.B
      }

      temp_laq_head = Mux(io.commit_load_mask(w), WrapInc(temp_laq_head, NUM_LDQ_ENTRIES), temp_laq_head)
   }
   laq_head := temp_laq_head


   //-------------------------------------------------------------
   // Handle Nacks
   // the data cache may nack our requests, requiring us to resend our request,
   // the forwarding logic (from the STD) may be "nacking" us, in which case,
   // we ignore the nack (the nack is for the D$, not the LSU).

   val clr_ld = Wire(Bool())
   clr_ld := false.B

   // did the load execute, but was then killed/nacked (will overcount)?
   val ld_was_killed       = Wire(Bool())
   // did the load execute, but was then killed/nacked (only high once per load)?
   val ld_was_put_to_sleep = Wire(Bool())
   ld_was_killed           := false.B
   ld_was_put_to_sleep     := false.B

   when (io.nack.valid)
   {
      // the cache nacked our store
      when (!io.nack.isload)
      {
         stq_executed(io.nack.lsu_idx) := false.B
         when (IsOlder(io.nack.lsu_idx, stq_execute_head, stq_head) || stq_executed.reduce(_&&_))
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
               clr_ld := true.B
            }
         }
         .otherwise
         {
            clr_ld := true.B
         }

         when (clr_ld)
         {
            laq_executed(io.nack.lsu_idx) := false.B
            debug_laq_put_to_sleep(io.nack.lsu_idx) := true.B
            ld_was_killed := true.B
            ld_was_put_to_sleep := !debug_laq_put_to_sleep(io.nack.lsu_idx)
            laq_forwarded_std_val(io.nack.lsu_idx) := false.B
         }
      }
   }

   //-------------------------------------------------------------
   // Exception / Reset

   // for the live_store_mask, need to kill stores that haven't been committed
   val st_exc_killed_mask = Wire(Vec(NUM_STQ_ENTRIES, Bool()))
   (0 until NUM_STQ_ENTRIES).map(i => st_exc_killed_mask(i) := false.B)

   val null_uop = NullMicroOp

   when (reset.asBool || io.exception)
   {
      laq_head := 0.U(LDQ_ADDR_SZ.W)
      laq_tail := 0.U(LDQ_ADDR_SZ.W)

      when (reset.asBool)
      {
         stq_head := 0.U(STQ_ADDR_SZ.W)
         stq_tail := 0.U(STQ_ADDR_SZ.W)
         stq_commit_head := 0.U(STQ_ADDR_SZ.W)
         stq_execute_head := 0.U(STQ_ADDR_SZ.W)

         for (i <- 0 until NUM_STQ_ENTRIES)
         {
            saq_val(i)         := false.B
            sdq_val(i)         := false.B
            stq_allocated(i)   := false.B
         }
         for (i <- 0 until NUM_STQ_ENTRIES)
         {
            stq_uop(i) := null_uop
         }
      }
      .otherwise // exception
      {
         stq_tail := stq_commit_head

         for (i <- 0 until NUM_STQ_ENTRIES)
         {
            when (!stq_committed(i))
            {
               saq_val(i)            := false.B
               sdq_val(i)            := false.B
               stq_allocated(i)      := false.B
               st_exc_killed_mask(i) := true.B
            }
         }
      }

      for (i <- 0 until NUM_LDQ_ENTRIES)
      {
         laq_addr_val(i)    := false.B
         laq_allocated(i)   := false.B
         laq_executed(i)    := false.B
      }

   }

   //-------------------------------------------------------------
   // Live Store Mask
   // track a bit-array of stores that are alive
   // (could maybe be re-produced from the stq_head/stq_tail, but need to know include spec_killed entries)

   // TODO is this the most efficient way to compute the live store mask?
   live_store_mask := next_live_store_mask &
                        ~(st_brkilled_mask.asUInt) &
                        ~(st_exc_killed_mask.asUInt)

   //-------------------------------------------------------------
   // Debug & Counter outputs

   io.counters.ld_valid        := RegNext(io.exe_resp.valid && io.exe_resp.bits.uop.is_load)
   io.counters.ld_forwarded    := RegNext(io.forward_val)
   io.counters.ld_sleep        := RegNext(ld_was_put_to_sleep)
   io.counters.ld_killed       := RegNext(ld_was_killed)
   io.counters.stld_order_fail := RegNext(stld_order_fail)
   io.counters.ldld_order_fail := RegNext(ldld_order_fail)

   if (DEBUG_PRINTF_LSU)
   {
      printf("LSU:\n")
      printf("    WakeupIdx:%d LDatROBHead:%c\n",
         exe_ld_idx_wakeup,
         BoolToChar(io.commit_load_at_rob_head, 'V'))
      for (i <- 0 until NUM_LDQ_ENTRIES)
      {
         val t_laddr = laq_addr(i)
         printf("    LDQ[%d]: State:(%c%c%c%c%c%c%c%d) STDep:(StqIdx:%d,Msk:%x) Addr:0x%x H,T:(%c %c)\n",
            i.U(LDQ_ADDR_SZ.W),
            BoolToChar(        laq_allocated(i), 'V'),
            BoolToChar(         laq_addr_val(i), 'A'),
            BoolToChar(         laq_executed(i), 'E'),
            BoolToChar(        laq_succeeded(i), 'S'),
            BoolToChar(          laq_failure(i), 'F'),
            BoolToChar(   laq_is_uncacheable(i), 'U'),
            BoolToChar(laq_forwarded_std_val(i), 'X'),
            laq_forwarded_stq_idx(i),
            laq_uop(i).stq_idx, // youngest dep-store
            laq_st_dep_mask(i),
            t_laddr(19,0),
            BoolToChar(laq_head === i.U, 'H', ' '),
            BoolToChar(laq_tail===  i.U, 'T', ' '))
      }
      for (i <- 0 until NUM_STQ_ENTRIES) {
         val t_saddr = saq_addr(i)
         printf("    SAQ[%d]: State:(%c%c%c%c%c%c%c) BMsk:0x%x (Addr:0x%x -> Data:0x%x) H,ExH,CmH,T:(%c %c %c %c)\n",
            i.U(STQ_ADDR_SZ.W),
            BoolToChar( stq_allocated(i), 'V'),
            BoolToChar(       saq_val(i), 'A'),
            BoolToChar(       sdq_val(i), 'D'),
            BoolToChar( stq_committed(i), 'C'),
            BoolToChar(  stq_executed(i), 'E'),
            BoolToChar( stq_succeeded(i), 'S'),
            BoolToChar(saq_is_virtual(i), 'T'),
            stq_uop(i).br_mask,
            t_saddr(19,0),
            sdq_data(i),
            BoolToChar(        stq_head === i.U, 'H', ' '),
            BoolToChar(stq_execute_head === i.U, 'E', ' '),
            BoolToChar( stq_commit_head === i.U, 'C', ' '),
            BoolToChar(        stq_tail === i.U, 'T', ' '))
      }
   }
}

/**
 * Object to take an address and generate an 8-bit mask of which bytes within a
 * double-word.
 */
object GenByteMask
{
   def apply(addr: UInt, size: UInt): UInt =
   {
      val mask = Wire(UInt(8.W))
      mask := MuxCase(255.U(8.W), Array(
                   (size === 0.U) -> (1.U(8.W) << addr(2,0)),
                   (size === 1.U) -> (3.U(8.W) << (addr(2,1) << 1.U)),
                   (size === 2.U) -> Mux(addr(2), 240.U(8.W), 15.U(8.W)),
                   (size === 3.U) -> 255.U(8.W)))
      mask
   }
}

/**
 * Object to generate data based on the size of the data (according to
 * its type.
 * TODO currently assumes w_addr and r_addr are identical, so no shifting
 * store data is already aligned (since its the value straight from the register
 * but the load data may need to be re-aligned...
 */
object LoadDataGenerator
{
   def apply(data: UInt, mem_size: UInt, mem_signed: Bool): UInt =
   {
     val sext  = mem_signed
     val word  = mem_size === 2.U
     val half  = mem_size === 1.U
     val byte_ = mem_size === 0.U
     val dword = mem_size === 3.U

      val out = Mux (dword, data,
                Mux (word , Cat(Fill(32, sext & data(31)), data(31, 0)),
                Mux (half , Cat(Fill(48, sext & data(15)), data(15, 0)),
                Mux (byte_, Cat(Fill(56, sext & data( 7)), data( 7, 0)),
                            data))))
      out
   }
}

/**
 * ...
 */
class ForwardingAgeLogic(num_entries: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new Bundle
   {
      val addr_matches    = Input(UInt(num_entries.W)) // bit vector of addresses that match
                                                       // between the load and the SAQ
      val youngest_st_idx = Input(UInt(STQ_ADDR_SZ.W)) // needed to get "age"

      val forwarding_val  = Output(Bool())
      val forwarding_idx  = Output(UInt(STQ_ADDR_SZ.W))
   })

   // generating mask that zeroes out anything younger than tail
   val age_mask = Wire(Vec(num_entries, Bool()))
   for (i <- 0 until num_entries)
   {
      age_mask(i) := true.B
      when (i.U >= io.youngest_st_idx) // currently the tail points PAST last store, so use >=
      {
         age_mask(i) := false.B
      }
   }

   // Priority encoder with moving tail: double length
   val matches = Wire(UInt((2*num_entries).W))
   matches := Cat(io.addr_matches & age_mask.asUInt,
                  io.addr_matches)

   val found_match = Wire(Bool())
   found_match       := false.B
   io.forwarding_idx := 0.U

   // look for youngest, approach from the oldest side, let the last one found stick
   for (i <- 0 until (2*num_entries))
   {
      when (matches(i))
      {
         found_match := true.B
         io.forwarding_idx := (i % num_entries).U
      }
   }

   io.forwarding_val := found_match
}
