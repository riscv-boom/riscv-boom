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
  with HasBoomUOP
{
  val addr  = UInt(coreMaxAddrBits.W)
  val data  = Bits(coreDataBits.W)
  val is_hella = Bool() // Is this the hellacache req? If so this is not tracked in LDQ or STQ
}

class BoomDCacheResp(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomUOP
{
  val data = Bits(coreDataBits.W)
  val is_hella = Bool()
  val nack = Bool()
}

class LSUDMemIO(implicit p: Parameters) extends BoomBundle()(p)
{
  val req         = new DecoupledIO(new BoomDCacheReq)
  val s1_kill     = Output(Bool())
  val resp        = Flipped(new ValidIO(new BoomDCacheResp))

  val brinfo       = Output(new BrResolutionInfo)
  val exception    = Output(Bool())
  val rob_pnr_idx  = Output(UInt(robAddrSz.W))
  val rob_head_idx = Output(UInt(robAddrSz.W))

  val ordered     = Input(Bool())
}

class LSUCoreIO(implicit p: Parameters) extends BoomBundle()(p)
{
  val exe = new LSUExeIO

  val dec_uops    = Flipped(Vec(coreWidth, Valid(new MicroOp)))
  val dec_ldq_idx = Output(Vec(coreWidth, UInt(LDQ_ADDR_SZ.W)))
  val dec_stq_idx = Output(Vec(coreWidth, UInt(STQ_ADDR_SZ.W)))

  val ldq_full    = Output(Vec(coreWidth, Bool()))
  val stq_full    = Output(Vec(coreWidth, Bool()))

  val fp_stdata   = Flipped(Decoupled(new MicroOpWithData(fLen)))

  val ld_issued   = Valid(UInt(PREG_SZ.W))
  val ld_miss     = Output(Bool())

  val commit_store_mask = Input(Vec(coreWidth, Bool()))
  val commit_load_mask  = Input(Vec(coreWidth, Bool()))
  val commit_load_at_rob_head = Input(Bool())

  // Stores clear busy bit when stdata is received
  // 1 for int, 1 for fp (to avoid back-pressure fpstdat)
  val clr_bsy         = Output(Vec(2, Valid(UInt(robAddrSz.W))))

  // Speculatively safe load (barring memory ordering failure)
  val clr_unsafe      = Output(Valid(UInt(robAddrSz.W)))

  val brinfo       = Input(new BrResolutionInfo)
  val rob_pnr_idx  = Input(UInt(robAddrSz.W))
  val rob_head_idx = Input(UInt(robAddrSz.W))
  val exception    = Input(Bool())

  val fencei_rdy  = Output(Bool())

  val lxcpt       = Output(Valid(new Exception))
}

class LSUIO(implicit p: Parameters) extends BoomBundle()(p)
{
  val ptw   = new rocket.TLBPTWIO
  val core  = new LSUCoreIO
  val dmem  = new LSUDMemIO

  val hellacache = Flipped(new freechips.rocketchip.rocket.HellaCacheIO)
}

class LDQEntry(implicit p: Parameters) extends BoomBundle()(p)
{
  val uop                 = new MicroOp

  val addr                = Valid(UInt(coreMaxAddrBits.W))
  val addr_is_virtual     = Bool() // Virtual address, we got a TLB miss
  val addr_is_uncacheable = Bool() // Uncacheable, wait until head of ROB to execute

  val executed            = Bool() // load sent to memory, reset by NACKs
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

  val committed           = Bool() // committed by ROB
  val succeeded           = Bool() // D$ has ack'd this, we don't need to maintain this anymore
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

  val h_ready :: h_s1 :: h_s2 :: h_s2_nack :: h_wait :: h_replay :: h_dead :: Nil = Enum(7)
  // s1 : do TLB, if success and not killed, fire request go to h_s2
  //      store s1_data to register
  //      if tlb miss, go to s2_nack
  //      if don't get TLB, go to s2_nack
  //      store tlb xcpt
  // s2 : If kill, go to dead
  //      If tlb xcpt, send tlb xcpt, go to dead
  // s2_nack : send nack, go to dead
  // wait : wait for response, if nack, go to replay
  // replay : refire request, use already translated address
  // dead : wait for response, ignore it
  val hella_state           = RegInit(h_ready)
  val hella_req             = Reg(new rocket.HellaCacheReq)
  val hella_data            = Reg(new rocket.HellaCacheWriteData)
  val hella_paddr           = Reg(UInt(paddrBits.W))
  val hella_xcpt            = Reg(new rocket.HellaCacheExceptions)

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

    val dec_ld_val = io.core.dec_uops(w).valid && io.core.dec_uops(w).bits.uses_ldq

    when (dec_ld_val)
    {
      ldq(ld_enq_idx).valid            := true.B
      ldq(ld_enq_idx).bits.uop         := io.core.dec_uops(w).bits
      ldq(ld_enq_idx).bits.st_dep_mask := next_live_store_mask

      ldq(ld_enq_idx).bits.addr.valid  := false.B
      ldq(ld_enq_idx).bits.executed    := false.B
      ldq(ld_enq_idx).bits.order_fail  := false.B
      ldq(ld_enq_idx).bits.forward_std_val := false.B

      assert (ld_enq_idx === io.core.dec_uops(w).bits.ldq_idx, "[lsu] mismatch enq load tag.")
      assert (!ldq(ld_enq_idx).valid, "[lsu] Enqueuing uop is overwriting ldq entries")
    }
    ld_enq_idx = Mux(dec_ld_val, WrapInc(ld_enq_idx, NUM_LDQ_ENTRIES),
                                 ld_enq_idx)

    stq_full = st_enq_idx === stq_head && stq_nonempty
    io.core.stq_full(w)    := stq_full
    io.core.dec_stq_idx(w) := st_enq_idx

    val dec_st_val = io.core.dec_uops(w).valid && io.core.dec_uops(w).bits.uses_stq
    when (dec_st_val)
    {
      stq(st_enq_idx).valid           := true.B
      stq(st_enq_idx).bits.uop        := io.core.dec_uops(w).bits
      stq(st_enq_idx).bits.addr.valid := false.B
      stq(st_enq_idx).bits.data.valid := false.B
      stq(st_enq_idx).bits.committed  := false.B
      stq(st_enq_idx).bits.succeeded  := false.B

      assert (st_enq_idx === io.core.dec_uops(w).bits.stq_idx, "[lsu] mismatch enq store tag.")
      assert (!stq(st_enq_idx).valid, "[lsu] Enqueuing uop is overwriting stq entries")
    }
    next_live_store_mask = Mux(dec_st_val, next_live_store_mask | (1.U << st_enq_idx),
                                           next_live_store_mask)
    st_enq_idx = Mux(dec_st_val, WrapInc(st_enq_idx, NUM_STQ_ENTRIES),
                                 st_enq_idx)
  }

  ldq_tail := ld_enq_idx
  stq_tail := st_enq_idx

  io.core.fencei_rdy := !stq_nonempty && io.dmem.ordered



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
  val will_fire_hella_incoming= WireInit(false.B) // uses TLB, D$
  val will_fire_hella_wakeup  = WireInit(false.B) // uses      D$

  val can_fire_sta_retry      = WireInit(false.B)
  val can_fire_load_retry     = WireInit(false.B)
  val can_fire_store_commit   = WireInit(false.B)
  val can_fire_load_wakeup    = WireInit(false.B)
  val can_fire_hella_incoming = WireInit(false.B)
  val can_fire_hella_wakeup   = WireInit(false.B)

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
    when (can_fire_hella_incoming) {
      will_fire_hella_incoming := true.B
      dc_avail := false.B
    } .elsewhen (can_fire_sta_retry && rob_avail) {
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
    will_fire_load_wakeup  := !will_fire_store_commit && can_fire_load_wakeup && lcam_avail
    will_fire_hella_wakeup := !will_fire_store_commit && !will_fire_load_wakeup && can_fire_hella_wakeup // TODO: Should this be higher priority?
  }



  //--------------------------------------------
  // TLB Access

  val stq_retry_idx = WireInit(0.U(STQ_ADDR_SZ.W))
  val ldq_retry_idx = WireInit(0.U(LDQ_ADDR_SZ.W))

  val hella_sfence = Wire(Valid(new rocket.SFenceReq))
  hella_sfence.valid     := hella_req.cmd === rocket.M_SFENCE
  hella_sfence.bits.rs1  := hella_req.size(0)
  hella_sfence.bits.rs2  := hella_req.size(1)
  hella_sfence.bits.addr := hella_req.addr
  hella_sfence.bits.asid := io.hellacache.s1_data.data


  // micro-op going through the TLB generate paddr's. If this is a load, it will continue
  // to the D$ and search the SAQ. uopSTD also uses this uop.
  val exe_tlb_uop = Mux(will_fire_sta_retry,      stq(stq_retry_idx).bits.uop,
                    Mux(will_fire_load_retry,     ldq(ldq_retry_idx).bits.uop,
                    Mux(will_fire_hella_incoming, NullMicroOp,
                                                  exe_req.bits.uop)))

  val exe_vaddr   = Mux(will_fire_sta_retry,      stq(stq_retry_idx).bits.addr.bits,
                    Mux(will_fire_load_retry,     ldq(ldq_retry_idx).bits.addr.bits,
                    Mux(will_fire_sfence,         exe_req.bits.sfence.bits.addr,
                    Mux(will_fire_hella_incoming, hella_req.addr,
                                                  exe_req.bits.addr))))

  val exe_sfence  = Mux(will_fire_hella_incoming, hella_sfence,
                                                  exe_req.bits.sfence)

  val exe_size    = Mux(will_fire_hella_incoming, hella_req.size,
                                                  exe_tlb_uop.mem_size)

  val exe_cmd     = Mux(will_fire_hella_incoming, hella_req.cmd,
                                                  exe_tlb_uop.mem_cmd)

  val exe_passthr = Mux(will_fire_hella_incoming, hella_req.phys,
                                                  false.B) // let status.vm decide

  val exe_kill    = Mux(will_fire_hella_incoming, io.hellacache.s1_kill,
                                                  false.B)

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
                       will_fire_sfence ||
                       will_fire_hella_incoming
  dtlb.io.req.bits.vaddr       := exe_vaddr
  dtlb.io.req.bits.size        := exe_size
  dtlb.io.req.bits.cmd         := exe_cmd
  dtlb.io.req.bits.passthrough := exe_passthr
  dtlb.io.kill                 := exe_kill
  dtlb.io.sfence               := exe_sfence

  // exceptions
  val ma_ld = will_fire_load_incoming && exe_req.bits.mxcpt.valid // We get ma_ld in memaddrcalc
  val ma_st = will_fire_sta_incoming && exe_req.bits.mxcpt.valid // We get ma_ld in memaddrcalc
  val pf_ld = dtlb.io.req.valid && dtlb.io.resp.pf.ld && (exe_tlb_uop.uses_ldq || exe_tlb_uop.is_amo) // TODO: uses_ldq is not right here
  val pf_st = dtlb.io.req.valid && dtlb.io.resp.pf.st && exe_tlb_uop.uses_stq
  val ae_ld = dtlb.io.req.valid && dtlb.io.resp.ae.ld && (exe_tlb_uop.uses_ldq|| exe_tlb_uop.is_amo)
  val ae_st = dtlb.io.req.valid && dtlb.io.resp.ae.st && exe_tlb_uop.uses_stq

  // TODO check for xcpt_if and verify that never happens on non-speculative instructions.
  val mem_xcpt_valid = RegNext((pf_ld || pf_st || ae_ld || ae_st || ma_ld || ma_st) &&
                                !io.core.exception &&
                                !IsKilledByBranch(io.core.brinfo, exe_tlb_uop),
                                init=false.B)

  val xcpt_uop       = RegNext(exe_tlb_uop)

  when (mem_xcpt_valid)
  {
    // Technically only faulting AMOs need this
    assert(xcpt_uop.uses_ldq ^ xcpt_uop.uses_stq)
    when (xcpt_uop.uses_ldq)
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
  io.dmem.brinfo         := io.core.brinfo
  io.dmem.exception      := io.core.exception
  io.dmem.rob_head_idx   := io.core.rob_head_idx
  io.dmem.rob_pnr_idx    := io.core.rob_pnr_idx

  io.dmem.req.valid         := false.B
  io.dmem.req.bits.uop      := NullMicroOp
  io.dmem.req.bits.addr     := 0.U
  io.dmem.req.bits.data     := 0.U
  io.dmem.req.bits.is_hella := false.B

  io.dmem.s1_kill           := false.B

  val mem_fired_st  = RegInit(false.B)
  mem_fired_st := false.B
  when (will_fire_store_commit) {
    io.dmem.req.valid         := true.B
    io.dmem.req.bits.addr     := stq(stq_execute_head).bits.addr.bits
    io.dmem.req.bits.data     := (new freechips.rocketchip.rocket.StoreGen(
      stq(stq_execute_head).bits.uop.mem_size, 0.U,
      stq(stq_execute_head).bits.data.bits,
      coreDataBytes)).data
    io.dmem.req.bits.uop      := stq(stq_execute_head).bits.uop

    // TODO Nacks
    stq_execute_head                    := Mux(io.dmem.req.fire(),
                                               WrapInc(stq_execute_head, NUM_STQ_ENTRIES),
                                               stq_execute_head)
    mem_fired_st                        := io.dmem.req.fire()

    stq(stq_execute_head).bits.succeeded := false.B
  }
    .elsewhen (will_fire_load_incoming || will_fire_load_retry)
  {
    // Uncacheable loads can't be sent until at head of ROB
    io.dmem.req.valid         := !tlb_miss && !tlb_addr_uncacheable
    io.dmem.req.bits.addr     := exe_tlb_paddr
    io.dmem.req.bits.uop      := exe_tlb_uop

    ldq(exe_ldq_idx).bits.executed  := io.dmem.req.fire()

    assert(!(will_fire_load_incoming && ldq(exe_ldq_idx).bits.executed),
      "[lsu] We are firing an incoming load, but the LDQ marks it as executed?")
    assert(!(will_fire_load_retry    && ldq(exe_ldq_idx).bits.executed),
      "[lsu] We are retrying a TLB missed load, but the LDQ marks it as executed?")
  }
    .elsewhen (will_fire_load_wakeup)
  {
    // TODO: We shouldn't fire this if we will ignore the response with forward-data
    io.dmem.req.valid      := true.B
    io.dmem.req.bits.addr  := ldq(exe_ldq_idx).bits.addr.bits
    io.dmem.req.bits.uop   := ldq(exe_ldq_idx).bits.uop

    ldq(exe_ldq_idx).bits.executed := io.dmem.req.fire()

    assert(!(will_fire_load_wakeup && ldq(exe_ldq_idx).bits.executed),
      "[lsu] We are firing a load that the D$ rejected, but the LDQ marks it as executed?")
    assert(!(will_fire_load_wakeup && ldq(exe_ldq_idx).bits.addr_is_virtual),
      "[lsu] We are firing a load that the D$ rejected, but the address is still virtual?")

  }
    .elsewhen (will_fire_hella_incoming)
  {
    assert(hella_state === h_s1)
    io.dmem.req.valid               := !io.hellacache.s1_kill && (!tlb_miss || hella_req.phys)
    io.dmem.req.bits.addr           := exe_tlb_paddr
    io.dmem.req.bits.data           := (new freechips.rocketchip.rocket.StoreGen(
      hella_req.size, 0.U,
      io.hellacache.s1_data.data,
      coreDataBytes)).data
    io.dmem.req.bits.uop.mem_cmd    := hella_req.cmd
    io.dmem.req.bits.uop.mem_size   := hella_req.size
    io.dmem.req.bits.uop.mem_signed := hella_req.signed
    io.dmem.req.bits.is_hella       := true.B

    hella_paddr := exe_tlb_paddr
  }
    .elsewhen (will_fire_hella_wakeup)
  {
    assert(hella_state === h_replay)
    io.dmem.req.valid               := true.B
    io.dmem.req.bits.addr           := hella_paddr
    io.dmem.req.bits.data           := (new freechips.rocketchip.rocket.StoreGen(
      hella_req.size, 0.U,
      hella_data.data,
      coreDataBytes)).data
    io.dmem.req.bits.uop.mem_cmd    := hella_req.cmd
    io.dmem.req.bits.uop.mem_size   := hella_req.size
    io.dmem.req.bits.uop.mem_signed := hella_req.signed
    io.dmem.req.bits.is_hella       := true.B
  }

  assert (PopCount(VecInit(will_fire_store_commit, will_fire_load_incoming,
                           will_fire_load_retry, will_fire_load_wakeup))
    <= 1.U, "Multiple requestors firing to the data cache.")

  //-------------------------------------------------------------
  // Write Addr into the LAQ/SAQ
  when (will_fire_load_incoming || will_fire_load_retry)
  {
    ldq(exe_ldq_idx).bits.addr.valid          := true.B
    ldq(exe_ldq_idx).bits.addr.bits           := Mux(tlb_miss, exe_vaddr, exe_tlb_paddr)
    ldq(exe_ldq_idx).bits.uop.pdst            := exe_tlb_uop.pdst
    ldq(exe_ldq_idx).bits.addr_is_virtual     := tlb_miss
    ldq(exe_ldq_idx).bits.addr_is_uncacheable := tlb_addr_uncacheable && !tlb_miss

    assert(!(will_fire_load_incoming && ldq(exe_ldq_idx).bits.addr.valid),
      "[lsu] Incoming load is overwriting a valid address")
  }

  when (will_fire_sta_incoming || will_fire_sta_retry)
  {
    stq(exe_tlb_uop.stq_idx).bits.addr.valid := !pf_st // Prevent AMOs from executing!
    stq(exe_tlb_uop.stq_idx).bits.addr.bits  := Mux(tlb_miss, exe_vaddr, exe_tlb_paddr)
    stq(exe_tlb_uop.stq_idx).bits.uop.pdst   := exe_tlb_uop.pdst // Needed for AMOs
    stq(exe_tlb_uop.stq_idx).bits.addr_is_virtual := tlb_miss

    assert(!(will_fire_sta_incoming && stq(exe_tlb_uop.stq_idx).bits.addr.valid),
      "[lsu] Incoming store is overwriting a valid address")

  }

  //-------------------------------------------------------------
  // Write data into the STQ
  io.core.fp_stdata.ready := !will_fire_std_incoming
  when (will_fire_std_incoming || io.core.fp_stdata.fire())
  {
    val sidx = Mux(will_fire_std_incoming, exe_req.bits.uop.stq_idx,
                                           io.core.fp_stdata.bits.uop.stq_idx)
    stq(sidx).bits.data.valid := true.B
    stq(sidx).bits.data.bits  := Mux(will_fire_std_incoming, exe_req.bits.data,
                                                             io.core.fp_stdata.bits.data)

    assert(!(stq(sidx).bits.data.valid),
      "[lsu] Incoming store is overwriting a valid data entry")
  }
  require (xLen >= fLen) // for correct SDQ size

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Cache Access Cycle (Mem)
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // search SAQ for matches
  val mem_tlb_paddr       = RegNext(exe_tlb_paddr)
  val mem_tlb_uop         = RegNext(exe_tlb_uop) // not valid for std_incoming
  mem_tlb_uop.br_mask    := GetNewBrMask(io.core.brinfo, exe_tlb_uop)
  val mem_tlb_miss        = RegNext(tlb_miss, init=false.B)
  val mem_tlb_uncacheable = RegNext(tlb_addr_uncacheable, init=false.B)
  val mem_ld_used_tlb     = RegNext(will_fire_load_incoming || will_fire_load_retry)

  val mem_ld_addr         = RegNext(io.dmem.req.bits.addr) // This should be right?
  val mem_ld_uop          = RegNext(io.dmem.req.bits.uop)
  mem_ld_uop.br_mask     := GetNewBrMask(io.core.brinfo, io.dmem.req.bits.uop)

  val mem_sta_addr        = mem_tlb_paddr
  val mem_sta_uop         = mem_tlb_uop

  val mem_fired_ld        = RegNext((will_fire_load_incoming || will_fire_load_retry || will_fire_load_wakeup) &&
                                    io.dmem.req.fire(),
                                    init=false.B)
  val mem_fired_sta       = RegNext(will_fire_sta_incoming || will_fire_sta_retry,
                                    init=false.B)
  val mem_fired_stdi      = RegNext(will_fire_std_incoming,
                                    init=false.B)
  val mem_fired_stdf      = RegNext(io.core.fp_stdata.fire(),
                                    init=false.B)
  val mem_fired_sfence    = RegNext(will_fire_sfence,
                                    init=false.B)
  val mem_fired_hella     = RegNext(will_fire_hella_incoming || will_fire_hella_wakeup,
                                    init=false.B)

  // TODO: handle mem load killed, ldspecwakeup

  // tell the ROB to clear the busy bit on the incoming store
  val clr_bsy_valid   = RegInit(false.B)
  val clr_bsy_rob_idx = Reg(UInt(robAddrSz.W))
  val clr_bsy_brmask  = Reg(UInt(MAX_BR_COUNT.W))
  clr_bsy_valid    := false.B
  clr_bsy_rob_idx  := 0.U
  clr_bsy_brmask   := 0.U

  when (mem_fired_sta && !mem_tlb_miss && mem_fired_stdi)
  {
    clr_bsy_valid   := !mem_tlb_uop.is_amo &&
                       !IsKilledByBranch(io.core.brinfo, mem_tlb_uop) &&
                       !io.core.exception &&
                       !RegNext(io.core.exception)
    clr_bsy_rob_idx := mem_tlb_uop.rob_idx
    clr_bsy_brmask  := GetNewBrMask(io.core.brinfo, mem_tlb_uop)
  }
  .elsewhen (mem_fired_sta && !mem_tlb_miss)
  {
    clr_bsy_valid   := stq(mem_tlb_uop.stq_idx).bits.data.valid &&
                       !mem_tlb_uop.is_amo &&
                       !IsKilledByBranch(io.core.brinfo, mem_tlb_uop) &&
                       !io.core.exception &&
                       !RegNext(io.core.exception)
    clr_bsy_rob_idx := mem_tlb_uop.rob_idx
    clr_bsy_brmask  := GetNewBrMask(io.core.brinfo, mem_tlb_uop)
  }
  .elsewhen (mem_fired_stdi)
  {
    val mem_std_uop = RegNext(io.core.exe.req.bits.uop)
    clr_bsy_valid   := stq(mem_std_uop.stq_idx).bits.addr.valid &&
                       !stq(mem_std_uop.stq_idx).bits.addr_is_virtual &&
                       !mem_std_uop.is_amo &&
                       !IsKilledByBranch(io.core.brinfo, mem_std_uop) &&
                       !io.core.exception &&
                       !RegNext(io.core.exception)
    clr_bsy_rob_idx := mem_std_uop.rob_idx
    clr_bsy_brmask  := GetNewBrMask(io.core.brinfo, mem_std_uop)
  }
  .elsewhen (mem_fired_sfence)
  {
    clr_bsy_valid   := true.B
    clr_bsy_rob_idx := mem_tlb_uop.rob_idx
    clr_bsy_brmask  := GetNewBrMask(io.core.brinfo, mem_tlb_uop)
  }

  val mem_uop_stdf = RegNext(io.core.fp_stdata.bits.uop)
  val stdf_clr_bsy_valid = RegNext( mem_fired_stdf &&
                                    stq(mem_uop_stdf.stq_idx).valid &&
                                   !stq(mem_uop_stdf.stq_idx).bits.addr_is_virtual &&
                                   !mem_uop_stdf.is_amo &&
                                   !IsKilledByBranch(io.core.brinfo, mem_uop_stdf)) &&
                           (!io.core.exception && RegNext(!io.core.exception))
  val stdf_clr_bsy_rob_idx = RegNext(mem_uop_stdf.rob_idx)
  val stdf_clr_bsy_brmask  = RegNext(GetNewBrMask(io.core.brinfo, mem_uop_stdf))

  io.core.clr_bsy(0).valid := clr_bsy_valid &&
                                      !io.core.exception &&
                                      !IsKilledByBranch(io.core.brinfo, clr_bsy_brmask)
  io.core.clr_bsy(0).bits  := clr_bsy_rob_idx
  io.core.clr_bsy(1).valid := stdf_clr_bsy_valid &&
                                      !io.core.exception &&
                                      !IsKilledByBranch(io.core.brinfo, stdf_clr_bsy_brmask)
  io.core.clr_bsy(1).bits  := stdf_clr_bsy_rob_idx


  // Mark instructions as safe after successful address translation.
  // Need to delay to same cycle as exception broadcast into ROB to avoid
  // the PNR 'jumping the gun' over a misspeculated ordering.
  io.core.clr_unsafe.valid   := RegNext(!mem_tlb_miss && (mem_ld_used_tlb || mem_fired_sta))
  io.core.clr_unsafe.bits    := RegNext(mem_tlb_uop.rob_idx)

  //-------------------------------------------------------------
  // Load Issue Datapath (ALL loads need to use this path,
  //    to handle forwarding from the STORE QUEUE, etc.)
  // search entire STORE QUEUE for match on load
  //-------------------------------------------------------------

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Search LAQ/STQ for misspeculated load orderings and possible forwarding
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // At Store (or Load) Execute (address generation)...
  //    Check the incoming store/load address against younger loads that have
  //    executed, looking for memory ordering failures. This check occurs the
  //    cycle after address generation and TLB lookup.

  // Things that can happen here
  // - Firing load sees older store
  //  - Address match completely, mark load as forwarded, kill load req, mark load un-executed
  //  - Addresses match partially, kill load req, mark load un-executed
  // - Firing load sees older load
  //  - Older load has not fired, kill load req, mark load un-executed
  // - Firing load sees younger load
  //  - Younger load has fired, mark younger load as order fail

  // load queue CAM search
  val do_st_search = mem_fired_sta
  val do_ld_search = mem_fired_ld && MCM_ORDER_DEPENDENT_LOADS.B

  val lcam_addr        = Mux(do_st_search, mem_sta_addr, mem_ld_addr)
  val lcam_uop         = Mux(do_st_search, mem_sta_uop , mem_ld_uop)
  val lcam_mask        = GenByteMask(lcam_addr, lcam_uop.mem_size)
  val lcam_st_dep_mask = ldq(mem_ld_uop.ldq_idx).bits.st_dep_mask
  val lcam_is_fence    = lcam_uop.is_fence
  val lcam_ldq_idx     = lcam_uop.ldq_idx
  val lcam_stq_idx     = lcam_uop.stq_idx
  val failed_loads     = Wire(Vec(NUM_LDQ_ENTRIES, Bool()))


  val ldst_forward          = WireInit(false.B)
  val ldst_forward_matches  = WireInit(VecInit((0 until NUM_STQ_ENTRIES).map(x=>false.B))) // What can we forward from?
  val stld_order_fail       = WireInit(false.B) // Need to mini-xcpt
  val ldld_order_fail       = WireInit(false.B) // Need to mini-xcpt



  require(MCM_ORDER_DEPENDENT_LOADS)
  assert (!(do_st_search && do_ld_search), "[lsu]: contention on LAQ CAM search.")

  // does the incoming load match any store addresses?
  // NOTE: these are fully translated physical addresses, as
  // forwarding requires a full address check.
  for (i <- 0 until NUM_STQ_ENTRIES)
  {
    val s_addr = stq(i).bits.addr.bits
    val s_uop  = stq(i).bits.uop

    val dword_addr_matches = (lcam_st_dep_mask(i) &&
                              stq(i).bits.addr.valid &&
                              !stq(i).bits.addr_is_virtual &&
                              (s_addr(corePAddrBits-1,3) === lcam_addr(corePAddrBits-1,3)))

    // check the lower-order bits for overlap/conflicts and matches
    val write_mask = GenByteMask(s_addr, s_uop.mem_size)

    // exact match on masks? we can forward the data, if data is also present!
    when (do_ld_search && stq(i).valid) {
      when (((lcam_mask & write_mask) === lcam_mask) && !s_uop.is_fence && dword_addr_matches) {
        ldst_forward            := true.B
        ldst_forward_matches(i) := true.B
      } .elsewhen (((lcam_mask & write_mask) =/= 0.U) && dword_addr_matches) {
        io.dmem.s1_kill := true.B
        ldq(lcam_ldq_idx).bits.executed := false.B
      } .elsewhen (lcam_st_dep_mask(i) && (s_uop.is_fence || s_uop.is_amo)) {
        io.dmem.s1_kill := true.B
        ldq(lcam_ldq_idx).bits.executed := false.B
      }
    }
  }
  for (i <- 0 until NUM_LDQ_ENTRIES)
  {
    val l_addr        = ldq(i).bits.addr.bits
    val l_mask        = GenByteMask(l_addr, ldq(i).bits.uop.mem_size)
    val l_allocated   = ldq(i).valid
    val l_addr_val    = ldq(i).bits.addr.valid
    val l_is_virtual  = ldq(i).bits.addr_is_virtual
    val l_executed    = ldq(i).bits.executed
    val l_st_dep_mask = ldq(i).bits.st_dep_mask
    val l_forwarded   = ldq(i).bits.forward_std_val

    val fid           = ldq(i).bits.forward_stq_idx
    val dword_addr_matches = lcam_addr(corePAddrBits-1,3) === l_addr(corePAddrBits-1,3)

    failed_loads(i) := false.B
    when (do_st_search && l_executed && l_allocated && l_addr_val && !l_is_virtual &&
          l_st_dep_mask(lcam_stq_idx) && dword_addr_matches)
    {
      // We are older than this load, which needs us, but the load didn't get its data
      // from us. Kill the younger load
      when (!l_forwarded ||
            ((fid =/= lcam_stq_idx) && IsOlder(fid, lcam_stq_idx, stq_head)))
      {
        ldq(i).bits.order_fail := true.B
        failed_loads(i)        := true.B
        stld_order_fail        := true.B
      }

    } .elsewhen (do_ld_search && l_allocated && l_addr_val && !l_is_virtual &&
                 dword_addr_matches && (lcam_mask & l_mask) =/= 0.U) {
      val searcher_is_older = IsOlder(lcam_ldq_idx, i.U, ldq_head)
      when (searcher_is_older) {
        // younger load already executed! fail it
        when (l_executed) {
          ldq(i).bits.order_fail := true.B
          failed_loads(i)        := true.B
          ldld_order_fail        := true.B
        }
      } .elsewhen (lcam_ldq_idx =/= i.U) {
        when (!l_executed) {
          // kill ourselves, else an order fail will occur
          io.dmem.s1_kill                 := true.B
          ldq(lcam_ldq_idx).bits.executed := false.B
        }
      }
    }

  }

  val forwarding_age_logic = Module(new ForwardingAgeLogic(NUM_STQ_ENTRIES))
  forwarding_age_logic.io.addr_matches    := ldst_forward_matches.asUInt
  forwarding_age_logic.io.youngest_st_idx := lcam_uop.stq_idx
  when (ldst_forward) {
    ldq(lcam_uop.ldq_idx).bits.forward_std_val := true.B
    ldq(lcam_uop.ldq_idx).bits.forward_stq_idx := forwarding_age_logic.io.forwarding_idx
  }

  // detect which loads get marked as failures, but broadcast to the ROB the oldest failing load
  // TODO encapsulate this in an age-based  priority-encoder
  //   val l_idx = AgePriorityEncoder((Vec(Vec.tabulate(NUM_LDQ_ENTRIES)(i => failed_loads(i) && i.U >= laq_head)
  //   ++ failed_loads)).asUInt)
  val temp_bits = (VecInit(VecInit.tabulate(NUM_LDQ_ENTRIES)(i =>
    failed_loads(i) && i.U >= ldq_head) ++ failed_loads)).asUInt
  val l_idx = PriorityEncoder(temp_bits)


  // TODO always pad out the input to PECircular() to pow2
  // convert it to vec[bool], then in.padTo(1 << log2Ceil(in.size), false.B)


  // one exception port, but multiple causes!
   // - 1) the incoming store-address finds a faulting load (it is by definition younger)
   // - 2) the incoming load or store address is excepting. It must be older and thus takes precedent.
  val r_xcpt_valid = RegInit(false.B)
  val r_xcpt       = Reg(new Exception)

  val mem_xcpt_uop = Mux(mem_xcpt_valid, mem_tlb_uop,
                         ldq(Mux(l_idx >= NUM_LDQ_ENTRIES.U, l_idx - NUM_LDQ_ENTRIES.U, l_idx)).bits.uop)

  r_xcpt_valid := (failed_loads.reduce(_|_) || mem_xcpt_valid) &&
                   !io.core.exception &&
                   !IsKilledByBranch(io.core.brinfo, mem_xcpt_uop)
  r_xcpt.uop         := mem_xcpt_uop
  r_xcpt.uop.br_mask := GetNewBrMask(io.core.brinfo, mem_xcpt_uop)
  r_xcpt.cause       := Mux(mem_xcpt_valid, mem_xcpt_cause, MINI_EXCEPTION_MEM_ORDERING)
  r_xcpt.badvaddr    := RegNext(exe_vaddr) // TODO is there another register we can use instead?

  io.core.lxcpt.valid := r_xcpt_valid && !io.core.exception && !IsKilledByBranch(io.core.brinfo, r_xcpt.uop)
  io.core.lxcpt.bits  := r_xcpt

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Writeback Cycle (St->Ld Forwarding Path)
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // val wb_forward_std_val = RegInit(false.B)
  // val wb_forward_std_idx = RegNext(forwarding_age_logic.io.forwarding_idx)
  // val wb_uop             = RegNext(mem_ld_uop)
  // wb_uop.br_mask        := GetNewBrMask(io.brinfo, mem_ld_uop)
  // wb_forward_std_val    := !IsKilledByBranch(io.brinfo, mem_ld_uop) &&
  //                          mem_fired_ld &&
  //                          forwarding_age_logic.io.forwarding_val &&
  //                          !force_ld_to_sleep &&
  //                          !(mem_tlb_miss && mem_ld_used_tlb) &&
  //                          !io.exception

  //----------------------------------
  // Handle Memory Responses and nacks
  //----------------------------------

  io.core.exe.iresp.valid := false.B
  io.core.exe.fresp.valid := false.B
  when (io.dmem.resp.valid)
  {
    when (io.dmem.resp.bits.nack)
    {
      // We have to re-execute this!
      when (io.dmem.resp.bits.is_hella)
      {
        assert(hella_state === h_wait)
      }
        .elsewhen (io.dmem.resp.bits.uop.uses_ldq)
      {
        assert(ldq(io.dmem.resp.bits.uop.ldq_idx).bits.executed)
        ldq(io.dmem.resp.bits.uop.ldq_idx).bits.executed  := false.B
      }
        .otherwise
      {
        when (IsOlder(io.dmem.resp.bits.uop.stq_idx, stq_execute_head, stq_head)) {
          stq_execute_head := io.dmem.resp.bits.uop.stq_idx
        }
      }
    }
      .otherwise
    {
      when (io.dmem.resp.bits.uop.uses_ldq)
      {
        val ldq_idx = io.dmem.resp.bits.uop.ldq_idx
        // TODO: keep ctrl signals through cache datapath, or store them in the queues
        // TODO: Get forwarded data out faster, don't wait for response from dcache for the load
        val forward_stq_idx   = ldq(ldq_idx).bits.forward_stq_idx
        val req_was_forwarded = ldq(ldq_idx).bits.forward_std_val
        val stq_val           = stq(forward_stq_idx).valid

        val forward_data      = LoadDataGenerator(
          stq(forward_stq_idx).bits.data.bits,
          io.dmem.resp.bits.uop.mem_size,
          io.dmem.resp.bits.uop.mem_signed)
        val forward_ready     = stq(forward_stq_idx).bits.data.valid

        val send_iresp = ldq(ldq_idx).bits.uop.dst_rtype === RT_FIX
        val send_fresp = ldq(ldq_idx).bits.uop.dst_rtype === RT_FLT

        io.core.exe.iresp.bits.uop := ldq(ldq_idx).bits.uop
        io.core.exe.fresp.bits.uop := ldq(ldq_idx).bits.uop
        when (req_was_forwarded && !stq_val) {
          // If the store fired before this did, just fire again normally
          ldq(ldq_idx).bits.forward_std_val := false.B
          ldq(ldq_idx).bits.executed        := false.B
        } .elsewhen (req_was_forwarded && forward_ready) {
          // If we are ready to forward, send out forward data
          io.core.exe.iresp.valid     := send_iresp
          io.core.exe.iresp.bits.data := forward_data
          io.core.exe.fresp.valid     := send_fresp
          io.core.exe.fresp.bits.data := forward_data
        } .elsewhen (req_was_forwarded && !forward_ready) {
          // If we are not ready to forward, replay this
          ldq(ldq_idx).bits.executed  := false.B
        } .otherwise {
          io.core.exe.iresp.valid     := send_iresp
          io.core.exe.iresp.bits.data := io.dmem.resp.bits.data
          io.core.exe.fresp.valid     := send_fresp
          io.core.exe.fresp.bits.data := io.dmem.resp.bits.data
        }
      }
        .otherwise
      {
        stq(io.dmem.resp.bits.uop.stq_idx).bits.succeeded := true.B
        when (io.dmem.resp.bits.uop.is_amo) {
          io.core.exe.iresp.valid     := stq(io.dmem.resp.bits.uop.stq_idx).bits.uop.dst_rtype === RT_FIX
          io.core.exe.iresp.bits.uop  := stq(io.dmem.resp.bits.uop.stq_idx).bits.uop
          io.core.exe.iresp.bits.data := io.dmem.resp.bits.data
        }
      }
    }
  }



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
  //-------------------------------------------------------------
  // dequeue old entries on commit
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  var temp_stq_commit_head = stq_commit_head
  for (w <- 0 until coreWidth)
  {
    when (io.core.commit_store_mask(w))
    {
      stq(temp_stq_commit_head).bits.committed := true.B
    }

    temp_stq_commit_head = Mux(io.core.commit_store_mask(w),
                               WrapInc(temp_stq_commit_head, NUM_STQ_ENTRIES),
                               temp_stq_commit_head)
  }

  stq_commit_head := temp_stq_commit_head

  // store has been committed AND successfully sent data to memory
  when (stq(stq_head).valid && stq(stq_head).bits.committed)
  {
    clear_store := Mux(stq(stq_head).bits.uop.is_fence, io.dmem.ordered,
                                                        stq(stq_head).bits.succeeded)
  }

  when (clear_store)
  {
    stq(stq_head).valid           := false.B
    stq(stq_head).bits.addr.valid := false.B
    stq(stq_head).bits.data.valid := false.B
    stq(stq_head).bits.succeeded  := false.B
    stq(stq_head).bits.committed  := false.B

    stq_head := WrapInc(stq_head, NUM_STQ_ENTRIES)
    when (stq(stq_head).bits.uop.is_fence)
    {
      stq_execute_head := WrapInc(stq_execute_head, NUM_STQ_ENTRIES)
    }
  }

  var temp_ldq_head = ldq_head
  for (w <- 0 until coreWidth)
  {
    val idx = temp_ldq_head
    when (io.core.commit_load_mask(w))
    {
      assert (ldq(idx).valid         , "[lsu] trying to commit an un-allocated load entry.")
      assert (ldq(idx).bits.executed , "[lsu] trying to commit an un-executed load entry.")

      ldq(idx).valid                 := false.B
      ldq(idx).bits.addr.valid       := false.B
      ldq(idx).bits.executed         := false.B
      ldq(idx).bits.order_fail       := false.B
      ldq(idx).bits.forward_std_val  := false.B
    }

    temp_ldq_head = Mux(io.core.commit_load_mask(w), WrapInc(temp_ldq_head, NUM_LDQ_ENTRIES), temp_ldq_head)
  }
  ldq_head := temp_ldq_head

  // -----------------------
  // Hellacache interface
  // We need to time things like a HellaCache would
  io.hellacache.req.ready := false.B
  io.hellacache.s2_nack   := false.B
  io.hellacache.s2_xcpt   := (0.U).asTypeOf(new rocket.HellaCacheExceptions)
  io.hellacache.resp.valid := false.B
  when (hella_state === h_ready) {
    io.hellacache.req.ready := true.B
    when (io.hellacache.req.fire()) {
      hella_req   := io.hellacache.req.bits
      hella_state := h_s1
    }
  } .elsewhen (hella_state === h_s1) {
    can_fire_hella_incoming := true.B

    hella_data := io.hellacache.s1_data
    hella_xcpt := dtlb.io.resp

    when (io.hellacache.s1_kill) {
      when (will_fire_hella_incoming && io.dmem.req.fire()) {
        hella_state := h_dead
      } .otherwise {
        hella_state := h_ready
      }
    } .elsewhen (will_fire_hella_incoming && io.dmem.req.fire()) {
      hella_state := h_s2
    } .otherwise {
      hella_state := h_s2_nack
    }
  } .elsewhen (hella_state === h_s2_nack) {
    io.hellacache.s2_nack := true.B
    hella_state := h_ready
  } .elsewhen (hella_state === h_s2) {
    io.hellacache.s2_xcpt := hella_xcpt
    when (io.hellacache.s2_kill || hella_xcpt.asUInt =/= 0.U) {
      hella_state := h_dead
    } .otherwise {
      hella_state := h_wait
    }
  } .elsewhen (hella_state === h_wait) {
    when (io.dmem.resp.valid && io.dmem.resp.bits.is_hella) {
      when (io.dmem.resp.bits.nack) {
        hella_state := h_replay
      } .otherwise {
        hella_state := h_ready

        io.hellacache.resp.valid       := true.B
        io.hellacache.resp.bits.addr   := hella_req.addr
        io.hellacache.resp.bits.tag    := hella_req.tag
        io.hellacache.resp.bits.cmd    := hella_req.cmd
        io.hellacache.resp.bits.signed := hella_req.signed
        io.hellacache.resp.bits.size   := hella_req.size
        io.hellacache.resp.bits.data   := io.dmem.resp.bits.data
      }
    }
  } .elsewhen (hella_state === h_replay) {
    can_fire_hella_wakeup := true.B

    when (will_fire_hella_wakeup && io.dmem.req.fire()) {
      hella_state := h_wait
    }
  } .elsewhen (hella_state === h_dead) {
    when (io.dmem.resp.valid && io.dmem.resp.bits.is_hella) {
      hella_state := h_ready
    }
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
        when (!stq(i).bits.committed && !stq(i).bits.succeeded)
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
