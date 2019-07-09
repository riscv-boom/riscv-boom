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
import boom.util.{BoolToChar, AgePriorityEncoder, IsKilledByBranch, GetNewBrMask, WrapInc, IsOlder, UpdateBrMask}

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
}

class LSUDMemIO(implicit p: Parameters) extends BoomBundle()(p)
{
  // In LSU's dmem stage, send the request
  val req         = new DecoupledIO(Vec(memWidth, Valid(new BoomDCacheReq)))
  // In LSU's LCAM search stage, kill if order fail (or forwarding possible)
  val s1_kill     = Vec(memWidth, Output(Bool()))
  // Get a request any cycle
  val resp        = Vec(memWidth, Flipped(new ValidIO(new BoomDCacheResp)))
  // In our response stage, if we get a nack, we need to reexecute
  val nack        = Vec(memWidth, Flipped(new ValidIO(new BoomDCacheReq)))


  val brinfo       = Output(new BrResolutionInfo)
  val exception    = Output(Bool())
  val rob_pnr_idx  = Output(UInt(robAddrSz.W))
  val rob_head_idx = Output(UInt(robAddrSz.W))

  // Clears prefetching MSHRs
  val force_order  = Output(Bool())
  val ordered     = Input(Bool())
}

class LSUCoreIO(implicit p: Parameters) extends BoomBundle()(p)
{
  val exe = Vec(memWidth, new LSUExeIO)

  val dec_uops    = Flipped(Vec(coreWidth, Valid(new MicroOp)))
  val dec_ldq_idx = Output(Vec(coreWidth, UInt(LDQ_ADDR_SZ.W)))
  val dec_stq_idx = Output(Vec(coreWidth, UInt(STQ_ADDR_SZ.W)))

  val ldq_full    = Output(Vec(coreWidth, Bool()))
  val stq_full    = Output(Vec(coreWidth, Bool()))

  val fp_stdata   = Flipped(Decoupled(new MicroOpWithData(fLen)))

  val commit_store_mask = Input(Vec(coreWidth, Bool()))
  val commit_load_mask  = Input(Vec(coreWidth, Bool()))
  val commit_load_at_rob_head = Input(Bool())

  // Stores clear busy bit when stdata is received
  // memWidth for int, 1 for fpstdata (to avoid back-pressure fpstdat)
  val clr_bsy         = Output(Vec(memWidth + 1, Valid(UInt(robAddrSz.W))))

  // Speculatively safe load (barring memory ordering failure)
  val clr_unsafe      = Output(Vec(memWidth, Valid(UInt(robAddrSz.W))))

  // Tell the DCache to clear prefetches/speculating misses
  val fence_dmem   = Input(Bool())

  // Speculatively tell the IQs that we'll get load data back next cycle
  val spec_ld_wakeup = Output(Valid(UInt(PREG_SZ.W)))
  // Tell the IQs that the load we speculated last cycle was misspeculated
  val ld_miss      = Output(Bool())

  val brinfo       = Input(new BrResolutionInfo)
  val rob_pnr_idx  = Input(UInt(robAddrSz.W))
  val rob_head_idx = Input(UInt(robAddrSz.W))
  val exception    = Input(Bool())

  val fencei_rdy  = Output(Bool())

  val lxcpt       = Output(Valid(new Exception))
}

class LSUIO(implicit p: Parameters) extends BoomBundle()(p)
{
  val ptw   = Vec(memWidth, new rocket.TLBPTWIO)
  val core  = new LSUCoreIO
  val dmem  = new LSUDMemIO

  val hellacache = Flipped(new freechips.rocketchip.rocket.HellaCacheIO)
}

class LDQEntry(implicit p: Parameters) extends BoomBundle()(p)
    with HasBoomUOP
{
  val addr                = Valid(UInt(coreMaxAddrBits.W))
  val addr_is_virtual     = Bool() // Virtual address, we got a TLB miss
  val addr_is_uncacheable = Bool() // Uncacheable, wait until head of ROB to execute

  val executed            = Bool() // load sent to memory, reset by NACKs
  val execute_ignore      = Bool() // Ignore the next response we get from memory, we need to replay it
  val succeeded           = Bool() // Load send data back to core
  val order_fail          = Bool()

  val st_dep_mask         = UInt(NUM_STQ_ENTRIES.W) // list of stores older than us
  val youngest_stq_idx      = UInt(STQ_ADDR_SZ.W) // index of the oldest store younger than us

  val forward_std_val     = Bool()
  val forward_stq_idx     = UInt(STQ_ADDR_SZ.W) // Which store did we get the store-load forward from?
}

class STQEntry(implicit p: Parameters) extends BoomBundle()(p)
   with HasBoomUOP
{
  val addr                = Valid(UInt(coreMaxAddrBits.W))
  val addr_is_virtual     = Bool() // Virtual address, we got a TLB miss
  val data                = Valid(UInt(xLen.W))

  val committed           = Bool() // committed by ROB
  val succeeded           = Bool() // D$ has ack'd this, we don't need to maintain this anymore
}

class LSU(implicit p: Parameters, edge: freechips.rocketchip.tilelink.TLEdgeOut) extends BoomModule()(p)
{
  val io = IO(new LSUIO)
  io.dmem.brinfo         := io.core.brinfo
  io.dmem.exception      := io.core.exception
  io.dmem.rob_head_idx   := io.core.rob_head_idx
  io.dmem.rob_pnr_idx    := io.core.rob_pnr_idx


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
  val hella_dtlb_resp       = WireInit((0.U).asTypeOf(new rocket.TLBResp))

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

  val stq_nonempty = VecInit(stq.map(_.valid)).asUInt =/= 0.U

  var ldq_full = Bool()
  var stq_full = Bool()

  for (w <- 0 until coreWidth)
  {
    ldq_full = WrapInc(ld_enq_idx, NUM_LDQ_ENTRIES) === ldq_head
    io.core.ldq_full(w)    := ldq_full
    io.core.dec_ldq_idx(w) := ld_enq_idx

    val dec_ld_val = io.core.dec_uops(w).valid && io.core.dec_uops(w).bits.uses_ldq

    when (dec_ld_val)
    {
      ldq(ld_enq_idx).valid                := true.B
      ldq(ld_enq_idx).bits.uop             := io.core.dec_uops(w).bits
      ldq(ld_enq_idx).bits.youngest_stq_idx  := st_enq_idx
      ldq(ld_enq_idx).bits.st_dep_mask     := next_live_store_mask

      ldq(ld_enq_idx).bits.addr.valid      := false.B
      ldq(ld_enq_idx).bits.executed        := false.B
      ldq(ld_enq_idx).bits.execute_ignore  := false.B
      ldq(ld_enq_idx).bits.succeeded       := false.B
      ldq(ld_enq_idx).bits.order_fail      := false.B
      ldq(ld_enq_idx).bits.forward_std_val := false.B

      assert (ld_enq_idx === io.core.dec_uops(w).bits.ldq_idx, "[lsu] mismatch enq load tag.")
      assert (!ldq(ld_enq_idx).valid, "[lsu] Enqueuing uop is overwriting ldq entries")
    }
    ld_enq_idx = Mux(dec_ld_val, WrapInc(ld_enq_idx, NUM_LDQ_ENTRIES),
                                 ld_enq_idx)

    stq_full = WrapInc(st_enq_idx, NUM_STQ_ENTRIES) === stq_head
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

  assert (stq(stq_execute_head).valid ||
          stq_head === stq_execute_head || stq_tail === stq_execute_head,
            "stq_execute_head got off track.")



  io.dmem.force_order   := io.core.fence_dmem
  io.core.fencei_rdy    := !stq_nonempty && io.dmem.ordered

  def widthMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))

  val dtlb = Seq.fill(memWidth) { Module(new rocket.TLB(
    instruction = false, lgMaxSize = log2Ceil(coreDataBytes), rocket.TLBConfig(dcacheParams.nTLBEntries))) }
  io.ptw <> dtlb.map(_.io.ptw)

  val mem_xcpt_valid  = Wire(Bool())
  val mem_xcpt_cause  = Wire(UInt())
  val mem_xcpt_uop    = Wire(new MicroOp)
  val mem_xcpt_vaddr  = Wire(UInt())


  //---------------------------------------
  // Can-fire logic and wakeup/retry select
  //
  // First we determine what operations are waiting to execute.
  // These are the "can_fire" signals

  val will_fire_load_incoming  = Wire(Vec(memWidth, Bool()))
  val will_fire_stad_incoming  = Wire(Vec(memWidth, Bool()))
  val will_fire_sta_incoming   = Wire(Vec(memWidth, Bool()))
  val will_fire_std_incoming   = Wire(Vec(memWidth, Bool()))
  val will_fire_sfence         = Wire(Vec(memWidth, Bool()))
  val will_fire_hella_incoming = Wire(Vec(memWidth, Bool()))
  val will_fire_hella_wakeup   = Wire(Vec(memWidth, Bool()))
  val will_fire_load_retry     = Wire(Vec(memWidth, Bool()))
  val will_fire_sta_retry      = Wire(Vec(memWidth, Bool()))
  val will_fire_store_commit   = Wire(Vec(memWidth, Bool()))
  val will_fire_load_wakeup    = Wire(Vec(memWidth, Bool()))

  val exe_reqs = WireInit(widthMap(i => io.core.exe(i).req))
  // Sfence goes through both
  for (i <- 0 until memWidth) {
    when (io.core.exe(i).req.bits.sfence.valid) {
      exe_reqs := VecInit(Seq.fill(memWidth) { io.core.exe(i).req })
    }
  }

  val exe_req = widthMap(i => exe_reqs(i))

  // Don't wakeup a load if we just sent it last cycle or two cycles ago
  val block_load_mask    = WireInit(VecInit((0 until NUM_LDQ_ENTRIES).map(x=>false.B)))
  val p1_block_load_mask = RegNext(block_load_mask)
  val p2_block_load_mask = RegNext(p1_block_load_mask)

  val ldq_incoming_idx = widthMap(i => exe_req(i).bits.uop.ldq_idx)
  val ldq_incoming_e   = widthMap(i => ldq(ldq_incoming_idx(i)))

  val stq_incoming_idx = widthMap(i => exe_req(i).bits.uop.stq_idx)
  val stq_incoming_e   = widthMap(i => stq(stq_incoming_idx(i)))

  val ldq_retry_idx = RegNext(AgePriorityEncoder((0 until NUM_LDQ_ENTRIES).map(i => {
    val e = ldq(i).bits
    val block = block_load_mask(i) || p1_block_load_mask(i)
    e.addr.valid && e.addr_is_virtual && !block
  }), ldq_head))
  val ldq_retry_e   = ldq(ldq_retry_idx)

  val stq_retry_idx = RegNext(AgePriorityEncoder(stq.map(e=>e.bits.addr.valid && e.bits.addr_is_virtual), stq_commit_head))
  val stq_retry_e   = stq(stq_retry_idx)

  val ldq_wakeup_idx = RegNext(AgePriorityEncoder((0 until NUM_LDQ_ENTRIES).map(i=> {
    val e = ldq(i).bits
    val block = block_load_mask(i) || p1_block_load_mask(i)
    e.addr.valid && !e.executed && !e.succeeded && !e.addr_is_virtual && !block
  }), ldq_head))
  val ldq_wakeup_e   = ldq(ldq_wakeup_idx)

  val stq_commit_e   = stq(stq_execute_head)

  val can_fire_load_incoming = widthMap(i => exe_req(i).valid && exe_req(i).bits.uop.ctrl.is_load)
  val can_fire_stad_incoming = widthMap(i => exe_req(i).valid && exe_req(i).bits.uop.ctrl.is_sta
                                                              && exe_req(i).bits.uop.ctrl.is_std)

  val can_fire_sta_incoming  = widthMap(i => exe_req(i).valid && exe_req(i).bits.uop.ctrl.is_sta
                                                              && !exe_req(i).bits.uop.ctrl.is_std)
  val can_fire_std_incoming  = widthMap(i => exe_req(i).valid && exe_req(i).bits.uop.ctrl.is_std
                                                              && !exe_req(i).bits.uop.ctrl.is_sta)
  val can_fire_sfence       = widthMap(i => exe_req(i).valid && exe_req(i).bits.sfence.valid)
  val can_fire_load_retry   = widthMap(i =>
                                 ( ldq_retry_e.valid                            &&
                                   ldq_retry_e.bits.addr.valid                  &&
                                   ldq_retry_e.bits.addr_is_virtual             &&
                                  !p1_block_load_mask(ldq_retry_idx)            &&
                                  !p2_block_load_mask(ldq_retry_idx)            &&
                                  RegNext(dtlb(i).io.req.ready)                 &&
                                  !ldq_retry_e.bits.order_fail                  &&
                                  !(if (i == 1) will_fire_load_retry(0) else false.B)
                                 ))

  val can_fire_sta_retry    = widthMap(i =>
                                 ( stq_retry_e.valid                            &&
                                   stq_retry_e.bits.addr.valid                  &&
                                   stq_retry_e.bits.addr_is_virtual             &&
                                  !(if (i == 1) will_fire_sta_retry(0) else false.B)
                                 ))
  val can_fire_store_commit = widthMap(i =>
                                 ( stq_commit_e.valid                           &&
                                  !stq_commit_e.bits.uop.is_fence               &&
                                  !mem_xcpt_valid                               &&
                                  !stq_commit_e.bits.uop.exception              &&
                                  (i == 0).B                                    &&
                                  (stq_commit_e.bits.committed || ( stq_commit_e.bits.uop.is_amo      &&
                                                                    stq_commit_e.bits.addr.valid      &&
                                                                   !stq_commit_e.bits.addr_is_virtual &&
                                                                    stq_commit_e.bits.data.valid))
                                 ))
  val can_fire_load_wakeup = widthMap(i =>
                               ( ldq_wakeup_e.valid                       &&
                                 ldq_wakeup_e.bits.addr.valid             &&
                                !ldq_wakeup_e.bits.succeeded              &&
                                !ldq_wakeup_e.bits.addr_is_virtual        &&
                                !ldq_wakeup_e.bits.executed               &&
                                !ldq_wakeup_e.bits.order_fail             &&
                                !p1_block_load_mask(ldq_wakeup_idx)       &&
                                !p2_block_load_mask(ldq_wakeup_idx)       &&
                                !(if (i == 1) will_fire_load_wakeup(0) else false.B) &&
                                (!ldq_wakeup_e.bits.addr_is_uncacheable || (io.core.commit_load_at_rob_head &&
                                                                            ldq_head === ldq_wakeup_idx))
                               ))

  val can_fire_hella_incoming = WireInit(widthMap(i => false.B))
  val can_fire_hella_wakeup   = WireInit(widthMap(i => false.B))

  val exe_tlb_valid = Wire(Vec(memWidth, Bool()))
  //---------------------------------------------------------
  // Controller logic. Arbitrate which request actually fires
  for (i <- 0 until memWidth) {
    var tlb_avail  = true.B
    var dc_avail   = true.B
    var lcam_avail = true.B
    var rob_avail  = true.B

    def lsu_sched(can_fire: Bool, uses_tlb:Boolean, uses_dc:Boolean, uses_lcam: Boolean, uses_rob:Boolean): Bool = {
      val will_fire_this = can_fire    && !(uses_tlb.B && !tlb_avail) &&
                                          !(uses_lcam.B && !lcam_avail) &&
                                          !(uses_dc.B && !dc_avail) &&
                                          !(uses_rob.B && !rob_avail)
      tlb_avail  = tlb_avail  && !(will_fire_this && uses_tlb.B)
      lcam_avail = lcam_avail && !(will_fire_this && uses_lcam.B)
      dc_avail   = dc_avail   && !(will_fire_this && uses_dc.B)
      rob_avail  = rob_avail  && !(will_fire_this && uses_rob.B)
      dontTouch(will_fire_this) // dontTouch these so we can inspect the will_fire signals
      will_fire_this
    }

    // The order of these statements is the priority
    // Some restrictions
    //  - Incoming ops must get precedence, can't backpresure memaddrgen
    //  - Incoming hellacache ops must get precedence over retrying ops (PTW must get precedence over retrying translation)
    will_fire_load_incoming (i) := lsu_sched(can_fire_load_incoming (i) , true , true , true , false) // TLB , DC , LCAM
    will_fire_stad_incoming (i) := lsu_sched(can_fire_stad_incoming (i) , true , false, true , true)  // TLB ,    , LCAM , ROB
    will_fire_sta_incoming  (i) := lsu_sched(can_fire_sta_incoming  (i) , true , false, true , true)  // TLB ,    , LCAM , ROB
    will_fire_std_incoming  (i) := lsu_sched(can_fire_std_incoming  (i) , false, false, false, true)  //                 , ROB
    will_fire_sfence        (i) := lsu_sched(can_fire_sfence        (i) , true , false, false, true)  // TLB ,    ,      , ROB
    will_fire_hella_incoming(i) := lsu_sched(can_fire_hella_incoming(i) , true , true , false, false) // TLB , DC
    will_fire_hella_wakeup  (i) := lsu_sched(can_fire_hella_wakeup  (i) , false, true , false, false) //     , DC
    will_fire_load_retry    (i) := lsu_sched(can_fire_load_retry    (i) , true , true , true , false) // TLB , DC , LCAM
    will_fire_sta_retry     (i) := lsu_sched(can_fire_sta_retry     (i) , true , false, true , true)  // TLB ,    , LCAM , ROB
    will_fire_store_commit  (i) := lsu_sched(can_fire_store_commit  (i) , false, true , false, false) //     , DC
    will_fire_load_wakeup   (i) := lsu_sched(can_fire_load_wakeup   (i) , false, true , true , false) //     , DC , LCAM

    assert(!(exe_req(i).valid && !(will_fire_load_incoming(i) ||
                                   will_fire_stad_incoming(i) ||
                                   will_fire_sta_incoming(i)  ||
                                   will_fire_std_incoming(i)  ||
                                   will_fire_sfence(i))))

    when (will_fire_load_wakeup(i)) {
      block_load_mask(ldq_wakeup_idx)              := true.B
    } .elsewhen (will_fire_load_incoming(i)) {
      block_load_mask(exe_req(i).bits.uop.ldq_idx) := true.B
    } .elsewhen (will_fire_load_retry(i)) {
      block_load_mask(ldq_retry_idx)               := true.B
    }
    exe_tlb_valid(i) := !tlb_avail
  }

  if (memWidth > 1) {
    assert(!(will_fire_sfence.reduce(_||_) && !will_fire_sfence.reduce(_&&_)) &&
      !will_fire_hella_incoming.reduce(_&&_) &&
      !will_fire_hella_wakeup.reduce(_&&_)   &&
      !will_fire_load_retry.reduce(_&&_)     &&
      !will_fire_sta_retry.reduce(_&&_)      &&
      !will_fire_store_commit.reduce(_&&_)   &&
      !will_fire_load_wakeup.reduce(_&&_))
  }

  //--------------------------------------------
  // TLB Access

  val hella_sfence = Wire(Valid(new rocket.SFenceReq))
  hella_sfence.valid     := hella_req.cmd === rocket.M_SFENCE
  hella_sfence.bits.rs1  := hella_req.size(0)
  hella_sfence.bits.rs2  := hella_req.size(1)
  hella_sfence.bits.addr := hella_req.addr
  hella_sfence.bits.asid := io.hellacache.s1_data.data

  val exe_tlb_uop = widthMap(i =>
                      Mux(will_fire_load_incoming (i) ||
                          will_fire_stad_incoming (i) ||
                          will_fire_sta_incoming  (i) ||
                          will_fire_sfence        (i) , exe_req(i).bits.uop,
                      Mux(will_fire_load_retry    (i) , ldq_retry_e.bits.uop,
                      Mux(will_fire_sta_retry     (i) , stq_retry_e.bits.uop,
                      Mux(will_fire_hella_incoming(i) , NullMicroOp,
                                                        NullMicroOp)))))

  val exe_tlb_vaddr = widthMap(i =>
                        Mux(will_fire_load_incoming (i) ||
                            will_fire_stad_incoming (i) ||
                            will_fire_sta_incoming  (i) , exe_req(i).bits.addr,
                        Mux(will_fire_sfence        (i) , exe_req(i).bits.sfence.bits.addr,
                        Mux(will_fire_load_retry    (i) , ldq_retry_e.bits.addr.bits,
                        Mux(will_fire_sta_retry     (i) , stq_retry_e.bits.addr.bits,
                        Mux(will_fire_hella_incoming(i) , hella_req.addr,
                                                          0.U))))))

  val exe_sfence    = widthMap(i =>
                      Mux(will_fire_load_incoming (i) ||
                          will_fire_stad_incoming (i) ||
                          will_fire_sta_incoming  (i) , (0.U).asTypeOf(Valid(new rocket.SFenceReq)),
                      Mux(will_fire_sfence        (i) , exe_req(i).bits.sfence,
                      Mux(will_fire_load_retry    (i) ||
                          will_fire_sta_retry     (i) , (0.U).asTypeOf(Valid(new rocket.SFenceReq)),
                      Mux(will_fire_hella_incoming(i) , hella_sfence,
                                                        (0.U).asTypeOf(Valid(new rocket.SFenceReq)))))))

  val exe_size      = widthMap(i =>
                     Mux(will_fire_load_incoming   (i) ||
                         will_fire_stad_incoming   (i) ||
                         will_fire_sta_incoming    (i) ||
                         will_fire_sfence          (i) ||
                         will_fire_load_retry      (i) ||
                         will_fire_sta_retry       (i) , exe_tlb_uop(i).mem_size,
                     Mux(will_fire_hella_incoming  (i) , hella_req.size,
                                                         0.U)))
  val exe_cmd      = widthMap(i =>
                     Mux(will_fire_load_incoming   (i) ||
                         will_fire_stad_incoming   (i) ||
                         will_fire_sta_incoming    (i) ||
                         will_fire_sfence          (i) ||
                         will_fire_load_retry      (i) ||
                         will_fire_sta_retry       (i) , exe_tlb_uop(i).mem_cmd,
                     Mux(will_fire_hella_incoming  (i) , hella_req.cmd,
                                                         0.U)))

  val exe_passthr  = widthMap(i =>
                     Mux(will_fire_load_incoming   (i) ||
                         will_fire_stad_incoming   (i) ||
                         will_fire_sta_incoming    (i) ||
                         will_fire_sfence          (i) ||
                         will_fire_load_retry      (i) ||
                         will_fire_sta_retry       (i) , false.B,
                     Mux(will_fire_hella_incoming  (i) , hella_req.phys,
                                                         false.B)))
  val exe_kill     = widthMap(i =>
                     Mux(will_fire_load_incoming   (i) ||
                         will_fire_stad_incoming   (i) ||
                         will_fire_sta_incoming    (i) ||
                         will_fire_sfence          (i) ||
                         will_fire_load_retry      (i) ||
                         will_fire_sta_retry       (i) , false.B,
                     Mux(will_fire_hella_incoming  (i) , io.hellacache.s1_kill,
                                                         false.B)))
  for (i <- 0 until memWidth) {
    dtlb(i).io.req.valid            := exe_tlb_valid(i)
    dtlb(i).io.req.bits.vaddr       := exe_tlb_vaddr(i)
    dtlb(i).io.req.bits.size        := exe_size(i)
    dtlb(i).io.req.bits.cmd         := exe_cmd(i)
    dtlb(i).io.req.bits.passthrough := exe_passthr(i)
    dtlb(i).io.kill                 := exe_kill(i)
    dtlb(i).io.sfence               := exe_sfence(i)
  }

  // exceptions
  val ma_ld = widthMap(i => will_fire_load_incoming(i) && exe_req(i).bits.mxcpt.valid) // We get ma_ld in memaddrcalc
  val ma_st = widthMap(i => (will_fire_sta_incoming(i) || will_fire_stad_incoming(i)) && exe_req(i).bits.mxcpt.valid) // We get ma_ld in memaddrcalc
  val pf_ld = widthMap(i => dtlb(i).io.req.valid && dtlb(i).io.resp.pf.ld && (exe_tlb_uop(i).uses_ldq || exe_tlb_uop(i).is_amo)) // TODO: uses_ldq is not right here
  val pf_st = widthMap(i => dtlb(i).io.req.valid && dtlb(i).io.resp.pf.st && exe_tlb_uop(i).uses_stq)
  val ae_ld = widthMap(i => dtlb(i).io.req.valid && dtlb(i).io.resp.ae.ld && (exe_tlb_uop(i).uses_ldq|| exe_tlb_uop(i).is_amo))
  val ae_st = widthMap(i => dtlb(i).io.req.valid && dtlb(i).io.resp.ae.st && exe_tlb_uop(i).uses_stq)

  for (i <- 0 until memWidth) {
    when (will_fire_hella_incoming(i)) {
      hella_dtlb_resp := dtlb(i).io.resp
    }
  }

  // TODO check for xcpt_if and verify that never happens on non-speculative instructions.
  val mem_xcpt_valids  = RegNext(widthMap(i =>
                         (pf_ld(i) || pf_st(i) || ae_ld(i) || ae_st(i) || ma_ld(i) || ma_st(i)) &&
                         !io.core.exception &&
                         !IsKilledByBranch(io.core.brinfo, exe_tlb_uop(i))))
  val mem_xcpt_uops    = RegNext(exe_tlb_uop)
  val mem_xcpt_causes  = RegNext(widthMap(i =>
                          Mux(ma_ld(i), rocket.Causes.misaligned_load.U,
                          Mux(ma_st(i), rocket.Causes.misaligned_store.U,
                          Mux(pf_ld(i), rocket.Causes.load_page_fault.U,
                          Mux(pf_st(i), rocket.Causes.store_page_fault.U,
                          Mux(ae_ld(i), rocket.Causes.load_access.U,
                                        rocket.Causes.store_access.U)))))))
  val mem_xcpt_vaddrs  = RegNext(exe_tlb_vaddr)

  for (i <- 0 until memWidth) {
    assert (!(dtlb(i).io.req.valid && exe_tlb_uop(i).is_fence), "Fence is pretending to talk to the TLB")
    assert (!((will_fire_load_incoming(i) || will_fire_sta_incoming(i) || will_fire_stad_incoming(i)) &&
      exe_req(i).bits.mxcpt.valid && dtlb(i).io.req.valid &&
    !(exe_tlb_uop(i).ctrl.is_load || exe_tlb_uop(i).ctrl.is_sta)),
      "A uop that's not a load or store-address is throwing a memory exception.")
  }

  // Find the youngest entry in the xcpt vector
  mem_xcpt_valid := mem_xcpt_valids.reduce(_||_)
  require(memWidth <= 2)
  if (memWidth == 1) {
    mem_xcpt_cause   := mem_xcpt_causes(0)
    mem_xcpt_uop     := mem_xcpt_uops(0)
    mem_xcpt_vaddr   := mem_xcpt_vaddrs(0)
  } else {
    when (mem_xcpt_valids.asUInt === 1.U) {
      mem_xcpt_cause := mem_xcpt_causes(0)
      mem_xcpt_uop   := mem_xcpt_uops(0)
      mem_xcpt_vaddr := mem_xcpt_vaddrs(0)
    } .elsewhen (mem_xcpt_valids.asUInt === 2.U) {
      mem_xcpt_cause := mem_xcpt_causes(1)
      mem_xcpt_uop   := mem_xcpt_uops(1)
      mem_xcpt_vaddr := mem_xcpt_vaddrs(1)
    } .otherwise {
      val first_older = IsOlder(mem_xcpt_uops(0).rob_idx, mem_xcpt_uops(1).rob_idx,
                                io.core.rob_head_idx)
      mem_xcpt_cause := Mux(first_older, mem_xcpt_causes(0), mem_xcpt_causes(1))
      mem_xcpt_uop   := Mux(first_older, mem_xcpt_uops(0)  , mem_xcpt_uops(1))
      mem_xcpt_vaddr := Mux(first_older, mem_xcpt_vaddrs(0), mem_xcpt_vaddrs(1))
    }
  }

  val exe_tlb_miss        = widthMap(i => dtlb(i).io.req.valid && (dtlb(i).io.resp.miss || !dtlb(i).io.req.ready))
  val exe_tlb_paddr       = widthMap(i => Cat(dtlb(i).io.resp.paddr(paddrBits-1,corePgIdxBits), exe_tlb_vaddr(i)(corePgIdxBits-1,0)))
  val exe_tlb_uncacheable = widthMap(i => dtlb(i).io.req.valid && !dtlb(i).io.resp.cacheable)

  for (i <- 0 until memWidth) {
    assert (exe_tlb_paddr(i) === dtlb(i).io.resp.paddr || exe_req(i).bits.sfence.valid, "[lsu] paddrs should match.")

    when (mem_xcpt_valids(i))
    {
      // Technically only faulting AMOs need this
      assert(mem_xcpt_uops(i).uses_ldq ^ mem_xcpt_uops(i).uses_stq)
      when (mem_xcpt_uops(i).uses_ldq)
      {
        ldq(mem_xcpt_uops(i).ldq_idx).bits.uop.exception := true.B
      }
        .otherwise
      {
        stq(mem_xcpt_uops(i).stq_idx).bits.uop.exception := true.B
      }
    }
  }

  //------------------------------
  // Issue Someting to Memory
  //
  // A memory op can come from many different places
  // The address either was freshly translated, or we are
  // reading a physical address from the LDQ,STQ, or the HellaCache adapter

  val dmem_req             = Wire(Vec(memWidth, Valid(new BoomDCacheReq)))
  dontTouch(dmem_req)
  io.dmem.req.valid       := dmem_req.map(_.valid).reduce(_||_)
  io.dmem.req.bits        := dmem_req
  val dmem_req_fire        = widthMap(i => dmem_req(i).valid && io.dmem.req.fire())

  for (i <- 0 until memWidth) {
    // defaults
    dmem_req(i).valid         := false.B
    dmem_req(i).bits.uop      := NullMicroOp
    dmem_req(i).bits.addr     := 0.U
    dmem_req(i).bits.data     := 0.U
    dmem_req(i).bits.is_hella := false.B

    io.dmem.s1_kill(i)           := false.B

    when (will_fire_load_incoming(i)) {
      dmem_req(i).valid      := !exe_tlb_miss(i) && !exe_tlb_uncacheable(i) && !will_fire_store_commit.reduce(_||_)
      dmem_req(i).bits.addr  := exe_tlb_paddr(i)
      dmem_req(i).bits.uop   := exe_tlb_uop(i)

      ldq(ldq_incoming_idx(i)).bits.executed := dmem_req_fire(i)
      assert(!ldq_incoming_e(i).bits.executed)

    } .elsewhen (will_fire_load_retry(i)) {
      dmem_req(i).valid      := !exe_tlb_miss(i) && !exe_tlb_uncacheable(i) && !will_fire_store_commit.reduce(_||_)
      dmem_req(i).bits.addr  := exe_tlb_paddr(i)
      dmem_req(i).bits.uop   := exe_tlb_uop(i)

      ldq(ldq_retry_idx).bits.executed := dmem_req_fire(i)
      assert(!ldq_retry_e.bits.executed)

    } .elsewhen (will_fire_store_commit(i)) {
      assert((i == 0).B)
      dmem_req(i).valid         := true.B
      dmem_req(i).bits.addr     := stq_commit_e.bits.addr.bits
      dmem_req(i).bits.data     := (new freechips.rocketchip.rocket.StoreGen(
                                       stq_commit_e.bits.uop.mem_size, 0.U,
                                       stq_commit_e.bits.data.bits,
                                       coreDataBytes)).data
      dmem_req(i).bits.uop      := stq_commit_e.bits.uop

      stq_execute_head          := Mux(dmem_req_fire(i),
                                       WrapInc(stq_execute_head, NUM_STQ_ENTRIES),
                                       stq_execute_head)

      stq(stq_execute_head).bits.succeeded := false.B

    } .elsewhen (will_fire_load_wakeup(i)) {
      dmem_req(i).valid      := !will_fire_store_commit.reduce(_||_)
      dmem_req(i).bits.addr  := ldq_wakeup_e.bits.addr.bits
      dmem_req(i).bits.uop   := ldq_wakeup_e.bits.uop

      ldq(ldq_wakeup_idx).bits.executed := dmem_req_fire(i)

      assert(!ldq_wakeup_e.bits.executed && !ldq_wakeup_e.bits.addr_is_virtual)

    } .elsewhen (will_fire_hella_incoming(i)) {
      assert(hella_state === h_s1)

      dmem_req(i).valid               := !io.hellacache.s1_kill && (!exe_tlb_miss(i) || hella_req.phys)
      dmem_req(i).bits.addr           := exe_tlb_paddr(i)
      dmem_req(i).bits.data           := (new freechips.rocketchip.rocket.StoreGen(
                                               hella_req.size, 0.U,
                                               io.hellacache.s1_data.data,
                                               coreDataBytes)).data
      dmem_req(i).bits.uop.mem_cmd    := hella_req.cmd
      dmem_req(i).bits.uop.mem_size   := hella_req.size
      dmem_req(i).bits.uop.mem_signed := hella_req.signed
      dmem_req(i).bits.is_hella       := true.B

      hella_paddr := exe_tlb_paddr(i)
    }
      .elsewhen (will_fire_hella_wakeup(i))
    {
      assert(hella_state === h_replay)
      dmem_req(i).valid               := true.B
      dmem_req(i).bits.addr           := hella_paddr
      dmem_req(i).bits.data           := (new freechips.rocketchip.rocket.StoreGen(
        hella_req.size, 0.U,
        hella_data.data,
        coreDataBytes)).data
      dmem_req(i).bits.uop.mem_cmd    := hella_req.cmd
      dmem_req(i).bits.uop.mem_size   := hella_req.size
      dmem_req(i).bits.uop.mem_signed := hella_req.signed
      dmem_req(i).bits.is_hella       := true.B
    }

    //-------------------------------------------------------------
    // Write Addr into the LAQ/SAQ
    when (will_fire_load_incoming(i) || will_fire_load_retry(i))
    {
      val ldq_idx = Mux(will_fire_load_incoming(i), ldq_incoming_idx(i), ldq_retry_idx)
      ldq(ldq_idx).bits.addr.valid          := true.B
      ldq(ldq_idx).bits.addr.bits           := Mux(exe_tlb_miss(i), exe_tlb_vaddr(i), exe_tlb_paddr(i))
      ldq(ldq_idx).bits.uop.pdst            := exe_tlb_uop(i).pdst
      ldq(ldq_idx).bits.addr_is_virtual     := exe_tlb_miss(i)
      ldq(ldq_idx).bits.addr_is_uncacheable := exe_tlb_uncacheable(i) && !exe_tlb_miss(i)

      assert(!(will_fire_load_incoming(i) && ldq_incoming_e(i).bits.addr.valid),
        "[lsu] Incoming load is overwriting a valid address")
      }

    when (will_fire_sta_incoming(i) || will_fire_stad_incoming(i) || will_fire_sta_retry(i))
    {
      val stq_idx = Mux(will_fire_sta_incoming(i) || will_fire_stad_incoming(i),
        stq_incoming_idx(i), stq_retry_idx)

      stq(stq_idx).bits.addr.valid := !pf_st(i) // Prevent AMOs from executing!
      stq(stq_idx).bits.addr.bits  := Mux(exe_tlb_miss(i), exe_tlb_vaddr(i), exe_tlb_paddr(i))
      stq(stq_idx).bits.uop.pdst   := exe_tlb_uop(i).pdst // Needed for AMOs
      stq(stq_idx).bits.addr_is_virtual := exe_tlb_miss(i)

      assert(!(will_fire_sta_incoming(i) && stq_incoming_e(i).bits.addr.valid),
        "[lsu] Incoming store is overwriting a valid address")

    }

    //-------------------------------------------------------------
    // Write data into the STQ
    if (i == 0) {
      io.core.fp_stdata.ready := !will_fire_std_incoming(i) && !will_fire_stad_incoming(i)
    }
    val fire_fp_stdata = io.core.fp_stdata.fire() && (i == 0).B
    when (will_fire_std_incoming(i) || will_fire_stad_incoming(i) || fire_fp_stdata)
    {
      val sidx = Mux(will_fire_std_incoming(i) || will_fire_stad_incoming(i),
        stq_incoming_idx(i),
        io.core.fp_stdata.bits.uop.stq_idx)
      stq(sidx).bits.data.valid := true.B
      stq(sidx).bits.data.bits  := Mux(will_fire_std_incoming(i) || will_fire_stad_incoming(i),
        exe_req(i).bits.data,
        io.core.fp_stdata.bits.data)

      assert(!(stq(sidx).bits.data.valid),
        "[lsu] Incoming store is overwriting a valid data entry")
    }
  }

  val will_fire_stdf_incoming = io.core.fp_stdata.fire()
  require (xLen >= fLen) // for correct SDQ size

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Cache Access Cycle (Mem)
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Note the DCache may not have accepted our request
  
  val fired_load_incoming  = RegNext(will_fire_load_incoming)
  val fired_stad_incoming  = RegNext(will_fire_stad_incoming)
  val fired_sta_incoming   = RegNext(will_fire_sta_incoming)
  val fired_std_incoming   = RegNext(will_fire_std_incoming)
  val fired_sfence         = RegNext(will_fire_sfence)
  val fired_load_retry     = RegNext(will_fire_load_retry)
  val fired_sta_retry      = RegNext(will_fire_sta_retry)
  val fired_store_commit   = RegNext(will_fire_store_commit)
  val fired_load_wakeup    = RegNext(will_fire_load_wakeup)
  val fired_hella_incoming = RegNext(will_fire_hella_incoming)
  val fired_hella_wakeup   = RegNext(will_fire_hella_wakeup)
  val fired_stdf_incoming  = RegNext(will_fire_stdf_incoming)

  val mem_incoming_uop     = RegNext(widthMap(i => UpdateBrMask(io.core.brinfo, exe_req(i).bits.uop)))
  val mem_ldq_incoming_e   = RegNext(widthMap(i => UpdateBrMask(io.core.brinfo, ldq_incoming_e(i))))
  val mem_stq_incoming_e   = RegNext(widthMap(i => UpdateBrMask(io.core.brinfo, stq_incoming_e(i))))
  val mem_ldq_wakeup_e     = RegNext(UpdateBrMask(io.core.brinfo, ldq_wakeup_e))
  val mem_ldq_retry_e      = RegNext(UpdateBrMask(io.core.brinfo, ldq_retry_e))
  val mem_stq_retry_e      = RegNext(UpdateBrMask(io.core.brinfo, stq_retry_e))
  val mem_stdf_uop         = RegNext(UpdateBrMask(io.core.brinfo, io.core.fp_stdata.bits.uop))
  val mem_ldq_e            = widthMap(i =>
                               Mux(fired_load_incoming(i), mem_ldq_incoming_e(i),
                               Mux(fired_load_retry   (i), mem_ldq_retry_e,
                               Mux(fired_load_wakeup  (i), mem_ldq_wakeup_e, (0.U).asTypeOf(Valid(new LDQEntry))))))
  val mem_stq_e            = widthMap(i =>
                               Mux(fired_stad_incoming(i) ||
                                   fired_sta_incoming (i), mem_stq_incoming_e(i),
                               Mux(fired_sta_retry    (i), mem_stq_retry_e, (0.U).asTypeOf(Valid(new STQEntry)))))

  val mem_tlb_miss         = RegNext(exe_tlb_miss)
  val mem_tlb_uncacheable  = RegNext(exe_tlb_uncacheable)
  val mem_paddr            = RegNext(widthMap(i => io.dmem.req.bits(i).bits.addr))

  // Task 1: Clr ROB busy bit
  val clr_bsy_valid   = RegInit(widthMap(i => false.B))
  val clr_bsy_rob_idx = Reg(Vec(memWidth, UInt(robAddrSz.W)))
  val clr_bsy_brmask  = Reg(Vec(memWidth, UInt(MAX_BR_COUNT.W)))

  for (i <- 0 until memWidth) {
    clr_bsy_valid(i)   := false.B
    clr_bsy_rob_idx(i) := 0.U
    clr_bsy_brmask(i)  := 0.U


    when (fired_stad_incoming(i)) {
      clr_bsy_valid(i)   :=  mem_stq_incoming_e(i).valid           &&
                            !mem_tlb_miss(i)                       &&
                            !mem_stq_incoming_e(i).bits.uop.is_amo &&
                            !IsKilledByBranch(io.core.brinfo, mem_stq_incoming_e(i).bits.uop)
      clr_bsy_rob_idx(i) := mem_stq_incoming_e(i).bits.uop.rob_idx
      clr_bsy_brmask(i)  := GetNewBrMask(io.core.brinfo, mem_stq_incoming_e(i).bits.uop)
    } .elsewhen (fired_sta_incoming(i)) {
      clr_bsy_valid(i)   := mem_stq_incoming_e(i).valid           &&
                            mem_stq_incoming_e(i).bits.data.valid &&
                           !mem_tlb_miss(i)                       &&
                           !mem_stq_incoming_e(i).bits.uop.is_amo &&
                           !IsKilledByBranch(io.core.brinfo, mem_stq_incoming_e(i).bits.uop)
      clr_bsy_rob_idx(i) := mem_stq_incoming_e(i).bits.uop.rob_idx
      clr_bsy_brmask(i)  := GetNewBrMask(io.core.brinfo, mem_stq_incoming_e(i).bits.uop)
    } .elsewhen (fired_std_incoming(i)) {
      clr_bsy_valid(i)   := mem_stq_incoming_e(i).valid                &&
                            mem_stq_incoming_e(i).bits.addr.valid      &&
                           !mem_stq_incoming_e(i).bits.addr_is_virtual &&
                           !mem_stq_incoming_e(i).bits.uop.is_amo      &&
                           !IsKilledByBranch(io.core.brinfo, mem_stq_incoming_e(i).bits.uop)
      clr_bsy_rob_idx(i) := mem_stq_incoming_e(i).bits.uop.rob_idx
      clr_bsy_brmask(i)  := GetNewBrMask(io.core.brinfo, mem_stq_incoming_e(i).bits.uop)
    } .elsewhen (fired_sfence(i)) {
      if (i == 0) {
        // Only allow one path to fire the clear signal from sfence
        clr_bsy_valid(i)   := true.B
        clr_bsy_rob_idx(i) := mem_incoming_uop(i).rob_idx
        clr_bsy_brmask(i)  := GetNewBrMask(io.core.brinfo, mem_incoming_uop(i))
      }
    } .elsewhen (fired_sta_retry(i)) {
      clr_bsy_valid(i)   := mem_stq_retry_e.valid           &&
                            mem_stq_retry_e.bits.data.valid &&
                           !mem_tlb_miss(i)                 &&
                           !mem_stq_retry_e.bits.uop.is_amo &&
                           !IsKilledByBranch(io.core.brinfo, mem_stq_retry_e.bits.uop)
      clr_bsy_rob_idx(i) := mem_stq_retry_e.bits.uop.rob_idx
      clr_bsy_brmask(i)  := GetNewBrMask(io.core.brinfo, mem_stq_retry_e.bits.uop)
    }



    io.core.clr_bsy(i).valid := clr_bsy_valid(i) &&
                               !IsKilledByBranch(io.core.brinfo, clr_bsy_brmask(i)) &&
                               !io.core.exception && !RegNext(io.core.exception) && !RegNext(RegNext(io.core.exception))
    io.core.clr_bsy(i).bits  := clr_bsy_rob_idx(i)
  }

  val stdf_clr_bsy_valid   = RegInit(false.B)
  val stdf_clr_bsy_rob_idx = Reg(UInt(robAddrSz.W))
  val stdf_clr_bsy_brmask  = Reg(UInt(MAX_BR_COUNT.W))

  stdf_clr_bsy_valid   := false.B
  stdf_clr_bsy_rob_idx := 0.U
  stdf_clr_bsy_brmask  := 0.U
  when (fired_stdf_incoming) {
    val s_idx = mem_stdf_uop.stq_idx
    stdf_clr_bsy_valid   := stq(s_idx).valid                 &&
                            stq(s_idx).bits.addr.valid       &&
                            !stq(s_idx).bits.addr_is_virtual  &&
                            !stq(s_idx).bits.uop.is_amo       &&
                            !IsKilledByBranch(io.core.brinfo, mem_stdf_uop)
    stdf_clr_bsy_rob_idx := mem_stdf_uop.rob_idx
    stdf_clr_bsy_brmask  := GetNewBrMask(io.core.brinfo, mem_stdf_uop)
  }

  io.core.clr_bsy(memWidth).valid := stdf_clr_bsy_valid &&
                                     !IsKilledByBranch(io.core.brinfo, stdf_clr_bsy_brmask) &&
                                     !io.core.exception && !RegNext(io.core.exception) && !RegNext(RegNext(io.core.exception))
  io.core.clr_bsy(memWidth).bits  := stdf_clr_bsy_rob_idx

  // Task 2: Do LD-LD. ST-LD searches for ordering failures
  //         Do LD-ST search for forwarding opportunities
  // We have the opportunity to kill a request we sent last cycle. Use it wisely!

  // We translated a store last cycle
  val do_st_search = widthMap(i => (fired_stad_incoming(i) || fired_sta_incoming(i) || fired_sta_retry(i)) && !mem_tlb_miss(i))
  // We translated a load last cycle
  val do_ld_search = widthMap(i => ((fired_load_incoming(i) || fired_load_retry(i)) && !mem_tlb_miss(i)) ||
                         fired_load_wakeup(i))

  for (i <- 0 until memWidth) {
    assert(!(do_st_search(i) && do_ld_search(i)))
  }

  // Store addrs don't go to memory yet, get it from the TLB response
  // Load wakeups don't go through TLB, get it through memory
  // Load incoming and load retries go through both

  val lcam_addr  = widthMap(i =>
                   Mux(fired_stad_incoming(i) || fired_sta_incoming(i) || fired_sta_retry(i),
                       RegNext(exe_tlb_paddr(i)), mem_paddr(i)))
  val lcam_uop   = widthMap(i =>
                   Mux(do_st_search(i), mem_stq_e(i).bits.uop,
                   Mux(do_ld_search(i), mem_ldq_e(i).bits.uop, NullMicroOp)))

  val lcam_valid = widthMap(i =>
                   Mux(do_st_search(i), mem_stq_e(i).valid,
                   Mux(do_ld_search(i), mem_ldq_e(i).valid, false.B)))

  val lcam_mask        = widthMap(i => GenByteMask(lcam_addr(i), lcam_uop(i).mem_size))
  val lcam_st_dep_mask = widthMap(i => mem_ldq_e(i).bits.st_dep_mask)
  val lcam_is_fence    = widthMap(i => lcam_uop(i).is_fence)
  val lcam_ldq_idx     = widthMap(i =>
                         Mux(fired_load_incoming(i), mem_incoming_uop(i).ldq_idx,
                         Mux(fired_load_wakeup(i)  , RegNext(ldq_wakeup_idx),
                         Mux(fired_load_retry(i)   , RegNext(ldq_retry_idx), 0.U))))
  val lcam_stq_idx     = widthMap(i =>
                         Mux(fired_stad_incoming(i) ||
                             fired_sta_incoming(i) , mem_incoming_uop(i).stq_idx,
                         Mux(fired_sta_retry(i)    , RegNext(stq_retry_idx), 0.U)))

  val can_forward = WireInit(widthMap(i => Mux(fired_load_incoming(i) || fired_load_retry(i),
                                               !mem_tlb_uncacheable(i),
                                               !ldq(lcam_ldq_idx(i)).bits.addr_is_uncacheable)))

  // Mask of stores which we conflict on address with
  val ldst_addr_matches    = WireInit(widthMap(i => VecInit((0 until NUM_STQ_ENTRIES).map(x=>false.B))))
  // Mask of stores which we can forward from
  val ldst_forward_matches = WireInit(widthMap(i => VecInit((0 until NUM_STQ_ENTRIES).map(x=>false.B))))

  val mem_executing_ld_mask   = WireInit(VecInit((0 until NUM_LDQ_ENTRIES).map(x=>false.B)))
  val failed_loads            = WireInit(VecInit((0 until NUM_LDQ_ENTRIES).map(x=>false.B)))
  val succeeding_ld_mask      = WireInit(VecInit((0 until NUM_LDQ_ENTRIES).map(x=>false.B)))
  val nacking_ld_mask         = WireInit(VecInit((0 until NUM_LDQ_ENTRIES).map(x=>false.B)))


  for (l <- 0 until NUM_LDQ_ENTRIES) {
    val l_valid = ldq(l).valid
    val l_bits  = ldq(l).bits
    val l_addr  = ldq(l).bits.addr.bits
    val l_mask  = GenByteMask(l_addr, l_bits.uop.mem_size)

    val dword_addr_matches = widthMap(i => lcam_addr(i)(corePAddrBits-1,3) === l_addr(corePAddrBits-1,3))
    val mask_match         = widthMap(i => (l_mask & lcam_mask(i)) === l_mask)
    val l_is_succeeding    = succeeding_ld_mask(l)

    for (i <- 0 until memWidth) {
      // Searcher is a store
      when (do_st_search(i)                                                                      &&
            lcam_valid(i)                                                                        &&
            l_valid                                                                              &&
            l_bits.addr.valid                                                                    &&
            ((l_bits.executed && !l_bits.execute_ignore) || l_bits.succeeded || l_is_succeeding) &&
            !l_bits.addr_is_virtual                                                              &&
            l_bits.st_dep_mask(lcam_stq_idx(i))                                                  &&
            dword_addr_matches(i)) {
        val forwarded_is_older = IsOlder(l_bits.forward_stq_idx, lcam_stq_idx(i), l_bits.youngest_stq_idx)
        // We are older than this load, which overlapped us.
        when (!l_bits.forward_std_val || // If the load wasn't forwarded, it definitely failed
          ((l_bits.forward_stq_idx =/= lcam_stq_idx(i)) && forwarded_is_older)) { // If the load forwarded from us, we might be ok
          when (l_bits.succeeded || l_is_succeeding) { // If the younger load already succeeded, we are screwed. Throw order fail
            ldq(l).bits.order_fail := true.B
            failed_loads(l)        := true.B
          } .otherwise { // If the younger load hasn't responded yet, tell it to kill its response
            ldq(l).bits.execute_ignore := true.B
          }
        }
      } .elsewhen (do_ld_search(i)         &&
                   lcam_valid(i)           &&
                   l_valid                 &&
                   l_bits.addr.valid       &&
                   !l_bits.addr_is_virtual &&
                   dword_addr_matches(i)   &&
                   ((lcam_mask(i) & l_mask) =/= 0.U)) {
        val searcher_is_older = IsOlder(lcam_ldq_idx(i), l.U, ldq_head)
        when (searcher_is_older) {
          when (l_bits.executed && !l_bits.execute_ignore && !mem_executing_ld_mask(l)) { // If the load is proceeding with us, we don't need to kill it
            when (l_bits.succeeded || l_is_succeeding) { // If the younger load is executing and succeeded, we are screwed. Throw order fail
              ldq(l).bits.order_fail := true.B
              failed_loads(l)        := true.B
            } .otherwise { // The younger load hasn't returned yet, we can kill its response
              ldq(l).bits.execute_ignore := true.B
            }
          }
        } .elsewhen (lcam_ldq_idx(i) =/= l.U) {
          // The load is older, and either it hasn't executed, it was nacked, or it is ignoring its response
          // we need to kill ourselves, and prevent forwarding
          when (!l_bits.executed || nacking_ld_mask(l) || l_bits.execute_ignore) {
            io.dmem.s1_kill(i)                       := RegNext(dmem_req_fire(i))
            ldq(lcam_ldq_idx(i)).bits.executed       := false.B
            ldq(lcam_ldq_idx(i)).bits.execute_ignore := false.B
            can_forward(i)                           := false.B
          }
        }
      }
    }
  }


  for (s <- 0 until NUM_STQ_ENTRIES) {
    val s_addr = stq(s).bits.addr.bits
    val s_uop  = stq(s).bits.uop
    val dword_addr_matches = widthMap(i =>
                             ( stq(s).bits.addr.valid      &&
                              !stq(s).bits.addr_is_virtual &&
                              (s_addr(corePAddrBits-1,3) === lcam_addr(i)(corePAddrBits-1,3))))
    val write_mask = GenByteMask(s_addr, s_uop.mem_size)
    for (i <- 0 until memWidth) {
      when (do_ld_search(i) && stq(s).valid && lcam_valid(i) && lcam_st_dep_mask(i)(s)) {
        when (((lcam_mask(i) & write_mask) === lcam_mask(i)) && !s_uop.is_fence && dword_addr_matches(i) && can_forward(i))
        {
          ldst_addr_matches(i)(s)         := true.B
          ldst_forward_matches(i)(s)      := true.B
          io.dmem.s1_kill(i)              := RegNext(dmem_req_fire(i))
          ldq(lcam_ldq_idx(i)).bits.executed       := false.B
          ldq(lcam_ldq_idx(i)).bits.execute_ignore := false.B
        }
          .elsewhen (((lcam_mask(i) & write_mask) =/= 0.U) && dword_addr_matches(i))
        {
          ldst_addr_matches(i)(s)         := true.B
          io.dmem.s1_kill(i)              := RegNext(dmem_req_fire(i))
          ldq(lcam_ldq_idx(i)).bits.executed       := false.B
          ldq(lcam_ldq_idx(i)).bits.execute_ignore := false.B
        }
          .elsewhen (s_uop.is_fence || s_uop.is_amo)
        {
          ldst_addr_matches(i)(s)         := true.B
          io.dmem.s1_kill(i)              := RegNext(dmem_req_fire(i))
          ldq(lcam_ldq_idx(i)).bits.executed       := false.B
          ldq(lcam_ldq_idx(i)).bits.execute_ignore := false.B
        }
      }
    }
  }

  // Find the youngest store which the load is dependent on
  val forwarding_age_logic = Seq.fill(memWidth) { Module(new ForwardingAgeLogic(NUM_STQ_ENTRIES)) }
  for (i <- 0 until memWidth) {
    forwarding_age_logic(i).io.addr_matches    := ldst_addr_matches(i).asUInt
    forwarding_age_logic(i).io.youngest_st_idx := lcam_uop(i).stq_idx
  }
  val forwarding_idx = widthMap(i => forwarding_age_logic(i).io.forwarding_idx)

  // Forward if st-ld forwarding is possible from the writemask and loadmask
  val mem_forward_valid       = widthMap(i =>
                                 (ldst_forward_matches(i)(forwarding_idx(i))    &&
                                 !IsKilledByBranch(io.core.brinfo, lcam_uop(i)) &&
                                 !io.core.exception && !RegNext(io.core.exception)))
  val mem_forward_ldq_idx     = lcam_ldq_idx
  val mem_forward_ld_addr     = lcam_addr
  val mem_forward_stq_idx     = forwarding_idx

  // Task 3: Clr unsafe bit in ROB for succesful translations
  //         Delay this a cycle to avoid going ahead of the exception broadcast
  //         The unsafe bit is cleared on the first translation, so no need to fire for load wakeups
  for (i <- 0 until memWidth) {
    io.core.clr_unsafe(i).valid := RegNext((do_st_search(i) || do_ld_search(i)) && !fired_load_wakeup(i))
    io.core.clr_unsafe(i).bits  := RegNext(lcam_uop(i).rob_idx)
  }


  // Task 4: Speculatively wakeup loads 1 cycle before they come back. Only do this for one path
  io.core.spec_ld_wakeup.valid := enableFastLoadUse.B         &&
                                  fired_load_incoming(0)      &&
                                 !mem_incoming_uop(0).fp_val  &&
                                  mem_incoming_uop(0).pdst =/= 0.U
  io.core.spec_ld_wakeup.bits  := mem_incoming_uop(0).pdst

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Writeback Cycle (St->Ld Forwarding Path)
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  val wb_forward_valid    = RegNext(mem_forward_valid)
  val wb_forward_ldq_idx  = RegNext(mem_forward_ldq_idx)
  val wb_forward_ld_addr  = RegNext(mem_forward_ld_addr)
  val wb_forward_stq_idx  = RegNext(mem_forward_stq_idx)

  // Handle Memory Responses and nacks
  //----------------------------------
  for (i <- 0 until memWidth) {
    io.core.exe(i).iresp.valid := false.B
    io.core.exe(i).iresp.bits  := DontCare
    io.core.exe(i).fresp.valid := false.B
    io.core.exe(i).fresp.bits  := DontCare
  }

  val dmem_resp_fired = WireInit(widthMap(i => false.B))
  // Handle nacks
  for (i <- 0 until memWidth) {
    when (io.dmem.nack(i).valid)
    {
      // We have to re-execute this!
      when (io.dmem.nack(i).bits.is_hella)
      {
        assert(hella_state === h_wait || hella_state === h_dead)
      }
        .elsewhen (io.dmem.nack(i).bits.uop.uses_ldq)
      {
        assert(ldq(io.dmem.nack(i).bits.uop.ldq_idx).bits.executed)
        ldq(io.dmem.nack(i).bits.uop.ldq_idx).bits.executed  := false.B
        ldq(io.dmem.nack(i).bits.uop.ldq_idx).bits.execute_ignore  := false.B
        nacking_ld_mask(io.dmem.nack(i).bits.uop.ldq_idx) := true.B
      }
        .otherwise
      {
        assert(io.dmem.nack(i).bits.uop.uses_stq)
        when (IsOlder(io.dmem.nack(i).bits.uop.stq_idx, stq_execute_head, stq_head)) {
          stq_execute_head := io.dmem.nack(i).bits.uop.stq_idx
        }
      }
    }
    // Handle the response
    when (io.dmem.resp(i).valid)
    {
      when (io.dmem.resp(i).bits.uop.uses_ldq)
      {
        assert(!io.dmem.resp(i).bits.is_hella)
        val ldq_idx = io.dmem.resp(i).bits.uop.ldq_idx
        val send_iresp = ldq(ldq_idx).bits.uop.dst_rtype === RT_FIX
        val send_fresp = ldq(ldq_idx).bits.uop.dst_rtype === RT_FLT
        val ignore     = ldq(ldq_idx).bits.execute_ignore

        io.core.exe(i).iresp.bits.uop := ldq(ldq_idx).bits.uop
        io.core.exe(i).fresp.bits.uop := ldq(ldq_idx).bits.uop
        io.core.exe(i).iresp.valid     := send_iresp && !ignore
        io.core.exe(i).iresp.bits.data := io.dmem.resp(i).bits.data
        io.core.exe(i).fresp.valid     := send_fresp && !ignore
        io.core.exe(i).fresp.bits.data := io.dmem.resp(i).bits.data

        assert(send_iresp ^ send_fresp)
        dmem_resp_fired(i) := true.B

        ldq(ldq_idx).bits.succeeded      := io.core.exe(i).iresp.valid || io.core.exe(i).fresp.valid
        ldq(ldq_idx).bits.execute_ignore := false.B
        when (ignore) {
          // We were told to ignore this response because of order fail
          // Clear the execute bit, so we can re-fire this load
          ldq(ldq_idx).bits.executed := false.B
        }
      }
        .elsewhen (io.dmem.resp(i).bits.uop.uses_stq)
      {
        assert(!io.dmem.resp(i).bits.is_hella)
        stq(io.dmem.resp(i).bits.uop.stq_idx).bits.succeeded := true.B
        when (io.dmem.resp(i).bits.uop.is_amo) {
          dmem_resp_fired(i) := true.B
          io.core.exe(i).iresp.valid     := true.B
          io.core.exe(i).iresp.bits.uop  := stq(io.dmem.resp(i).bits.uop.stq_idx).bits.uop
          io.core.exe(i).iresp.bits.data := io.dmem.resp(i).bits.data
        }
      }
    }

    when (dmem_resp_fired(i) && wb_forward_valid(i))
    {
      // Twiddle thumbs. Can't forward because dcache response takes precedence
    }
      .elsewhen (!dmem_resp_fired(i) && wb_forward_valid(i))
    {
      val f_idx       = wb_forward_ldq_idx(i)
      val forward_uop = ldq(f_idx).bits.uop
      val stq_e       = stq(wb_forward_stq_idx(i))
      val data_ready  = stq_e.bits.data.valid
      val live        = !IsKilledByBranch(io.core.brinfo, forward_uop)
      val storegen = new freechips.rocketchip.rocket.StoreGen(
                                stq_e.bits.uop.mem_size, stq_e.bits.addr.bits,
                                stq_e.bits.data.bits, coreDataBytes)
      val loadgen  = new freechips.rocketchip.rocket.LoadGen(
                                forward_uop.mem_size, forward_uop.mem_signed,
                                wb_forward_ld_addr(i),
                                storegen.data, false.B, coreDataBytes)

      io.core.exe(i).iresp.valid := (forward_uop.dst_rtype === RT_FIX) && data_ready && live
      io.core.exe(i).fresp.valid := (forward_uop.dst_rtype === RT_FLT) && data_ready && live
      io.core.exe(i).iresp.bits.uop  := forward_uop
      io.core.exe(i).fresp.bits.uop  := forward_uop
      io.core.exe(i).iresp.bits.data := loadgen.data
      io.core.exe(i).fresp.bits.data := loadgen.data

      when (data_ready && live) {
        ldq(f_idx).bits.succeeded := data_ready
        ldq(f_idx).bits.forward_std_val := true.B
        ldq(f_idx).bits.forward_stq_idx := wb_forward_stq_idx(i)
      }
      assert(!ldq(f_idx).bits.execute_ignore)
    }

    when (io.core.exe(i).iresp.valid && io.core.exe(i).iresp.bits.uop.uses_ldq) {
      succeeding_ld_mask(io.core.exe(i).iresp.bits.uop.ldq_idx) := true.B
    } .elsewhen (io.core.exe(i).fresp.valid && io.core.exe(i).fresp.bits.uop.uses_ldq) {
      succeeding_ld_mask(io.core.exe(i).fresp.bits.uop.ldq_idx) := true.B
    }
  }

  // Initially assume the speculative load wakeup failed
  io.core.ld_miss         := RegNext(io.core.spec_ld_wakeup.valid)
  when (io.core.exe(0).iresp.valid && io.core.exe(0).iresp.bits.uop.ldq_idx === RegNext(mem_incoming_uop(0).ldq_idx)) {
    // We correcty speculated last cycle, so we don't send miss signal
    io.core.ld_miss := false.B
  }


  // detect which loads get marked as failures, but broadcast to the ROB the oldest failing load
  // TODO encapsulate this in an age-based  priority-encoder
  //   val l_idx = AgePriorityEncoder((Vec(Vec.tabulate(NUM_LDQ_ENTRIES)(i => failed_loads(i) && i.U >= laq_head)
  //   ++ failed_loads)).asUInt)
  val temp_bits = (VecInit(VecInit.tabulate(NUM_LDQ_ENTRIES)(i =>
    failed_loads(i) && i.U >= ldq_head) ++ failed_loads)).asUInt
  val l_idx = PriorityEncoder(temp_bits)

  val mem_xcpt_older = IsOlder(mem_xcpt_uop.ldq_idx, l_idx, ldq_head)
  val r_xcpt_uop = Mux(mem_xcpt_valid && (mem_xcpt_older || !failed_loads.reduce(_|_)), mem_xcpt_uop,
                       ldq(Mux(l_idx >= NUM_LDQ_ENTRIES.U, l_idx - NUM_LDQ_ENTRIES.U, l_idx)).bits.uop)

  // one exception port, but multiple causes!
  // - 1) the incoming store-address finds a faulting load (it is by definition younger)
  // - 2) the incoming load or store address is excepting. It must be older and thus takes precedent.
  val r_xcpt_valid = RegInit(false.B)
  val r_xcpt       = Reg(new Exception)

  r_xcpt_valid := (failed_loads.reduce(_|_) || mem_xcpt_valid) &&
                   !io.core.exception &&
                   !IsKilledByBranch(io.core.brinfo, r_xcpt_uop)
  r_xcpt.uop         := r_xcpt_uop
  r_xcpt.uop.br_mask := GetNewBrMask(io.core.brinfo, r_xcpt_uop)
  r_xcpt.cause       := Mux(mem_xcpt_valid, mem_xcpt_cause, MINI_EXCEPTION_MEM_ORDERING)
  r_xcpt.badvaddr    := mem_xcpt_vaddr // TODO is there another register we can use instead?

  io.core.lxcpt.valid := r_xcpt_valid && !io.core.exception && !IsKilledByBranch(io.core.brinfo, r_xcpt.uop)
  io.core.lxcpt.bits  := r_xcpt

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
    when (stq(stq_head).bits.uop.is_fence && !io.dmem.ordered) {
      io.dmem.force_order := true.B
    }
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
      assert ((ldq(idx).bits.executed || ldq(idx).bits.forward_std_val) && ldq(idx).bits.succeeded ,
        "[lsu] trying to commit an un-executed load entry.")

      ldq(idx).valid                 := false.B
      ldq(idx).bits.addr.valid       := false.B
      ldq(idx).bits.executed         := false.B
      ldq(idx).bits.succeeded        := false.B
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
    can_fire_hella_incoming(0) := true.B

    hella_data := io.hellacache.s1_data
    hella_xcpt := hella_dtlb_resp


    when (io.hellacache.s1_kill) {
      when (will_fire_hella_incoming(0) && dmem_req_fire(0)) {
        hella_state := h_dead
      } .otherwise {
        hella_state := h_ready
      }
    } .elsewhen (will_fire_hella_incoming(0) && dmem_req_fire(0)) {
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
    for (i <- 0 until memWidth) {
      when (io.dmem.resp(i).valid && io.dmem.resp(i).bits.is_hella) {
        hella_state := h_ready

        io.hellacache.resp.valid       := true.B
        io.hellacache.resp.bits.addr   := hella_req.addr
        io.hellacache.resp.bits.tag    := hella_req.tag
        io.hellacache.resp.bits.cmd    := hella_req.cmd
        io.hellacache.resp.bits.signed := hella_req.signed
        io.hellacache.resp.bits.size   := hella_req.size
        io.hellacache.resp.bits.data   := io.dmem.resp(i).bits.data
      } .elsewhen (io.dmem.nack(i).valid && io.dmem.nack(i).bits.is_hella) {
        hella_state := h_replay
      }
    }
  } .elsewhen (hella_state === h_replay) {
    can_fire_hella_wakeup(0) := true.B

    when (will_fire_hella_wakeup(0) && dmem_req_fire(0)) {
      hella_state := h_wait
    }
  } .elsewhen (hella_state === h_dead) {
    for (i <- 0 until memWidth) {
      when (io.dmem.resp(i).valid && io.dmem.resp(i).bits.is_hella) {
        hella_state := h_ready
      }
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
