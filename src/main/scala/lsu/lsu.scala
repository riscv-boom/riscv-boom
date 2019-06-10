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
  val req         = new DecoupledIO(new BoomDCacheReq)
  // In LSU's LCAM search stage, kill if order fail (or forwarding possible)
  val s1_kill     = Output(Bool())
  // Get a request any cycle
  val resp        = Flipped(new ValidIO(new BoomDCacheResp))
  // In our response stage, if we get a nack, we need to reexecute
  val nack        = Flipped(new ValidIO(new BoomDCacheReq))

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


  val dtlb = Module(new rocket.TLB(
    instruction = false, lgMaxSize = log2Ceil(coreDataBytes), rocket.TLBConfig(dcacheParams.nTLBEntries)))
  dontTouch(dtlb.io)

  io.ptw <> dtlb.io.ptw


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

  val mem_xcpt_valid = RegInit(false.B)


  //---------------------------------------
  // Can-fire logic and wakeup/retry select
  //
  // First we determine what operations are waiting to execute.
  // These are the "can_fire" signals

  val exe_req = io.core.exe.req

  // Can we fire an incoming load
  val ldq_incoming_idx = exe_req.bits.uop.ldq_idx
  val ldq_incoming_e   = ldq(ldq_incoming_idx)
  val can_fire_load_incoming = exe_req.valid && exe_req.bits.uop.ctrl.is_load

  // Can we fire an incoming store addrgen + store datagen
  val stq_incoming_idx = exe_req.bits.uop.stq_idx
  val stq_incoming_e   = stq(stq_incoming_idx)
  val can_fire_stad_incoming = exe_req.valid && exe_req.bits.uop.ctrl.is_sta && exe_req.bits.uop.ctrl.is_std

  // Can we fire an incoming store addrgen
  val can_fire_sta_incoming  = exe_req.valid && exe_req.bits.uop.ctrl.is_sta && !exe_req.bits.uop.ctrl.is_std

  // Can we fire an incoming store datagen
  val can_fire_std_incoming  = exe_req.valid && exe_req.bits.uop.ctrl.is_std && !exe_req.bits.uop.ctrl.is_sta

  // Can we fire an incoming sfence
  val can_fire_sfence        = exe_req.valid && exe_req.bits.sfence.valid


  // Don't wakeup a load if we just sent it last cycle or two cycles ago
  val block_load_mask    = WireInit(VecInit((0 until NUM_LDQ_ENTRIES).map(x=>false.B)))
  val p1_block_load_mask = RegNext(block_load_mask)
  val p2_block_load_mask = RegNext(p1_block_load_mask)

  // Can we retry a load that missed in the TLB
  val ldq_retry_idx = RegNext(AgePriorityEncoder((0 until NUM_LDQ_ENTRIES).map(i => {
    val e = ldq(i).bits
    val block = block_load_mask(i) || p1_block_load_mask(i)
    e.addr.valid && e.addr_is_virtual && !block
  }), ldq_head))
  val ldq_retry_e            = ldq(ldq_retry_idx)
  val can_fire_load_retry    = ( ldq_retry_e.valid                            &&
                                 ldq_retry_e.bits.addr.valid                  &&
                                 ldq_retry_e.bits.addr_is_virtual             &&
                                !p1_block_load_mask(ldq_retry_idx)            &&
                                !p2_block_load_mask(ldq_retry_idx)            &&
                                RegNext(dtlb.io.req.ready)                    &&
                                !ldq_retry_e.bits.order_fail)

  // Can we retry a store addrgen that missed in the TLB
  val stq_retry_idx = RegNext(AgePriorityEncoder(stq.map(e=>e.bits.addr.valid && e.bits.addr_is_virtual), stq_commit_head))
  val stq_retry_e   = stq(stq_retry_idx)
  val can_fire_sta_retry     = ( stq_retry_e.valid                            &&
                                 stq_retry_e.bits.addr.valid                  &&
                                 stq_retry_e.bits.addr_is_virtual)

  // Can we commit a store
  val stq_commit_e  = stq(stq_execute_head)
  val can_fire_store_commit  = ( stq_commit_e.valid                           &&
                                !stq_commit_e.bits.uop.is_fence               &&
                                !mem_xcpt_valid                               &&
                                !stq_commit_e.bits.uop.exception              &&
                                (stq_commit_e.bits.committed || ( stq_commit_e.bits.uop.is_amo      &&
                                                                  stq_commit_e.bits.addr.valid      &&
                                                                 !stq_commit_e.bits.addr_is_virtual &&
                                                                  stq_commit_e.bits.data.valid)))

  // Can we wakeup a load that was nack'd
  val ldq_wakeup_idx = RegNext(AgePriorityEncoder((0 until NUM_LDQ_ENTRIES).map(i=> {
    val e = ldq(i).bits
    val block = block_load_mask(i) || p1_block_load_mask(i)
    e.addr.valid && !e.executed && !e.succeeded && !e.addr_is_virtual && !block
  }), ldq_head))

  val ldq_wakeup_e   = ldq(ldq_wakeup_idx)
  val can_fire_load_wakeup = ( ldq_wakeup_e.valid                      &&
                               ldq_wakeup_e.bits.addr.valid            &&
                              !ldq_wakeup_e.bits.succeeded             &&
                              !ldq_wakeup_e.bits.addr_is_virtual       &&
                              !ldq_wakeup_e.bits.executed              &&
                              !ldq_wakeup_e.bits.order_fail            &&
                              !p1_block_load_mask(ldq_wakeup_idx)      &&
                              !p2_block_load_mask(ldq_wakeup_idx)      &&
                              (!ldq_wakeup_e.bits.addr_is_uncacheable || (io.core.commit_load_at_rob_head && ldq_head === ldq_wakeup_idx)))

  // Can we fire an incoming hellacache request
  val can_fire_hella_incoming  = WireInit(false.B) // This is assigned to in the hellashim controller

  // Can we fire a hellacache request that the dcache nack'd
  val can_fire_hella_wakeup    = WireInit(false.B) // This is assigned to in the hellashim controller

  //---------------------------------------------------------
  // Controller logic. Arbitrate which request actually fires

  var tlb_avail  = true.B
  var dc_avail   = true.B
  var lcam_avail = true.B
  var rob_avail  = true.B

  def lsu_sched(can_fire: Bool, uses_tlb:Boolean, uses_dc:Boolean, uses_lcam: Boolean, uses_rob:Boolean): Bool = {
    val will_fire = can_fire && !(uses_tlb.B && !tlb_avail) &&
                                !(uses_lcam.B && !lcam_avail) &&
                                !(uses_dc.B && !dc_avail) &&
                                !(uses_rob.B && !rob_avail)
    tlb_avail  = tlb_avail  && !(will_fire && uses_tlb.B)
    lcam_avail = lcam_avail && !(will_fire && uses_lcam.B)
    dc_avail   = dc_avail   && !(will_fire && uses_dc.B)
    rob_avail  = rob_avail  && !(will_fire && uses_rob.B)
    dontTouch(will_fire) // dontTouch these so we can inspect the will_fire signals
    will_fire
  }

  // The order of these statements is the priority
  // Some restrictions
  //  - Incoming ops must get precedence, can't backpresure memaddrgen
  //  - Incoming hellacache ops must get precedence over retrying ops (PTW must get precedence over retrying translation)
  val will_fire_load_incoming  = lsu_sched(can_fire_load_incoming  , true , true , true , false) // TLB , DC , LCAM
  val will_fire_stad_incoming  = lsu_sched(can_fire_stad_incoming  , true , false, true , true)  // TLB ,    , LCAM , ROB
  val will_fire_sta_incoming   = lsu_sched(can_fire_sta_incoming   , true , false, true , true)  // TLB ,    , LCAM , ROB
  val will_fire_std_incoming   = lsu_sched(can_fire_std_incoming   , false, false, false, true)  //                 , ROB
  val will_fire_sfence         = lsu_sched(can_fire_sfence         , true , false, false, true)  // TLB ,    ,      , ROB
  val will_fire_hella_incoming = lsu_sched(can_fire_hella_incoming , true , true , false, false) // TLB , DC
  val will_fire_hella_wakeup   = lsu_sched(can_fire_hella_wakeup   , false, true , false, false) //     , DC
  val will_fire_load_retry     = lsu_sched(can_fire_load_retry     , true , true , true , false) // TLB , DC , LCAM
  val will_fire_sta_retry      = lsu_sched(can_fire_sta_retry      , true , false, true , true)  // TLB ,    , LCAM , ROB
  val will_fire_store_commit   = lsu_sched(can_fire_store_commit   , false, true , false, false) //     , DC
  val will_fire_load_wakeup    = lsu_sched(can_fire_load_wakeup    , false, true , true , false) //     , DC , LCAM

  assert(!(exe_req.valid && !(will_fire_load_incoming || will_fire_stad_incoming || will_fire_sta_incoming || will_fire_std_incoming || will_fire_sfence)))

  when (will_fire_load_wakeup) {
    block_load_mask(ldq_wakeup_idx)           := true.B
  } .elsewhen (will_fire_load_incoming) {
    block_load_mask(exe_req.bits.uop.ldq_idx) := true.B
  } .elsewhen (will_fire_load_retry) {
    block_load_mask(ldq_retry_idx)            := true.B
  }


  //--------------------------------------------
  // TLB Access

  val hella_sfence = Wire(Valid(new rocket.SFenceReq))
  hella_sfence.valid     := hella_req.cmd === rocket.M_SFENCE
  hella_sfence.bits.rs1  := hella_req.size(0)
  hella_sfence.bits.rs2  := hella_req.size(1)
  hella_sfence.bits.addr := hella_req.addr
  hella_sfence.bits.asid := io.hellacache.s1_data.data

  val exe_tlb_uop = Mux(will_fire_load_incoming ||
                        will_fire_stad_incoming ||
                        will_fire_sta_incoming  ||
                        will_fire_sfence          , exe_req.bits.uop,
                    Mux(will_fire_load_retry      , ldq_retry_e.bits.uop,
                    Mux(will_fire_sta_retry       , stq_retry_e.bits.uop,
                    Mux(will_fire_hella_incoming  , NullMicroOp,
                                                    NullMicroOp))))

  val exe_vaddr   = Mux(will_fire_load_incoming ||
                        will_fire_stad_incoming ||
                        will_fire_sta_incoming  ||
                        will_fire_sfence          , exe_req.bits.addr,
                    Mux(will_fire_load_retry      , ldq_retry_e.bits.addr.bits,
                    Mux(will_fire_sta_retry       , stq_retry_e.bits.addr.bits,
                    Mux(will_fire_hella_incoming  , hella_req.addr,
                                                    0.U))))

  val exe_sfence  = Mux(will_fire_load_incoming ||
                        will_fire_stad_incoming ||
                        will_fire_sta_incoming    , (0.U).asTypeOf(Valid(new rocket.SFenceReq)),
                    Mux(will_fire_sfence          , exe_req.bits.sfence,
                    Mux(will_fire_load_retry    ||
                        will_fire_sta_retry       , (0.U).asTypeOf(Valid(new rocket.SFenceReq)),
                    Mux(will_fire_hella_incoming  , hella_sfence,
                                                    (0.U).asTypeOf(Valid(new rocket.SFenceReq))))))

  val exe_size   = Mux(will_fire_load_incoming   ||
                       will_fire_stad_incoming   ||
                       will_fire_sta_incoming    ||
                       will_fire_sfence          ||
                       will_fire_load_retry      ||
                       will_fire_sta_retry       , exe_tlb_uop.mem_size,
                   Mux(will_fire_hella_incoming  , hella_req.size,
                                                   0.U))
  val exe_cmd    = Mux(will_fire_load_incoming   ||
                       will_fire_stad_incoming   ||
                       will_fire_sta_incoming    ||
                       will_fire_sfence          ||
                       will_fire_load_retry      ||
                       will_fire_sta_retry       , exe_tlb_uop.mem_cmd,
                   Mux(will_fire_hella_incoming  , hella_req.cmd,
                                                   0.U))

  val exe_passthr= Mux(will_fire_load_incoming   ||
                       will_fire_stad_incoming   ||
                       will_fire_sta_incoming    ||
                       will_fire_sfence          ||
                       will_fire_load_retry      ||
                       will_fire_sta_retry       , false.B,
                   Mux(will_fire_hella_incoming  , hella_req.phys,
                                                   false.B))
  val exe_kill   = Mux(will_fire_load_incoming   ||
                       will_fire_stad_incoming   ||
                       will_fire_sta_incoming    ||
                       will_fire_sfence          ||
                       will_fire_load_retry      ||
                       will_fire_sta_retry       , false.B,
                   Mux(will_fire_hella_incoming  , io.hellacache.s1_kill,
                                                   false.B))
  dtlb.io.req.valid            := !tlb_avail
  dtlb.io.req.bits.vaddr       := exe_vaddr
  dtlb.io.req.bits.size        := exe_size
  dtlb.io.req.bits.cmd         := exe_cmd
  dtlb.io.req.bits.passthrough := exe_passthr
  dtlb.io.kill                 := exe_kill
  dtlb.io.sfence               := exe_sfence

  // exceptions
  val ma_ld = will_fire_load_incoming && exe_req.bits.mxcpt.valid // We get ma_ld in memaddrcalc
  val ma_st = (will_fire_sta_incoming || will_fire_stad_incoming) && exe_req.bits.mxcpt.valid // We get ma_ld in memaddrcalc
  val pf_ld = dtlb.io.req.valid && dtlb.io.resp.pf.ld && (exe_tlb_uop.uses_ldq || exe_tlb_uop.is_amo) // TODO: uses_ldq is not right here
  val pf_st = dtlb.io.req.valid && dtlb.io.resp.pf.st && exe_tlb_uop.uses_stq
  val ae_ld = dtlb.io.req.valid && dtlb.io.resp.ae.ld && (exe_tlb_uop.uses_ldq|| exe_tlb_uop.is_amo)
  val ae_st = dtlb.io.req.valid && dtlb.io.resp.ae.st && exe_tlb_uop.uses_stq

  // TODO check for xcpt_if and verify that never happens on non-speculative instructions.
  mem_xcpt_valid := (pf_ld || pf_st || ae_ld || ae_st || ma_ld || ma_st) &&
                     !io.core.exception &&
                     !IsKilledByBranch(io.core.brinfo, exe_tlb_uop)

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

  assert (stq(stq_execute_head).valid ||
          stq_head === stq_execute_head || stq_tail === stq_execute_head,
            "stq_execute_head got off track.")

  //------------------------------
  // Issue Someting to Memory
  //
  // A memory op can come from many different places
  // The address either was freshly translated, or we are
  // reading a physical address from the LDQ,STQ, or the HellaCache adapter


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

  when (will_fire_load_incoming) {
    io.dmem.req.valid      := !tlb_miss && !tlb_addr_uncacheable
    io.dmem.req.bits.addr  := exe_tlb_paddr
    io.dmem.req.bits.uop   := exe_tlb_uop

    ldq(ldq_incoming_idx).bits.executed := io.dmem.req.fire()
    assert(!ldq_incoming_e.bits.executed)
  } .elsewhen (will_fire_load_retry) {
    io.dmem.req.valid      := !tlb_miss && !tlb_addr_uncacheable
    io.dmem.req.bits.addr  := exe_tlb_paddr
    io.dmem.req.bits.uop   := exe_tlb_uop

    ldq(ldq_retry_idx).bits.executed := io.dmem.req.fire()
    assert(!ldq_retry_e.bits.executed)
  } .elsewhen (will_fire_store_commit) {
    io.dmem.req.valid         := true.B
    io.dmem.req.bits.addr     := stq_commit_e.bits.addr.bits
    io.dmem.req.bits.data     := (new freechips.rocketchip.rocket.StoreGen(
                                  stq_commit_e.bits.uop.mem_size, 0.U,
                                  stq_commit_e.bits.data.bits,
                                  coreDataBytes)).data
    io.dmem.req.bits.uop      := stq_commit_e.bits.uop

    stq_execute_head                     := Mux(io.dmem.req.fire(),
                                                WrapInc(stq_execute_head, NUM_STQ_ENTRIES),
                                                stq_execute_head)

    stq(stq_execute_head).bits.succeeded := false.B
  } .elsewhen (will_fire_load_wakeup) {
    io.dmem.req.valid      := true.B
    io.dmem.req.bits.addr  := ldq_wakeup_e.bits.addr.bits
    io.dmem.req.bits.uop   := ldq_wakeup_e.bits.uop

    ldq(ldq_wakeup_idx).bits.executed := io.dmem.req.fire()

    assert(!ldq_wakeup_e.bits.executed && !ldq_wakeup_e.bits.addr_is_virtual)
  } .elsewhen (will_fire_hella_incoming) {
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

  //-------------------------------------------------------------
  // Write Addr into the LAQ/SAQ
  when (will_fire_load_incoming || will_fire_load_retry)
  {
    val ldq_idx = Mux(will_fire_load_incoming, ldq_incoming_idx, ldq_retry_idx)
    ldq(ldq_idx).bits.addr.valid          := true.B
    ldq(ldq_idx).bits.addr.bits           := Mux(tlb_miss, exe_vaddr, exe_tlb_paddr)
    ldq(ldq_idx).bits.uop.pdst            := exe_tlb_uop.pdst
    ldq(ldq_idx).bits.addr_is_virtual     := tlb_miss
    ldq(ldq_idx).bits.addr_is_uncacheable := tlb_addr_uncacheable && !tlb_miss

    assert(!(will_fire_load_incoming && ldq_incoming_e.bits.addr.valid),
      "[lsu] Incoming load is overwriting a valid address")
  }

  when (will_fire_sta_incoming || will_fire_stad_incoming || will_fire_sta_retry)
  {
    val stq_idx = Mux(will_fire_sta_incoming || will_fire_stad_incoming,
      stq_incoming_idx, stq_retry_idx)

    stq(stq_idx).bits.addr.valid := !pf_st // Prevent AMOs from executing!
    stq(stq_idx).bits.addr.bits  := Mux(tlb_miss, exe_vaddr, exe_tlb_paddr)
    stq(stq_idx).bits.uop.pdst   := exe_tlb_uop.pdst // Needed for AMOs
    stq(stq_idx).bits.addr_is_virtual := tlb_miss

    assert(!(will_fire_sta_incoming && stq_incoming_e.bits.addr.valid),
      "[lsu] Incoming store is overwriting a valid address")

  }

  //-------------------------------------------------------------
  // Write data into the STQ

  io.core.fp_stdata.ready := !will_fire_std_incoming && !will_fire_stad_incoming
  when (will_fire_std_incoming || will_fire_stad_incoming || io.core.fp_stdata.fire())
  {
    val sidx = Mux(will_fire_std_incoming || will_fire_stad_incoming,
      stq_incoming_idx,
      io.core.fp_stdata.bits.uop.stq_idx)
    stq(sidx).bits.data.valid := true.B
    stq(sidx).bits.data.bits  := Mux(will_fire_std_incoming || will_fire_stad_incoming,
      exe_req.bits.data,
      io.core.fp_stdata.bits.data)

    assert(!(stq(sidx).bits.data.valid),
      "[lsu] Incoming store is overwriting a valid data entry")
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
  val fired_stdf_incoming  = RegNext(will_fire_stdf_incoming)
  val fired_sfence         = RegNext(will_fire_sfence)
  val fired_load_retry     = RegNext(will_fire_load_retry)
  val fired_sta_retry      = RegNext(will_fire_sta_retry)
  val fired_store_commit   = RegNext(will_fire_store_commit)
  val fired_load_wakeup    = RegNext(will_fire_load_wakeup)
  val fired_hella_incoming = RegNext(will_fire_hella_incoming)
  val fired_hella_wakeup   = RegNext(will_fire_hella_wakeup)

  val mem_incoming_uop     = RegNext(UpdateBrMask(io.core.brinfo, exe_req.bits.uop))
  val mem_ldq_incoming_e   = RegNext(UpdateBrMask(io.core.brinfo, ldq_incoming_e))
  val mem_stq_incoming_e   = RegNext(UpdateBrMask(io.core.brinfo, stq_incoming_e))
  val mem_ldq_wakeup_e     = RegNext(UpdateBrMask(io.core.brinfo, ldq_wakeup_e))
  val mem_ldq_retry_e      = RegNext(UpdateBrMask(io.core.brinfo, ldq_retry_e))
  val mem_stq_retry_e      = RegNext(UpdateBrMask(io.core.brinfo, stq_retry_e))
  val mem_ldq_e            = Mux(fired_load_incoming, mem_ldq_incoming_e,
                             Mux(fired_load_retry   , mem_ldq_retry_e,
                             Mux(fired_load_wakeup  , mem_ldq_wakeup_e, (0.U).asTypeOf(Valid(new LDQEntry)))))
  val mem_stq_e            = Mux(fired_stad_incoming ||
                                 fired_sta_incoming , mem_stq_incoming_e,
                             Mux(fired_sta_retry    , mem_stq_retry_e, (0.U).asTypeOf(Valid(new STQEntry))))
  val mem_stdf_uop         = RegNext(UpdateBrMask(io.core.brinfo, io.core.fp_stdata.bits.uop))


  val mem_tlb_miss             = RegNext(dtlb.io.req.valid && tlb_miss)
  val mem_tlb_addr_uncacheable = RegNext(dtlb.io.req.valid && tlb_addr_uncacheable)
  val mem_paddr                = RegNext(io.dmem.req.bits.addr)

  // Task 1: Clr ROB busy bit
  val clr_bsy_valid   = RegInit(false.B)
  val clr_bsy_rob_idx = Reg(UInt(robAddrSz.W))
  val clr_bsy_brmask  = Reg(UInt(MAX_BR_COUNT.W))
  clr_bsy_valid   := false.B
  clr_bsy_rob_idx := 0.U
  clr_bsy_brmask  := 0.U

  val stdf_clr_bsy_valid   = RegInit(false.B)
  val stdf_clr_bsy_rob_idx = Reg(UInt(robAddrSz.W))
  val stdf_clr_bsy_brmask  = Reg(UInt(MAX_BR_COUNT.W))
  stdf_clr_bsy_valid   := false.B
  stdf_clr_bsy_rob_idx := 0.U
  stdf_clr_bsy_brmask  := 0.U

  when (fired_stad_incoming) {
    clr_bsy_valid   := mem_stq_incoming_e.valid            &&
                       !mem_tlb_miss                       &&
                       !mem_stq_incoming_e.bits.uop.is_amo &&
                       !IsKilledByBranch(io.core.brinfo, mem_stq_incoming_e.bits.uop)
    clr_bsy_rob_idx := mem_stq_incoming_e.bits.uop.rob_idx
    clr_bsy_brmask  := GetNewBrMask(io.core.brinfo, mem_stq_incoming_e.bits.uop)
  } .elsewhen (fired_sta_incoming) {
    clr_bsy_valid   := mem_stq_incoming_e.valid            &&
                       mem_stq_incoming_e.bits.data.valid  &&
                       !mem_tlb_miss                       &&
                       !mem_stq_incoming_e.bits.uop.is_amo &&
                       !IsKilledByBranch(io.core.brinfo, mem_stq_incoming_e.bits.uop)
    clr_bsy_rob_idx := mem_stq_incoming_e.bits.uop.rob_idx
    clr_bsy_brmask  := GetNewBrMask(io.core.brinfo, mem_stq_incoming_e.bits.uop)
  } .elsewhen (fired_std_incoming) {
    clr_bsy_valid   := mem_stq_incoming_e.valid                 &&
                       mem_stq_incoming_e.bits.addr.valid       &&
                       !mem_stq_incoming_e.bits.addr_is_virtual &&
                       !mem_stq_incoming_e.bits.uop.is_amo      &&
                       !IsKilledByBranch(io.core.brinfo, mem_stq_incoming_e.bits.uop)
    clr_bsy_rob_idx := mem_stq_incoming_e.bits.uop.rob_idx
    clr_bsy_brmask  := GetNewBrMask(io.core.brinfo, mem_stq_incoming_e.bits.uop)
  } .elsewhen (fired_sfence) {
    clr_bsy_valid   := true.B
    clr_bsy_rob_idx := mem_incoming_uop.rob_idx
    clr_bsy_brmask  := GetNewBrMask(io.core.brinfo, mem_incoming_uop)
  } .elsewhen (fired_sta_retry) {
    clr_bsy_valid   := mem_stq_retry_e.valid            &&
                       mem_stq_retry_e.bits.data.valid  &&
                       !mem_tlb_miss                    &&
                       !mem_stq_retry_e.bits.uop.is_amo &&
                       !IsKilledByBranch(io.core.brinfo, mem_stq_retry_e.bits.uop)
    clr_bsy_rob_idx := mem_stq_retry_e.bits.uop.rob_idx
    clr_bsy_brmask  := GetNewBrMask(io.core.brinfo, mem_stq_retry_e.bits.uop)
  }

  when (fired_stdf_incoming) {
    val s_idx = mem_stdf_uop.stq_idx
    stdf_clr_bsy_valid   := stq(s_idx).valid                 &&
                            stq(s_idx).bits.addr.valid       &&
                            !stq(s_idx).bits.addr_is_virtual &&
                            !stq(s_idx).bits.uop.is_amo      &&
                            !IsKilledByBranch(io.core.brinfo, mem_stdf_uop)
    stdf_clr_bsy_rob_idx := mem_stdf_uop.rob_idx
    stdf_clr_bsy_brmask  := GetNewBrMask(io.core.brinfo, mem_stdf_uop)
  }

  io.core.clr_bsy(0).valid := clr_bsy_valid &&
                             !IsKilledByBranch(io.core.brinfo, clr_bsy_brmask) &&
                             !io.core.exception && !RegNext(io.core.exception)
  io.core.clr_bsy(0).bits  := clr_bsy_rob_idx

  io.core.clr_bsy(1).valid := stdf_clr_bsy_valid &&
                             !IsKilledByBranch(io.core.brinfo, stdf_clr_bsy_brmask) &&
                             !io.core.exception && !RegNext(io.core.exception)
  io.core.clr_bsy(1).bits  := stdf_clr_bsy_rob_idx



  // Task 2: Do LD-LD. ST-LD searches for ordering failures
  //         Do LD-ST search for forwarding opportunities
  // We have the opportunity to kill a request we sent last cycle. Use it wisely!

  // We translated a store last cycle
  val do_st_search = (fired_stad_incoming || fired_sta_incoming || fired_sta_retry) && !mem_tlb_miss
  // We translated a load last cycle
  val do_ld_search = ((fired_load_incoming || fired_load_retry) && !mem_tlb_miss) ||
                     fired_load_wakeup

  assert(!(do_st_search && do_ld_search))

  // Store addrs don't go to memory yet, get it from the TLB response
  // Load wakeups don't go through TLB, get it through memory
  // Load incoming and load retries go through both
  val lcam_addr  = Mux(fired_stad_incoming || fired_sta_incoming || fired_sta_retry,
                       RegNext(exe_tlb_paddr), mem_paddr)
  val lcam_uop   = Mux(do_st_search, mem_stq_e.bits.uop,
                   Mux(do_ld_search, mem_ldq_e.bits.uop, NullMicroOp))

  val lcam_mask  = GenByteMask(lcam_addr, lcam_uop.mem_size)
  val lcam_st_dep_mask = mem_ldq_e.bits.st_dep_mask
  val lcam_is_fence = lcam_uop.is_fence
  val lcam_ldq_idx  = Mux(fired_load_incoming, mem_incoming_uop.ldq_idx,
                      Mux(fired_load_wakeup  , RegNext(ldq_wakeup_idx),
                      Mux(fired_load_retry   , RegNext(ldq_retry_idx), 0.U)))
  val lcam_stq_idx  = Mux(fired_stad_incoming ||
                          fired_sta_incoming , mem_incoming_uop.stq_idx,
                      Mux(fired_sta_retry    , RegNext(stq_retry_idx), 0.U))


  val failed_loads = WireInit(VecInit((0 until NUM_LDQ_ENTRIES).map(x=>false.B)))
  val stld_order_fail = WireInit(false.B)
  val ldld_order_fail = WireInit(false.B)

  val can_forward = WireInit(!ldq(lcam_ldq_idx).bits.addr_is_uncacheable) // We might not be able to forward if order fail
  val ldst_forward_matches = WireInit(VecInit((0 until NUM_STQ_ENTRIES).map(x=>false.B)))

  // We might need to ignore something returning THIS cycle (next stage in pipe)
  // In that case writing it into the ldq register is to slow
  val mem_execute_ignore_valid   = WireInit(false.B)
  val mem_execute_ignore_ldq_idx = WireInit(0.U)

  for (i <- 0 until NUM_LDQ_ENTRIES) {
    val l_valid = ldq(i).valid
    val l_bits  = ldq(i).bits
    val l_addr  = ldq(i).bits.addr.bits
    val l_mask  = GenByteMask(l_addr, l_bits.uop.mem_size)

    val dword_addr_matches = lcam_addr(corePAddrBits-1,3) === l_addr(corePAddrBits-1,3)
    val mask_match = (l_mask & lcam_mask) === l_mask
    val l_is_succeeding = (io.dmem.resp.valid &&
                           io.dmem.resp.bits.uop.uses_ldq &&
                           io.dmem.resp.bits.uop.ldq_idx === i.U)

    // Searcher is a store
    when (do_st_search &&
          l_valid &&
          l_bits.addr.valid &&
          (l_bits.executed && !l_bits.execute_ignore) &&
          !l_bits.addr_is_virtual &&
          l_bits.st_dep_mask(lcam_stq_idx) &&
          dword_addr_matches) {
      // We are older than this load, which overlapped us.
      when (!l_bits.forward_std_val || // If the load wasn't forwarded, it definitely failed
        (l_bits.forward_stq_idx =/= lcam_stq_idx) && IsOlder(l_bits.forward_stq_idx, lcam_stq_idx, stq_head)) { // If the load forwarded from us, we might be ok
        when (l_bits.succeeded || l_is_succeeding) { // If the younger load already succeeded, we are screwed. Throw order fail
          ldq(i).bits.order_fail := true.B
          failed_loads(i)        := true.B
          stld_order_fail        := true.B
        } .otherwise { // If the younger load hasn't responded yet, tell it to kill its response
          ldq(i).bits.execute_ignore := true.B
          mem_execute_ignore_valid   := true.B
          mem_execute_ignore_ldq_idx := i.U
        }
      }
    } .elsewhen (do_ld_search &&
                 l_valid &&
                 l_bits.addr.valid &&
                 !l_bits.addr_is_virtual &&
                 dword_addr_matches &&
                 ((lcam_mask & l_mask) =/= 0.U)) {
      val searcher_is_older = IsOlder(lcam_ldq_idx, i.U, ldq_head)
      when (searcher_is_older) {
        when ((l_bits.executed && !l_bits.execute_ignore)) {
          when (l_bits.succeeded || l_is_succeeding) { // If the younger load is executing and succeeded, we are screwed. Throw order fail
            ldq(i).bits.order_fail := true.B
            failed_loads(i)        := true.B
            ldld_order_fail        := true.B
          } .otherwise { // The younger load hasn't returned yet, we can kill its response
            ldq(i).bits.execute_ignore := true.B
          }
        }
      } .elsewhen (lcam_ldq_idx =/= i.U) {
        // The load is older, and either it hasn't executed, it was nacked, or it is ignoring its response
        // we need to kill ourselves, and prevent forwarding
        val older_nacked = (io.dmem.nack.valid &&
                            io.dmem.nack.bits.uop.uses_ldq &&
                            io.dmem.nack.bits.uop.ldq_idx === i.U)
        when (!l_bits.executed || older_nacked || l_bits.execute_ignore) {
          io.dmem.s1_kill                 := RegNext(io.dmem.req.fire())
          ldq(lcam_ldq_idx).bits.executed := false.B
          can_forward                     := false.B
        }
      }
    }
  }
  for (i <- 0 until NUM_STQ_ENTRIES) {
    val s_addr = stq(i).bits.addr.bits
    val s_uop  = stq(i).bits.uop
    val dword_addr_matches = (lcam_st_dep_mask(i) &&
                              stq(i).bits.addr.valid &&
                              !stq(i).bits.addr_is_virtual &&
                              (s_addr(corePAddrBits-1,3) === lcam_addr(corePAddrBits-1,3)))
    val write_mask = GenByteMask(s_addr, s_uop.mem_size)
    when (do_ld_search && stq(i).valid) {
      when (((lcam_mask & write_mask) === lcam_mask) && !s_uop.is_fence && dword_addr_matches && can_forward) {
        ldst_forward_matches(i) := true.B
        io.dmem.s1_kill := RegNext(io.dmem.req.fire())
        ldq(lcam_ldq_idx).bits.executed := false.B
      } .elsewhen (((lcam_mask & write_mask) =/= 0.U) && dword_addr_matches) {
        io.dmem.s1_kill := RegNext(io.dmem.req.fire())
        ldq(lcam_ldq_idx).bits.executed := false.B
      } .elsewhen (lcam_st_dep_mask(i) && (s_uop.is_fence || s_uop.is_amo)) {
        io.dmem.s1_kill := RegNext(io.dmem.req.fire())
        ldq(lcam_ldq_idx).bits.executed := false.B
      }
    }
  }
  val forwarding_age_logic = Module(new ForwardingAgeLogic(NUM_STQ_ENTRIES))
  forwarding_age_logic.io.addr_matches    := ldst_forward_matches.asUInt
  forwarding_age_logic.io.youngest_st_idx := lcam_uop.stq_idx
  // when (ldst_forward_matches.reduce(_||_)) {
  //   ldq(lcam_ldq_idx).bits.forward_std_val := true.B
  //   ldq(lcam_ldq_idx).bits.forward_stq_idx := forwarding_age_logic.io.forwarding_idx
  // }
  val mem_forward_valid       = (ldst_forward_matches.reduce(_||_) &&
                                 !IsKilledByBranch(io.core.brinfo, lcam_uop))
  val mem_forward_ldq_idx     = lcam_ldq_idx
  val mem_forward_stq_idx     = forwarding_age_logic.io.forwarding_idx


  // Task 3: Clr unsafe bit in ROB for succesful translations
  //         Delay this a cycle to avoid going ahead of the exception broadcast
  //         The unsafe bit is cleared on the first translation, so no need to fire for load wakeups
  io.core.clr_unsafe.valid := RegNext((do_st_search || do_ld_search) && !fired_load_wakeup)
  io.core.clr_unsafe.bits  := RegNext(lcam_uop.rob_idx)


  // detect which loads get marked as failures, but broadcast to the ROB the oldest failing load
  // TODO encapsulate this in an age-based  priority-encoder
  //   val l_idx = AgePriorityEncoder((Vec(Vec.tabulate(NUM_LDQ_ENTRIES)(i => failed_loads(i) && i.U >= laq_head)
  //   ++ failed_loads)).asUInt)
  val temp_bits = (VecInit(VecInit.tabulate(NUM_LDQ_ENTRIES)(i =>
    failed_loads(i) && i.U >= ldq_head) ++ failed_loads)).asUInt
  val l_idx = PriorityEncoder(temp_bits)

  // one exception port, but multiple causes!
  // - 1) the incoming store-address finds a faulting load (it is by definition younger)
  // - 2) the incoming load or store address is excepting. It must be older and thus takes precedent.
  val r_xcpt_valid = RegInit(false.B)
  val r_xcpt       = Reg(new Exception)

  val mem_xcpt_uop = Mux(mem_xcpt_valid, RegNext(exe_tlb_uop),
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

  // Task 4: handle mem load killed, ldspecwakeup

  //-------------------------------------------------------------
  // Load Issue Datapath (ALL loads need to use this path,
  //    to handle forwarding from the STORE QUEUE, etc.)
  // search entire STORE QUEUE for match on load
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Writeback Cycle (St->Ld Forwarding Path)
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Handle Memory Responses and nacks
  //----------------------------------

  io.core.exe.iresp.valid := false.B
  io.core.exe.fresp.valid := false.B
  val dmem_resp_fired = WireInit(false.B)
  // Handle nacks
  when (io.dmem.nack.valid)
  {
    // We have to re-execute this!
    when (io.dmem.nack.bits.is_hella)
    {
      assert(hella_state === h_wait)
    }
      .elsewhen (io.dmem.nack.bits.uop.uses_ldq)
    {
      assert(ldq(io.dmem.nack.bits.uop.ldq_idx).bits.executed)
      ldq(io.dmem.nack.bits.uop.ldq_idx).bits.executed  := false.B
      ldq(io.dmem.nack.bits.uop.ldq_idx).bits.execute_ignore  := false.B
    }
      .otherwise
    {
      assert(io.dmem.nack.bits.uop.uses_stq)
      when (IsOlder(io.dmem.nack.bits.uop.stq_idx, stq_execute_head, stq_head)) {
        stq_execute_head := io.dmem.nack.bits.uop.stq_idx
      }
    }
  }
  // Handle the response
  when (io.dmem.resp.valid)
  {
    when (io.dmem.resp.bits.uop.uses_ldq)
    {
      assert(!io.dmem.resp.bits.is_hella)
      val ldq_idx = io.dmem.resp.bits.uop.ldq_idx
      val send_iresp = ldq(ldq_idx).bits.uop.dst_rtype === RT_FIX
      val send_fresp = ldq(ldq_idx).bits.uop.dst_rtype === RT_FLT

      io.core.exe.iresp.bits.uop := ldq(ldq_idx).bits.uop
      io.core.exe.fresp.bits.uop := ldq(ldq_idx).bits.uop
      io.core.exe.iresp.valid     := send_iresp && !ldq(ldq_idx).bits.execute_ignore
      io.core.exe.iresp.bits.data := io.dmem.resp.bits.data
      io.core.exe.fresp.valid     := send_fresp && !ldq(ldq_idx).bits.execute_ignore
      io.core.exe.fresp.bits.data := io.dmem.resp.bits.data

      assert(send_iresp ^ send_fresp)
      dmem_resp_fired := true.B

      ldq(ldq_idx).bits.succeeded      := io.core.exe.iresp.valid || io.core.exe.fresp.valid
      ldq(ldq_idx).bits.execute_ignore := false.B
    }
      .elsewhen (io.dmem.resp.bits.uop.uses_stq)
    {
      assert(!io.dmem.resp.bits.is_hella)
      stq(io.dmem.resp.bits.uop.stq_idx).bits.succeeded := true.B
      when (io.dmem.resp.bits.uop.is_amo) {
        dmem_resp_fired := true.B
        io.core.exe.iresp.valid     := true.B
        io.core.exe.iresp.bits.uop  := stq(io.dmem.resp.bits.uop.stq_idx).bits.uop
        io.core.exe.iresp.bits.data := io.dmem.resp.bits.data
      }
    }
  }

  when (dmem_resp_fired && RegNext(mem_forward_valid))
  {
      //   ldq(lcam_ldq_idx).bits.forward_std_val := true.B
  //   ldq(lcam_ldq_idx).bits.forward_stq_idx := forwarding_age_logic.io.forwarding_idx
    val f_idx = RegNext(mem_forward_ldq_idx)
    //    ldq(f_idx).bits.executed := false.B
    //assert(!ldq(f_idx).bits.executed)
    //ldq(f_idx).bits.execute_ignore := false.B
  }
    .elsewhen (!dmem_resp_fired && RegNext(mem_forward_valid))
  {
    val f_idx = RegNext(mem_forward_ldq_idx)
    val forward_uop = ldq(f_idx).bits.uop
    val stq_e = stq(RegNext(mem_forward_stq_idx))
    val data_ready = stq_e.bits.data.valid
    val live = !IsKilledByBranch(io.core.brinfo, forward_uop)
    val forward_data = LoadDataGenerator(stq_e.bits.data.bits,
                                         forward_uop.mem_size, forward_uop.mem_signed)
    io.core.exe.iresp.valid := (forward_uop.dst_rtype === RT_FIX) && data_ready && live
    io.core.exe.fresp.valid := (forward_uop.dst_rtype === RT_FLT) && data_ready && live
    io.core.exe.iresp.bits.uop  := forward_uop
    io.core.exe.fresp.bits.uop  := forward_uop
    io.core.exe.iresp.bits.data := forward_data
    io.core.exe.fresp.bits.data := forward_data

    when (data_ready && live) {
      ldq(f_idx).bits.succeeded := data_ready
      ldq(f_idx).bits.forward_std_val := true.B
      ldq(f_idx).bits.forward_stq_idx := RegNext(mem_forward_stq_idx)
    }
    // when (!data_ready) {
    //   ldq(f_idx).bits.executed := false.B
    // }
    assert(!ldq(f_idx).bits.execute_ignore)
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
      hella_state := h_ready

      io.hellacache.resp.valid       := true.B
      io.hellacache.resp.bits.addr   := hella_req.addr
      io.hellacache.resp.bits.tag    := hella_req.tag
      io.hellacache.resp.bits.cmd    := hella_req.cmd
      io.hellacache.resp.bits.signed := hella_req.signed
      io.hellacache.resp.bits.size   := hella_req.size
      io.hellacache.resp.bits.data   := io.dmem.resp.bits.data
    } .elsewhen (io.dmem.nack.valid && io.dmem.nack.bits.is_hella) {
      hella_state := h_replay
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
