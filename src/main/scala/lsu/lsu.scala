//******************************************************************************
// Copyright (c) 2012 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
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

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.Str

import boom.common._
import boom.exu.{BrUpdateInfo, Exception, FuncUnitResp, CommitSignals, ExeUnitResp}
import boom.util._

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

class LSUDMemIO(implicit p: Parameters, edge: TLEdgeOut) extends BoomBundle()(p)
{
  // In LSU's dmem stage, send the request
  val req         = new DecoupledIO(Vec(lsuWidth, Valid(new BoomDCacheReq)))
  // In LSU's LCAM search stage, kill if order fail (or forwarding possible)
  val s1_kill     = Output(Vec(lsuWidth, Bool()))
  // Get a request any cycle
  val resp        = Flipped(Vec(lsuWidth, new ValidIO(new BoomDCacheResp)))
  // The cache irrevocably accepted our store
  val store_ack   = Flipped(Vec(lsuWidth, new ValidIO(new MicroOp)))
  // In our response stage, if we get a nack, we need to reexecute
  val nack        = Flipped(Vec(lsuWidth, new ValidIO(new BoomDCacheReq)))

  val brupdate       = Output(new BrUpdateInfo)
  val exception    = Output(Bool())
  val rob_pnr_idx  = Output(UInt(robAddrSz.W))
  val rob_head_idx = Output(UInt(robAddrSz.W))

  val release = Flipped(new DecoupledIO(new TLBundleC(edge.bundle)))

  // Clears prefetching MSHRs
  val force_order  = Output(Bool())
  val ordered     = Input(Bool())

  val perf = Input(new Bundle {
    val acquire = Bool()
    val release = Bool()
  })

  override def cloneType = new LSUDMemIO().asInstanceOf[this.type]
}

class LSUCoreIO(implicit p: Parameters) extends BoomBundle()(p)
{

  val agen        = Flipped(Vec(lsuWidth, Valid(new FuncUnitResp(xLen))))
  val dgen        = Flipped(Vec(memWidth + 1, Valid(new ExeUnitResp(xLen))))

  val iresp       = Vec(lsuWidth, Valid(new ExeUnitResp(xLen)))
  val fresp       = Vec(lsuWidth, Valid(new ExeUnitResp(xLen)))

  val sfence      = Flipped(Valid(new rocket.SFenceReq))

  val dis_uops    = Flipped(Vec(coreWidth, Valid(new MicroOp)))
  val dis_ldq_idx = Output(Vec(coreWidth, UInt(ldqAddrSz.W)))
  val dis_stq_idx = Output(Vec(coreWidth, UInt(stqAddrSz.W)))

  val ldq_full    = Output(Vec(coreWidth, Bool()))
  val stq_full    = Output(Vec(coreWidth, Bool()))

  val commit      = Input(new CommitSignals)
  val commit_load_at_rob_head = Input(Bool())

  // Stores clear busy bit when stdata is received
  // memWidth for incoming addr/datagen, +1 for fp dgen, +1 for clr_head pointer
  val clr_bsy         = Output(Vec(lsuWidth, Valid(UInt(robAddrSz.W))))

  // Speculatively safe load (barring memory ordering failure)
  val clr_unsafe      = Output(Vec(lsuWidth, Valid(UInt(robAddrSz.W))))

  // Tell the DCache to clear prefetches/speculating misses
  val fence_dmem   = Input(Bool())

  // Speculatively tell the IQs that we'll get load data back next cycle
  val spec_ld_wakeup = Output(Vec(lsuWidth, Valid(UInt(maxPregSz.W))))
  // Tell the IQs that the load we speculated last cycle was misspeculated
  val ld_miss      = Output(Bool())

  val brupdate       = Input(new BrUpdateInfo)
  val rob_pnr_idx  = Input(UInt(robAddrSz.W))
  val rob_head_idx = Input(UInt(robAddrSz.W))
  val exception    = Input(Bool())

  val fencei_rdy  = Output(Bool())

  val lxcpt       = Output(Valid(new Exception))

  val tsc_reg     = Input(UInt())

  val perf        = Output(new Bundle {
    val acquire = Bool()
    val release = Bool()
    val tlbMiss = Bool()
  })
}

class LSUIO(implicit p: Parameters, edge: TLEdgeOut) extends BoomBundle()(p)
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
  val succeeded           = Bool()
  val order_fail          = Bool()
  val observed            = Bool()

  val st_dep_mask         = UInt(numStqEntries.W) // list of stores older than us
  val youngest_stq_idx    = UInt(stqAddrSz.W) // index of the oldest store younger than us

  val forward_std_val     = Bool()
  val forward_stq_idx     = UInt(stqAddrSz.W) // Which store did we get the store-load forward from?

  val debug_wb_data       = UInt(xLen.W)
}

class STQEntry(implicit p: Parameters) extends BoomBundle()(p)
   with HasBoomUOP
{
  val addr                = Valid(UInt(coreMaxAddrBits.W))
  val addr_is_virtual     = Bool() // Virtual address, we got a TLB miss
  val data                = Valid(UInt(xLen.W))

  val committed           = Bool() // committed by ROB
  val succeeded           = Bool() // D$ has ack'd this, we don't need to maintain this anymore
  val cleared             = Bool() // we've cleared this entry in the ROB

  val debug_wb_data       = UInt(xLen.W)
}

class LSU(implicit p: Parameters, edge: TLEdgeOut) extends BoomModule()(p)
  with rocket.HasL1HellaCacheParameters
{
  val io = IO(new LSUIO)


  val ldq = Reg(Vec(numLdqEntries, Valid(new LDQEntry)))
  val stq = Reg(Vec(numStqEntries, Valid(new STQEntry)))



  val ldq_head         = Reg(UInt(ldqAddrSz.W))
  val ldq_tail         = Reg(UInt(ldqAddrSz.W))
  val stq_head         = Reg(UInt(stqAddrSz.W)) // point to next store to clear from STQ (i.e., send to memory)
  val stq_tail         = Reg(UInt(stqAddrSz.W))
  val stq_commit_head  = Reg(UInt(stqAddrSz.W)) // point to next store to commit
  val stq_execute_head = Reg(UInt(stqAddrSz.W)) // point to next store to execute



  // If we got a mispredict, the tail will be misaligned for 1 extra cycle
  assert (io.core.brupdate.b2.mispredict ||
          stq(stq_execute_head).valid ||
          stq_head === stq_execute_head ||
          stq_tail === stq_execute_head,
            "stq_execute_head got off track.")

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


  val dtlb = Module(new NBDTLB(
    instruction = false, lgMaxSize = log2Ceil(coreDataBytes), rocket.TLBConfig(dcacheParams.nTLBEntries)))

  io.ptw <> dtlb.io.ptw
  io.core.perf.tlbMiss := io.ptw.req.fire()
  io.core.perf.acquire := io.dmem.perf.acquire
  io.core.perf.release := io.dmem.perf.release


  val clear_store     = WireInit(false.B)
  val live_store_mask = RegInit(0.U(numStqEntries.W))
  var next_live_store_mask = Mux(clear_store, live_store_mask & ~(1.U << stq_head),
                                              live_store_mask)


  def widthMap[T <: Data](f: Int => T) = VecInit((0 until lsuWidth).map(f))


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Enqueue new entries
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // This is a newer store than existing loads, so clear the bit in all the store dependency masks
  for (i <- 0 until numLdqEntries)
  {
    when (clear_store)
    {
      ldq(i).bits.st_dep_mask := ldq(i).bits.st_dep_mask & ~(1.U << stq_head)
    }
  }

  // Decode stage
  var ld_enq_idx = ldq_tail
  var st_enq_idx = stq_tail

  var ldq_tail_oh = UIntToOH(ldq_tail)(numLdqEntries-1,0)
  val ldq_valids  = VecInit(ldq.map(_.valid)).asUInt
  var stq_tail_oh = UIntToOH(stq_tail)(numStqEntries-1,0)
  val stq_valids  = VecInit(stq.map(_.valid)).asUInt

  val stq_nonempty = stq.map(_.valid).reduce(_||_)

  for (w <- 0 until coreWidth)
  {

    val ldq_full = (ldq_tail_oh & ldq_valids).orR
    io.core.ldq_full(w)    := ldq_full
    io.core.dis_ldq_idx(w) := ld_enq_idx

    val stq_full = (stq_tail_oh & stq_valids).orR
    io.core.stq_full(w)    := stq_full
    io.core.dis_stq_idx(w) := st_enq_idx

    val dis_ld_val = io.core.dis_uops(w).valid && io.core.dis_uops(w).bits.uses_ldq && !io.core.dis_uops(w).bits.exception
    val dis_st_val = io.core.dis_uops(w).valid && io.core.dis_uops(w).bits.uses_stq && !io.core.dis_uops(w).bits.exception
    when (dis_ld_val)
    {
      ldq(ld_enq_idx).valid                := true.B
      ldq(ld_enq_idx).bits.uop             := io.core.dis_uops(w).bits
      ldq(ld_enq_idx).bits.youngest_stq_idx  := st_enq_idx
      ldq(ld_enq_idx).bits.st_dep_mask     := next_live_store_mask

      ldq(ld_enq_idx).bits.addr.valid      := false.B
      ldq(ld_enq_idx).bits.executed        := false.B
      ldq(ld_enq_idx).bits.succeeded       := false.B
      ldq(ld_enq_idx).bits.order_fail      := false.B
      ldq(ld_enq_idx).bits.observed        := false.B
      ldq(ld_enq_idx).bits.forward_std_val := false.B

      assert (ld_enq_idx === io.core.dis_uops(w).bits.ldq_idx, "[lsu] mismatch enq load tag.")
      assert (!ldq(ld_enq_idx).valid, "[lsu] Enqueuing uop is overwriting ldq entries")
    }
    when (dis_st_val)
    {
      stq(st_enq_idx).valid           := true.B
      stq(st_enq_idx).bits.uop        := io.core.dis_uops(w).bits
      stq(st_enq_idx).bits.addr.valid := false.B
      stq(st_enq_idx).bits.data.valid := false.B
      stq(st_enq_idx).bits.committed  := false.B
      stq(st_enq_idx).bits.succeeded  := false.B
      stq(st_enq_idx).bits.cleared    := false.B

      assert (st_enq_idx === io.core.dis_uops(w).bits.stq_idx, "[lsu] mismatch enq store tag.")
      assert (!stq(st_enq_idx).valid, "[lsu] Enqueuing uop is overwriting stq entries")
    }

    ld_enq_idx = Mux(dis_ld_val, WrapInc(ld_enq_idx, numLdqEntries),
                                 ld_enq_idx)

    next_live_store_mask = Mux(dis_st_val, next_live_store_mask | (1.U << st_enq_idx),
                                           next_live_store_mask)
    st_enq_idx = Mux(dis_st_val, WrapInc(st_enq_idx, numStqEntries),
                                 st_enq_idx)

    assert(!(dis_ld_val && dis_st_val), "A UOP is trying to go into both the LDQ and the STQ")

    ldq_tail_oh = Mux(io.core.dis_uops(w).bits.uses_ldq && !io.core.dis_uops(w).bits.exception,
      RotateL1(ldq_tail_oh), ldq_tail_oh)
    stq_tail_oh = Mux(io.core.dis_uops(w).bits.uses_stq && !io.core.dis_uops(w).bits.exception,
      RotateL1(stq_tail_oh), stq_tail_oh)
  }

  ldq_tail := ld_enq_idx
  stq_tail := st_enq_idx

  io.dmem.force_order   := io.core.fence_dmem
  io.core.fencei_rdy    := !stq_nonempty && io.dmem.ordered


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Execute stage (access TLB, send requests to Memory)
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // We can only report 1 exception per cycle.
  // Just be sure to report the youngest one
  val mem_xcpt_valid  = Wire(Bool())
  val mem_xcpt_cause  = Wire(UInt())
  val mem_xcpt_uop    = Wire(new MicroOp)
  val mem_xcpt_vaddr  = Wire(UInt())


  //---------------------------------------
  // Can-fire logic and wakeup/retry select
  //
  // First we determine what operations are waiting to execute.
  // These are the "can_fire"/"will_fire" signals

  val will_fire_load_agen_exec  = Wire(Vec(lsuWidth, Bool()))
  val will_fire_load_agen         = Wire(Vec(lsuWidth, Bool()))
  val will_fire_store_agen        = Wire(Vec(lsuWidth, Bool()))
  val will_fire_sfence            = Wire(Vec(lsuWidth, Bool()))
  val will_fire_hella_incoming    = Wire(Vec(lsuWidth, Bool()))
  val will_fire_hella_wakeup      = Wire(Vec(lsuWidth, Bool()))
  val will_fire_release           = Wire(Vec(lsuWidth, Bool()))
  val will_fire_load_retry        = Wire(Vec(lsuWidth, Bool()))
  val will_fire_store_retry       = Wire(Vec(lsuWidth, Bool()))
  val will_fire_store_commit_fast = Wire(Vec(lsuWidth, Bool()))
  val will_fire_store_commit_slow = Wire(Vec(lsuWidth, Bool()))
  val will_fire_load_wakeup       = Wire(Vec(lsuWidth, Bool()))

  val agen = io.core.agen
  // -------------------------------
  // Assorted signals for scheduling

  // Don't wakeup a load if we just sent it last cycle or two cycles ago
  // The block_load_mask may be wrong, but the executing_load mask must be accurate
  val block_load_mask    = WireInit(VecInit((0 until numLdqEntries).map(x=>false.B)))
  val p1_block_load_mask = RegNext(block_load_mask)
  val p2_block_load_mask = RegNext(p1_block_load_mask)

 // Prioritize emptying the store queue when it is almost full
  val stq_tail_plus   = WrapAdd(stq_tail, (2*coreWidth).U, numStqEntries)
  val stq_almost_full = IsOlder(stq_head, stq_tail_plus, stq_tail)

  // The store at the commit head needs the DCache to appear ordered
  // Delay firing load wakeups and retries now
  val store_needs_order = WireInit(false.B)

  val ldq_incoming_idx = widthMap(i => agen(i).bits.uop.ldq_idx)
  val ldq_incoming_e   = widthMap(i => ldq(ldq_incoming_idx(i)))

  val stq_incoming_idx = widthMap(i => agen(i).bits.uop.stq_idx)
  val stq_incoming_e   = widthMap(i => stq(stq_incoming_idx(i)))


  val stq_commit_e  = WireInit(stq(stq_execute_head))

  val ldq_wakeup_idx = RegNext(AgePriorityEncoder((0 until numLdqEntries).map(i=> {
    val e = ldq(i).bits
    val block = block_load_mask(i) || p1_block_load_mask(i)
    e.addr.valid && !e.executed && !e.succeeded && !e.addr_is_virtual && !block
  }), ldq_head))
  val ldq_wakeup_e   = WireInit(ldq(ldq_wakeup_idx))


  // Enqueue into the retry queue
  val ldq_enq_retry_idx = Reg(UInt(ldqAddrSz.W))
  ldq_enq_retry_idx := AgePriorityEncoder((0 until numLdqEntries).map(i => {
    val e = WireInit(ldq(i).bits)
    e.addr.valid && e.addr_is_virtual && (i.U =/= ldq_enq_retry_idx)
  }), ldq_head)
  val ldq_enq_retry_e = WireInit(ldq(ldq_enq_retry_idx))

  val stq_enq_retry_idx = Reg(UInt(stqAddrSz.W))
  stq_enq_retry_idx := AgePriorityEncoder((0 until numStqEntries).map(i => {
    val e = WireInit(stq(i).bits)
    e.addr.valid && e.addr_is_virtual && (i.U =/= stq_enq_retry_idx)
  }), stq_commit_head)
  val stq_enq_retry_e   = WireInit(stq(stq_enq_retry_idx))

  val can_enq_load_retry    = (ldq_enq_retry_e.valid                            &&
                               ldq_enq_retry_e.bits.addr.valid                  &&
                               ldq_enq_retry_e.bits.addr_is_virtual)
  val can_enq_store_retry   = (stq_enq_retry_e.valid                            &&
                               stq_enq_retry_e.bits.addr.valid                  &&
                               stq_enq_retry_e.bits.addr_is_virtual)

  val retry_queue = Module(new BranchKillableQueue(new FuncUnitResp(xLen), 8))
  retry_queue.io.brupdate := io.core.brupdate
  retry_queue.io.flush    := io.core.exception

  retry_queue.io.enq.valid     := can_enq_store_retry || can_enq_load_retry
  retry_queue.io.enq.bits      := DontCare
  retry_queue.io.enq.bits.addr := Mux(can_enq_store_retry, stq_enq_retry_e.bits.addr.bits, ldq_enq_retry_e.bits.addr.bits)
  retry_queue.io.enq.bits.uop  := Mux(can_enq_store_retry, stq_enq_retry_e.bits.uop      , ldq_enq_retry_e.bits.uop)
  retry_queue.io.enq.bits.uop.uses_ldq := can_enq_load_retry && !can_enq_store_retry
  retry_queue.io.enq.bits.uop.ldq_idx  := ldq_enq_retry_idx
  retry_queue.io.enq.bits.uop.stq_idx  := can_enq_store_retry
  retry_queue.io.enq.bits.uop.stq_idx  := stq_enq_retry_idx

  when (can_enq_store_retry && retry_queue.io.enq.fire()) {
    stq(stq_enq_retry_idx).bits.addr.valid := false.B
  } .elsewhen (can_enq_load_retry && retry_queue.io.enq.fire()) {
    ldq(ldq_enq_retry_idx).bits.addr.valid := false.B
  }

  val ldq_retry_idx = retry_queue.io.deq.bits.uop.ldq_idx
  val stq_retry_idx = retry_queue.io.deq.bits.uop.stq_idx
  retry_queue.io.deq.ready := will_fire_load_retry.reduce(_||_) || will_fire_store_retry.reduce(_||_)

  // -----------------------
  // Determine what can fire

  // Can we fire a incoming load
  val can_fire_load_agen_exec = widthMap(w => agen(w).valid && agen(w).bits.uop.uses_ldq)
  val can_fire_load_agen      = can_fire_load_agen_exec

  // Can we fire an incoming store addrgen + store datagen
  val can_fire_store_agen     = widthMap(w => agen(w).valid && agen(w).bits.uop.uses_stq)


  // Can we fire an incoming sfence
  val can_fire_sfence        = widthMap(w => io.core.sfence.valid)

  // Can we fire a request from dcache to release a line
  // This needs to go through LDQ search to mark loads as dangerous
  val can_fire_release       = widthMap(w => (w == lsuWidth-1).B && io.dmem.release.valid)
  io.dmem.release.ready     := will_fire_release.reduce(_||_)

  // Can we retry a load that missed in the TLB
  val can_fire_load_retry    = widthMap(w =>
                               ( retry_queue.io.deq.valid                     &&
                                 retry_queue.io.deq.bits.uop.uses_ldq         &&
                                !store_needs_order                            &&
                                (w == lsuWidth-1).B))

  // Can we retry a store addrgen that missed in the TLB
  val can_fire_store_retry   = widthMap(w =>
                               ( retry_queue.io.deq.valid                     &&
                                 retry_queue.io.deq.bits.uop.uses_stq         &&
                                 (w == lsuWidth-1).B))

  // Can we commit a store
  val can_fire_store_commit_slow  = widthMap(w =>
                                    ( stq_commit_e.valid                           &&
                                     !stq_commit_e.bits.uop.is_fence               &&
                                     !mem_xcpt_valid                               &&
                                     !stq_commit_e.bits.uop.exception              &&
                                      (w == 0).B                                   &&
                                      stq_commit_e.bits.data.valid                 &&
                                      (stq_commit_e.bits.committed || ( stq_commit_e.bits.uop.is_amo      &&
                                                                        stq_commit_e.bits.addr.valid      &&
                                                                       !stq_commit_e.bits.addr_is_virtual &&
                                                                        stq_commit_e.bits.data.valid))))
  val can_fire_store_commit_fast = widthMap(w => can_fire_store_commit_slow(w) && stq_almost_full)

  // Can we wakeup a load that was nack'd
  val block_load_wakeup = WireInit(false.B)
  val can_fire_load_wakeup = widthMap(w =>
                             ( ldq_wakeup_e.valid                                      &&
                               ldq_wakeup_e.bits.addr.valid                            &&
                              !ldq_wakeup_e.bits.succeeded                             &&
                              !ldq_wakeup_e.bits.addr_is_virtual                       &&
                              !ldq_wakeup_e.bits.executed                              &&
                              !ldq_wakeup_e.bits.order_fail                            &&
                              !p1_block_load_mask(ldq_wakeup_idx)                      &&
                              !p2_block_load_mask(ldq_wakeup_idx)                      &&
                              !store_needs_order                                       &&
                              !block_load_wakeup                                       &&
                              (w == lsuWidth-1).B                                      &&
                              (!ldq_wakeup_e.bits.addr_is_uncacheable || (io.core.commit_load_at_rob_head &&
                                                                          ldq_head === ldq_wakeup_idx &&
                                                                          ldq_wakeup_e.bits.st_dep_mask.asUInt === 0.U))))

  // Can we fire an incoming hellacache request
  val can_fire_hella_incoming  = WireInit(widthMap(w => false.B)) // This is assigned to in the hellashim ocntroller

  // Can we fire a hellacache request that the dcache nack'd
  val can_fire_hella_wakeup    = WireInit(widthMap(w => false.B)) // This is assigned to in the hellashim controller

  //---------------------------------------------------------
  // Controller logic. Arbitrate which request actually fires

  val exe_tlb_valid = Wire(Vec(lsuWidth, Bool()))
  for (w <- 0 until lsuWidth) {
    var tlb_avail  = true.B
    var dc_avail   = true.B
    var lcam_avail = true.B

    def lsu_sched(can_fire: Bool, uses_tlb:Boolean, uses_dc:Boolean, uses_lcam: Boolean): Bool = {
      val will_fire = can_fire && !(uses_tlb.B && !tlb_avail) &&
                                  !(uses_lcam.B && !lcam_avail) &&
                                  !(uses_dc.B && !dc_avail)
      tlb_avail  = tlb_avail  && !(will_fire && uses_tlb.B)
      lcam_avail = lcam_avail && !(will_fire && uses_lcam.B)
      dc_avail   = dc_avail   && !(will_fire && uses_dc.B)
      dontTouch(will_fire) // dontTouch these so we can inspect the will_fire signals
      will_fire
    }

    // The order of these statements is the priority
    // Some restrictions
    //  - Incoming ops must get precedence, can't backpresure memaddrgen
    //  - Incoming hellacache ops must get precedence over retrying ops (PTW must get precedence over retrying translation)
    // Notes on performance
    //  - Prioritize releases, this speeds up cache line writebacks and refills
    //  - Store commits are lowest priority, since they don't "block" younger instructions unless stq fills up
    will_fire_sfence           (w) := lsu_sched(can_fire_sfence           (w) , true , false, false) // TLB ,    ,
    will_fire_store_commit_fast(w) := lsu_sched(can_fire_store_commit_fast(w) , false, true , false) //     , DC          If store queue is filling up, prioritize draining it
    will_fire_load_agen_exec   (w) := lsu_sched(can_fire_load_agen_exec   (w) , true , true , true ) // TLB , DC , LCAM   Normally fire loads as soon as translation completes
    will_fire_load_agen        (w) := lsu_sched(can_fire_load_agen        (w) , true , false, true ) // TLB ,    , LCAM   If we are draining stores, still translate the loads
    will_fire_store_agen       (w) := lsu_sched(can_fire_store_agen       (w) , true , false, true ) // TLB ,    , LCAM
    will_fire_release          (w) := lsu_sched(can_fire_release          (w) , false, false, true ) //            LCAM
    will_fire_hella_incoming   (w) := lsu_sched(can_fire_hella_incoming   (w) , true , true , false) // TLB , DC
    will_fire_hella_wakeup     (w) := lsu_sched(can_fire_hella_wakeup     (w) , false, true , false) //     , DC
    will_fire_store_retry      (w) := lsu_sched(can_fire_store_retry      (w) , true , false, true ) // TLB ,    , LCAM
    will_fire_load_retry       (w) := lsu_sched(can_fire_load_retry       (w) , true , true , true ) // TLB , DC , LCAM
    will_fire_load_wakeup      (w) := lsu_sched(can_fire_load_wakeup      (w) , false, true , true ) //     , DC , LCAM
    will_fire_store_commit_slow(w) := lsu_sched(can_fire_store_commit_slow(w) , false, true , false) //     , DC


    assert(!(agen(w).valid && !(will_fire_load_agen_exec(w) || will_fire_load_agen(w) || will_fire_store_agen(w))))

    when (will_fire_load_wakeup(w)) {
      block_load_mask(ldq_wakeup_idx)           := true.B
    } .elsewhen (will_fire_load_agen(w) || will_fire_load_agen_exec(w)) {
      block_load_mask(agen(w).bits.uop.ldq_idx) := true.B
    } .elsewhen (will_fire_load_retry(w)) {
      block_load_mask(ldq_retry_idx)            := true.B
    }
    exe_tlb_valid(w) := !tlb_avail
  }
  assert((lsuWidth == 1).B ||
    (!will_fire_hella_incoming.reduce(_&&_)    &&
     !will_fire_hella_wakeup.reduce(_&&_)      &&
     !will_fire_load_retry.reduce(_&&_)        &&
     !will_fire_store_retry.reduce(_&&_)       &&
     !will_fire_store_commit_fast.reduce(_&&_) &&
     !will_fire_store_commit_slow.reduce(_&&_) &&
     !will_fire_load_wakeup.reduce(_&&_)),
    "Some operations is proceeding down multiple pipes")

  require(lsuWidth <= 2)

  //--------------------------------------------
  // TLB Access

  assert(!(hella_state =/= h_ready && hella_req.cmd === rocket.M_SFENCE),
    "SFENCE through hella interface not supported")

  val exe_tlb_uop = widthMap(w =>
                    Mux(will_fire_load_agen_exec(w) ||
                        will_fire_load_agen     (w)  , ldq_incoming_e(w).bits.uop,
                    Mux(will_fire_store_agen    (w)  , stq_incoming_e(w).bits.uop,
                    Mux(will_fire_load_retry    (w) ||
                        will_fire_store_retry   (w)  , retry_queue.io.deq.bits.uop,
                    Mux(will_fire_hella_incoming(w)  , NullMicroOp,
                                                       NullMicroOp)))))

  val exe_tlb_vaddr = widthMap(w =>
                    Mux(will_fire_load_agen_exec(w) ||
                        will_fire_load_agen     (w) ||
                        will_fire_store_agen    (w)  , agen(w).bits.addr,
                    Mux(will_fire_sfence        (w)  , io.core.sfence.bits.addr,
                    Mux(will_fire_load_retry    (w) ||
                        will_fire_store_retry   (w)  , retry_queue.io.deq.bits.addr,
                    Mux(will_fire_hella_incoming(w)  , hella_req.addr,
                                                       0.U)))))

  val exe_sfence = io.core.sfence

  val exe_size   = widthMap(w =>
                   Mux(will_fire_load_agen_exec(w) ||
                       will_fire_load_agen     (w) ||
                       will_fire_store_agen    (w) ||
                       will_fire_load_retry    (w) ||
                       will_fire_store_retry   (w)  , exe_tlb_uop(w).mem_size,
                   Mux(will_fire_hella_incoming(w)  , hella_req.size,
                                                      0.U)))
  val exe_cmd    = widthMap(w =>
                   Mux(will_fire_load_agen_exec(w) ||
                       will_fire_load_agen     (w) ||
                       will_fire_store_agen    (w) ||
                       will_fire_load_retry    (w) ||
                       will_fire_store_retry   (w)  , exe_tlb_uop(w).mem_cmd,
                   Mux(will_fire_hella_incoming(w)  , hella_req.cmd,
                   Mux(will_fire_sfence        (w)  , rocket.M_SFENCE,
                                                      0.U))))

  val exe_passthr= widthMap(w =>
                   Mux(will_fire_hella_incoming(w)  , hella_req.phys,
                                                      false.B))
  val exe_kill   = widthMap(w =>
                   Mux(will_fire_hella_incoming(w)  , io.hellacache.s1_kill,
                                                      false.B))
  for (w <- 0 until lsuWidth) {
    dtlb.io.req(w).valid            := exe_tlb_valid(w)
    dtlb.io.req(w).bits.vaddr       := exe_tlb_vaddr(w)
    dtlb.io.req(w).bits.size        := exe_size(w)
    dtlb.io.req(w).bits.cmd         := exe_cmd(w)
    dtlb.io.req(w).bits.passthrough := exe_passthr(w)
  }
  dtlb.io.kill                      := exe_kill.reduce(_||_)
  dtlb.io.sfence                    := exe_sfence

  // exceptions
  val ma_ld = widthMap(w => (will_fire_load_agen(w) || will_fire_load_agen_exec(w)) && agen(w).bits.mxcpt.valid) // We get ma_ld in memaddrcalc
  val ma_st = widthMap(w => will_fire_store_agen(w) && agen(w).bits.mxcpt.valid) // We get ma_ld in memaddrcalc
  val pf_ld = widthMap(w => dtlb.io.req(w).valid && dtlb.io.resp(w).pf.ld && exe_tlb_uop(w).uses_ldq)
  val pf_st = widthMap(w => dtlb.io.req(w).valid && dtlb.io.resp(w).pf.st && exe_tlb_uop(w).uses_stq)
  val ae_ld = widthMap(w => dtlb.io.req(w).valid && dtlb.io.resp(w).ae.ld && exe_tlb_uop(w).uses_ldq)
  val ae_st = widthMap(w => dtlb.io.req(w).valid && dtlb.io.resp(w).ae.st && exe_tlb_uop(w).uses_stq)

  // TODO check for xcpt_if and verify that never happens on non-speculative instructions.
  val mem_xcpt_valids = RegNext(widthMap(w =>
                     (pf_ld(w) || pf_st(w) || ae_ld(w) || ae_st(w) || ma_ld(w) || ma_st(w)) &&
                     !io.core.exception &&
                     !IsKilledByBranch(io.core.brupdate, exe_tlb_uop(w))))
  val mem_xcpt_uops   = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, exe_tlb_uop(w))))
  val mem_xcpt_causes = RegNext(widthMap(w =>
    Mux(ma_ld(w), rocket.Causes.misaligned_load.U,
    Mux(ma_st(w), rocket.Causes.misaligned_store.U,
    Mux(pf_ld(w), rocket.Causes.load_page_fault.U,
    Mux(pf_st(w), rocket.Causes.store_page_fault.U,
    Mux(ae_ld(w), rocket.Causes.load_access.U,
                  rocket.Causes.store_access.U)))))))
  val mem_xcpt_vaddrs = RegNext(exe_tlb_vaddr)

  for (w <- 0 until lsuWidth) {
    assert (!(dtlb.io.req(w).valid && exe_tlb_uop(w).is_fence), "Fence is pretending to talk to the TLB")
  }

  mem_xcpt_valid := mem_xcpt_valids.reduce(_||_)
  mem_xcpt_cause := mem_xcpt_causes(0)
  mem_xcpt_uop   := mem_xcpt_uops(0)
  mem_xcpt_vaddr := mem_xcpt_vaddrs(0)
  var xcpt_found = mem_xcpt_valids(0)
  var oldest_xcpt_rob_idx = mem_xcpt_uops(0).rob_idx
  for (w <- 1 until lsuWidth) {
    val is_older = WireInit(false.B)
    when (mem_xcpt_valids(w) &&
      (IsOlder(mem_xcpt_uops(w).rob_idx, oldest_xcpt_rob_idx, io.core.rob_head_idx) || !xcpt_found)) {
      is_older := true.B
      mem_xcpt_cause := mem_xcpt_causes(w)
      mem_xcpt_uop   := mem_xcpt_uops(w)
      mem_xcpt_vaddr := mem_xcpt_vaddrs(w)
    }
    xcpt_found = xcpt_found || mem_xcpt_valids(w)
    oldest_xcpt_rob_idx = Mux(is_older, mem_xcpt_uops(w).rob_idx, oldest_xcpt_rob_idx)
  }

  val exe_tlb_miss  = widthMap(w => dtlb.io.req(w).valid && (dtlb.io.resp(w).miss || !dtlb.io.req(w).ready))
  val exe_tlb_paddr = widthMap(w => Cat(dtlb.io.resp(w).paddr(paddrBits-1,corePgIdxBits),
                                        exe_tlb_vaddr(w)(corePgIdxBits-1,0)))
  val exe_tlb_uncacheable = widthMap(w => !(dtlb.io.resp(w).cacheable))

  for (w <- 0 until lsuWidth) {
    assert (exe_tlb_paddr(w) === dtlb.io.resp(w).paddr, "[lsu] paddrs should match.")

    when (mem_xcpt_valids(w))
    {
      assert(RegNext(will_fire_load_agen_exec(w) || will_fire_load_agen(w) || will_fire_store_agen(w) ||
        will_fire_load_retry(w) || will_fire_store_retry(w)))
      // Technically only faulting AMOs need this
      assert(mem_xcpt_uops(w).uses_ldq ^ mem_xcpt_uops(w).uses_stq)
      when (mem_xcpt_uops(w).uses_ldq)
      {
        ldq(mem_xcpt_uops(w).ldq_idx).bits.uop.exception := true.B
      }
        .otherwise
      {
        stq(mem_xcpt_uops(w).stq_idx).bits.uop.exception := true.B
      }
    }
  }

  val exe_agen_killed = widthMap(w => IsKilledByBranch(io.core.brupdate, agen(w).bits.uop))


  //------------------------------
  // Issue Someting to Memory
  //
  // A memory op can come from many different places
  // The address either was freshly translated, or we are
  // reading a physical address from the LDQ,STQ, or the HellaCache adapter


  // defaults
  io.dmem.brupdate       := io.core.brupdate
  io.dmem.exception      := io.core.exception
  io.dmem.rob_head_idx   := io.core.rob_head_idx
  io.dmem.rob_pnr_idx    := io.core.rob_pnr_idx

  val dmem_req = Wire(Vec(lsuWidth, Valid(new BoomDCacheReq)))
  io.dmem.req.valid := dmem_req.map(_.valid).reduce(_||_)
  io.dmem.req.bits  := dmem_req
  val dmem_req_fire = widthMap(w => dmem_req(w).valid && io.dmem.req.fire())

  val s0_executing_loads = WireInit(VecInit((0 until numLdqEntries).map(x=>false.B)))
  val s0_kills = Wire(Vec(lsuWidth, Bool()))

  for (w <- 0 until lsuWidth) {
    dmem_req(w).valid := false.B
    dmem_req(w).bits.uop   := NullMicroOp
    dmem_req(w).bits.addr  := 0.U
    dmem_req(w).bits.data  := 0.U
    dmem_req(w).bits.is_hella := false.B

    s0_kills(w) := false.B
    io.dmem.s1_kill(w) := RegNext(s0_kills(w) && dmem_req_fire(w))

    when (will_fire_load_agen_exec(w)) {
      dmem_req(w).valid      := true.B
      dmem_req(w).bits.addr  := exe_tlb_paddr(w)
      dmem_req(w).bits.uop   := exe_tlb_uop(w)

      s0_kills(w)            := exe_tlb_miss(w) || exe_tlb_uncacheable(w)
      s0_executing_loads(ldq_incoming_idx(w)) := dmem_req_fire(w) && !s0_kills(w)

      assert(!ldq_incoming_e(w).bits.executed)
    } .elsewhen (will_fire_load_retry(w)) {
      dmem_req(w).valid      := true.B
      dmem_req(w).bits.addr  := exe_tlb_paddr(w)
      dmem_req(w).bits.uop   := exe_tlb_uop(w)

      s0_kills(w) := exe_tlb_miss(w) || exe_tlb_uncacheable(w)
      s0_executing_loads(ldq_retry_idx) := dmem_req_fire(w) && !s0_kills(w)
    } .elsewhen (will_fire_store_commit_slow(w) || will_fire_store_commit_fast(w)) {
      dmem_req(w).valid         := true.B
      dmem_req(w).bits.addr     := stq_commit_e.bits.addr.bits
      dmem_req(w).bits.data     := (new freechips.rocketchip.rocket.StoreGen(
                                    stq_commit_e.bits.uop.mem_size, 0.U,
                                    stq_commit_e.bits.data.bits,
                                    coreDataBytes)).data
      dmem_req(w).bits.uop      := stq_commit_e.bits.uop

      stq_execute_head                     := Mux(dmem_req_fire(w),
                                                WrapInc(stq_execute_head, numStqEntries),
                                                stq_execute_head)

      stq(stq_execute_head).bits.succeeded := false.B
    } .elsewhen (will_fire_load_wakeup(w)) {
      dmem_req(w).valid      := true.B
      dmem_req(w).bits.addr  := ldq_wakeup_e.bits.addr.bits
      dmem_req(w).bits.uop   := ldq_wakeup_e.bits.uop

      s0_executing_loads(ldq_wakeup_idx) := dmem_req_fire(w)

      assert(!ldq_wakeup_e.bits.executed && !ldq_wakeup_e.bits.addr_is_virtual)
    } .elsewhen (will_fire_hella_incoming(w)) {
      assert(hella_state === h_s1)

      dmem_req(w).valid               := !io.hellacache.s1_kill && (!exe_tlb_miss(w) || hella_req.phys)
      dmem_req(w).bits.addr           := exe_tlb_paddr(w)
      dmem_req(w).bits.data           := (new freechips.rocketchip.rocket.StoreGen(
        hella_req.size, 0.U,
        io.hellacache.s1_data.data,
        coreDataBytes)).data
      dmem_req(w).bits.uop.mem_cmd    := hella_req.cmd
      dmem_req(w).bits.uop.mem_size   := hella_req.size
      dmem_req(w).bits.uop.mem_signed := hella_req.signed
      dmem_req(w).bits.is_hella       := true.B

      hella_paddr := exe_tlb_paddr(w)
    }
      .elsewhen (will_fire_hella_wakeup(w))
    {
      assert(hella_state === h_replay)
      dmem_req(w).valid               := true.B
      dmem_req(w).bits.addr           := hella_paddr
      dmem_req(w).bits.data           := (new freechips.rocketchip.rocket.StoreGen(
        hella_req.size, 0.U,
        hella_data.data,
        coreDataBytes)).data
      dmem_req(w).bits.uop.mem_cmd    := hella_req.cmd
      dmem_req(w).bits.uop.mem_size   := hella_req.size
      dmem_req(w).bits.uop.mem_signed := hella_req.signed
      dmem_req(w).bits.is_hella       := true.B
    }

    //-------------------------------------------------------------
    // Write Addr into the LAQ/SAQ
    when (will_fire_load_agen(w) || will_fire_load_agen_exec(w) || will_fire_load_retry(w))
    {
      val ldq_idx = Mux(will_fire_load_agen(w) || will_fire_load_agen_exec(w), ldq_incoming_idx(w), ldq_retry_idx)
      ldq(ldq_idx).bits.addr.valid          := !exe_agen_killed(w) || will_fire_load_retry(w)
      ldq(ldq_idx).bits.addr.bits           := Mux(exe_tlb_miss(w), exe_tlb_vaddr(w), exe_tlb_paddr(w))
      ldq(ldq_idx).bits.uop.pdst            := exe_tlb_uop(w).pdst
      ldq(ldq_idx).bits.addr_is_virtual     := exe_tlb_miss(w)
      ldq(ldq_idx).bits.addr_is_uncacheable := exe_tlb_uncacheable(w) && !exe_tlb_miss(w)

      assert(!ldq(ldq_idx).bits.addr.valid,
        "[lsu] Translating load is overwriting a valid address")
    }

    when (will_fire_store_agen(w) || will_fire_store_retry(w))
    {
      val stq_idx = Mux(will_fire_store_agen(w),
        stq_incoming_idx(w), stq_retry_idx)

      stq(stq_idx).bits.addr.valid := (!exe_agen_killed(w) || will_fire_store_retry(w)) && !pf_st(w) // Prevent AMOs from executing!
      stq(stq_idx).bits.addr.bits  := Mux(exe_tlb_miss(w), exe_tlb_vaddr(w), exe_tlb_paddr(w))
      stq(stq_idx).bits.uop.pdst   := exe_tlb_uop(w).pdst // Needed for AMOs
      stq(stq_idx).bits.addr_is_virtual := exe_tlb_miss(w)

      assert(!stq(stq_idx).bits.addr.valid,
        "[lsu] Translating store is overwriting a valid address")

    }

  }

  //-------------------------------------------------------------
  // Write data into the STQ
  for (dgen <- io.core.dgen) {
    when (dgen.valid) {
      val sidx = dgen.bits.uop.stq_idx
      stq(sidx).bits.data.valid := true.B
      stq(sidx).bits.data.bits  := dgen.bits.data
      assert(!(stq(sidx).bits.data.valid))
    }
  }

  require (xLen >= fLen) // for correct SDQ size

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Cache Access Cycle (Mem)
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Note the DCache may not have accepted our request

  val fired_load_agen_exec = widthMap(w => RegNext(will_fire_load_agen_exec(w) && !exe_agen_killed(w)))
  val fired_load_agen      = widthMap(w => RegNext(will_fire_load_agen     (w) && !exe_agen_killed(w)))
  val fired_store_agen     = widthMap(w => RegNext(will_fire_store_agen    (w) && !exe_agen_killed(w)))
  val fired_sfence         = RegNext(will_fire_sfence)
  val fired_release        = RegNext(will_fire_release)
  val fired_load_retry     = widthMap(w => RegNext(will_fire_load_retry   (w) && !IsKilledByBranch(io.core.brupdate, retry_queue.io.deq.bits.uop)))
  val fired_store_retry    = widthMap(w => RegNext(will_fire_store_retry  (w) && !IsKilledByBranch(io.core.brupdate, retry_queue.io.deq.bits.uop)))
  val fired_store_commit   = widthMap(w => RegNext(will_fire_store_commit_slow(w) || will_fire_store_commit_fast(w)))
  val fired_load_wakeup    = widthMap(w => RegNext(will_fire_load_wakeup  (w) && !IsKilledByBranch(io.core.brupdate, ldq_wakeup_e.bits.uop)))
  val fired_hella_incoming = RegNext(will_fire_hella_incoming)
  val fired_hella_wakeup   = RegNext(will_fire_hella_wakeup)

  val mem_incoming_uop     = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, agen(w).bits.uop)))
  val mem_ldq_incoming_e   = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, ldq_incoming_e(w))))
  val mem_stq_incoming_e   = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, stq_incoming_e(w))))
  val mem_ldq_wakeup_e     = RegNext(UpdateBrMask(io.core.brupdate, ldq_wakeup_e))
  val mem_ldq_retry_e      = RegNext(UpdateBrMask(io.core.brupdate, ldq(ldq_retry_idx)))
  val mem_stq_retry_e      = RegNext(UpdateBrMask(io.core.brupdate, stq(stq_retry_idx)))
  val mem_ldq_e            = widthMap(w =>
                             Mux(fired_load_agen     (w) ||
                                 fired_load_agen_exec(w), mem_ldq_incoming_e(w),
                             Mux(fired_load_retry    (w), mem_ldq_retry_e,
                             Mux(fired_load_wakeup   (w), mem_ldq_wakeup_e,
                                                          (0.U).asTypeOf(Valid(new LDQEntry))))))
  val mem_stq_e            = widthMap(w =>
                             Mux(fired_store_agen (w), mem_stq_incoming_e(w),
                             Mux(fired_store_retry(w), mem_stq_retry_e, (0.U).asTypeOf(Valid(new STQEntry)))))


  val mem_tlb_miss             = RegNext(exe_tlb_miss)
  val mem_tlb_uncacheable      = RegNext(exe_tlb_uncacheable)
  val mem_paddr                = RegNext(widthMap(w => dmem_req(w).bits.addr))

  // Task 1: Clr ROB busy bit

  // Delay store clearing 1 extra cycle, as the store clear can't occur
  // too quickly before an order fail caused by this store propagates to ROB

  for (w <- 0 until lsuWidth) {
    // Delay store clearing 1 extra cycle, as the store clear can't occur
    // too quickly before an order fail caused by this store propagates to ROB
    val clr_valid = Reg(Bool())
    clr_valid    := false.B
    val clr_uop   = Reg(new MicroOp)

    val clr_valid_1 = RegNext(clr_valid && !io.core.exception && !IsKilledByBranch(io.core.brupdate, clr_uop))
    val clr_uop_1   = RegNext(UpdateBrMask(io.core.brupdate, clr_uop))

    val stq_idx = Mux(fired_store_agen(w) , RegNext(stq_incoming_idx(w))
                                          , RegNext(stq_retry_idx))
    val stq_clr = fired_store_agen(w) || fired_store_retry(w)
    val stq_e   = stq(stq_idx)

    when ( stq_clr &&
           stq_e.valid &&
           stq_e.bits.addr.valid &&
          !stq_e.bits.addr_is_virtual &&
          !stq_e.bits.uop.is_amo &&
          !stq_e.bits.cleared &&
          !io.core.exception &&
          !IsKilledByBranch(io.core.brupdate, stq_e.bits.uop)) {
      clr_valid := true.B
      clr_uop   := UpdateBrMask(io.core.brupdate, stq_e.bits.uop)

      stq(stq_idx).bits.cleared := true.B
    }

    io.core.clr_bsy(w).valid := clr_valid_1 && !io.core.exception && !IsKilledByBranch(io.core.brupdate, clr_uop_1)
    io.core.clr_bsy(w).bits  := clr_uop_1.rob_idx
  }





  // Task 2: Do LD-LD. ST-LD searches for ordering failures
  //         Do LD-ST search for forwarding opportunities
  // We have the opportunity to kill a request we sent last cycle. Use it wisely!

  // We translated a store last cycle
  val do_st_search = widthMap(w => (fired_store_agen(w) || fired_store_retry(w)) && !mem_tlb_miss(w))
  // We translated a load last cycle
  val do_ld_search = widthMap(w => ((fired_load_agen(w) || fired_load_agen_exec(w) || fired_load_retry(w)) && !mem_tlb_miss(w)) ||
                     fired_load_wakeup(w))
  // We are making a local line visible to other harts
  val do_release_search = widthMap(w => fired_release(w))

  // Store addrs don't go to memory yet, get it from the TLB response
  // Load wakeups don't go through TLB, get it through memory
  // Load incoming and load retries go through both

  val lcam_addr  = widthMap(w => Mux(fired_store_agen(w) || fired_store_retry(w) || fired_load_agen(w) || fired_load_agen_exec(w),
                                     RegNext(exe_tlb_paddr(w)),
                                     Mux(fired_release(w), RegNext(io.dmem.release.bits.address),
                                         mem_paddr(w))))
  val lcam_uop   = widthMap(w => Mux(do_st_search(w), mem_stq_e(w).bits.uop,
                                 Mux(do_ld_search(w), mem_ldq_e(w).bits.uop, NullMicroOp)))

  val lcam_mask  = widthMap(w => GenByteMask(lcam_addr(w), lcam_uop(w).mem_size))
  val lcam_st_dep_mask = widthMap(w => mem_ldq_e(w).bits.st_dep_mask)
  val lcam_is_release = widthMap(w => fired_release(w))
  val lcam_ldq_idx  = widthMap(w =>
                      Mux(fired_load_agen     (w) ||
                          fired_load_agen_exec(w)  , mem_incoming_uop(w).ldq_idx,
                      Mux(fired_load_wakeup  (w), RegNext(ldq_wakeup_idx),
                      Mux(fired_load_retry   (w), RegNext(ldq_retry_idx), 0.U))))
  val lcam_stq_idx  = widthMap(w =>
                      Mux(fired_store_agen (w), mem_incoming_uop(w).stq_idx,
                      Mux(fired_store_retry(w), RegNext(stq_retry_idx), 0.U)))

  val can_forward = WireInit(widthMap(w =>
    Mux(fired_load_agen(w) || fired_load_agen_exec(w) || fired_load_retry(w), !mem_tlb_uncacheable(w),
      !ldq(lcam_ldq_idx(w)).bits.addr_is_uncacheable)))

  // Mask of stores which we conflict on address with
  val ldst_addr_matches    = WireInit(widthMap(w => VecInit((0 until numStqEntries).map(x=>false.B))))
  // Mask of stores which we can forward from
  val ldst_forward_matches = WireInit(widthMap(w => VecInit((0 until numStqEntries).map(x=>false.B))))

  val s1_executing_loads = RegNext(s0_executing_loads)
  val s1_set_execute     = WireInit(s1_executing_loads)

  val mem_forward_valid   = Wire(Vec(lsuWidth, Bool()))
  val mem_forward_ldq_idx = lcam_ldq_idx
  val mem_forward_ld_addr = lcam_addr
  val mem_forward_stq_idx = Wire(Vec(lsuWidth, UInt(log2Ceil(numStqEntries).W)))

  val wb_forward_valid    = RegNext(mem_forward_valid)
  val wb_forward_ldq_idx  = RegNext(mem_forward_ldq_idx)
  val wb_forward_ld_addr  = RegNext(mem_forward_ld_addr)
  val wb_forward_stq_idx  = RegNext(mem_forward_stq_idx)

  for (i <- 0 until numLdqEntries) {
    val l_valid = ldq(i).valid
    val l_bits  = ldq(i).bits
    val l_addr  = ldq(i).bits.addr.bits
    val l_mask  = GenByteMask(l_addr, l_bits.uop.mem_size)

    val l_forwarders      = widthMap(w => wb_forward_valid(w) && wb_forward_ldq_idx(w) === i.U)
    val l_is_forwarding   = l_forwarders.reduce(_||_)
    val l_forward_stq_idx = Mux(l_is_forwarding, Mux1H(l_forwarders, wb_forward_stq_idx), l_bits.forward_stq_idx)


    val block_addr_matches = widthMap(w => lcam_addr(w) >> blockOffBits === l_addr >> blockOffBits)
    val dword_addr_matches = widthMap(w => block_addr_matches(w) && lcam_addr(w)(blockOffBits-1,3) === l_addr(blockOffBits-1,3))
    val mask_match   = widthMap(w => (l_mask & lcam_mask(w)) === l_mask)
    val mask_overlap = widthMap(w => (l_mask & lcam_mask(w)).orR)

    // Searcher is a store
    for (w <- 0 until lsuWidth) {

      when ( do_release_search(w)   &&
             l_valid                &&
             l_bits.addr.valid      &&
            !l_bits.addr_is_virtual &&
             block_addr_matches(w)) {
        // This load has been observed, so if a younger load to the same address has not
        // executed yet, this load must be squashed
        ldq(i).bits.observed := true.B
      }
      when (       do_st_search(w)                                                                                                &&
                   l_valid                                                                                                        &&
                   l_bits.addr.valid                                                                                              &&
                   (l_bits.executed || l_bits.succeeded || l_is_forwarding)                                                       &&
                   !l_bits.addr_is_virtual                                                                                        &&
                   l_bits.st_dep_mask(lcam_stq_idx(w))                                                                            &&
                   dword_addr_matches(w)                                                                                          &&
                   mask_overlap(w)) {

        val forwarded_is_older = IsOlder(l_forward_stq_idx, lcam_stq_idx(w), l_bits.youngest_stq_idx)
        // We are older than this load, which overlapped us.
        when (!l_bits.forward_std_val || // If the load wasn't forwarded, it definitely failed
          ((l_forward_stq_idx =/= lcam_stq_idx(w)) && forwarded_is_older)) { // If the load forwarded from us, we might be ok
          ldq(i).bits.order_fail := true.B
        }
      }
      when (       do_ld_search(w)            &&
                   l_valid                    &&
                   l_bits.addr.valid          &&
                   !l_bits.addr_is_virtual    &&
                   dword_addr_matches(w)      &&
                   mask_overlap(w)) {
        val searcher_is_older = IsOlder(lcam_ldq_idx(w), i.U, ldq_head)
        when (searcher_is_older) {
          when ((l_bits.executed || l_bits.succeeded || l_is_forwarding) &&
                !s1_executing_loads(i) && // If the load is proceeding in parallel we don't need to kill it
                l_bits.observed) {        // Its only a ordering failure if the cache line was observed between the younger load and us
            ldq(i).bits.order_fail := true.B
          }
        } .elsewhen (lcam_ldq_idx(w) =/= i.U) {
          // The load is older, and it wasn't executed
          // we need to kill ourselves, and prevent forwarding
          when (!(l_bits.executed || l_bits.succeeded)) {
            s1_set_execute(lcam_ldq_idx(w))    := false.B
            when (RegNext(dmem_req_fire(w) && !s0_kills(w)) && !fired_load_agen(w)) {
              io.dmem.s1_kill(w)               := true.B
            }
            can_forward(w)                     := false.B
          }
        }
      }
    }
  }

  for (i <- 0 until numStqEntries) {
    val s_addr = stq(i).bits.addr.bits
    val s_uop  = stq(i).bits.uop
    val dword_addr_matches = widthMap(w =>
                             ( stq(i).bits.addr.valid      &&
                              !stq(i).bits.addr_is_virtual &&
                              (s_addr(corePAddrBits-1,3) === lcam_addr(w)(corePAddrBits-1,3))))
    val write_mask = GenByteMask(s_addr, s_uop.mem_size)
    for (w <- 0 until lsuWidth) {
      when (do_ld_search(w) && stq(i).valid && lcam_st_dep_mask(w)(i)) {
        when (((lcam_mask(w) & write_mask) === lcam_mask(w)) && !s_uop.is_fence && dword_addr_matches(w) && can_forward(w))
        {
          ldst_addr_matches(w)(i)            := true.B
          ldst_forward_matches(w)(i)         := true.B
          when (RegNext(dmem_req_fire(w) && !s0_kills(w)) && !fired_load_agen(w)) {
            io.dmem.s1_kill(w)               := true.B
          }
          s1_set_execute(lcam_ldq_idx(w))    := false.B
        }
          .elsewhen (((lcam_mask(w) & write_mask) =/= 0.U) && dword_addr_matches(w))
        {
          ldst_addr_matches(w)(i)            := true.B
          when (RegNext(dmem_req_fire(w) && !s0_kills(w)) && !fired_load_agen(w)) {
            io.dmem.s1_kill(w)               := true.B
          }
          s1_set_execute(lcam_ldq_idx(w))    := false.B
        }
          .elsewhen (s_uop.is_fence || s_uop.is_amo)
        {
          ldst_addr_matches(w)(i)            := true.B
          when (RegNext(dmem_req_fire(w) && !s0_kills(w)) && !fired_load_agen(w)) {
            io.dmem.s1_kill(w)               := true.B
          }
          s1_set_execute(lcam_ldq_idx(w))    := false.B
        }
      }
    }
  }

  // Set execute bit in LDQ
  for (i <- 0 until numLdqEntries) {
    when (s1_set_execute(i)) { ldq(i).bits.executed := true.B }
  }

  // Find the youngest store which the load is dependent on
  val forwarding_age_logic = Seq.fill(lsuWidth) { Module(new ForwardingAgeLogic(numStqEntries)) }
  for (w <- 0 until lsuWidth) {
    forwarding_age_logic(w).io.addr_matches    := ldst_addr_matches(w).asUInt
    forwarding_age_logic(w).io.youngest_st_idx := lcam_uop(w).stq_idx
  }
  val forwarding_idx = widthMap(w => forwarding_age_logic(w).io.forwarding_idx)

  // Forward if st-ld forwarding is possible from the writemask and loadmask
  mem_forward_valid       := widthMap(w =>
                                  (ldst_forward_matches(w)(forwarding_idx(w))        &&
                                 !IsKilledByBranch(io.core.brupdate, lcam_uop(w))    &&
                                 !io.core.exception && !RegNext(io.core.exception)))
  mem_forward_stq_idx     := forwarding_idx

  // Avoid deadlock with a 1-w LSU prioritizing load wakeups > store commits
  // On a 2W machine, load wakeups and store commits occupy separate pipelines,
  // so only add this logic for 1-w LSU
  if (lsuWidth == 1) {
    // Wakeups may repeatedly find a st->ld addr conflict and fail to forward,
    // repeated wakeups may block the store from ever committing
    // Disallow load wakeups 1 cycle after this happens to allow the stores to drain
    when (RegNext(ldst_addr_matches(0).reduce(_||_) && !mem_forward_valid(0))) {
      block_load_wakeup := true.B
    }

    // If stores remain blocked for 15 cycles, block load wakeups to get a store through
    val store_blocked_counter = Reg(UInt(4.W))
    when (will_fire_store_commit_fast(0) || will_fire_store_commit_slow(0) || !can_fire_store_commit_slow(0)) {
      store_blocked_counter := 0.U
    } .elsewhen (can_fire_store_commit_slow(0) && !(will_fire_store_commit_slow(0) || will_fire_store_commit_fast(0))) {
      store_blocked_counter := Mux(store_blocked_counter === 15.U, store_blocked_counter + 1.U, 15.U)
    }
    when (store_blocked_counter === 15.U) {
      block_load_wakeup := true.B
    }
  }


  // Task 3: Clr unsafe bit in ROB for succesful translations
  //         Delay this a cycle to avoid going ahead of the exception broadcast
  //         The unsafe bit is cleared on the first translation, so no need to fire for load wakeups
  for (w <- 0 until lsuWidth) {
    io.core.clr_unsafe(w).valid := RegNext((do_st_search(w) || do_ld_search(w)) && !fired_load_wakeup(w)) && false.B
    io.core.clr_unsafe(w).bits  := RegNext(lcam_uop(w).rob_idx)
  }

  // detect which loads get marked as failures, but broadcast to the ROB the oldest failing load
  // TODO encapsulate this in an age-based  priority-encoder
  val l_idx = AgePriorityEncoder(ldq.map(e => e.valid && e.bits.order_fail), ldq_head)

  // one exception port, but multiple causes!
  // - 1) the incoming store-address finds a faulting load (it is by definition younger)
  // - 2) the incoming load or store address is excepting. It must be older and thus takes precedent.
  val r_xcpt_valid = RegInit(false.B)
  val r_xcpt       = Reg(new Exception)

  val ld_xcpt_valid = ldq.map(e => e.bits.order_fail && e.valid).reduce(_||_)
  val ld_xcpt_uop   = ldq(Mux(l_idx >= numLdqEntries.U, l_idx - numLdqEntries.U, l_idx)).bits.uop

  val use_mem_xcpt = (mem_xcpt_valid && IsOlder(mem_xcpt_uop.rob_idx, ld_xcpt_uop.rob_idx, io.core.rob_head_idx)) || !ld_xcpt_valid

  val xcpt_uop = Mux(use_mem_xcpt, mem_xcpt_uop, ld_xcpt_uop)

  r_xcpt_valid := (ld_xcpt_valid || mem_xcpt_valid) &&
                   !io.core.exception &&
                   !IsKilledByBranch(io.core.brupdate, xcpt_uop)
  r_xcpt.uop         := xcpt_uop
  r_xcpt.uop.br_mask := GetNewBrMask(io.core.brupdate, xcpt_uop)
  r_xcpt.cause       := Mux(use_mem_xcpt, mem_xcpt_cause, MINI_EXCEPTION_MEM_ORDERING)
  r_xcpt.badvaddr    := mem_xcpt_vaddr // TODO is there another register we can use instead?

  io.core.lxcpt.valid := r_xcpt_valid && !io.core.exception && !IsKilledByBranch(io.core.brupdate, r_xcpt.uop)
  io.core.lxcpt.bits  := r_xcpt

  // Task 4: Speculatively wakeup loads 1 cycle before they come back
  for (w <- 0 until lsuWidth) {
    io.core.spec_ld_wakeup(w).valid := (enableFastLoadUse.B                             &&
                                        (fired_load_agen_exec(w) || fired_load_agen(w)) &&
                                        !mem_incoming_uop(w).fp_val                     &&
                                        mem_incoming_uop(w).pdst =/= 0.U)
    io.core.spec_ld_wakeup(w).bits  := mem_incoming_uop(w).pdst
  }


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Writeback Cycle (St->Ld Forwarding Path)
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Handle Memory Responses and nacks
  //----------------------------------
  for (w <- 0 until lsuWidth) {
    io.core.iresp(w).valid := false.B
    io.core.fresp(w).valid := false.B
  }

  // Initially assume the speculative load wakeup failed
  io.core.ld_miss    := RegNext(io.core.spec_ld_wakeup.map(_.valid).reduce(_||_))
  val spec_ld_succeed = WireInit(widthMap(w => !RegNext(io.core.spec_ld_wakeup(w).valid)))
  when (spec_ld_succeed.reduce(_&&_)) {
    io.core.ld_miss := false.B
  }


  val dmem_resp_fired = WireInit(widthMap(w => false.B))

  for (w <- 0 until lsuWidth) {
    // Handle nacks
    when (io.dmem.nack(w).valid)
    {
      // We have to re-execute this!
      when (io.dmem.nack(w).bits.is_hella)
      {
        assert(hella_state === h_wait || hella_state === h_dead)
      }
        .elsewhen (io.dmem.nack(w).bits.uop.uses_ldq)
      {
        assert(ldq(io.dmem.nack(w).bits.uop.ldq_idx).bits.executed)
        ldq(io.dmem.nack(w).bits.uop.ldq_idx).bits.executed  := false.B
      }
        .otherwise
      {
        assert(io.dmem.nack(w).bits.uop.uses_stq)
        when (IsOlder(io.dmem.nack(w).bits.uop.stq_idx, stq_execute_head, stq_head)) {
          stq_execute_head := io.dmem.nack(w).bits.uop.stq_idx
        }
      }
    }
    // Handle the response
    when (io.dmem.resp(w).valid)
    {
      when (io.dmem.resp(w).bits.uop.uses_ldq)
      {
        assert(!io.dmem.resp(w).bits.is_hella)
        val ldq_idx = io.dmem.resp(w).bits.uop.ldq_idx
        val send_iresp = ldq(ldq_idx).bits.uop.dst_rtype === RT_FIX
        val send_fresp = ldq(ldq_idx).bits.uop.dst_rtype === RT_FLT

        io.core.iresp(w).bits.uop  := ldq(ldq_idx).bits.uop
        io.core.fresp(w).bits.uop  := ldq(ldq_idx).bits.uop
        io.core.iresp(w).valid     := send_iresp

        spec_ld_succeed(w) := send_iresp && ldq_idx === RegNext(mem_incoming_uop(w).ldq_idx)

        io.core.iresp(w).bits.data := io.dmem.resp(w).bits.data
        io.core.fresp(w).valid     := send_fresp
        io.core.fresp(w).bits.data := io.dmem.resp(w).bits.data

        assert(send_iresp ^ send_fresp)
        dmem_resp_fired(w) := true.B

        ldq(ldq_idx).bits.succeeded      := io.core.iresp(w).valid || io.core.fresp(w).valid
        ldq(ldq_idx).bits.debug_wb_data  := io.dmem.resp(w).bits.data
      }
        .elsewhen (io.dmem.resp(w).bits.uop.uses_stq)
      {
        assert(!io.dmem.resp(w).bits.is_hella && io.dmem.resp(w).bits.uop.is_amo)
        dmem_resp_fired(w) := true.B
        io.core.iresp(w).valid     := true.B
        io.core.iresp(w).bits.uop  := stq(io.dmem.resp(w).bits.uop.stq_idx).bits.uop
        io.core.iresp(w).bits.data := io.dmem.resp(w).bits.data

        stq(io.dmem.resp(w).bits.uop.stq_idx).bits.debug_wb_data := io.dmem.resp(w).bits.data
      }
    }
    // Handle store acks
    when (io.dmem.store_ack(w).valid) {
      stq(io.dmem.store_ack(w).bits.stq_idx).bits.succeeded := true.B
    }


    when (dmem_resp_fired(w) && wb_forward_valid(w))
    {
      // Twiddle thumbs. Can't forward because dcache response takes precedence
    }
      .elsewhen (!dmem_resp_fired(w) && wb_forward_valid(w))
    {
      val f_idx       = wb_forward_ldq_idx(w)
      val forward_uop = ldq(f_idx).bits.uop
      val stq_e       = stq(wb_forward_stq_idx(w))
      val data_ready  = stq_e.bits.data.valid && stq_e.valid
      val live        = !IsKilledByBranch(io.core.brupdate, forward_uop)
      val storegen = new freechips.rocketchip.rocket.StoreGen(
                                stq_e.bits.uop.mem_size, stq_e.bits.addr.bits,
                                stq_e.bits.data.bits, coreDataBytes)
      val loadgen  = new freechips.rocketchip.rocket.LoadGen(
                                forward_uop.mem_size, forward_uop.mem_signed,
                                wb_forward_ld_addr(w),
                                storegen.data, false.B, coreDataBytes)

      io.core.iresp(w).valid := (forward_uop.dst_rtype === RT_FIX) && data_ready && live
      io.core.fresp(w).valid := (forward_uop.dst_rtype === RT_FLT) && data_ready && live
      io.core.iresp(w).bits.uop  := forward_uop
      io.core.fresp(w).bits.uop  := forward_uop
      io.core.iresp(w).bits.data := loadgen.data
      io.core.fresp(w).bits.data := loadgen.data

      when (data_ready && live) {
        ldq(f_idx).bits.succeeded := data_ready
        ldq(f_idx).bits.forward_std_val := true.B
        ldq(f_idx).bits.forward_stq_idx := wb_forward_stq_idx(w)

        ldq(f_idx).bits.debug_wb_data   := loadgen.data
      }
    }
  }



  //-------------------------------------------------------------
  // Kill speculated entries on branch mispredict
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Kill stores
  val st_brkilled_mask = Wire(Vec(numStqEntries, Bool()))
  for (i <- 0 until numStqEntries)
  {
    st_brkilled_mask(i) := false.B

    when (stq(i).valid)
    {
      stq(i).bits.uop.br_mask := GetNewBrMask(io.core.brupdate, stq(i).bits.uop.br_mask)

      when (IsKilledByBranch(io.core.brupdate, stq(i).bits.uop))
      {
        stq(i).valid           := false.B
        stq(i).bits.addr.valid := false.B
        stq(i).bits.data.valid := false.B
        st_brkilled_mask(i)    := true.B
      }
    }

    assert (!(IsKilledByBranch(io.core.brupdate, stq(i).bits.uop) && stq(i).valid && stq(i).bits.committed),
      "Branch is trying to clear a committed store.")
  }

  // Kill loads
  for (i <- 0 until numLdqEntries)
  {
    when (ldq(i).valid)
    {
      ldq(i).bits.uop.br_mask := GetNewBrMask(io.core.brupdate, ldq(i).bits.uop.br_mask)
      when (IsKilledByBranch(io.core.brupdate, ldq(i).bits.uop))
      {
        ldq(i).valid           := false.B
        ldq(i).bits.addr.valid := false.B
      }
    }
  }

  //-------------------------------------------------------------
  when (io.core.brupdate.b2.mispredict && !io.core.exception)
  {
    stq_tail := io.core.brupdate.b2.uop.stq_idx
    ldq_tail := io.core.brupdate.b2.uop.ldq_idx
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // dequeue old entries on commit
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  var temp_stq_commit_head = stq_commit_head
  var temp_ldq_head        = ldq_head
  for (w <- 0 until coreWidth)
  {
    val commit_store = io.core.commit.valids(w) && io.core.commit.uops(w).uses_stq
    val commit_load  = io.core.commit.valids(w) && io.core.commit.uops(w).uses_ldq
    val idx = Mux(commit_store, temp_stq_commit_head, temp_ldq_head)
    when (commit_store)
    {
      stq(idx).bits.committed := true.B
    } .elsewhen (commit_load) {
      assert (ldq(idx).valid, "[lsu] trying to commit an un-allocated load entry.")
      assert ((ldq(idx).bits.executed || ldq(idx).bits.forward_std_val) && ldq(idx).bits.succeeded ,
        "[lsu] trying to commit an un-executed load entry.")

      ldq(idx).valid                 := false.B
    }

    if (MEMTRACE_PRINTF) {
      when (commit_store || commit_load) {
        val uop    = Mux(commit_store, stq(idx).bits.uop, ldq(idx).bits.uop)
        val addr   = Mux(commit_store, stq(idx).bits.addr.bits, ldq(idx).bits.addr.bits)
        val stdata = Mux(commit_store, stq(idx).bits.data.bits, 0.U)
        val wbdata = Mux(commit_store, stq(idx).bits.debug_wb_data, ldq(idx).bits.debug_wb_data)
        printf("MT %x %x %x %x %x %x %x\n",
          io.core.tsc_reg, uop.uopc, uop.mem_cmd, uop.mem_size, addr, stdata, wbdata)
      }
    }

    temp_stq_commit_head = Mux(commit_store,
                               WrapInc(temp_stq_commit_head, numStqEntries),
                               temp_stq_commit_head)

    temp_ldq_head        = Mux(commit_load,
                               WrapInc(temp_ldq_head, numLdqEntries),
                               temp_ldq_head)
  }
  stq_commit_head := temp_stq_commit_head
  ldq_head        := temp_ldq_head

  // store has been committed AND successfully sent data to memory
  when (stq(stq_head).valid && stq(stq_head).bits.committed)
  {
    when (stq(stq_head).bits.uop.is_fence && !io.dmem.ordered) {
      io.dmem.force_order := true.B
      store_needs_order   := true.B
    }
    clear_store := Mux(stq(stq_head).bits.uop.is_fence, io.dmem.ordered,
                                                        stq(stq_head).bits.succeeded)
  }

  when (clear_store)
  {
    stq(stq_head).valid           := false.B

    stq_head := WrapInc(stq_head, numStqEntries)
    when (stq(stq_head).bits.uop.is_fence)
    {
      stq_execute_head := WrapInc(stq_execute_head, numStqEntries)
    }
  }


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
    hella_xcpt := dtlb.io.resp(0)

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
    for (w <- 0 until lsuWidth) {
      when (io.dmem.resp(w).valid && io.dmem.resp(w).bits.is_hella) {
        hella_state := h_ready

        io.hellacache.resp.valid       := true.B
        io.hellacache.resp.bits.addr   := hella_req.addr
        io.hellacache.resp.bits.tag    := hella_req.tag
        io.hellacache.resp.bits.cmd    := hella_req.cmd
        io.hellacache.resp.bits.signed := hella_req.signed
        io.hellacache.resp.bits.size   := hella_req.size
        io.hellacache.resp.bits.data   := io.dmem.resp(w).bits.data
      } .elsewhen (io.dmem.nack(w).valid && io.dmem.nack(w).bits.is_hella) {
        hella_state := h_replay
      }
    }
  } .elsewhen (hella_state === h_replay) {
    can_fire_hella_wakeup(0) := true.B

    when (will_fire_hella_wakeup(0) && dmem_req_fire(0)) {
      hella_state := h_wait
    }
  } .elsewhen (hella_state === h_dead) {
    for (w <- 0 until lsuWidth) {
      when (io.dmem.resp(w).valid && io.dmem.resp(w).bits.is_hella) {
        hella_state := h_ready
      }
    }
  }

  //-------------------------------------------------------------
  // Exception / Reset

  // for the live_store_mask, need to kill stores that haven't been committed
  val st_exc_killed_mask = WireInit(VecInit((0 until numStqEntries).map(x=>false.B)))

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

      for (i <- 0 until numStqEntries)
      {
        stq(i).valid           := false.B
      }
    }
      .otherwise // exception
    {
      stq_tail := stq_commit_head

      for (i <- 0 until numStqEntries)
      {
        when (!stq(i).bits.committed && !stq(i).bits.succeeded)
        {
          stq(i).valid           := false.B
          st_exc_killed_mask(i)  := true.B
        }
      }
    }

    for (i <- 0 until numLdqEntries)
    {
      ldq(i).valid           := false.B
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
      val youngest_st_idx = Input(UInt(stqAddrSz.W)) // needed to get "age"

      val forwarding_val  = Output(Bool())
      val forwarding_idx  = Output(UInt(stqAddrSz.W))
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
