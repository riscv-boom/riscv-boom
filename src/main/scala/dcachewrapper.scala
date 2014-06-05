//**************************************************************************
// Data Cache Wrapper to the Hella-Cache
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2012 Oct 15

// We need to track inflight loads that may have been misspeculated, and filter
// them out before they can be returned to the pipeline (we do not want to hold
// up pipeline resources like LD/ST entries on them).

// Also, the hellacache was designed for a 5-stage pipeline, and has some
// pecularities regarding nacks, kills, store-data forwarding, etc.
//
// Contract:
//    everything put in here will be executed by memory
//    branch/kill signals will filter resp_val signals, but otherwise continue on


// TODO handle memory misalignment exceptions...

package BOOM
{

import Chisel._
import Node._

import uncore._
 
import rocket.DCacheConfig
import rocket.ICacheConfig
 

// Track Inflight Memory Requests
class LoadReqSlotIo extends Bundle
{
   val valid      = Bool(OUTPUT) //slot has an entry
   
   val wen        = Bool(INPUT)
   val in_uop     = new MicroOp().asInput() //need ldq_idx, brmask

   val clear      = Bool(INPUT) // kill slot immediately (either nacked or succeeded)
   val brinfo     = new BrResolutionInfo().asInput() 
   val flush_pipe = Bool(INPUT) // exceptions, etc. but keep slot valid
   
   val out_uop    = new MicroOp().asOutput() //need ldq_idx

   val was_killed = Bool(OUTPUT) // should we filter out returning mem op?
}

// Note: Anything incoming that gets killed by br or exception is still marked
// as "valid", since it also got sent to the datacache.
class LoadReqSlot extends Module
{
   val io = new LoadReqSlotIo()

   val valid      = Reg(init=Bool(false))
   val was_killed = Reg(init=Bool(false))
   val uop        = Reg(outType=(new MicroOp()))

   // did the existing uop get killed by a branch?
   val br_killed = Bool()
   br_killed := Bool(false)
   // Note: we need to check/clr br_mask for incoming uop, despite previous MAddr
   // unit will have already performed that for us, the LSU waking up loads may not have.
   val br_killed_incoming = Bool()
   br_killed_incoming := Bool(false)

   // only allow the clearing of a valid entry
   // otherwise we might overwrite an incoming entry
   when (io.clear && valid)
   {
      //assert (valid)
      valid      := Bool(false)
   }
   .elsewhen (io.wen)
   {
      valid      := Bool(true)
      was_killed := io.flush_pipe || br_killed_incoming 
      uop        := io.in_uop
   }
   .elsewhen (io.flush_pipe || br_killed)
   {
      was_killed := Bool(true)
   }

   val bmask_match = io.brinfo.valid && maskMatch(io.brinfo.mask, uop.br_mask)
   val bmask_match_incoming = io.brinfo.valid && maskMatch(io.brinfo.mask, io.in_uop.br_mask)
   
   // Handle the Branch Mask (incoming not necessarily handled, if coming uop from LSU)
   when (io.wen)
   {
      uop.br_mask := Mux(io.brinfo.valid, (io.in_uop.br_mask & ~io.brinfo.mask),
                                          (io.in_uop.br_mask))
   }
   .elsewhen (bmask_match)
   {
      uop.br_mask := uop.br_mask & ~io.brinfo.mask
   }

   // handle branch killed
   when (io.brinfo.mispredict)
   {
      when (bmask_match)
      {
         br_killed := Bool(true)
      }
      when (bmask_match_incoming)
      {
         br_killed_incoming := Bool(true)
      }
   }

   // outputs
   io.valid      := valid
   io.was_killed := was_killed || br_killed //handles branch killing us same cycle as resp is valid
   io.out_uop    := uop
}
 
class DCacheReq(implicit conf: DCacheConfig) extends Bundle
{
   val addr    = UInt(width = conf.ppnbits.max(conf.vpnbits+1) + conf.pgidxbits)
   val uop     = new MicroOp()
   val data    = Bits(width = conf.databits)
   val kill    = Bool()    // e.g., LSU detects load misspeculation 

   override def clone = new DCacheReq().asInstanceOf[this.type]
}

class NackInfo extends Bundle
{
   val valid      = Bool()
   val lsu_idx    = UInt(width = MEM_ADDR_SZ)
   val isload     = Bool()
   val cache_nack = Bool() // was the cache nacking us, or the LSU
                           // cache nacks for stuctural hazards
                           // LSU nacks for address conflicts/forwarding
}
                                                                               
class DCacheResp(implicit conf: DCacheConfig) extends Bundle 
{            
   val data   = Bits(width = conf.databits)
   val uop    = new MicroOp
   val xcpt   = (new rocket.HellaCacheExceptions).asInput()
   // TODO should nack go in here?
   
   override def clone = new DCacheResp().asInstanceOf[this.type]
}


// from pov of datapath
class DCMemPortIo(implicit conf: DCacheConfig) extends Bundle 
{
   // TODO provide "hellacacheIO" to connect to D$ (via an arbiter)
   val req    = (new DecoupledIO(new DCacheReq))
   val resp   = (new ValidIO(new DCacheResp)).flip

   val brinfo = new BrResolutionInfo().asOutput() 
   val nack   = new NackInfo().asInput() 
   val flush_pipe  = Bool(OUTPUT) //exception or other misspec which flushes entire pipeline
   val ordered = Bool(INPUT) // is the dcache ordered? (fence is done)

   val ptw = new rocket.TLBPTWIO()(conf.as).flip
//   val status = new Status().asOutput

   val debug = new Bundle
   {
      val memreq = Bool()
      val memresp = Bool()
      val req_kill = Bool()
      val nack = Bool()
      val cache_nack = Bool()
      val cache_resp_idx = UInt(width=log2Up(MAX_LD_COUNT))

      val ld_req_slot = Vec.fill(MAX_LD_COUNT) { new Bundle {
         val valid = Bool()
         val killed = Bool()
         val uop = new MicroOp()
      }}
   }.asInput
}

class DCacheWrapper(implicit conf: DCacheConfig, lnconf: TileLinkConfiguration) extends Module
{
   val max_num_inflight = MAX_LD_COUNT
   isPow2(max_num_inflight)

   val io = new Bundle 
   {
      val core = (new DCMemPortIo()).flip
      val mem  = new TileLinkIO
   }


   //------------------------------------------------------------
   // The thing we're wrapping
   val nbdcache = Module(new rocket.HellaCache)

   // Hook nbdcache's tilelink straight out
   io.mem <> nbdcache.io.mem

//   nbdcache.io.cpu.ptw.status := io.core.status
   nbdcache.io.cpu.ptw <> io.core.ptw

   // we are going to ignore store acks (for now at least), so filter them out and only listen to load acks
   // we know the store succeeded if it was not nacked
   val nbdcache_load_ack = nbdcache.io.cpu.resp.valid && nbdcache.io.cpu.resp.bits.has_data

   //------------------------------------------------------------

   val inflight_load_buffer  = Vec.fill(max_num_inflight) {Module(new LoadReqSlot()).io}
   
   val m1_inflight_tag  = Bits() // one cycle ago, aka now in the Mem1 Stage
   val m2_inflight_tag  = Bits() // two cycles ago, aka now in the Mem2 Stage
   val m2_req_uop       = Reg(next=Reg(next=io.core.req.bits.uop)) // nack signals come two cycles later
   
   val enq_val = io.core.req.valid && io.core.req.bits.uop.is_load
   val enq_rdy = Bool()
        
   for (i <- 0 until max_num_inflight)
   {
      inflight_load_buffer(i).clear       := (nbdcache_load_ack && nbdcache.io.cpu.resp.bits.tag === UInt(i)) ||
                                             (nbdcache.io.cpu.resp.bits.nack && m2_req_uop.is_load && m2_inflight_tag === UInt(i) && Reg(next=Reg(next=(enq_val && enq_rdy)))) || 
                                             (io.core.req.bits.kill && m1_inflight_tag === UInt(i) && Reg(next=(enq_val && enq_rdy))) // don't clr random entry, make sure m1_tag is correct
      inflight_load_buffer(i).brinfo      := io.core.brinfo
      inflight_load_buffer(i).flush_pipe  := io.core.flush_pipe
      inflight_load_buffer(i).in_uop      := io.core.req.bits.uop
   }


   // dispatch/entry logic
   val enq_idx = UInt(width = log2Up(max_num_inflight))
   enq_idx := UInt(0)

   for (i <- max_num_inflight-1 to 0 by -1)
   {
      when (!inflight_load_buffer(i).valid) // TODO do I need to check for m2_inflight_tag too?
      {
         enq_idx := UInt(i)
      }
   }
 
   // ready logic (is there a inflight buffer slot that's not valid yet?
   enq_rdy := Bool(false)
   for (i <- 0 until max_num_inflight)
   {
      when (!inflight_load_buffer(i).valid && nbdcache.io.cpu.req.ready)
      {
         enq_rdy := Bool(true)
      }
   }
                       
   val new_inflight_tag = enq_idx
   m2_inflight_tag := Reg(next=Reg(next=enq_idx))
   m1_inflight_tag := Reg(next=enq_idx)

   val enq_can_occur = enq_val && enq_rdy 

   val enq_idx_1h = (Bits(1) << enq_idx) & 
                  Fill(enq_can_occur, max_num_inflight) 


   for (i <- 0 until max_num_inflight)
   {
      inflight_load_buffer(i).wen := enq_idx_1h(i) 
   }

   // NOTE: if !enq_rdy, then we have to kill the memory request, and nack the LSU
   // inflight load buffer resource hazard
   val iflb_kill = Reg(next=(enq_val && !enq_rdy))


   // try to catch if there's a resource leak
   val full_counter = Reg(init=UInt(0,32))
   when (enq_rdy) { full_counter := UInt(0) }
   .otherwise     { full_counter := full_counter + UInt(1) }

   assert(full_counter <= UInt(10000), "Inflight buffers have been busy for 10k cycles. Probably a resource leak.")

 
   //------------------------------------------------------------
   //-- Data Prefetcher
   //------------------------------------------------------------
   // listen in on the core<->cache requests/responses, and insert our own
   // prefetch requests to the data cache

//   val prefetcher = Module(new Prefetcher())
//       
//      prefetcher.io.core_requests.valid := Reg(next=Reg(next=io.core.req.valid))
//      prefetcher.io.core_requests.bits.addr := Reg(next=Reg(next=io.core.req.bits.addr))
//      // TODO add back miss, secondary_miss to the nbdcache
//      prefetcher.io.core_requests.bits.miss := Bool(false) //Reg(next=Reg(next=io.core.req.valid)) && nbdcache.io.cpu.resp.bits.miss
//      prefetcher.io.core_requests.bits.secondary_miss := Bool(false) 
////                                                         Reg(next=Reg(next=io.core.req.valid)) && 
////                                                         nbdcache.io.cpu.resp.bits.miss &&
////                                                         nbdcache.io.cpu.resp.bits.secondary_miss
//
//      prefetcher.io.cache.req.ready := !io.core.req.valid && nbdcache.io.cpu.req.ready

   

   //------------------------------------------------------------
   // hook up requests

//   val store_data_gen = Module(new StoreDataGen())
//      store_data_gen.io.typ := io.core.req.bits.uop.mem_typ
//      store_data_gen.io.din := io.core.req.bits.data
   

//   val prefetch_req_val = prefetcher.io.cache.req.valid && Bool(ENABLE_PREFETCHING)
   val prefetch_req_val = Bool(false)

   io.core.req.ready              := enq_rdy && nbdcache.io.cpu.req.ready 
   nbdcache.io.cpu.req.valid      := (io.core.req.valid || prefetch_req_val)
   nbdcache.io.cpu.req.bits.kill  := io.core.req.bits.kill || iflb_kill
                                          // kills request sent out last cycle
   nbdcache.io.cpu.req.bits.typ   := io.core.req.bits.uop.mem_typ
//   nbdcache.io.cpu.req.bits.addr  := Mux(io.core.req.valid, io.core.req.bits.addr,prefetcher.io.cache.req.bits.addr) TODO get core.req.valid off critical path here
   nbdcache.io.cpu.req.bits.addr  := io.core.req.bits.addr
   nbdcache.io.cpu.req.bits.tag   := Cat(!io.core.req.valid, new_inflight_tag)
   nbdcache.io.cpu.req.bits.cmd   := Mux(io.core.req.valid, io.core.req.bits.uop.mem_cmd, M_PFW)
   nbdcache.io.cpu.req.bits.data  := Reg(next=io.core.req.bits.data) //notice this is delayed a cycle
   nbdcache.io.cpu.req.bits.phys  := Bool(true) // use physical address? otherwise, use status bit of is VM enabled

   //------------------------------------------------------------
   // handle responses and nacks
   
   // note: nacks come two cycles after a response, so I'm delaying everything
   // properly to line up stores, loads, nacks, and subword loads
   val was_store  = !m2_req_uop.is_load && Reg(next=Reg(next=(io.core.req.valid && nbdcache.io.cpu.req.ready)))  // was two cycles ago a store request?

   
   // Todo add entry valid bit?
   val resp_idx   = nbdcache.io.cpu.resp.bits.tag

   io.core.resp.valid := Mux(nbdcache_load_ack,                            !inflight_load_buffer(resp_idx).was_killed, // hide loads that were killed due to branches, etc.
                         Mux(was_store && !nbdcache.io.cpu.resp.bits.nack,  Bool(true),    // stores succeed quietly, so valid if no nack
                                                                            Bool(false)))  // filter out nacked responses

   io.core.resp.bits.data := nbdcache.io.cpu.resp.bits.data_subword   // comes out the same cycle as the resp.valid signal
                                                                      // but is a few gates slower than resp.bits.data 
   
   io.core.resp.bits.uop := Mux(nbdcache_load_ack, inflight_load_buffer(resp_idx).out_uop,
                                                            m2_req_uop)
   //------------------------------------------------------------
   // handle nacks from the cache (or from the IFLB or the LSU)

   io.core.nack.valid     := (nbdcache.io.cpu.resp.bits.nack) || Reg(next=io.core.req.bits.kill) || Reg(next=iflb_kill) || 
                              Reg(next=Reg(next=(io.core.req.valid && !(nbdcache.io.cpu.req.ready))))
   io.core.nack.lsu_idx   := Mux(m2_req_uop.is_load, m2_req_uop.ldq_idx, m2_req_uop.stq_idx)
   io.core.nack.isload    := m2_req_uop.is_load
   io.core.nack.cache_nack:= nbdcache.io.cpu.resp.bits.nack || Reg(next=iflb_kill) || Reg(next=Reg(next= (!(nbdcache.io.cpu.req.ready))))
   
   //------------------------------------------------------------
   // Handle exceptions and fences
   io.core.resp.bits.xcpt := nbdcache.io.cpu.xcpt
   io.core.ordered := nbdcache.io.cpu.ordered
   
   //------------------------------------------------------------
   // debug

   io.core.debug.memreq := io.core.req.valid
   io.core.debug.memresp := io.core.resp.valid
   io.core.debug.req_kill := io.core.req.bits.kill
   io.core.debug.nack := io.core.nack.valid
   io.core.debug.cache_nack := io.core.nack.cache_nack
   io.core.debug.cache_resp_idx := resp_idx

   for (i <- 0 until max_num_inflight)
   {
      io.core.debug.ld_req_slot(i).valid := inflight_load_buffer(i).valid
      io.core.debug.ld_req_slot(i).killed := inflight_load_buffer(i).was_killed
      io.core.debug.ld_req_slot(i).uop := inflight_load_buffer(i).out_uop
   }

   //------------------------------------------------------------

}

}
