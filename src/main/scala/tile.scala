//**************************************************************************
// RISCV Processor Tile
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2012 Feb 5
//
// Describes a RISC-V Out-of-Order processor tile

package BOOM
{

import Chisel._
import Node._
import uncore._
import rocket.CoreName

class BOOMTile(resetSignal: Bool = null) extends rocket.Tile(resetSignal)
{
   val dcachePortId = 0
   val icachePortId = 1

   val core = Module(new Core, { case CoreName => "BOOM"})
   val icache = Module(new rocket.Frontend(btb_updates_out_of_order=true), { case CacheName => "L1I"; case CoreName => "BOOM" })
   val dcache = Module(new rocket.HellaCache, { case CacheName => "L1D" })
   val dc_shim = Module(new DCacheShim)
   val ptw = Module(new rocket.PTW(params(rocket.NPTWPorts)))

   val dcArb = Module(new rocket.HellaCacheArbiter(params(rocket.NDCachePorts)))
   dcArb.io.requestor(0) <> ptw.io.mem
   dcArb.io.requestor(1) <> dc_shim.io.dmem
   dcArb.io.mem <> dcache.io.cpu

   ptw.io.requestor(0) <> icache.io.ptw
   ptw.io.requestor(1) <> core.io.ptw_tlb

   // the dcache's built-in TLB will be unused, but it still needs some of the
   // status/sret signals for things such as lr/sc
   //ptw.io.requestor(1) <> dcache.io.cpu.ptw
   dcache.io.cpu.sret := core.io.dmem.sret
   dcache.io.ptw.status <> ptw.io.requestor(1).status
   dcache.io.ptw.invalidate := ptw.io.requestor(1).invalidate

   core.io.host <> io.host
   core.io.imem <> icache.io.cpu
   core.io.dmem <> dc_shim.io.core
   core.io.ptw_dat <> ptw.io.dpath


   // Connect the caches and ROCC to the outer memory system
   io.cached <> dcache.io.mem
   // If so specified, build an RoCC module and wire it in
   // otherwise, just hookup the icache
   io.uncached <> params(rocket.BuildRoCC).map { buildItHere =>
      val rocc = buildItHere()
      val memArb = Module(new rocket.RocketUncachedTileLinkIOArbiter(3))
      val dcIF = Module(new rocket.SimpleHellaCacheIF)
      core.io.rocc <> rocc.io
      dcIF.io.requestor <> rocc.io.mem
      dcArb.io.requestor(2) <> dcIF.io.cache
      memArb.io.in(0) <> icache.io.mem
      memArb.io.in(1) <> rocc.io.imem
      memArb.io.in(2) <> rocc.io.dmem
      ptw.io.requestor(2) <> rocc.io.iptw
      ptw.io.requestor(3) <> rocc.io.dptw
      ptw.io.requestor(4) <> rocc.io.pptw
      memArb.io.out
     }.getOrElse(icache.io.mem)

   // Cache Counters
   val cache_counters = new CacheCounters()
   cache_counters.dc_miss := dcache.io.mem.acquire.fire().toBool
   cache_counters.ic_miss := icache.io.mem.acquire.fire()
   core.io.counters := cache_counters
}

}

