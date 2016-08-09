//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Tile
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2012 Feb 5
//
// Describes a RISC-V Out-of-Order processor tile

package boom

import Chisel._
import cde.{Parameters, Field}

class BOOMTile(clockSignal: Clock = null, resetSignal: Bool = null)
   (implicit p: Parameters) extends rocket.Tile(clockSignal, resetSignal)(p)
{
   val core = Module(new BOOMCore()(p.alterPartial({case rocket.CoreName => "BOOM"})))
   val icache = Module(new rocket.Frontend()(p.alterPartial({
      case uncore.agents.CacheName => "L1I"
      case rocket.CoreName => "BOOM"})))
   val dcache = Module(new rocket.HellaCache()(dcacheParams))
   val dc_shim = Module(new DCacheShim()(dcacheParams))

   val ptwPorts = collection.mutable.ArrayBuffer(icache.io.ptw, core.io.ptw_tlb)
   val dcPorts = collection.mutable.ArrayBuffer(dc_shim.io.dmem)
   val uncachedArbPorts = collection.mutable.ArrayBuffer(icache.io.mem)
   val uncachedPorts = collection.mutable.ArrayBuffer[uncore.tilelink.ClientUncachedTileLinkIO]()
   val cachedPorts = collection.mutable.ArrayBuffer(dcache.io.mem)
   core.io.prci <> io.prci
   dc_shim.io.core <> core.io.dmem
   icache.io.cpu <> core.io.imem


   val uncachedArb = Module(new uncore.tilelink.ClientUncachedTileLinkIOArbiter(uncachedArbPorts.size))
   uncachedArb.io.in <> uncachedArbPorts
   uncachedArb.io.out +=: uncachedPorts

   // Connect the caches and RoCC to the outer memory system
   io.uncached <> uncachedPorts
   io.cached <> cachedPorts
   // TODO remove nCached/nUncachedTileLinkPorts parameters and these assertions
   require(uncachedPorts.size == nUncachedTileLinkPorts)
   require(cachedPorts.size == nCachedTileLinkPorts)

   if (p(rocket.UseVM))
   {
      val ptw = Module(new rocket.PTW(ptwPorts.size)(dcacheParams))
      ptw.io.requestor <> ptwPorts
      ptw.io.mem +=: dcPorts
      core.io.ptw_dat <> ptw.io.dpath

      // the dcache's built-in TLB will be unused, but it still needs some of the
      // status/sret signals for things such as lr/sc
      dcache.io.ptw.status <> ptw.io.requestor(1).status
      dcache.io.ptw.invalidate := ptw.io.requestor(1).invalidate
      dcache.io.ptw.req.ready := Bool(false)
      dcache.io.ptw.resp.valid := Bool(false)
   }

   val dcArb = Module(new rocket.HellaCacheArbiter(dcPorts.size)(dcacheParams))
   dcArb.io.requestor <> dcPorts
   dcache.io.cpu <> dcArb.io.mem
   dcache.io.cpu.invalidate_lr := core.io.dmem.invalidate_lr


   // Cache Counters
   core.io.counters.dc_miss := dcache.io.mem.acquire.fire()
   core.io.counters.ic_miss := icache.io.mem.acquire.fire()
}

