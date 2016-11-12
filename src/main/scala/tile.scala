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

class BOOMTile(implicit p: Parameters) extends rocket.LazyTile
{
   val slave = if (p(rocket.DataScratchpadSize) == 0) None else Some(uncore.tilelink2.TLInputNode())
   val scratch = if (p(rocket.DataScratchpadSize) == 0) None else Some(diplomacy.LazyModule(new rocket.ScratchpadSlavePort()(dcacheParams)))

   (slave zip scratch) foreach { case (node, lm) => lm.node := uncore.tilelink2.TLFragmenter(p(rocket.XLen)/8, p(uncore.agents.CacheBlockBytes))(node) }

   // I don't know what this is, but I probably don't support it. - Chris
   require (p(rocket.DataScratchpadSize) == 0)

   lazy val module = new rocket.TileImp(this) 
   {
      val io = new rocket.TileIO(bc, slave)
      val buildRocc = p(rocket.BuildRoCC)
      val usingRocc = !buildRocc.isEmpty
      val nRocc = buildRocc.size
      val nFPUPorts = buildRocc.filter(_.useFPU).size
      require (!usingRocc) // we don't support rocc
      require (nFPUPorts == 0) // we don't support rocc shared FPUs

      val core = Module(new BOOMCore())
      val icache = Module(new rocket.Frontend()(p.alterPartial({
         case uncore.agents.CacheName => "L1I"
         })))
      val dcache = rocket.HellaCache(p(rocket.DCacheKey))(dcacheParams)
      val dc_shim = Module(new DCacheShim()(dcacheParams))

      val ptwPorts = collection.mutable.ArrayBuffer(icache.io.ptw, core.io.ptw_tlb)
      val dcPorts = collection.mutable.ArrayBuffer(dc_shim.io.dmem)
      val uncachedArbPorts = collection.mutable.ArrayBuffer(icache.io.mem)
      val uncachedPorts = collection.mutable.ArrayBuffer[uncore.tilelink.ClientUncachedTileLinkIO]()
      val cachedPorts = collection.mutable.ArrayBuffer(dcache.mem)
      core.io.interrupts := io.interrupts
      core.io.hartid := io.hartid
      dc_shim.io.core <> core.io.dmem
      icache.io.cpu <> core.io.imem
      icache.io.resetVector := io.resetVector

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
         dcache.ptw.status <> ptw.io.requestor(1).status
         dcache.ptw.invalidate := ptw.io.requestor(1).invalidate
         dcache.ptw.req.ready := Bool(false)
         dcache.ptw.resp.valid := Bool(false)
      }

      scratch.foreach { lm => lm.module.io.dmem +=: dcPorts }

      require(dcPorts.size == core.dcacheArbPorts)
      val dcArb = Module(new rocket.HellaCacheArbiter(dcPorts.size)(dcacheParams))
      dcArb.io.requestor <> dcPorts
      dcache.cpu <> dcArb.io.mem
      dcache.cpu.invalidate_lr := core.io.dmem.invalidate_lr


      // Cache Counters
      core.io.counters.dc_miss := dcache.mem.acquire.fire()
      core.io.counters.ic_miss := icache.io.mem.acquire.fire()
   }
}

