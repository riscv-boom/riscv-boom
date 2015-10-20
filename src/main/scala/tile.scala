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
{

import Chisel._
import Node._
import uncore._
import rocket.CoreName

class BOOMTile(resetSignal: Bool = null)(implicit p: Parameters) extends rocket.Tile(resetSignal)(p)
{
   val dcacheParams = p.alterPartial({ case CacheName => "L1D" })

   val core = Module(new Core()(p.alterPartial({case CoreName => "BOOM"})))
   val icache = Module(new rocket.Frontend()(p.alterPartial({
      case CacheName => "L1I"
      case CoreName => "BOOM"})))
   val dcache = Module(new rocket.HellaCache()(dcacheParams))
   val dc_shim = Module(new DCacheShim()(dcacheParams))
   val ptw = Module(new rocket.PTW(nPTWPorts)(dcacheParams))

   val dcArb = Module(new rocket.HellaCacheArbiter(nDCachePorts)(dcacheParams))
   dcArb.io.requestor(0) <> ptw.io.mem
   dcArb.io.requestor(1) <> dc_shim.io.dmem
   dcArb.io.mem <> dcache.io.cpu

   ptw.io.requestor(0) <> icache.io.ptw
   ptw.io.requestor(1) <> core.io.ptw_tlb

   // the dcache's built-in TLB will be unused, but it still needs some of the
   // status/sret signals for things such as lr/sc
   //ptw.io.requestor(1) <> dcache.io.cpu.ptw
   dcache.io.cpu.invalidate_lr := core.io.dmem.invalidate_lr
   dcache.io.ptw.status <> ptw.io.requestor(1).status
   dcache.io.ptw.invalidate := ptw.io.requestor(1).invalidate

   core.io.host <> io.host
   core.io.imem <> icache.io.cpu
   core.io.dmem <> dc_shim.io.core
   core.io.ptw_dat <> ptw.io.dpath

   // Connect the caches and ROCC to the outer memory system
   io.cached.head <> dcache.io.mem
   // If so specified, build an RoCC module and wire it in
   // otherwise, just hookup the icache
   io.uncached <> p(TileKey).buildRoCC.map { buildItHere =>
     val rocc = buildItHere(p)
     val iMemArb = Module(new ClientTileLinkIOArbiter(2))
     val dcIF = Module(new SimpleHellaCacheIF()(dcacheParams))
     core.io.rocc <> rocc.io
     dcIF.io.requestor <> rocc.io.mem
     dcArb.io.requestor(2) <> dcIF.io.cache
     iMemArb.io.in(0) <> icache.io.mem
     iMemArb.io.in(1) <> rocc.io.imem
     ptw.io.requestor(2) <> rocc.io.iptw
     ptw.io.requestor(3) <> rocc.io.dptw
     ptw.io.requestor(4) <> rocc.io.pptw
     rocc.io.dmem :+ iMemArb.io.out
   }.getOrElse(List(icache.io.mem))

   // Cache Counters
   core.io.counters.dc_miss := dcache.io.mem.acquire.fire().toBool
   core.io.counters.ic_miss := icache.io.mem.acquire.fire()
}

}
