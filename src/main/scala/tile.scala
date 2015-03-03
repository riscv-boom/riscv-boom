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
import rocket.NTilePorts
import rocket.NPTWPorts
import rocket.PTW
import rocket.Tile


class BOOMTile(resetSignal: Bool = null) extends Tile(resetSignal)
{
  val dcachePortId = 0
  val icachePortId = 1

  val core = Module(new Core, { case CoreName => "BOOM"})
  val icache = Module(new rocket.Frontend(btb_updates_out_of_order=true), { case CacheName => "L1I"; case CoreName => "BOOM" })
  val dcache = Module(new rocket.HellaCache, { case CacheName => "L1D" })
  val dc_shim = Module(new DCacheShim)
  val ptw = Module(new PTW(params(NPTWPorts)))

  val dcArb = Module(new rocket.HellaCacheArbiter(params(rocket.NDCachePorts)))
  dcArb.io.requestor(0) <> ptw.io.mem
  dcArb.io.requestor(1) <> dc_shim.io.dmem
  dcArb.io.mem <> dcache.io.cpu

  ptw.io.requestor(0) <> icache.io.cpu.ptw
  ptw.io.requestor(1) <> core.io.ptw_tlb

  // the dcache's built-in TLB will be unused, but it still needs some of the
  // status/sret signals for things such as lr/sc
  //ptw.io.requestor(1) <> dcache.io.cpu.ptw
  dcache.io.cpu.ptw.sret := ptw.io.requestor(1).sret
  dcache.io.cpu.ptw.status <> ptw.io.requestor(1).status
  dcache.io.cpu.ptw.invalidate := ptw.io.requestor(1).invalidate

  core.io.host <> io.host
  core.io.imem <> icache.io.cpu
  core.io.dmem <> dc_shim.io.core
  core.io.ptw_dat <> ptw.io.dpath


  val memArb = Module(new UncachedTileLinkIOArbiterThatAppendsArbiterId(params(NTilePorts)))
  memArb.io.in(dcachePortId) <> dcache.io.mem
  memArb.io.in(icachePortId) <> icache.io.mem

  io.tilelink.acquire <> memArb.io.out.acquire
  memArb.io.out.grant <> io.tilelink.grant
  io.tilelink.finish <> memArb.io.out.finish
  dcache.io.mem.probe <> io.tilelink.probe
  io.tilelink.release.valid   := dcache.io.mem.release.valid
  dcache.io.mem.release.ready := io.tilelink.release.ready
  io.tilelink.release.bits := dcache.io.mem.release.bits
  io.tilelink.release.bits.payload.client_xact_id :=  Cat(dcache.io.mem.release.bits.payload.client_xact_id, UInt(dcachePortId, log2Up(params(NTilePorts)))) // Mimic client id extension done by UncachedTileLinkIOArbiter for Acquires from either client)


  // Cache Counters
  val cache_counters = new CacheCounters()
  cache_counters.dc_miss := dcache.io.mem.acquire.fire().toBool
  // TODO track cache writebacks
//     val dcache_wbacks = Counter(dcache.io.mem.releases.fire()) // also check hasdata property
  cache_counters.ic_miss := icache.io.mem.acquire.fire()
  core.io.counters := cache_counters

}

}

