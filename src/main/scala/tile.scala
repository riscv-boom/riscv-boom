//**************************************************************************
// RISCV Processor Tile
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2012 Feb 5
//
// Describes a simple RISC-V Out-of-Order processor

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

 
class BOOMTile(resetSignal: Bool = null) extends Tile(resetSignal) {

  val dcachePortId = 0
  val icachePortId = 1

  val core = Module(new Core, { case CoreName => "BOOM"})
  val icache = Module(new Frontend, { case CacheName => "L1I"; case CoreName => "BOOM" })
  val dcache = Module(new DCacheWrapper, { case CacheName => "L1D" })
  val ptw = Module(new PTW(params(NPTWPorts)))

// TODO add this back, but need to understand what "dmem" means (core.io.dmem is different from hellacacherequest)
//  val dcachePorts = 2 //+ !confIn.rocc.isEmpty // Number of ports into D$: 1 from core, 1 from PTW, maybe 1 from RoCC
//  val dcacheArb = Module(new HellaCacheArbiter(dcachePorts))
//  dcacheArb.io.requestor(0) <> ptw.io.mem
//  dcacheArb.io.requestor(1) <> core.io.dmem
//  dcache.io.core <> dcacheArb.io.mem

  ptw.io.requestor(0) <> icache.io.cpu.ptw
  ptw.io.requestor(1) <> dcache.io.core.ptw 

//  if (!conf.rocc.isEmpty) {
//    val dcIF = Module(new SimpleHellaCacheIF)
//    val rocc = Module((conf.rocc.get)(conf))
//    dcIF.io.requestor <> rocc.io.mem
//    core.io.rocc <> rocc.io
//    dcacheArb.io.requestor(2) <> dcIF.io.cache
//  }

  core.io.host <> io.host
  core.io.imem <> icache.io.cpu
  core.io.dmem <> dcache.io.core 
  core.io.ptw <> ptw.io.dpath

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

