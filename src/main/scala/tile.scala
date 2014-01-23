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

 
class BoomTile(resetSignal: Bool = null)(confIn: BOOMConfiguration) extends Module(_reset = resetSignal) with ClientCoherenceAgent
{

  // Override some of the external inputs 
  val memPorts = 2 // Number of ports to outer memory system from tile: 1 from I$, 1 from D$
  val dcachePortId = 0
  val icachePortId = 1
  val dcachePorts = 2 //+ !confIn.rocc.isEmpty // Number of ports into D$: 1 from core, 1 from PTW, maybe 1 from RoCC
  val rc = confIn.rc
  implicit val tlConf = rc.tl
  implicit val lnConf = rc.tl.ln
  implicit val icConf = rc.icache.copy(ibytes = (FETCH_WIDTH*4))
  implicit val dcConf = rc.dcache.copy(reqtagbits = rc.dcacheReqTagBits + log2Up(dcachePorts), databits = rc.xprlen)

  implicit val new_rc : rocket.RocketConfiguration = rc.copy(icache = icConf, dcache = dcConf)
  implicit val bc = confIn.copy(rc = new_rc)

  val io = new Bundle {
    val tilelink = new TileLinkIO
    val host = new HTIFIO(lnConf.nClients)
  }

  val core = Module(new Core)
  val icache = Module(new Frontend)
  val dcache = Module(new DCacheWrapper) // wrapper for HellaCache
  val ptw = Module(new rocket.PTW(2)) // 2 ports, 1 from I$, 1 from D$

// TODO add this back, but need to understand what "dmem" means (core.io.dmem is different from hellacacherequest)
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

  val memArb = Module(new UncachedTileLinkIOArbiterThatAppendsArbiterId(memPorts))
  memArb.io.in(dcachePortId) <> dcache.io.mem
  memArb.io.in(icachePortId) <> icache.io.mem

  io.tilelink.acquire <> memArb.io.out.acquire
  memArb.io.out.grant <> io.tilelink.grant
  io.tilelink.grant_ack <> memArb.io.out.grant_ack
  dcache.io.mem.probe <> io.tilelink.probe
  io.tilelink.release.data <> dcache.io.mem.release.data
  io.tilelink.release.meta.valid   := dcache.io.mem.release.meta.valid
  dcache.io.mem.release.meta.ready := io.tilelink.release.meta.ready
  io.tilelink.release.meta.bits := dcache.io.mem.release.meta.bits
  io.tilelink.release.meta.bits.payload.client_xact_id :=  Cat(dcache.io.mem.release.meta.bits.payload.client_xact_id, UInt(dcachePortId, log2Up(memPorts))) // Mimic client id extension done by UncachedTileLinkIOArbiter for Acquires from either client)
}

}

