//**************************************************************************
// RISC-V Processor Core
//--------------------------------------------------------------------------

package BOOM
{

import Chisel._
import Node._
import rocket._
import uncore.HTIFIO

abstract class BOOMCoreBundle extends Bundle with BOOMCoreParameters

class CoreIo() extends Bundle
{
   val host = new HTIFIO
   val dmem = new DCMemPortIo
   val imem = new rocket.CPUFrontendIO
   val ptw_dat  = new DatapathPTWIO().flip
   val ptw_tlb  = new TLBPTWIO()
   val rocc     = new rocket.RoCCInterface().flip
   val counters = new CacheCounters().asInput
}

class Core() extends Module
{
   val io = new CoreIo()

   val dpath  = Module(new DatPath())

   dpath.io.imem <> io.imem
   dpath.io.dmem <> io.dmem

   dpath.io.host <> io.host

   dpath.io.ptw_dat <> io.ptw_dat
   dpath.io.ptw_tlb <> io.ptw_tlb

   dpath.io.counters <> io.counters
}

}
