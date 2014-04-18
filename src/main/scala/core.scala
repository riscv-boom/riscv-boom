//**************************************************************************
// RISC-V Processor Core
//--------------------------------------------------------------------------

package BOOM
{

import Chisel._
import Node._
import uncore.HTIFIO
import rocket.DatapathPTWIO
 
class CoreIo(implicit conf: BOOMConfiguration) extends Bundle 
{
  val host = new HTIFIO(conf.rc.tl.ln.nClients)
  val dmem = new DCMemPortIo()(conf.rc.dcache)
  val imem = new CPUFrontendIO()(conf.rc.icache)
  val ptw  = new DatapathPTWIO()(conf.rc.as).flip
  val counters = new CacheCounters().asInput
}
 
class Core(implicit conf: BOOMConfiguration) extends Module
{
  val io = new CoreIo()
  
  val dpath  = Module(new DatPath())
  
  dpath.io.imem <> io.imem
  dpath.io.dmem <> io.dmem
  
  dpath.io.host <> io.host

  dpath.io.ptw <> io.ptw 

  dpath.io.counters <> io.counters
}
 
}
