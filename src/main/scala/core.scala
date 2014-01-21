//**************************************************************************
// RISC-V Processor Core
//--------------------------------------------------------------------------

package BOOM
{

import Chisel._
import Node._
import Common._
//import uncore.constants.MemoryOpConstants._
//import Util._ 
 
class CoreIo(implicit conf: BOOMConfiguration) extends Bundle 
{
  val host = new HTIFIO(conf.tl.ln.nClients)
  val dmem = new DCMemPortIo()(conf.dcache)
  val imem = new CPUFrontendIO()(conf.icache)
//  val dmem = new HellaCacheIO()(conf.dcache)
  val ptw = new DatapathPTWIO().flip
//  val rocc = new RoCCInterface().flip
}
 
class Core(implicit conf: BOOMConfiguration) extends Module
{
  val io = new CoreIo()
  
  val dpath  = Module(new DatPath())
  
  dpath.io.imem <> io.imem
  dpath.io.dmem <> io.dmem
  
  dpath.io.host <> io.host

  dpath.io.ptw <> io.ptw 
}
 
}
