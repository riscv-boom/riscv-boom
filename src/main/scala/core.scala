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
   val imem = new CPUFrontendIO
   val ptw  = new DatapathPTWIO().flip
   val counters = new CacheCounters().asInput
}
 
class Core() extends Module
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
