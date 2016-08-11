//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Micro-architectural Counters
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom

import Chisel._
import cde.Parameters

class UarchCounters()(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new BoomBundle()(p)
   {
      val inc  = Vec(16, UInt(INPUT, log2Up(1+retireWidth))) 
      val csrs = Vec(nCustomMrwCsrs, UInt(OUTPUT, xLen))
   }

   val reg_counters = io.inc.map(rocket.WideCounter(xLen, _))
   for (i <- 0 until io.csrs.size)
      io.csrs(i) := reg_counters(i).value

   if (p(EnableUarchCounters))
   {
      println("\n   UArch Counters Enabled\n")
   }
   else
   {
      println("\n   UArch Counters Disabled\n")
      reg_counters.foreach(_ := Bool(false))
   }
}
