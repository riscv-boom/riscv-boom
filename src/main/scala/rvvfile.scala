//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom

import Chisel._
import cde.Parameters

class RVVCfgFileIO(implicit p: Parameters) extends BoomBundle
{
   val vlen = new Bundle 
   {
      val en    = Bool(INPUT)
      val rdata = Bits(OUTPUT, log2Up(maxVlen))
      val wdata = Bits(INPUT, log2Up(maxVlen))
   }
}

class RVVCfgFile(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new RVVCfgFileIO

   // TODO rename vlen
   val r_vlen = Reg(init=UInt(0, log2Up(maxVlen)))
   // TODO implement cfg
   val r_cfg  = Reg(init=UInt(0, 1))

   val next_vlen = Wire(init = r_vlen)

   next_vlen := Mux(io.vlen.wdata > UInt(maxVlen), 
      UInt(maxVlen),
      io.vlen.wdata)

   when (io.vlen.en)
   {
      printf("Set VLEN to: %d\n", next_vlen)
      r_vlen := next_vlen
//      assert(Bool(false), "SetVlen unsupproted for now.")
   }

   io.vlen.rdata := next_vlen
}
