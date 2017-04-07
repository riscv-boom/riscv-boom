//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Transformable SeqReadMem
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2017 Apr 2
//
// Provide an abstract sequential-read memory that can be transformed from a
// tall, skinny aspect ratio to a more rectangular shape.

package boom

import Chisel._


// Provide the "logical" sizes and we will map
// the logical SeqMem into a physical, realizable SeqMem
// that is in a square aspect ratio (rounded to a pow2 for the depth).
// Only supports a single read/write port.
class SeqMem1rwTransformable (
   l_depth: Int,
   l_width: Int
   ) extends Module
{
   val p_depth = 1 << log2Up(scala.math.sqrt(l_depth*l_width).toInt)
   val p_width = l_depth*l_width/p_depth

   require (l_depth*l_width == p_depth*p_width)
   require (p_depth > 0)
   require (p_width > 0)
   require (p_width % l_width == 0)

   val l_idx_sz = log2Up(l_depth)
   val p_idx_sz = log2Up(p_depth)
   val l_off_sz = log2Up(l_width)
   val p_off_sz = log2Up(p_width/l_width)

   val io = new Bundle
   {
      val wen   = Bool(INPUT)
      val waddr = UInt(INPUT, width = l_idx_sz)
      val wmask = UInt(INPUT, width = l_width) 
      val wdata = UInt(INPUT, width = l_width)

      val ren   = Bool(INPUT)                   // valid cycle s0
      val raddr = UInt(INPUT, width = l_idx_sz) // input cycle s0
      val rout  = UInt(OUTPUT, width = l_width) // returned cycle s1
   }

   val smem = SeqMem(p_depth, Vec(p_width, Bool()))


   private def getIdx(addr:UInt) = 
      addr >> p_off_sz
      
   // must compute offset from address but then factor in the l_width.
   private def getOffset(addr:UInt) = 
      addr(p_off_sz-1,0) << l_off_sz
      
   assert (!(io.wen && io.ren), "[SMUtil] writer and reader fighting over the single port.")
   when (io.wen && !io.ren)
   {
      val waddr = getIdx(io.waddr)
      val wdata = (io.wdata << getOffset(io.waddr))(p_width-1, 0)
      val wmask = (io.wmask << getOffset(io.waddr))(p_width-1, 0)
      smem.write(waddr, Vec(wdata.toBools), wmask.toBools)
   }

   // read
   val p_raddr = getIdx(io.raddr)
   val r_p_raddr = RegEnable(p_raddr, io.ren)
   // returned cycle s2
   val s1_rrow = smem.read(p_raddr, io.ren).toBits
   io.rout := (s1_rrow >> (getOffset(r_p_raddr)))(l_width-1, 0)
}

