//******************************************************************************
// Copyright (c) 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Floating Point Datapath Pipeline
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Hankun Zhao

// The vector issue window, regfile, and arithmetic units are all handled here.

package boom.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket
import freechips.rocketchip.tile
import boom.exu.FUConstants._
import boom.common._

class VecPipeline(implicit p: Parameters) extends BoomModule()(p) with tile.HasFPUParameters
{
  val vecIssueParams = issueParams.find(_.iqType == IQT_VEC.litValue).get
  val num_ll_ports = 2 // hard-wired, used by i2v and f2v ports
  val num_wakeup_ports = vecIssueParams.issueWidth + num_ll_ports
  val vec_preg_sz = log2Up(numVecPhysRegs)

  val io = new Bundle
  {
    val brinfo         = Input(new BrResolutionInfo())
    val flush_pipeline = Input(Bool())
    // TODO: Add inputs from rocket CSRFile

    val dis_valids     = Input(Vec(DISPATCH_WIDTH, Bool()))
    val dis_uops       = Input(Vec(DISPATCH_WIDTH, new MicroOp()))
    val dis_readys     = Output(Vec(DISPATCH_WIDTH, Bool()))

    val fromint        = Flipped(Decoupled(new FuncUnitReq(fLen+1))) // from integer RF
    val fromfp         = Flipped(Decoupled(new FuncUnitReq(fLen+1))) // from fp RF
    val toint          = Decoupled(new ExeUnitResp(xLen))
    val wakeups        = Vec(num_wakeup_ports, Valid(new ExeUnitResp(128)))
    val wb_valids      = Input(Vec(num_wakeup_ports, Bool()))
    val vb_pdsts       = Input(Vec(num_wakeup_ports, UInt(width=vec_preg_sz.W)))
  }

   //**********************************
   // construct all of the modules
   // TODO: Design and build VectorExecutionUnit
   // TODO: Design and build VectorIssueUnit
   // TODO: Design and build VectorRegisterFile
   // Initial Plan pSIMD vectors
   //   - Registers are fixed length, 128 bits
   //   - Registers are 64bit fp only
   //   - For now, leave i2v and f2v ports unconnected, no way to load vectors
   //   - Later - add memory execution unit
   //   - Later - add polymorphism, in decode microops should determine where operands come from

} 

