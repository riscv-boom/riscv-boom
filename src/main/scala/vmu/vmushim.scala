//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Data Cache Shim/Wrapper to the Hella-Cache
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio, Hankun Zhao
// 2018 Sept 5

// We need to track inflight loads that may have been misspeculated, and filter
// them out before they can be returned to the pipeline (we do not want to hold
// up pipeline resources like LD/ST entries on them).
//
// We also need to mandate loads return in order here, since the rest of the
// vector pipeline assumes elements are committed in order
//
// Contract:
//    everything put in here will be executed by memory
//    branch/kill signals will filter resp_val signals, but otherwise continue on

package boom.vmu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import boom.common._
import boom.exu.BrResolutionInfo
import boom.lsu.{LoadReqSlotIo, LoadReqSlot}
import boom.util.WrapInc

class VMUReq(implicit p: Parameters) extends BoomBundle()(p)
{
   val addr    = UInt(width = coreMaxAddrBits.W)
   val uop     = new MicroOp()
   val data    = Bits(width = vecStripLen.W)
   val mask    = Bits(width = (vecStripLen/8).W)
}

class VMUResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val data    = Bits(width = vecStripLen.W)
   val uop     = new MicroOp
}

class VMUShimIO(implicit p: Parameters) extends BoomBundle()(p)
{
   val req      = (new DecoupledIO(new VMUReq))
   val resp     = (new ValidIO(new VMUResp)).flip

   val brinfo   = new BrResolutionInfo().asOutput
   val flush_pipe = Output(Bool())
}
class VMUShim(implicit p: Parameters) extends BoomModule()(p)
      with freechips.rocketchip.rocket.constants.MemoryOpConstants
{

   val io = IO(new Bundle
      {
         val core = new VMUShimIO()
         val vmu = (new BoomVecMemIO()).flip
      })


   val load_ack = io.vmu.memresp_val && !io.vmu.memresp_store

   // Inflight Load Queue
   val ilq_vals = Vec.fill(numVMUEntries) {Reg(Bool(), init=false.B)}
   val ilq_uops = Vec.fill(numVMUEntries) {Reg(new MicroOp())}

   val ilq_head = Reg(UInt(width=log2Up(numVMUEntries).W), init=0.U)
   val ilq_tail = Reg(UInt(width=log2Up(numVMUEntries).W), init=0.U)
   val full = Reg(Bool(), init=false.B)

   val enq_val = io.core.req.valid && io.core.req.bits.uop.is_load



   io.vmu.memreq_val := false.B
   when (enq_val) {
      assert(!full)
      val next_tail = WrapInc(ilq_tail, numVMUEntries)
      when (next_tail === ilq_head) { full := true.B }

      ilq_uops(ilq_tail) := io.core.req.bits.uop
      ilq_vals(ilq_tail) := true.B


      ilq_tail := next_tail

   }
   io.vmu.memreq_val    := io.core.req.valid
   io.vmu.memreq_addr   := io.core.req.bits.addr
   io.vmu.memreq_wdata  := io.core.req.bits.data
   io.vmu.memreq_mask   := io.core.req.bits.mask
   io.vmu.memreq_uop    := io.core.req.bits.uop
   io.vmu.memreq_tag    := Cat(io.core.req.bits.uop.is_store, ilq_tail)

}

