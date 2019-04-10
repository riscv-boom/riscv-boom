//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Jerry Zhao
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// The RoCC shim unit. Similar to the LSU, in that we need to allocate entries
// for instruction bits at decode, and send commands strictly in order.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.{RoCCCoreIO, RoCCInstruction}
import freechips.rocketchip.rocket._
import boom.common._
import boom.util._

class RoCCShimCoreIO(implicit p: Parameters) extends BoomBundle()(p)
{
   // Decode Stage
   val dec_rocc_vals    = Input(Vec(decodeWidth, Bool()))
   val dec_uops         = Input(Vec(decodeWidth, new MicroOp))
   val rxq_full         = Output(Bool())
   val rxq_empty        = Output(Bool())
   val rxq_idx          = Output(UInt(log2Ceil(NUM_RXQ_ENTRIES).W))
   val rob_pnr_idx      = Input(UInt(ROB_ADDR_SZ.W))
   val rob_tail_idx     = Input(UInt(ROB_ADDR_SZ.W))

   val rocc             = Flipped(new RoCCCoreIO)
}

/**
 * IO bundle representing the different signals to interact with the RoCC
 *
  */
class RoCCShimIO(implicit p: Parameters) extends BoomBundle()(p)
{
   val core             = new RoCCShimCoreIO

   val req              = Flipped(new DecoupledIO(new FuncUnitReq(xLen)))
   val resp             = new DecoupledIO(new FuncUnitResp(xLen))
   val brinfo           = Input(new BrResolutionInfo())
   val status           = Input(new MStatus)
   val exception        = Input(Bool())
}

class RoCCShim(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new RoCCShimIO)

   val enq_val      = WireInit(false.B)

   // RoCC execute queue. Wait for PNR, holds operands and inst bits
   val rxq_val       = Reg(Vec(NUM_RXQ_ENTRIES, Bool()))
   val rxq_op_val    = Reg(Vec(NUM_RXQ_ENTRIES, Bool()))
   val rxq_executed  = Reg(Vec(NUM_RXQ_ENTRIES, Bool()))
   val rxq_committed = Reg(Vec(NUM_RXQ_ENTRIES, Bool()))
   val rxq_uop       = Reg(Vec(NUM_RXQ_ENTRIES, new MicroOp()))
   val rxq_rs1       = Reg(Vec(NUM_RXQ_ENTRIES, UInt(xLen.W)))
   val rxq_rs2       = Reg(Vec(NUM_RXQ_ENTRIES, UInt(xLen.W)))

   // The instruction we are waiting for response from
   val rxq_head     = RegInit(0.U(log2Ceil(NUM_RXQ_ENTRIES).W))
   // The instruction we are waiting for clearance to execute
   val rxq_exe_head = RegInit(0.U(log2Ceil(NUM_RXQ_ENTRIES).W))
   // The next instruction we are waiting to "commit" through PNR
   val rxq_com_head = RegInit(0.U(log2Ceil(NUM_RXQ_ENTRIES).W))
   val rxq_tail     = RegInit(0.U(log2Ceil(NUM_RXQ_ENTRIES).W))

   io.core.rxq_idx := rxq_tail

   // Decode
   val rocc_idx = WireInit(0.U)
   val br_mask = WireInit(0.U(MAX_BR_COUNT.W))
   for (w <- 0 until decodeWidth)
   {
      when (io.core.dec_rocc_vals(w) && io.core.dec_uops(w).uopc === uopROCC)
      {
         enq_val      := true.B
         rocc_idx     := w.U

      }
   }

   when (enq_val)
   {
      rxq_val      (rxq_tail) := true.B
      rxq_op_val   (rxq_tail) := false.B
      rxq_executed (rxq_tail) := false.B
      rxq_committed(rxq_tail) := false.B
      rxq_uop      (rxq_tail) := io.core.dec_uops(rocc_idx)
      rxq_tail                  := WrapInc(rxq_tail, NUM_RXQ_ENTRIES)
   }

   // Issue
   when (io.req.valid)
   {
      val rxq_idx = io.req.bits.uop.rxq_idx
      assert(io.req.bits.uop.rob_idx === rxq_uop(rxq_idx).rob_idx,
         "Mismatch between RoCCUnit request and RoCC execute head")
      assert(rxq_val(rxq_idx),
         "Trying to execute rocc inst without the instruction bits")

      rxq_op_val   (rxq_idx)      := true.B
      rxq_uop      (rxq_idx).pdst := io.req.bits.uop.pdst
      rxq_rs1      (rxq_idx)      := io.req.bits.rs1_data
      rxq_rs2      (rxq_idx)      := io.req.bits.rs2_data
   }

   // Commit
   when (rxq_op_val(rxq_com_head) &&
      IsOlder(rxq_uop(rxq_com_head).rob_idx, io.core.rob_pnr_idx, io.core.rob_tail_idx))
   {
      rxq_committed(rxq_com_head) := true.B
      rxq_com_head                  := WrapInc(rxq_com_head, NUM_RXQ_ENTRIES)
   }
   // Execute
   io.core.rocc.cmd.valid := false.B
   when (rxq_op_val(rxq_exe_head) && io.core.rocc.cmd.ready &&
      rxq_committed(rxq_exe_head))
   {
      io.core.rocc.cmd.valid         := true.B
      io.core.rocc.cmd.bits.inst     := rxq_uop(rxq_exe_head).inst.asTypeOf(new RoCCInstruction)
      io.core.rocc.cmd.bits.rs1      := rxq_rs1(rxq_exe_head)
      io.core.rocc.cmd.bits.rs2      := rxq_rs2(rxq_exe_head)
      io.core.rocc.cmd.bits.status   := io.status
      rxq_executed(rxq_exe_head) := true.B
      rxq_exe_head                 := WrapInc(rxq_exe_head, NUM_RXQ_ENTRIES)
   }

   //------------------
   // Handle responses

   // Either we get a response, or the RoCC op expects no response
   val handle_resp = (io.core.rocc.resp.valid || rxq_uop(rxq_head).dst_rtype === RT_X) && io.resp.ready

   io.core.rocc.resp.ready := io.resp.ready
   io.resp.valid           := false.B
   when (rxq_head =/= rxq_exe_head &&
         rxq_val(rxq_head) &&
         handle_resp)
   {
      assert((rxq_uop(rxq_head).dst_rtype === RT_X)
          || io.core.rocc.resp.bits.rd === rxq_uop(rxq_head).ldst,
         "RoCC response destination register does not match expected")
      assert(rxq_executed(rxq_head),
         "Received a response for a RoCC instruction we haven't executed")
      io.resp.valid              := true.B
      io.resp.bits.uop           := rxq_uop(rxq_head)
      io.resp.bits.data          := io.core.rocc.resp.bits.data

      rxq_val      (rxq_head) := false.B
      rxq_op_val   (rxq_head) := false.B
      rxq_executed (rxq_head) := false.B
      rxq_committed(rxq_head) := false.B

      rxq_head                 := WrapInc(rxq_head, NUM_RXQ_ENTRIES)
   }

   io.core.rxq_full  := WrapInc(rxq_tail, NUM_RXQ_ENTRIES) === rxq_head
   io.core.rxq_empty :=  rxq_tail === rxq_head
   //--------------------------
   // Branches
   for (i <- 0 until NUM_RXQ_ENTRIES)
   {
      when (rxq_val(i))
      {
         rxq_uop(i).br_mask := GetNewBrMask(io.brinfo, rxq_uop(i))
         when (IsKilledByBranch(io.brinfo, rxq_uop(i)))
         {
            assert(!rxq_executed(i),
              "We executed a RoCC instruction that was killed by branch!")
            rxq_val(i)      := false.B
            rxq_op_val(i)   := false.B
            rxq_executed(i) := false.B
         }
      }
   }
   when (io.brinfo.valid && io.brinfo.mispredict && !io.exception)
   {
      rxq_tail := io.brinfo.rxq_idx
   }


   //--------------------------
   // Exception / Reset

   when (reset.toBool)
   {
      rxq_tail     := 0.U
      rxq_head     := 0.U
      rxq_exe_head := 0.U
      rxq_com_head := 0.U
      for (i <- 0 until NUM_RXQ_ENTRIES)
      {
         rxq_val(i)       := false.B
         rxq_op_val(i)    := false.B
         rxq_executed(i)  := false.B
         rxq_committed(i) := false.B
      }
   }
     .elsewhen (io.exception)
   {
      rxq_tail := rxq_com_head
      for (i <- 0 until NUM_RXQ_ENTRIES)
      {
         when (!rxq_committed(i))
         {
            rxq_val(i)      := false.B
            rxq_op_val(i)   := false.B
            rxq_executed(i) := false.B
         }
      }
   }
}
