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
   val roccq_full       = Output(Bool())
   val roccq_idx        = Output(UInt(log2Ceil(NUM_ROCC_ENTRIES).W))
   val rob_pnr          = Input(UInt(log2Ceil(NUM_ROB_ROWS).W))
   val rob_tail         = Input(UInt(log2Ceil(NUM_ROB_ROWS).W))

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

   val enq_val      = Wire(Bool())
   val enq_uop      = Wire(new MicroOp)
   enq_val := false.B
   enq_uop := DontCare

   val roccq_val      = Reg(Vec(NUM_ROCC_ENTRIES, Bool()))
   val roccq_op_val   = Reg(Vec(NUM_ROCC_ENTRIES, Bool()))
   val roccq_executed = Reg(Vec(NUM_ROCC_ENTRIES, Bool()))
   val roccq_uop      = Reg(Vec(NUM_ROCC_ENTRIES, new MicroOp()))
   val roccq_rs1      = Reg(Vec(NUM_ROCC_ENTRIES, UInt(xLen.W)))
   val roccq_rs2      = Reg(Vec(NUM_ROCC_ENTRIES, UInt(xLen.W)))

   // The instruction we are waiting for response from
   val roccq_head     = RegInit(0.U(log2Ceil(NUM_ROCC_ENTRIES).W))
   // The instruction we are waiting for PNR to execute
   val roccq_exe_head = RegInit(0.U(log2Ceil(NUM_ROCC_ENTRIES).W))
   val roccq_tail     = RegInit(0.U(log2Ceil(NUM_ROCC_ENTRIES).W))

   io.core.roccq_idx := roccq_tail

   // Decode
   for (w <- 0 until decodeWidth)
   {
      when (io.core.dec_rocc_vals(w) && io.core.dec_uops(w).is_rocc)
      {
         enq_val      := true.B
         enq_uop      := io.core.dec_uops(w)
      }
   }

   when (enq_val)
   {
      roccq_val      (roccq_tail) := true.B
      roccq_op_val   (roccq_tail) := false.B
      roccq_executed (roccq_tail) := false.B
      roccq_uop      (roccq_tail) := enq_uop
      roccq_tail                  := WrapInc(roccq_tail, NUM_ROCC_ENTRIES)
   }

   // Issue
   when (io.req.valid)
   {
      val roccq_idx = io.req.bits.uop.roccq_idx
      assert(io.req.bits.uop.rob_idx === roccq_uop(roccq_idx).rob_idx,
         "Mismatch between RoCCUnit request and RoCC execute head")
      assert(roccq_val(roccq_idx),
         "Trying to execute rocc inst without the instruction bits")

      roccq_op_val   (roccq_idx) := true.B
      roccq_rs1      (roccq_idx) := io.req.bits.rs1_data
      roccq_rs2      (roccq_idx) := io.req.bits.rs2_data
   }

   // Execute
   val head_rob_idx = roccq_uop(roccq_exe_head).rob_idx
   when (roccq_val(roccq_exe_head) &&
      (IsOlder(head_rob_idx >> log2Ceil(decodeWidth).U, io.core.rob_pnr, io.core.rob_tail)))
   {
      io.core.rocc.cmd.valid       := true.B
      io.core.rocc.cmd.bits.inst   := roccq_uop(roccq_exe_head).inst.asTypeOf(new RoCCInstruction)
      io.core.rocc.cmd.bits.rs1    := roccq_rs1(roccq_exe_head)
      io.core.rocc.cmd.bits.rs2    := roccq_rs2(roccq_exe_head)
      io.core.rocc.cmd.bits.status := io.status
      roccq_executed(roccq_exe_head) := true.B
      roccq_exe_head               := WrapInc(roccq_exe_head, NUM_ROCC_ENTRIES)
   }

   // Handle responses
   when (roccq_head =/= roccq_exe_head && roccq_val(roccq_head))
   {
      val resp_rcvd = io.core.rocc.resp.valid
      when (roccq_uop(roccq_head).dst_rtype === RT_X || resp_rcvd)
      {
         assert(!resp_rcvd || io.core.rocc.resp.bits.rd === roccq_uop(roccq_head).ldst,
            "RoCC response destination register does not match expected")
         assert(!(resp_rcvd && !roccq_executed(roccq_head)),
            "Received a response for a RoCC instruction we haven't executed")
         io.resp.valid             := resp_rcvd
         io.resp.bits.uop          := roccq_uop(roccq_head)
         io.resp.bits.data         := io.core.rocc.resp.bits.data
         roccq_head                := WrapInc(roccq_head, NUM_ROCC_ENTRIES)
      }
   }

   io.core.roccq_full := WrapInc(roccq_tail, NUM_ROCC_ENTRIES) === roccq_head

   //--------------------------
   // Branches
   for (i <- 0 until NUM_ROCC_ENTRIES)
   {
      when (roccq_val(i) && IsKilledByBranch(io.brinfo, roccq_uop(i)))
      {
         assert(!roccq_executed(i),
            "We executed a RoCC instruction that was killed by branch!")
         roccq_val(i)      := false.B
         roccq_op_val(i)   := false.B
         roccq_executed(i) := false.B
      }
   }
   when (io.brinfo.valid && io.brinfo.mispredict && !io.exception)
   {
      roccq_tail := io.brinfo.roccq_idx
   }


   //--------------------------
   // Exception / Reset

   when (io.exception || reset.toBool)
   {
      roccq_tail     := 0.U
      roccq_head     := 0.U
      roccq_exe_head := 0.U
      for (i <- 0 until NUM_ROCC_ENTRIES)
      {
         roccq_val(i)      := false.B
         roccq_op_val(i)   := false.B
         roccq_executed(i) := false.B
      }
   }
}

