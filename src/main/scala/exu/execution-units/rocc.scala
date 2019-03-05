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
import freechips.rocketchip.tile.RoCCCoreIO
import freechips.rocketchip.rocket._
import boom.common._

/**
 * IO bundle representing the different signals to interact with the RoCC
 *
  */
class RoCCShimIO(implicit p: Parameters) extends BoomBundle()(p)
{
   // Decode Stage
   val dec_rocc_vals    = Input(Vec(decodeWidth, Bool()))
   val dec_uops         = Input(Vec(decodeWidth, new MicroOp))
   val roccq_full       = Output(Bool())
   val rob_pnr 


   val req              = Flipped(new DecoupledIO(new FuncUnitReq(xLen)))
   val resp             = new DecoupledIO(new FuncUnitResp(xLen))
   val brinfo           = Input(new BrResolutionInfo())
   val status           = Input(new MStatus)

   val rocc             = Flipped(new RoCCCoreIO)
}

class RoCCShim(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new RoCCShimIO)

   val enq_val      = Wire(Bool())
   val enq_uop      = Wire(new MicroOp)

   val roccq_val      = Reg(Vec(NUM_ROCC_ENTRIES, Bool()))
   val roccq_op_val   = Reg(Vec(NUM_ROCC_ENTRIES, Bool()))
   val roccq_uop      = Reg(Vec(NUM_ROCC_ENTRIES, new MicroOp()))
   val roccq_rs1      = Reg(Vec(NUM_ROCC_ENTRIES, UInt(xLen)))
   val roccq_rs2      = Reg(Vec(NUM_ROCC_ENTRIES, UInt(xLen)))

   val roccq_head     = RegInit(0.U(log2Ceil(NUM_ROCC_ENTRIES).W))
   val roccq_exe_head = RegInit(0.U(log2Ceil(NUM_ROCC_ENTRIES).W))
   val roccq_tail     = RegInit(0.U(log2Ceil(NUM_ROCC_ENTRIES).W))

   // Decode
   for (w <- 0 until decodeWidth)
   {
      when (io.dec_rocc_vals(w) && io.dec_uops(w).is_rocc)
      {
         enq_val      := true.B
         enq_uop      := io.dec_uops(w)
      }
   }

   when (enq_val)
   {
      roccq_val   (roccq_tail) := true.B
      roccq_op_val(roccq_tail) := false.B
      roccq_uop   (roccq_tail) := enq_uop
      roccq_tail               := roccq_tail + 1.U
   }

   // Issue
   when (io.req.valid)
   {
      assert(io.req.bits.uop.rob_idx === roccq_uops(roccq_exe_head).rob_idx,
         "Mismatch between RoCCUnit request and RoCC execute head")
      assert(roccq_val(roccq_exe_head),
         "Trying to execute rocc inst without the instruction bits")

      roccq_op_val   (roccq_exe_head) := true.B
      roccq_rs1      (roccq_exe_head) := io.req.bits.rs1_data
      roccq_rs2      (roccq_exe_head) := io.req.bits.rs2_data
   }

   io.roccq_full := (roccq_tail + 1.U) === roccq_head
}

