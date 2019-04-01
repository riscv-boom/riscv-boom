//******************************************************************************
// Copyright (c) 2018 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Fetch Monitor
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.core.DontCare

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util.{PrintUtil}

/**
 * A class to monitor a vector of MicroOps and their PCs and verify it is a valid sequence.
 *
 * NOTE: I should not be synthesized!
 */
class FetchMonitor(implicit p: Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
{
   val io = IO(new BoomBundle()(p)
   {
      // The stream is valid and accepted by the backend.
      val fire = Input(Bool())
      // The stream of uops being sent to the backend.
      val uops = Input(Vec(decodeWidth, new MicroOp()))
      // Was the pipeline redirected? Clear/reset the fetchbuffer.
      val clear = Input(Bool())
   })

   //-------------------------------------------------------------
   // **** Assertions ****
   //-------------------------------------------------------------

   // Check that within a sequence the PCs are correct.

   // Was the previous uop valid?
   var prev_valid  = WireInit(false.B)
   // What was the previous uop's PC?
   var prev_pc = WireInit(0.U(vaddrBitsExtended.W))
   var prev_cfitype = WireInit(CfiType.NONE)
   // What is the straight-line next PC for the previous uop?
   var prev_npc = WireInit(0.U(vaddrBitsExtended.W))
   // What is the target of the previous PC if a CFI.
   var prev_target = WireInit(0.U(vaddrBitsExtended.W))

   if (DEBUG_PRINTF)
   {
      printf("FetchMonitor:\n")
      printf("    Fetch4:\n")
      for ((uop,i) <- io.uops.zipWithIndex)
      {
         printf("        UOP[%d]: Fire:%c V:%c PC:0x%x\n",
                i.U,
                PrintUtil.ConvertChar(io.fire, 'F'),
                PrintUtil.ConvertChar(uop.valid, 'V'),
                uop.pc)
      }
   }

   for (uop <- io.uops)
   {
      when (prev_valid && uop.valid && io.fire)
      {
         when (prev_cfitype === CfiType.NONE)
         {
            assert (uop.pc === prev_npc, "[fetchmonitor] non-cfi went to bad next-pc.")
         }
         .elsewhen (prev_cfitype === CfiType.BRANCH)
         {
            assert (uop.pc === prev_npc || uop.pc === prev_target,
               "[fetchmonitor] branch went to bad next-pc.")
         }
         .elsewhen (prev_cfitype === CfiType.JAL)
         {
            assert (uop.pc === prev_target, "[fetchmonitor] JAL went to bad target.")
         }
         .otherwise
         {
            // should only be here if a JALR.
            assert (prev_cfitype === CfiType.JALR, "[fetchmonitor CFI type not JALR.")
         }
      }

      prev_valid = uop.valid && io.fire
      prev_pc  = uop.pc
      prev_npc = prev_pc + Mux(uop.is_rvc, 2.U, 4.U)
      val inst = ExpandRVC(uop.inst)
      prev_cfitype = GetCfiType(inst)
      prev_target =
         Mux(prev_cfitype === CfiType.JAL,
            ComputeJALTarget(uop.pc, inst, xLen),
            ComputeBranchTarget(uop.pc, inst, xLen))
   }

   // Check if the enqueue'd PC is a target of the previous valid enqueue'd PC.

   // Was the last uop from the previous decode group valid?
   var last_valid  = RegInit(false.B)
   // What was the previous decode group's last uop's PC?
   var last_pc = RegInit(0.U(vaddrBitsExtended.W))
   var last_cfitype = RegInit(CfiType.NONE)
   // What is the straight-line next PC for the previous uop?
   var last_npc = RegInit(0.U(vaddrBitsExtended.W))
   // What is the target of the previous PC if a CFI.
   var last_target = RegInit(0.U(vaddrBitsExtended.W))

   when (io.fire)
   {
      last_valid := true.B

      val valid_mask = VecInit(io.uops map {u => u.valid}).asUInt
      assert (valid_mask =/= 0.U)
      val end_idx    = (fetchWidth-1).U - PriorityEncoder(Reverse(valid_mask))
      val end_uop    = io.uops(end_idx)
      val end_pc     = end_uop.pc
      val end_compressed = end_uop.inst(1,0) =/= 3.U && usingCompressed.B
      val inst       = ExpandRVC(end_uop.inst)
      last_pc := end_pc
      when (end_compressed) {
         last_npc := end_pc + 2.U
      } .otherwise {
         last_npc := end_pc + 4.U
      }
      last_cfitype := GetCfiType(inst)
      last_target :=
         Mux(GetCfiType(inst) === CfiType.JAL,
            ComputeJALTarget(end_uop.pc, inst, xLen),
            ComputeBranchTarget(end_uop.pc, inst, xLen))

      when (last_valid)
      {
         val first_idx = PriorityEncoder(valid_mask)
         val first_pc  = io.uops(first_idx).pc
         when (last_cfitype === CfiType.NONE)
         {
            when (first_pc =/= last_npc)
            {
               printf("  first_pc: 0x%x last_npc: 0x%x  ",
                  first_pc, last_npc)
            }
            assert (first_pc === last_npc,
               "[fetchmonitor] A non-cfi instruction is followed by the wrong instruction.")
         }
         .elsewhen (last_cfitype === CfiType.JAL)
         {
            // ignore misaligned fetches.
            val f_pc = first_pc(vaddrBitsExtended-1, log2Ceil(coreInstBytes))
            val targ = last_target(vaddrBitsExtended-1, log2Ceil(coreInstBytes))
            assert (f_pc === targ,
               "[fetchmonitor] A jump is followed by the wrong instruction.")
         }
         .elsewhen (last_cfitype === CfiType.BRANCH)
         {
            // ignore misaligned fetches.
            val f_pc = first_pc(vaddrBitsExtended-1, log2Ceil(coreInstBytes))
            val targ = last_target(vaddrBitsExtended-1, log2Ceil(coreInstBytes))
            assert (first_pc === last_npc || f_pc === targ,
               "[fetchmonitor] A branch is followed by the wrong instruction.")
         }
         .otherwise
         {
            // we can't verify JALR instruction stream integrity --  /throws hands up.
            assert (last_cfitype === CfiType.JALR,
               "[fetchmonitor] Should be a JALR if none of the others were valid.")
         }
      }
   }

   when (io.clear)
   {
      last_valid := false.B
   }
}
