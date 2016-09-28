//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Two-bit Counter Table
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2016 Sept 27
//
// Provide a two-bit counter table for use by branch predictors. Should only be
// updated during Commit.
//
//    P | H
//    1 | 1 strongly taken
//    1 | 0 weakly taken
//    0 | 1 weakly not-taken
//    0 | 0 strongly not-taken
//
// To reduce port access requirements, the two-bit state machine is not actually
// a counter - a misprediction when in the weak state pushes the counter into
// the strong state for the correct prediction.
//
//    Example: (00 -> 01 -> 11) for a string of taken branches.
//
// P-bits
//    Read: every cycle for a prediction.
//    Write: on a branch misprediction (with the h-bit value).
//
// H-bits
//    Read: on a branch misprediction.
//    Write: on a branch resolution (with the direction of the branch).
//
//
// TODO:
//    - Combine the DualPorted and Banked GShare implementations into a single,
//       parameterized Module.
//    - Support having a smaller h-table (map 1 h-bit entry to multiple p-bit entries).
//    - Don't read the p-table SRAM if stalled (need extra state to store data
//       while stalled)..


package boom

import Chisel._

class UpdateEntry(fetch_width: Int, index_sz: Int) extends Bundle
{
   val was_mispredicted = Bool()
   val index            = UInt(width = index_sz)
   val executed         = Vec(fetch_width, Bool())
   val takens           = Vec(fetch_width, Bool())

   override def cloneType: this.type = new UpdateEntry(fetch_width, index_sz).asInstanceOf[this.type]
}

class TwobcCounterTable(fetch_width: Int,
                        num_entries: Int,
                        dualported: Boolean = false
   ) extends Module
{
   private val index_sz = log2Up(num_entries)
   val io = new Bundle
   {
      // send read addr on cycle 0, get data out on cycle 2.
      val s0_r_idx = UInt(INPUT, width = index_sz)
      val s2_r_out = UInt(OUTPUT, width = fetch_width)

      val update   = Valid(new UpdateEntry(fetch_width, index_sz)).flip
   }

   println ("\t\tBuilding (" + (num_entries * fetch_width * 2/8/1024) +
      " kB) 2-bit counter table for (" +
      fetch_width + "-wide fetch) and " + num_entries + " entries.")

   //------------------------------------------------------------
   // prediction bits
   // hysteresis bits

   val p_table = SeqMem(num_entries, Vec(fetch_width, Bool()))
   val h_table = SeqMem(num_entries, Vec(fetch_width, Bool()))

   require(dualported == false) // TODO unsupported

   //------------------------------------------------------------
   // buffer writes to the h-table as required



   class BrTableUpdate extends Bundle
   {
      val index      = UInt(width = index_sz)
      val executed   = UInt(width = fetch_width) // which words in the fetch packet does the update correspond to?
      val new_value  = UInt(width = fetch_width)

      override def cloneType: this.type = new BrTableUpdate().asInstanceOf[this.type]
   }

   // write queues
   val pwq = Module(new Queue(new BrTableUpdate, entries=2))
   val hwq = Module(new Queue(new UpdateEntry(fetch_width, index_sz), entries=4))

   //------------------------------------------------------------
   // p-table
   // Read table every cycle for a prediction.
   // Write table only if a misprediction occurs.

   val p_addr = Wire(UInt())
   val p_out = Reg(UInt())

   pwq.io.deq.ready := Bool(true)
   val p_wmask = pwq.io.deq.bits.executed.toBools

   when (pwq.io.deq.valid)
   {
      val waddr = pwq.io.deq.bits.index
      val wdata = Vec(pwq.io.deq.bits.new_value.toBools)
      p_table.write(waddr, wdata, p_wmask)
   }

   p_addr := io.s0_r_idx
   p_out := p_table.read(p_addr).toBits
   io.s2_r_out := p_out

   //------------------------------------------------------------
   // h-table
   // Write table for every branch resolution (we can buffer these up).
   // Read table immediately to update the p-table (only if a mispredict occurred).

   hwq.io.enq <> io.update

   val h_ren = io.update.valid && io.update.bits.was_mispredicted
   hwq.io.deq.ready := !h_ren
   when (!h_ren && hwq.io.deq.valid)
   {
      val waddr = hwq.io.deq.bits.index
      val wmask = hwq.io.deq.bits.executed
      val wdata = hwq.io.deq.bits.takens
      h_table.write(waddr, wdata, wmask)
   }

   val h_raddr = io.update.bits.index
   pwq.io.enq.valid          := RegNext(h_ren)
   pwq.io.enq.bits.index     := RegNext(h_raddr)
   pwq.io.enq.bits.executed  := RegNext(io.update.bits.executed.toBits)
   pwq.io.enq.bits.new_value := h_table.read(h_raddr, h_ren).toBits

   //------------------------------------------------------------

}

