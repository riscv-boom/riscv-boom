//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Fetch Target Queue (FTQ)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Each entry in the FTQ holds the fetch address and branch prediction snapshot state.
//
// NOTE: FTQ is a skid-buffer: it unreadies once it hits (FULL-1) entries. It is
// the responsibility of the user to not enqueue while the FTQ is full.


package boom

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters

class FTQBundle(implicit p: Parameters) extends BoomBundle()(p)
{
   val fetch_pc = UInt(width = vaddrBitsExtended.W)
//   val btb_info =
//   val bim_info =
//   val bpd_info =
}

class FetchTargetQueue(num_entries: Int)(implicit p: Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
{
   private val idx_sz = log2Up(num_entries)

   val io = IO(new BoomBundle()(p)
   {
      // enqueue one entry for every fetch cycle
      val enq = Flipped(Decoupled(new FTQBundle()))
      // pass to :sp .
      val enq_idx = UInt(width=idx_sz.W)
      // broadcast youngest 
      val deq = Valid(UInt(width=idx_sz.W))
   })

   val deq_ptr = RegInit(0.asUInt(log2Up(num_entries).W))
   val enq_ptr = Counter(num_entries)
   val maybe_full = RegInit(false.B)
   val ptr_match = enq_ptr.value === deq_ptr
   val empty = ptr_match && !maybe_full
   val full = ptr_match && maybe_full

   val ram = SeqMem(num_entries, new FTQBundle())

   assert(!(full && io.enq.valid), "[ftq] is a skid-buffer. Should never have an enq while full.")

//   private val do_enq = WireInit(io.enq.fire())
//   private val do_deq = WireInit(io.deq.fire())
//
//   when (do_enq) {
//     ram(enq_ptr.value) := io.enq.bits
//     enq_ptr.inc()
//   }
//   when (do_deq) {
//     deq_ptr.inc()
//   }
//   when (do_enq != do_deq) {
//     maybe_full := do_enq
//   }
//
//   io.deq.valid := !empty
//   io.enq.ready := !full
//   io.deq.bits := ram(deq_ptr.value)
//
//   private val ptr_diff = enq_ptr.value - deq_ptr.value
//   if (isPow2(num_entries)) {
//     io.count := Cat(maybe_full && ptr_match, ptr_diff)
//   } else {
//     io.count := Mux(ptr_match,
//                     Mux(maybe_full,
//                       num_entries.asUInt, 0.U),
//                     Mux(deq_ptr.value > enq_ptr.value,
//                       num_entries.asUInt + ptr_diff, ptr_diff))
//   }

}

