//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Fetch Target Queue (FTQ)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Each entry in the FTQ holds the fetch address and branch prediction snapshot state.
//
// TODO:
// * reduce port counts.

package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.core.{DontCare}
import chisel3.experimental.{dontTouch}

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.util.{Str}

import boom.common._
import boom.exu._
import boom.util._

/**
 * FTQ Parameters used in configurations
 *
 * @param nEntries # of entries in the FTQ
 */
case class FtqParameters(
  nEntries: Int = 16
)

/**
 * Bundle to add to the FTQ RAM and to be used as the pass in IO
 */
class FTQBundle(implicit p: Parameters) extends BoomBundle
{
  val fetch_pc = UInt(vaddrBitsExtended.W) // TODO compress out high-order bits
}

/**
 * IO to provide a port for a FunctionalUnit to get the PC of an instruction.
 * And for JALRs, the PC of the next instruction.
 */
class GetPCFromFtqIO(implicit p: Parameters) extends BoomBundle
{
  val ftq_idx  = Input(UInt(log2Ceil(ftqSz).W))
  val fetch_pc = Output(UInt(vaddrBitsExtended.W))
  val com_pc   = Output(UInt(vaddrBitsExtended.W))
  // the next_pc may not be valid (stalled or still being fetched)
  val next_val = Output(Bool())
  val next_pc  = Output(UInt(vaddrBitsExtended.W))
}

/**
 * Queue to store the fetch PC and other relevant branch predictor signals that are inflight in the
 * processor.
 *
 * @param num_entries # of entries in the FTQ
 */
class FetchTargetQueue(num_entries: Int)(implicit p: Parameters) extends BoomModule
  with HasBoomCoreParameters
{
  private val idx_sz = log2Ceil(num_entries)

  val io = IO(new BoomBundle {
    // Enqueue one entry for every fetch cycle.
    val enq = Flipped(Decoupled(new FTQBundle()))
    // Pass to FetchBuffer (newly fetched instructions).
    val enq_idx = Output(UInt(idx_sz.W))
    // ROB tells us the youngest committed ftq_idx to remove from FTQ.
    val deq = Flipped(Valid(UInt(idx_sz.W)))

    // Give PC info to BranchUnit.
    val get_ftq_pc = new GetPCFromFtqIO()

    val redirect = Flipped(Valid(UInt(idx_sz.W)))
    // val brinfo = Input(new BrResolutionInfo)

  })
  val deq_ptr    = RegInit(0.U(idx_sz.W))
  val enq_ptr    = RegInit(1.U(idx_sz.W))

  val full = ((WrapInc(WrapInc(enq_ptr, num_entries), num_entries) === deq_ptr) ||
              (WrapInc(enq_ptr, num_entries) === deq_ptr))


  val ram = SyncReadMem(num_entries, new FTQBundle)

  val do_enq = io.enq.fire()

  when (do_enq) {
    ram.write(enq_ptr, io.enq.bits)
    enq_ptr := WrapInc(enq_ptr, num_entries)
  }

  io.enq.ready := !full
  io.enq_idx := enq_ptr

  when (io.deq.valid) {
    deq_ptr := io.deq.bits
  }

  when (io.redirect.valid) {
    enq_ptr    := WrapInc(io.redirect.bits, num_entries)
  }

  //-------------------------------------------------------------
  // **** Core Read PCs ****
  //-------------------------------------------------------------

  io.get_ftq_pc.fetch_pc := ram.read(io.get_ftq_pc.ftq_idx).fetch_pc
  io.get_ftq_pc.next_pc  := ram.read(WrapInc(io.get_ftq_pc.ftq_idx, num_entries)).fetch_pc
  io.get_ftq_pc.next_val := WrapInc(io.get_ftq_pc.ftq_idx, num_entries) =/= enq_ptr
  io.get_ftq_pc.com_pc   := ram.read(Mux(io.deq.valid, io.deq.bits, deq_ptr)).fetch_pc
}
