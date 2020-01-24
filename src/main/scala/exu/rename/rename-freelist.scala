//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Rename FreeList
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._
import boom.common._
import boom.util._
import freechips.rocketchip.config.Parameters

class RenameFreeList(
  val plWidth: Int,
  val numPregs: Int,
  val float: Boolean)
  (implicit p: Parameters) extends BoomModule
{
  private val pregSz = log2Ceil(numPregs)
  private val n = numPregs

  val io = IO(new BoomBundle()(p) {
    // Physical register requests.
    val reqs           = Input(Vec(plWidth, Bool()))
    val alloc_pregs    = Output(Vec(plWidth, Valid(UInt(pregSz.W))))

    // Pregs returned by the ROB.
    val dealloc_pregs  = Input(Vec(plWidth, Valid(UInt(pregSz.W))))

    // Branch info for starting new allocation lists.
    val ren_br_tags    = Input(Vec(plWidth, Valid(UInt(brTagSz.W))))

    // Mispredict info for recovering speculatively allocated registers.
    val brinfo         = Input(new BrResolutionInfo)

    // Used to check for physical register leaks.
    val pipeline_empty = Input(Bool())
  })
  // The free list register array and its branch allocation lists.
  val free_list = RegInit(UInt(numPregs.W), ~(1.U(numPregs.W)))
  val br_alloc_lists = Reg(Vec(maxBrCount, UInt(numPregs.W)))

  // Select pregs from the free list.
  val sels = SelectFirstN(free_list, plWidth)
  val sel_fire  = Wire(Vec(plWidth, Bool()))

  // Allocations seen by branches in each pipeline slot.
  val allocs = io.alloc_pregs map (a => UIntToOH(a.bits))
  val alloc_masks = (allocs zip io.reqs).scanRight(0.U(n.W)) { case ((a,r),m) => m | a & Fill(n,r) }

  // Masks that modify the freelist array.
  val sel_mask = (sels zip sel_fire) map { case (s,f) => s & Fill(n,f) } reduce(_|_)
  val br_deallocs = br_alloc_lists(io.brinfo.tag) & Fill(n, io.brinfo.mispredict)
  val dealloc_mask = io.dealloc_pregs.map(d => UIntToOH(d.bits)(numPregs-1,0) & Fill(n,d.valid)).reduce(_|_) | br_deallocs

  val br_slots = VecInit(io.ren_br_tags.map(tag => tag.valid)).asUInt
  // Create branch allocation lists.
  for (i <- 0 until maxBrCount) {
    val list_req = VecInit(io.ren_br_tags.map(tag => UIntToOH(tag.bits)(i))).asUInt & br_slots
    val new_list = list_req.orR
    br_alloc_lists(i) := Mux(new_list, Mux1H(list_req, alloc_masks.slice(1, plWidth+1)),
                                       br_alloc_lists(i) & ~br_deallocs | alloc_masks(0))
  }

  // Update the free list.
  free_list := (free_list & ~sel_mask | dealloc_mask) & ~(1.U(numPregs.W))

  // Pipeline logic | hookup outputs.
  for (w <- 0 until plWidth) {
    val can_sel = sels(w).orR
    val r_valid = RegInit(false.B)
    val r_sel   = RegEnable(OHToUInt(sels(w)), sel_fire(w))

    r_valid := r_valid && !io.reqs(w) || can_sel
    sel_fire(w) := (!r_valid || io.reqs(w)) && can_sel

    io.alloc_pregs(w).bits  := r_sel
    io.alloc_pregs(w).valid := r_valid
  }

  // Get the complete freelist as a bit vector (include pipelined selections).
  val debug_freelist = free_list | io.alloc_pregs.map(p => UIntToOH(p.bits) & Fill(n,p.valid)).reduce(_|_)

  val numLregs = if(float) 32 else 31
  assert (!(debug_freelist & dealloc_mask).orR, "[freelist] Returning a free physical register.")
  assert (!io.pipeline_empty || PopCount(debug_freelist) >= (numPregs - numLregs - 1).U,
    "[freelist] Leaking physical registers.")
}
