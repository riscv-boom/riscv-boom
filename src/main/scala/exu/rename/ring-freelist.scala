//******************************************************************************
// Copyright (c) 2015 - 2020, The Regents of the University of California (Regents).
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

class RingFreeList(
  val plWidth: Int,
  val numPregs: Int)
  (implicit p: Parameters) extends BoomModule
{
  private val pregSz = log2Ceil(numPregs)
  private val n = numPregs

  val io = IO(new BoomBundle()(p) {
    // Physical register requests.
    val reqs           = Input(Vec(plWidth, Bool()))
    val alloc_pregs    = Output(Vec(plWidth, Valid(UInt(pregSz.W))))

    val com_pdsts      = Input(Vec(plWidth, Valid(UInt(pregSz.W))))
    val stale_pdsts    = Input(Vec(plWidth, Valid(UInt(pregSz.W))))

    // Branch info for starting new allocation lists.
    val ren_br_tags    = Input(Vec(plWidth, Valid(UInt(brTagSz.W))))

    // Mispredict info for recovering speculatively allocated registers.
    val brupdate       = Input(new BrUpdateInfo)
    val flashback      = Input(Bool())

    // Used to check for physical register leaks.
    val debug_freelist = Output(UInt(numPregs.W))
  })
  // The free list register array and its branch allocation lists.
  val free_list        = RegInit(UInt(numPregs.W), ~(1.U(numPregs.W)))
  val commit_free_list = RegInit(UInt(numPregs.W), ~(1.U(numPregs.W)))
  val br_alloc_lists   = Reg(Vec(maxBrCount, UInt(numPregs.W)))

  // Select pregs from the free list.
  val sels = SelectFirstN(free_list, plWidth)
  val sel_fire  = Wire(Vec(plWidth, Bool()))

  // Allocations seen by branches in each pipeline slot.
  val allocs = RegNext(io.alloc_pregs) map (a => UIntToOH(a.bits))
  val alloc_masks = (allocs zip RegNext(io.reqs)).scanRight(0.U(n.W)) { case ((a,r),m) => m | a & Fill(n,r) }

  // Masks that modify the freelist array.
  val sel_mask = (sels zip sel_fire) map { case (s,f) => s & Fill(n,f) } reduce(_|_)
  val br_deallocs = br_alloc_lists(RegNext(io.brupdate.b2.uop.br_tag)) & Fill(n, RegNext(io.brupdate.b2.mispredict))
  val dealloc_mask = io.stale_pdsts.map(p => Mux(p.valid, UIntToOH(p.bits), 0.U)).reduce(_|_) | br_deallocs

  val r_br_tags = RegNext(io.ren_br_tags)
  // Create branch allocation lists.
  for (i <- 0 until maxBrCount) {
    val list_req = VecInit(r_br_tags.map(tag => tag.valid && UIntToOH(tag.bits)(i)))
    br_alloc_lists(i) := Mux(list_req.reduce(_||_), Mux1H(list_req, alloc_masks.slice(1, plWidth+1)),
                                                    br_alloc_lists(i) & ~br_deallocs | alloc_masks(0))
  }

  // Update the free list.
  free_list := (free_list & ~sel_mask | dealloc_mask) & ~(1.U(numPregs.W))
  when (io.flashback) { free_list := commit_free_list }

  // Update the commit free list.
  val com_alloc_mask   = io.com_pdsts  .map(p => Mux(p.valid, UIntToOH(p.bits), 0.U)).reduce(_|_)
  val com_dealloc_mask = io.stale_pdsts.map(p => Mux(p.valid, UIntToOH(p.bits), 0.U)).reduce(_|_)
  commit_free_list    := (commit_free_list & ~com_alloc_mask | com_dealloc_mask) & ~(1.U(numPregs.W))

  // Pipeline logic | hookup outputs.
  for (w <- 0 until plWidth) {
    val can_sel = sels(w).orR
    val r_valid = RegInit(false.B)
    val r_sel   = RegEnable(OHToUInt(sels(w)), sel_fire(w))

    r_valid := r_valid && !io.reqs(w) || can_sel
    sel_fire(w) := (!r_valid || io.reqs(w)) && can_sel

    io.alloc_pregs(w).bits  := r_sel
    io.alloc_pregs(w).valid := r_valid

    when (io.flashback) { r_valid := false.B }
  }

  // Get the complete freelist as a bit vector (include pipelined selections).
  io.debug_freelist := free_list | io.alloc_pregs.map(p => UIntToOH(p.bits) & Fill(n,p.valid)).reduce(_|_)
  assert (!(io.debug_freelist & dealloc_mask).orR, "[freelist] Returning a free physical register.")
}
