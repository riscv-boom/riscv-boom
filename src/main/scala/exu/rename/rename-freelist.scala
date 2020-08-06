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
  val allocWidth: Int,
  val deallocWidth: Int,
  val numPregs: Int,
  val numLregs: Int,
  val isImm: Boolean
)
  (implicit p: Parameters) extends BoomModule
{
  private val pregSz = log2Ceil(numPregs)
  private val n = numPregs

  val io = IO(new BoomBundle()(p) {
    // Physical register requests.
    val reqs          = Input(Vec(allocWidth, Bool()))
    val alloc_pregs   = Output(Vec(allocWidth, Valid(UInt(pregSz.W))))

    // Pregs returned by the ROB.
    val despec      = Input(Vec(deallocWidth, Valid(UInt(pregSz.W))))
    val dealloc     = Input(Vec(deallocWidth, Valid(UInt(pregSz.W))))

    // Branch info for starting new allocation lists.
    val ren_br_tags   = Input(Vec(allocWidth, Valid(UInt(brTagSz.W))))

    // Mispredict info for recovering speculatively allocated registers.
    val brupdate        = Input(new BrUpdateInfo)

    val rollback      = Input(Bool())
  })
  // The free list register array and its branch allocation lists.
  val free_list = RegInit(UInt(numPregs.W),
    if (numLregs == 0) ~(0.U((numPregs-numLregs).W))
    else Cat(~(0.U((numPregs-numLregs).W)), 0.U(numLregs.W))
  )
  val spec_alloc_list = RegInit(0.U(numPregs.W))
  val br_alloc_lists = Reg(Vec(maxBrCount, UInt(numPregs.W)))

  // Select pregs from the free list.
  val sels = SelectFirstN(free_list, allocWidth)
  val sel_fire  = Wire(Vec(allocWidth, Bool()))

  // Allocations seen by branches in each pipeline slot.
  val allocs = io.alloc_pregs map (a => UIntToOH(a.bits)(n-1,0))
  val alloc_masks = (allocs zip io.reqs).scanRight(0.U(n.W)) { case ((a,r),m) => m | a & Fill(n,r) }

  // Masks that modify the freelist array.
  val sel_mask = (sels zip sel_fire) map { case (s,f) => s & Fill(n,f) } reduce(_|_)

  val br_deallocs = br_alloc_lists(io.brupdate.b2.uop.br_tag) & Fill(n, io.brupdate.b2.mispredict)
  val com_deallocs = RegNext(io.dealloc).map(d => UIntToOH(d.bits)(numPregs-1,0) & Fill(n,d.valid)).reduce(_|_)
  val rollback_deallocs = spec_alloc_list & Fill(n, io.rollback)
  val dealloc_mask = com_deallocs | br_deallocs | rollback_deallocs
 
  val com_despec = io.despec.map(d => UIntToOH(d.bits)(numPregs-1,0) & Fill(n,d.valid)).reduce(_|_)

  // Update branch snapshots
  for (i <- 0 until maxBrCount) {
    val updated_br_alloc_list = if (isImm) {
      // Immediates clear the busy table when they read, potentially before older branches resolve.
      // Thus the branch alloc lists must be updated as well
      br_alloc_lists(i) & ~br_deallocs & ~com_deallocs | alloc_masks(0)
    } else {
      br_alloc_lists(i) & ~br_deallocs | alloc_masks(0)
    }
    br_alloc_lists(i) := updated_br_alloc_list
  }
  if (enableSuperscalarSnapshots) {
    val br_slots = VecInit(io.ren_br_tags.map(tag => tag.valid)).asUInt
    // Create branch allocation lists.
    for (i <- 0 until maxBrCount) {
      val list_req = VecInit(io.ren_br_tags.map(tag => UIntToOH(tag.bits)(i))).asUInt & br_slots
      val new_list = list_req.orR
      when (new_list) {
        br_alloc_lists(i) := Mux1H(list_req, alloc_masks.slice(1, allocWidth+1))
      }
    }
  } else {
    assert(PopCount(io.ren_br_tags.map(_.valid)) <= 1.U)
    val do_br_snapshot = io.ren_br_tags.map(_.valid).reduce(_||_)
    val br_snapshot_tag   = Mux1H(io.ren_br_tags.map(_.valid), io.ren_br_tags.map(_.bits))
    val br_snapshot_list  = Mux1H(io.ren_br_tags.map(_.valid), alloc_masks.slice(1, allocWidth+1))
    when (do_br_snapshot) {
      br_alloc_lists(br_snapshot_tag) := br_snapshot_list
    }
  }


  spec_alloc_list := (spec_alloc_list | alloc_masks(0)) & ~dealloc_mask & ~com_despec

  // Update the free list.
  free_list     := (free_list & ~sel_mask) | dealloc_mask

  // Pipeline logic | hookup outputs.
  for (w <- 0 until allocWidth) {
    val can_sel = sels(w).orR
    val r_valid = RegInit(false.B)
    val r_sel   = RegEnable(OHToUInt(sels(w)), sel_fire(w))

    r_valid := r_valid && !io.reqs(w) || can_sel
    sel_fire(w) := (!r_valid || io.reqs(w)) && can_sel

    io.alloc_pregs(w).bits  := r_sel
    io.alloc_pregs(w).valid := r_valid
  }

  val debug_freelist = free_list | io.alloc_pregs.map(p => UIntToOH(p.bits)(n-1,0) & Fill(n,p.valid)).reduce(_|_)

  if (!isImm)
    assert (!(debug_freelist & dealloc_mask).orR, "[freelist] Returning a free physical register.")
  assert (!RegNext(io.rollback) || PopCount(debug_freelist) === (numPregs - numLregs).U,
    "[freelist] Leaking physical registers.")
}
