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

  val io = IO(new BoomBundle()(p) {
    // Physical register requests.
    val reqs = Input(Vec(plWidth, Bool()))
    val can_allocate = Output(Vec(plWidth, Bool()))
    val alloc_pregs = Output(Vec(plWidth, UInt(pregSz.W)))

    // Pregs returned by the ROB.
    // They come from the "stale" field of committed uops during normal operation,
    // or the pdst field of uops at the tail during exception rollback.
    val rob_uops = Input(Vec(plWidth, new MicroOp))
    val com_valids = Input(Vec(plWidth, Bool()))
    val rbk_valids = Input(Vec(plWidth, Bool()))
    val rollback = Input(Bool())

    // Branch info for starting new allocation lists.
    val ren_br_tags = Input(Vec(plWidth, Valid(UInt(BR_TAG_SZ.W))))

    // Mispredict info for recovering speculatively allocated registers.
    val brinfo = Input(new BrResolutionInfo)

    val debug = new Bundle {
      val rob_empty = Input(Bool())
      val freelist = Output(Bits(numPregs.W))
      val isprlist = Output(Bits(numPregs.W))
    }
  })

  // The free list register array and its branch allocation lists.
  val free_list = RegInit(UInt(numPregs.W), ~(1.U(numPregs.W)))
  val br_alloc_lists = Reg(Vec(MAX_BR_COUNT, UInt(numPregs.W)))

  // Select pregs from the free list.
  val preg_sels = SelectFirstN(free_list, plWidth)
  io.can_allocate := preg_sels.map(_.orR)

  // Allocations seen by branches in each pipeline slot.
  val alloc_masks = (preg_sels zip io.reqs).scanRight(0.U(numPregs.W)) {case ((preg, req), mask) => Mux(req, mask | preg, mask)}

  // Pregs returned by the ROB via commit or rollback.
  val ret_valids = Mux(io.rollback, io.rbk_valids, io.com_valids)
  val ret_pregs = io.rob_uops.map(uop => Mux(io.rollback, uop.pdst, uop.stale_pdst))
  val ret_mask = (ret_pregs zip ret_valids).map {case (preg, valid) => UIntToOH(preg)(numPregs-1,0) & Cat(Fill(numPregs-1, valid.asUInt), 0.U(1.W))}.reduce(_|_)

  val br_slots = VecInit(io.ren_br_tags.map(tag => tag.valid)).asUInt
  // Create branch allocation lists.
  for (i <- 0 until MAX_BR_COUNT) {
    val list_req = VecInit(io.ren_br_tags.map(tag => UIntToOH(tag.bits)(i))).asUInt & br_slots
    val new_list = list_req.orR
    br_alloc_lists(i) := Mux(new_list, Mux1H(list_req, alloc_masks.slice(1, plWidth+1)), br_alloc_lists(i) | alloc_masks(0))
  }

  when (io.brinfo.mispredict) {
    // Recover pregs allocated past a mispredicted branch.
    free_list := free_list | br_alloc_lists(io.brinfo.tag) | ret_mask
  } .otherwise {
    // Update the free list.
    free_list := free_list & ~alloc_masks(0) | ret_mask
  }

  // Encode outputs.
  io.alloc_pregs := VecInit(preg_sels.map(s => OHToUInt(s)))

  io.debug.freelist := free_list
  io.debug.isprlist := 0.U  // TODO track commit free list.

  val numLregs = if(float) 32 else 31
  assert (!io.debug.rob_empty || PopCount(free_list) >= (numPregs - numLregs - 1).U,
    "[freelist] Leaking physical registers.")
}
