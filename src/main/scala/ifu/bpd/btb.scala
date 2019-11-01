package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix}

class DenseBTBEntry(implicit p: Parameters) extends BoomBundle()(p)
{
  val tag      = UInt(btbTagSz.W)
  val extended = Bool()
  val offset   = SInt(btbOffsetSz.W)
}

class DenseBTBBranchPredictorBank(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  val bim = Module(new BIMBranchPredictorBank)
  bim.io.f0_req := io.f0_req
  bim.io.update := io.update
  io.f1_resp := bim.io.f1_resp
  io.f2_resp := bim.io.f2_resp
  io.f3_resp := RegNext(io.f2_resp)
  io.f3_meta := bim.io.f3_meta


  val doing_reset = RegInit(true.B)
  val reset_idx   = RegInit(0.U(log2Ceil(btbNSets).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (btbNSets-1).U) { doing_reset := false.B }

  val btb      = Seq.fill(bankWidth) { SyncReadMem(btbNSets, new DenseBTBEntry) }
  val ebtb     = SyncReadMem(ebtbNSets, UInt(vaddrBitsExtended.W))

  val s1_req_rbtb  = VecInit(btb.map(_.read(s0_req_idx,  io.f0_req.valid)))
  val s1_req_rebtb = ebtb.read(s0_req_idx,  io.f0_req.valid)
  val s1_req_tag   = s1_req_idx >> log2Ceil(btbNSets)

  val s2_req_rbtb  = RegNext(s1_req_rbtb)
  val s2_req_rebtb = RegNext(s1_req_rebtb)
  val s2_req_tag   = RegNext(s1_req_tag)

  for (w <- 0 until bankWidth) {
    io.f2_resp(w).predicted_pc.valid := !doing_reset && s2_req.valid && s2_req_tag(btbTagSz-1,0) === s2_req_rbtb(w).tag
    io.f2_resp(w).predicted_pc.bits  := Mux(
      s2_req_rbtb(w).extended,
      s2_req_rebtb,
      (s2_req.bits.pc.asSInt + (w << 1).S + s2_req_rbtb(w).offset).asUInt)

  }

  val s1_update_cfi_idx = s1_update.bits.cfi_idx.bits

  val max_offset_value = (~(0.U)((btbOffsetSz-1).W)).asSInt
  val min_offset_value = Cat(1.B, (0.U)((btbOffsetSz-1).W)).asSInt
  val new_offset_value = (s1_update.bits.target.asSInt -
    (s1_update.bits.pc + (s1_update.bits.cfi_idx.bits << 1)).asSInt)
  val offset_is_extended = (new_offset_value > max_offset_value ||
                            new_offset_value < min_offset_value)
  val s1_update_wbtb = Wire(new DenseBTBEntry)
  s1_update_wbtb.extended := offset_is_extended
  s1_update_wbtb.offset   := new_offset_value
  s1_update_wbtb.tag      := s1_update_idx >> log2Ceil(btbNSets)

  val s1_update_wmask = (UIntToOH(s1_update_cfi_idx) &
    Fill(bankWidth, s1_update.bits.cfi_idx.valid && s1_update.valid && s1_update.bits.cfi_taken))


  for (w <- 0 until bankWidth) {
    when (doing_reset || (s1_update_wmask(w) && s1_update.valid)) {
      btb(w).write(
        Mux(doing_reset, reset_idx, s1_update_idx),
        Mux(doing_reset, (0.U).asTypeOf(new DenseBTBEntry), s1_update_wbtb)
      )
    }
  }

  when (s1_update_wmask =/= 0.U && offset_is_extended) {
    ebtb.write(s1_update_idx, s1_update.bits.target)
  }
}
