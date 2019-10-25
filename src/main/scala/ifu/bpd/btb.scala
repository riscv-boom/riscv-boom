package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix}

class OffsetBTBEntry(implicit p: Parameters) extends BoomBundle()(p)
{
  val extended = Bool()
  val offset   = SInt(offsetBTBSz.W)
}

class OffsetBTBBranchPredictorBank(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  val bim = Module(new BIMBranchPredictorBank)
  bim.io.f0_req := io.f0_req
  bim.io.update := io.update
  io.f1_resp := bim.io.f1_resp
  io.f2_resp := bim.io.f2_resp
  io.f3_resp := RegNext(io.f2_resp)


  val doing_reset = RegInit(true.B)
  val reset_idx   = RegInit(0.U(log2Ceil(nBTBSets).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nBTBSets-1).U) { doing_reset := false.B }

  val extended = Reg(Vec(nBTBSets, Vec(bankWidth, Bool())))
  val btb      = SyncReadMem(nBTBSets, Vec(bankWidth, SInt(offsetBTBSz.W)))
  val ebtb     = SyncReadMem(nEBTBSets, UInt(vaddrBitsExtended.W))

  val s1_req_rext  = extended(s1_req_idx)
  val s1_req_rbtb  = btb.read (s0_req_idx,  io.f0_req.valid)
  val s1_req_rebtb = ebtb.read(s0_req_idx,  io.f0_req.valid)

  val s2_req_rext  = RegNext(s1_req_rext)
  val s2_req_rbtb  = RegNext(s1_req_rbtb)
  val s2_req_rebtb = RegNext(s1_req_rebtb)

  for (w <- 0 until bankWidth) {
    io.f2_resp(w).predicted_pc.valid := !doing_reset
    io.f2_resp(w).predicted_pc.bits  := Mux(
      s2_req_rext(w),
      s2_req_rebtb,
      (s2_req_pc.asSInt + (w << 1).S + s2_req_rbtb(w)).asUInt)

  }

  val s2_update_cfi_idx = s2_update.bits.cfi_idx.bits

  val max_offset_value = (~(0.U)((offsetBTBSz-1).W)).asSInt
  val min_offset_value = Cat(1.B, (0.U)((offsetBTBSz-1).W)).asSInt
  val new_offset_value = (s2_update.bits.target.asSInt -
    (s2_update.bits.pc + (s2_update.bits.cfi_idx.bits << 1)).asSInt)
  val offset_is_extended = (new_offset_value > max_offset_value ||
                            new_offset_value < min_offset_value)
  val s2_update_wbtb = Wire(new OffsetBTBEntry)
  s2_update_wbtb.extended := offset_is_extended
  s2_update_wbtb.offset   := new_offset_value

  val s2_update_wmask = (UIntToOH(s2_update_cfi_idx) &
    Fill(bankWidth, s2_update.bits.cfi_idx.valid && s2_update.valid))



  btb.write(
    Mux(doing_reset, reset_idx, s2_update_idx),
    VecInit(Seq.fill(bankWidth) {
      Mux(doing_reset, (0.S), s2_update_wbtb.offset)
    }),
    VecInit(Mux(doing_reset, ~(0.U(bankWidth.W)), s2_update_wmask).asBools)
  )
  when (doing_reset) { extended(reset_idx) := (0.U(bankWidth.W)).asBools }

  when (s2_update.valid && s2_update.bits.cfi_idx.valid && offset_is_extended) {
    ebtb.write(s2_update_idx, s2_update.bits.target)
  }
}

