package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}

case class BHTParameters(
  enabled: Boolean = true,
  num_entries: Int = 128)

case object BHTKey extends Field[BHTParameters]

class BHTBrPredictor(
    fetch_width: Int,
    num_entries: Int = 128)(implicit p: Parameters)
      extends BrPredictor(fetch_width, 1)(p)
{
  require (coreInstBytes == 4)

  // Predictor state
  val counters = Reg(init =
    Vec(Seq.fill(num_entries) {
      Vec(Seq.fill(fetch_width) { UInt("b00", width = 2) })
    }))

  val stall = !io.resp.ready

  val s1_pc = io.req_pc
  val s1_r_idx = s1_pc >> UInt(log2Ceil(fetch_width * coreInstBytes))
  val s2_count = RegEnable(counters(s1_r_idx), !stall)

  io.resp.valid := !this.disable_bpd
  io.resp.bits.takens := Cat(s2_count.map(count => count(1)).reverse)
  io.resp.bits.info := RegNext(s1_r_idx)

  val commit_s1_en = this.commit.valid
  val commit_s1_idx = this.commit.bits.info.info
  val commit_s1_taken = this.commit.bits.ctrl.taken

  val commit_s2_idx = RegEnable(commit_s1_idx, commit_s1_en)
  val commit_s2_counts = RegEnable(counters(commit_s1_idx), commit_s1_en)
  val commit_s2_taken = RegEnable(commit_s1_taken, commit_s1_en)
  val commit_s2_en = RegNext(commit_s1_en)

  val commit_s2_updates = commit_s2_counts.zip(commit_s2_taken).map {
    case (count, taken) => Mux(taken,
      Mux(count === "b11".U, count, count + 1.U),
      Mux(count === "b00".U, count, count - 1.U))
  }

  when (commit_s2_en) { counters(commit_s2_idx) := commit_s2_updates }
}
