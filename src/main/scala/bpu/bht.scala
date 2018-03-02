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
  require (fetch_width == 1)

  // Predictor state: Table of 2-bit counters initialized to 0
  val counters = Reg(
    init = Vec(Seq.fill(num_entries) { UInt("b00", width = 2) }))

  // pause prediction
  val stall = !io.resp.ready

  // index into the table to get the count
  val s1_pc = io.req_pc
  val s1_r_idx = s1_pc >> UInt(log2Ceil(coreInstBytes))
  val s2_count = RegEnable(counters(s1_r_idx), !stall)

  // keep sending predictions as long as not disabled
  io.resp.valid := !this.disable_bpd
  // prediction is the upper bit of two-bit counter
  io.resp.bits.takens := s2_count(1)
  // tell the pipeline to save the index for commit
  io.resp.bits.info := RegNext(s1_r_idx)

  // on commit, get the index and whether the branch was actually taken
  val commit_s1_en = this.commit.valid
  val commit_s1_idx = this.commit.bits.info.info
  val commit_s1_taken = this.commit.bits.ctrl.taken(0)

  // index into table to get previous state
  val commit_s2_idx = RegEnable(commit_s1_idx, commit_s1_en)
  val commit_s2_count = RegEnable(counters(commit_s1_idx), commit_s1_en)
  val commit_s2_taken = RegEnable(commit_s1_taken, commit_s1_en)
  val commit_s2_en = RegNext(commit_s1_en)

  // calculate updated counter value
  val commit_s2_update = Mux(commit_s2_taken,
    Mux(commit_s2_count === "b11".U, commit_s2_count, commit_s2_count + 1.U),
    Mux(commit_s2_count === "b00".U, commit_s2_count, commit_s2_count - 1.U))

  // write back to table
  when (commit_s2_en) { counters(commit_s2_idx) := commit_s2_update }
}
