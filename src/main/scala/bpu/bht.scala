package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}

case class BHTParameters(
  enabled: Boolean = true,
  num_entries: Int = 128,
  dualported: Boolean = false)

case object BHTKey extends Field[BHTParameters]

class BHTBrPredictor(
    fetch_width: Int,
    num_entries: Int = 128,
    dualported: Boolean = false)(implicit p: Parameters)
      extends BrPredictor(fetch_width, 1)(p)
{
  require (coreInstBytes == 4)

  // Predictor state
  val counters = Module(new TwobcCounterTable(fetch_width, num_entries, dualported))

  val stall = !io.resp.ready

  val s1_pc = io.req_pc
  val s1_r_idx = s1_pc >> UInt(log2Ceil(fetch_width * coreInstBytes))

  counters.io.s1_r_idx := s1_r_idx
  counters.io.stall := stall

  io.resp.valid := !this.disable_bpd
  io.resp.bits.takens := counters.io.s2_r_out
  io.resp.bits.info := RegNext(s1_r_idx)

  counters.io.update.valid := this.commit.valid && !this.disable_bpd
  counters.io.update.bits.index := this.commit.bits.info.info
  counters.io.update.bits.executed         := this.commit.bits.ctrl.executed
  counters.io.update.bits.takens           := this.commit.bits.ctrl.taken
  counters.io.update.bits.was_mispredicted := this.commit.bits.ctrl.mispredicted.reduce(_|_)
  counters.io.update.bits.do_initialize    := Bool(false)
}
