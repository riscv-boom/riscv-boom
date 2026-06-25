package boom.v3.common

import chisel3._
import chisel3.experimental.IntParam
import chisel3.util.HasBlackBoxResource

class SimTMACounterDump(numCounters: Int, tileId: Int)
    extends BlackBox(Map("NUM_COUNTERS" -> IntParam(numCounters), "TILE_ID" -> IntParam(tileId)))
    with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock    = Input(Clock())
    val reset    = Input(Bool())
    val counters = Input(Vec(numCounters, UInt(64.W)))
  })
  addResource("/vsrc/SimTMACounterDump.v")
  addResource("/csrc/SimTMACounterDump.cc")
}
