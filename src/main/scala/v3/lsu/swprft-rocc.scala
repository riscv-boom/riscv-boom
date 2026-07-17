// Ported from EECS-NTNU/riscv-boom TEA commit (Björn Gottschall, 2022)
// https://github.com/EECS-NTNU/riscv-boom/commit/a5ddebab
// Adapted to boom.v3 package namespace.
// Original: src/main/scala/lsu/swprft-rocc.scala

package boom.v3.lsu

import chisel3._
import chisel3.util._
import boom.v3.common._
import boom.v3.util.WrapInc
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._

class PrftEntry(implicit p : Parameters) extends BoomBundle()(p) {
  val addr: UInt = UInt(coreMaxAddrBits.W)
}

class SoftwarePrefetchRoCC(opcodes: OpcodeSet, queueSize: Int = 32)
                          (implicit p: Parameters) extends LazyRoCC(opcodes = opcodes) {
  override lazy val module = new SoftwarePrefetchRoCCModule(outer = this, queueSize = queueSize)
}

class SoftwarePrefetchRoCCModule(outer: SoftwarePrefetchRoCC, queueSize: Int)
  extends LazyRoCCModuleImp(outer) with HasBoomCoreParameters {
  override def desiredName = "SoftwarePrefetchRoCC"

  // Prevent inlining so the module is visible in waveforms
  dontTouch(io.cmd)
  dontTouch(io.mem.req)

  val prftQueue: Vec[ValidIO[PrftEntry]] = Reg(Vec(queueSize, Valid(new PrftEntry)))
  val prftHead: UInt = RegInit(0.U(log2Ceil(queueSize).W))
  val prftTail: UInt = RegInit(0.U(log2Ceil(queueSize).W))
  val addrCalc = WireInit(0.U(coreMaxAddrBits.W))

  val prftQueueFull: Bool = (prftHead === prftTail) && prftQueue(prftHead).valid

  io.busy := prftQueueFull
  io.cmd.ready := !prftQueueFull

  when (io.cmd.fire) {
    val imm_addr: UInt = (io.cmd.bits.rs1.asSInt + io.cmd.bits.inst.asUInt(31, 20).asSInt).asUInt
    val addr_sign: Bool = Mux(imm_addr(vaddrBits - 1),
      ~imm_addr(63, vaddrBits) === 0.U,
      imm_addr(63, vaddrBits) =/= 0.U
    )

    addrCalc := Cat(addr_sign, imm_addr(vaddrBits-1, 0)).asUInt

    prftQueue(prftTail).bits.addr := addrCalc
    prftQueue(prftTail).valid := true.B
    prftTail := WrapInc(prftTail, queueSize)
  }

  io.mem.req.valid := false.B
  io.mem.req.bits.cmd := M_PFR
  io.mem.req.bits.size := log2Ceil(8).U
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.data := 0.U
  io.mem.req.bits.phys := false.B
  io.mem.s1_kill := false.B
  io.mem.s2_kill := false.B

  when(prftQueue(prftHead).valid) {
    io.mem.req.valid := true.B
    io.mem.req.bits.addr := prftQueue(prftHead).bits.addr
  }.elsewhen(io.cmd.fire) {
    io.mem.req.valid := true.B
    io.mem.req.bits.addr := addrCalc
  }

  when (io.mem.req.fire) {
    prftQueue(prftHead).valid := false.B
    prftHead := WrapInc(prftHead, queueSize)
  }

  io.interrupt := false.B
}
