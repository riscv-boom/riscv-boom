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
  // Write-intent hint: M_PFW instead of M_PFR. A read prefetch can leave the line
  // short of exclusive, so a read-modify-write access (e.g. liblzma's match-finder
  // hash buckets: cur_match = hash[h]; hash[h] = pos) still pays an ownership
  // upgrade on the store and re-exposes the latency at the ROB head. Measured on
  // tests/prefetch-bench (VCS, MediumBoom): read-only walk recovers 8.3 cyc/access
  // with M_PFR, the same walk with an added store recovers only 1.2 cyc/access.
  val write: Bool = Bool()
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
  val writeCalc = WireInit(false.B)

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

    // funct3 picks the hint: `ld x0` (0b011) -> M_PFR, `lw x0` (0b010) -> M_PFW.
    // rxq_inst carries the full 32-bit instruction and is reinterpreted bit-for-bit
    // as RoCCInstruction, so bits 14:12 are still the load's funct3 field (the
    // immediate is already read the same way, via inst(31,20) above).
    writeCalc := io.cmd.bits.inst.asUInt(14, 12) === "b010".U

    prftQueue(prftTail).bits.addr := addrCalc
    prftQueue(prftTail).bits.write := writeCalc
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
    io.mem.req.bits.cmd  := Mux(prftQueue(prftHead).bits.write, M_PFW, M_PFR)
  }.elsewhen(io.cmd.fire) {
    io.mem.req.valid := true.B
    io.mem.req.bits.addr := addrCalc
    io.mem.req.bits.cmd  := Mux(writeCalc, M_PFW, M_PFR)
  }

  when (io.mem.req.fire) {
    prftQueue(prftHead).valid := false.B
    prftHead := WrapInc(prftHead, queueSize)
  }

  io.interrupt := false.B
}
