//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Utility Functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.util

import chisel3._
import chisel3.util._

import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util.{Str}
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.tile.{TileKey}

import boom.common.{MicroOp}
import boom.exu.{BrResolutionInfo}

/**
 * Object to XOR fold a input register of fullLength into a compressedLength.
 */
object Fold
{
  def apply(input: UInt, compressedLength: Int, fullLength: Int): UInt = {
    val clen = compressedLength
    val hlen = fullLength
    if (hlen <= clen) {
      input
    } else {
      var res = 0.U(clen.W)
      var remaining = input.asUInt
      for (i <- 0 to hlen-1 by clen) {
        val len = if (i + clen > hlen ) (hlen - i) else clen
        require(len > 0)
        res = res(clen-1,0) ^ remaining(len-1,0)
        remaining = remaining >> len.U
      }
      res
    }
  }
}

/**
 * Object to check if MicroOp was killed due to a branch mispredict.
 */
object IsKilledByBranch
{
  def apply(brinfo: BrResolutionInfo, uop: MicroOp): Bool = {
    return (brinfo.valid &&
            brinfo.mispredict &&
            maskMatch(brinfo.mask, uop.br_mask))
  }

  def apply(brinfo: BrResolutionInfo, uop_mask: UInt): Bool = {
    return (brinfo.valid &&
            brinfo.mispredict &&
            maskMatch(brinfo.mask, uop_mask))
  }
}

/**
 * Object to return new MicroOp with a new BR mask given a MicroOp mask
 * and old BR mask.
 */
object GetNewUopAndBrMask
{
  def apply(uop: MicroOp, brinfo: BrResolutionInfo)
    (implicit p: Parameters): MicroOp = {
    val newuop = WireInit(uop)
    newuop.br_mask := Mux(brinfo.valid,
                          (uop.br_mask & ~brinfo.mask),
                           uop.br_mask)
    newuop
  }
}

/**
 * Object to return a BR mask given a MicroOp mask and old BR mask.
 */
object GetNewBrMask
{
   def apply(brinfo: BrResolutionInfo, uop: MicroOp): UInt = {
     return Mux(brinfo.valid,
                (uop.br_mask & ~brinfo.mask),
                uop.br_mask)
   }

   def apply(brinfo: BrResolutionInfo, br_mask: UInt): UInt = {
     return Mux(brinfo.valid,
                (br_mask & ~brinfo.mask),
                br_mask)
   }
}

/**
 * Object to check if at least 1 bit matches in two masks
 */
object maskMatch
{
  def apply(msk1: UInt, msk2: UInt): Bool = (msk1 & msk2) =/= 0.U
}

/**
 * Object to clear one bit in a mask given an index
 */
object clearMaskBit
{
  def apply(msk: UInt, idx: UInt): UInt = (msk & ~(1.U << idx))(msk.getWidth-1, 0)
}

/**
 * Object to shift a register over by one bit and concat a new one
 */
object PerformShiftRegister
{
  def apply(reg_val: UInt, new_bit: Bool): UInt = {
    reg_val := Cat(reg_val(reg_val.getWidth-1, 0).asUInt, new_bit.asUInt).asUInt
    reg_val
  }
}

/**
 * Object to shift a register over by one bit, wrapping the top bit around to the bottom
 * (XOR'ed with a new-bit), and evicting a bit at index HLEN.
 * This is used to simulate a longer HLEN-width shift register that is folded
 * down to a compressed CLEN.
 */
object PerformCircularShiftRegister
{
  def apply(csr: UInt, new_bit: Bool, evict_bit: Bool, hlen: Int, clen: Int): UInt = {
    val carry = csr(clen-1)
    val newval = Cat(csr, new_bit ^ carry) ^ (evict_bit << (hlen % clen).U)
    newval
  }
}

/**
 * Object to increment an input value, wrapping it if
 * necessary.
 */
object WrapAdd
{
  // "n" is the number of increments, so we wrap at n-1.
  def apply(value: UInt, amt: UInt, n: Int): UInt = {
    if (isPow2(n)) {
      (value + amt)(log2Ceil(n)-1,0)
    } else {
      val sum = Cat(0.U(1.W), value) + Cat(0.U(1.W), amt)
      Mux(sum >= n.U,
          sum - n.U,
          sum)
    }
  }
}

/**
 * Object to decrement an input value, wrapping it if
 * necessary.
 */
object WrapSub
{
  // "n" is the number of increments, so we wrap to n-1.
  def apply(value: UInt, amt: Int, n: Int): UInt = {
    if (isPow2(n)) {
       (value - amt.U)(log2Ceil(n)-1,0)
    } else {
      val v = Cat(0.U(1.W), value)
      val b = Cat(0.U(1.W), amt.U)
      Mux(value >= amt.U,
          value - amt.U,
          n.U - amt.U - value)
    }
  }
}

/**
 * Object to increment an input value, wrapping it if
 * necessary.
 */
object WrapInc
{
  // "n" is the number of increments, so we wrap at n-1.
  def apply(value: UInt, n: Int): UInt = {
    if (isPow2(n)) {
      (value + 1.U)(log2Ceil(n)-1,0)
    } else {
      val wrap = (value === (n-1).U)
      Mux(wrap, 0.U, value + 1.U)
    }
  }
}

/**
 * Object to decrement an input value, wrapping it if
 * necessary.
 */
object WrapDec
{
  // "n" is the number of increments, so we wrap at n-1.
  def apply(value: UInt, n: Int): UInt = {
    if (isPow2(n)) {
      (value - 1.U)(log2Ceil(n)-1,0)
    } else {
      val wrap = (value === 0.U)
      Mux(wrap, (n-1).U, value - 1.U)
    }
  }
}

/**
 * Object to mask off lower bits of a PC to align to a "b"
 * Byte boundary.
 */
object AlignPCToBoundary
{
  def apply(pc: UInt, b: Int): UInt = {
    // Invert for scenario where pc longer than b
    //   (which would clear all bits above size(b)).
    ~(~pc | (b-1).U)
  }
}

/**
 * Object to rotate a signal left by one
 */
object RotateL1
{
  def apply(signal: UInt): UInt = {
    val w = signal.getWidth
    val out = Cat(signal(w-2,0), signal(w-1))

    return out
  }
}

/**
 * Object to sext a value to a particular length.
 */
object Sext
{
  def apply(x: UInt, length: Int): UInt = {
    if (x.getWidth == length) return x
    else return Cat(Fill(length-x.getWidth, x(x.getWidth-1)), x)
  }
}

/**
 * Object to translate from BOOM's special "packed immediate" to a 32b signed immediate
 * Asking for U-type gives it shifted up 12 bits.
 */
object ImmGen
{
  import boom.common.{LONGEST_IMM_SZ, IS_B, IS_I, IS_J, IS_S, IS_U}
  def apply(ip: UInt, isel: UInt): SInt = {
    val sign = ip(LONGEST_IMM_SZ-1).asSInt
    val i30_20 = Mux(isel === IS_U, ip(18,8).asSInt, sign)
    val i19_12 = Mux(isel === IS_U || isel === IS_J, ip(7,0).asSInt, sign)
    val i11    = Mux(isel === IS_U, 0.S,
                 Mux(isel === IS_J || isel === IS_B, ip(8).asSInt, sign))
    val i10_5  = Mux(isel === IS_U, 0.S, ip(18,14).asSInt)
    val i4_1   = Mux(isel === IS_U, 0.S, ip(13,9).asSInt)
    val i0     = Mux(isel === IS_S || isel === IS_I, ip(8).asSInt, 0.S)

    return Cat(sign, i30_20, i19_12, i11, i10_5, i4_1, i0).asSInt
  }
}

/**
 * Object to get the FP rounding mode out of a packed immediate.
 */
object ImmGenRm { def apply(ip: UInt): UInt = { return ip(2,0) } }

/**
 * Object to get the FP function fype from a packed immediate.
 * Note: only works if !(IS_B or IS_S)
 */
object ImmGenTyp { def apply(ip: UInt): UInt = { return ip(9,8) } }

/**
 * Object to see if an instruction is a JALR.
 */
object DebugIsJALR
{
  def apply(inst: UInt): Bool = {
    // TODO Chisel not sure why this won't compile
//    val is_jalr = rocket.DecodeLogic(inst, List(Bool(false)),
//                                     Array(
//                                     JALR -> Bool(true)))
    inst(6,0) === "b1100111".U
  }
}

/**
 * Object to take an instruction and output its branch or jal target. Only used
 * for a debug assert (no where else would we jump straight from instruction
 * bits to a target).
 */
object DebugGetBJImm
{
  def apply(inst: UInt): UInt = {
    // TODO Chisel not sure why this won't compile
    //val csignals =
    //rocket.DecodeLogic(inst,
    //                    List(Bool(false), Bool(false)),
    //      Array(
    //         BEQ     -> List(Bool(true ), Bool(false)),
    //         BNE     -> List(Bool(true ), Bool(false)),
    //         BGE     -> List(Bool(true ), Bool(false)),
    //         BGEU    -> List(Bool(true ), Bool(false)),
    //         BLT     -> List(Bool(true ), Bool(false)),
    //         BLTU    -> List(Bool(true ), Bool(false))
    //      ))
    //val is_br :: nothing :: Nil = csignals

    val is_br = (inst(6,0) === "b1100011".U)

    val br_targ = Cat(Fill(12, inst(31)), Fill(8,inst(31)), inst(7), inst(30,25), inst(11,8), 0.U(1.W))
    val jal_targ= Cat(Fill(12, inst(31)), inst(19,12), inst(20), inst(30,25), inst(24,21), 0.U(1.W))

    Mux(is_br, br_targ, jal_targ)
  }
}

/**
 * Object to return the lowest bit position after the head.
 */
object AgePriorityEncoder
{
  def apply(in: Seq[Bool], head: UInt): UInt = {
    val n = in.size
    val width = log2Ceil(in.size)
    val n_padded = 1 << width
    val temp_vec = (0 until n_padded).map(i => if (i < n) in(i) && i.U >= head else false.B) ++ in
    val idx = PriorityEncoder(temp_vec)
    idx(width-1, 0) //discard msb
  }
}

/**
  * Object to determine whether queue
  * index i0 is older than index i1.
 */
object IsOlder
{
  def apply(i0: UInt, i1: UInt, head: UInt) = ((i0 < i1) ^ (i0 < head) ^ (i1 < head))
}

/**
 * Set all bits at or below the highest order '1'.
 */
object MaskLower
{
  def apply(in: UInt) = {
    val n = in.getWidth
    (0 until n).map(i => in >> i.U).reduce(_|_)
  }
}

/**
 * Set all bits at or above the lowest order '1'.
 */
object MaskUpper
{
  def apply(in: UInt) = {
    val n = in.getWidth
    (0 until n).map(i => (in << i.U)(n-1,0)).reduce(_|_)
  }
}

/**
  * N-wide one-hot priority encoder.
 */
object SelectFirstN
{
  def apply(in: UInt, n: Int) = {
    val counts = in.asBools.scanLeft(1.U(n.W))((cnt, elt) => Mux(elt, cnt << 1, cnt))
    val sels = (0 until n).map(j => VecInit((0 until in.getWidth).map(i => counts(i)(j) & in(i))).asUInt)
    VecInit(sels)
  }
}

/**
 * Create a queue that can be killed with a branch kill signal.
 * Assumption: enq.valid only high if not killed by branch (so don't check IsKilled on io.enq).
 */
class BranchKillableQueue[T <: boom.common.HasBoomUOP](gen: T, entries: Int)
  (implicit p: Parameters)
  extends boom.common.BoomModule()(p)
  with boom.common.HasBoomCoreParameters
{
  val io = IO(new Bundle {
    val enq     = Flipped(Decoupled(gen))
    val deq     = Decoupled(gen)

    val brinfo  = Input(new BrResolutionInfo())
    val flush   = Input(Bool())

    val empty   = Output(Bool())
    val count   = Output(UInt(log2Ceil(entries).W))
  })

  private val ram     = Mem(entries, gen)
  private val valids  = RegInit(VecInit(Seq.fill(entries) {false.B}))
  private val brmasks = Reg(Vec(entries, UInt(maxBrCount.W)))

  private val enq_ptr = Counter(entries)
  private val deq_ptr = Counter(entries)
  private val maybe_full = RegInit(false.B)

  private val ptr_match = enq_ptr.value === deq_ptr.value
  io.empty := ptr_match && !maybe_full
  private val full = ptr_match && maybe_full
  private val do_enq = WireInit(io.enq.fire())

  private val deq_ram_valid = WireInit(!(io.empty))
  private val do_deq = WireInit(io.deq.ready && deq_ram_valid)

  for (i <- 0 until entries) {
    val mask = brmasks(i)
    valids(i)  := valids(i) && !IsKilledByBranch(io.brinfo, mask) && !io.flush
    when (valids(i)) {
      brmasks(i) := GetNewBrMask(io.brinfo, mask)
    }
  }

  when (do_enq) {
    ram(enq_ptr.value) := io.enq.bits
    valids(enq_ptr.value) := true.B //!IsKilledByBranch(io.brinfo, io.enq.bits.uop)
    brmasks(enq_ptr.value) := GetNewBrMask(io.brinfo, io.enq.bits.uop)
    enq_ptr.inc()
  }

  when (do_deq) {
    deq_ptr.inc()
  }

  when (do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  io.enq.ready := !full

  private val out = ram(deq_ptr.value)
  val out_brmask = brmasks(deq_ptr.value)
  io.deq.valid := deq_ram_valid && valids(deq_ptr.value) && !IsKilledByBranch(io.brinfo, out_brmask)
  io.deq.bits := out
  io.deq.bits.uop.br_mask := GetNewBrMask(io.brinfo, out_brmask)

  // For flow queue behavior.
  when (io.empty) {
    io.deq.valid := io.enq.valid //&& !IsKilledByBranch(io.brinfo, io.enq.bits.uop)
    io.deq.bits := io.enq.bits
    io.deq.bits.uop.br_mask := GetNewBrMask(io.brinfo, io.enq.bits.uop)

    do_deq := false.B
    when (io.deq.ready) { do_enq := false.B }
  }

  private val ptr_diff = enq_ptr.value - deq_ptr.value
  if (isPow2(entries)) {
    io.count := Cat(maybe_full && ptr_match, ptr_diff)
  }
  else {
    io.count := Mux(ptr_match,
                    Mux(maybe_full,
                        entries.asUInt, 0.U),
                    Mux(deq_ptr.value > enq_ptr.value,
                        entries.asUInt + ptr_diff, ptr_diff))
  }
}

// ------------------------------------------
// Printf helper functions
// ------------------------------------------

object BoolToChar
{
  /**
   * Take in a Chisel Bool and convert it into a Str
   * based on the Chars given
   *
   * @param c_bool Chisel Bool
   * @param trueChar Scala Char if bool is true
   * @param falseChar Scala Char if bool is false
   * @return UInt ASCII Char for "trueChar" or "falseChar"
   */
  def apply(c_bool: Bool, trueChar: Char, falseChar: Char = '-'): UInt = {
    Mux(c_bool, Str(trueChar), Str(falseChar))
  }
}

object CfiTypeToChars
{
  /**
   * Get a Vec of Strs that can be used for printing
   *
   * @param cfi_type specific cfi type
   * @return Vec of Strs (must be indexed to get specific char)
   */
  def apply(cfi_type: UInt) = {
    val strings = Seq("----", "BR  ", "JAL ", "JALR")
    val multiVec = VecInit(for(string <- strings) yield { VecInit(for (c <- string) yield { Str(c) }) })
    multiVec(cfi_type)
  }
}

object BpdTypeToChars
{
  /**
   * Get a Vec of Strs that can be used for printing
   *
   * @param bpd_type specific bpd type
   * @return Vec of Strs (must be indexed to get specific char)
   */
  def apply(bpd_type: UInt) = {
    val strings = Seq("BR  ", "JUMP", "----", "RET ", "----", "CALL", "----", "----")
    val multiVec = VecInit(for(string <- strings) yield { VecInit(for (c <- string) yield { Str(c) }) })
    multiVec(bpd_type)
  }
}

object RobTypeToChars
{
  /**
   * Get a Vec of Strs that can be used for printing
   *
   * @param rob_type specific rob type
   * @return Vec of Strs (must be indexed to get specific char)
   */
  def apply(rob_type: UInt) = {
    val strings = Seq("RST", "NML", "RBK", " WT")
    val multiVec = VecInit(for(string <- strings) yield { VecInit(for (c <- string) yield { Str(c) }) })
    multiVec(rob_type)
  }
}

object XRegToChars
{
  /**
   * Get a Vec of Strs that can be used for printing
   *
   * @param xreg specific register number
   * @return Vec of Strs (must be indexed to get specific char)
   */
  def apply(xreg: UInt) = {
    val strings = Seq(" x0", " ra", " sp", " gp",
                      " tp", " t0", " t1", " t2",
                      " s0", " s1", " a0", " a1",
                      " a2", " a3", " a4", " a5",
                      " a6", " a7", " s2", " s3",
                      " s4", " s5", " s6", " s7",
                      " s8", " s9", "s10", "s11",
                      " t3", " t4", " t5", " t6")
    val multiVec = VecInit(for(string <- strings) yield { VecInit(for (c <- string) yield { Str(c) }) })
    multiVec(xreg)
  }
}

object FPRegToChars
{
  /**
   * Get a Vec of Strs that can be used for printing
   *
   * @param fpreg specific register number
   * @return Vec of Strs (must be indexed to get specific char)
   */
  def apply(fpreg: UInt) = {
    val strings = Seq(" ft0", " ft1", " ft2", " ft3",
                      " ft4", " ft5", " ft6", " ft7",
                      " fs0", " fs1", " fa0", " fa1",
                      " fa2", " fa3", " fa4", " fa5",
                      " fa6", " fa7", " fs2", " fs3",
                      " fs4", " fs5", " fs6", " fs7",
                      " fs8", " fs9", "fs10", "fs11",
                      " ft8", " ft9", "ft10", "ft11")
    val multiVec = VecInit(for(string <- strings) yield { VecInit(for (c <- string) yield { Str(c) }) })
    multiVec(fpreg)
  }
}

object BoomCoreStringPrefix
{
  /**
  * Add prefix to BOOM strings (currently only adds the hartId)
  *
  * @param strs list of strings
  * @return String combining the list with the prefix per line
  */
  def apply(strs: String*)(implicit p: Parameters) = {
    val prefix = "[C" + s"${p(TileKey).hartId}" + "] "
    strs.map(str => prefix + str + "\n").mkString("")
  }
}
