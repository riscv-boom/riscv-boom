//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV BOOM Utility Functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.util

import Chisel._

import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.rocket.constants.{ScalarOpConstants, MemoryOpConstants}
import boom.common._
import freechips.rocketchip.util._
import boom.exu.{BrResolutionInfo}


// XOR fold an input that is full_length sized down to a compressed_length.
object Fold
{
   def apply(input: UInt, compressed_length: Int, full_length: Int): UInt =
   {
      val clen = compressed_length
      val hlen = full_length
      if (hlen <= clen)
      {
         input
      }
      else
      {
         var res = UInt(0,clen)
         var remaining = input.asUInt
         for (i <- 0 to hlen-1 by clen)
         {
            val len = if (i + clen > hlen ) (hlen - i) else clen
            require(len > 0)
            res = res(clen-1,0) ^ remaining(len-1,0)
            remaining = remaining >> UInt(len)
         }
         res
      }
   }
}

object IsKilledByBranch
{
   def apply(brinfo: BrResolutionInfo, uop: MicroOp): Bool =
   {
      return (brinfo.valid &&
              brinfo.mispredict &&
              maskMatch(brinfo.mask, uop.br_mask))
   }

   def apply(brinfo: BrResolutionInfo, uop_mask: UInt): Bool =
   {
      return (brinfo.valid &&
              brinfo.mispredict &&
              maskMatch(brinfo.mask, uop_mask))
   }
}

object GetNewUopAndBrMask
{
   def apply(uop: MicroOp, brinfo: BrResolutionInfo)
      (implicit p: freechips.rocketchip.config.Parameters): MicroOp =
   {
      val newuop = Wire(init = uop)
      newuop.br_mask :=
         Mux(brinfo.valid,
            (uop.br_mask & ~brinfo.mask),
            uop.br_mask)
      newuop
   }
}

object GetNewBrMask
{
   def apply(brinfo: BrResolutionInfo, uop: MicroOp): UInt =
   {
      return Mux(brinfo.valid, (uop.br_mask & ~brinfo.mask),
                               uop.br_mask)
   }
   def apply(brinfo: BrResolutionInfo, br_mask: UInt): UInt =
   {
      return Mux(brinfo.valid, (br_mask & ~brinfo.mask),
                               br_mask)
   }
}

//do two masks have at least 1 bit match?
object maskMatch
{
   def apply(msk1: UInt, msk2: UInt): Bool = (msk1 & msk2) =/= UInt(0)
}

//clear one-bit in the Mask as specified by the idx
object clearMaskBit
{
   def apply(msk: UInt, idx: UInt): UInt = (msk & ~(UInt(1) << idx))(msk.getWidth-1, 0)
}

//shift a register over by one bit
object PerformShiftRegister
{
   def apply(reg_val: UInt, new_bit: Bool): UInt =
   {
      reg_val := Cat(reg_val(reg_val.getWidth-1, 0).asUInt, new_bit.asUInt).asUInt
      reg_val
   }
}

// Shift a register over by one bit, wrapping the top bit around to the bottom
// (XOR'ed with a new-bit), and evicting a bit at index HLEN.
// This is used to simulate a longer HLEN-width shift register that is folded
// down to a compressed CLEN.
object PerformCircularShiftRegister
{
   def apply(csr: UInt, new_bit: Bool, evict_bit: Bool, hlen: Int, clen: Int): UInt =
   {
      val carry = csr(clen-1)
      val newval = Cat(csr, new_bit ^ carry) ^ (evict_bit << UInt(hlen % clen))
      newval
   }
}

// Increment the input "value", wrapping it if necessary.
object WrapAdd
{
   // "n" is the number of increments, so we wrap at n-1.
   def apply(value: UInt, amt: UInt, n: Int): UInt =
   {
      if (isPow2(n))
      {
         (value + amt)(log2Up(n)-1,0)
      }
      else
      {
         val sum = Cat(UInt(0,1), value) + Cat(UInt(0,1), amt)
         Mux(sum >= UInt(n),
            sum - UInt(n),
            sum)
      }
   }
}

// Decrement the input "value", wrapping it if necessary.
object WrapSub
{
   // "n" is the number of increments, so we wrap to n-1.
   def apply(value: UInt, amt: Int, n: Int): UInt =
   {
      if (isPow2(n))
      {
         (value - UInt(amt))(log2Up(n)-1,0)
      }
      else
      {
         val v = Cat(UInt(0,1), value)
         val b = Cat(UInt(0,1), UInt(amt))
         Mux(value >= UInt(amt),
            value - UInt(amt),
            UInt(n) - (UInt(amt) - value))
      }
   }
}

// Increment the input "value", wrapping it if necessary.
object WrapInc
{
   // "n" is the number of increments, so we wrap at n-1.
   def apply(value: UInt, n: Int): UInt =
   {
      if (isPow2(n))
      {
         (value + UInt(1))(log2Up(n)-1,0)
      }
      else
      {
         val wrap = (value === UInt(n-1))
         Mux(wrap, UInt(0), value + UInt(1))
      }
   }
}
// Decrement the input "value", wrapping it if necessary.
object WrapDec
{
   // "n" is the number of increments, so we wrap at n-1.
   def apply(value: UInt, n: Int): UInt =
   {
      if (isPow2(n))
      {
         (value - UInt(1))(log2Up(n)-1,0)
      }
      else
      {
         val wrap = (value === UInt(0))
         Mux(wrap, UInt(n-1), value - UInt(1))
      }
   }
}

// Mask off lower bits of a PC to align to a "b" Byte boundary.
object AlignPCToBoundary
{
   def apply(pc: UInt, b: Int): UInt =
   {
   // Invert for scenario where pc longer than b
   // (which would clear all bits above size(b)).
      ~(~pc | (b-1).U)
   }
}


object RotateL1
{
   def apply(signal: UInt): UInt =
   {
      val w = signal.getWidth
      val out = Cat(signal(w-2,0), signal(w-1))

      return out
   }
}


object Sext
{
   def apply(x: UInt, length: Int): UInt =
   {
      if (x.getWidth == length) return x
      else return Cat(Fill(length-x.getWidth, x(x.getWidth-1)), x)
   }
}


// translates from BOOM's special "packed immediate" to a 32b signed immediate
// Asking for U-type gives it shifted up 12 bits.
object ImmGen
{
   import boom.common.{LONGEST_IMM_SZ, IS_B, IS_I, IS_J, IS_S, IS_U, IS_V}
   def apply(ip: UInt, isel: UInt): SInt =
   {
      val sign = Mux(isel === IS_V, ip(15).asSInt, ip(LONGEST_IMM_SZ-1).asSInt)
      val i30_20 = Mux(isel === IS_U, ip(18,8).asSInt, sign)
      val i19_12 = Mux(isel === IS_U || isel === IS_J, ip(7,0).asSInt, sign)
      val i11    = Mux(isel === IS_U, SInt(0),
                   Mux(isel === IS_J || isel === IS_B, ip(8).asSInt, sign))
      val i10_5  = Mux(isel === IS_U, SInt(0),
                   Mux(isel === IS_V, ip(15, 14).asSInt, ip(18,14).asSInt))
      val i4_1   = Mux(isel === IS_U, SInt(0), ip(13,9).asSInt)
      val i0     = Mux(isel === IS_S || isel === IS_I || isel === IS_V, ip(8).asSInt, SInt(0))

      return Cat(sign, i30_20, i19_12, i11, i10_5, i4_1, i0).asSInt
   }
}

// store the rounding-mode and func type for FP in the packed immediate as well
object ImmGenRm { def apply(ip: UInt): UInt = { return ip(2,0) }}
object ImmGenTyp { def apply(ip: UInt): UInt = { return ip(9,8) }} // only works if !(IS_B or IS_S)

object DebugIsJALR
{
   def apply(inst: UInt): Bool =
   {
      // TODO Chisel not sure why this won't compile
//      val is_jalr = rocket.DecodeLogic(inst, List(Bool(false)),
//                                       Array(
//                                       JALR -> Bool(true)))
      inst(6,0) === UInt("b1100111")
   }
}

// take an instruction and output its branch or jal target. Only used for a
// debug assert (no where else would we jump straight from instruction bits to
// a target).
object DebugGetBJImm
{
   def apply(inst: UInt): UInt =
   {
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

   val is_br = (inst(6,0) === UInt("b1100011"))

   val br_targ = Cat(Fill(12, inst(31)), Fill(8,inst(31)), inst(7), inst(30,25), inst(11,8), UInt(0,1))
   val jal_targ= Cat(Fill(12, inst(31)), inst(19,12), inst(20), inst(30,25), inst(24,21), UInt(0,1))

   Mux(is_br, br_targ, jal_targ)
  }
}

object AgePriorityEncoder
{
   def apply(in: Seq[Bool], head: UInt): UInt =
   {
      val n = in.size
      require (isPow2(n))
      val temp_vec = (0 until n).map(i => in(i) && UInt(i) >= head) ++ in
      val idx = PriorityEncoder(temp_vec)
      idx(log2Up(n)-1, 0) //discard msb
   }
}



class BranchKillableQueue[T <: boom.common.HasBoomUOP](gen: T, entries: Int)
   (implicit p: freechips.rocketchip.config.Parameters)
   extends boom.common.BoomModule()(p)
   with boom.common.HasBoomCoreParameters
{
   val io = IO(new Bundle
   {
      val enq     = Decoupled(gen).flip
      val deq     = Decoupled(gen)

      val brinfo  = new BrResolutionInfo().asInput
      val flush   = Bool(INPUT)

      val empty   = Bool(OUTPUT)
      val count   = UInt(OUTPUT, log2Up(entries))
   })

   private val ram     = Mem(entries, gen)
   private val valids  = Reg(init = Vec.fill(entries) {Bool(false)})
   private val brmasks = Reg(Vec(entries, UInt(width = MAX_BR_COUNT)))

   private val enq_ptr = Counter(entries)
   private val deq_ptr = Counter(entries)
   private val maybe_full = Reg(init=false.B)

   private val ptr_match = enq_ptr.value === deq_ptr.value
   io.empty := ptr_match && !maybe_full
   private val full = ptr_match && maybe_full
   private val do_enq = Wire(init=io.enq.fire() && !io.flush)

   private val deq_ram_valid = Wire(init= !(io.empty))
   private val do_deq = Wire(init=(io.deq.ready || !valids(deq_ptr.value)) && deq_ram_valid)

   for (i <- 0 until entries)
   {
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
   io.deq.valid := deq_ram_valid && valids(deq_ptr.value) && !IsKilledByBranch(io.brinfo, out.uop)
   io.deq.bits := out
   io.deq.bits.uop.br_mask := GetNewBrMask(io.brinfo, brmasks(deq_ptr.value))


   // For flow queue behavior.
   when (io.empty)
   {
      io.deq.valid := io.enq.valid //&& !IsKilledByBranch(io.brinfo, io.enq.bits.uop)
      io.deq.bits := io.enq.bits
      io.deq.bits.uop.br_mask := GetNewBrMask(io.brinfo, io.enq.bits.uop)

      do_deq := false.B
      when (io.deq.ready) { do_enq := false.B }
   }

   private val ptr_diff = enq_ptr.value - deq_ptr.value
   if (isPow2(entries)) {
      io.count := Cat(maybe_full && ptr_match, ptr_diff)
   } else {
      io.count := Mux(ptr_match,
                     Mux(maybe_full,
                        entries.asUInt, 0.U),
                     Mux(deq_ptr.value > enq_ptr.value,
                        entries.asUInt + ptr_diff, ptr_diff))
   }
}

abstract trait Packing extends HasBoomCoreParameters{
   def recode_dp(n: Bits) = hardfloat.recFNFromFN(11, 53, n.asUInt)
   def recode_sp(n: Bits) = hardfloat.recFNFromFN(8, 24, n.asUInt)
   def recode_hp(n: Bits) = hardfloat.recFNFromFN(5, 11, n.asUInt)
   def ieee_dp(n: Bits) = hardfloat.fNFromRecFN(11, 53, n.asUInt)
   def ieee_sp(n: Bits) = hardfloat.fNFromRecFN(8, 24, n.asUInt)
   def ieee_hp(n: Bits) = hardfloat.fNFromRecFN(5, 11, n.asUInt)

   def _unpack(n: Bits, idx: Int, extent: Int, period: Int, width: Int): UInt = {
      require((idx+1)*period <= extent)
      val base = idx*period
      n(width+base-1, base)
   }
   def _unpack(n: Bits, idx: Int, extent: Int, period: Int): UInt =
      _unpack(n, idx, extent, period, period)

   def unpack_d(n: Bits, idx: Int) = _unpack(n, idx, vecStripLen, SZ_D, SZ_D)
   def unpack_w(n: Bits, idx: Int) = _unpack(n, idx, vecStripLen, SZ_W, SZ_W)
   def unpack_h(n: Bits, idx: Int) = _unpack(n, idx, vecStripLen, SZ_H, SZ_H)
   def _repack(n: Seq[Bits], len: Int) = {
      require(n.length == len)
      Cat(n.reverse)
   }

   def repack_d(n: Seq[Bits]) = _repack(n, vecStripLen/SZ_D)
   def repack_w(n: Seq[Bits]) = _repack(n, vecStripLen/SZ_W)
   def repack_h(n: Seq[Bits]) = _repack(n, vecStripLen/SZ_H)
   def repack_b(n: Seq[Bits]) = _repack(n, vecStripLen/SZ_B)

   def fill_d(n: UInt) = Fill(vecStripLen/SZ_D, n(SZ_D-1, 0))
   def fill_w(n: UInt) = Fill(vecStripLen/SZ_W, n(SZ_W-1, 0))
   def fill_h(n: UInt) = Fill(vecStripLen/SZ_H, n(SZ_H-1, 0))
   def fill_b(n: UInt) = Fill(vecStripLen/SZ_B, n(SZ_B-1, 0))

   def _expand(n: Bits, s: Bits, width: Int) = {
      Cat(Fill(SZ_D - width, s.asUInt), n)
   }

   def expand_d(n: Bits) = n
   def expand_w(n: Bits) = _expand(n, n(SZ_W-1), SZ_W)
   def expand_h(n: Bits) = _expand(n, n(SZ_H-1), SZ_H)
   def expand_b(n: Bits) = _expand(n, n(SZ_B-1), SZ_B)
   def expand_float_d(n: Bits) = expand_d(n)
   def expand_float_s(n: Bits) = expand_w(n)
   def expand_float_h(n: Bits) = expand_h(n)
}

object CalcVecRegAddr
{
   def apply(vew: UInt, eidx: UInt, prs: UInt, numVecPhysRegs: Int) : UInt = {
      val prs_w = log2Ceil(numVecPhysRegs) // TODO_Vec this belongs somewhere in util
      val shiftn = MuxLookup(vew, VEW_8, Array(
         VEW_8 -> UInt(4),
         VEW_16-> UInt(3),
         VEW_32-> UInt(2),
         VEW_64-> UInt(1)))
      val addr = ((eidx >> shiftn) << prs_w) | prs
      addr
   }
}

object CalcEidxUpper
{
   def apply(vew: UInt, eidx: UInt) : UInt = {
      val shiftn = MuxLookup(vew, VEW_8, Array(
         VEW_8 -> UInt(4),
         VEW_16-> UInt(3),
         VEW_32-> UInt(2),
         VEW_64-> UInt(1)))
      val shifted = eidx >> shiftn
      shifted
   }
}

object CalcEidxUpperMask
{
   def apply(vew: UInt, eidx: UInt) : UInt = {
      val shiftn = MuxLookup(vew, VEW_8, Array(
         VEW_8 -> UInt(4),
         VEW_16-> UInt(3),
         VEW_32-> UInt(2),
         VEW_64-> UInt(1)))
      val shifted = (eidx >> shiftn) << shiftn
      shifted
   }
}

object CalcEidxLower
{
   def apply(vew: UInt, eidx: UInt) : UInt = {
      val shiftn = MuxLookup(vew, VEW_8, Array(
         VEW_8 -> "b1111".U,
         VEW_16-> "b111".U,
         VEW_32-> "b11".U,
         VEW_64-> "b1".U))
      val shifted = eidx & shiftn
      shifted
   }
}

object CalcVecMaskFromData
{
   def apply(vew: UInt, data: UInt, ew: Int, stripLen: Int) = {
      val mask = MuxLookup(vew, VEW_8, Array(
         VEW_8  -> Vec((0 until stripLen by ew  ).map(data(_))).asUInt,
         VEW_16 -> Vec((0 until stripLen by ew*2).map(data(_))).asUInt,
         VEW_32 -> Vec((0 until stripLen by ew*4).map(data(_))).asUInt,
         VEW_64 -> Vec((0 until stripLen by ew*8).map(data(_))).asUInt))
      mask
   }
}

object FindFirst {
   def apply(v: Vec[Bool], n: Int, head: UInt, fn: Int=>Bool) = {
      val internal = Wire(Vec(2*n, Bool()))
      for (i <- 0 until n) {
         internal(i+n) := v(i) && fn(i)
         internal(i) := internal(i+n) && (UInt(i) >= head)
      }
      val priority_oh = PriorityEncoderOH(internal)
      val out = Wire(Vec(n, Bool()))
      for (i <- 0 until n) {
         out(i) := priority_oh(i) | priority_oh(i+n)
      }
      out
   }
}
