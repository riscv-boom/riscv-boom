//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV BOOM Utility Functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom

import Chisel._

import rocket.Instructions._
import rocket._

object assertNever
{
   def apply(cond: Bool, message: String): Unit =
   {
      assert(!cond, message)
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
   def apply(uop: MicroOp, brinfo: BrResolutionInfo)(implicit p: cde.Parameters): MicroOp =
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
      reg_val := Cat(reg_val(reg_val.getWidth-1, 0).toUInt, new_bit.toUInt).toUInt
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
      return Cat(Fill(length-x.getWidth, x(x.getWidth-1)), x)
   }
}


// translates from BOOM's special "packed immediate" to a 32b signed immediate
// Asking for U-type gives it shifted up 12 bits.
object ImmGen
{
   def apply(ip: UInt, isel: UInt): SInt =
   {
      val sign = ip(LONGEST_IMM_SZ-1).toSInt
      val i30_20 = Mux(isel === IS_U, ip(18,8).toSInt, sign)
      val i19_12 = Mux(isel === IS_U || isel === IS_J, ip(7,0).toSInt, sign)
      val i11    = Mux(isel === IS_U, SInt(0),
                   Mux(isel === IS_J || isel === IS_B, ip(8).toSInt, sign))
      val i10_5  = Mux(isel === IS_U, SInt(0), ip(18,14).toSInt)
      val i4_1   = Mux(isel === IS_U, SInt(0), ip(13,9).toSInt)
      val i0     = Mux(isel === IS_S || isel === IS_I, ip(8).toSInt, SInt(0))

      return Cat(sign, i30_20, i19_12, i11, i10_5, i4_1, i0).toSInt
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


class QueueForMicroOpWithData(entries: Int, data_width: Int)(implicit p: cde.Parameters) extends BoomModule()(p)
{
   val io = new Bundle
   {
      val enq     = Decoupled(new ExeUnitResp(data_width)).flip
      val deq     = Decoupled(new ExeUnitResp(data_width))

      val brinfo  = new BrResolutionInfo().asInput
      val flush   = Bool(INPUT)

      val empty   = Bool(OUTPUT)
      val count   = UInt(OUTPUT, log2Up(entries))
   }

   private val ram     = Mem(entries, new ExeUnitResp(data_width))
   private val valids  = Reg(init = Vec.fill(entries) {Bool(false)})
   private val brmasks = Reg(Vec(entries, UInt(width = MAX_BR_COUNT)))

   private val enq_ptr = Counter(entries)
   private val deq_ptr = Counter(entries)
   private val maybe_full = Reg(init=false.B)

   private val ptr_match = enq_ptr.value === deq_ptr.value
   io.empty := ptr_match && !maybe_full
   private val full = ptr_match && maybe_full
   private val do_enq = Wire(init=io.enq.fire())

   private val deq_ram_valid = Wire(init= !(io.empty))
   private val do_deq = Wire(init=io.deq.ready && deq_ram_valid)

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
      valids(enq_ptr.value) := true.B
      brmasks(enq_ptr.value) := GetNewBrMask(io.brinfo, io.enq.bits.uop)
      enq_ptr.inc()
   }
   when (do_deq) {
      deq_ptr.inc()
   }
   when (do_enq != do_deq) {
      maybe_full := do_enq
   }

   io.enq.ready := !full

   private val out = ram(deq_ptr.value)
   io.deq.valid := deq_ram_valid && valids(deq_ptr.value) && !IsKilledByBranch(io.brinfo, out.uop) //!empty
   io.deq.bits := out
   io.deq.bits.uop.br_mask := GetNewBrMask(io.brinfo, brmasks(deq_ptr.value))


   // For flow queue behavior.
   when (io.enq.valid) { io.deq.valid := true.B }
   when (io.empty) {
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

