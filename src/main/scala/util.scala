//**************************************************************************
// RISCV BOOM Utility Functions
//--------------------------------------------------------------------------
// include stuff in here that is specific to BOOM
// otherwise, put functions in ../common/util.h

package BOOM
{

import Chisel._
import Node._

import rocket.Instructions._
import rocket._

object IsKilledByBranch
{
   def apply(brinfo: BrResolutionInfo, uop: MicroOp): Bool =
   {
      return (brinfo.valid && 
              brinfo.mispredict && 
              maskMatch(brinfo.mask, uop.br_mask))
   }
}
   
object GetNewBrMask
{
   def apply(brinfo: BrResolutionInfo, uop: MicroOp): Bits =
   {
      return Mux(brinfo.valid, (uop.br_mask & ~brinfo.mask),
                               uop.br_mask) 
   }
   def apply(brinfo: BrResolutionInfo, br_mask: Bits): Bits =
   {
      return Mux(brinfo.valid, (br_mask & ~brinfo.mask),
                               br_mask) 
   }
}
 
//do two masks have at least 1 bit match?
object maskMatch
{
   def apply(msk1: Bits, msk2: Bits): Bool = (msk1 & msk2) != Bits(0)
}
   
//clear one-bit in the Mask as specified by the idx
object clearMaskBit
{
   def apply(msk: Bits, idx: UInt): Bits = (msk & ~(Bits(1) << idx))(msk.getWidth-1, 0)
}
  
//shift a register over by one bit
object PerformShiftRegister
{
   def apply(reg_val: Bits, new_bit: Bool): Bits =
   {
      reg_val := Cat(reg_val(reg_val.getWidth-1, 0).toBits, new_bit.toBits).toBits
      reg_val
   }
}
   
// Increment the input "value", wrapping it if necessary.
object WrapInc
{
   def apply(value: UInt, max: Int): UInt =
   {
      if (isPow2(max))
         (value + UInt(1))(log2Up(max)-1,0)
      else
      {
         val wrap = (value === UInt(max-1))
         Mux(wrap, UInt(0), value + UInt(1))
      }
   }
} 
// Decrement the input "value", wrapping it if necessary.
object WrapDec
{
   def apply(value: UInt, max: Int): UInt =
   {
      if (isPow2(max))
         (value - UInt(1))(log2Up(max)-1,0)
      else
      {
         val wrap = (value === UInt(0))
         Mux(wrap, UInt(max-1), value - UInt(1))
      }
   }
}
         

object RotateL1
{
   def apply(signal: Bits): Bits =
   {
      val w = signal.getWidth
      val out = Cat(signal(w-2,0), signal(w-1))
      
      return out
   }
}


object Sext
{
   def apply(x: Bits, length: Int): Bits = 
   {
      return Cat(Fill(x(x.getWidth-1), (length-x.getWidth)), x)
   }
}
        
 
// translates from BOOM's special "packed immediate" to a 32b signed immediate
// Asking for U-type gives it shifted up 12 bits.
object ImmGen
{
   def apply(ip: Bits, isel: Bits): SInt =
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

object DebugIsJALR
{
   def apply(inst: Bits): Bool =
   {
      // TODO CHisel not sure why this won't compile
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
   def apply(inst: Bits): UInt =
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

   val br_targ = Cat(Fill(inst(31),12), Fill(inst(31),8), inst(7), inst(30,25), inst(11,8), Bits(0,1))
   val jal_targ= Cat(Fill(inst(31),12), inst(19,12), inst(20), inst(30,25), inst(24,21), Bits(0,1))

   Mux(is_br, br_targ, jal_targ)
  }
}

}
