//**************************************************************************
// RISCV BOOM Utility Functions
//--------------------------------------------------------------------------
// include stuff in here that is specific to BOOM
// otherwise, put functions in ../common/util.h

package BOOM
{

import Chisel._
import Node._

import Common._

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

// translates from BOOM's special "packed immediate" to a 32b signed immediate
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

}
