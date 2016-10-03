//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Circular Shift Register (CSR), used by the TAGE branch predictor.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2016 Sept 23

// Instead of attempting to dynamically fold a very long history register (1000s
// of bits) into ~10 bits, we will use a ~10b CSR instead. Faster and cheaper.

// Example: An 12 bit value (0b_0111_1001_1111) folded onto a 5 bit CSR becomes
// (0b_0_0010), which can be found of as:


//                /-- history[12] (evict bit)
//                |
//  c[4], c[3], c[2], c[1], c[0]
//   |                        ^
//   |                        |
//   \_______________________/ \---history[0] (newly taken bit)
//
//
// (c[4] ^ h[0] generates the new c[0]).
// (c[1] ^ h[1] generates the new c[2]).

package boom

import Chisel._
import cde.Parameters


class CircularShiftRegisterIO(compressed_length: Int, history_length: Int) extends Bundle
{
   val value = UInt(OUTPUT, compressed_length)

   val do_shift = Bool(INPUT)
   val taken = Bool(INPUT)
   val evict = Bool(INPUT)
   def shift(taken: Bool, evict: Bool) =
   {
      this.do_shift := Bool(true)
      this.taken := taken
      this.evict := evict
   }

   val do_rollback= Bool(INPUT)
   val rollback_value = UInt(INPUT, compressed_length)
   def rollback(v: UInt) =
   {
      do_rollback := Bool(true)
      rollback_value := v
   }

   def InitializeIo(dummy: Int=0) =
   {
      this.do_shift := Bool(false)
      this.taken := Bool(false)
      this.evict := Bool(false)
      this.do_rollback := Bool(false)
      this.rollback_value := UInt(0)
   }
}

class CircularShiftRegister(
   compressed_length: Int,
   history_length: Int
   ) extends Module
{
   val io = new CircularShiftRegisterIO(compressed_length, history_length)

//   val csr = Reg(UInt(width=compressed_length))
   // TODO XXX remove init once fully debugged
   // TODO handle case when clen > hlen
   val csr = Reg(init = UInt(0, width=compressed_length))
   io.value := csr

   when (io.do_rollback)
   {
      csr := io.rollback_value
   }
   .elsewhen (io.do_shift)
   {
      val carry = csr(compressed_length-1)
      csr := Cat(csr, io.taken ^ carry) ^ (io.evict << UInt(history_length % compressed_length))
   }
}
