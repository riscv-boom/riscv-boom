//******************************************************************************
// Copyright (c) 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Vector Configuration
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Hankun Zhao

// The vector confirguration stuff
// TODO: This will likely all be replaced with the Hwacha implementation

package boom.vec

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket
import freechips.rocketchip.rocket.CSR
import freechips.rocketchip.tile
import freechips.rocketchip.util._
import scala.collection.mutable.LinkedHashMap
import boom.common._
import boom.util._
import boom.common.constants.VecCSRs


class VecStatus extends Bundle {
   val vl      = UInt(width=64.W)
   val vshapes = Vec(32, Bits(width=VSHAPE_SZ.W))
   val vereps  = Vec(32, Bits(width=VEREP_SZ.W))
   val vews    = Vec(32, Bits(width=VEW_SZ.W))
}

class VecConfig(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new Bundle
   {
      val rw        = new Bundle {
         val addr  = Input(UInt(width=CSR.ADDRSZ.W))
         val cmd   = Input(Bits(width=CSR.SZ.W))
         val rdata = Output(Bits(width=xLen.W))
         val wdata = Input(Bits(width=xLen.W))
      }
      val vecstatus = Output(new VecStatus()) // This is definitely not the right way to do this
   })
   val reg_vl      = Reg(UInt(width=32.W))
   val reg_vtypes  = RegInit(VecInit(Seq.fill(32)(0.U(16.W))))
   val reg_vshapes = reg_vtypes.map(x => x(15, 11))
   val reg_vereps  = reg_vtypes.map(x => x(10, 6))
   val reg_vews    = reg_vtypes.map(x => x(5, 0))

   io.vecstatus.vl      := reg_vl
   io.vecstatus.vshapes := reg_vshapes
   io.vecstatus.vereps  := reg_vereps
   io.vecstatus.vews    := reg_vews

   val reg_vxrm  = Reg(Bits(width=3.W))
   val reg_vxsat = Reg(Bits(width=1.W))
   val reg_vmaxew = 0.asUInt(6.W)
   val max_vl = {
      val enabled = (0 until 32).map { x => reg_vews(x) =/= VEW_DISABLE && reg_vshapes(x) === VSHAPE_VECTOR }
      val any_64  = (0 until 32).map { x => enabled(x) && reg_vews(x) === VEW_64 }.reduce(_||_)
      val any_32  = (0 until 32).map { x => enabled(x) && reg_vews(x) === VEW_32 }.reduce(_||_)
      val any_16  = (0 until 32).map { x => enabled(x) && reg_vews(x) === VEW_16 }.reduce(_||_)
      val any_8   = (0 until 32).map { x => enabled(x) && reg_vews(x) === VEW_8 }.reduce(_||_)

      val vl = Mux(any_64, 8.U,
               Mux(any_32, 16.U,
               Mux(any_16, 32.U, 64.U)))
      vl
   }

   val vec_csrs = LinkedHashMap[Int,Bits](
      VecCSRs.vcs     -> Cat(reg_vl, reg_vxrm, reg_vxsat),
      VecCSRs.vxrm    -> reg_vxrm,
      VecCSRs.vl      -> reg_vl,
      VecCSRs.vxsat   -> reg_vxsat,
      VecCSRs.vmaxew  -> reg_vmaxew,
      VecCSRs.vcfg0   -> Cat(reg_vtypes(3) , reg_vtypes(2) , reg_vtypes(1) , reg_vtypes(0)),
      VecCSRs.vcfg2   -> Cat(reg_vtypes(7) , reg_vtypes(6) , reg_vtypes(5) , reg_vtypes(4)),
      VecCSRs.vcfg4   -> Cat(reg_vtypes(11), reg_vtypes(10), reg_vtypes(9) , reg_vtypes(8)),
      VecCSRs.vcfg6   -> Cat(reg_vtypes(15), reg_vtypes(14), reg_vtypes(13), reg_vtypes(12)),
      VecCSRs.vcfg8   -> Cat(reg_vtypes(19), reg_vtypes(18), reg_vtypes(17), reg_vtypes(16)),
      VecCSRs.vcfg10  -> Cat(reg_vtypes(23), reg_vtypes(22), reg_vtypes(21), reg_vtypes(20)),
      VecCSRs.vcfg12  -> Cat(reg_vtypes(27), reg_vtypes(26), reg_vtypes(25), reg_vtypes(24)),
      VecCSRs.vcfg14  -> Cat(reg_vtypes(31), reg_vtypes(30), reg_vtypes(29), reg_vtypes(28))
   )

   def readModifyWriteCSR(cmd: UInt, rdata: UInt, wdata: UInt) =
    (Mux(cmd.isOneOf(CSR.S, CSR.C), rdata, 0.U) | wdata) & ~Mux(cmd === CSR.C, wdata, 0.U)

   val decoded_addr = vec_csrs map { case (k, v) => k -> (io.rw.addr === k.U) }
   val wdata = readModifyWriteCSR(io.rw.cmd, io.rw.rdata, io.rw.wdata)
   val vreg_enabled = (0 until 32).map(x => reg_vtypes(x)(5, 0) =/= VEW_DISABLE)

   assert((0 until 32).map(x => !vreg_enabled(x)
      || reg_vereps(x) === VEREP_FP || reg_vereps(x) === VEREP_UINT).andR,
      "Only IEEE fp/UINT supported\n")
   when (io.rw.cmd.isOneOf(CSR.S, CSR.C, CSR.W)) {
    when (decoded_addr(VecCSRs.vcs))   {
                                         val usr_vl = wdata >> 4;
                                         reg_vl    := Mux(usr_vl < max_vl, usr_vl, max_vl);   // TODO_VEC: fix this
                                         reg_vxrm  := wdata >> 1;
                                         reg_vxsat := wdata; }
    when (decoded_addr(VecCSRs.vl))    { reg_vl    := Mux(wdata < max_vl, wdata, max_vl) }
    when (decoded_addr(VecCSRs.vxrm))  { reg_vxrm  := wdata }
    when (decoded_addr(VecCSRs.vxsat)) { reg_vxsat := wdata }

    // Todo: Make writes zero all above
    when (decoded_addr(VecCSRs.vcfg0))  { reg_vtypes(0)   := wdata      ;
                                          reg_vtypes(1)   := wdata >> 16;
                                          reg_vtypes(2)   := wdata >> 32;
                                          reg_vtypes(3)   := wdata >> 48;
                                          for (x <- 4 until 32) { reg_vtypes(x) := 0.U }
    }
    when (decoded_addr(VecCSRs.vcfg2))  { reg_vtypes(4)   := wdata      ;
                                          reg_vtypes(5)   := wdata >> 16;
                                          reg_vtypes(6)   := wdata >> 32;
                                          reg_vtypes(7)   := wdata >> 48;
                                          for (x <- 8 until 32) { reg_vtypes(x) := 0.U }
    }
    when (decoded_addr(VecCSRs.vcfg4))  { reg_vtypes(8)   := wdata      ;
                                          reg_vtypes(9)   := wdata >> 16;
                                          reg_vtypes(10)  := wdata >> 32;
                                          reg_vtypes(11)  := wdata >> 48;
                                          for (x <- 12 until 32) { reg_vtypes(x) := 0.U }
    }
    when (decoded_addr(VecCSRs.vcfg6))  { reg_vtypes(12)  := wdata      ;
                                          reg_vtypes(13)  := wdata >> 16;
                                          reg_vtypes(14)  := wdata >> 32;
                                          reg_vtypes(15)  := wdata >> 48;
                                          for (x <- 16 until 32) { reg_vtypes(x) := 0.U }
    }
    when (decoded_addr(VecCSRs.vcfg8))  { reg_vtypes(16)  := wdata      ;
                                          reg_vtypes(17)  := wdata >> 16;
                                          reg_vtypes(18)  := wdata >> 32;
                                          reg_vtypes(19)  := wdata >> 48;
                                          for (x <- 20 until 32) { reg_vtypes(x) := 0.U }
    }
    when (decoded_addr(VecCSRs.vcfg10)) { reg_vtypes(20)  := wdata      ;
                                          reg_vtypes(21)  := wdata >> 16;
                                          reg_vtypes(22)  := wdata >> 32;
                                          reg_vtypes(23)  := wdata >> 48;
                                          for (x <- 24 until 32) { reg_vtypes(x) := 0.U }
    }
    when (decoded_addr(VecCSRs.vcfg12)) { reg_vtypes(24)  := wdata      ;
                                          reg_vtypes(25)  := wdata >> 16;
                                          reg_vtypes(26)  := wdata >> 32;
                                          reg_vtypes(27)  := wdata >> 48;
                                          for (x <- 28 until 32) { reg_vtypes(x) := 0.U }
    }
    when (decoded_addr(VecCSRs.vcfg14)) { reg_vtypes(28)  := wdata      ;
                                          reg_vtypes(29)  := wdata >> 16;
                                          reg_vtypes(30)  := wdata >> 32;
                                          reg_vtypes(31)  := wdata >> 48;
    }
   }
   io.rw.rdata := Mux1H(for ((k, v) <- vec_csrs) yield decoded_addr(k) -> v)
}

