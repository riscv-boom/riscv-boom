//******************************************************************************
// Copyright (c) 2020 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Boom RVC Expander
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters}

import boom.common._

class Expander(implicit p: Parameters) extends BoomModule with HasBoomFrontendParameters
{
  val io = IO(new BoomBundle {
    val imemresp     = Input(new FrontendResp)
    val fire         = Input(Bool())
    val clear        = Input(Bool())

    val fetch_bundle = Output(new FetchBundle)
  })

  require(fetchWidth >= 4) // Logic gets kind of annoying with fetchWidth = 2

  def isRVC(inst: UInt) = (inst(1,0) =/= 3.U)

  // Tracks trailing 16b of previous fetch packet
  val prev_half    = Reg(UInt(16.W))
  // Tracks if last fetchpacket contained a half-inst
  val prev_is_half = RegInit(false.B)

  var bank_prev_is_half = prev_is_half
  var bank_prev_half    = prev_half

  io.fetch_bundle := DontCare

  val mask = Wire(Vec(fetchWidth, Bool()))

  for (b <- 0 until nBanks) {
    val data       = io.imemresp.data
    val bank_data  = data((b+1)*bankWidth*16-1, b*bankWidth*16)
    val bank_mask  = Wire(Vec(bankWidth, Bool()))
    val bank_insts = Wire(Vec(bankWidth, UInt(32.W)))

    for (w <- 0 until bankWidth) {
      val i = (b * bankWidth) + w

      val valid = Wire(Bool())
      val inst = Wire(UInt(32.W))
      if (w == 0) {
        when (bank_prev_is_half) {
          inst := Cat(bank_data(15,0), bank_prev_half)
          io.fetch_bundle.edge_inst(b) := true.B
        } .otherwise {
          inst := bank_data(31,0)
          io.fetch_bundle.edge_inst(b) := false.B
        }
        valid := true.B
      } else if (w == 1) {
        // Need special case since 0th instruction may carry over the wrap around
        inst  := bank_data(47,16)
        valid := bank_prev_is_half || !(bank_mask(0) && !isRVC(bank_insts(0)))
      } else if (w == bankWidth - 1) {
        inst  := Cat(0.U(16.W), bank_data(bankWidth*16-1,(bankWidth-1)*16))
        valid := !((bank_mask(w-1) && !isRVC(bank_insts(w-1))) ||
                   !isRVC(inst))
      } else {
        inst  := bank_data(w*16+32-1,w*16)
        valid := !(bank_mask(w-1) && !isRVC(bank_insts(w-1)))
      }

      bank_insts(w) := inst
      bank_mask(w)  := io.imemresp.mask(i) && valid

      val exp_inst = ExpandRVC(inst)

      io.fetch_bundle.exp_insts(i) := exp_inst
      io.fetch_bundle.insts    (i) := inst
      io.fetch_bundle.is_rvc   (i) := isRVC(inst)
      mask                     (i) := bank_mask(w)
    }

    val last_inst = bank_insts(bankWidth-1)
    bank_prev_is_half = Mux(bank_mask(b),
      (!(bank_mask(bankWidth-2) && !isRVC(bank_insts(bankWidth-2))) && !isRVC(last_inst)),
      bank_prev_is_half)
    bank_prev_half    = Mux(bank_mask(b),
      last_inst(15,0),
      bank_prev_half)
  }

  io.fetch_bundle.mask := mask.asUInt

  io.fetch_bundle.end_half.valid := bank_prev_is_half
  io.fetch_bundle.end_half.bits  := bank_prev_half

  when (io.clear) {
    prev_is_half := false.B
  } .elsewhen (io.fire) {
    prev_is_half := bank_prev_is_half
    prev_half    := bank_prev_half
  }
}
