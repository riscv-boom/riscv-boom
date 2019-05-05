//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Branch Checker Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// The branch checker performs the following tasks:
//    - Verify BTB predicted the type and target of instructions correctly.
//    - Catch any JALs and redirect the frontend.
// This is purely combinational logic.
// If an error is found, redirect the front-end to refetch and correct the
// misprediction.

package boom.ifu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.util.{Str}

import boom.bpu._
import boom.common._

/**
 * Combinational logic to verify that the BoomBTB predicted correctly. This compares the
 * decoded instruction to the BTB prediction. If they differ then there is a redirect.
 * Also catch JALs that have not been predicted by the BTB.
 */
class BranchChecker(implicit p: Parameters) extends BoomModule
  with HasL1ICacheBankedParameters
{
  val io = IO(new Bundle {
    // input responses from the decoded insts and btb
    val insts        = Flipped(Valid(new BoomBundle {
      val mask            = Vec(fetchWidth, Bool()) // which insts are valid?
      val decode_signals  = Vec(fetchWidth, new BoomBundle { // decoded inst signals
        val is_br        = Bool() // is it a type of instruction?
        val is_jal       = Bool()
        val is_jalr      = Bool()
        val is_call      = Bool()
        val is_ret       = Bool()
        val is_rvc       = Bool()

        val br_target    = UInt(vaddrBitsExtended.W) // calculated targets from branch decode
        val jal_target   = UInt(vaddrBitsExtended.W)
      })
      val edge_inst    = Bool()
      val fetch_pc        = UInt(vaddrBitsExtended.W) // fetch packet pc
      val aligned_pc      = UInt(vaddrBitsExtended.W) // fetch packet pc aligned to fetch boundary
    }))
    val btb_resp     = Flipped(Valid(new BoomBTBResp))

    // redirection of the frontend based on results
    val resp         = Valid(new BoomBundle {
      val pc_req = new PCReq // address to redirect to
      val cfi_idx = UInt(log2Ceil(fetchWidth).W) // cfi idx of redirect
    })
  })

  // Did the BTB mispredict the cfi type?
  val wrong_cfi = WireInit(false.B)
  // Did the BTB mispredict the cfi target?
  val wrong_target = WireInit(false.B)

  val btb_cfi_idx = io.btb_resp.bits.cfi_idx
  val btb_target = io.btb_resp.bits.target
  val btb_cfi_type = io.btb_resp.bits.cfi_type

  val dec_inst = io.insts.bits.decode_signals(btb_cfi_idx)

  // determine based on the btb resp if the btb gave a wrong target or wrong cfi type
  //   based on what the decoded instructions tell you
  when (io.btb_resp.valid) {
    when (btb_cfi_type === CfiType.BRANCH && io.btb_resp.bits.taken) {
      wrong_cfi    := !dec_inst.is_br
      wrong_target := dec_inst.br_target =/= btb_target
    } .elsewhen (btb_cfi_type === CfiType.JAL) {
      wrong_cfi    := !dec_inst.is_jal
      wrong_target := dec_inst.jal_target =/= btb_target
    } .elsewhen (btb_cfi_type === CfiType.JALR) {
      wrong_cfi := !dec_inst.is_jalr
    } .otherwise {
      wrong_cfi := btb_cfi_type === CfiType.NONE && io.btb_resp.bits.taken
      when (io.insts.valid) {
        assert (btb_cfi_type =/= CfiType.NONE, "[br-checker] BTB predicted on a non-cfi type")
      }
    }
  }

  // get the PC +X to fetch from
  val nextline_pc = nextFetchStart(io.insts.bits.aligned_pc)

  // did the btb actually predict
  val btb_hit  = io.btb_resp.valid

  // was the btb wrong
  val btb_was_wrong = io.btb_resp.valid && (wrong_cfi || wrong_target || !io.insts.bits.mask(btb_cfi_idx))

  // check to see if btb predicted instruction after a jal (when it should've predicted the jal)
  //   if so then adjust for the jal
  val jal_vec  = VecInit(io.insts.bits.decode_signals.map{ sigs => sigs.is_jal })
  val jal_idx  = PriorityEncoder(jal_vec.asUInt)
  val jal_wins = jal_vec.reduce(_|_) &&
                 (!btb_hit || btb_was_wrong || (jal_idx < btb_cfi_idx) || !io.btb_resp.bits.taken)
  val  jal_target = io.insts.bits.decode_signals(jal_idx).jal_target
  val jal_is_call = io.insts.bits.decode_signals(jal_idx).is_call
  val  jal_is_ret = io.insts.bits.decode_signals(jal_idx).is_ret
  val  jal_is_rvc = io.insts.bits.decode_signals(jal_idx).is_rvc

  //-------------------------------------------------------------
  // Perform redirection

  // Redirect if:
  //    - JAL comes before BTB's cfi_idx
  //       * kill everything behind JAL -- including BTB's predinfo
  //          - BTB should have predicted the JAL
  //    - BTB was wrong
  //       * if JAL, take JAL (if valid instructions available)
  //       * if !JAL, request nextline (set all masks to valid).
  //    - No JAL, BTB correct
  //       * do nothing

  io.resp.valid := io.insts.valid && (jal_wins || btb_was_wrong)
  io.resp.bits.pc_req.addr := Mux(jal_wins, jal_target, nextline_pc)
  // mask out instructions after predicted cfi
  io.resp.bits.cfi_idx := Mux(jal_wins, jal_idx, (fetchWidth-1).U)

  if (DEBUG_BPU_PRINTF) {
    printf("BR Checker:\n")
    when (io.resp.valid) {
      printf("    Redirect -> Was:")
      when (jal_wins) {
        printf("JAL")
      } .elsewhen (btb_was_wrong) {
        printf("BTB Wrong")
      }
      printf(" TARG:0x%x CfiIdx:%d\n",
        io.resp.bits.pc_req.addr,
        io.resp.bits.cfi_idx)
    } .otherwise {
      printf("    None\n")
    }
  }
}
