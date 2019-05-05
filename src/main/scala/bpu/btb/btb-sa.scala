//******************************************************************************
// Copyright (c) 2017 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Set-associative Branch Target Buffer with RAS and BIM predictor (BTB-sa)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Stages:
//   * S0 -- receive address to predict on
//   * S1 -- perform lookup
//   * S2 -- return our prediction
//
// A predicted-taken will insert 1 bubble into the pipeline.
//
// TODO:
//   - provide way to clear/reset BTB.
//
// NOTES:
//   - No compression of high-order tag bits or target bits.
//   - We store the full targets, instead of just the branch/jump offsets.
//   - Only performs partial tag matches -- must verify elsewhere that target was valid.
//   - BTB is allowed to be stale (Debug Program Buffer and other self-modifying code may end up here).

package boom.bpu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.exu._
import boom.util.{BoolToChar, CfiTypeToChars, BpdTypeToChars}

/**
 * Normal set-associative branch target buffer. Checks an incoming
 * address against the BTB, returns a target if any, then uses
 * a bi-modal table to determine whether the prediction is taken or not
 * taken.
 */
class BTBsa(implicit p: Parameters) extends BoomBTB
{
  val bim = Module(new BimodalTable())
  bim.io.req := io.req
  bim.io.do_reset := false.B // TODO
  bim.io.flush := false.B // TODO
  bim.io.update := io.bim_update

  private val lsbSz = log2Ceil(coreInstBytes)
  private def getTag (addr: UInt): UInt = addr(tagSz+idxSz+lsbSz-1, idxSz+lsbSz)
  private def getIdx (addr: UInt): UInt = addr(idxSz+lsbSz-1, lsbSz)

  /**
   * Data stored in the BTB entry
   */
  class BTBSetData extends Bundle
  {
    val target    = UInt((vaddrBits - log2Ceil(coreInstBytes)).W)
    val cfi_idx   = UInt(log2Ceil(fetchWidth).W)
    val is_rvc    = Bool()
    val edge_inst = Bool()
    val bpd_type  = BpredType()
    val cfi_type  = CfiType()
  }

  val stall = !io.req.valid

  // index into the tag/data arrays
  val s0_idx = getIdx(io.req.bits.addr)(idxSz-1,0)
  val s1_idx = RegEnable(s0_idx, !stall)

  // prediction
  val s1_valid = Wire(Bool())
  val s1_resp_bits = Wire(new BoomBTBResp)
  s1_resp_bits := DontCare
  val hits_oh = Wire(Vec(nWays, Bool()))
  val data_out = Wire(Vec(nWays, new BTBSetData()))
  val s1_req_tag = RegEnable(getTag(io.req.bits.addr), !stall)

  // updates
  val r_btb_update = Pipe(io.btb_update)
  val r_status_debug = RegNext(io.status_debug)
  val update_valid = r_btb_update.valid && !r_status_debug // align the status debug with actual btb update
  val widx = getIdx(r_btb_update.bits.pc)
  val wtag = getTag(r_btb_update.bits.pc)
  // TODO: currently a not-very-clever way to choose a replacement way
  val next_replace = Counter(r_btb_update.valid, nWays)._1
  val way_wen = UIntToOH(next_replace)

  // clear entries (e.g., multiple tag hits, which is an invalid variant)
  val clear_valid = WireInit(false.B)
  val clear_way_oh = Wire(Vec(nWays, Bool()))
  clear_way_oh.map(_ := false.B)
  val clear_idx = s1_idx

  if (DEBUG_PRINTF || DEBUG_BPU_PRINTF) {
    printf("BTB-SA:\n")
  }

  for (w <- 0 until nWays) {
    val wen = update_valid && way_wen(w)

    val valids   = RegInit(0.U(nSets.W)).suggestName(s"btb_valids_$w")
    val tags     = SyncReadMem(nSets, UInt(tagSz.W)).suggestName(s"btb_tag_array_$w")
    val data     = SyncReadMem(nSets, new BTBSetData()).suggestName(s"btb_data_array_$w")

    val is_valid = (valids >> s1_idx)(0) && RegNext(!wen)
    val rout     = data.read(s0_idx, !wen)
    val rtag     = tags.read(s0_idx, !wen)
    hits_oh(w)   := is_valid && (rtag === s1_req_tag)
    data_out(w)  := rout

    when (wen) {
      valids := valids.bitSet(widx, true.B)

      val newdata = Wire(new BTBSetData())
      newdata.target  := r_btb_update.bits.target(vaddrBits-1, log2Ceil(coreInstBytes))
      newdata.cfi_idx := r_btb_update.bits.cfi_idx
      newdata.is_rvc := r_btb_update.bits.is_rvc
      newdata.edge_inst := r_btb_update.bits.edge_inst
      newdata.bpd_type := r_btb_update.bits.bpd_type
      newdata.cfi_type := r_btb_update.bits.cfi_type

      tags(widx) := wtag
      data(widx) := newdata
    }

    // currently CLEAR will take priority (may lose updates)
    //assert(!(wen && clear_valid), "[btb-sa] both should not be high")

    // if multiple ways hit, clear the set last read
    when (clear_valid && clear_way_oh(w)) {
      printf("BTB: Cleared Idx:%d Way:%d\n",
        clear_idx,
        w.U)
      valids := valids.bitSet(clear_idx, false.B)
    }

    if (DEBUG_BPU_PRINTF) {
      when (wen) {
         printf("    Write to (Idx:%d Way:%d) <- ((PC:0x%x Tag:0x%x) TARG:0x%x)\n",
           widx,
           w.U,
           r_btb_update.bits.pc,
           wtag,
           r_btb_update.bits.target)
      }
    }

    if (DEBUG_PRINTF) {
      printf("    Write (%c): (TAG[%d][%d] <- 0x%x) (PC:0x%x TARG:0x%x)\n",
        BoolToChar(wen, 'W'),
        w.U,
        widx,
        wtag,
        r_btb_update.bits.pc,
        r_btb_update.bits.target)

      //for (set <- 0 until nSets) {
      //  printf("        BTB-ARRAY[%d][%d]: V:%c TAG:0x%x TARG:0x%x [Shifted: TAG:0x%x TARG:0x%x]\n",
      //    way.U,
      //    set.U,
      //    BoolToChar((valids >> set.U)(0), 'V'),
      //    tags.read(set.U),
      //    data.read(set.U).target,
      //    tags.read(set.U) << (idx_sz + log2Ceil(fetchWidth*coreInstBytes)).U,
      //    data.read(set.U).target << log2Ceil(coreInstBytes))
      //}
    }
  }

  // if multiple ways hit, invalidate all matched entries
  when (freechips.rocketchip.util.PopCountAtLeast(hits_oh.asUInt, 2)) {
    clear_way_oh := (hits_oh.asUInt & ~PriorityEncoderOH(hits_oh.asUInt)).asBools // Choose bottom bit then use it to not clear that entry
    //printf("DEBUG: hits_oh:b%b withPri:(b%b,%d) afterwards:b%b\n",
    //  hits_oh.asUInt,
    //  PriorityEncoderOH(hits_oh.asUInt),
    //  PriorityEncoderOH(hits_oh.asUInt),
    //  hits_oh.asUInt & ~PriorityEncoderOH(hits_oh.asUInt))
    clear_valid := true.B
  }

  // Mux out the winning hit.
  s1_valid := (PopCount(hits_oh) >= 1.U) && !io.flush
  val s1_data = PriorityMux(hits_oh, data_out) // arbitrarily choose data out on multiple hit
  val s1_target = Cat(s1_data.target, 0.U(log2Ceil(coreInstBytes).W))
  val s1_cfi_idx = s1_data.cfi_idx
  val s1_is_rvc = s1_data.is_rvc
  val s1_edge_inst = s1_data.edge_inst
  val s1_bpd_type = s1_data.bpd_type
  val s1_cfi_type = s1_data.cfi_type

  s1_resp_bits.target := s1_target
  s1_resp_bits.cfi_idx := (if (fetchWidth > 1) s1_cfi_idx else 0.U)
  s1_resp_bits.is_rvc := s1_is_rvc
  s1_resp_bits.edge_inst := s1_edge_inst
  s1_resp_bits.bpd_type := s1_bpd_type
  s1_resp_bits.cfi_type := s1_cfi_type

  val s1_pc = RegEnable(io.req.bits.addr, !stall)
  s1_resp_bits.fetch_pc := s1_pc

  // modify the RAS state and give RAS predictions
  //   only gives values on BTB hit since you need the BTB prediction to determine the type of instruction
  if (nRAS > 0) {
    val ras = new RAS(nRAS, coreInstBytes)
    val doPeek = (hits_oh zip data_out map {case(hit, data) => hit && BpredType.isReturn(data.bpd_type)}).reduce(_||_)
    val isEmpty = if (rasCheckForEmpty) ras.isEmpty else false.B
    when (!isEmpty && doPeek) {
      s1_resp_bits.target := ras.peek
    }

    // update the RAS and bypass if available
    when (io.ras_update.valid && !io.status_debug) {
      when (io.ras_update.bits.is_call) {
        ras.push(io.ras_update.bits.return_addr)
        if (bypassCalls) {
          // bypassing couples ras_update.valid to the critical path.
          when (doPeek) {
            s1_resp_bits.target := io.ras_update.bits.return_addr
          }
        }
      } .elsewhen (io.ras_update.bits.is_ret) {// only pop if BTB hit!
        ras.pop()
      }
    }
  }

  //************************************************
  // Output.

  io.resp.valid := RegNext(s1_valid)
  io.resp.bits := RegNext(s1_resp_bits)

  io.resp.bits.bim_resp := bim.io.resp

  // Does the BIM think we should take it?
  io.resp.bits.taken :=
    (bim.io.resp.valid && bim.io.resp.bits.isTaken(io.resp.bits.cfi_idx)) ||
    RegNext(BpredType.isAlwaysTaken(s1_bpd_type))
  io.resp.bits.mask := Cat((1.U << ~Mux(io.resp.bits.taken, ~io.resp.bits.cfi_idx, 0.U))-1.U, 1.U)

  //************************************************
  // Debug.

  if (DEBUG_PRINTF) {
    val cfiTypeStrs = CfiTypeToChars(io.resp.bits.cfi_type)
    printf("    Predicted (%c): Hits:b%b (PC:0x%x -> TARG:0x%x) CFI:%c%c%c%c\n",
      BoolToChar(io.resp.valid, 'V'),
      RegNext(hits_oh.asUInt),
      io.resp.bits.fetch_pc,
      io.resp.bits.target,
      cfiTypeStrs(0),
      cfiTypeStrs(1),
      cfiTypeStrs(2),
      cfiTypeStrs(3))
    printf("    BIM: Predicted (%c): Idx:%d Row:0x%x\n",
      BoolToChar(bim.io.resp.valid, 'V'),
      bim.io.resp.bits.entry_idx,
      bim.io.resp.bits.rowdata)
  }

  if (DEBUG_BPU_PRINTF) {
    val cfiTypeStrs = CfiTypeToChars(io.resp.bits.cfi_type)
    val bpdTypeStrs = BpdTypeToChars(io.resp.bits.bpd_type)
    printf("    Resp: V:%c Hits:b%b T:%c PC:0x%x TARG:0x%x RVC:%c EDGE:%c CfiType:%c%c%c%c BrType:%c%c%c%c\n",
      BoolToChar(io.resp.valid, 'V'),
      RegNext(hits_oh.asUInt),
      BoolToChar(io.resp.bits.taken, 'T'),
      io.resp.bits.fetch_pc,
      io.resp.bits.target,
      BoolToChar(io.resp.bits.is_rvc, 'C'),
      BoolToChar(io.resp.bits.edge_inst, 'E'),
      cfiTypeStrs(0),
      cfiTypeStrs(1),
      cfiTypeStrs(2),
      cfiTypeStrs(3),
      bpdTypeStrs(0),
      bpdTypeStrs(1),
      bpdTypeStrs(2),
      bpdTypeStrs(3))
    printf("    BimResp: V:%c EntryIdx:%d Row:0x%x\n",
      BoolToChar(bim.io.resp.valid, 'V'),
      bim.io.resp.bits.entry_idx,
      bim.io.resp.bits.rowdata)
  }

  override def toString: String =
    "   ==BTB-SA==" +
    "\n   Sets          : " + nSets +
    "\n   Ways          : " + nWays +
    "\n   Tag Size      : " + tagSz +
    "\n\n" +
    bim.toString
}
