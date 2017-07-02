//******************************************************************************
// Copyright (c) 2017, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Set-associative Branch Target Buffer (BTB-sa)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// TODO:
//    - compress high-order tag bits, target bits?
//    - only store offsets, not full targets? (requires adder)
//    - support RAS

package boom

import Chisel._
import config.Parameters

import util.Str

case class BTBsaParameters(
  nSets: Int = 1024,
  nWays: Int = 2
)

trait HasBTBsaParameters extends HasBoomCoreParameters
{
   val btbParams = boomParams.btb
   val nSets = btbParams.nSets
   val nWays = btbParams.nWays
   val setidx_sz = log2Ceil(nSets)
   val tag_sz = 38
   val idx_sz = log2Up(nSets)
}

//trait HasBTBParameters extends HasCoreParameters {
//  val btbParams = tileParams.btb.getOrElse(BTBParams(nEntries = 0))
//  val matchBits = pgIdxBits max log2Ceil(p(coreplex.CacheBlockBytes) * tileParams.icache.get.nSets)
//  val entries = btbParams.nEntries
//  val nRAS = btbParams.nRAS
//}

abstract class BTBsaBundle(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasBTBsaParameters

// BTB update occurs during branch resolution (and only on a mispredict that's taken).
//  - "pc" is what future fetch PCs will tag match against.
//  - "cfi_pc" is the PC of the branch instruction.
class BTBsaUpdate(implicit p: Parameters) extends BTBsaBundle()(p) {
//  val prediction = Valid(new BTBResp)
   val pc = UInt(width = vaddrBits)
   val target = UInt(width = vaddrBits)
   val taken = Bool()
//  val isValid = Bool()
//  val isReturn = Bool()
//   val is_jump = Bool() // jal or jr; don't listen to bimodal
//   val is_ret  = Bool() // is return; 
   val cfi_pc = UInt(width = vaddrBits)
	val cfi_type = CFIType()
}

//  - "bridx" is the low-order PC bits of the predicted branch (after
//     shifting off the lowest log(inst_bytes) bits off).
//  - "mask" provides a mask of valid instructions (instructions are
//     masked off by the predicted taken branch from the BTB).
class BTBsaResp(implicit p: Parameters) extends BTBsaBundle()(p) 
{
   val taken       = Bool()   // is BTB predicting a taken cfi?
   val target      = UInt(width = vaddrBits) // what target are we predicting?
   val mask        = Bits(width = fetchWidth) // mask of valid instructions.
   val cfi_idx     = Bits(width = log2Up(fetchWidth)) // where is cfi we are predicting?
   val cfi_type    = CFIType()
//   val entry_idx   = UInt(width = setidx_sz) // what entry in the set is the prediction coming from?
}


class PCReq(implicit p: Parameters) extends BTBsaBundle()(p) 
{
   val addr = UInt(width = vaddrBitsExtended)
}

// set-associative branch target buffer
class BTBsa(implicit p: Parameters) extends BoomModule()(p) with HasBTBsaParameters
{
   val io = new Bundle 
   {
      val req = Valid(new PCReq).flip
      val resp = Valid(new BTBsaResp)
      val btb_update = Valid(new BTBsaUpdate).flip
   }

//   val isValid = Reg(init = UInt(0, entries))
//   val isReturn = Reg(UInt(width = entries))
//   val isJump = Reg(UInt(width = entries))
//   val brIdx = Reg(Vec(entries, UInt(width=log2Up(fetchWidth))))

//   private def tagMatch(addr: UInt, pgMatch: UInt) = {
//      val idxMatch = idxs.map(_ === addr(matchBits-1, log2Up(coreInstBytes))).asUInt
//      val idxPageMatch = idxPagesOH.map(_ & pgMatch).map(_.orR).asUInt
//      idxMatch & idxPageMatch & isValid
//   }

   private def getTag (addr: UInt): UInt = (addr >> UInt(idx_sz + log2Up(fetchWidth*coreInstBytes)))(tag_sz-1,0)
   private def getIdx (addr: UInt): UInt = (addr >> UInt(log2Up(fetchWidth*coreInstBytes)))(idx_sz-1,0)
 
   class BTBSetMetaData extends Bundle
   {
      val target = UInt(width = vaddrBits - log2Up(coreInstBytes))
      val cfi_idx = UInt(width = log2Up(fetchWidth))
      val cfi_type = CFIType()
   }
 
   val hits         = Wire(Vec(nWays, Bool()))
   val metadata_out = Wire(Vec(nWays, new BTBSetMetaData()))

   val ren = RegNext(io.req.valid)
   val s0_idx = getIdx(io.req.bits.addr)
   val s1_req_tag = RegEnable(getTag(io.req.bits.addr), io.req.valid)

   val r_btb_update = Pipe(io.btb_update)
   val update_valid = r_btb_update.valid 
   val widx = getIdx(r_btb_update.bits.pc)

   // a not-very-clever way to replace a way
   val next_replace = Counter(r_btb_update.valid, nWays)._1
   val way_wen = UIntToOH(next_replace)

   for (w <- 0 until nWays)
   {
      val wen = update_valid && way_wen(w)

      val valids     = Reg(init = UInt(0, nSets))
      val tags       = SeqMem(nSets, UInt(width = tag_sz))
      val metadata   = SeqMem(nSets, new BTBSetMetaData())

      val is_valid    = RegNext((valids >> s0_idx)(0)) && !wen && ren
      val rout        = metadata.read(s0_idx, ren && !wen)
      val rtag        = tags.read(s0_idx, ren && !wen)
      hits(w)         := is_valid && (rtag === s1_req_tag)
		metadata_out(w) := rout

      when (wen)
      {
         val wtag = getTag(r_btb_update.bits.pc) 
         val widx = getIdx(r_btb_update.bits.pc) 
         valids := valids.bitSet(widx, Bool(true))

         val newmeta = Wire(new BTBSetMetaData())
         newmeta.target  := r_btb_update.bits.target(vaddrBits-1, log2Up(coreInstBytes))
         newmeta.cfi_idx := r_btb_update.bits.cfi_pc >> log2Up(coreInstBytes)
         newmeta.cfi_type := r_btb_update.bits.cfi_type
         
         tags(widx)     := wtag
         metadata(widx) := newmeta
      }
//      printf("BTB write (%c): %d 0x%x (PC= 0x%x, TARG= 0x%x) way=%d\n", Mux(wen, Str("w"), Str("-")), widx, wtag, r_btb_update.bits.pc, r_btb_update.bits.target, UInt(w))

//      for (i <- 0 until nSets)
//      {
//         printf("    [%d] %d tag=0x%x targ=0x%x\n", UInt(i), (valids >> UInt(i))(0), tags.read(UInt(i)),
//         metadata.read(UInt(i)).target)
//      }
   }

   //TODO zap entries if multiple hits
//   assert (PopCount(hits_out) <= UInt(1) && ren, "[btbsa] more than 1 valid hit.")
//    when (PopCountAtLeast(hits, 2)) { 
//         isValid := isValid & ~hits      
//    }                                 
   
   // mux out the winning hit
   val s1_valid = PopCount(hits) === UInt(1)
	val s1_metadata = Mux1H(hits, metadata_out)
   val s1_target = Cat(s1_metadata.target, UInt(0, log2Up(coreInstBytes)))
   val s1_cfi_idx = s1_metadata.cfi_idx
   val s1_cfi_type = s1_metadata.cfi_type

//   when (s1_valid)
//   {
//      printf("BTB predi: hits:%x %d (PC= 0x%x, TARG= 0x%x %d)\n", 
//         hits.asUInt, Bool(true), RegNext(io.req.bits.addr), s1_target, s1_cfi_type)
//   }

//   val update_target = io.req.bits.addr

   io.resp.valid := Bool(false) // s1_valid XXX, does this respect taken?
   io.resp.bits.taken := Bool(true) // TODO XXX add bimodal predictor; always take if JUMP
   io.resp.bits.target := s1_target
   io.resp.bits.cfi_idx := (if (fetchWidth > 1) s1_cfi_idx else UInt(0))
   io.resp.bits.cfi_type := s1_cfi_type
   io.resp.bits.mask := Cat((UInt(1) << ~Mux(io.resp.bits.taken, ~io.resp.bits.cfi_idx, UInt(0)))-UInt(1), UInt(1))
//  io.resp.bits.entry := 



}
