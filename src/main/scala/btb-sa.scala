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
//    - Support RAS.
//    - Add bimodal predictor.
//    - compress high-order tag bits, target bits?
//    - only store offsets, not full targets? (requires adder).

package boom

import Chisel._
import config.Parameters

import util.Str

case class BTBsaParameters(
  nSets: Int = 4,
  nWays: Int = 2,
  tagSz: Int = 10
)

trait HasBTBsaParameters extends HasBoomCoreParameters
{
   val btbParams = boomParams.btb
   val nSets = btbParams.nSets
   val nWays = btbParams.nWays
   val tag_sz = btbParams.tagSz
   val idx_sz = log2Ceil(nSets)
//  val nRAS = btbParams.nRAS
//  val nBIM = btbParams.nBIM
}


// Which predictor should we listen to?
object BpredType
{
   def SZ = 2
   def apply() = UInt(width = SZ)
   def branch = 0.U
   def jump = 1.U
   def call = 2.U
   def ret = 3.U
}


abstract class BTBsaBundle(implicit val p: Parameters) extends util.ParameterizedBundle()(p)
  with HasBTBsaParameters

// BTB update occurs during branch resolution (and only on a mispredict that's taken).
//  - "pc" is what future fetch PCs will tag match against.
//  - "cfi_pc" is the PC of the branch instruction.
class BTBsaUpdate(implicit p: Parameters) extends BTBsaBundle()(p)
{
//  val prediction = Valid(new BTBResp)
   val pc = UInt(width = vaddrBits)
   val target = UInt(width = vaddrBits)
   val taken = Bool()
//  val isValid = Bool()
   val cfi_pc = UInt(width = vaddrBits)
	val bpd_type = BpredType()
	val cfi_type = CFIType()
}

//  - "bridx" is the low-order PC bits of the predicted branch (after
//     shifting off the lowest log(inst_bytes) bits off).
//  - "mask" provides a mask of valid instructions (instructions are
//     masked off by the predicted taken branch from the BTB).
class BTBsaResp(implicit p: Parameters) extends BTBsaBundle()(p)
{
   val taken     = Bool()   // is BTB predicting a taken cfi?
   val target    = UInt(width = vaddrBits) // what target are we predicting?
   val mask      = UInt(width = fetchWidth) // mask of valid instructions.
   val cfi_idx   = UInt(width = log2Up(fetchWidth)) // where is cfi we are predicting?
   val bpd_type  = BpredType()
   val cfi_type  = CFIType()
   val entry_idx = UInt(width = idx_sz) // what entry in the set is the prediction coming from?
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
      // req.valid is false if stalling (aka, we won't read and use BTB results, on cycle S1).
      // req.bits.addr is available on cycle S0. 
      // Resp is expected on cycle S1.
      // TODO when adding BHT, don't enact state-change until req.valid
      val req = Valid(new PCReq).flip
      val resp = Valid(new BTBsaResp)
      // Update comes in during branch resolution (Execute Stage). Yes, that's out-of-order.
      val btb_update = Valid(new BTBsaUpdate).flip
      
      // HACK: prevent BTB predicting during program load.
      // Easier to diff against spike which doesn't run debug mode.
      val status_debug = Bool(INPUT)
   }

   private def getTag (addr: UInt): UInt = (addr >> UInt(idx_sz + log2Up(fetchWidth*coreInstBytes)))(tag_sz-1,0)
   private def getIdx (addr: UInt): UInt = (addr >> UInt(log2Up(fetchWidth*coreInstBytes)))(idx_sz-1,0)

   class BTBSetData extends Bundle
   {
      val target = UInt(width = vaddrBits - log2Up(coreInstBytes))
      val cfi_idx = UInt(width = log2Up(fetchWidth))
      val bpd_type = BpredType()
      val cfi_type = CFIType()
   }


   val stall = !io.req.valid
   val s0_idx = Wire(UInt(width=idx_sz))
   val last_idx = RegNext(s0_idx)
   val new_idx = getIdx(io.req.bits.addr)
   s0_idx := Mux(stall, last_idx, new_idx)
 
   // prediction
   val hits_oh = Wire(Vec(nWays, Bool()))
   val data_out = Wire(Vec(nWays, new BTBSetData()))
   val s1_req_tag = RegEnable(getTag(io.req.bits.addr), !stall)

   // updates
   val r_btb_update = Pipe(io.btb_update)
   val update_valid = r_btb_update.valid && !io.status_debug
   val widx = getIdx(r_btb_update.bits.pc)
   // TODO: currently a not-very-clever way to choose a replacement way.
   val next_replace = Counter(r_btb_update.valid, nWays)._1
   val way_wen = UIntToOH(next_replace)

   // clear entries (e.g., multiple tag hits, which is an invalid variant)
   val clear_valid = Wire(init=false.B)
   val clear_idx = RegNext(s0_idx)

   for (w <- 0 until nWays)
   {
      val wen = update_valid && way_wen(w)

      val valids   = Reg(init = UInt(0, nSets))
      val tags     = SeqMem(nSets, UInt(width = tag_sz))
      val data     = SeqMem(nSets, new BTBSetData())

      val is_valid = RegNext((valids >> s0_idx)(0) && !wen)
      val rout     = data.read(s0_idx, !wen)
      val rtag     = tags.read(s0_idx, !wen)
      hits_oh(w)   := is_valid && (rtag === s1_req_tag)
		data_out(w)  := rout

      val wtag = getTag(r_btb_update.bits.pc)
      val widx = getIdx(r_btb_update.bits.pc)
      when (wen)
      {
         valids := valids.bitSet(widx, true.B)

         val newdata = Wire(new BTBSetData())
         newdata.target  := r_btb_update.bits.target(vaddrBits-1, log2Up(coreInstBytes))
         newdata.cfi_idx := r_btb_update.bits.cfi_pc >> log2Up(coreInstBytes)
         newdata.bpd_type := r_btb_update.bits.bpd_type
         newdata.cfi_type := r_btb_update.bits.cfi_type

         tags(widx) := wtag
         data(widx) := newdata
      }

      when (clear_valid)
      {
         valids := valids.bitSet(clear_idx, false.B)
      }

      if (DEBUG_PRINTF)
      {
         printf("BTB write (%c): %d 0x%x (PC= 0x%x, TARG= 0x%x) way=%d C=%d\n", Mux(wen, Str("w"), Str("-")), widx,
         wtag, r_btb_update.bits.pc, r_btb_update.bits.target, UInt(w), clear_valid)

         for (i <- 0 until nSets)
         {
            printf("    [%d] %d tag=0x%x targ=0x%x [0x%x 0x%x]\n", UInt(i), (valids >> UInt(i))(0),
            tags.read(UInt(i)),
            data.read(UInt(i)).target,
            tags.read(UInt(i)) << UInt(idx_sz + log2Up(fetchWidth*coreInstBytes)),
            data.read(UInt(i)).target << log2Up(coreInstBytes)
            )
         }
      }
   }

   // Zap entries if multiple hits.
   when (util.PopCountAtLeast(hits_oh.asUInt, 2))
   {
      clear_valid := true.B
   }

   // Mux out the winning hit.
   val s1_valid = PopCount(hits_oh) === UInt(1)
	val s1_data = Mux1H(hits_oh, data_out)
   val s1_target = Cat(s1_data.target, UInt(0, log2Up(coreInstBytes)))
   val s1_cfi_idx = s1_data.cfi_idx
   val s1_bpd_type = s1_data.bpd_type
   val s1_cfi_type = s1_data.cfi_type

   if (DEBUG_PRINTF)
   {
      printf("BTB predi (%c): hits:%x %d (PC= 0x%x, TARG= 0x%x %d)\n",
         Mux(s1_valid, Str("V"), Str("-")), hits_oh.asUInt, true.B, RegNext(io.req.bits.addr), s1_target, s1_cfi_type)
   }

   io.resp.valid := s1_valid && !io.status_debug
   io.resp.bits.target := s1_target
   io.resp.bits.taken := true.B || (s1_bpd_type === BpredType.jump) // TODO XXX add bimodal predictor
   io.resp.bits.cfi_idx := (if (fetchWidth > 1) s1_cfi_idx else UInt(0))
   io.resp.bits.bpd_type := s1_bpd_type
   io.resp.bits.cfi_type := s1_cfi_type
   io.resp.bits.mask := Cat((UInt(1) << ~Mux(io.resp.bits.taken, ~io.resp.bits.cfi_idx, UInt(0)))-UInt(1), UInt(1))

   // Debugging Signals
   //val s0_addr = Wire(UInt())
   //val last_addr = RegNext(s0_addr)
   //s0_addr := Mux(stall, last_addr, io.req.bits.addr)
   //val s1_addr = RegNext(s0_addr)
   //assert (s1_req_tag === getTag(s0_addr)

}

