//******************************************************************************
// Copyright (c) 2017, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Set-associative Branch Target Buffer (BTB-sa)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Stages:
//    * S0 -- receive address to predict on
//    * S1 -- perform lookup
//    * S2 -- return our prediction
//
// A predicted-taken will insert 1 bubble into the pipeline.
//
// NOTES:
//    - No compression of high-order tag bits or target bits.
//    - We store the full targets, instead of just the branch/jump offsets (requires adder).
//    - Only performs partial tag matches -- must verify elsewhere that target was valid.
//    - BTB is allowed to be stale (Debug Program Buffer and other self-modifying code may end up here).

package boom

import Chisel._
import freechips.rocketchip.config.Parameters

import freechips.rocketchip.util.Str

case class BTBsaParameters(
  nSets: Int = 128,
  nWays: Int = 4,
  tagSz: Int = 20,
  nRAS : Int = 16,
  // Extra knobs.
  bypassCalls: Boolean = true,
  rasCheckForEmpty: Boolean = true
)

trait HasBTBsaParameters extends HasBoomCoreParameters
{
   val btbParams = boomParams.btb
   val nSets = btbParams.nSets
   val nWays = btbParams.nWays
   val tag_sz = btbParams.tagSz
   val idx_sz = log2Ceil(nSets)
   val nRAS = btbParams.nRAS
   val bypassCalls = btbParams.bypassCalls
   val rasCheckForEmpty = btbParams.rasCheckForEmpty
}

// Which predictor should we listen to?
object BpredType
{
   def SZ = 3
   def apply() = UInt(width = SZ)
   def branch = 0.U
   def jump = 1.U
   def ret =  (2+1).U
   def call = (4+1).U

   def isAlwaysTaken(typ: UInt): Bool = typ(0)
   def isReturn(typ: UInt): Bool = typ(1)
   def isCall(typ: UInt): Bool = typ(2)
   def isJump(typ: UInt): Bool = typ === jump
   def isBranch(typ: UInt): Bool = typ === branch
}

abstract class BTBsaBundle(implicit val p: Parameters) extends freechips.rocketchip.util.ParameterizedBundle()(p)
  with HasBTBsaParameters


//  - "cfi_idx" is the low-order PC bits of the predicted branch (after
//     shifting off the lowest log(inst_bytes) bits off).
//  - "mask" provides a mask of valid instructions (instructions are
//     masked off by the predicted taken branch from the BTB).
class BTBsaResp(implicit p: Parameters) extends BTBsaBundle()(p)
{
   val taken     = Bool()   // is BTB predicting a taken cfi?
   val target    = UInt(width = vaddrBits) // what target are we predicting?
   val mask      = UInt(width = fetchWidth) // mask of valid instructions.
   val cfi_idx   = UInt(width = log2Up(fetchWidth)) // where is cfi we are predicting?
   val bpd_type  = BpredType() // which predictor should we use?
   val cfi_type  = CfiType()  // what type of instruction is this?
   val bim_resp  = new BimResp // response from BIM -- it needs it back later for updating.
   val fetch_pc  = UInt(width = vaddrBits) // the PC we're predicting on (start of the fetch packet).
}

class BimResp(implicit p: Parameters) extends BTBsaBundle()(p)
{
  val value = UInt(width = 2) // save the old value -- needed for updating entry.
  val entry_idx = UInt(width = idx_sz) // what entry in the set is the prediction coming from?
  val way_idx = UInt(width = log2Up(nWays)) // which way did we come from?

  def isTaken = value(0)
}


// BTB update occurs during branch resolution (and only on a mispredict that's taken).
//  - "pc" is what future fetch PCs will tag match against.
//  - "cfi_pc" is the PC of the branch instruction.
class BTBsaUpdate(implicit p: Parameters) extends BTBsaBundle()(p)
{
   val pc = UInt(width = vaddrBits)
   val target = UInt(width = vaddrBits)
   val taken = Bool()
   val cfi_pc = UInt(width = vaddrBits)
   val bpd_type = BpredType()
   val cfi_type = CfiType()
}

class BimUpdate(implicit p: Parameters) extends BTBsaBundle()(p)
{
  val taken = Bool()
  val bim_resp = new BimResp
}

class RasUpdate(implicit p: Parameters) extends BTBsaBundle()(p)
{
   val is_call = Bool()
   val is_ret = Bool()
   val return_addr = UInt(width = vaddrBits)
}


// BIM contains table of 2-bit counters (bimodal predictor).
// Each way of the BTB will have its own BIM predictor.
// The BIM only predicts and updates when there is a BTB hit.
//    - update speculatively when a branch is predicted (and BTB was a hit for that branch).
//    - updated when a branch is mispredicted in Execute (and BTB was a hit for that branch).
//    - initialize counter when a branch is mispredicted in Execute and BTB is updated with new entry.
class BIM(bim_entries: Int, way_idx: Int)(implicit val p: Parameters) extends HasBoomCoreParameters
{
   val idx_sz = log2Up(bim_entries)

   def read(index: UInt): BimResp =
   {
      val res = Wire(new BimResp)
      // read-data valid on the next cycle
      res.value := table.read(index(idx_sz-1, 0))
      res.way_idx := UInt(way_idx)
      res.entry_idx := 0.U // unused -- set externally to save a flip-flop.
      res
   }

   def update(index: UInt, value: UInt, taken: Bool): Unit =
   {
      // LSB gives the taken/not-taken prediction (p-bit).
      // MSB gives "how strongly" biased the counter is.
      table(index(idx_sz-1, 0)) := Cat(taken, (value(1) & value(0)) | ((value(1) | value(0)) & taken))
   }

   private val table = SeqMem(bim_entries, UInt(width = 2))
}


class RAS(nras: Int, coreInstBytes: Int)
{
   def push(addr: UInt): Unit =
   {
      when (count < nras.U) { count := count + 1.U }
      val nextPos = Mux(Bool(isPow2(nras)) || pos < UInt(nras-1), pos+1.U, 0.U)
      stack(nextPos) := addr >> log2Up(coreInstBytes)
      pos := nextPos
   }
   def peek: UInt = Cat(stack(pos), UInt(0, log2Up(coreInstBytes)))
   def pop(): Unit = when (!isEmpty)
   {
      count := count - 1.U
      pos := Mux(Bool(isPow2(nras)) || pos > 0.U, pos-1.U, UInt(nras-1))
   }
   //def clear(): Unit = count := UInt(0)
   def isEmpty: Bool = count === UInt(0)

   private val count = Reg(UInt(width = log2Up(nras+1)))
   private val pos = Reg(UInt(width = log2Up(nras)))
   private val stack = Reg(Vec(nras, UInt()))
}


class PCReq(implicit p: Parameters) extends BTBsaBundle()(p)
{
   val addr = UInt(width = vaddrBitsExtended)
}


// Set-associative branch target buffer.
class BTBsa(implicit p: Parameters) extends BoomModule()(p) with HasBTBsaParameters
{
   val io = IO(new Bundle
   {
      // req.valid is false if stalling (aka, we won't read and use BTB results, on cycle S1).
      // req.bits.addr is available on cycle S0.
      // resp is expected on cycle S1.
      val req = Valid(new PCReq).flip
      val resp = Valid(new BTBsaResp)
      // the PC we're predicting on (start of the fetch packet).
      // Pass this to the BPD.
      val s1_pc  = UInt(width = vaddrBits)
      // RAS prediction gets pipelined and handled in next stage (optionally)
      val ras_resp = Valid(new BTBsaResp)
      // If there's an icmiss, the Frontend replays the s2_pc as s0_pc (even though req.valid is low),
      // so don't stall and begin predicting s0_pc.
      val icmiss = Bool(INPUT)

      // supress S1/upcoming S2 valids.
      val flush = Bool(INPUT)

      // BTB update comes in during branch resolution (Execute Stage). Yes, that's out-of-order.
      val btb_update = Valid(new BTBsaUpdate).flip
      // BIM is updated speculatively in Frontend (and during Execute if mispredicted).
      val bim_update = Valid(new BimUpdate).flip
      // RAS is updated in Frontend, in-order.
      val ras_update = Valid(new RasUpdate).flip

      // HACK: prevent BTB updating/predicting during program load.
      // Easier to diff against spike which doesn't run debug mode.
      val status_debug = Bool(INPUT)
   })

   private val lsb_sz = log2Up(coreInstBytes)
   private def getTag (addr: UInt): UInt = addr(tag_sz+idx_sz+lsb_sz-1, idx_sz+lsb_sz)
   private def getIdx (addr: UInt): UInt = addr(idx_sz+lsb_sz-1, lsb_sz)


   class BTBSetData extends Bundle
   {
      val target = UInt(width = vaddrBits - log2Up(coreInstBytes))
      val cfi_idx = UInt(width = log2Up(fetchWidth))
      val bpd_type = BpredType()
      val cfi_type = CfiType()
   }


   val stall = !io.req.valid && !io.icmiss
   val s0_idx = Wire(UInt(width=idx_sz))
   val last_idx = RegNext(s0_idx)
   val new_idx = getIdx(io.req.bits.addr)
   s0_idx := Mux(stall, last_idx, new_idx)
   val s1_idx = RegNext(s0_idx)

   // prediction
   val s1_valid = Wire(Bool())
   val s1_resp_bits = Wire(new BTBsaResp)
   val hits_oh = Wire(Vec(nWays, Bool()))
   val data_out = Wire(Vec(nWays, new BTBSetData()))
   val bim_out = Wire(Vec(nWays, new BimResp()))
   val s1_req_tag = RegEnable(getTag(io.req.bits.addr), !stall)

   // updates
   val r_btb_update = Pipe(io.btb_update)
   val r_bim_update = Pipe(io.bim_update)
   val update_valid = r_btb_update.valid && !io.status_debug
   val widx = getIdx(r_btb_update.bits.pc)
   val wtag = getTag(r_btb_update.bits.pc)
   // TODO: currently a not-very-clever way to choose a replacement way.
   // Also, doesn't search for invalid ways!
   val next_replace = Counter(r_btb_update.valid, nWays)._1
   val way_wen = UIntToOH(next_replace)

   // clear entries (e.g., multiple tag hits, which is an invalid variant)
   val clear_valid = Wire(init=false.B)
   val clear_idx = s1_idx


   for (w <- 0 until nWays)
   {
      val wen = update_valid && way_wen(w)

      val valids   = Reg(init = UInt(0, nSets))
      val tags     = SeqMem(nSets, UInt(width = tag_sz))
      val data     = SeqMem(nSets, new BTBSetData())
      val bim      = new BIM(nSets, way_idx = w)

      tags.suggestName("btb_tag_array")
      data.suggestName("btb_data_array")

      val is_valid = (valids >> s1_idx)(0) && RegNext(!wen)
      val rout     = data.read(s0_idx, !wen)
      val rtag     = tags.read(s0_idx, !wen)
      hits_oh(w)   := is_valid && (rtag === s1_req_tag)
      data_out(w)  := rout
      bim_out(w)   := bim.read(s0_idx)

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

         // Initialize BIM counter to weakly taken.
         bim.update(widx, value = 3.U, taken = false.B)
      }
      .elsewhen (r_bim_update.valid && r_bim_update.bits.bim_resp.way_idx === UInt(w))
      {
         // update bim on a misprediction
         bim.update(r_bim_update.bits.bim_resp.entry_idx, r_bim_update.bits.bim_resp.value, r_bim_update.bits.taken)
      }
      .elsewhen (RegNext(s1_valid &&
         s1_resp_bits.bim_resp.way_idx === w.U &&
         BpredType.isBranch(s1_resp_bits.bpd_type)))
      {
         // speculatively update bim
         bim.update(
            RegNext(s1_resp_bits.bim_resp.entry_idx),
            RegNext(s1_resp_bits.bim_resp.value),
            RegNext(s1_resp_bits.taken))
      }

      when (clear_valid)
      {
         valids := valids.bitSet(clear_idx, false.B)
      }

      if (DEBUG_PRINTF)
      {
         //printf("BTB write (%c): %d 0x%x (PC= 0x%x, TARG= 0x%x) way=%d C=%d\n", Mux(wen, Str("w"), Str("-")), widx,
         //wtag, r_btb_update.bits.pc, r_btb_update.bits.target, UInt(w), clear_valid)
         //for (i <- 0 until nSets)
         //{
         //   printf("    [%d] %d tag=0x%x targ=0x%x [0x%x 0x%x]\n", UInt(i), (valids >> UInt(i))(0),
         //   tags.read(UInt(i)),
         //   data.read(UInt(i)).target,
         //   tags.read(UInt(i)) << UInt(idx_sz + log2Up(fetchWidth*coreInstBytes)),
         //   data.read(UInt(i)).target << log2Up(coreInstBytes)
         //   )
         //}
      }
   }

   // Zap entries if multiple hits.
   when (freechips.rocketchip.util.PopCountAtLeast(hits_oh.asUInt, 2))
   {
      clear_valid := true.B
   }


   // Mux out the winning hit.
   s1_valid := PopCount(hits_oh) === UInt(1) && !io.flush
   val s1_data = Mux1H(hits_oh, data_out)
   val s1_bim_resp = Mux1H(hits_oh, bim_out)
   val s1_target = Cat(s1_data.target, UInt(0, log2Up(coreInstBytes)))
   val s1_cfi_idx = s1_data.cfi_idx
   val s1_bpd_type = s1_data.bpd_type
   val s1_cfi_type = s1_data.cfi_type


   s1_resp_bits.target := s1_target
   s1_resp_bits.taken := (if (enableBIM) s1_bim_resp.isTaken else true.B) || (BpredType.isAlwaysTaken(s1_bpd_type))
   s1_resp_bits.cfi_idx := (if (fetchWidth > 1) s1_cfi_idx else UInt(0))
   s1_resp_bits.bpd_type := s1_bpd_type
   s1_resp_bits.cfi_type := s1_cfi_type
   s1_resp_bits.mask := Cat((UInt(1) << ~Mux(s1_resp_bits.taken, ~s1_resp_bits.cfi_idx, UInt(0)))-UInt(1), UInt(1))
   s1_resp_bits.bim_resp := s1_bim_resp
   s1_resp_bits.bim_resp.entry_idx  := s1_idx

   val s0_pc = Wire(UInt(width=vaddrBits))
   val last_pc = RegNext(s0_pc)
   s0_pc := Mux(stall, last_pc, io.req.bits.addr)
   val s1_pc = RegNext(s0_pc)
   s1_resp_bits.fetch_pc := s1_pc
   io.s1_pc := s1_pc

   if (nRAS > 0)
   {
      val ras = new RAS(nRAS, coreInstBytes)
      val doPeek = (hits_oh zip data_out map {case(hit, d) => hit && BpredType.isReturn(d.bpd_type)}).reduce(_||_)
      val isEmpty = if (rasCheckForEmpty) ras.isEmpty else false.B
      when (!isEmpty && doPeek)
      {
         s1_resp_bits.target := ras.peek
      }

      when (io.ras_update.valid)
      {
         when (io.ras_update.bits.is_call)
         {
            ras.push(io.ras_update.bits.return_addr)
            if (bypassCalls)
            {
               // bypassing couples ras_update.valid to the critical path.
               when (doPeek)
               {
                  s1_resp_bits.target := io.ras_update.bits.return_addr
               }
            }
         }
         .elsewhen (io.ras_update.bits.is_ret) // only pop if BTB hit!
         {
            ras.pop()
         }
      }
   }


   //************************************************
   // Output.

   io.resp.valid := false.B // [TODO XXX renable BTB once we have sorted out request/I$ interaction] RegNext(s1_valid)
   io.resp.bits := RegNext(s1_resp_bits)


   //************************************************
   // Debug.

   if (false) //DEBUG_PRINTF)
   {
      printf("BTB predi (%c): hits:%x %d (PC= 0x%x, TARG= 0x%x %d) BIM [%d, %d]\n",
         Mux(s1_valid, Str("V"), Str("-")), hits_oh.asUInt, true.B, RegNext(io.req.bits.addr), s1_target, s1_cfi_type,
         s1_resp_bits.bim_resp.way_idx, s1_resp_bits.bim_resp.entry_idx)
   }
}

