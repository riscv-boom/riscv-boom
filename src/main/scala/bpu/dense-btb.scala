//******************************************************************************
// Copyright (c) 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Dense Branch Target Buffer with RAS and BIM predictor (DenseBTB)
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
// TODO:
//    - provide way to clear/reset BTB.
//
// NOTES:
//    - Stores partial tags that results in partial resolution
//    - Stores branch offsets that reduces the storage state and explots branch locality
//    - Currently, supports only simple combinations of short and long branches
//    - BTB is allowed to be stale (Debug Program Buffer and other self-modifying code may end up here).

package boom.bpu

import Chisel._
import freechips.rocketchip.config.Parameters
import boom.common._
import boom.exu._

import freechips.rocketchip.util.Str

class DenseBTB(implicit p: Parameters) extends BoomBTB
{
   val bim = Module(new BimodalTable())
   bim.io.req := io.req
   bim.io.do_reset := false.B // TODO
   bim.io.flush := false.B // TODO
   bim.io.update := io.bim_update

   // Currently, the math below is defined assuming that the BTB has 2 banks and assumes a fetch width of 8 instructions
   // TBD: generalize this
   private val lsb_sz = log2Ceil(coreInstBytes)
   private val bank_bit = log2Ceil(fetchWidth*coreInstBytes)
   private def getBank (addr: UInt): UInt = addr(bank_bit)
   private def getIdx (addr: UInt): UInt = Cat(addr(idx_sz+lsb_sz,bank_bit+1),addr(bank_bit-1,lsb_sz))
   private def getTag (addr: UInt): UInt = addr(tag_sz+idx_sz+lsb_sz, idx_sz+lsb_sz+1)
   private def getOffset (addr: UInt): UInt = addr(offset_sz+lsb_sz-1, lsb_sz)
   private val way_idx_sz = log2Up(nWays)

   class BTBSetData extends Bundle
   {
      val tag      = UInt(width = tag_sz)
      val offset   = UInt(width = offset_sz)
      val bpd_type = BpredType()
      val cfi_type = CfiType()
      val cfi_idx  = UInt(width = log2Up(fetchWidth))
   }

   class BTBUpdateEntry extends Bundle
   {
      // true = long branch
      // false = short branch
      val br_type  = Bool()
      val entry    = new BoomBTBUpdate()
   }

   val stall = !io.req.valid
   val s0_idx = getIdx(io.req.bits.addr)(idx_sz-1,0)

   val s1_idx = RegEnable(s0_idx, !stall)

   val s1_pc  = RegEnable(io.req.bits.addr, !stall)

   // prediction

   // wire declarations for collecting response
   val s1_valid     = Wire(Bool())
   val s1_resp_bits = Wire(new BoomBTBResp)

   // used to collect each hit; used in RAS
   // FIXME: assume only short branches
   // NOTE: figure out how to handle long vs. short branches
   val hits = Wire(Vec(nWays, Bool()))

   // collect data out of the corresponding bank
   val data_out = Wire(Vec(nWays, new BTBSetData()))

   val s1_req_tag = RegEnable(getTag(io.req.bits.addr), !stall)

   // updates
   val btb_update_q = Module(new Queue(new BTBUpdateEntry, entries=num_buff_entries))

   // silently drop writes
   btb_update_q.io.enq.valid := io.btb_update.valid
   val btb_update = Wire(new BTBUpdateEntry())
   btb_update.br_type       := (getTag(io.btb_update.bits.pc) ^ getTag(io.btb_update.bits.target)).orR
   btb_update.entry         := io.btb_update.bits
   btb_update_q.io.enq.bits := btb_update

   val widx = getIdx(btb_update_q.io.deq.bits.entry.pc)

   // Global rotating counter to pick the way to write into
   // TODO: figure out some other replacement policy
   // logic for counter enable depends on late-arriving pc-addr signal!
   val next_way = Counter(btb_update_q.io.deq.fire(), nWays)._1

   // logic to know if we are to use the short entry or the large entry
   // TODO: currently only assumes a short branch
   val wmask = Wire(UInt(width=nWays))
   wmask := (1.U << next_way)

   btb_update_q.io.deq.ready := false.B
   hits := Wire(Vec(nWays, false.B))
   data_out := Wire(Vec(nWays, (new BTBSetData()).fromBits(0.U)))
   for (b <- 0 until nBanks)
   {
      val valids = RegInit(Vec(Seq.fill(nSets)(0.U(nWays.W))))
      val data   = SeqMem(nSets, Vec(nWays, UInt(width=(new BTBSetData).getWidth.W)))
      data.suggestName("btb_data_array")
      valids.suggestName("valids_array")

      val bank_vals = valids(s1_idx)
      val ren       = getBank(io.req.bits.addr).toBool === b.U
      val rout_bits = data.read(s0_idx, ren)
      val rout      = Vec(rout_bits map { x => new BTBSetData().fromBits(x) })
      val bank_hits = (bank_vals.toBools zip rout map {case(hit, data) => ren && hit && data.tag === s1_req_tag})

      when (getBank(s1_pc) === b.U)
      {
         hits     := bank_hits
         data_out := rout
      }

      val wbank = getBank(btb_update_q.io.deq.bits.entry.pc).toBool === b.U
      val wen = btb_update_q.io.deq.valid && !io.status_debug && !btb_update_q.io.deq.bits.br_type && wbank
      when (!ren && wen)
      {
         btb_update_q.io.deq.ready := true.B
         valids(widx) := valids(widx).bitSet(next_way, true.B)

         // TODO: assumes only short branches, need to handle longer branches
         val wdata = Wire(new BTBSetData())
         wdata.tag      := getTag(btb_update_q.io.deq.bits.entry.pc)
         wdata.offset   := getOffset(btb_update_q.io.deq.bits.entry.target)
         wdata.cfi_idx  := btb_update_q.io.deq.bits.entry.cfi_pc >> log2Up(coreInstBytes)
         wdata.bpd_type := btb_update_q.io.deq.bits.entry.bpd_type
         wdata.cfi_type := btb_update_q.io.deq.bits.entry.cfi_type

         data.write(widx, Vec.fill(nWays)(wdata.asUInt), wmask.toBools)
      }
   }

   s1_valid := hits.asUInt.orR && !io.flush

   // TODO: figure out a circuit for the case of multiple-hits, need to pick the earliest branch
   // currently going with the PriorityEncoder
   val hits_oh = PriorityEncoderOH(hits)
   val s1_data = Mux1H(hits_oh, data_out)

   // TODO: needs to change to handle long branches
   val s1_target   = Cat(s1_pc(vaddrBits-1,offset_sz+lsb_sz), s1_data.offset, UInt(0, lsb_sz))
   val s1_cfi_idx  = s1_data.cfi_idx
   val s1_bpd_type = s1_data.bpd_type
   val s1_cfi_type = s1_data.cfi_type

   s1_resp_bits.target   := s1_target
   s1_resp_bits.cfi_idx  := (if (fetchWidth > 1) s1_cfi_idx else 0.U)
   s1_resp_bits.bpd_type := s1_bpd_type
   s1_resp_bits.cfi_type := s1_cfi_type
   s1_resp_bits.fetch_pc := s1_pc

   if (nRAS > 0)
   {
      val ras = new RAS(nRAS, coreInstBytes)
      // Assumes only short branches...
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

   if (DEBUG_PRINTF)
   {
      printf("BTB predi (%c): hits:%x %d (PC= 0x%x, TARG= 0x%x %d) s2_BIM [%d %d 0x%x]\n",
         Mux(s1_valid, Str("V"), Str("-")), hits.asUInt, true.B, RegNext(io.req.bits.addr), s1_target, s1_cfi_type,
         bim.io.resp.valid, bim.io.resp.bits.entry_idx, bim.io.resp.bits.rowdata)
   }

   override def toString: String =
      "\n   ==Dense BTB==" +
      "\n   Sets          : " + nSets +
      "\n   Banks         : " + nBanks +
      "\n   Ways          : " + nWays +
      "\n   Tag Size      : " + tag_sz +
      "\n   Offset Size   : " + offset_sz +
      "\n   Idx high      : (%d,%d)".format(idx_sz+lsb_sz, bank_bit+1) + "\n"
      bim.toString

   override val compileOptions = chisel3.core.ExplicitCompileOptions.NotStrict.copy(explicitInvalidate = true)
}
