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
//    - Stores partial tags that results in partial resolution and reduces state
//    - Stores branch offsets which reduces the storage state by exploiting branch locality
//    - Currently, assumes 4 ways and limited entry types
//    - BTB is allowed to be stale (Debug Program Buffer and other self-modifying code may end up here).

package boom.bpu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import boom.common._
import boom.exu._

import freechips.rocketchip.util._

import scala.math.{ceil,min}

//------------------------------------------------------------------------------
// DenseBTB
//------------------------------------------------------------------------------

class DenseBTB(implicit p: Parameters) extends BoomBTB
{
   private val lsb_sz = log2Ceil(coreInstBytes)
   private val bank_bit = log2Ceil(fetchWidth*coreInstBytes)
   private val way_idx_sz = log2Ceil(nWays)
   private val branch_levels = {
      val bitsToEncode = (vaddrBits - lsb_sz)
      val levelsRequired = ceil( (bitsToEncode - offset_sz).toFloat/(tag_sz + offset_sz) ).toInt + 1
      min(levelsRequired, nWays-1)
   }
   private val blevel_sz = if (branch_levels > 1) log2Ceil(branch_levels) else 1

   private def getBank (addr: UInt): UInt = addr(bank_bit)
   private def getIdx (addr: UInt): UInt = Cat(addr(idx_sz+lsb_sz,bank_bit+1),addr(bank_bit-1,lsb_sz))
   private def getTag (addr: UInt): UInt = addr(tag_sz+idx_sz+lsb_sz, idx_sz+lsb_sz+1)
   private def getOffset (addr: UInt): UInt = addr(offset_sz+lsb_sz-1, lsb_sz)

   class BTBSetData extends Bundle
   {
      val tag      = UInt(tag_sz.W)
      val offset   = UInt(offset_sz.W)
      val bpd_type = BpredType()
      val cfi_type = CfiType()
      val cfi_idx  = UInt(log2Ceil(fetchWidth).W)
   }

   class BTBUpdateQueueEntry extends Bundle
   {
      val level  = UInt(blevel_sz.W)
      val update = new BoomBTBUpdate()
   }


   private def getBTBUpdateQueueEntry(btb_update: BoomBTBUpdate): BTBUpdateQueueEntry = {
      val entry = Wire(new BTBUpdateQueueEntry())
      entry.update := btb_update

      val overflow = (0 until branch_levels).map { l =>
         if (vaddrBits-1 > (l*(tag_sz+offset_sz)+offset_sz+lsb_sz)) {
            val update_bits = btb_update.pc(vaddrBits-1, l*(tag_sz+offset_sz)+offset_sz+lsb_sz)
            val target_bits = btb_update.target(vaddrBits-1, l*(tag_sz+offset_sz)+offset_sz+lsb_sz)
            (update_bits ^ target_bits).orR
         } else {
            false.B
         }
      }

      entry.level := 0.U
      for (i <- 0 until branch_levels) {
         when (overflow(i)) {
            entry.level := (i+1).U
         }
      }

      entry
   }

   // TODO: Generalize this logic to handle writing to mixed type entries based on the number of ways; Currently, the
   // logic below assumes number of ways to be 4 and only supports writing the dense branches biased toward the lower ways
   require(nWays == 4)
   private def getBankWriteData(next_way: UInt, btb_q_entry: BTBUpdateQueueEntry) = {
      val wdata = WireInit(VecInit(Seq.fill(nWays){(0.U).asTypeOf(new BTBSetData())}))
      val wmask = WireInit(0.U(nWays.W))

      val level = btb_q_entry.level
      when (level === 0.U) {
         for (i <- 0 until nWays) {
            wdata(i).tag      := getTag(btb_q_entry.update.pc)
            wdata(i).offset   := btb_q_entry.update.target(offset_sz+lsb_sz-1,lsb_sz)
            wdata(i).cfi_idx  := btb_q_entry.update.cfi_pc >> log2Ceil(coreInstBytes)
            wdata(i).bpd_type := btb_q_entry.update.bpd_type
            wdata(i).cfi_type := btb_q_entry.update.cfi_type
         }
         wmask := (1.U << next_way)
      }

      when (level === 1.U) {
         wdata(1).tag      := getTag(btb_q_entry.update.pc)
         wdata(1).offset   := btb_q_entry.update.target(min(vaddrBits-1, tag_sz+2*offset_sz+lsb_sz-1), tag_sz+offset_sz+lsb_sz)
         wdata(1).cfi_idx  := btb_q_entry.update.cfi_pc >> log2Ceil(coreInstBytes)
         wdata(1).bpd_type := btb_q_entry.update.bpd_type
         wdata(1).cfi_type := btb_q_entry.update.cfi_type
         wdata(0).tag      := btb_q_entry.update.target(tag_sz+offset_sz+lsb_sz-1, offset_sz+lsb_sz)
         wdata(0).offset   := btb_q_entry.update.target(offset_sz+lsb_sz-1, lsb_sz)
         wmask             := 3.U
      }

      if (branch_levels > 2) {
         when (level === 2.U) {
            for (i <- 0 until 1) {
               wdata(i).tag    := btb_q_entry.update.target((i+1)*(tag_sz+offset_sz)+lsb_sz-1, i*tag_sz+(i+1)*offset_sz+lsb_sz)
               wdata(i).offset := btb_q_entry.update.target(i*tag_sz+(i+1)*offset_sz+lsb_sz-1, i*(tag_sz+offset_sz)+lsb_sz)
            }
            wdata(2).tag      := getTag(btb_q_entry.update.pc)
            wdata(2).offset   := btb_q_entry.update.target(min(vaddrBits-1, 2*tag_sz + 3*offset_sz+lsb_sz-1), 2*(tag_sz+offset_sz)+lsb_sz)
            wdata(2).cfi_idx  := btb_q_entry.update.cfi_pc >> log2Ceil(coreInstBytes)
            wdata(2).bpd_type := btb_q_entry.update.bpd_type
            wdata(2).cfi_type := btb_q_entry.update.cfi_type
            wmask             := 7.U
         }
      }

      (wmask, wdata)
   }

   // bim

   val bim = Module(new BimodalTable())
   bim.io.req := io.req
   bim.io.do_reset := false.B // TODO
   bim.io.flush := false.B // TODO
   bim.io.update := io.bim_update

   val stall = !io.req.valid
   val s0_idx = getIdx(io.req.bits.addr)(idx_sz-1,0)

   val s1_idx = RegEnable(s0_idx, !stall)

   val s1_pc  = RegEnable(io.req.bits.addr, !stall)

   // prediction

   // wire declarations for collecting response
   val s1_valid     = Wire(Bool())
   val s1_resp_bits = Wire(new BoomBTBResp)
   s1_resp_bits := DontCare //Overridden later

   // used to collect each hit; used in RAS
   val hits        = Wire(Vec(nWays, Bool()))
   val blevels_vec = Wire(Vec(nWays, UInt(blevel_sz.W)))

   // collect data out of the corresponding bank
   val data_out = Wire(Vec(nWays, new BTBSetData()))

   val s1_req_tag = RegEnable(getTag(io.req.bits.addr), !stall)

   // updates
   val btb_update_q = Module(new Queue(new BTBUpdateQueueEntry, entries=num_buff_entries))

   // silently drop writes
   btb_update_q.io.enq.valid := io.btb_update.valid && !io.status_debug
   btb_update_q.io.enq.bits  := getBTBUpdateQueueEntry( io.btb_update.bits )

   val widx = getIdx(btb_update_q.io.deq.bits.update.pc)

   // TODO: figure out some other replacement policy
   // logic for counter enable depends on late-arriving pc-addr signal!
   // Global rotating counter to pick the way to write into
   val next_way = Counter(btb_update_q.io.deq.fire(), nWays)._1

   btb_update_q.io.deq.ready := false.B
   for (b <- 0 until nBanks)
   {
      val valids  = RegInit(VecInit(Seq.fill(nSets)(0.U(nWays.W))))
      val blevels = RegInit(VecInit(Seq.fill(nSets)(0.U((nWays*blevel_sz).W))))
      val data    = SyncReadMem(nSets, Vec(nWays, UInt((new BTBSetData).getWidth.W)))
      data.suggestName("btb_data_array")
      valids.suggestName("valids_array")

      val bank_vals = valids(s1_idx)
      val ren       = getBank(io.req.bits.addr) === b.U
      val rout_bits = data.read(s0_idx, ren)
      val rout      = VecInit(rout_bits map { x => x.asTypeOf(new BTBSetData()) })
      val bank_hits = (bank_vals.toBools zip rout map {case(hit, data) => hit && data.tag === s1_req_tag})

      if (b == 0) {
         hits      := bank_hits
         blevels_vec := VecInit(blevels(s1_idx).grouped(blevel_sz))
         data_out  := rout
      } else {
         when (getBank(s1_pc) === b.U) {
            hits        := bank_hits
            blevels_vec := VecInit(blevels(s1_idx).grouped(blevel_sz))
            data_out  := rout
         }
      }

      val wen    = btb_update_q.io.deq.valid && !io.status_debug && (getBank(btb_update_q.io.deq.bits.update.pc) === b.U)
      val bmask  = if (blevel_sz > 1) ((1.U << blevel_sz) - 1.U) else 1.U
      when (!ren && wen)
      {
         btb_update_q.io.deq.ready := true.B
         val (wmask, wdata) = getBankWriteData(next_way, btb_update_q.io.deq.bits)
         val wdata_bits = VecInit(wdata map { x => x.asUInt })
         data.write(widx, wdata_bits, wmask.toBools)

         when (btb_update_q.io.deq.bits.level === 0.U) {
            valids(widx)  := valids(widx).bitSet(next_way, true.B)
            blevels(widx) := blevels(widx) & ~(bmask << (blevel_sz.U*next_way)) | (btb_update_q.io.deq.bits.level << (blevel_sz.U*next_way))
         } .elsewhen (btb_update_q.io.deq.bits.level === 1.U) {
            // change only the lower two bits of valid
            valids(widx)  := (valids(widx) & 12.U) | 2.U
            blevels(widx) := blevels(widx) & ~(bmask << blevel_sz) | (1.U << blevel_sz)
         } .elsewhen (btb_update_q.io.deq.bits.level === 2.U) {
            // change only the lower three bits of valid
            valids(widx)  := (valids(widx) & 8.U) | 4.U
            blevels(widx) := blevels(widx) & ~(bmask << (2*blevel_sz).U) | (2.U << (blevel_sz.U*next_way))
         }
      }
   }

   // TODO: need a better circuit for this!
   // currently, we create a one-hot matrix for all the cfi_idx's that hit that is combined with the response from bim
   // and use the result to get a cfi_idx based on a priority-encoding. We finally take the result of the selected
   // cfi_idx to figure out which way has this to select the data!

   val cfi_oh = ((0 until nWays).map {
      i => Mux(hits(i), UIntToOH(data_out(i.U).cfi_idx, fetchWidth), 0.U(fetchWidth.W))
   }.reduce(_ | _)) & bim.io.resp.bits.getTakens

   val sel_cfi_idx = PriorityEncoder(cfi_oh)

   val data_sel = WireInit(0.U(way_idx_sz.W))
   val hits_oh  = WireInit(VecInit(Seq.fill(nWays){ false.B }))
   for (i <- 0 until nWays) {
      when (data_out(i).cfi_idx === sel_cfi_idx && hits(i) && bim.io.resp.valid) {
         data_sel   := i.U
         hits_oh(i) := true.B
      }
   }

   s1_valid := hits(data_sel) && !io.flush

   s1_resp_bits.fetch_pc := s1_pc

   // TODO: Generalize the logic to read out; Currently, uses the same assumptions as in getBankWriteData
   val blevel = blevels_vec(data_sel)
   when (blevel === 0.U) {
      s1_resp_bits.target   := Cat(s1_pc(vaddrBits-1,offset_sz+lsb_sz), data_out(data_sel).offset, 0.U(lsb_sz.W))
      s1_resp_bits.cfi_idx  := (if (fetchWidth > 1) data_out(data_sel).cfi_idx else 0.U)
      s1_resp_bits.bpd_type := data_out(data_sel).bpd_type
      s1_resp_bits.cfi_type := data_out(data_sel).cfi_type
   }

   when (blevel === 1.U) {
      s1_resp_bits.cfi_idx  := (if (fetchWidth > 1) data_out(1).cfi_idx else 0.U)
      s1_resp_bits.bpd_type := data_out(1).bpd_type
      s1_resp_bits.cfi_type := data_out(1).cfi_type
      val resp_target = if ((vaddrBits-lsb_sz) <= tag_sz+2*offset_sz+lsb_sz) {
         Cat(
            data_out(1).offset((vaddrBits-1)-(tag_sz+offset_sz+lsb_sz), 0),
            data_out(0).tag,
            data_out(0).offset,
            0.U(lsb_sz.W)
         )
      } else {
         Cat(
            s1_pc(vaddrBits-1,tag_sz+2*offset_sz+lsb_sz),
            data_out(1).offset,
            data_out(0).tag,
            data_out(0).offset,
            0.U(lsb_sz.W)
         )
      }
      s1_resp_bits.target := resp_target
   }

   if (branch_levels > 2) {
      when (blevel === 2.U) {
         s1_resp_bits.cfi_idx  := (if (fetchWidth > 1) data_out(2).cfi_idx else 0.U)
         s1_resp_bits.bpd_type := data_out(2).bpd_type
         s1_resp_bits.cfi_type := data_out(2).cfi_type
         val resp_target = if ((vaddrBits-lsb_sz) <= 2*tag_sz+3*offset_sz+lsb_sz) {
            Cat(
               data_out(2).offset((vaddrBits-1)-(2*(tag_sz+offset_sz)+lsb_sz), 0),
               data_out(1).tag,
               data_out(1).offset,
               data_out(0).tag,
               data_out(0).offset,
               0.U(lsb_sz.W)
            )
         } else {
            Cat(
               s1_pc(vaddrBits-1,2*tag_sz+3*offset_sz+lsb_sz),
               data_out(2).offset,
               data_out(1).tag,
               data_out(1).offset,
               data_out(0).tag,
               data_out(0).offset,
               0.U(lsb_sz.W)
            )
         }
         s1_resp_bits.target := resp_target
      }
   }

   if (nRAS > 0)
   {
      val ras = new RAS(nRAS, coreInstBytes)
      // TODO: assumes only short branches...need to verify this
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
      RegNext(BpredType.isAlwaysTaken(s1_resp_bits.bpd_type))
   io.resp.bits.mask := Cat((1.U << ~Mux(io.resp.bits.taken, ~io.resp.bits.cfi_idx, 0.U))-1.U, 1.U)


   //************************************************
   // Debug.

   if (DEBUG_PRINTF)
   {
      printf("BTB predi (%c): hits:%x %d (PC= 0x%x, TARG= 0x%x %d) s2_BIM [%d %d 0x%x]\n",
         Mux(s1_valid, Str("V"), Str("-")), hits.asUInt, true.B, RegNext(io.req.bits.addr), s1_resp_bits.target, s1_resp_bits.cfi_type,
         bim.io.resp.valid, bim.io.resp.bits.entry_idx, bim.io.resp.bits.rowdata)
   }

   override def toString: String =
      "\n   ==Dense BTB==" +
      "\n   Sets          : " + nSets +
      "\n   Banks         : " + nBanks +
      "\n   Ways          : " + nWays +
      "\n   Branch Levels : " + branch_levels +
      "\n   Tag Size      : " + tag_sz +
      "\n   Offset Size   : " + offset_sz + "\n" +
      bim.toString

   override val compileOptions = chisel3.core.ExplicitCompileOptions.NotStrict.copy(explicitInvalidate = true)
}
