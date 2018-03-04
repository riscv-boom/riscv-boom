//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// TAGE-based Branch Predictor
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2016 Feb 26

// Terminology:
//    - provider
//       The table that provides the prediction (typically the "best" prediction).
//    - alternate
//       The table that would have provided the prediction if the provider had
//       missed.

// TODO:
//    - alt-pred tracking (choosing between +2 tables, sometimes using alt pred if u is low)
//    - u-bit handling, clearing (count failed allocations)
//    - lower required parameters, arguments to bundles and objects
//    stats we want to track:
//       - how often entries are used
//       - how often allocation fails (%)
//       - how often we reset the useful-ness bits
// SCHEMES
//    - u-bit incremented only if alt-pred available?
//    - change when we use alt-pred instead of first-pred
//    - 1 or 2-bit u-bits? (almost certainly 2-bits)
//    - 2 or 3 bit counters

package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}

import freechips.rocketchip.util.Str

case class TageParameters(
   enabled: Boolean = true,
   num_tables: Int = 4,
   table_sizes: Seq[Int] = Seq(4096,4096,2048,2048),
   history_lengths: Seq[Int] = Seq(5,17,44,130),
   tag_sizes: Seq[Int] = Seq(10,10,10,12),
   ubit_sz: Int = 1)

class TageResp(
   fetch_width: Int,
   num_tables: Int,
   max_history_length: Int,
   max_index_sz: Int,
   max_tag_sz: Int)
   extends Bundle
{
   val provider_hit = Bool() // did tage make a prediction?
   val provider_id = UInt(width = 5) // which table is providing the prediction?
   val provider_predicted_takens = UInt(width = fetch_width)
   val alt_hit = Bool()  // an alternate table made a prediction too
   val alt_id = UInt(width = 5)  // which table is the alternative?
   val alt_predicted_takens = UInt(width = fetch_width)

   val indexes  = Vec(num_tables, UInt(width = max_index_sz)) // needed to update predictor at Commit
   val tags     = Vec(num_tables, UInt(width = max_tag_sz))   // needed to update predictor at Commit
   val evict_bits = Vec(num_tables, Bool())                   // needed to update predictor on branch misprediction

   val idx_csr  = Vec(num_tables, UInt(width = max_index_sz)) // needed to perform rollback
   val tag_csr1 = Vec(num_tables, UInt(width = max_tag_sz))   // needed to perform rollback
   val tag_csr2 = Vec(num_tables, UInt(width = max_tag_sz-1)) // needed to perform rollback


   val debug_history_ptr = UInt(width = max_history_length) // stored in snapshots (dealloc after Execute)

   override def cloneType: this.type =
      new TageResp(
         fetch_width,
         num_tables,
         max_history_length,
         max_index_sz,
         max_tag_sz).asInstanceOf[this.type]
}

// provide information to the BpdResp bundle how many bits a TageResp needs
object TageBrPredictor
{
   def GetRespInfoSize(p: Parameters, fetchWidth: Int): Int =
   {
      val boomParams: BoomCoreParams = p(freechips.rocketchip.tile.TileKey).core.asInstanceOf[BoomCoreParams]
      val params = boomParams.tage.get
      val dummy = new TageResp(
         fetch_width = fetchWidth,
         num_tables = params.num_tables,
         max_history_length = params.history_lengths.max,
         max_index_sz = log2Ceil(params.table_sizes.max),
         max_tag_sz = params.tag_sizes.max
         )
      dummy.getWidth
   }
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------

class TageBrPredictor(
   fetch_width: Int,
   num_tables: Int,
   table_sizes: Seq[Int],
   history_lengths: Seq[Int],
   tag_sizes: Seq[Int],
   ubit_sz: Int
   )(implicit p: Parameters)
   extends BrPredictor(
      fetch_width    = fetch_width,
      history_length = history_lengths.max)(p)
{
   val counter_sz = 2
   val size_in_bits = (for (i <- 0 until num_tables) yield
   {
      val entry_sz_in_bits = tag_sizes(i) + ubit_sz + (counter_sz*fetch_width)
      table_sizes(i) * entry_sz_in_bits
   }).reduce(_+_)

   println ("\tBuilding " + (size_in_bits/8/1024.0) + " kB TAGE Predictor ("
      + (size_in_bits/1024) + " Kbits) (max history length: " + history_lengths.max + " bits)")
   require (num_tables == table_sizes.size)
   require (num_tables == history_lengths.size)
   require (num_tables == tag_sizes.size)
   // require (log2Ceil(num_tables) <= TageResp.provider_id.getWidth()) TODO implement this check
   require (coreInstBytes == 4)

   //------------------------------------------------------------
   //------------------------------------------------------------

   private val MAX_TABLE_ID = num_tables-1

   //------------------------------------------------------------
   //------------------------------------------------------------

   def GetProviderTableId(hits:IndexedSeq[Bool]): UInt =
   {
      // return the id of the highest table with a hit
      PriorityMux(hits.reverse, (num_tables-1 to 0 by -1).map(UInt(_)))
   }

   def GetAlternateTableId(hits:IndexedSeq[Bool]): (Bool, UInt) =
   {
      // return the id of the 2nd highest table with a hit
      // also returns whether a 2nd hit was found (PopCount(hits) > 1)
      val alt_id = Wire(init=UInt(0))
      var found_first = Bool(false)
      var found_second = Bool(false)
      for (i <- num_tables-1 to 0 by -1)
      {
         when (found_first && !found_second)
         {
            alt_id := UInt(i)
         }
         found_second = (hits(i) && found_first) | found_second
         found_first = hits(i) | found_first
      }
      assert ((PopCount(hits) > UInt(1)) ^ !found_second,
         "[Tage] GetAltId has a disagreement on finding a second hit.")
      (found_second, alt_id)
   }

   //------------------------------------------------------------
   //------------------------------------------------------------

   val stall = !io.resp.ready

   //------------------------------------------------------------
   //------------------------------------------------------------

   val tables = for (i <- 0 until num_tables) yield
   {
      val table = Module(new TageTable(
         fetch_width        = fetch_width,
         id                 = i,
         num_tables         = num_tables,
         num_entries        = table_sizes(i),
         history_length     = history_lengths(i),
         tag_sz             = tag_sizes(i),
         max_num_entries    = table_sizes.max,
         max_history_length = history_lengths.max,
         max_tag_sz         = tag_sizes.max,
         counter_sz         = counter_sz,
         ubit_sz            = ubit_sz))

      // check that the user ordered his TAGE tables properly
      if (i > 0) require(history_lengths(i) > history_lengths(i-1))

      table
   }

   val tables_io = Vec(tables.map(_.io))

   tables_io.zipWithIndex.map{ case (table, i) =>
      table.InitializeIo()

      // Send prediction request. ---
      table.if_req_pc := 0.U // TODO XXX pass in index, tag // io.req_pc
   }


   // get prediction (priority to last table)
   val valids = tables_io.map{ _.bp2_resp.valid }
   val predictions = tables_io.map{ _.bp2_resp.bits }
   tables_io.map{ _.bp2_resp.ready := io.resp.ready }
   val best_prediction_valid = valids.reduce(_|_)
   val best_prediction_bits = PriorityMux(valids.reverse, predictions.reverse)

   val resp_info = Wire(new TageResp(
      fetch_width = fetch_width,
      num_tables = num_tables,
      max_history_length = history_lengths.max,
      max_index_sz = log2Ceil(table_sizes.max),
      max_tag_sz = tag_sizes.max))

   io.resp.valid       := best_prediction_valid
   io.resp.bits.takens := best_prediction_bits.takens
   resp_info.indexes   := Vec(predictions.map(_.index))
   resp_info.tags      := Vec(predictions.map(_.tag))
   resp_info.idx_csr   := Vec(predictions.map(_.idx_csr))
   resp_info.tag_csr1  := Vec(predictions.map(_.tag_csr1))
   resp_info.tag_csr2  := Vec(predictions.map(_.tag_csr2))

   resp_info.provider_hit := io.resp.valid
   resp_info.provider_id := GetProviderTableId(valids)
   resp_info.provider_predicted_takens := best_prediction_bits.takens

   val (p_alt_hit, p_alt_id) = GetAlternateTableId(valids)
   resp_info.alt_hit := p_alt_hit
   resp_info.alt_id  := p_alt_id
   resp_info.alt_predicted_takens := Vec(predictions.map(_.takens))(p_alt_id)

   io.resp.bits.info := resp_info.asUInt

   require (log2Ceil(num_tables) <= resp_info.provider_id.getWidth)

   //------------------------------------------------------------
   //------------------------------------------------------------
   // update predictor during commit

   // Commit&Update takes 3 cycles.
   //    - First cycle: begin read of state (u-bits).
   //    - Second cycle: compute (some) updates.
   //    - Second cycle: perform updates.
   // Specifically, the u-bits are "read-do-stuff-write", so spreading
   // across three-cycles is a requirement:
   //    (address setup, read, compute/write).

   //-------------------------------------------------------------
   // Cycle 0 and 1 - read info

   val info = new TageResp(
      fetch_width = fetch_width,
      num_tables = num_tables,
      max_history_length = history_lengths.max,
      max_index_sz = log2Ceil(table_sizes.max),
      max_tag_sz = tag_sizes.max
   ).fromBits(io.commit.bits.info)

   val executed = 0.U // TODO XXX commit.bits.ctrl.executed.asUInt

// TODO XXX
//   when (commit.valid && commit.bits.ctrl.executed.reduce(_|_))
//   {
//      assert (info.provider_id < UInt(num_tables) || !info.provider_hit, "[Tage] provider_id is out-of-bounds.")
//   }

   // TODO verify this behavior/logic is correct (re: toBits/Vec conversion)
   val s2_alt_agrees = RegNext(RegNext(
      info.alt_hit && (info.provider_predicted_takens & executed) === (info.alt_predicted_takens & executed)))

   val s2_ubits_notuseful = Range(0, num_tables).map{ i =>
      !(tables_io(i).GetUsefulness(info.indexes(i), log2Ceil(table_sizes(i))))
   }

   //-------------------------------------------------------------
   // Track ubit degrade flush timer.

   val degrade_counter = freechips.rocketchip.util.WideCounter(20, false.B) // TODO XXX commit.valid && commit.bits.ctrl.executed.reduce(_|_))
   val do_degrade = degrade_counter === UInt(1<<19)
   when (do_degrade)
   {
      degrade_counter := UInt(0)
      for (i <- 0 until num_tables)
      {
         tables_io(i).DegradeUsefulness()
      }
   }

   //-------------------------------------------------------------
   // Cycle 1 - perform state changes

   val s2_commit      = RegNext(RegNext(io.commit))
   val s2_info        = RegNext(RegNext(info))
   val s2_provider_id = RegNext(RegNext(info.provider_id))
   val s2_takens      = 0.U // TODO XXX RegNext(RegNext(commit.bits.ctrl.taken.asUInt))
   val s2_correct     = true.B // TODO XXX RegNext(RegNext(!commit.bits.ctrl.mispredicted.reduce(_|_)))
   val s2_executed    = 0.U // TODO XXX RegNext(RegNext(commit.bits.ctrl.executed.asUInt))


   // provide some randomization to the allocation process
   val rand = Reg(init=UInt(0,2))
   rand := rand + UInt(1)

   val ubit_update_wens = Wire(init = Vec.fill(num_tables) {Bool(false)})
   val ubit_update_incs = Wire(init = Vec.fill(num_tables) {Bool(false)})

   when (s2_commit.valid) // TODO XXX && s2_commit.bits.ctrl.executed.reduce(_|_))
   {
      // TODO XXX what if there's no branch in here?

      // no matter what happens, update table that made a prediction
      when (s2_info.provider_hit)
      {
         tables_io(s2_provider_id).UpdateCounters(s2_info.indexes(s2_provider_id), s2_executed, s2_takens, !s2_correct)
         when (!s2_alt_agrees)
         {
            ubit_update_wens(s2_provider_id) := Bool(true)
            ubit_update_incs(s2_provider_id) := s2_correct
         }
      }


      when (!s2_correct && (s2_provider_id < UInt(MAX_TABLE_ID) || !s2_info.provider_hit))
      {
         // try to allocate a new entry

         // if provider not T_max, try to allocate an entry on T_k (i < k < max).
         // - only allocate one entry.
         // - a) if find an entry u_k that == 0, then allocate T_k
         // - b) ELSE decrement u_counters from Tj where (i<j<=max), or just (i<j<max).
         //    b.i) randomize r, where i<=(i+r)<k<=max, to prevent ping-ponging
         //       where new allocations simply over-write once another before the u-bit
         //       can be strengthened.


         val temp = Mux(rand === UInt(3), UInt(2),
                      Mux(rand === UInt(2), UInt(1),
                                            UInt(0)))
         val ridx = Mux((Cat(UInt(0), s2_provider_id) + temp) >= UInt(MAX_TABLE_ID),
                     UInt(0),
                     temp)


         // find lowest alloc_idx where u_bits === 0
         val can_allocates = Range(0, num_tables).map{ i =>
            s2_ubits_notuseful(i) &&
            ((UInt(i) > (Cat(UInt(0), s2_provider_id) + ridx)) || !s2_info.provider_hit)
         }

         val alloc_id = PriorityEncoder(can_allocates)
         when (can_allocates.reduce(_|_))
         {
            tables_io(alloc_id).AllocateNewEntry(
               s2_info.indexes(alloc_id),
               s2_info.tags(alloc_id),
               s2_executed,
               s2_takens,
               0.U, //s2_info.debug_br_pc,
               s2_info.debug_history_ptr)
         }
         .otherwise
         {
            //decrementUBits for tables[provider_id+1: T_max]
            for (i <- 0 until num_tables)
            {
               when ((UInt(i) > s2_provider_id) || !s2_info.provider_hit)
               {
                  ubit_update_wens(i) := Bool(true)
                  ubit_update_incs(i) := Bool(false)
               }
            }
         }
      }
   }

   for (i <- 0 until num_tables)
   {
      when (ubit_update_wens(i))
      {
         tables_io(i).UpdateUsefulness(s2_info.indexes(i), inc=ubit_update_incs(i))
//         TODO assert (s2_commit.valid && s2_commit.bits.ctrl.executed.reduce(_|_),
//            "[tage] updating ubits when not committing.")
      }
   }
}

