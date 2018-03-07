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
//

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
//    - 2 or 3 bit counters

package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}

import freechips.rocketchip.util.Str

case class TageParameters(
   enabled: Boolean = true,
   num_tables: Int = 4,
   table_sizes: Seq[Int] = Seq(1024, 1024, 1024, 1024),
   history_lengths: Seq[Int] = Seq(5, 13, 34, 89),
   tag_sizes: Seq[Int] = Seq(11, 11, 11, 11),
   cntr_sz: Int = 3,
   ubit_sz: Int = 1)

class TageResp(
   fetch_width: Int,
   num_tables: Int,
   max_history_length: Int,
   max_index_sz: Int,
   max_tag_sz: Int,
   cntr_sz: Int,
   ubit_sz: Int
   )
   extends Bundle
{
   // which table is providing the prediction?
   val provider_hit = Bool()
   val provider_id = UInt(width = log2Ceil(num_tables))
   // which table is the alternative?
   val alt_hit = Bool()
   val alt_id = UInt(width = log2Ceil(num_tables))

   // What were the counter values and the u-bit values for each table?
   // Store all of these to avoid a RMW during update.
   val tags  = Vec(num_tables, UInt(width = max_tag_sz))
   val cntrs = Vec(num_tables, UInt(width = cntr_sz))
   val cidxs = Vec(num_tables, UInt(width = log2Ceil(fetch_width)))
   val ubits = Vec(num_tables, UInt(width = ubit_sz))

//   val alt_used = Bool() // did we instead use the alt table?

   // Only used for error checking --- recompute these during commit.
   val debug_indexes  = Vec(num_tables, UInt(width = max_index_sz))
   val debug_tags     = Vec(num_tables, UInt(width = max_tag_sz))

   override def cloneType: this.type =
      new TageResp(
         fetch_width,
         num_tables,
         max_history_length,
         max_index_sz,
         max_tag_sz,
         cntr_sz,
         ubit_sz).asInstanceOf[this.type]
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
         max_index_sz = log2Up(params.table_sizes.max),
         max_tag_sz = params.tag_sizes.max,
         cntr_sz = params.cntr_sz,
         ubit_sz = params.ubit_sz)
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
   cntr_sz: Int,
   ubit_sz: Int
   )(implicit p: Parameters)
   extends BrPredictor(
      fetch_width    = fetch_width,
      history_length = history_lengths.max)(p)
{
   val size_in_bits = (for (i <- 0 until num_tables) yield
   {
      table_sizes(i) * tag_sizes(i) + ubit_sz + cntr_sz
   }).reduce(_+_)

   println ("\tBuilding " + (size_in_bits/8/1024.0) + " kB TAGE Predictor ("
      + (size_in_bits/1024) + " Kbits) (max history length: " + history_lengths.max + " bits)")

   require (num_tables == table_sizes.size)
   require (num_tables == history_lengths.size)
   require (num_tables == tag_sizes.size)

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
      val alt_id = Wire(init=0.U)
      var found_first = false.B
      var found_second = false.B
      for (i <- num_tables-1 to 0 by -1)
      {
         when (found_first && !found_second)
         {
            alt_id := i.U
         }
         found_second = (hits(i) && found_first) | found_second
         found_first = hits(i) | found_first
      }
      assert ((PopCount(hits) > 1.U) ^ !found_second,
         "[Tage] GetAltId has a disagreement on finding a second hit.")
      (found_second, alt_id)
   }

   //------------------------------------------------------------
   //------------------------------------------------------------

   val idxs = Wire(init=Vec.fill(num_tables){0.U})
   val tags = Wire(init=Vec.fill(num_tables){0.U})

   val tables = for (i <- 0 until num_tables) yield
   {
      val table = Module(new TageTable(
         fetch_width        = fetch_width,
         id                 = i,
         num_entries        = table_sizes(i),
         history_length     = history_lengths(i),
         tag_sz             = tag_sizes(i),
         max_num_entries    = table_sizes.max,
         max_history_length = history_lengths.max,
         cntr_sz            = cntr_sz,
         ubit_sz            = ubit_sz))

      // check that the user ordered his TAGE tables properly
      if (i > 0) require(history_lengths(i) > history_lengths(i-1))

      table
   }

   val tables_io = Vec(tables.map(_.io))

   tables_io.zipWithIndex.map{ case (table, i) =>
      table.InitializeIo

      // Send prediction request. ---
      table.bp1_req.bits.index := idxs(i)
      table.bp1_req.bits.tag := tags(i)
   }


   // get prediction (priority to last table)
   val valids = tables_io.map{ _.bp2_resp.valid }
   val predictions = tables_io.map{ _.bp2_resp.bits }
   val best_prediction_valid = valids.reduce(_|_)
   val best_prediction_bits = PriorityMux(valids.reverse, predictions.reverse)

   val resp_info = Wire(new TageResp(
      fetch_width = fetch_width,
      num_tables = num_tables,
      max_history_length = history_lengths.max,
      max_index_sz = log2Up(table_sizes.max),
      max_tag_sz = tag_sizes.max,
      cntr_sz = cntr_sz,
      ubit_sz = ubit_sz))

   f2_resp.valid       := best_prediction_valid
   f2_resp.bits.takens := Mux(best_prediction_valid,
                              0.U, // generateTakensFromCfiIdx() TODO XXX best_prediction.takens
                              io.f2_bim_resp.bits.getTakens)

   resp_info.tags    := Vec(predictions.map(_.tag))
   resp_info.cntrs   := Vec(predictions.map(_.cntr))
   resp_info.cidxs   := Vec(predictions.map(_.cidx))
   resp_info.ubits   := Vec(predictions.map(_.ubit))

   resp_info.debug_indexes := idxs
   resp_info.debug_tags    := tags

   resp_info.provider_hit  := io.resp.valid
   resp_info.provider_id   := GetProviderTableId(valids)

   val (p_alt_hit, p_alt_id) = GetAlternateTableId(valids)
   resp_info.alt_hit       := p_alt_hit
   resp_info.alt_id        := p_alt_id

   f2_resp.bits.info       := resp_info.asUInt

   require (log2Ceil(num_tables) <= resp_info.provider_id.getWidth)

   //------------------------------------------------------------
   //------------------------------------------------------------
   // update predictor during commit

   val r_commit = RegNext(io.commit)
   val r_info = new TageResp(
      fetch_width = fetch_width,
      num_tables = num_tables,
      max_history_length = history_lengths.max,
      max_index_sz = log2Up(table_sizes.max),
      max_tag_sz = tag_sizes.max,
      cntr_sz = cntr_sz,
      ubit_sz = ubit_sz
   ).fromBits(r_commit.bits.info)


   val com_indexes    = 0.U // TODO XXX recompute indices, tags
   val com_tags       = 0.U // TODO XXX recompute indices, tags

   val alt_agrees     = false.B // TODO XXX.


   // Provide some randomization to the allocation process (count up to 4).
   val (rand, overflow) = Counter(io.commit.valid, 4)

   val ubit_update_wens = Wire(init = Vec.fill(num_tables) {Bool(false)})
   val ubit_update_incs = Wire(init = Vec.fill(num_tables) {Bool(false)})

   // Are we going to perform a write to the table?
   // What action are we going to perform? Allocate? Update? Or Degrade?
   val table_wens       = Wire(init = Vec.fill(num_tables) {false.B})
   val table_allocates  = Wire(init = Vec.fill(num_tables) {false.B})
   val table_updates    = Wire(init = Vec.fill(num_tables) {false.B})
   val table_degrades   = Wire(init = Vec.fill(num_tables) {false.B})

   // TODO XXX don't just check HIT, also verify cfi-idx matches.


   when (r_commit.valid)
   {
      // TODO XXX what if there's no branch in here?

      // no matter what happens, update table that made a prediction
      when (r_info.provider_hit)
      {
         table_wens(r_info.provider_id) := true.B
         table_updates(r_info.provider_id) := true.B
         //when (!alt_agrees) { ubit(provider_id) := !com_mispredict } TODO XXX
      }


      when (r_commit.bits.mispredict && (r_info.provider_id < MAX_TABLE_ID.U || !r_info.provider_hit))
      {
         // try to allocate a new entry

         // if provider not T_max, try to allocate an entry on T_k (i < k < max).
         // - only allocate one entry.
         // - a) if find an entry u_k that == 0, then allocate T_k
         // - b) ELSE decrement u_counters from Tj where (i<j<=max), or just (i<j<max).
         //    b.i) randomize r, where i<=(i+r)<k<=max, to prevent ping-ponging
         //       where new allocations simply over-write once another before the u-bit
         //       can be strengthened.


         val temp = Mux(rand === 3.U,   2.U,
                      Mux(rand === 2.U, 1.U,
                                        0.U))
         val ridx = Mux((Cat(0.U, r_info.provider_id) + temp) >= MAX_TABLE_ID.U,
                     0.U,
                     temp)


         // find lowest alloc_idx where u_bits === 0
         val can_allocates = Range(0, num_tables).map{ i =>
            r_info.ubits(i) === 0.U && // not useful
            ((i.U > (Cat(0.U, r_info.provider_id) + ridx)) || !r_info.provider_hit)
         }

         val alloc_id = PriorityEncoder(can_allocates)
         when (can_allocates.reduce(_|_))
         {
            table_wens(alloc_id) := true.B
            table_allocates(alloc_id) := true.B
         }
         .otherwise
         {
            //decrementUBits for tables[provider_id+1: T_max]
            for (i <- 0 until num_tables)
            {
               when ((i.U > r_info.provider_id) || !r_info.provider_hit)
               {
                  table_wens(i) := true.B
                  table_degrades(i) := true.B
               }
            }
         }
      }
   }

   for (i <- 0 until num_tables)
   {
      when (table_wens(i))
      {
         // construct old entry so we can overwrite parts without having to do a RMW.
         val com_entry = Wire(new TageTableEntry(fetch_width, tag_sizes.max, cntr_sz, ubit_sz))
         com_entry.tag  := r_info.tags (i)
         com_entry.cntr := r_info.cntrs(i)
         com_entry.cidx := r_info.cidxs(i)
         com_entry.ubit := r_info.ubits(i)

         tables_io(i).WriteEntry(
            com_indexes(i),
            com_entry,
            table_allocates(i),
            table_updates(i),
            table_degrades(i),
            r_commit.bits.mispredict,
            r_commit.bits.taken)
         // TODO XXX if "update", only write to u-bit (!mispredict=>ubit) if !alt_agrees.
      }
   }


   when (r_commit.valid)
   {
      assert (r_info.provider_id < num_tables.U || !r_info.provider_hit, "[Tage] provider_id is out-of-bounds.")
   }


}

