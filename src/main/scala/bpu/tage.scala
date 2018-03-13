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
// SCHEMES
//    - u-bit incremented only if alt-pred available?

package boom

import Chisel._
import chisel3.core.withReset
import freechips.rocketchip.config.{Parameters, Field}

import freechips.rocketchip.util.Str

case class TageParameters(
   enabled: Boolean = true,
   num_tables: Int = 4,
   table_sizes: Seq[Int] = Seq(1024, 1024, 1024, 1024),
   history_lengths: Seq[Int] = Seq(27, 45, 63, 90),
   tag_sizes: Seq[Int] = Seq(9, 9, 9, 9),
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
   // Utility Functions

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

   // Input: Take in a cfi index and prediction counter.
   // Output: generate a one-hot encoded mask. Place a 1 in the cidx
   // position if the counter predicts taken, otherwise return a mask
   // of all zeroes.
   private def GetPredictionOH(cidx: UInt, cntr: UInt): UInt =
   {
      val mask_oh = Wire(UInt(width=fetch_width.W))
      // Check high-order bit for prediction.
      val taken = cntr(cntr_sz-1)
      mask_oh := UIntToOH(cidx) & Fill(fetch_width, taken)
      mask_oh
   }

   private def IdxHash(addr: UInt, hist: UInt, hlen: Int, idx_sz: Int): UInt =
   {
      val idx = Cat(Fold(hist, idx_sz, hlen), addr(4)) ^ (addr >> 5)
      idx
   }

   private def TagHash(addr: UInt, a: UInt, b: UInt): UInt =
   {
      a ^ (b >> 2)
   }

   private def TagHashTerribad(addr: UInt, hist: UInt, hlen: Int, idx_sz: Int): UInt =
   {
      //val tag = Fold(hist, idx_sz, hlen) ^ addr
      val tag = ((addr >> 4.U) & 0xf.U) | ((hist & 0xf.U) << 4.U)
      tag
   }


   //------------------------------------------------------------
   //------------------------------------------------------------

   val bp1_idxs = Wire(Vec(num_tables, UInt()))
   val bp1_tags = Wire(Vec(num_tables, UInt()))

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


   // perform index hash
   tables_io.zipWithIndex.map{ case (table, i) =>
      bp1_idxs(i) := IdxHash(r_f1_fetchpc, r_f1_history, history_lengths(i), log2Ceil(table_sizes(i)))
   }

   tables_io.zipWithIndex.map{ case (table, i) =>
      table.InitializeIo

      // perform tag hash
      val n = num_tables
      bp1_tags(i) := TagHash(r_f1_fetchpc, bp1_idxs((i+1) % n), bp1_idxs((i+2) % n))

      // Send prediction request. ---
      table.bp1_req.valid := f1_valid
      table.bp1_req.bits.index := bp1_idxs(i)
      table.bp1_req.bits.tag := bp1_tags(i)

      table.do_reset := false.B // TODO
   }


   // Buffer all of the table responses into a queue.
   // Match the other ElasticRegs in the FrontEnd.
   val q_f3_resps = for (i <- 0 until num_tables) yield
   {
      val q_resp = withReset(reset || io.fe_clear || io.f4_redirect)
       {Module(new ElasticReg(Valid(new TageTableResp(fetch_width, tag_sizes.max, cntr_sz, ubit_sz))))}

      q_resp.io.enq.valid := io.f2_valid
      q_resp.io.enq.bits := tables_io(i).bp2_resp
      q_resp.io.deq.ready := io.resp.ready

      assert (q_resp.io.enq.ready === !io.f2_stall)
      assert (q_resp.io.deq.valid === q_f3_history.io.deq.valid)

      q_resp
   }

   // get predictions.
   val f3_tag_hits    = q_f3_resps.map{ q => q.io.deq.valid && q.io.deq.bits.valid }
   val f3_predictions = q_f3_resps.map{ q => q.io.deq.bits.bits }

//      f3_tag_hits(i) && io.f3_is_br(f3_predictions(i).cidx)

   // Construct a fetchWidth * numTables hit matrix,
   // where hits(i)(w) describes if table i has a tag and cidx hit
   // for cidx==w.
   // Only counts as a hit if there is a tag-hit AND there is a
   // branch at cidx==w.
   val f3_hits_matrix = for (i <- 0 until num_tables) yield
   {
      // For each table, return a one-hot bit-mask if there's a tag-hit AND cfi-idx hit.
      io.f3_is_br.asUInt & Mux(f3_tag_hits(i), UIntToOH(f3_predictions(i).cidx), 0.U)
   }

   // Vector/bit-mask of taken/not-taken predictions.
   val f3_takens = Wire(init = Vec.fill(fetch_width) { false.B })
   // Vector of best predictors (one for each cfi index).
   val f3_best_hits = Wire(init = Vec.fill(fetch_width) { false.B })
   val f3_best_ids  = Wire(Vec(fetch_width, UInt(width=log2Ceil(num_tables))))
   // Vector of alt predictors (one for each cfi index).
   val f3_alt_hits  = Wire(init = Vec.fill(fetch_width) { false.B })
   val f3_alt_ids   = Wire(Vec(fetch_width, UInt(width=log2Ceil(num_tables))))

   // Build up a bit-mask of taken predictions (1 bit per cfi index).
   // Build up a best predictor (one per cfi index).
   // Build up an alt predictor (one per cfi index).
   for (w <- 0 until fetch_width) yield
   {
      val found_first = f3_hits_matrix.map{mask => mask(w)}.reduce(_|_)

      // Grab a slice by only looking at hits-matrix for cidx=w.
      val hits_slice = for (i <- 0 until num_tables) yield { f3_hits_matrix(i)(w) }
      f3_best_hits(w) := hits_slice.reduce(_|_)
      f3_best_ids(w) := GetProviderTableId(hits_slice)
      val (alt_hit, alt_id) = GetAlternateTableId(hits_slice)
      f3_alt_hits(w) := alt_hit
      f3_alt_ids(w) := alt_id

      // TODO allow use_alt
      f3_takens(w) :=
         Mux(f3_best_hits(w),
            Vec(f3_predictions)(f3_best_ids(w)).predictsTaken,
            false.B)
   }

   // Vector of OH bitmasks fo
//   val f3_hit_matrix = for (i <- 0 until num_tables) yield
//   {
//      cidx_mask_oh = Mux(f3_tag_hits(i), UIntToOH(f3_predictions(i).cidx) & io.f3_is_br, 0.U)
//   }

   // Which cidx are we predicting on?
   // Earliest cidx with a taken prediction
   // OR earliest cidx with a hit if there is no takens.
   val predicted_cidx =
      Mux(f3_takens.reduce(_|_),
         PriorityEncoder(f3_takens),
         PriorityEncoder(f3_best_hits)) // TODO allow use_alt

   val resp_info = Wire(new TageResp(
      fetch_width = fetch_width,
      num_tables = num_tables,
      max_history_length = history_lengths.max,
      max_index_sz = log2Up(table_sizes.max),
      max_tag_sz = tag_sizes.max,
      cntr_sz = cntr_sz,
      ubit_sz = ubit_sz))

   val f3_has_hit = f3_best_hits.reduce(_|_)
   val f3_pred = Vec(f3_predictions)(f3_best_ids(predicted_cidx))

   assert (!(f3_has_hit && !f3_best_hits(predicted_cidx)), "[tage] was a hit but our cidx is wrong.")

   io.resp.valid       := f3_has_hit || io.f2_bim_resp.valid
   io.resp.bits.takens := Mux(f3_has_hit,
                              GetPredictionOH(f3_pred.cidx, f3_pred.cntr),
                              io.f2_bim_resp.bits.getTakens)

   resp_info.tags    := Vec(f3_predictions.map(_.tag))
   resp_info.cntrs   := Vec(f3_predictions.map(_.cntr))
   resp_info.cidxs   := Vec(f3_predictions.map(_.cidx))
   resp_info.ubits   := Vec(f3_predictions.map(_.ubit))

   resp_info.debug_indexes := RegEnable(bp1_idxs, f1_valid)
   resp_info.debug_tags    := RegEnable(bp1_tags, f1_valid)

   resp_info.provider_hit  := f3_best_hits.reduce(_|_)
   resp_info.provider_id   := f3_best_ids(predicted_cidx)

   resp_info.alt_hit       := f3_alt_hits(predicted_cidx)
   resp_info.alt_id        := f3_alt_ids(predicted_cidx)

   io.resp.bits.info       := resp_info.asUInt

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


   val com_indexes = history_lengths zip table_sizes map { case (hlen, tsize)  =>
      val idx = IdxHash(r_commit.bits.fetch_pc, r_commit.bits.history, hlen, log2Ceil(tsize))
      idx
   }

   val com_tags = for (i <- 0 until num_tables) yield
   {
      val n = num_tables
      TagHash(r_commit.bits.fetch_pc, com_indexes((i+1) % n), com_indexes((i+2) % n))
   }

   val alt_agrees =
      r_info.alt_hit &&
      r_info.cntrs(r_info.alt_id)(cntr_sz-1) === r_info.cntrs(r_info.provider_id)(cntr_sz-1)


   // Provide some randomization to the allocation process (count up to 4).
   val (rand, overflow) = Counter(io.commit.valid, 4)

   // Are we going to perform a write to the table?
   // What action are we going to perform? Allocate? Update? Or Degrade?
   val table_wens       = Wire(init = Vec.fill(num_tables) {false.B})
   val table_allocates  = Wire(init = Vec.fill(num_tables) {false.B})
   val table_updates    = Wire(init = Vec.fill(num_tables) {false.B})
   val table_degrades   = Wire(init = Vec.fill(num_tables) {false.B})


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

         val temp = Mux(rand === 3.U, 2.U,
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
         com_entry.tag  := Mux(table_allocates(i), com_tags(i), r_info.tags(i))
         com_entry.cntr := r_info.cntrs(i)
         com_entry.cidx := Mux(table_allocates(i), r_commit.bits.miss_cfi_idx,  r_info.cidxs(i))
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

   override val compileOptions = chisel3.core.ExplicitCompileOptions.NotStrict.copy(explicitInvalidate = true)
}

