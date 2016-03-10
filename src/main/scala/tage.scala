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

// TODO:
//    - veirfy the prediction stage is setting up the proper TageResp pieces
//    - figure out how to handle ping-ponging over not-useful state
//    - verify tables/predictor is passing down proper state (br resolution and commit)
//    - make visualizer dump to see how it predicts
//    - abstract BpdResp stuff, make sure we're happy with interfaces, functions
//    - abstract ability to set BpdResp automatically (and the proper lengths, index_sz, etc.
//    - add proper fold logic, make sure full history is being used.
//    - get the sizes of tables, tags, csrs, etc., all correct.
//    - verify performance is where it should be (dhrystone, coremark, gcc)
//    - make predictor handle superscalar fetch
//    - make predictor sequential (first show it works, then make it sequential)
//    - add very-long histories (VLH)
//    - alt-pred tracking (choosing between +2 tables, sometimes using alt pred if u is low)
//    - u-bit handling, clearing (count failed allocations?)
//    - SRAM handling
//    - banking
//    - implement updating
//    - abstract the bpd response packet
//    - lower required parameters, arguments to bundles and objects
//    - able to allocate >1 tables
//    - randomly pick lowest 2 tables to allocate 
//    - brpredictor seems to couple fetch-width and commit-width :(
//    - do ALL the tags need to be tracked? can we compute alloc_id during prediction?
//       - no, maintain commit-copy of CSRs, pass in committed Fetch_pC to recompute
//    stats we want to track:
//       - how often no BTB hit and no table hit
//       - how often entries are used
//       - how often allocation fails
//       - how often we reset the useful-ness bits
// SCHEMES
//    - u-bit incremented only if alt-pred available? 
//    - frequency of clearing u-bits
//    - 1 or 2-bit u-bits?
//
// DEBUGGING:
//    - verify priority mux is in correct order
//    - is the BROB giving us correct information? Are jumps polluting ghist_commit, etc.?

package boom

import Chisel._
import Node._
import cde.Parameters

import rocket.Str


class TageResp(history_length: Int, index_length: Int, num_tables: Int, max_tag_sz: Int) 
   extends Bundle
{
   val provider_hit = Bool() // did tage make a prediction? 
   val provider_id = UInt(width = 5) // which table is providing the prediction?
   val provider_predicted_taken = Bool() 
   val alt_hit = Bool()  // an alternate table made a prediction too
   val alt_id = UInt(width = 5)  // which table is the alternative?
   val alt_predicted_taken = Bool() 
   val tags = Vec(num_tables, Bits(width = max_tag_sz))

   val history = Bits(width = history_length) // stored in snapshots (dealloc after Execute)
   val index =  Bits(width = index_length) // needed to update predictor at Commit

   val br_pc = UInt(width=64)
}
  
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
  
class TageBrPredictor(
   fetch_width: Int,
   num_tables: Int,
   table_sizes: Seq[Int],
   history_lengths: Seq[Int],
   tag_sizes: Seq[Int]
   )(implicit p: Parameters) 
   extends BrPredictor(
      fetch_width, 
      history_length = history_lengths.max)(p)
{
   println ("\tBuilding prototype TAGE Predictor (max history length: " 
      + history_lengths.max + " bits)")
   require (num_tables == table_sizes.size)
   require (num_tables == history_lengths.size)
   require (num_tables == tag_sizes.size)
   // require (log2Up(num_tables) <= TageResp.provider_id.getWidth()) TODO implement this check
   require (coreInstBytes == 4)

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

   val tables = for (i <- 0 until num_tables) yield
   {
      val table = Module(new TageTable(
         fetch_width    = fetch_width,
         num_entries    = table_sizes(i),
         history_length = history_lengths(i),
         tag_sz         = tag_sizes(i),
         counter_sz     = 2))
      table.io.InitializeIo()

      // send prediction request
      table.io.if_req_pc := io.req_pc
      table.io.if_req_history := this.ghistory
 
      // update during mispredict 
      table.io.bp2_update_history <> io.hist_update_spec
   
      // check that the user ordered his TAGE tables properly
      if (i > 0) require(history_lengths(i) > history_lengths(i-1))

      table
   }

   val valids = tables.map{ _.io.bp2_resp.valid }
   val predictions = tables.map{ _.io.bp2_resp.bits }

   // get BestPred (priority to last table) 
   val final_prediction_valid = valids.reduce(_|_)
   val final_prediction_bits = PriorityMux(valids.reverse, predictions.reverse)

   io.resp.valid             := final_prediction_valid
   io.resp.bits.takens       := final_prediction_bits.taken // TODO superscalar
   io.resp.bits.info.index   := final_prediction_bits.index
   io.resp.bits.info.history := RegNext(RegNext(this.ghistory))
   io.resp.bits.info.provider_hit := io.resp.valid
   io.resp.bits.info.provider_id := GetProviderTableId(valids)
   io.resp.bits.info.provider_predicted_taken := final_prediction_bits.taken// TODO superscalar

   val (p_alt_hit, p_alt_id) = GetAlternateTableId(valids)
   io.resp.bits.info.alt_hit := p_alt_hit
   io.resp.bits.info.alt_id  := p_alt_id
   io.resp.bits.info.alt_predicted_taken := Vec(predictions.map(_.taken))(p_alt_id)
   io.resp.bits.info.br_pc := RegNext(RegNext(io.req_pc))

   println("tags len: " + io.resp.bits.info.tags.length + ", predictions len: " + 
      predictions.map(_.tag_csr1).length)
   io.resp.bits.info.tags := Vec(predictions.map(_.tag))

   printf("\n")
   when (io.resp.valid)
   {
      printf(red + "prediction made hit: PC 0x%x, ghistory=0x%x" + end + "\n"
         , RegNext(RegNext(io.req_pc))
         , io.resp.bits.info.history
      )
   }

   printf("0x%x Prediction Hits Array, Provider: %d\n", 
      Vec(valids).toBits, io.resp.bits.info.provider_id)
   for (i <- num_tables-1 to 0 by -1)
   {
      printf("%d ", valids(i).toUInt)
   }
   printf("\n")

   //------------------------------------------------------------
   //------------------------------------------------------------
   // update predictor during commit
  
   val tables_io = Vec(tables.map(_.io))
 
   when (commit.valid && commit.bits.executed.reduce(_|_))
   {
      val correct = !commit.bits.mispredicted.reduce(_|_)
      val info = commit.bits.info.info
      val taken = commit.bits.taken.reduce(_|_); require(fetch_width == 1)

      // consider making a dummy table0, so the indexing works
      val provider_id = info.provider_id
      val alt_id      = info.alt_id
      val alt_agrees  = info.alt_hit && (info.provider_predicted_taken === info.alt_predicted_taken)

      assert (provider_id < UInt(num_tables) || !info.provider_hit, 
         "[Tage] provider_id is out-of-bounds.")

      // no matter what happens, update table that made a prediction
      when (info.provider_hit)
      {
         tables_io(provider_id).UpdateCounters(
            info.index, taken, alt_agrees, info.provider_predicted_taken)
      }

      printf("Committing and updating predictor: PC: 0x%x correct=%d\n", info.br_pc, correct)
      when (!correct && (provider_id < UInt(MAX_TABLE_ID) || !info.provider_hit)) 
      {
         // try to allocate a new entry 
         
         // 2. if provider not T_max, try to allocate an entry on T_k (i < k < max).       
         //    - only allocate one entry.                                                  
         //    - a) if find an entry u_k that == 0, then allocate T_k                      
         //    - b) ELSE decrement u_counters from Tj where (i<j<=max), or just (i<j<max). 
         //                                                                                

         printf("Trying to allocate an entry.... hit=%d, provider=%d\n", info.provider_hit, provider_id)

         // find lowest alloc_idx where u_bits === 0
         val can_allocates = Range(0, num_tables).map(i => 
            tables(i).io.GetUsefulness(info.index) === Bits(0) && ((UInt(i) > provider_id) || !info.provider_hit))
         
         val alloc_id = PriorityEncoder(can_allocates)
         when (can_allocates.reduce(_|_))
         {
            tables_io(alloc_id).AllocateNewEntry(info.index, info.tags(alloc_id), taken)
            printf("Allocating on Table[%d] at index:%d tag=0x%x  taken=%d, provider=%d, can_allocates=0x%x\n", 
               alloc_id, info.index, info.tags(alloc_id), taken, provider_id, Vec(can_allocates).toBits)
         }
         .otherwise
         {
            //decrementUBits for tables[provider_id+1: T_max]
            for (i <- 0 until num_tables)
            {
               when ((UInt(i) > provider_id) || !info.provider_hit)
               {
                  tables(i).io.DecrementUseful(info.index)
                  printf("Decrementing useful bit for Table[%d]\n", UInt(i))
               }
            }
            printf("Failed allocation index:%d  taken=%d, can_allocates=0x%x\n", 
               info.index, taken, Vec(can_allocates).toBits)
         }
      }
   }
}

