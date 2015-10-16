//**************************************************************************
// RISCV Branch Predictor (abstract class)
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2015 Oct 12

// provide an abstract class for branch predictors. Also provides support by
// maintaining the global history. However, sub-classes may want to implement
// their own, optimized implementation of global history (e.g. if history is
// very long!).


package BOOM

import Chisel._
import Node._

import rocket.Str
 
class BrPredictorIo extends BOOMCoreBundle
{
   val req_pc = UInt(INPUT, width = vaddrBits)
   val resp = Decoupled(new BpdResp)
   val hist_update_spec = Valid(new GHistUpdate).flip // speculative update to ghist
// TODO brinfo, or br_unit instead?
   val br_resolution = Valid(new BpdUpdate).flip // from branch-unit
   val brob = new BrobBackendIo
   val flush = Bool(INPUT) // pipeline flush
}
    
class BpdResp extends BOOMCoreBundle
{
   val takens = Bits(width = FETCH_WIDTH)
   val history = Bits(width = GHIST_LENGTH)
}
 
// BP2 stage needs to speculatively update the history register with what the
// processor decided to do (takes BTB's effect into account).
// Also used for updating the commit-copy during commit.
class GHistUpdate extends BOOMCoreBundle
{
   val taken = Bool()
}

// update comes from the branch-unit with the actual outcome
// it needs to do three things:
//    - 1) correct the history if the processor mispredicted
//    - 2) correct the p-table if it mispredicted (the processor may have been correct though)
//    - 3) strengthen the h-table (on all branch resolutions)
class BpdUpdate extends BOOMCoreBundle
{
   // the fetch pc (points to start of the fetch packet)
   // which word in the fetch packet does the update correspond to?
   // processor mispredicted -> reset history
   // what was the history our branch saw?
   // bpd mispredicted -> correct predictor
   // was the branch taken?
   // is the new target PC after a misprediction found within the same fetch
   // packet as the mispredicting branch? If yes, then we have to be careful
   // which "history" we show the new PC.
   val pc = UInt(width = vaddrBits)
   val br_pc = UInt(width = log2Up(FETCH_WIDTH)+log2Ceil(coreInstBytes))
   val mispredict = Bool()
   val history = Bits(width = GHIST_LENGTH)
   val bpd_mispredict = Bool()
   val taken = Bool()
   val new_pc_same_packet = Bool()
}

// commit update information on the entire fetch packet
//class BrCommitUpdate extends BOOMCoreBundle
//{
//   val com_brob_idx = UInt(width=BROB_ADDR_SZ)
//   val kill = Bool()
//}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------

abstract class BrPredictor(val history_length: Int) extends Module with BOOMCoreParameters
{
   val io = new BrPredictorIo

   // the (speculative) global history wire (used for accessing the branch predictor state).
   val ghistory = Wire(Bits(width = history_length))
 
   // the (speculative) global history register. Needs to be massaged before usably by the bpd.
   private val r_ghistory = Reg(init = Bits(0, width = history_length))
  
   // we need to maintain a copy of the commit history, in-case we need to
   // reset it on a pipeline replay.
   private val r_ghistory_commit_copy = Reg(init = Bits(0, width = history_length))


   // Bypass some history modifications before it can be used by the predictor
   // hash functions. For example, "massage" the history for the scenario where
   // the mispredicted branch is not taken AND we're having to refetch the rest
   // of the fetch_packet.
   private val fixed_history = Cat(io.br_resolution.bits.history, io.br_resolution.bits.taken)
   ghistory :=
      Mux(io.flush,                       r_ghistory_commit_copy,
      Mux(io.br_resolution.valid &&
          io.br_resolution.bits.bpd_mispredict &&
          io.br_resolution.bits.new_pc_same_packet, io.br_resolution.bits.history,
      Mux(io.br_resolution.valid &&
          io.br_resolution.bits.bpd_mispredict    , fixed_history,
                                                    r_ghistory)))

   when (io.flush)
   {
      r_ghistory := r_ghistory_commit_copy
   }
   when (io.br_resolution.valid && io.br_resolution.bits.mispredict)
   {
      r_ghistory := fixed_history
   }
   .elsewhen (io.hist_update_spec.valid)
   {
      r_ghistory := Cat(r_ghistory, io.hist_update_spec.bits.taken)
   }
    
   // -----------------------------------------------
   
   val brob = Module(new BranchReorderBuffer(NUM_BROB_ENTRIES))
   brob.io.backend <> io.brob
    
   
   when (brob.io.backend.deallocate.valid)
   {
      r_ghistory_commit_copy := Cat(r_ghistory_commit_copy, brob.io.backend.deallocate.bits.taken)
   }

   if (DEBUG_PRINTF)
   {
      printf(" predictor: ghist: 0x%x, r_ghist: 0x%x commit: 0x%x\n"
         , ghistory
         , r_ghistory
         , r_ghistory_commit_copy
         )
   }
}
 
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------

// The BranchReorderBuffer holds all inflight branchs for the purposes of
// bypassing inflight prediction updates to the predictor. It also holds expensive predictor state needed for updating the predictor at commit time.


class BrobBackendIo extends BOOMCoreBundle
{
   val allocate = Decoupled(new BrobEntry).flip // Decode/Dispatch stage, allocate new brob entry
   val allocate_brob_tail = UInt(OUTPUT, width = BROB_ADDR_SZ) // tell Decode which entry gets allocated

   val deallocate = Valid(new BrobEntry).flip // Commmit stage, from the ROB
   
   val br_unit = new BranchUnitResp().asInput  // reset tail on mispredicts
   val flush = Bool(INPUT) // wipe the ROB
}

class BrobEntry extends BOOMCoreBundle
{
   val executed   = Bool()      
   val taken      = Bool()
   val mispredict = Bool()
   val brob_idx   = UInt(width = BROB_ADDR_SZ)
   
   val rob_idx    = UInt(width = ROB_ADDR_SZ) // debug purposes

   val pred_info = new BranchPredictionResp
}

class BranchReorderBuffer(num_entries: Int) extends Module
{
   val io = new BOOMCoreBundle
   {
      // connection to BOOM's ROB/backend/etc.
      val backend = new BrobBackendIo

      // update predictor at commit
      val commit_entry = Valid(new BrobEntry)

      // forward predictions
      // TODO enable bypassing of information
   //  val pred_req = Valid(new // from fetch, requesting if a prediction matches an inflight entry.
   //  val pred_resp = Valid(new // from fetch, return a prediction
   }

   println ("\t    BROB Size (" + num_entries + ") entries)")

   // each entry corresponds to a fetch-packet
   // ROB shouldn't send "deallocate signal" until the entire packet has finished committing.
   val entries  = Reg(Vec(num_entries, new BrobEntry()))
   val head_ptr = Reg(init = UInt(0, log2Up(num_entries)))
   val tail_ptr = Reg(init = UInt(0, log2Up(num_entries)))

   val br_resolution_valid = io.backend.br_unit.brinfo.valid && Reg(next=io.backend.br_unit.bpd_update.valid)
   // -----------------------------------------------
   when (io.backend.allocate.valid)
   {
      entries(tail_ptr) := io.backend.allocate.bits
      tail_ptr := WrapInc(tail_ptr, num_entries)

      assert (tail_ptr === io.backend.allocate.bits.brob_idx, "allocate incorrect entry")
   }
   when (io.backend.deallocate.valid)
   {
      head_ptr := WrapInc(head_ptr, num_entries)

      assert (entries(head_ptr).executed === Bool(true), "Committing an entry not executed.")
      assert (head_ptr === io.backend.deallocate.bits.brob_idx , "Committing wrong entry.")
   }
   when (io.backend.br_unit.brinfo.valid && 
         io.backend.br_unit.brinfo.is_jr &&
         io.backend.br_unit.brinfo.mispredict)
   {
      // jalr isn't allocated an entry, but if it mispredicts we need to reset the BROB 
      // to the state the JALR saw when it was fetched/decoded.
      tail_ptr := io.backend.br_unit.brinfo.brob_idx
   }
   when (io.backend.br_unit.brinfo.valid && 
         !io.backend.br_unit.brinfo.is_jr &&
         Reg(next=io.backend.br_unit.bpd_update.valid))
   {
      entries(io.backend.br_unit.brinfo.brob_idx).executed := Bool(true)
      entries(io.backend.br_unit.brinfo.brob_idx).taken := io.backend.br_unit.taken
      when (io.backend.br_unit.brinfo.mispredict)
      {
         tail_ptr := WrapInc(io.backend.br_unit.brinfo.brob_idx, num_entries)
      }
   }
   when (io.backend.flush)
   {
      head_ptr := UInt(0)
      tail_ptr := UInt(0)
   }

   // -----------------------------------------------
   // outputs
   io.commit_entry.valid := io.backend.deallocate.valid
   io.commit_entry.bits := entries(head_ptr)

   // TODO allow filling the entire BROB ROB.
   // I had difficulty getting the "maybe_full" correct when dealing with
   // mispredicts from jr's while already full.
   val maybe_full = Reg(init=Bool(false))
   val full = (head_ptr === tail_ptr) && maybe_full

   val do_alloc = io.backend.allocate.valid && !full
   val do_dealloc = io.backend.deallocate.valid
   when (io.backend.flush || 
         ((io.backend.br_unit.brinfo.valid && 
         io.backend.br_unit.brinfo.mispredict) && 
         io.backend.br_unit.brinfo.brob_idx != tail_ptr))
   {
      maybe_full := Bool(false)
   }
   .elsewhen (do_alloc != do_dealloc)
   {
      maybe_full := do_alloc
   }
//   val full = (head_ptr === WrapInc(tail_ptr, num_entries))
   io.backend.allocate.ready := !full

   assert (!(full && io.backend.allocate.valid), "Trying to allocate entry while full.")

   io.backend.allocate_brob_tail := tail_ptr

   // -----------------------------------------------

   if (DEBUG_PRINTF)
   {
      for (i <- 0 until num_entries)
      {
         printf (" brob[%d] (%s) r=%d, hist=%x"
            , UInt(i, log2Up(num_entries))
            , Mux(entries(i).executed && entries(i).taken, Str("E,T"), 
              Mux(entries(i).executed && !entries(i).taken, Str("E,n"), 
                  Str("_,_")))
            , entries(i).rob_idx
            , entries(i).pred_info.bpd_history
            )
         
         printf("%s %s %s\n"
            ,  Mux(head_ptr === UInt(i) && tail_ptr === UInt(i), Str("<-HEAD TL"),
               Mux(head_ptr === UInt(i),                         Str("<-HEAD   "),
               Mux(tail_ptr === UInt(i),                         Str("<-     TL"),
                                                                 Str(" "))))
            , Mux(full, Str("F"), Str("_")) 
            , Mux(maybe_full, Str("M"), Str("_")) 
            )
      }
   }
}
