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

class BrPredictorIo(fetch_width: Int) extends BOOMCoreBundle
{
   val req_pc = UInt(INPUT, width = vaddrBits)
   val resp = Decoupled(new BpdResp)
   val hist_update_spec = Valid(new GHistUpdate).flip // speculative update to ghist
// TODO brinfo, or br_unit instead? do I actually need br_unit?
   val br_resolution = Valid(new BpdUpdate).flip // from branch-unit
   val brob = new BrobBackendIo(fetch_width)
   val flush = Bool(INPUT) // pipeline flush
}

class BpdResp extends BOOMCoreBundle
{
   val takens = Bits(width = FETCH_WIDTH)

   // TODO customize this based on the sub-class predictor
   // probable solution is to turn it into a Bits() of a parameterized width,
   // which is set by querying the subclass.
   val info = new GShareResp
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

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------

abstract class BrPredictor(fetch_width: Int, val history_length: Int) extends Module with BOOMCoreParameters
{
   val io = new BrPredictorIo(fetch_width)

   // the (speculative) global history wire (used for accessing the branch predictor state).
   val ghistory = Wire(Bits(width = history_length))

   // the commit update bundle (we update predictors).
//   val commit = Wire(new BpdUpdate)
   val commit = Wire(Valid(new BrobEntry(fetch_width))) // TODO move to BpdCommitUpdate, instead?

   // the (speculative) global history register. Needs to be massaged before usably by the bpd.
   private val r_ghistory = Reg(init = Bits(0, width = history_length))

   // we need to maintain a copy of the commit history, in-case we need to
   // reset it on a pipeline flush/replay.
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

   val brob = Module(new BranchReorderBuffer(fetch_width, NUM_BROB_ENTRIES))
   brob.io.backend <> io.brob
   commit := brob.io.commit_entry

   when (commit.valid)
   {
      r_ghistory_commit_copy := Cat(r_ghistory_commit_copy, commit.bits.taken.reduce(_|_))
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
// bypassing inflight prediction updates to the predictor. It also holds
// expensive predictor state needed for updating the predictor at commit time.

// NOTE: JALRs also go into the BROB. Their effect is ignored for the purposes
// of updating the branch predictor, as it is the job of the BTB and not the
// BPD to predict JALRs.  However, as JALRs can mispredict and kill inflight
// instructions, it GREATLY simplifies the BROB misprediction handling logic to
// include JALR in the BROB entries (as the brob_tail must be reset on a
// misprediction).  This is further exacerbated by superscalar issues
// <-,br,jalr,br>, in which how the brob_tail is reset depends on whether or
// not a valid branch is before the jalr instruction (and what if the br has
// already been committed?). Yuck. But it does waste BROB entries to include
// JALRs.

class BrobBackendIo(fetch_width: Int) extends BOOMCoreBundle
{
   val allocate = Decoupled(new BrobEntry(fetch_width)).flip // Decode/Dispatch stage, allocate new brob entry
   val allocate_brob_tail = UInt(OUTPUT, width = BROB_ADDR_SZ) // tell Decode which entry gets allocated

   val deallocate = Valid(new BrobDeallocateIdx).flip // Commmit stage, from the ROB

   val br_unit = new BranchUnitResp().asInput  // reset tail on mispredicts
   val flush = Bool(INPUT) // wipe the ROB
}

class BrobDeallocateIdx extends BOOMCoreBundle
{
   val brob_idx = UInt(width = BROB_ADDR_SZ)
}

// Each "entry" corresponds to a single fetch packet.
// Each fetch packet may contain up to W branches, where W is the fetch_width.
class BrobEntry(fetch_width: Int) extends BOOMCoreBundle
{
   val executed   = Vec.fill(fetch_width) {Bool()} // mark that a branch executed (and should update the predictor).
   val taken      = Vec.fill(fetch_width) {Bool()}
   val mispredict = Vec.fill(fetch_width) {Bool()} //did bpd mispredict this br? (aka, should we update the predictor).
   val brob_idx   = UInt(width = BROB_ADDR_SZ)

   val debug_executed = Bool() // did a br or jalr get executed? verify we're not deallocating an empty entry.
   val debug_rob_idx = UInt(width = ROB_ADDR_SZ)

   val info = new BpdResp

  override def cloneType: this.type = new BrobEntry(fetch_width).asInstanceOf[this.type]
}

class BranchReorderBuffer(fetch_width: Int, num_entries: Int) extends Module with BOOMCoreParameters
{
   val io = new BOOMCoreBundle
   {
      // connection to BOOM's ROB/backend/etc.
      val backend = new BrobBackendIo(fetch_width)

      // update predictor at commit
      val commit_entry = Valid(new BrobEntry(fetch_width))

      // forward predictions
      // TODO enable bypassing of information. See if there's a "match", and then forward the outcome.
   //  val pred_req = Valid(new // from fetch, requesting if a prediction matches an inflight entry.
   //  val pred_resp = Valid(new // from fetch, return a prediction
   }

   println ("\tBROB (w=" + fetch_width + ") Size (" + num_entries + ") entries")

   // each entry corresponds to a fetch-packet
   // ROB shouldn't send "deallocate signal" until the entire packet has finished committing.
   val entries  = Reg(Vec(num_entries, new BrobEntry(fetch_width)))
   val head_ptr = Reg(init = UInt(0, log2Up(num_entries)))
   val tail_ptr = Reg(init = UInt(0, log2Up(num_entries)))

   val br_unit = Wire(new BranchUnitResp())
   br_unit <> io.backend.br_unit

   val br_resolution_valid = br_unit.brinfo.valid &&
                             Reg(next=br_unit.bpd_update.valid)

   private def GetIdx(addr: UInt) =
      if (fetch_width == 1) UInt(0)
      else (addr >> UInt(log2Ceil(coreInstBytes))) & SInt(-1, log2Ceil(fetch_width))

   // -----------------------------------------------
   when (io.backend.allocate.valid)
   {
      entries(tail_ptr) := io.backend.allocate.bits
      tail_ptr := WrapInc(tail_ptr, num_entries)

      assert (tail_ptr === io.backend.allocate.bits.brob_idx,
         "[BROB] allocating the wrong entry.")
   }
   when (io.backend.deallocate.valid)
   {
      head_ptr := WrapInc(head_ptr, num_entries)

      assert (entries(head_ptr).debug_executed === Bool(true),
         "[BROB] Committing an entry with no executed branches or jalrs.")
      assert (head_ptr === io.backend.deallocate.bits.brob_idx ,
         "[BROB] Committing wrong entry.")
   }

   val idx = GetIdx(Reg(next=br_unit.bpd_update.bits.br_pc))
   when (br_unit.brinfo.valid)
   {
      entries(br_unit.brinfo.brob_idx).executed(idx) := br_unit.brinfo.is_br
      entries(br_unit.brinfo.brob_idx).taken(idx) := br_unit.brinfo.taken
      entries(br_unit.brinfo.brob_idx).debug_executed := Bool(true)

      when (br_unit.brinfo.mispredict)
      {
         entries(br_unit.brinfo.brob_idx).mispredict(idx) :=
            Reg(next=br_unit.bpd_update.bits.bpd_mispredict)

         // clear the executed bits behind this instruction
         // (as they are on the misspeculated path)
         for (w <- 0 until fetch_width)
         {
            when (UInt(w) > idx)
            {
               entries(br_unit.brinfo.brob_idx).executed(w) := Bool(false)
            }
         }

         tail_ptr := WrapInc(br_unit.brinfo.brob_idx, num_entries)
      }
      require (coreInstBytes == 4)
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
   // mispredicts from jr's and branches while already full.
//   val maybe_full = Reg(init=Bool(false))
//   val full = (head_ptr === tail_ptr) && maybe_full
//   val do_alloc = io.backend.allocate.valid && !full
//   val do_dealloc = io.backend.deallocate.valid
//   when (io.backend.flush ||
//         ((io.backend.br_unit.brinfo.valid &&
//         io.backend.br_unit.brinfo.mispredict) &&
//         io.backend.br_unit.brinfo.brob_idx != tail_ptr)) <<--- broken
//   {
//      maybe_full := Bool(false)
//   }
//   .elsewhen (do_alloc != do_dealloc)
//   {
//      maybe_full := do_alloc
//   }

   val full = (head_ptr === WrapInc(tail_ptr, num_entries))
   io.backend.allocate.ready := !full

   assert (!(full && io.backend.allocate.valid), "Trying to allocate entry while full.")

   io.backend.allocate_brob_tail := tail_ptr

   // -----------------------------------------------

   if (DEBUG_PRINTF)
   {
      for (i <- 0 until num_entries)
      {
         printf (" brob[%d] (%x) T=%x m=%x r=%d, hist=%x, 0x%x, br_upd_idx=%d "
            , UInt(i, log2Up(num_entries))
            , entries(i).executed.toBits
            , entries(i).taken.toBits
            , entries(i).mispredict.toBits
            , entries(i).debug_rob_idx
            , entries(i).info.info.history
            , entries(i).info.info.index
            , idx
            )

         printf("%s\n"
            ,  Mux(head_ptr === UInt(i) && tail_ptr === UInt(i), Str("<-HEAD TL"),
               Mux(head_ptr === UInt(i),                         Str("<-HEAD   "),
               Mux(tail_ptr === UInt(i),                         Str("<-     TL"),
                                                                 Str(" "))))
            )
      }
   }
}
