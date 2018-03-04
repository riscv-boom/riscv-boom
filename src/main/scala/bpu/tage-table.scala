//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// TAGE Table (used by the TAGE branch predictor)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2016 Feb 26



package boom

import Chisel._
import freechips.rocketchip.config.Parameters

import freechips.rocketchip.util.Str


class TageTableIo(
   fetch_width: Int,
   num_entries: Int,
   history_length: Int,
   tag_sz: Int,
   counter_sz: Int,
   this_index_sz: Int
   )(implicit p: Parameters) extends BoomBundle()(p)
{
   private val index_sz = log2Ceil(num_entries)

   // instruction fetch - request prediction
   val if_req_pc = UInt(INPUT, width = xLen)

   // bp2 - send prediction to bpd pipeline
   val bp2_resp = new DecoupledIO(new TageTableResp(fetch_width, history_length, log2Ceil(num_entries), tag_sz))

   // commit - update predictor tables (allocate entry)
   val allocate = (new ValidIO(new TageAllocateEntryInfo(fetch_width, index_sz, tag_sz, history_length))).flip
   def AllocateNewEntry(idx: UInt, tag: UInt, executed: UInt, taken: UInt, debug_pc: UInt, debug_hist_ptr: UInt) =
   {
      this.allocate.valid := Bool(true)
      this.allocate.bits.index := idx
      this.allocate.bits.tag :=tag
      this.allocate.bits.executed :=executed
      this.allocate.bits.taken :=taken
      this.allocate.bits.debug_pc := debug_pc
      this.allocate.bits.debug_hist_ptr :=debug_hist_ptr
   }

   // commit - update predictor tables (update counters)
   val update_counters = (new ValidIO(new TageUpdateCountersInfo(fetch_width, index_sz))).flip
   def UpdateCounters(idx: UInt, executed: UInt, taken: UInt, mispredicted: Bool) =
   {
      this.update_counters.valid := Bool(true)
      this.update_counters.bits.index := idx
      this.update_counters.bits.executed := executed
      this.update_counters.bits.taken := taken
      this.update_counters.bits.mispredicted := mispredicted
   }

   // commit - update predictor tables (update u-bits)
   val update_usefulness = (new ValidIO(new TageUpdateUsefulInfo(index_sz))).flip
   def UpdateUsefulness(idx: UInt, inc: Bool) =
   {
      this.update_usefulness.valid := Bool(true)
      this.update_usefulness.bits.index := idx
      this.update_usefulness.bits.inc := inc
   }

   val usefulness_req_idx = UInt(INPUT, index_sz)
   val usefulness_resp = UInt(OUTPUT, 2) // TODO u-bit_sz
   def GetUsefulness(idx: UInt, idx_sz: Int) =
   {
//      this.usefulness_req_idx := idx(this_index_sz-1,0) // TODO CODEREVIEW
      this.usefulness_req_idx := idx(idx_sz-1,0) // TODO CODEREVIEW
      this.usefulness_resp
   }

   val degrade_usefulness_valid = Bool(INPUT)
   def DegradeUsefulness(dummy: Int=0) =
   {
      this.degrade_usefulness_valid := Bool(true)
   }

   // branch resolution comes from the branch-unit, during the Execute stage.
   val br_resolution = Valid(new BpdUpdate).flip

   def InitializeIo(dummy: Int=0) =
   {
      this.allocate.valid := Bool(false)
      this.update_counters.valid := Bool(false)
      this.update_usefulness.valid := Bool(false)
      this.allocate.bits.index := UInt(0)
      this.allocate.bits.tag := UInt(0)
      this.allocate.bits.executed := UInt(0)
      this.allocate.bits.taken := UInt(0)
      this.allocate.bits.debug_pc := UInt(0)
      this.allocate.bits.debug_hist_ptr := UInt(0)
      this.update_counters.bits.index := UInt(0)
      this.update_counters.bits.executed := UInt(0)
      this.update_counters.bits.taken := UInt(0)
      this.update_counters.bits.mispredicted := Bool(false)
      this.update_usefulness.bits.index := UInt(0)
      this.update_usefulness.bits.inc := Bool(false)
      this.usefulness_req_idx := UInt(0)
      this.degrade_usefulness_valid := Bool(false)
   }

   override def cloneType: this.type = new TageTableIo(
      fetch_width, num_entries, history_length, tag_sz, counter_sz, this_index_sz).asInstanceOf[this.type]
}

class TageTableResp(fetch_width: Int, history_length: Int, index_length: Int, tag_sz: Int) extends Bundle
{
   val takens  = UInt(width = fetch_width)  // the actual prediction
   val index   = UInt(width = index_length) // the index of the prediction
   val tag     = UInt(width = tag_sz)       // the tag we computed for the prediction

   // Instead of passing huge histories around, just pass around a CSR of the
   // folded history (circular shift register).
   // This are snapshotted and reset on a misprediction.
   // Two CSRs are used for the tags to manage the scenario of repeating history
   // with the frequency equal to the history_length (it would fold down to
   // 0x0).
   val idx_csr  = UInt(width = index_length)
   val tag_csr1 = UInt(width = tag_sz)
   val tag_csr2 = UInt(width = tag_sz-1)

   override def cloneType: this.type = new TageTableResp(fetch_width, history_length, index_length, tag_sz).asInstanceOf[this.type]
}

class TageIndex(index_sz: Int) extends Bundle
{
   val index = UInt(width = index_sz)
   override def cloneType: this.type = new TageIndex(index_sz).asInstanceOf[this.type]
}

class TageUpdateUsefulInfo(index_sz: Int) extends Bundle
{
   val index = UInt(width = index_sz)
   val inc = Bool()
   override def cloneType: this.type = new TageUpdateUsefulInfo(index_sz).asInstanceOf[this.type]
}

class TageAllocateEntryInfo(fetch_width: Int, index_sz: Int, tag_sz: Int, hist_sz: Int) extends Bundle //TageIndex(index_sz)
{
   val index = UInt(width = index_sz)
   val tag = UInt(width = tag_sz)
   val executed = UInt(width = fetch_width)
   val taken = UInt(width = fetch_width)
   val debug_pc = UInt(width = 32)
   val debug_hist_ptr = UInt(width = hist_sz)
   override def cloneType: this.type = new TageAllocateEntryInfo(fetch_width, index_sz, tag_sz, hist_sz).asInstanceOf[this.type]
}

class TageUpdateCountersInfo(fetch_width: Int, index_sz: Int) extends Bundle //extends TageIndex(index_sz)
{
   val index = UInt(width = index_sz)
   val executed = UInt(width = fetch_width)
   val taken = UInt(width = fetch_width)
   val mispredicted = Bool()
   override def cloneType: this.type = new TageUpdateCountersInfo(fetch_width, index_sz).asInstanceOf[this.type]
}

// The CSRs contain the "folded" history. For them to work, we need to pass them
// the latest new bit to add in and the oldest bit to evict out.
class CircularShiftRegisterUpdate extends Bundle
{
   val new_bit = Bool()
   val evict_bit = Bool()
}


// In Chisel3, all Bundle elements in a Vec() must be homogenous (i.e., when
// using a Vec() of TageTableIOs, the sub-fields within the TageTableIOs must
// have the exact same widths (no heterogenous types/widths). Therefore, we must
// track the max_* size of the parameters, and then within the TageTable we must
// mask off extra bits as needed.
class TageTable(
   fetch_width: Int,
   num_entries: Int,
   history_length: Int,
   tag_sz: Int,
   max_num_entries: Int,
   max_history_length: Int,
   max_tag_sz: Int,
   counter_sz: Int,
   ubit_sz: Int,
   id: Int,
   num_tables: Int
   )(implicit p: Parameters) extends BoomModule()(p)
{
   val index_sz = log2Ceil(num_entries)

   val io = IO(
      new TageTableIo(fetch_width, max_num_entries, max_history_length, max_tag_sz, counter_sz, this_index_sz = index_sz))

   private val CNTR_MAX = (1 << counter_sz) - 1
   private val CNTR_WEAK_TAKEN = 1 << (counter_sz-1)
   private val CNTR_WEAK_NOTTAKEN = CNTR_WEAK_TAKEN - 1
   private val UBIT_MAX = (1 << ubit_sz) - 1
   private val UBIT_INIT_VALUE = 1

   println("\t    TageTable - "
      + num_entries + " entries, "
      + history_length + " bits of history, "
      + tag_sz + "-bit tags, "
      + counter_sz + "-bit counters (max value=" + CNTR_MAX + ")")

   require (counter_sz == 2)

   //------------------------------------------------------------
   // State
   val counter_table = Module(new TwobcCounterTable(fetch_width, num_entries, dualported=false))
   val tag_table     = Module(new TageTagMemory(num_entries, memwidth = tag_sz))
   val ubit_table    = if (ubit_sz == 1) Module(new TageUbitMemoryFlipFlop(num_entries, ubit_sz))
                       else              Module(new TageUbitMemorySeqMem(num_entries, ubit_sz))
   val debug_pc_table= Mem(num_entries, UInt(width = 32))
   val debug_hist_ptr_table=Mem(num_entries,UInt(width = log2Ceil(VLHR_LENGTH)))

   //history ghistory
   val idx_csr         = Module(new CircularShiftRegister(index_sz, history_length))
   val tag_csr1        = Module(new CircularShiftRegister(tag_sz  , history_length))
   val tag_csr2        = Module(new CircularShiftRegister(tag_sz-1, history_length))
   val commit_idx_csr  = Module(new CircularShiftRegister(index_sz, history_length))
   val commit_tag_csr1 = Module(new CircularShiftRegister(tag_sz  , history_length))
   val commit_tag_csr2 = Module(new CircularShiftRegister(tag_sz-1, history_length))

   tag_table.io.InitializeIo()
   ubit_table.io.InitializeIo()
   idx_csr.io.InitializeIo()
   tag_csr1.io.InitializeIo()
   tag_csr2.io.InitializeIo()
   commit_idx_csr.io.InitializeIo()
   commit_tag_csr1.io.InitializeIo()
   commit_tag_csr2.io.InitializeIo()


   //------------------------------------------------------------
   // functions

   private def Fold (input: UInt, compressed_length: Int) =
   {
      val clen = compressed_length
      val hlen = history_length
      if (hlen <= clen)
      {
         input
      }
      else
      {
         var res = UInt(0,clen)
         var remaining = input.asUInt
         for (i <- 0 to hlen-1 by clen)
         {
            val len = if (i + clen > hlen ) (hlen - i) else clen
            require(len > 0)
            res = res(clen-1,0) ^ remaining(len-1,0)
            remaining = remaining >> UInt(len)
         }
         res
      }
   }

   private def IdxHash (addr: UInt) =
   {
      val idx =
         ((addr >> UInt(log2Ceil(fetch_width*coreInstBytes))) ^
         idx_csr.io.next)

      idx(index_sz-1,0)
   }

   private def TagHash (addr: UInt) =
   {
      // the tag is computed by pc[n:0] ^ CSR1[n:0] ^ (CSR2[n-1:0]<<1).
      val tag_hash =
         (addr >> UInt(log2Ceil(fetch_width*coreInstBytes))) ^
         tag_csr1.io.next ^
         (tag_csr2.io.next << UInt(1))
      tag_hash(tag_sz-1,0)
   }

   //------------------------------------------------------------
   // Get Prediction

   val stall = !io.bp2_resp.ready

   val s1_pc = io.if_req_pc
   val p_idx = IdxHash(s1_pc)
   val p_tag = TagHash(s1_pc)

   counter_table.io.s1_r_idx := p_idx
   tag_table.io.s1_r_idx := p_idx
   counter_table.io.stall := stall
   tag_table.io.stall := stall

   val s2_tag      = tag_table.io.s2_r_out
   val bp2_tag_hit = s2_tag === RegEnable(p_tag, !stall)

   io.bp2_resp.valid       := bp2_tag_hit
   io.bp2_resp.bits.takens := counter_table.io.s2_r_out
   io.bp2_resp.bits.index  := RegEnable(p_idx, !stall)(index_sz-1,0)
   io.bp2_resp.bits.tag    := RegEnable(p_tag, !stall)(tag_sz-1,0)

   io.bp2_resp.bits.idx_csr  := idx_csr.io.value
   io.bp2_resp.bits.tag_csr1 := tag_csr1.io.value
   io.bp2_resp.bits.tag_csr2 := tag_csr2.io.value

   //------------------------------------------------------------
   // Update (Commit)


   assert(!(io.allocate.valid && io.update_counters.valid),
      "[tage-table] trying to allocate and update the counters simultaneously.")

   when (io.allocate.valid)
   {
      val a_idx = io.allocate.bits.index(index_sz-1,0)

      ubit_table.io.allocate(a_idx)
      tag_table.io.write(a_idx, io.allocate.bits.tag(tag_sz-1,0))

      counter_table.io.update.valid                 := Bool(true)
      counter_table.io.update.bits.index            := a_idx
      counter_table.io.update.bits.executed         := io.allocate.bits.executed.toBools
      counter_table.io.update.bits.was_mispredicted := Bool(true)
      counter_table.io.update.bits.takens           := io.allocate.bits.taken.toBools
      counter_table.io.update.bits.do_initialize    := Bool(true)

      debug_pc_table(a_idx) := io.allocate.bits.debug_pc
      debug_hist_ptr_table(a_idx) := io.allocate.bits.debug_hist_ptr(history_length-1,0)

      assert (a_idx < UInt(num_entries), "[TageTable] out of bounds index on allocation")
   }
   .elsewhen (io.update_counters.valid)
   {
      counter_table.io.update.valid                 := Bool(true)
      counter_table.io.update.bits.index            := io.update_counters.bits.index
      counter_table.io.update.bits.executed         := io.update_counters.bits.executed.toBools
      counter_table.io.update.bits.was_mispredicted := io.update_counters.bits.mispredicted
      counter_table.io.update.bits.takens           := io.update_counters.bits.taken.toBools
      counter_table.io.update.bits.do_initialize    := Bool(false)
   }

   when (io.update_usefulness.valid)
   {
      ubit_table.io.update(io.update_usefulness.bits.index(index_sz-1,0), io.update_usefulness.bits.inc)
   }

   val ub_read_idx = io.usefulness_req_idx(index_sz-1,0)
   ubit_table.io.s0_read_idx := ub_read_idx
   io.usefulness_resp := ubit_table.io.s2_is_useful

   when (io.degrade_usefulness_valid)
   {
      ubit_table.io.degrade()
   }
}

