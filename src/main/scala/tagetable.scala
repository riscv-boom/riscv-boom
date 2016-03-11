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
import Node._
import cde.Parameters

import rocket.Str


class TageTableResp(fetch_width: Int, history_length: Int, index_length: Int, tag_sz: Int)(implicit p: Parameters)
   extends BoomBundle()(p)
{
//   val takens = Bits(width = fetch_width)
   val taken = Bool() // TODO make superscalar

   val index = Bits(width = index_length)
   val tag = Bits(width = tag_sz)

   val tag_csr1 = Bits(width = history_length) // TODO BUG XXX a total NOP
   val tag_csr2 = Bits(width = history_length) // TODO BUG XXX a total NOP
   val idx_csr = Bits(width = history_length) // TODO BUG XXX a total NOP


   override def cloneType: this.type = new TageTableResp(fetch_width, history_length, index_length, tag_sz).asInstanceOf[this.type]
}

class TageIndex(index_sz: Int) extends Bundle
{
   val index = UInt(width = index_sz)
   override def cloneType: this.type = new TageIndex(index_sz).asInstanceOf[this.type]
}

class TageAllocateEntryInfo(index_sz: Int, tag_sz: Int, hist_sz: Int) extends Bundle //TageIndex(index_sz)
{
   val index = UInt(width = index_sz)
   val tag = Bits(width=tag_sz)
   val taken = Bool()
   val debug_pc = UInt(width=32)
   val debug_hist = UInt(width=hist_sz)
   override def cloneType: this.type = new TageAllocateEntryInfo(index_sz, tag_sz, hist_sz).asInstanceOf[this.type]
}

class TageUpdateCountersInfo(index_sz: Int) extends Bundle //extends TageIndex(index_sz)
{
   val index = UInt(width = index_sz)
   val taken = Bool()
   val alt_agrees = Bool()
   val last_pred = Bool()
   override def cloneType: this.type = new TageUpdateCountersInfo(index_sz).asInstanceOf[this.type]
}

class TageTableIo(
   fetch_width: Int,
   num_entries: Int,
   history_length: Int,
   tag_sz: Int,
   counter_sz: Int
   )(implicit p: Parameters) extends BoomBundle()(p)
{
   private val index_sz = log2Up(num_entries)

   // instruction fetch - request prediction
   val if_req_pc = UInt(INPUT, width = 64)
   val if_req_history = Bits(INPUT, width = history_length)

   // bp2 - send prediction to bpd pipeline
   val bp2_resp = new ValidIO(new TageTableResp(fetch_width, history_length, log2Up(num_entries), tag_sz))

   // bp2 - update histories speculatively
   val bp2_update_history = (new ValidIO(new GHistUpdate)).flip

   // execute - perform updates on misprediction (reset the histories)
   val exe_update_history = new ValidIO(new BpdResp())

   // commit - update predictor tables (allocate entry)
   val allocate = (new ValidIO(new TageAllocateEntryInfo(index_sz, tag_sz, history_length))).flip
   def AllocateNewEntry(idx: UInt, tag: UInt, taken: Bool, pc: UInt, hist: Bits) =
   {
      this.allocate.valid := Bool(true)
      this.allocate.bits.index := idx
      this.allocate.bits.tag :=tag
      this.allocate.bits.taken :=taken
      this.allocate.bits.debug_pc := pc
      this.allocate.bits.debug_hist :=hist
   }

   // commit - update predictor tables (update counters, u-bits)
   val update_counters = (new ValidIO(new TageUpdateCountersInfo(index_sz))).flip
   def UpdateCounters(idx: UInt, taken: Bool, alt_agrees: Bool, last_pred: Bool) =
   {
      this.update_counters.valid := Bool(true)
      this.update_counters.bits.index := idx
      this.update_counters.bits.taken := taken
      this.update_counters.bits.alt_agrees := alt_agrees
      this.update_counters.bits.last_pred := last_pred
   }

   // commit - update predictor tables (decrement u-bits)
   val decrement_usefulness = (new ValidIO(new TageIndex(index_sz))).flip
   def DecrementUseful(idx: UInt) =
   {
      this.decrement_usefulness.valid := Bool(true)
      this.decrement_usefulness.bits.index := idx
   }

   val usefulness_req_idx = Bits(INPUT, index_sz)
   val usefulness_resp = UInt(OUTPUT, 2) // TODO u-bit_sz
   def GetUsefulness(idx: UInt) =
   {
      this.usefulness_req_idx := idx
      this.usefulness_resp
   }

   def InitializeIo(dummy: Int=0) =
   {
      this.allocate.valid := Bool(false)
      this.update_counters.valid := Bool(false)
      this.decrement_usefulness.valid := Bool(false)
      // TODO better way to provide initial values?
      this.allocate.bits.index := UInt(0)
      this.allocate.bits.tag := UInt(0)
      this.allocate.bits.taken := UInt(0)
      this.allocate.bits.debug_pc := UInt(0)
      this.allocate.bits.debug_hist := UInt(0)
      this.update_counters.bits.index := UInt(0)
      this.update_counters.bits.taken := Bool(false)
      this.update_counters.bits.alt_agrees := Bool(false)
      this.update_counters.bits.last_pred := Bool(false)
      this.decrement_usefulness.bits.index := UInt(0)
      this.usefulness_req_idx := UInt(0)
   }

   override def cloneType: this.type = new TageTableIo(
      fetch_width, num_entries, history_length, tag_sz, counter_sz).asInstanceOf[this.type]
}

class TageTable(
   fetch_width: Int,
   num_entries: Int,
   history_length: Int,
   tag_sz: Int,
   counter_sz: Int
   )(implicit p: Parameters) extends BoomModule()(p)
{
   val index_sz = log2Up(num_entries)

   val io = new TageTableIo(fetch_width, num_entries, history_length, tag_sz, counter_sz)

   private val CNTR_MAX = (1 << counter_sz) - 1
   private val CNTR_WEAK_TAKEN = 1 << (counter_sz-1)
   private val CNTR_WEAK_NOTTAKEN = CNTR_WEAK_TAKEN - 1
   private val UBIT_SZ = 2
   private val UBIT_MAX = (1 << UBIT_SZ) - 1
   private val UBIT_INIT_VALUE = 1

   println("\t    TageTable - "
      + num_entries + " entries, "
      + history_length + " bits of history, "
      + tag_sz + "-bit tags, "
      + counter_sz + "-bit counters (max value=" + CNTR_MAX + ")")

   //------------------------------------------------------------
   // State
   val counter_table = Mem(num_entries, Bits(width = counter_sz))
   val tag_table     = Mem(num_entries, Bits(width = tag_sz))
   val ubit_table    = Mem(num_entries, Bits(width = UBIT_SZ))
   val debug_pc_table= Mem(num_entries, UInt(width = 32))
   val debug_hist_table=Mem(num_entries,Bits(width = history_length))

   //history ghistory
   //csr idx_csr
   //csr tag_csr1
   //csr tag_csr2

   //------------------------------------------------------------
   // functions

   //updateHistory()
   //clearUBit() TODO XXX


   // TODO XXX implement Fold in TAGE
   private def Fold (input: Bits, compressed_length: Int) =
   {
      if (history_length < compressed_length) input
      else input(compressed_length-1, 0)
   }

   private def IdxHash (addr: UInt, hist: Bits) =
   {
//      (addr >> UInt(4))(index_sz-1,0)
      ((addr >> UInt(log2Up(fetch_width*coreInstBytes))) ^ Fold(hist, history_length))(index_sz-1,0)
   }

   private def TagHash (addr: UInt, hist: Bits) =
   {
      // the tag is computed by pc[7:0] ^ CSR1[7:0] ^ (CSR2[6:0]<<1).
//      (addr >> UInt(4))(tag_sz-1,0)
      (addr >> UInt(log2Up(fetch_width*coreInstBytes)))(tag_sz-1,0) ^
         Fold(hist, history_length) ^
         (Fold(hist, history_length-1) << UInt(1))
   }

   // saturating increment or decrement
   // TODO throws an uninitialized wire error :(
   //def SatUpdate(value: UInt, inc: Bool, max: Int): UInt =
   //{
   //   val ret = Wire(UInt())
   //   ret := value
   //   when (inc && value =/= UInt(max))
   //   {
   //      ret := (value + UInt(1))
   //   }
   //   .elsewhen (!inc && value =/= UInt(0))
   //   {
   //      ret := (value - UInt(1))
   //   }
   //   ret
   //}


   //------------------------------------------------------------
   // Get Prediction

   val p_idx   = IdxHash(io.if_req_pc, io.if_req_history)
   val p_tag   = TagHash(io.if_req_pc, io.if_req_history)
   val counter = counter_table(p_idx)
   val tag     = tag_table(p_idx)
   val bp2_tag_hit = RegNext(RegNext(tag)) === RegNext(RegNext(p_tag)) // TODO delay this until bp2 stage

   io.bp2_resp.valid := bp2_tag_hit
   io.bp2_resp.bits.taken := RegNext(RegNext(counter >= UInt(CNTR_WEAK_TAKEN)))
   require (fetch_width == 1)//hack for now
   io.bp2_resp.bits.index := RegNext(RegNext(p_idx))
   io.bp2_resp.bits.tag := RegNext(RegNext(p_tag))

   //------------------------------------------------------------
   // Update (Branch Resolution)

   // only update history (CSRs)

   when (io.bp2_update_history.valid)
   {
      // TODO XXX once CSRs have been implemented, support updating them
      // shift all of the histories, CSRs over by one bit.
      // io.bp2_update_history.bits.taken
   }


   //------------------------------------------------------------
   // Update (Commit)

   when (io.allocate.valid)
   {
      val a_idx = io.allocate.bits.index
      assert (a_idx < UInt(num_entries), "[TageTable] out of bounds index on allocation")
      ubit_table(a_idx)    := UInt(UBIT_INIT_VALUE)
      tag_table(a_idx)     := io.allocate.bits.tag
      counter_table(a_idx) := Mux(io.allocate.bits.taken, UInt(CNTR_WEAK_TAKEN), UInt(CNTR_WEAK_NOTTAKEN))
      assert (ubit_table(a_idx) === Bits(0), "[TageTable] Tried to allocate a useful entry")

      debug_pc_table(a_idx) := io.allocate.bits.debug_pc
      debug_hist_table(a_idx) := io.allocate.bits.debug_hist
   }

   when (io.update_counters.valid)
   {
      val u_idx = io.update_counters.bits.index

      // TODO can we abstract the Counter update into a function?
      val inc = io.update_counters.bits.taken
      val value = counter_table(u_idx)
      when (inc && value =/= UInt(CNTR_MAX))
      {
         counter_table(u_idx) := value + UInt(1)
      }
      .elsewhen (!inc && value =/= UInt(0))
      {
         counter_table(u_idx) := value - UInt(1)
      }
      when (!io.update_counters.bits.alt_agrees)
      {
         val correct = (io.update_counters.bits.last_pred === io.update_counters.bits.taken)
         val u_value = ubit_table(u_idx)
         //ubit_table(u_idx) := SatUpdate(ubit_table(u_idx), correct, UBIT_MAX)
         when (correct && u_value =/= UInt(UBIT_MAX))
         {
            ubit_table(u_idx) := u_value + UInt(1)
            if (DEBUG_PRINTF_TAGE)
               printf("Alt disagreemtn - Updating UBit + 1  ")
         }
         .elsewhen (!correct && u_value =/= UInt(0))
         {
            ubit_table(u_idx) := u_value - UInt(1)
            if (DEBUG_PRINTF_TAGE)
               printf("Alt disagreemtn - Updating UBit - 1  ")
         }
         .otherwise
         {
            if (DEBUG_PRINTF_TAGE)
               printf("Alt disagreemtn - no change in ubit!, correct=%d, u_bit=%d  ", correct, u_value)
         }
      }
   }

   when (io.decrement_usefulness.valid)
   {
      val ub_idx = io.decrement_usefulness.bits.index
      val u = ubit_table(ub_idx)
      ubit_table(ub_idx) := Mux(u > UInt(0), u - UInt(1), UInt(0))
   }

   io.usefulness_resp := ubit_table(io.usefulness_req_idx)

   //------------------------------------------------------------
   // Debug/Visualize

   if (DEBUG_PRINTF_TAGE)
   {
      printf("TAGETable: PC: 0x%x history: 0x%x, tag[%d]=0x%x, p_tag=0x%x " + mgt + "%s\n" + end,
         io.if_req_pc,
         io.if_req_history + UInt(0,64),
         p_idx,
         tag + UInt(0,64),
         TagHash(io.if_req_pc, io.if_req_history),
         Mux(tag === p_tag, Str("HIT!"), Str(" "))
      )

      for (i <- 0 to num_entries-1 by 4)
      {
         val lst = Seq(4,6,8,12,13)
         for (j <- 0 until 4)
         {
            printf("(%d) [tag=0x%x] [c=%d] [u=%d] " + red + "PC=0x%x hist=0x%x " + end,
               UInt(i+j,4),
               tag_table(UInt(i+j)) & UInt(0xffff),
               counter_table(UInt(i+j)),
               ubit_table(UInt(i+j)),
               (debug_pc_table(UInt(i+j)) & UInt(0xff))(11,0),
               (debug_hist_table(UInt(i+j)) & UInt(0xffff))(15,0)
               )
         }
         printf("\n")
      }
      printf("\n")

   }

}

