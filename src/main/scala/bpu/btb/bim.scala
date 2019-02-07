//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Bimodal Predictor Table
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// BIM is a table of 2-bit counters.
//
// Stages:
//    * S0 -- receive address to predict on
//    * S1 -- perform lookup
//    * S2 -- return the read data
//
// TODO:
//    - Add reset FSM
//    - Parameterize Pbit:Hbit ratio

package boom.bpu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import boom.common._
import boom.exu._

case class BimParameters(
   nSets: Int = 1024, // how many sets (conceptually) should we have?
   nBanks: Int = 2, // how many banks should we have? Reduces dropped updates.
   nResetLagCycles: Int = 128, // how many cycles after reset should we start initialization?
   nUpdateQueueEntries: Int = 4,
   nWriteQueueEntries: Int = 4
)

trait HasBimParameters extends HasBoomCoreParameters
{
   val bimParams = boomParams.bim
   val nSets = bimParams.nSets
   val nBanks = bimParams.nBanks
   val nResetLagCycles = bimParams.nResetLagCycles
   val nUpdateQueueEntries = bimParams.nUpdateQueueEntries
   val nWriteQueueEntries = bimParams.nWriteQueueEntries

   val idx_sz = log2Ceil(nSets)
   val row_idx_sz = log2Ceil(nSets)-log2Ceil(nBanks)
   val row_sz = fetchWidth*2
}

abstract class BimBundle(implicit val p: Parameters) extends freechips.rocketchip.util.ParameterizedBundle()(p)
  with HasBimParameters

// The output from the BIM table.
class BimResp(implicit p: Parameters) extends BimBundle()(p)
{
   val rowdata = UInt(row_sz.W)
   val entry_idx = UInt(log2Ceil(nSets).W) // what (logical) entry in the set is the prediction coming from?

   def isTaken(cfi_idx: UInt) =
   {
      val cntr = getCounterValue(cfi_idx)
      val taken = cntr(1)
      taken
   }

   def getCounterValue(cfi_idx: UInt) =
   {
      val cntr = (rowdata >> (cfi_idx << 1.U)) & 0x3.U
      cntr
   }

   // Get a fetchWidth length bit-vector of taken/not-takens.
   def getTakens(): UInt =
   {
      val takens = WireInit(VecInit(Seq.fill(fetchWidth){false.B}))
      for (i <- 0 until fetchWidth)
      {
         // assumes 2-bits per branch.
         takens(i) := rowdata(2*i+1)
      }
      takens.asUInt
   }
}

// What do we store in the FTQ for updating BIM later?
// Only store one branch worth of info.
class BimStorage(implicit p: Parameters) extends BimBundle()(p)
{
   val value = UInt(2.W) // save the old value -- needed for updating entry.
   val entry_idx = UInt(log2Ceil(nSets).W) // what (logical) entry in the set is the prediction coming from?

   // TODO make sure these two signals aren't stored-- push them into CfiMissInfo instead.
   val br_seen = Bool() // Track that there was a branch in the fetch packet (this storage info is valid).
   val cfi_idx = UInt(log2Ceil(fetchWidth).W)

   def isTaken = value(1)
}

class BimUpdate(implicit p: Parameters) extends BimBundle()(p)
{
   val entry_idx = UInt(log2Ceil(nSets).W)
   val cfi_idx = UInt(log2Ceil(fetchWidth).W)
   val cntr_value = UInt(2.W)
   val mispredicted = Bool()
   val taken = Bool()
}

class BimWrite(implicit p: Parameters) extends BimBundle()(p)
{
   val addr = UInt(row_idx_sz.W)
   val data = UInt(row_sz.W)
   val mask = UInt(row_sz.W)
}

class BimodalTable(implicit p: Parameters) extends BoomModule()(p) with HasBimParameters
{
   val io = IO(new Bundle
   {
      // req.valid is false if stalling (aka, we won't read and use BTB results, on cycle S1).
      // req.bits.addr is available on cycle S0.
      // resp is expected on cycle S2.
      val req = Flipped(Valid(new PCReq))
      val resp = Valid(new BimResp)
      // Reset the table to some initialization state.
      val do_reset = Input(Bool())

      // supress S1/upcoming S2 valids.
      val flush = Input(Bool())

      val update = Flipped(Valid(new BimUpdate))
   })

   // Which (conceptual) index do we map to?
   private def getIdx (addr: UInt): UInt = addr >> log2Ceil(fetchWidth*coreInstBytes)
   // Which physical row do we map to?
   private def getRowFromIdx (idx: UInt): UInt = idx >> log2Ceil(nBanks)
   // Which physical bank do we map to?
   // TODO which bits are the best to get the bank from?
   private def getBankFromIdx (idx: UInt): UInt = idx(log2Ceil(nBanks)-1, 0)

   // for initializing the BIM, this is the value to reset the row to.
   private def initRowValue (): Vec[Bool] =
   {
      val row = Wire(UInt(row_sz.W))
      row := Fill(fetchWidth, 2.U)
      VecInit(row.toBools)
   }

   private def generateWriteInfo(update: BimUpdate): (UInt, UInt, UInt) =
   {
      val row_addr = getRowFromIdx(update.entry_idx)
      val shamt = update.cfi_idx << 1.U
      val mask = 0x3.U << shamt
      val next = updateCounter(update.cntr_value, update.taken)
      val data = next << shamt

      (row_addr, data, mask)
   }

   // Given old counter value, provide the new counter value.
   private def updateCounter(cntr: UInt, taken: Bool): UInt =
   {
      val next = Wire(UInt(2.W))
      next :=
         Mux(taken && cntr =/= 3.U, cntr + 1.U,
         Mux(!taken && cntr =/= 0.U, cntr - 1.U,
            cntr))
      next
   }

   // Pick out the old counter value from a full row and increment it.
   // Return the new row.
   private def updateCounterInRow(old_row: UInt, cfi_idx: UInt, taken: Bool): UInt =
   {
      val row = Wire(UInt(row_sz.W))
      val shamt = cfi_idx << 1.U
      val mask = Wire(UInt(row_sz.W))
      mask := ~(0x3.U << shamt)
      val old_cntr = (old_row >> shamt) & 0x3.U
      val new_cntr = updateCounter(old_cntr, taken)
      row := (old_row & mask) | (new_cntr << shamt)
      row
   }

   require (nBanks >= 2)
   require (isPow2(nBanks))

   val stall = !io.req.valid
   // Logical Index gets broken down into RowIdx and BankIdx.
   val s0_logical_idx = Wire(UInt(idx_sz.W))
   val last_idx = RegNext(s0_logical_idx)
   val new_idx = getIdx(io.req.bits.addr)
   s0_logical_idx := Mux(stall, last_idx, new_idx)
   val s0_bank_idx = getBankFromIdx(s0_logical_idx)
   val s2_logical_idx = RegNext(RegNext(s0_logical_idx))
   val s2_bank_idx = getBankFromIdx(s2_logical_idx)

   if (DEBUG_PRINTF)
   {
      printf("BIM: fetchpc: 0x%x, idx:[%d,%d,%d] s2_resp_idx: %d\n",
         io.req.bits.addr,
         s0_logical_idx,
         s0_bank_idx,
         getRowFromIdx(s0_logical_idx),
         io.resp.bits.entry_idx
         )
   }

   // prediction
   val s2_read_out = Reg(Vec(nBanks, UInt(row_sz.W)))
   val s2_conflict = RegInit(VecInit(Seq.fill(nBanks){false.B}))

   // updates
   val r_update = Pipe(io.update)

   // reset/initialization
   val s_reset :: s_wait :: s_clear :: s_idle :: Nil = Enum(4)
   val fsm_state = RegInit(s_reset)
   val (lag_counter, lag_done) = Counter(fsm_state === s_wait, nResetLagCycles)
   val (clear_row_addr, clear_done) = Counter(fsm_state === s_clear, nSets/nBanks)

   for (w <- 0 until nBanks)
   {
      val ram = SyncReadMem(nSets/nBanks, Vec(row_sz, Bool()))
      ram.suggestName("bimDataArray")

      val ren = Wire(Bool())
      val s2_rmw_valid = Wire(Bool())

      // Does the predictor want read access? Give him his read access then.
      val p_will_read = s0_bank_idx === w.U && io.req.valid

      // Update Queue.
      val uq = Module(new Queue(new BimUpdate(), entries=nUpdateQueueEntries))
      // Write Queue.
      val wq = Module(new Queue(new BimWrite(), entries=nWriteQueueEntries))

      uq.io.enq.valid := r_update.valid && getBankFromIdx(r_update.bits.entry_idx) === w.U
      uq.io.enq.bits  := r_update.bits
      uq.io.deq.ready := wq.io.enq.ready && !s2_rmw_valid
      val (u_waddr, u_wdata, u_wmask) = generateWriteInfo(uq.io.deq.bits)

      // Read-Modify-Write update path.
      // If a misprediction occurs, read out the counters and then enqueue update onto wq.
      val s0_rmw_valid = uq.io.deq.fire()
      s2_rmw_valid    := RegNext(RegNext(s0_rmw_valid))
      val s2_rmw_row   = Wire(UInt(row_idx_sz.W))
      val s2_rmw_data  = Wire(UInt(row_sz.W))
      val s2_rmw_mask  = Wire(UInt(row_sz.W))

      wq.io.enq.valid     := (uq.io.deq.valid && !uq.io.deq.bits.mispredicted) || s2_rmw_valid
      wq.io.enq.bits.addr := Mux(s2_rmw_valid, s2_rmw_row,  u_waddr)
      wq.io.enq.bits.data := Mux(s2_rmw_valid, s2_rmw_data, u_wdata)
      wq.io.enq.bits.mask := Mux(s2_rmw_valid, s2_rmw_mask, u_wmask)

      wq.io.deq.ready := !p_will_read

      if (DEBUG_PRINTF) printf("BIM bank[" + w + "] (r:%d ", ren)

      val wen = ((wq.io.deq.valid && !ren) || fsm_state === s_clear)
      when (wen)
      {
         val waddr = Mux(fsm_state === s_clear, clear_row_addr, wq.io.deq.bits.addr)
         val wdata = Mux(fsm_state === s_clear, initRowValue(), VecInit(wq.io.deq.bits.data.toBools))
         val wmask = Mux(fsm_state === s_clear, Fill(row_sz, 1.U), wq.io.deq.bits.mask).toBools

         ram.write(waddr, wdata, wmask)
         if (DEBUG_PRINTF) printf("w:W (%d==%x) %x %x ", waddr, waddr, wdata.asUInt, VecInit(wmask).asUInt)
      }
      .otherwise
      {
         if (DEBUG_PRINTF) printf("w:----------")
      }

      // read (either for prediction or rmw for updates)
      // Send address in S0
      // Read array in S1
      // Flop into S2
      ren := (s0_bank_idx === w.U && io.req.valid) || s0_rmw_valid
      val rrow = Mux(s0_rmw_valid,
         getRowFromIdx(uq.io.deq.bits.entry_idx),
         getRowFromIdx(s0_logical_idx))
      val s0_rconflict = s0_rmw_valid && (io.req.valid && s0_bank_idx === w.U)
      s2_conflict(w) := RegNext(s0_rconflict)
      s2_read_out(w) := ram.read(rrow, ren && !wen).asUInt

      val s2_rmw_cfi_idx = RegNext(RegNext(uq.io.deq.bits.cfi_idx))
      s2_rmw_row  := RegNext(RegNext(getRowFromIdx(uq.io.deq.bits.entry_idx)))
      s2_rmw_data := updateCounterInRow(
         s2_read_out(w),
         s2_rmw_cfi_idx,
         RegNext(RegNext(uq.io.deq.bits.taken)))
      s2_rmw_mask := 0x3.U << (s2_rmw_cfi_idx << 1.U)

      assert(!(uq.io.deq.valid && getBankFromIdx(uq.io.deq.bits.entry_idx) =/= w.U))

      if (DEBUG_PRINTF)
      {
         printf("uq.enq:(%d,%d) uq.deq:(%d,%d), s0_rmw:%d s2_out: %x rmw_data: %x  wq:%d,%d\n",
            uq.io.enq.valid,
            uq.io.enq.ready,
            uq.io.deq.valid,
            uq.io.deq.ready,
            s0_rmw_valid,
            s2_read_out(w),
            s2_rmw_data.asUInt,
            wq.io.enq.valid,
            wq.io.deq.valid)
      }

   }

   //************************************************
   // Output.

   val s2_fsm_idle = RegNext(RegNext(fsm_state === s_idle, false.B), false.B)
   io.resp.valid := !Mux1H(UIntToOH(s2_bank_idx), s2_conflict) && s2_fsm_idle
   io.resp.bits.rowdata := Mux1H(UIntToOH(s2_bank_idx), s2_read_out)
   io.resp.bits.entry_idx := s2_logical_idx

   //************************************************
   // Reset FSM.

   switch (fsm_state)
   {
      is (s_reset)
      {
         fsm_state := s_wait
      }
      is (s_wait)
      {
         when (lag_done)
         {
            fsm_state := s_clear
         }
      }
      is (s_clear)
      {
         when (clear_done)
         {
            fsm_state := s_idle
         }
      }
      is (s_idle)
      {
         when (io.do_reset)
         {
            fsm_state := s_clear
         }
      }
   }

   //************************************************
   // Debug.
   // Trust me, I just work.

   val size_kbits = nSets * fetchWidth * 2/1024 // assumes 2 bits / fetchWidth
   override def toString: String =
      "\n   ==BIM==" +
      "\n   (" + size_kbits + " Kbits = " + size_kbits/8 + " kB) Bimodal Table (" +
      nSets + " entries across " + nBanks + " banks)"
}

