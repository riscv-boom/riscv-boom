//**************************************************************************
// RISCV Branch Predictor
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2015 Apr 28

package BOOM
{
 
import Chisel._
import Node._

// use this Bundle as information on the prediction (taken, not taken)
// pass down the pipeline with the micro-op to "remember" meta-data like
// agreement of predictors, etc.
class BrPrediction extends BOOMCoreBundle
{
   // (store both information on taken/not taken, and on agreement/disagreement)
   val taken = Bool()
//   val agreement   = Bool()
//   val local_taken = Bool()
//   val history = Bits() // TODO compress this out of the pipeline
   
   // I could probably do this in 2-bits
   // TODO optimize by using disagreement
   def isTaken(): Bool = taken
//   def isBrPdAgreement() = agreement
//   def isLocalPrTaken() = local_taken
}

class BrpdUpdate extends BOOMCoreBundle
{
   val pc         = UInt(width = vaddrBits)
   val mispredict = Bool()
   val taken      = Bool()
}
        
class BrPredictorIo extends BOOMCoreBundle
{
   val curr_pc = UInt(INPUT, xLen)
   val resp    = new BrPrediction().asOutput()
   val update  = Valid(new BrpdUpdate).flip
//   val update_wen      = Bool(INPUT)
//   val update_pc       = UInt(INPUT, xLen)
//   val update_taken    = Bool(INPUT)                  // what actually happened
//   val update_pred     = new BrPrediction().asInput() // the prediction info 
//   val update_pred     = prediction.asInput //new BrPrediction().asInput() // the prediction info 
}

abstract class BrPredictor extends Module with BOOMCoreParameters
{   
   val io = new BrPredictorIo
}

// simple 2bc BHT predictor (based on the MIPS R10K predictor)
class TwobcBrPredictor(num_entries: Int = 128) extends BrPredictor
{
   private def hash (addr: UInt) = (addr >> UInt(2))

   // TODO widen to fetch width
   // TODO make table synchronous
   val p_table = Mem(Bool(), num_entries) // prediction bits
   val h_table = Mem(Bool(), num_entries) // hysteresis bits

   io.resp.taken := p_table(hash(io.curr_pc))

   val u_idx = hash(io.update.bits.pc)
   when (io.update.valid)
   {
      when (io.update.bits.mispredict)
      {
         // only update p-table on misprediction
         p_table(u_idx) := h_table(u_idx)
      }
      h_table(u_idx) := io.update.bits.taken
   }



   //val counter_table = Module(new CounterTable(num_entries, counter_sz))
   //counter_table.io.predict_index := io.curr_pc(xLen-1,pc_lsb)
   //prediction                     := counter_table.io.predict_taken
   //// update counter table
   //counter_table.io.update_taken  := io.update_taken
   //counter_table.io.update_index  := io.update_pc(xLen-1,pc_lsb)
   //counter_table.io.update_wen    := io.update_wen
}

//class GlobalOnlyBrPredictor(num_entries: Int = 128, counter_sz: Int = 2, pc_lsb: Int = 2) extends BrPredictor(pc_lsb = pc_lsb)
//{
//   val global_history = Module(new HistoryRegister(log2Up(num_entries)))
//   val counter_table  =Module(new CounterTable(num_entries, counter_sz))
//
//   counter_table.io.predict_index := global_history.io.out.history.toUInt
//   val prediction = counter_table.io.predict_taken
//
//   // update counter table
//   counter_table.io.update_wen   := io.update_wen
//   counter_table.io.update_taken := io.update_taken
//   counter_table.io.update_index := global_history.io.out.history.toUInt
//
//   //update history register
//   global_history.io.in.update_wen  := io.update_wen
//   global_history.io.in.update_taken:= io.update_taken
//   
//   io.prediction_info.taken := prediction
//}
//
//class GShareBrPredictor(num_entries: Int = 128, counter_sz: Int = 2, pc_lsb: Int = 2) extends BrPredictor(pc_lsb = pc_lsb)
//{
//   val global_history = Reg(outType=Bits(width=log2Up(num_entries)))
//   val counter_table  = Module(new CounterTable(num_entries, counter_sz))
//
//   val prediction = counter_table.io.predict_taken
//   counter_table.io.predict_index := io.curr_pc(xLen-1,pc_lsb) ^ global_history.toUInt
//
//   // update counter table
//   counter_table.io.update_wen   := io.update_wen
//   counter_table.io.update_taken := io.update_taken
//   counter_table.io.update_index := io.update_pc(xLen-1,pc_lsb) ^ global_history.toUInt
//
//   //update history register
//   when (io.update_wen)
//   {
//      PerformShiftRegister(global_history, io.update_taken)
//   }
//   
//   io.prediction_info.taken := prediction
//}
//   
//class TournamentBrPredictor(num_bht_entries: Int = 128, counter_sz: Int = 2, num_lhist_entries: Int = 128, pc_lsb: Int = 2) extends BrPredictor(pc_lsb = pc_lsb)
//{
//   val prediction = Bool()
//   val agreement = Bool()
//   val local_prediction = Bool() 
//
//   val global_hist_sz  = 10
//   val global_history  = Reg(outType=Bits(width = global_hist_sz))
//   val g_counter_table = Module(new CounterTable(1 << global_hist_sz, counter_sz))
//   val a_counter_table = Module(new CounterTable(256, counter_sz))
//   val l_hist_table    = Vec.fill(num_lhist_entries) { Reg(outType=Bits(width = log2Up(num_bht_entries))) }
//   val l_counter_table = Module(new CounterTable(num_bht_entries, counter_sz+1))
//       
//   
//   val curr_pc_idx = io.curr_pc >> UInt(2)
//   val corr_pc_idx = io.update_pc >> UInt(2)
//   
//   // --global predictor--- 
//   g_counter_table.io.predict_index := global_history.toUInt
//   val global_prediction            = g_counter_table.io.predict_taken
//
//   // update counter table
//   g_counter_table.io.update_wen   := io.update_wen
//   g_counter_table.io.update_taken := io.update_taken
//   g_counter_table.io.update_index := global_history.toUInt
//
//   //update history register
//   when (io.update_wen)
//   {
//      PerformShiftRegister(global_history, io.update_taken)
//   }
//
//   // --arbiter predictor--- 
//
//   // decision time: use the global history, or the PC? (21264 used ghist)
//   //a_counter_table.io.predict_index := global_history.toUInt 
//   a_counter_table.io.predict_index := curr_pc_idx
//   val arbiter_prediction            = a_counter_table.io.predict_taken
//
//   //update arbiter only on disagreement
//   //bias 2'b11 towards local predictor
//   a_counter_table.io.update_wen   := io.update_wen && !(io.update_pred.agreement)
//   a_counter_table.io.update_taken := (io.update_pred.local_taken === io.update_taken)
//   a_counter_table.io.update_index := corr_pc_idx
//   
//    
//   // --local predictor--- 
//
//   val local_history = l_hist_table(curr_pc_idx)
//
//   // get prediction
//   l_counter_table.io.predict_index := local_history.toUInt
//   local_prediction := l_counter_table.io.predict_taken
//
//   val update_index = l_hist_table(corr_pc_idx)
//      
//   // update counter table
//   l_counter_table.io.update_wen   := io.update_wen
//   l_counter_table.io.update_taken := io.update_taken
//   l_counter_table.io.update_index := update_index.toUInt
//
//   // update an entry in the history table
//   when (io.update_wen)
//   {
//      PerformShiftRegister(l_hist_table(corr_pc_idx), io.update_taken)
//   }
//
//
//   io.prediction_info.taken       := Mux(arbiter_prediction, local_prediction, global_prediction)
//   io.prediction_info.agreement := !(local_prediction ^ global_prediction)
//   io.prediction_info.local_taken := local_prediction
//}
//
//
//                          
//class HistoryRegisterInputIo() extends Bundle()
//{
//   val update_wen   = Bool()
//   val update_taken = Bool()
//}
// 
//class HistoryRegisterOutputIo(hist_len: Int) extends Bundle()
//{
//   val history      = Bits(width = hist_len)
//   override def clone = new HistoryRegisterOutputIo(hist_len).asInstanceOf[this.type]
//}
//
//class HistoryRegisterIo(hist_len: Int) extends Bundle()
//{
//   val in  = new HistoryRegisterInputIo().asInput
//   val out = new HistoryRegisterOutputIo(hist_len).asOutput
//}
//
//class HistoryRegister(hist_len: Int) extends Module
//{
//   val io  = new HistoryRegisterIo(hist_len)
//
//   val reg_history = Reg(init=Bits(0,hist_len))
//   
//   when (io.in.update_wen)
//   {
//      reg_history := Cat(reg_history(hist_len-1,0), io.in.update_taken)
//   }
//
//   io.out.history := reg_history
//}
//
//
//
//class CounterTableIo(idx_sz: Int) extends Bundle()
//{
//   val predict_taken  = Bool(        OUTPUT)
//   val predict_index  = UInt(INPUT, idx_sz)
//   
//   val update_wen    = Bool(        INPUT)
//   val update_index  = UInt(INPUT, idx_sz)
//   val update_taken  = Bool(        INPUT)
//}
//
//
//class CounterTable(num_entries: Int, counter_sz: Int) extends Module
//{
//   val idx_sz      = log2Up(num_entries)
//   val io = new CounterTableIo(idx_sz)
//
//
//   // Treat the table of counters as a Memory with 2 read, 1 write port
//   //    1st read port for reading out current entry's value (for computing next value)
//   //    2nd read port for prediction
//   //        write port for updating the table 
//   val counter_table = Mem(out=UInt(width=counter_sz),n=num_entries)  
//   
//   val update_index = io.update_index(idx_sz-1,0)
//   val predict_index = io.predict_index(idx_sz-1,0)
//
//   // compute updated value
//   
//   // forward declare a Wire of type UInt 
//   val next_counter_val = UInt(width = counter_sz) 
//
//   // 1st Read Port for reading out 
//   val curr_counter_val = counter_table.read(update_index)
//   
//   // Set default
//   next_counter_val := curr_counter_val
//
//   // remember, counters are saturating (so watch for wrap-around)
//   when (io.update_taken && curr_counter_val != ~Bits(0,counter_sz))
//   {
//      next_counter_val := curr_counter_val + UInt(1,counter_sz)
//
//   }
//   .elsewhen(!io.update_taken && curr_counter_val != Bits(0,counter_sz))
//   {
//      next_counter_val := curr_counter_val - UInt(1,counter_sz)
//   }
//
//   
//   // Write Port to the counter table
//   when (io.update_wen)
//   {
//      counter_table(update_index) := next_counter_val
//   }
//
//   // 2nd Read Port (to get prediction)
//   val counter_entry = counter_table.read(predict_index)
//   io.predict_taken  := counter_entry(counter_sz-1).toBool
//}
 

}
 
