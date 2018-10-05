package boom.vec

import chisel3._
import chisel3.util.{log2Ceil, PriorityEncoder, Fill, UIntToOH}
import freechips.rocketchip.config.Parameters
import boom.common._
import boom.util._
import boom.exu.{BrResolutionInfo, RenameFreeListHelper}

class ScalarOpBuffer(implicit p: Parameters) extends BoomModule()(p) with freechips.rocketchip.rocket.constants.VecCfgConstants
{
   val buf_size  = scalar_op_buff_sz

   val io = IO(new Bundle {
      val r_idx   = Input(UInt(width=log2Ceil(buf_size).W))
      val r_data  = Output(Vec(3, UInt(width=xLen.W)))

      // TODO_Vec: Should we write operands one at a time or three at a time?
      // Current behavior is one at a time
      val w_valid = Input(Vec(2, Bool()))
      val w_idx   = Input(Vec(2, UInt(width=log2Ceil(buf_size).W)))
      val w_op_id = Input(Vec(2, UInt(width=2.W)))
      val w_data  = Input(Vec(2, UInt(width=xLen.W)))

   })

   val buffer = SyncReadMem(buf_size, Vec(3, UInt(width=xLen.W)))

   // Write operand into buffer
   for (i <- 0 until 2) {
      when (io.w_valid(i)) {
         val mask = UIntToOH(io.w_op_id(i))
         buffer.write(io.w_idx(i), VecInit(Seq.fill(3){io.w_data(i)}), (0 until 3) map {i => mask(i)})
      }
   }

   io.r_data := RegNext(buffer.read(io.r_idx))
   // This is fragile, it matches the latency of the normal register-read pipeline
}
class ScalarOpFreeList(pl_width: Int)(implicit p: Parameters) extends BoomModule()(p)
      with freechips.rocketchip.rocket.constants.VecCfgConstants
{
   val buf_size = scalar_op_buff_sz

   val io = IO(new Bundle {
      val brinfo = Input(new BrResolutionInfo())
      val kill   = Input(Bool())

      val ren_will_fire = Input(Vec(pl_width, Bool()))
      val ren_uops      = Input(Vec(pl_width, new MicroOp()))
      val ren_br_vals   = Input(Vec(pl_width, Bool()))

      // These represent when entries are deallocated from the vector issue window
      // Vector window should retire one at a time
      val com_valids     = Input(Vec(pl_width, Bool()))
      val com_uops       = Input(Vec(pl_width, new MicroOp()))
      val com_rbk_valids = Input(Vec(pl_width, Bool()))

      val flush_pipeline = Input(Bool())

      // Requested Scalar Operand Buffer Index
      //val can_allocate = Vec(pl_width, Bool()).asOuput
      val req_scopb_idx  = Output(Vec(pl_width, UInt(width=log2Ceil(buf_size).W)))

      val can_allocate   = Output(Vec(pl_width, Bool()))
   })

   val freelist = Module(new RenameFreeListHelper(
      buf_size,
      pl_width))

   freelist.io.br_mispredict_val := io.brinfo.mispredict
   freelist.io.br_mispredict_tag := io.brinfo.tag
   freelist.io.flush_pipeline    := io.flush_pipeline

   for (w <- 0 until pl_width)
   {
      // TODO_Vec: Fix this logic
      freelist.io.req_preg_vals(w) := !io.kill &&
                                      io.ren_will_fire(w) &&
                                      io.ren_uops(w).use_vscopb


      freelist.io.enq_vals(w)      := io.com_valids(w) && io.com_uops(w).use_vscopb
      freelist.io.enq_pregs(w)     := io.com_uops(w).vscopb_idx

      freelist.io.ren_br_vals(w)   := io.ren_br_vals(w)
      freelist.io.ren_br_tags(w)   := io.ren_uops(w).br_tag

      // What does rolling back mean here? I don't think we ever roll back
      freelist.io.rollback_wens(w) := io.com_rbk_valids(w) && io.com_uops(w).use_vscopb
      freelist.io.rollback_pdsts(w):= io.com_uops(w).vscopb_idx

      freelist.io.com_wens(w)      := io.com_valids(w) && io.com_uops(w).use_vscopb
      freelist.io.com_uops(w)      := io.com_uops(w)



      io.req_scopb_idx(w) := freelist.io.req_pregs(w)
   }
   io.can_allocate := freelist.io.can_allocate

   //assert(freelist.io.can_allocate.reduce(_&&_), "We should always be able to allocate here")


}
