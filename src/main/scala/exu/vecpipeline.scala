//******************************************************************************
// Copyright (c) 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Floating Point Datapath Pipeline
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Hankun Zhao

// The vector issue window, regfile, and arithmetic units are all handled here.

package boom.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket
import freechips.rocketchip.tile
import freechips.rocketchip.util._
import boom.exu.FUConstants._
import boom.common._
import boom.util._

class VecPipeline(implicit p: Parameters) extends BoomModule()(p) with tile.HasFPUParameters
with freechips.rocketchip.rocket.constants.VecCfgConstants
with Packing
{
  val vecIssueParams = issueParams.find(_.iqType == IQT_VEC.litValue).get
  val num_ll_ports = 1 // TODO_VEC: add ll wb ports
  val num_wakeup_ports = vecIssueParams.issueWidth
  val vec_preg_sz = log2Up(numVecPhysRegs)

  val io = new Bundle
  {
     val brinfo         = Input(new BrResolutionInfo())
     val flush_pipeline = Input(Bool())
     val fcsr_rm = Input(UInt(width=freechips.rocketchip.tile.FPConstants.RM_SZ.W))
     // TODO: Add inputs from rocket CSRFile

     val dis_valids     = Input(Vec(DISPATCH_WIDTH, Bool()))
     val dis_uops       = Input(Vec(DISPATCH_WIDTH, new MicroOp()))
     val dis_readys     = Output(Vec(DISPATCH_WIDTH, Bool()))

     val ll_wport       = Flipped(Decoupled(new ExeUnitResp(128))) // from memory unit
     val tosdq          = new DecoupledIO(new MicroOpWithData(128))
     val fromint        = Flipped(Decoupled(new ExeUnitResp(xLen))) // from integer RF
     val fromfp         = Flipped(Decoupled(new ExeUnitResp(xLen))) // from fp RF.
     val toint          = new DecoupledIO(new ExeUnitResp(xLen))
     val memreq         = new DecoupledIO(new FuncUnitReq(xLen)) // Indexed load uops are issued here, executed in integer pipeline

     val wakeups        = Vec(num_wakeup_ports, Valid(new ExeUnitResp(128)))
     val wb_valids      = Input(Vec(num_wakeup_ports, Bool()))
     val vb_pdsts       = Input(Vec(num_wakeup_ports, UInt(width=vec_preg_sz.W)))

     val debug_tsc_reg  = Input(UInt(width=128.W))
     val vl             = Input(UInt(width=VL_SZ.W))

     val retire_valids    = Output(Bool())
     val retire_uops      = Output(new MicroOp())

     val lsu_stq_head      = Input(UInt())
  } 

   val exe_units = new boom.exu.ExecutionUnits(vec = true)
   val issue_unit = Module(new IssueUnitCollasping(issueParams.find(_.iqType == IQT_VEC.litValue).get, true,
      true,
      num_wakeup_ports)) // TODO_VEC: Make this a VectorIssueUnit
   assert(issue_unit.issue_width == 1, "Issue width of 1 supported only")
   val vregfile = Module(new VectorRegisterFileBehavorial(numVecRegFileRows,
      exe_units.withFilter(_.uses_iss_unit).map(e=>e.num_rf_read_ports).sum,
      exe_units.withFilter(_.uses_iss_unit).map(e=>e.num_rf_write_ports).sum, // TODO_VEC: Subtract write ports to IRF, FRF
      128,
      exe_units.bypassable_write_port_mask
   ))
   assert(exe_units.num_total_bypass_ports == 0, "Vector pipeline does not support bypassing")
   val vregister_read = Module(new VectorRegisterRead(
      issue_unit.issue_width,
      exe_units.withFilter(_.uses_iss_unit).map(_.supportedFuncUnits),
      128))

   val vscalaropbuffer = Module(new ScalarOpBuffer())

   require (exe_units.withFilter(_.uses_iss_unit).map(x=>x).length == issue_unit.issue_width)
   require (exe_units.map(_.num_rf_write_ports).sum == num_wakeup_ports)
   require (exe_units.withFilter(_.uses_iss_unit).map(e=>
      e.num_rf_write_ports).sum == num_wakeup_ports)


   val tosdq = Module(new Queue(new MicroOpWithData(128), 4))

   // Todo_vec add checking for num write ports and number of functional units which use the issue unit

   val iss_valids = Wire(Vec(exe_units.withFilter(_.uses_iss_unit).map(x=>x).length, Bool()))
   val iss_uops   = Wire(Vec(exe_units.withFilter(_.uses_iss_unit).map(x=>x).length, new MicroOp()))

   issue_unit.io.tsc_reg := io.debug_tsc_reg
   issue_unit.io.brinfo := io.brinfo
   issue_unit.io.flush_pipeline := io.flush_pipeline

   // Don't support ld-hit speculation fo FP window
   issue_unit.io.mem_ldSpecWakeup.valid := false.B
   issue_unit.io.mem_ldSpecWakeup.bits  := 0.U
   issue_unit.io.sxt_ldMiss             := false.B

   issue_unit.io.vl := io.vl
   issue_unit.io.lsu_stq_head      := io.lsu_stq_head

   issue_unit.io.fromfp            <> io.fromfp
   issue_unit.io.fromint           <> io.fromint

   io.retire_valids                := issue_unit.io.retire_valids
   io.retire_uops                  := issue_unit.io.retire_uops

   vscalaropbuffer.io.w_valid(0)   := io.fromfp.valid
   vscalaropbuffer.io.w_idx(0)     := io.fromfp.bits.uop.vscopb_idx
   vscalaropbuffer.io.w_op_id(0)   := io.fromfp.bits.uop.pdst
   vscalaropbuffer.io.w_data(0)    := io.fromfp.bits.data

   vscalaropbuffer.io.w_valid(1)   := io.fromint.valid
   vscalaropbuffer.io.w_idx(1)     := io.fromint.bits.uop.vscopb_idx
   vscalaropbuffer.io.w_op_id(1)   := io.fromint.bits.uop.pdst
   vscalaropbuffer.io.w_data(1)    := io.fromint.bits.data

   require (exe_units.num_total_bypass_ports == 0)


   //-------------------------------------------------------------
   // **** Dispatch Stage ****
   //-------------------------------------------------------------

   // Input (Dispatch)
   for (w <- 0 until DISPATCH_WIDTH)
   {
      issue_unit.io.dis_valids(w) := io.dis_valids(w) && io.dis_uops(w).iqtype === issue_unit.iqType.U
      issue_unit.io.dis_uops(w) := io.dis_uops(w)

      when (io.dis_uops(w).vec_val && io.dis_uops(w).is_store) {
         issue_unit.io.dis_valids(w)          := io.dis_valids(w)
         issue_unit.io.dis_uops(w).fu_code    := io.dis_uops(w).fu_code | FU_V2I
         when (io.dis_uops(w).uopc =/= uopVSTX) {
            issue_unit.io.dis_uops(w).lrs1_rtype := RT_X
            issue_unit.io.dis_uops(w).prs1_busy  := false.B
         }
         when (io.dis_uops(w).uopc === uopVSTS) {
            issue_unit.io.dis_uops(w).lrs2_rtype := RT_X
            issue_unit.io.dis_uops(w).prs2_busy  := false.B
         }
      }
      when (issue_unit.io.dis_uops(w).lrs1_rtype === RT_FLT || issue_unit.io.dis_uops(w).lrs1_rtype === RT_FIX) {
         issue_unit.io.dis_uops(w).prs1_busy := true.B
      }
      when (issue_unit.io.dis_uops(w).lrs2_rtype === RT_FLT || issue_unit.io.dis_uops(w).lrs2_rtype === RT_FIX) {
         issue_unit.io.dis_uops(w).prs2_busy := true.B
      }
      when (issue_unit.io.dis_uops(w).lrs3_rtype === RT_FLT || issue_unit.io.dis_uops(w).lrs3_rtype === RT_FIX) {
         issue_unit.io.dis_uops(w).prs3_busy := true.B
      }

   }
   io.dis_readys := issue_unit.io.dis_readys

   //-------------------------------------------------------------
   // **** Issue Stage ****
   //-------------------------------------------------------------

   val ll_wb_block_issue = Wire(Bool())

   // Output (Issue)
   for (i <- 0 until issue_unit.issue_width)
   {
      iss_valids(i) := issue_unit.io.iss_valids(i)
      iss_uops(i) := issue_unit.io.iss_uops(i)

      issue_unit.io.fu_types(i) := ((exe_units(i).io.fu_types & ~Mux(ll_wb_block_issue, FU_VALU | FU_VFPU, 0.U))
         | Mux(io.memreq.ready, FU_MEM, 0.U)) | Mux(tosdq.io.count < 2.U, FU_V2I, 0.U)

      require (exe_units(i).uses_iss_unit)
   }

   // Wakeup
   for ((writeback, issue_wakeup) <- io.wakeups zip issue_unit.io.wakeup_pdsts)
   {
      issue_wakeup.valid := writeback.valid
      issue_wakeup.bits.pdst  := writeback.bits.uop.pdst
      issue_wakeup.bits.eidx  := writeback.bits.uop.eidx + writeback.bits.uop.rate // TODO_vec: This is a lot of adders
   }


   //-------------------------------------------------------------
   // **** Register Read Stage ****
   //-------------------------------------------------------------

   // Register Read <- Issue (rrd <- iss)
   vregister_read.io.rf_read_ports <> vregfile.io.read_ports

   vregister_read.io.iss_valids <> iss_valids
   vregister_read.io.iss_uops := iss_uops

   vregister_read.io.brinfo := io.brinfo
   vregister_read.io.kill := io.flush_pipeline

   // Buffer Read
   vscalaropbuffer.io.r_idx := iss_uops(0).vscopb_idx

   def fill_case(n: UInt, s: UInt): UInt = {
      MuxLookup(s, VEW_8, Array(
         VEW_8  -> fill_b(n),
         VEW_16 -> fill_h(n),
         VEW_32 -> fill_w(n),
         VEW_64 -> fill_d(n)))
   }

   //-------------------------------------------------------------
   // **** Execute Stage ****
   //-------------------------------------------------------------

   exe_units.map(_.io.brinfo := io.brinfo)
   exe_units.map(_.io.com_exception := io.flush_pipeline)

   for ((ex,w) <- exe_units.withFilter(_.uses_iss_unit).map(x=>x).zipWithIndex)
   {
      val exe_req = vregister_read.io.exe_reqs(w)
      ex.io.req <> exe_req

      val rs1_data = Mux(exe_req.bits.uop.lrs1_rtype === RT_FLT, fill_case(vscalaropbuffer.io.r_data(0), exe_req.bits.uop.rs1_vew),
                     Mux(exe_req.bits.uop.lrs1_rtype === RT_FIX, vscalaropbuffer.io.r_data(0),
                                                                 exe_req.bits.rs1_data))
      val rs2_data = Mux(exe_req.bits.uop.lrs2_rtype === RT_FLT, fill_case(vscalaropbuffer.io.r_data(1), exe_req.bits.uop.rs2_vew),
                     Mux(exe_req.bits.uop.lrs2_rtype === RT_FIX, vscalaropbuffer.io.r_data(1),
                                                                 exe_req.bits.rs2_data))
      val rs3_data = Mux(exe_req.bits.uop.lrs3_rtype === RT_FLT, fill_case(vscalaropbuffer.io.r_data(2), exe_req.bits.uop.rs3_vew),
                     Mux(exe_req.bits.uop.lrs3_rtype === RT_FIX, vscalaropbuffer.io.r_data(2),
                                                                 exe_req.bits.rs3_data))


      ex.io.req.bits.rs1_data := rs1_data
      ex.io.req.bits.rs2_data := rs2_data
      ex.io.req.bits.rs3_data := rs3_data

      require (!ex.isBypassable)
      require (w == 0)
      if (w == 0) {
         when (exe_req.bits.uop.is_store || exe_req.bits.uop.is_load) {
            ex.io.req.valid := false.B
         }


         val vew = Mux(exe_req.bits.uop.is_load,
            exe_req.bits.uop.rs2_vew,
            exe_req.bits.uop.rs3_vew)
         val eidx = exe_req.bits.uop.eidx
         val shiftn = Cat((eidx <<
            MuxLookup(vew, VEW_8, Array(
               VEW_8  -> 0.U,
               VEW_16 -> 1.U,
               VEW_32 -> 2.U,
               VEW_64 -> 3.U))) & "b1111".U, 0.U(width=3.W))

         tosdq.io.enq.valid     := exe_req.bits.uop.is_store
         tosdq.io.enq.bits.uop  := exe_req.bits.uop
         tosdq.io.enq.bits.data := rs3_data >> shiftn
         io.tosdq               <> tosdq.io.deq
         tosdq.io.deq.ready     := io.tosdq.ready
         io.tosdq.valid         := tosdq.io.deq.valid
         io.tosdq.bits.uop      := tosdq.io.deq.bits.uop
         io.tosdq.bits.data     := tosdq.io.deq.bits.data


         io.memreq.valid         := exe_req.bits.uop.uopc === uopVLDX || exe_req.bits.uop.uopc === uopVSTX
         io.memreq.bits.uop      := exe_req.bits.uop
         io.memreq.bits.rs1_data := rs1_data
         io.memreq.bits.rs2_data := (rs2_data >> shiftn) & MuxLookup(vew, VEW_8, Array(
            VEW_8  -> "hff".U,
            VEW_16 -> "hffff".U,
            VEW_32 -> "hffffffff".U,
            VEW_64 -> "hffffffffffffffff".U)) // This is bad
         io.memreq.bits.rs3_data := DontCare

         // assert (!(io.memreq.valid && !io.memreq.ready), "No backpressure. Redesign")
         // This technically doesn't follow the ready-valid interface
      }
   }
   require (exe_units.num_total_bypass_ports == 0)


   //-------------------------------------------------------------
   // **** Writeback Stage ****
   //-------------------------------------------------------------

   val ll_wb = Module(new BranchKillableQueue(new ExeUnitResp(128), entries = 8)) // TODO_Vec: Tune these
   ll_wb_block_issue := ll_wb.io.count >= 4.U
   ll_wb.io.enq      <> io.ll_wport
   ll_wb.io.brinfo   := io.brinfo
   ll_wb.io.flush    := io.flush_pipeline
   assert (ll_wb.io.enq.ready, "We do not support backpressure on this queue")
   when   (io.ll_wport.valid) { assert(io.ll_wport.bits.uop.ctrl.rf_wen && io.ll_wport.bits.uop.dst_rtype === RT_VEC) }


   val toint = Module(new BranchKillableQueue(new ExeUnitResp(xLen), entries = 4))
   io.toint <> toint.io.deq
   toint.io.brinfo := io.brinfo
   toint.io.flush  := io.flush_pipeline

   var w_cnt = 0 // TODO_Vec: check if this should be 1 or 0 for vec?
   var vec_eu_wb = false.B
   for (eu <- exe_units)
   {
      eu.io.debug_tsc_reg := io.debug_tsc_reg
      for (wbresp <- eu.io.resp)
      {
         val valid_write = wbresp.valid && wbresp.bits.uop.ctrl.rf_wen
         vregfile.io.write_ports(w_cnt).valid := valid_write


         vec_eu_wb = valid_write | vec_eu_wb
         vregfile.io.write_ports(w_cnt).bits.addr := CalcVecRegAddr(
            wbresp.bits.uop.rd_vew,
            wbresp.bits.uop.eidx,
            wbresp.bits.uop.pdst,
            numVecPhysRegs)
         vregfile.io.write_ports(w_cnt).bits.data := wbresp.bits.data
         vregfile.io.write_ports(w_cnt).bits.mask := wbresp.bits.mask
         vregfile.io.write_ports(w_cnt).bits.eidx := wbresp.bits.uop.eidx
         vregfile.io.write_ports(w_cnt).bits.rd_vew := wbresp.bits.uop.rd_vew
         wbresp.ready := vregfile.io.write_ports(w_cnt).ready

         toint.io.enq.valid := wbresp.valid && wbresp.bits.uop.uopc === uopVEXTRACT
         toint.io.enq.bits  := wbresp.bits
         assert (!(toint.io.enq.ready && !toint.io.enq.ready), "Can't backpressure here")

         assert (!(wbresp.valid &&
            !wbresp.bits.uop.ctrl.rf_wen &&
            wbresp.bits.uop.dst_rtype === RT_VEC),
            "[vecpipeline] An VEC writeback is being attempted with rf_wen disabled.")

         assert (!(wbresp.valid &&
            wbresp.bits.uop.ctrl.rf_wen &&
            wbresp.bits.uop.dst_rtype =/= RT_VEC),
            "[vecpipeline] A writeback is being attempted to the VEC RF with dst != VEC type.")

         w_cnt += 1
      }
   }
   assert(w_cnt == 1, "Only one normal write back supported here")
   ll_wb.io.deq.ready := false.B
   when (!vec_eu_wb) {
      vregfile.io.write_ports(0) <> WritePort(ll_wb.io.deq, log2Ceil(numVecRegFileRows), 128, true, numVecPhysRegs)
      ll_wb.io.deq.ready := true.B
   }
   require (w_cnt == vregfile.io.write_ports.length)

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Commit Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   w_cnt = 0
   var vec_eu_wakeup = false.B
   for (eu <- exe_units)
   {
      for (exe_resp <- eu.io.resp)
      {
         val wb_uop = exe_resp.bits.uop
         assert (!exe_resp.bits.writesToIRF, "Why would this write to IRF?")
         if (!exe_resp.bits.writesToIRF && !eu.has_ifpu) { // TODO_Vec: What is this for?
            val wport        = io.wakeups(w_cnt)
            val wakeup_valid = exe_resp.valid && wb_uop.dst_rtype === RT_VEC
            wport.valid     := wakeup_valid
            vec_eu_wakeup    = wakeup_valid | vec_eu_wakeup
            wport.bits      := exe_resp.bits

            w_cnt += 1

            assert(!(exe_resp.valid && wb_uop.is_store))
            assert(!(exe_resp.valid && wb_uop.is_load))
            assert(!(exe_resp.valid && wb_uop.is_amo))
         }
      }
   }
   when (!vec_eu_wakeup) {
      io.wakeups(0).valid := ll_wb.io.deq.valid && ll_wb.io.deq.ready
      io.wakeups(0).bits  := ll_wb.io.deq.bits
   }


   exe_units.map(_.io.fcsr_rm := io.fcsr_rm)

   //-------------------------------------------------------------
   // **** Flush Pipeline ****
   //-------------------------------------------------------------
   // flush on exceptions, miniexeptions, and after some special instructions

   for (w <- 0 until exe_units.length)
   {
      exe_units(w).io.req.bits.kill := io.flush_pipeline
   }

   val vec_string = exe_units.toString
   override def toString: String =
      vregfile.toString +
   "\n   Num Wakeup Ports      : " + num_wakeup_ports +
   "\n   Num Bypass Ports      : " + exe_units.num_total_bypass_ports + "\n"
}
