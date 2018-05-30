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
import boom.exu.FUConstants._
import boom.common._

class VecPipeline(implicit p: Parameters) extends BoomModule()(p) with tile.HasFPUParameters
{
  val vecIssueParams = issueParams.find(_.iqType == IQT_VEC.litValue).get
  val num_ll_ports = 1 // TODO_VEC: add ll wb ports
  val num_wakeup_ports = vecIssueParams.issueWidth + num_ll_ports
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
//     val fromint        = Flipped(Decoupled(new FuncUnitReq(fLen+1))) // from integer RF
//     val fromfp         = Flipped(Decoupled(new FuncUnitReq(fLen+1))) // from fp RF
//     val toint          = Decoupled(new ExeUnitResp(xLen))

     val wakeups        = Vec(num_wakeup_ports, Valid(new ExeUnitResp(128)))
     val wb_valids      = Input(Vec(num_wakeup_ports, Bool()))
     val vb_pdsts       = Input(Vec(num_wakeup_ports, UInt(width=vec_preg_sz.W)))

     val debug_tsc_reg  = Input(UInt(width=128.W))
  }

   //**********************************
   // TODO_VEC
   // construct all of the modules
   // TODO: Design and build VectorExecutionUnit
   // TODO: Design and build VectorIssueUnit
   // TODO: Design and build VectorRegisterFile
   // Initial Plan pSIMD vectors
   //   - Registers are fixed length, 128 bits
   //   - Registers are 64bit fp only
   //   - For now, leave i2v and f2v ports unconnected, no way to load vectors
   //   - Later - add memory execution unit
   //   - Later - add polymorphism, in decode microops should determine where operands come from

   val exe_units = new boom.exu.ExecutionUnits(vec = true)
   val issue_unit = Module(new IssueUnitCollasping(issueParams.find(_.iqType == IQT_VEC.litValue).get,
      num_wakeup_ports)) // TODO_VEC: Make this a VectorIssueUnit
   val vregfile = Module(new RegisterFileBehavorial(numVecPhysRegs,
      exe_units.withFilter(_.uses_iss_unit).map(e=>e.num_rf_read_ports).sum,
      exe_units.withFilter(_.uses_iss_unit).map(e=>e.num_rf_write_ports).sum
         + num_ll_ports, // TODO_VEC: Subtract write ports to IRF, FRF
      128,
      exe_units.bypassable_write_port_mask
   ))
   val vregister_read = Module(new RegisterRead(
      issue_unit.issue_width,
      exe_units.withFilter(_.uses_iss_unit).map(_.supportedFuncUnits),
      exe_units.withFilter(_.uses_iss_unit).map(_.num_rf_read_ports).sum,
      exe_units.withFilter(_.uses_iss_unit).map(_.num_rf_read_ports),
      exe_units.num_total_bypass_ports,
      128))

   require (exe_units.withFilter(_.uses_iss_unit).map(x=>x).length == issue_unit.issue_width)
   require (exe_units.map(_.num_rf_write_ports).sum + num_ll_ports == num_wakeup_ports)
   require (exe_units.withFilter(_.uses_iss_unit).map(e=>
      e.num_rf_write_ports).sum + num_ll_ports == num_wakeup_ports)

   // Todo_vec add checking for num write ports and number of functional units which use the issue unit

   val iss_valids = Wire(Vec(exe_units.withFilter(_.uses_iss_unit).map(x=>x).length, Bool()))
   val iss_uops   = Wire(Vec(exe_units.withFilter(_.uses_iss_unit).map(x=>x).length, new MicroOp()))

   issue_unit.io.tsc_reg := io.debug_tsc_reg
   issue_unit.io.brinfo := io.brinfo
   issue_unit.io.flush_pipeline := io.flush_pipeline

   require (exe_units.num_total_bypass_ports == 0)


   //-------------------------------------------------------------
   // **** Dispatch Stage ****
   //-------------------------------------------------------------

   // Input (Dispatch)
   for (w <- 0 until DISPATCH_WIDTH)
   {
      issue_unit.io.dis_valids(w) := io.dis_valids(w) && io.dis_uops(w).iqtype === issue_unit.iqType.U
      issue_unit.io.dis_uops(w) := io.dis_uops(w)
   }
   io.dis_readys := issue_unit.io.dis_readys

   //-------------------------------------------------------------
   // **** Issue Stage ****
   //-------------------------------------------------------------

   // Output (Issue)
   for (i <- 0 until issue_unit.issue_width)
   {
      iss_valids(i) := issue_unit.io.iss_valids(i)
      iss_uops(i) := issue_unit.io.iss_uops(i)

      var fu_types = exe_units(i).io.fu_types
      // TODO_VEC: Add special case for fdiv?
      issue_unit.io.fu_types(i) := fu_types

      require (exe_units(i).uses_iss_unit)
   }

   // Wakeup
   for ((writeback, issue_wakeup) <- io.wakeups zip issue_unit.io.wakeup_pdsts)
   {
      when (writeback.valid)
      {
         // printf("%d Vec wakeup writeback valid received\n", io.debug_tsc_reg)
      }
      issue_wakeup.valid := writeback.valid
      issue_wakeup.bits  := writeback.bits.uop.pdst
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

   //-------------------------------------------------------------
   // **** Execute Stage ****
   //-------------------------------------------------------------

   exe_units.map(_.io.brinfo := io.brinfo)
   exe_units.map(_.io.com_exception := io.flush_pipeline)

   for ((ex,w) <- exe_units.withFilter(_.uses_iss_unit).map(x=>x).zipWithIndex)
   {
      ex.io.req <> vregister_read.io.exe_reqs(w)
      require (!ex.isBypassable)
   }
   require (exe_units.num_total_bypass_ports == 0)


   //-------------------------------------------------------------
   // **** Writeback Stage ****
   //-------------------------------------------------------------

   //TODO_VEC: add ll_wports?
   //TODO_VEC: Add ARB for multiple ll_wb
   val ll_wbarb = Module(new Arbiter(new ExeUnitResp(128), 1))
   ll_wbarb.io.in(0) <> io.ll_wport
   //ll_wbarb.io.in(1).valid := false.B // TODO_vec: fix this

   vregfile.io.write_ports(0) <> WritePort(ll_wbarb.io.out, VPREG_SZ, 128)
   assert (ll_wbarb.io.in(0).ready)
   when (io.ll_wport.valid) { assert(io.ll_wport.bits.uop.ctrl.rf_wen && io.ll_wport.bits.uop.dst_rtype === RT_VEC) }
   

   var w_cnt = num_ll_ports // TODO_Vec: check if this should be 1 or 0 for vec?
   var toint_found = false
   for (eu <- exe_units)
   {
      eu.io.debug_tsc_reg := io.debug_tsc_reg
      for (wbresp <- eu.io.resp)
      {
         when (wbresp.valid)
         {
            // printf("%d Writeback received valid resp\n", io.debug_tsc_reg)
         }
         vregfile.io.write_ports(w_cnt).valid :=
         wbresp.valid &&
         wbresp.bits.uop.ctrl.rf_wen
         vregfile.io.write_ports(w_cnt).bits.addr := wbresp.bits.uop.pdst
         vregfile.io.write_ports(w_cnt).bits.data := wbresp.bits.data
         wbresp.ready := vregfile.io.write_ports(w_cnt).ready
      

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
   require (w_cnt == vregfile.io.write_ports.length)

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Commit Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // TODO_VEC: Add ll_wb

   io.wakeups(0).valid := ll_wbarb.io.out.valid
   io.wakeups(0).bits  := ll_wbarb.io.out.bits
   ll_wbarb.io.out.ready := true.B

   w_cnt = num_ll_ports
   for (eu <- exe_units)
   {
      for (exe_resp <- eu.io.resp)
      {
         val wb_uop = exe_resp.bits.uop
         when (wb_uop.valid)
         {
            // printf("%d wb uop is %d\n", io.debug_tsc_reg, wb_uop.uopc)
            // printf("%d exe_resp uop is %d %d\n", io.debug_tsc_reg, exe_resp.bits.uop.uopc, exe_resp.valid)
            // printf("%d %x %x\n", io.debug_tsc_reg, exe_resp.bits.writesToIRF.B, eu.has_ifpu.B)
         }
         if (!exe_resp.bits.writesToIRF && !eu.has_ifpu) {
            val wport = io.wakeups(w_cnt)
            wport.valid := exe_resp.valid && wb_uop.dst_rtype === RT_VEC
            // when (exe_resp.valid)
            // {
            //    printf("%d Commit wport exe_resp valid\n", io.debug_tsc_reg)
            // }
            wport.bits := exe_resp.bits

            w_cnt += 1

            assert(!(exe_resp.valid && wb_uop.is_store))
            assert(!(exe_resp.valid && wb_uop.is_load))
            assert(!(exe_resp.valid && wb_uop.is_amo))
         }
      }
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

