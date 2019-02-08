//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Floating Point Datapath Pipeline
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// The floating point issue window, regfile, and arithmetic units are all handled here.

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket
import freechips.rocketchip.tile

import boom.exu.FUConstants._
import boom.common._

class FpPipeline(implicit p: Parameters) extends BoomModule()(p) with tile.HasFPUParameters
{
   val fpIssueParams = issueParams.find(_.iqType == IQT_FP.litValue).get
   val num_ll_ports = 1 // hard-wired; used by mem port and i2f port.
   val num_wakeup_ports = fpIssueParams.issueWidth + num_ll_ports
   val fp_preg_sz = log2Ceil(numFpPhysRegs)

   val io = IO(new Bundle
   {
      val brinfo           = Input(new BrResolutionInfo())
      val flush_pipeline   = Input(Bool())
      val fcsr_rm          = Input(UInt(width=freechips.rocketchip.tile.FPConstants.RM_SZ.W))

      val dis_valids       = Input(Vec(DISPATCH_WIDTH, Bool())) // REFACTOR into single Decoupled()
      val dis_uops         = Input(Vec(DISPATCH_WIDTH, new MicroOp()))
      val dis_readys       = Output(Vec(DISPATCH_WIDTH, Bool()))

      // +1 for recoding.
      val ll_wport         = Flipped(Decoupled(new ExeUnitResp(fLen+1)))// from memory unit
      val fromint          = Flipped(Decoupled(new ExeUnitResp(fLen+1)))// from integer RF
      val tosdq            = Valid(new MicroOpWithData(fLen))           // to Load/Store Unit
      val toint            = Decoupled(new ExeUnitResp(xLen))           // to integer RF

      val wakeups          = Vec(num_wakeup_ports, Valid(new ExeUnitResp(fLen+1)))
      val wb_valids        = Input(Vec(num_wakeup_ports, Bool()))
      val wb_pdsts         = Input(Vec(num_wakeup_ports, UInt(width=fp_preg_sz.W)))

      val debug_tsc_reg    = Input(UInt(width=xLen.W))
      val debug_wb_wdata   = Output(Vec(num_wakeup_ports, UInt((fLen+1).W)))
   })

   //**********************************
   // construct all of the modules

   val exe_units        = new boom.exu.ExecutionUnits(fpu=true)
   val issue_unit       = Module(new IssueUnitCollasping(
                           issueParams.find(_.iqType == IQT_FP.litValue).get,
                           num_wakeup_ports))
   val fregfile         = Module(new RegisterFileBehavorial(numFpPhysRegs,
                                 exe_units.num_frf_read_ports,
                                 exe_units.num_frf_write_ports + 1, // + 1 for ll writeback
                                 fLen+1,
                                 // No bypassing for any FP units, + 1 for ll_wb
                                 Seq.fill(exe_units.num_frf_write_ports + 1){ false }
                                 ))
   val fregister_read   = Module(new RegisterRead(
                           issue_unit.issue_width,
                           exe_units.withFilter(_.reads_frf).map(_.supportedFuncUnits),
                           exe_units.num_frf_read_ports,
                           exe_units.withFilter(_.reads_frf).map(x => 3),
                           0, // No bypass for FP
                           fLen+1))

   require (exe_units.count(_.reads_frf) == issue_unit.issue_width)
   require (exe_units.num_frf_write_ports + num_ll_ports == num_wakeup_ports)

   //*************************************************************
   // Issue window logic

   val iss_valids     = Wire(Vec(exe_units.num_frf_readers, Bool()))
   val iss_uops       = Wire(Vec(exe_units.num_frf_readers, new MicroOp()))

   issue_unit.io.tsc_reg := io.debug_tsc_reg
   issue_unit.io.brinfo := io.brinfo
   issue_unit.io.flush_pipeline := io.flush_pipeline
   // Don't support ld-hit speculation to FP window.
   issue_unit.io.mem_ldSpecWakeup.valid := false.B
   issue_unit.io.mem_ldSpecWakeup.bits := 0.U
   issue_unit.io.sxt_ldMiss := false.B

   require (exe_units.num_total_bypass_ports == 0)

   //-------------------------------------------------------------
   // **** Dispatch Stage ****
   //-------------------------------------------------------------

   // Input (Dispatch)
   for (w <- 0 until DISPATCH_WIDTH)
   {
      issue_unit.io.dis_valids(w) := io.dis_valids(w) && io.dis_uops(w).iqtype === issue_unit.iqType.U
      issue_unit.io.dis_uops(w) := io.dis_uops(w)

      // Or... add STDataGen micro-op for FP stores.
      when (io.dis_uops(w).uopc === uopSTA && io.dis_uops(w).lrs2_rtype === RT_FLT)
      {
         issue_unit.io.dis_valids(w) := io.dis_valids(w)
         issue_unit.io.dis_uops(w).uopc := uopSTD
         issue_unit.io.dis_uops(w).fu_code := FUConstants.FU_FPU
         issue_unit.io.dis_uops(w).lrs1_rtype := RT_X
         issue_unit.io.dis_uops(w).prs1_busy := false.B
      }
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
      if (exe_units(i).supportedFuncUnits.fdiv && regreadLatency > 0)
      {
         val fdiv_issued = iss_valids(i) && iss_uops(i).fu_code_is(FU_FDV)
         fu_types = fu_types & RegNext(~Mux(fdiv_issued, FU_FDV, 0.U))
      }
      issue_unit.io.fu_types(i) := fu_types

      require (exe_units(i).reads_frf)
   }

   // Wakeup
   for ((writeback, issue_wakeup) <- io.wakeups zip issue_unit.io.wakeup_pdsts)
   {
      issue_wakeup.valid := writeback.valid
      issue_wakeup.bits.pdst  := writeback.bits.uop.pdst
      issue_wakeup.bits.poisoned := false.B
   }

   //-------------------------------------------------------------
   // **** Register Read Stage ****
   //-------------------------------------------------------------

   // Register Read <- Issue (rrd <- iss)
   fregister_read.io.rf_read_ports <> fregfile.io.read_ports

   fregister_read.io.iss_valids <> iss_valids
   fregister_read.io.iss_uops := iss_uops

   fregister_read.io.brinfo := io.brinfo
   fregister_read.io.kill := io.flush_pipeline

   //-------------------------------------------------------------
   // **** Execute Stage ****
   //-------------------------------------------------------------

   exe_units.map(_.io.brinfo := io.brinfo)
   exe_units.map(_.io.com_exception := io.flush_pipeline)

   for ((ex,w) <- exe_units.withFilter(_.reads_frf).map(x=>x).zipWithIndex)
   {
      ex.io.req <> fregister_read.io.exe_reqs(w)
      require (!ex.bypassable)

      // TODO HACK only let one FPU issue port issue these.
      // Solution : Make STDataGen a functional unit time
      require (w == 0)
      when (fregister_read.io.exe_reqs(w).bits.uop.uopc === uopSTD)
      {
         ex.io.req.valid :=  false.B
      }

      io.tosdq.valid    := fregister_read.io.exe_reqs(w).bits.uop.uopc === uopSTD
      io.tosdq.bits.uop := fregister_read.io.exe_reqs(w).bits.uop
      val sdata = fregister_read.io.exe_reqs(w).bits.rs2_data

      io.tosdq.bits.data := ieee(sdata)
   }
   require (exe_units.num_total_bypass_ports == 0)

   //-------------------------------------------------------------
   // **** Writeback Stage ****
   //-------------------------------------------------------------

   val ll_wbarb = Module(new Arbiter(new ExeUnitResp(fLen+1), 2))
   val ifpu_resp = io.fromint

   // Hookup load writeback -- and recode FP values.
   ll_wbarb.io.in(0) <> io.ll_wport
   val typ = io.ll_wport.bits.uop.mem_typ
   val load_single = typ === rocket.MT_W || typ === rocket.MT_WU
   ll_wbarb.io.in(0).bits.data := recode(io.ll_wport.bits.data, !load_single)

   ll_wbarb.io.in(1) <> ifpu_resp

   if (regreadLatency > 0)
   {
      // Cut up critical path by delaying the write by a cycle.
      // Wakeup signal is sent on cycle S0, write is now delayed until end of S1,
      // but Issue happens on S1 and RegRead doesn't happen until S2 so we're safe.
      // (for regreadlatency >0).
      fregfile.io.write_ports(0) <> WritePort(RegNext(ll_wbarb.io.out), FPREG_SZ, fLen+1)
   }
   else
   {
      fregfile.io.write_ports(0) <> WritePort(ll_wbarb.io.out, FPREG_SZ, fLen+1)
   }

   assert (ll_wbarb.io.in(0).ready) // never backpressure the memory unit.
   when (ifpu_resp.valid) { assert (ifpu_resp.bits.uop.ctrl.rf_wen && ifpu_resp.bits.uop.dst_rtype === RT_FLT) }

   var w_cnt = 1
   var toint_found = false
   for (eu <- exe_units)
   {
      if (eu.writes_frf)
      {
         fregfile.io.write_ports(w_cnt).valid     := eu.io.fresp.valid && eu.io.fresp.bits.uop.ctrl.rf_wen
         fregfile.io.write_ports(w_cnt).bits.addr := eu.io.fresp.bits.uop.pdst
         fregfile.io.write_ports(w_cnt).bits.data := eu.io.fresp.bits.data
         eu.io.fresp.ready                        := fregfile.io.write_ports(w_cnt).ready
         when (eu.io.fresp.valid)
         {
            assert(eu.io.fresp.ready, "No backpressuring the FPU")
            assert(eu.io.fresp.bits.uop.ctrl.rf_wen, "rf_wen must be high here")
            assert(eu.io.fresp.bits.uop.dst_rtype === RT_FLT, "wb type must be FLT for fpu")
         }
         w_cnt += 1
      }
   }
   require(w_cnt == 2) // TODO: right now +1 for ll_wport, +1 for FPU
   require (w_cnt == fregfile.io.write_ports.length)

   val fpiu_unit = exe_units.fpiu_unit
   io.toint <> fpiu_unit.io.ll_iresp

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Commit Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   io.wakeups(0).valid := ll_wbarb.io.out.valid
   io.wakeups(0).bits := ll_wbarb.io.out.bits
   ll_wbarb.io.out.ready := true.B

   w_cnt = 1
   for (eu <- exe_units)
   {
      if (eu.writes_frf)
      {
         val exe_resp = eu.io.fresp
         val wb_uop = eu.io.fresp.bits.uop
         val wport = io.wakeups(w_cnt)
         wport.valid := exe_resp.valid && wb_uop.dst_rtype === RT_FLT
         wport.bits := exe_resp.bits

         w_cnt += 1

         assert(!(exe_resp.valid && wb_uop.is_store))
         assert(!(exe_resp.valid && wb_uop.is_load))
         assert(!(exe_resp.valid && wb_uop.is_amo))
      }
   }

   for ((wdata, wakeup) <- io.debug_wb_wdata zip io.wakeups)
   {
      wdata := ieee(wakeup.bits.data)
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

   val fp_string = exe_units.toString
   override def toString: String =
      fregfile.toString +
      "\n   Num Wakeup Ports      : " + num_wakeup_ports +
      "\n   Num Bypass Ports      : " + exe_units.num_total_bypass_ports + "\n"
}
