//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Floating Point Datapath Pipeline
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio

// The floating point issue window, regfile, and arithmetic units are all handled here.


package boom

import Chisel._
import config.Parameters


class FpPipeline(
)(implicit p: Parameters) extends BoomModule()(p)
{
   val fpIssueParams = issueParams.find(_.iqType == IQT_FP.litValue).get
   // TODO roll i2f into mem port.
   val num_ll_ports = 1 // hard-wired; used by mem port and i2f port.
//   val num_i2f_ports = 1 // hard-wired
//   val num_mem_ports = 1 // hard-wired
   val num_wakeup_ports = fpIssueParams.issueWidth + num_ll_ports
   val fp_preg_sz = log2Up(numFpPhysRegs)

   val io = new Bundle
   {
      val brinfo           = new BrResolutionInfo().asInput
      val flush_pipeline   = Bool(INPUT)
      val fcsr_rm          = UInt(INPUT, tile.FPConstants.RM_SZ)

      val dis_valids       = Vec(DISPATCH_WIDTH, Bool()).asInput
      val dis_uops         = Vec(DISPATCH_WIDTH, new MicroOp()).asInput
      val dis_readys       = Vec(DISPATCH_WIDTH, Bool()).asOutput

      // +1 for recoding.
      val ll_wport         = Valid(new ExeUnitResp(fLen+1)).flip
      val fromint          = Valid(new FuncUnitReq(fLen+1)).flip
      val tosdq            = Valid(new MicroOpWithData(fLen))
      val toint            = Decoupled(new ExeUnitResp(xLen))
      val wakeups          = Vec(num_wakeup_ports, Valid(new ExeUnitResp(fLen+1)))
      val wb_valids        = Vec(num_wakeup_ports, Bool()).asInput
      val wb_pdsts         = Vec(num_wakeup_ports, UInt(width=fp_preg_sz)).asInput

      //TODO -- hook up commit log stuff.
      val debug_tsc_reg    = UInt(INPUT, xLen)
   }

   val exe_units        = new boom.ExecutionUnits(fpu=true)
   val issue_unit       = Module(new IssueUnitCollasping(
                           issueParams.find(_.iqType == IQT_FP.litValue).get,
                           num_wakeup_ports))
   // Add a write-port for long latency operations (HACK XXX two - one for mem, the other for ifpu).
   val fregfile         = Module(new RegisterFile(numFpPhysRegs,
                                 exe_units.withFilter(_.uses_iss_unit).map(e => e.num_rf_read_ports).sum,
                                 // TODO get rid of -1, as that's a write-port going to IRF
                                 exe_units.withFilter(_.uses_iss_unit).map(e => e.num_rf_write_ports).sum - 1 +
                                    num_ll_ports, //num_mem_ports + num_i2f_ports,
                                 fLen+1,
                                 ENABLE_REGFILE_BYPASSING))// TODO disable for FP
   val fregister_read   = Module(new RegisterRead(
                                 issue_unit.issue_width,
                                 exe_units.withFilter(_.uses_iss_unit).map(_.supportedFuncUnits),
                                 exe_units.withFilter(_.uses_iss_unit).map(_.num_rf_read_ports).sum,
                                 exe_units.withFilter(_.uses_iss_unit).map(_.num_rf_read_ports),
                                 exe_units.num_total_bypass_ports,
                                 fLen+1))

   require (exe_units.withFilter(_.uses_iss_unit).map(x=>x).length == issue_unit.issue_width)

   // we're playing fast and loose on the number of wakeup and write ports.
   // TODO XXX and the -1 is for the F2I port; -1 for I2F port.
   println (exe_units.map(_.num_rf_write_ports).sum)
   require (exe_units.map(_.num_rf_write_ports).sum -1-1  + num_ll_ports == num_wakeup_ports)
   require (exe_units.withFilter(_.uses_iss_unit).map(e => e.num_rf_write_ports).sum -1 + num_ll_ports == num_wakeup_ports)

   override def toString: String =
      fregfile.toString +
      "\n   Num Wakeup Ports      : " + num_wakeup_ports +
      "\n   Num Bypass Ports      : " + exe_units.num_total_bypass_ports + "\n"


   //***********************************
   // Pipeline State Registers and Wires

   // Issue Stage/Register Read
   val iss_valids     = Wire(Vec(exe_units.withFilter(_.uses_iss_unit).map(x=>x).length, Bool()))
   val iss_uops       = Wire(Vec(exe_units.withFilter(_.uses_iss_unit).map(x=>x).length, new MicroOp()))

   require (exe_units.num_total_bypass_ports == 0)

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Dispatch Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // Input (Dispatch)
   for (w <- 0 until DISPATCH_WIDTH)
   {
      issue_unit.io.dis_valids(w) := io.dis_valids(w) && io.dis_uops(w).iqtype === UInt(issue_unit.iqType)
      issue_unit.io.dis_uops(w) := io.dis_uops(w)

      // Or... add STDataGen micro-op for FP stores.
      when (io.dis_uops(w).uopc === uopSTA && io.dis_uops(w).lrs2_rtype === RT_FLT) {
         issue_unit.io.dis_valids(w) := io.dis_valids(w)
         issue_unit.io.dis_uops(w).uopc := uopSTD
         issue_unit.io.dis_uops(w).fu_code := FUConstants.FU_FPU
         issue_unit.io.dis_uops(w).lrs1_rtype := RT_X
         issue_unit.io.dis_uops(w).prs1_busy := Bool(false)
      }
   }
   io.dis_readys := issue_unit.io.dis_readys

   issue_unit.io.tsc_reg := io.debug_tsc_reg
   issue_unit.io.brinfo := io.brinfo
   issue_unit.io.flush_pipeline := io.flush_pipeline

   // Output (Issue)
   for (i <- 0 until issue_unit.issue_width)
   {
      iss_valids(i) := issue_unit.io.iss_valids(i)
      iss_uops(i) := issue_unit.io.iss_uops(i)
      issue_unit.io.fu_types(i) := exe_units(i).io.fu_types

      require (exe_units(i).uses_iss_unit)
   }


   // Wakeup
   for ((writeback, issue_wakeup) <- io.wakeups zip issue_unit.io.wakeup_pdsts)
   {
      issue_wakeup.valid := writeback.valid
      issue_wakeup.bits  := writeback.bits.uop.pdst
   }

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Register Read Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // Register Read <- Issue (rrd <- iss)
   fregister_read.io.rf_read_ports <> fregfile.io.read_ports

   fregister_read.io.iss_valids <> iss_valids
   fregister_read.io.iss_uops := iss_uops

   fregister_read.io.brinfo := io.brinfo
   fregister_read.io.kill   := io.flush_pipeline


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Execute Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   exe_units.map(_.io.brinfo := io.brinfo)
   exe_units.map(_.io.com_exception := io.flush_pipeline)

   for ((ex,w) <- exe_units.withFilter(_.uses_iss_unit).map(x=>x).zipWithIndex)
   {
      ex.io.req <> fregister_read.io.exe_reqs(w)
      require (!ex.isBypassable)

      // TODO HACK only let one FPU issue port issue these.
      require (w == 0)
      when (fregister_read.io.exe_reqs(w).bits.uop.uopc === uopSTD) {
         ex.io.req.valid :=  Bool(false)
      }

      io.tosdq.valid    := fregister_read.io.exe_reqs(w).bits.uop.uopc === uopSTD
      io.tosdq.bits.uop := fregister_read.io.exe_reqs(w).bits.uop
      val sdata = fregister_read.io.exe_reqs(w).bits.rs2_data

      val unrec_s = hardfloat.fNFromRecFN(8, 24, sdata)
      val unrec_d = hardfloat.fNFromRecFN(11, 53, sdata)
      val unrec_out = Mux(io.tosdq.bits.uop.fp_single, Cat(Fill(32, unrec_s(31)), unrec_s), unrec_d)

      io.tosdq.bits.data := unrec_out
   }
   require (exe_units.num_total_bypass_ports == 0)

   exe_units.ifpu_unit.io.req <> io.fromint

   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Writeback Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // TODO add fdivsqrt to this port
   val ifpu_resp = exe_units.ifpu_unit.io.resp(0)
   val ll_wbarb = Module(new Arbiter(new ExeUnitResp(fLen+1), 2))
   ll_wbarb.io.in(0) <> io.ll_wport
   ll_wbarb.io.in(1) <> ifpu_resp

   fregfile.io.write_ports(0) <> WritePort(ll_wbarb.io.out, FPREG_SZ, fLen+1)

   assert (ll_wbarb.io.in(0).ready) // never backpressure the memory unit.
   when (ifpu_resp.valid) { assert (ifpu_resp.bits.uop.ctrl.rf_wen && ifpu_resp.bits.uop.dst_rtype === RT_FLT) }


   var w_cnt = 1
   for (i <- 0 until exe_units.length)
   {
      for (j <- 0 until exe_units(i).num_rf_write_ports)
      {
         val wbresp = exe_units(i).io.resp(j)
         println("ExeUnit(" + i + "), Resp Port: " + j + " writes FRF: " + !wbresp.bits.writesToIRF + ", fromInt:" +
            exe_units(i).has_ifpu)

         val toint = wbresp.bits.uop.dst_rtype === RT_FIX

         if (wbresp.bits.writesToIRF) {
            require(i==0 && j==1) // TODO super hacky... delete me once everything works
            assert(!(wbresp.valid && !toint))
            io.toint <> wbresp
         } else if (exe_units(i).has_ifpu) {
            // share with ll unit
         } else {
            assert (!(wbresp.valid && toint))
            fregfile.io.write_ports(w_cnt).valid :=
               wbresp.valid &&
               wbresp.bits.uop.ctrl.rf_wen
            fregfile.io.write_ports(w_cnt).bits.addr := wbresp.bits.uop.pdst
            fregfile.io.write_ports(w_cnt).bits.data := wbresp.bits.data
            wbresp.ready := fregfile.io.write_ports(w_cnt).ready
         }

         // need to make sure toint uops only ever come out of this wbresp port.
         // TODO add check on write port to check if it supports toint moves.
         require ((i==0 && (j==0 || j==1)) || !(exe_units(i).uses_iss_unit))

         assert (!(wbresp.valid &&
            !wbresp.bits.uop.ctrl.rf_wen &&
            wbresp.bits.uop.dst_rtype === RT_FLT),
            "[fppipeline] An FP writeback is being attempted with rf_wen disabled.")

         assert (!(wbresp.valid &&
            wbresp.bits.uop.ctrl.rf_wen &&
            wbresp.bits.uop.dst_rtype =/= RT_FLT &&
            !toint),
            "[fppipeline] A writeback is being attempted to the FP RF with dst != FP type.")

         if (!wbresp.bits.writesToIRF && !exe_units(i).has_ifpu) w_cnt += 1
      }
   }
   require (w_cnt == fregfile.io.write_ports.length)


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Commit Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   io.wakeups(0) <> ll_wbarb.io.out
   ll_wbarb.io.out.ready := Bool(true)

   w_cnt = 1
   for (eu <- exe_units)
   {
      for (exe_resp <- eu.io.resp)
      {
         println("FP exe-resp, w=" + w_cnt)
         val wb_uop = exe_resp.bits.uop

         if (!exe_resp.bits.writesToIRF && !eu.has_ifpu) {
            println("Hooking up to FRF, not a ll")
            val wport = io.wakeups(w_cnt)
            wport <> exe_resp
            wport.valid := exe_resp.valid && wb_uop.dst_rtype === RT_FLT

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

}
