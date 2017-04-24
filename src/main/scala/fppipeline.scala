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
   val num_wakeup_ports = issueWidths(2)+2
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
      val ll_wport         = new RegisterFileWritePortIO(fp_preg_sz, fLen+1)
      val ll_wport_uop     = (new MicroOp()).asInput // TODO consolidate this with ll_wport
      val fromint          = Valid(new FuncUnitReq(fLen+1)).flip
      val tosdq            = Valid(new MicroOpWithData(fLen))
      val toint            = new RegisterFileWritePortIO(log2Up(numIntPhysRegs), xLen).flip
      val toint_uop        = (new MicroOp()).asOutput

      val wakeups          = Vec(num_wakeup_ports, Valid(new ExeUnitResp(fLen+1)))
      val wb_valids        = Vec(num_wakeup_ports, Bool()).asInput
      val wb_pdsts         = Vec(num_wakeup_ports, UInt(width=fp_preg_sz)).asInput

      //TODO -- hook up commit log stuff.
      val debug_tsc_reg    = UInt(INPUT, xLen)

//      val events
   }

   val exe_units        = new boom.ExecutionUnits(fpu=true)
   val issue_unit       = Module(new IssueUnitCollasping(
                           numIssueSlotEntries(2), 
                           issueWidths(2), 
                           num_wakeup_ports, 
                           IQT_FP.litValue.intValue))
   // Add a write-port for long latency operations (HACK XXX two - one for mem, the other for ifpu).
   val fregfile         = Module(new RegisterFile(numFpPhysRegs,
                                 exe_units.withFilter(_.uses_iss_unit).map(e => e.num_rf_read_ports).sum,
                                 exe_units.withFilter(_.uses_iss_unit).map(e => e.num_rf_write_ports).sum + 2,
                                 fLen+1,
                                 ENABLE_REGFILE_BYPASSING))// TODO disable for FP
   println("fp: rrd")
   val fregister_read   = Module(new RegisterRead(
                                 issue_unit.issue_width,
                                 exe_units.withFilter(_.uses_iss_unit).map(_.supportedFuncUnits),
                                 exe_units.withFilter(_.uses_iss_unit).map(_.num_rf_read_ports).sum,
                                 exe_units.withFilter(_.uses_iss_unit).map(_.num_rf_read_ports),
                                 exe_units.num_total_bypass_ports,
                                 fLen+1))

   println("fp: done building")
   require (exe_units.withFilter(_.uses_iss_unit).map(x=>x).length == issue_unit.issue_width)

   // we're playing fast and loose on the number of wakeup and write ports.
   require (exe_units.map(_.num_rf_write_ports).sum+1 == num_wakeup_ports)
   require (exe_units.withFilter(_.uses_iss_unit).map(e => e.num_rf_write_ports).sum + 2 == num_wakeup_ports)

   override def toString: String =
      "\n   Floating Point Regfile: " +
      "\n   Num RF Read Ports     : " + exe_units.num_rf_read_ports +
      "\n   Num RF Write Ports    : " + exe_units.num_rf_write_ports + 
      "\n   RF Cost (R+W)*(R+2W)  : " + exe_units.rf_cost + 
      "\n   Num Slow Wakeup Ports : " + exe_units.num_slow_wakeup_ports +
      "\n   Num Fast Wakeup Ports : " + exe_units.num_fast_wakeup_ports +
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
//   for ((eu, w) <- 0 until exe_units.withFilter(_.uses_iss_unit).map(e => e).zipWithIndex)
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

   for ((ex,w) <- exe_units.withFilter(_.uses_iss_unit).map(x=>x).zipWithIndex)
   {
      ex.io.req <> fregister_read.io.exe_reqs(w)
      ex.io.brinfo := io.brinfo
      ex.io.com_exception := io.flush_pipeline
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
   // TODO XXX add ifpu to ll_wport, instead of giving all exe_units a dedicated port.
   io.ll_wport <> fregfile.io.write_ports(0)

   var w_cnt = 1
   for (i <- 0 until exe_units.length)
   {
      for (j <- 0 until exe_units(i).num_rf_write_ports)
      {
         val wbresp = exe_units(i).io.resp(j)

         val toint = wbresp.bits.uop.dst_rtype === RT_FIX

         fregfile.io.write_ports(w_cnt).wen :=
            wbresp.valid &&
            wbresp.bits.uop.ctrl.rf_wen &&
            !toint
         fregfile.io.write_ports(w_cnt).addr :=
            wbresp.bits.uop.pdst
         fregfile.io.write_ports(w_cnt).data :=
            wbresp.bits.data


//         io.toint.valid := wbresp.valid && toint
//         io.toint.bits  := wbresp.bits
         if (i==0 && j==0) {
            io.toint.wen  := wbresp.valid && toint
            io.toint.addr := wbresp.bits.uop.pdst
            io.toint.data := wbresp.bits.data
            io.toint_uop  := wbresp.bits.uop
         }
         // need to make sure toint uops only ever come out of this wbresp port.
         // TODO add check on write port to check if it supports toint moves.
         require ((i==0 && j==0) || !(exe_units(i).uses_iss_unit))

         assert (!(wbresp.valid &&
            !wbresp.bits.uop.ctrl.rf_wen &&
            wbresp.bits.uop.dst_rtype === RT_FLT),
            "[fppipeline] An FP writeback is being attempted with rf_wen disabled.")

         assert (!(wbresp.valid &&
            wbresp.bits.uop.ctrl.rf_wen &&
            wbresp.bits.uop.dst_rtype =/= RT_FLT &&
            !toint),
            "[fppipeline] A writeback is being attempted to the FP RF with dst != FP type.")
         
         w_cnt += 1
      }
   }
   require (w_cnt == fregfile.io.write_ports.length)


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Commit Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   // TODO the wakeup is being handled in core.scala for memory, but not other ll stuff.
   io.wakeups(0).valid := io.ll_wport.wen
   io.wakeups(0).bits.uop := io.ll_wport_uop
   io.wakeups(0).bits.data := io.ll_wport.data
   io.wakeups(0).bits.fflags.valid := Bool(false)

   w_cnt = 1
   for (eu <- exe_units)
   {
      // exe_units == 2
      //    exe_resp == 1
      // wakeups == 3 (2+mem)
      for (exe_resp <- eu.io.resp)
      {
         println("FP exe-resp, w=" + w_cnt)
         val wb_uop = exe_resp.bits.uop
         val wport = io.wakeups(w_cnt)
         wport <> exe_resp
         wport.valid := exe_resp.valid && wb_uop.dst_rtype === RT_FLT
         w_cnt += 1

         assert(!(exe_resp.valid && wb_uop.is_store))
         assert(!(exe_resp.valid && wb_uop.is_load))
         assert(!(exe_resp.valid && wb_uop.is_amo))
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
