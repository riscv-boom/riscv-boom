//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Floating Point Datapath Pipeline
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.rocket
import freechips.rocketchip.tile

import boom.exu.FUConstants._
import boom.common._
import boom.util.{BoomCoreStringPrefix}

/**
 * Top level datapath that wraps the floating point issue window, regfile, and arithmetic units.
 */
class FpPipeline(implicit p: Parameters) extends BoomModule with tile.HasFPUParameters
{
  val fpIssueParams = issueParams.find(_.iqType == IQT_FP.litValue).get
  val dispatchWidth = fpIssueParams.dispatchWidth
  val numLlPorts = lsuWidth
  val numWakeupPorts = fpIssueParams.issueWidth + numLlPorts
  val fpPregSz = log2Ceil(numFpPhysRegs)

  val io = IO(new Bundle {
    val brupdate         = Input(new BrUpdateInfo())
    val flush_pipeline   = Input(Bool())
    val fcsr_rm          = Input(UInt(width=freechips.rocketchip.tile.FPConstants.RM_SZ.W))
    val status           = Input(new freechips.rocketchip.rocket.MStatus())

    val dis_uops         = Vec(dispatchWidth, Flipped(Decoupled(new MicroOp)))

    // +1 for recoding.
    val ll_wports        = Flipped(Vec(lsuWidth, Valid(new ExeUnitResp(fLen+1))))// from memory unit
    val from_int         = Flipped(Decoupled(new ExeUnitResp(fLen+1)))// from integer RF
    val dgen             = Valid(new ExeUnitResp(fLen))           // to Load/Store Unit
    val to_int           = Decoupled(new ExeUnitResp(xLen))           // to integer RF

    val fflags           = Vec(fpIssueParams.issueWidth, Valid(new FFlagsResp))

    val wakeups          = Vec(numWakeupPorts, Valid(new ExeUnitResp(fLen+1)))
    val wb_valids        = Input(Vec(numWakeupPorts, Bool()))
    val wb_pdsts         = Input(Vec(numWakeupPorts, UInt(width=fpPregSz.W)))

    val debug_tsc_reg    = Input(UInt(width=xLen.W))
  })

  //**********************************
  // construct all of the modules

  val exe_units      = (0 until fpWidth) map { w =>
    Module(new FPUExeUnit(hasFpu = true,
      hasFdiv = usingFDivSqrt && (w==0),
      hasFpiu = (w==0)))
  }
  val numFrfReadPorts = fpWidth * 3
  val numFrfWritePorts = fpWidth + lsuWidth

  val issue_unit     = Module(new IssueUnitCollapsing(
                         issueParams.find(_.iqType == IQT_FP.litValue).get,
                         numWakeupPorts))
  issue_unit.suggestName("fp_issue_unit")
  val fregfile       = Module(new RegisterFileSynthesizable(numFpPhysRegs,
                         numFrfReadPorts,
                         numFrfWritePorts,
                         fLen+1
                         ))
  val fregister_read = Module(new RegisterRead(
                         issue_unit.issueWidth,
                         numFrfReadPorts,
                         exe_units.map(x => 3),
                         0, // No bypass for FP
                         0,
                         fLen+1))

  //*************************************************************
  // Issue window logic

  val iss_uops   = issue_unit.io.iss_uops

  issue_unit.io.tsc_reg := io.debug_tsc_reg
  issue_unit.io.brupdate := io.brupdate
  issue_unit.io.flush_pipeline := io.flush_pipeline
  // Don't support ld-hit speculation to FP window.
  for (w <- 0 until lsuWidth) {
    issue_unit.io.spec_ld_wakeup(w).valid := false.B
    issue_unit.io.spec_ld_wakeup(w).bits := 0.U
  }
  issue_unit.io.ld_miss := false.B


  //-------------------------------------------------------------
  // **** Dispatch Stage ****
  //-------------------------------------------------------------

  // Input (Dispatch)
  for (w <- 0 until dispatchWidth) {
    issue_unit.io.dis_uops(w) <> io.dis_uops(w)
  }

  //-------------------------------------------------------------
  // **** Issue Stage ****
  //-------------------------------------------------------------

  // Output (Issue)
  for (i <- 0 until issue_unit.issueWidth) {

    var fu_types = exe_units(i).io.fu_types
    if (exe_units(i).hasFdiv) {
      val fdiv_issued = iss_uops(i).valid && iss_uops(i).bits.fu_code_is(FU_FDV)
      fu_types = fu_types & RegNext(~Mux(fdiv_issued, FU_FDV, 0.U))
    }
    issue_unit.io.fu_types(i) := fu_types

  }

  // Wakeup
  for ((writeback, issue_wakeup) <- io.wakeups zip issue_unit.io.wakeup_ports) {
    issue_wakeup.valid := writeback.valid
    issue_wakeup.bits  := writeback.bits.uop.pdst
  }
  issue_unit.io.pred_wakeup_port.valid := false.B
  issue_unit.io.pred_wakeup_port.bits := DontCare

  //-------------------------------------------------------------
  // **** Register Read Stage ****
  //-------------------------------------------------------------

  // Register Read <- Issue (rrd <- iss)
  fregister_read.io.rf_read_ports <> fregfile.io.read_ports
  fregister_read.io.prf_read_ports map { port => port.data := false.B }

  fregister_read.io.iss_uops := iss_uops

  fregister_read.io.brupdate := io.brupdate
  fregister_read.io.kill := io.flush_pipeline

  //-------------------------------------------------------------
  // **** Execute Stage ****
  //-------------------------------------------------------------

  exe_units.map(_.io.brupdate := io.brupdate)

  for ((ex,w) <- exe_units.zipWithIndex) {
    ex.io.req <> fregister_read.io.exe_reqs(w)
  }

  //-------------------------------------------------------------
  // **** Writeback Stage ****
  //-------------------------------------------------------------

  val ll_wbarb = Module(new Arbiter(new ExeUnitResp(fLen+1), 2))


  // Hookup load writeback -- and recode FP values.
  ll_wbarb.io.in(0).valid := RegNext(io.ll_wports(0).valid)
  ll_wbarb.io.in(0).bits  := RegNext(io.ll_wports(0).bits)
  ll_wbarb.io.in(0).bits.data := recode(RegNext(io.ll_wports(0).bits.data),
                                        RegNext(io.ll_wports(0).bits.uop.mem_size =/= 2.U))

  val ifpu_resp = io.from_int
  ll_wbarb.io.in(1) <> ifpu_resp


  // Cut up critical path by delaying the write by a cycle.
  // Wakeup signal is sent on cycle S0, write is now delayed until end of S1,
  // but Issue happens on S1 and RegRead doesn't happen until S2 so we're safe.
  fregfile.io.write_ports(0) := WritePort(ll_wbarb.io.out, fpregSz, fLen+1, RT_FLT)

  assert (ll_wbarb.io.in(0).ready) // never backpressure the memory unit.
  when (ifpu_resp.valid) { assert (ifpu_resp.bits.uop.rf_wen && ifpu_resp.bits.uop.dst_rtype === RT_FLT) }

  var w_cnt = 1
  for (i <- 1 until lsuWidth) {
    fregfile.io.write_ports(w_cnt) := RegNext(WritePort(io.ll_wports(i), fpregSz, fLen+1, RT_FLT))
    fregfile.io.write_ports(w_cnt).bits.data := recode(RegNext(io.ll_wports(i).bits.data),
                                                       RegNext(io.ll_wports(i).bits.uop.mem_size =/= 2.U))
    w_cnt += 1
  }
  for (eu <- exe_units) {
    fregfile.io.write_ports(w_cnt).valid     := eu.io.resp.valid && eu.io.resp.bits.uop.rf_wen
    fregfile.io.write_ports(w_cnt).bits.addr := eu.io.resp.bits.uop.pdst
    fregfile.io.write_ports(w_cnt).bits.data := eu.io.resp.bits.data
    w_cnt += 1
  }
  require (w_cnt == fregfile.io.write_ports.length)

  val fpiu_unit = exe_units.find(_.hasFpiu).get
  io.to_int <> fpiu_unit.io.ll_iresp
  io.dgen   := fpiu_unit.io.dgen


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Commit Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  io.wakeups(0).valid := ll_wbarb.io.out.valid
  io.wakeups(0).bits := ll_wbarb.io.out.bits
  ll_wbarb.io.out.ready := true.B

  w_cnt = 1
  for (i <- 1 until lsuWidth) {
    io.wakeups(w_cnt) := io.ll_wports(i)
    io.wakeups(w_cnt).bits.data := recode(io.ll_wports(i).bits.data,
      io.ll_wports(i).bits.uop.mem_size =/= 2.U)
    w_cnt += 1
  }
  for (eu <- exe_units) {
    val exe_resp = eu.io.resp
    val wb_uop = eu.io.resp.bits.uop
    val wport = io.wakeups(w_cnt)
    wport.valid := exe_resp.valid && wb_uop.dst_rtype === RT_FLT
    wport.bits := exe_resp.bits

    w_cnt += 1
  }

  var f_cnt = 0
  for (eu <- exe_units) {
    io.fflags(f_cnt) := eu.io.fflags
    f_cnt += 1
  }

  exe_units.map(_.io.fcsr_rm := io.fcsr_rm)
  exe_units.map(_.io.status := io.status)

  //-------------------------------------------------------------
  // **** Flush Pipeline ****
  //-------------------------------------------------------------
  // flush on exceptions, miniexeptions, and after some special instructions

  for (w <- 0 until exe_units.length) {
    exe_units(w).io.req.bits.kill := io.flush_pipeline
  }

  override def toString: String =
    (BoomCoreStringPrefix("===FP Pipeline===") + "\n"
    + exe_units.map(_.toString).mkString("\n") + "\n"
    + fregfile.toString
    + BoomCoreStringPrefix(
      "Num Wakeup Ports      : " + numWakeupPorts))
}
