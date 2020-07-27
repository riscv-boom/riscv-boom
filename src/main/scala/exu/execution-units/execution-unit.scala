//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Execution Units
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// The issue window schedules micro-ops onto a specific execution pipeline
// A given execution pipeline may contain multiple functional units; one or more
// read ports, and one or more writeports.

package boom.exu

import scala.collection.mutable.{ArrayBuffer}

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.rocket.{BP, SFenceReq, CSR}
import freechips.rocketchip.tile.{XLen, RoCCCoreIO}
import freechips.rocketchip.tile
import freechips.rocketchip.util._

import FUConstants._
import boom.common._
import boom.ifu.{GetPCFromFtq}
import boom.util._

/**
 * Response from Execution Unit. Bundles a MicroOp with data
 *
 * @param dataWidth width of the data coming from the execution unit
 */
class ExeUnitResp(val dataWidth: Int)(implicit p: Parameters) extends BoomBundle
  with HasBoomUOP
{
  val data = Bits(dataWidth.W)
  val predicated = Bool() // Was this predicated off?
  val fflags = Valid(UInt(tile.FPConstants.FLAGS_SZ.W))
}

class MemGen(implicit p: Parameters) extends BoomBundle
  with HasBoomUOP
{
  val data = UInt(xLen.W)
}

class CSRResp(implicit p: Parameters) extends BoomBundle
  with HasBoomUOP
{
  val data = UInt(xLen.W)
  val addr = UInt(CSR.ADDRSZ.W)
}

abstract class ExecutionUnit(name: String)(implicit p: Parameters) extends BoomMultiIOModule
{
  val fu_types = ArrayBuffer[(UInt, Bool, String)]()
  def get_all_fu_types: UInt = fu_types.map(_._1).reduce(_|_)

  val io_kill = IO(Input(Bool()))
  val io_brupdate = IO(Input(new BrUpdateInfo))
  val io_status = IO(Input(new freechips.rocketchip.rocket.MStatus))
  val io_ready_fu_types = IO(Output(UInt(FUC_SZ.W)))

  val io_fcsr_rm = IO(Input(UInt(tile.FPConstants.RM_SZ.W)))
  override def toString = {
    BoomCoreStringPrefix(s"===${name}ExeUnit") +
    fu_types.map { case (_, _, s) => BoomCoreStringPrefix(s" - ${s}") }.reduce(_+_)
  }

  val io_iss_uop = IO(Input(Valid(new MicroOp)))

  val arb_uop = Reg(Valid(new MicroOp))
  arb_uop.valid := io_iss_uop.valid && !io_kill && !IsKilledByBranch(io_brupdate, io_iss_uop.bits)
  arb_uop.bits  := UpdateBrMask(io_brupdate, io_iss_uop.bits)
  val rrd_uop = Reg(Valid(new MicroOp))
  rrd_uop.valid := arb_uop.valid && !io_kill && !IsKilledByBranch(io_brupdate, arb_uop.bits)
  rrd_uop.bits  := UpdateBrMask(io_brupdate, arb_uop.bits)
  val exe_uop = Reg(Valid(new MicroOp))
  exe_uop.valid := rrd_uop.valid && !io_kill && !IsKilledByBranch(io_brupdate, rrd_uop.bits)
  exe_uop.bits  := UpdateBrMask(io_brupdate, rrd_uop.bits)
}

trait HasIrfReadPorts { this: ExecutionUnit =>
  def nReaders: Int

  val io_arb_irf_reqs = IO(Vec(nReaders, Decoupled(UInt(maxPregSz.W))))
  val io_rrd_irf_resps    = IO(Input (Vec(nReaders , UInt(xLen.W))))
  val io_rrd_irf_bypasses = IO(Input (Vec(coreWidth, Valid(new ExeUnitResp(xLen)))))

  def rrd_bypass_hit(prs: UInt, rdata: UInt): (Bool, UInt) = {
    val hits = io_rrd_irf_bypasses map { b => b.valid && prs === b.bits.uop.pdst }
    (hits.reduce(_||_), Mux(hits.reduce(_||_), Mux1H(hits, io_rrd_irf_bypasses.map(_.bits.data)), rdata))
  }
}

trait HasImmrfReadPort { this: ExecutionUnit =>
  val io_arb_immrf_req    = IO(Decoupled(UInt(immPregSz.W)))
  assert(io_arb_immrf_req.ready)
  val io_rrd_immrf_resp   = IO(Input(UInt(xLen.W)))
  val io_rrd_immrf_wakeup = IO(Output(Valid(new ExeUnitResp(1))))
}

trait HasPrfReadPort { this: ExecutionUnit =>
  val io_arb_prf_req    = IO(Decoupled(UInt(log2Ceil(ftqSz).W)))
  assert(io_arb_prf_req.ready)
  val io_rrd_prf_resp   = IO(Input(Bool()))
  val io_rrd_prf_bypass = IO(Input(Valid(new ExeUnitResp(1))))
}

trait HasFrfReadPorts { this: ExecutionUnit =>
  val io_arb_frf_reqs  = IO(Vec(3, Decoupled(UInt(maxPregSz.W))))
  io_arb_frf_reqs.map { r => assert(r.ready) }
  val io_rrd_frf_resps = IO(Input (Vec(3, UInt((xLen+1).W))))
}

class MemExeUnit(
  val hasAGen          : Boolean       = false,
  val hasDGen          : Boolean       = false
)(implicit p: Parameters) extends ExecutionUnit("Mem")
  with HasIrfReadPorts
  with HasImmrfReadPort
{
  def nReaders = 1
  // Mem pipe is iss->rrd->exu
  io_arb_irf_reqs(0).valid := arb_uop.valid && arb_uop.bits.lrs1_rtype === RT_FIX && !arb_uop.bits.iw_p1_bypass_hint
  io_arb_irf_reqs(0).bits  := arb_uop.bits.prs1
  assert(!(io_arb_irf_reqs(0).valid && !io_arb_irf_reqs(0).ready))

  io_arb_immrf_req.valid := (arb_uop.valid &&
    !arb_uop.bits.fu_code_is(FU_DGEN) &&
    !arb_uop.bits.imm_sel.isOneOf(IS_N, IS_SH)
  )
  io_arb_immrf_req.bits      := arb_uop.bits.pimm

  io_rrd_immrf_wakeup.valid := (rrd_uop.valid &&
    !rrd_uop.bits.fu_code_is(FU_DGEN) &&
    !rrd_uop.bits.imm_sel.isOneOf(IS_N, IS_SH)
  )
  io_rrd_immrf_wakeup.bits := DontCare
  io_rrd_immrf_wakeup.bits.uop := rrd_uop.bits


  val exe_rs1_data = Reg(UInt(xLen.W))
  val (rs1_hit, rs1_data) = rrd_bypass_hit(rrd_uop.bits.prs1, io_rrd_irf_resps(0))
  assert(!(rrd_uop.valid && rrd_uop.bits.lrs1_rtype === RT_FIX && rrd_uop.bits.iw_p1_bypass_hint && !rs1_hit))
  exe_rs1_data := Mux(rrd_uop.bits.lrs1_rtype === RT_ZERO, 0.U, rs1_data)

  val exe_imm_data = RegNext(Mux(rrd_uop.bits.imm_sel === IS_SH,
    Sext(rrd_uop.bits.pimm, xLen),
    Sext(ImmGen(io_rrd_immrf_resp, rrd_uop.bits.imm_sel), xLen)
  ))

  val io_agen = if (hasAGen) {
    val loads_saturating = exe_uop.valid && exe_uop.bits.uses_ldq && exe_uop.bits.fu_code_is(FU_AGEN)
    val saturating_loads_counter = RegInit(0.U(5.W))
    when (loads_saturating) { saturating_loads_counter := saturating_loads_counter + 1.U }
      .otherwise { saturating_loads_counter := 0.U }
    val pause_mem = RegNext(loads_saturating) && saturating_loads_counter === ~(0.U(5.W))
    val load_ready = !pause_mem
    fu_types += ((FU_AGEN, load_ready, "AGen"))

    val sum = (exe_rs1_data.asSInt + exe_imm_data.asSInt).asUInt
    val ea_sign = Mux(sum(vaddrBits-1), ~sum(63,vaddrBits) === 0.U,
                                         sum(63,vaddrBits) =/= 0.U)
    val effective_address = Cat(ea_sign, sum(vaddrBits-1,0)).asUInt

    val agen = IO(Output(Valid(new MemGen)))
    agen.valid     := exe_uop.valid && exe_uop.bits.fu_code_is(FU_AGEN)
    agen.bits.uop  := exe_uop.bits
    agen.bits.data := Sext(effective_address, xLen)
    Some(agen)
  } else {
    assert(!(exe_uop.valid && exe_uop.bits.fu_code_is(FU_AGEN)))
    None
  }

  val io_dgen = if (hasDGen) {
    fu_types += ((FU_DGEN, true.B, "DGen"))
    val dgen = IO(Output(Valid(new MemGen)))
    dgen.valid     := exe_uop.valid && exe_uop.bits.fu_code_is(FU_DGEN)
    dgen.bits.data := exe_rs1_data
    dgen.bits.uop  := exe_uop.bits
    Some(dgen)
  } else {
    assert(!(exe_uop.valid && exe_uop.bits.fu_code_is(FU_DGEN)))
    None
  }

  io_ready_fu_types := fu_types.map { case (code, ready, _) => Mux(ready, code, 0.U(FUC_SZ.W)) }.reduce(_|_)


}
class IntExeUnit(
  val hasCSR           : Boolean       = false,
  val hasJmp           : Boolean       = false,
  val hasMul           : Boolean       = false,
  val hasDiv           : Boolean       = false,
  val hasIfpu          : Boolean       = false,
  val hasRocc          : Boolean       = false
)(implicit p: Parameters) extends ExecutionUnit("Int")
  with HasIrfReadPorts
  with HasPrfReadPort
  with HasImmrfReadPort
{
  io_arb_irf_reqs(0).valid := arb_uop.valid && arb_uop.bits.lrs1_rtype === RT_FIX && !arb_uop.bits.iw_p1_bypass_hint
  io_arb_irf_reqs(0).bits  := arb_uop.bits.prs1
  io_arb_irf_reqs(1).valid := arb_uop.valid && arb_uop.bits.lrs2_rtype === RT_FIX && !arb_uop.bits.iw_p2_bypass_hint
  io_arb_irf_reqs(1).bits  := arb_uop.bits.prs2

  val io_squash_iss = IO(Output(Bool()))
  io_squash_iss := ((io_arb_irf_reqs(0).valid && !io_arb_irf_reqs(0).ready) ||
                    (io_arb_irf_reqs(1).valid && !io_arb_irf_reqs(1).ready))

  // The arbiter didn't grant us a slot. Thus, we should replay the instruction in this slot,
  // But next time we read, it reads from the regfile, not the bypass paths, so disable the bypass hints
  when (io_squash_iss) {
    arb_uop.valid := arb_uop.valid && !io_kill && !IsKilledByBranch(io_brupdate, arb_uop.bits)
    arb_uop.bits  := UpdateBrMask(io_brupdate, arb_uop.bits)
    arb_uop.bits.iw_p1_bypass_hint := false.B
    arb_uop.bits.iw_p2_bypass_hint := false.B
    rrd_uop.valid := false.B
  }

  def nReaders = 2
  io_arb_immrf_req.valid     := (arb_uop.valid && !arb_uop.bits.imm_sel.isOneOf(IS_N, IS_SH))
  io_arb_immrf_req.bits      := arb_uop.bits.pimm

  io_rrd_immrf_wakeup.valid    := (rrd_uop.valid && !rrd_uop.bits.imm_sel.isOneOf(IS_N, IS_SH))
  io_rrd_immrf_wakeup.bits     := DontCare
  io_rrd_immrf_wakeup.bits.uop := rrd_uop.bits



  io_arb_prf_req.valid := arb_uop.valid
  io_arb_prf_req.bits  := arb_uop.bits.ppred

  val exe_rs1_data = Reg(UInt(xLen.W))
  val exe_rs2_data = Reg(UInt(xLen.W))
  val (rs1_hit, rs1_data) = rrd_bypass_hit(rrd_uop.bits.prs1, io_rrd_irf_resps(0))
  val (rs2_hit, rs2_data) = rrd_bypass_hit(rrd_uop.bits.prs2, io_rrd_irf_resps(1))
  assert(!(rrd_uop.valid && rrd_uop.bits.lrs1_rtype === RT_FIX && rrd_uop.bits.iw_p1_bypass_hint && !rs1_hit))
  assert(!(rrd_uop.valid && rrd_uop.bits.lrs2_rtype === RT_FIX && rrd_uop.bits.iw_p2_bypass_hint && !rs2_hit))
  exe_rs1_data := Mux(rrd_uop.bits.lrs1_rtype === RT_ZERO, 0.U, rs1_data)
  exe_rs2_data := Mux(rrd_uop.bits.lrs2_rtype === RT_ZERO, 0.U, rs2_data)

  val exe_imm_data = RegNext(Mux(rrd_uop.bits.imm_sel === IS_SH,
    Sext(rrd_uop.bits.pimm, xLen),
    Sext(ImmGen(io_rrd_immrf_resp, rrd_uop.bits.imm_sel), xLen)
  ))

  val exe_pred_data = Reg(Bool())
  exe_pred_data := Mux(io_rrd_prf_bypass.valid && io_rrd_prf_bypass.bits.uop.pdst === rrd_uop.bits.ppred,
    io_rrd_prf_bypass.bits.data, io_rrd_prf_resp)

  val exe_int_req = Wire(new FuncUnitReq(xLen))
  exe_int_req.uop := exe_uop.bits
  exe_int_req.rs1_data := exe_rs1_data
  exe_int_req.rs2_data := exe_rs2_data
  exe_int_req.rs3_data := DontCare
  exe_int_req.imm_data := exe_imm_data
  exe_int_req.pred_data := exe_pred_data

  val alu_ready = WireInit(true.B)
  fu_types += ((FU_ALU, alu_ready, "ALU"))

  val alu = Module(new ALUUnit(isJmpUnit = hasJmp,
                                 dataWidth = xLen))
  val req_valid = (exe_uop.bits.fu_code_is(FU_ALU) ||
    (if (hasJmp) exe_uop.bits.fu_code_is(FU_JMP) else false.B) ||
    (if (hasCSR) exe_uop.bits.fu_code_is(FU_CSR) else false.B)
  )
  alu.io.req.valid  := exe_uop.valid && req_valid && !exe_uop.bits.is_rocc
  alu.io.req.bits   := exe_int_req
  alu.io.resp.ready := true.B
  alu.io.brupdate   := io_brupdate
  alu.io.kill       := io_kill

  val io_alu_resp = IO(Output(Valid(new ExeUnitResp(xLen))))
  io_alu_resp.valid := alu.io.resp.valid && !alu.io.resp.bits.uop.fu_code_is(FU_CSR)
  io_alu_resp.bits  := alu.io.resp.bits

  val io_brinfo = IO(Output(Valid(new BrResolutionInfo)))
  io_brinfo := alu.io.brinfo

  val (io_get_ftq_req, io_get_ftq_resp, io_pred_bypass) = if (hasJmp) {
    fu_types += ((FU_JMP, alu_ready, "Jmp"))

    val req = IO(Output(Valid(UInt(log2Ceil(ftqSz).W))))
    req.valid := rrd_uop.valid && rrd_uop.bits.fu_code === FU_JMP
    req.bits  := rrd_uop.bits.ftq_idx

    val resp = IO(Input(new GetPCFromFtq))
    alu.io.get_ftq_resp := resp

    val pred = IO(Output(Valid(new ExeUnitResp(1))))
    pred.valid := alu.io.resp.valid && alu.io.resp.bits.uop.is_sfb_br
    pred.bits  := alu.io.resp.bits
    (Some(req), Some(resp), Some(pred))
  } else {
    (None, None, None)
  }

  if (hasMul) {
    fu_types += ((FU_MUL, true.B, "IMul"))

    val imul = Module(new PipelinedMulUnit(imulLatency, xLen))
    require(imulLatency > 2)
    imul.io.req.valid  := exe_uop.valid && exe_uop.bits.fu_code_is(FU_MUL)
    imul.io.req.bits   := exe_int_req
    imul.io.brupdate   := io_brupdate
    imul.io.kill       := io_kill
    imul.io.resp.ready := true.B

    // If the mul unit is going to write, block issue of a ALU op so the mul can take the write por0

    val imul_block_alu = ShiftRegister(arb_uop.valid && arb_uop.bits.fu_code_is(FU_MUL), imulLatency-1)
    when (imul_block_alu) {
      alu_ready := false.B
    }

    when (imul.io.resp.valid) {
      io_alu_resp := imul.io.resp
      assert(!(alu.io.resp.valid && !alu.io.resp.bits.uop.fu_code_is(FU_CSR)))
    }
  } else {
    assert(!(exe_uop.valid && exe_uop.bits.fu_code_is(FU_MUL)))
  }

  val (io_csr, io_sfence) = if (hasCSR) {
    fu_types += ((FU_CSR, true.B, "CSR"))
    val c = IO(Output(Valid(new CSRResp)))
    c.valid     := RegNext(alu.io.resp.valid && exe_uop.bits.csr_cmd =/= CSR.N)
    c.bits.uop  := RegNext(alu.io.resp.bits.uop)
    c.bits.data := RegNext(alu.io.resp.bits.data)
    c.bits.addr := RegNext(exe_imm_data)

    val s = IO(Valid(new SFenceReq))
    s.valid    := RegNext(exe_uop.valid && exe_uop.bits.is_sfence)
    s.bits.rs1 := RegNext(exe_uop.bits.mem_size(0))
    s.bits.rs2 := RegNext(exe_uop.bits.mem_size(1))
    s.bits.addr := RegNext(exe_rs1_data)
    s.bits.asid := RegNext(exe_rs2_data)
    (Some(c), Some(s))
  } else {
    assert(!(exe_uop.valid && exe_uop.bits.fu_code_is(FU_CSR)))
    assert(!(exe_uop.valid && exe_uop.bits.is_sfence))
    (None, None)
  }

  val io_bypass = IO(Valid(new ExeUnitResp(xLen)))
  io_bypass.valid := (io_alu_resp.valid &&
    io_alu_resp.bits.uop.bypassable &&
    io_alu_resp.bits.uop.rf_wen &&
    io_alu_resp.bits.uop.dst_rtype === RT_FIX)
  io_bypass.bits := io_alu_resp.bits

  val (io_rocc_resp, io_rocc_core) = if (hasRocc) {
    val rocc_core = IO(new RoCCShimCoreIO)
    val rocc_resp = IO(Decoupled(new ExeUnitResp(xLen)))
    val rocc = Module(new RoCCShim)
    rocc.io.req.valid         := exe_uop.valid && exe_uop.bits.is_rocc
    rocc.io.req.bits          := exe_int_req
    rocc.io.brupdate          := io_brupdate // We should assert on this somewhere
    rocc.io.status            := io_status
    rocc.io.exception         := io_kill
    rocc_core                 <> rocc.io.core

    rocc_resp <> rocc.io.resp
    (Some(rocc_resp), Some(rocc_core))
  } else {
    assert(!(exe_uop.valid && exe_uop.bits.is_rocc))
    (None, None)
  }

  val (io_ifpu_resp) = if (hasIfpu) {
    val ifpu_ready = Wire(Bool())
    fu_types += ((FU_I2F, ifpu_ready, "IFPU"))

    val ifpu = Module(new IntToFPUnit(latency=intToFpLatency))
    ifpu.io.req.valid  := exe_uop.valid && exe_uop.bits.fu_code_is(FU_I2F)
    ifpu.io.req.bits   := exe_int_req
    ifpu.io.fcsr_rm    := io_fcsr_rm
    ifpu.io.brupdate   := io_brupdate
    ifpu.io.kill       := io_kill

    // buffer up results since we share write-port on integer regfile.
    val queue = Module(new BranchKillableQueue(new ExeUnitResp(xLen+1),
      entries = intToFpLatency + 6)) // TODO being overly conservative
    queue.io.enq <> ifpu.io.resp
    queue.io.brupdate := io_brupdate
    queue.io.flush    := io_kill
    assert (!(queue.io.enq.valid && !queue.io.enq.ready))
    ifpu_ready := RegNext(queue.io.count < 2.U)

    val ifpu_resp = IO(Decoupled(new ExeUnitResp(xLen+1)))
    ifpu_resp <> queue.io.deq
    (Some(ifpu_resp))
  } else {
    assert(!(exe_uop.valid && exe_uop.bits.fu_code_is(FU_I2F)))
    (None)
  }

  val (io_div_resp) = if (hasDiv) {
    val div_ready = Wire(Bool())
    fu_types += ((FU_DIV, div_ready, "IDiv"))

    val divq = Module(new BranchKillableQueue(new FuncUnitReq(xLen), 3))
    div_ready := divq.io.empty
    divq.io.enq.valid := (exe_uop.valid && exe_uop.bits.fu_code_is(FU_DIV))
    divq.io.enq.bits  := exe_int_req
    divq.io.brupdate  := io_brupdate
    divq.io.flush     := io_kill
    assert(!(divq.io.enq.valid && !divq.io.enq.ready))

    val div = Module(new DivUnit(xLen))
    div.io.req <> divq.io.deq
    div.io.brupdate  := io_brupdate
    div.io.kill      := io_kill

    val div_resp = IO(Decoupled(new ExeUnitResp(xLen)))
    div_resp <> div.io.resp
    Some(div_resp)
  } else {
    assert(!(exe_uop.valid && exe_uop.bits.fu_code_is(FU_DIV)))
    (None)
  }

  io_ready_fu_types := fu_types.map { case (code, ready, _) => Mux(ready, code, 0.U(FUC_SZ.W)) }.reduce(_|_)

}

class FPExeUnit(val hasFDiv: Boolean = false, val hasFpiu: Boolean = false)(implicit p: Parameters)
  extends ExecutionUnit("FP")
  with tile.HasFPUParameters
  with HasFrfReadPorts
{

  io_arb_frf_reqs(0).valid := arb_uop.valid && arb_uop.bits.lrs1_rtype === RT_FLT
  io_arb_frf_reqs(0).bits  := arb_uop.bits.prs1
  io_arb_frf_reqs(1).valid := arb_uop.valid && arb_uop.bits.lrs2_rtype === RT_FLT
  io_arb_frf_reqs(1).bits  := arb_uop.bits.prs2
  io_arb_frf_reqs(2).valid := arb_uop.valid && arb_uop.bits.frs3_en
  io_arb_frf_reqs(2).bits  := arb_uop.bits.prs3

  val exe_rs1_data = RegNext(io_rrd_frf_resps(0))
  val exe_rs2_data = RegNext(io_rrd_frf_resps(1))
  val exe_rs3_data = RegNext(io_rrd_frf_resps(2))

  val exe_fp_req = Wire(new FuncUnitReq(xLen+1))
  exe_fp_req.uop := exe_uop.bits
  exe_fp_req.rs1_data := exe_rs1_data
  exe_fp_req.rs2_data := exe_rs2_data
  exe_fp_req.rs3_data := exe_rs3_data
  exe_fp_req.pred_data := DontCare
  exe_fp_req.imm_data := DontCare

  val fpu = Module(new FPUUnit)
  fu_types += ((FU_FPU, true.B, "FPU"))
  fpu.io.req.valid := exe_uop.valid && (
    exe_uop.bits.fu_code_is(FU_FPU) || (if (hasFpiu) exe_uop.bits.fu_code_is(FU_F2I) else false.B)
  )
  fpu.io.req.bits := exe_fp_req
  fpu.io.fcsr_rm  := io_fcsr_rm
  fpu.io.brupdate := io_brupdate
  fpu.io.kill     := io_kill
  fpu.io.resp.ready := true.B

  val io_fpu_resp = IO(Output(Valid(new ExeUnitResp(xLen+1))))
  io_fpu_resp.valid := fpu.io.resp.valid && !fpu.io.resp.bits.uop.fu_code_is(FU_F2I)
  io_fpu_resp.bits  := fpu.io.resp.bits

  val io_fdiv_resp = if (hasFDiv) {
    val fdivsqrt_ready = Wire(Bool())
    fu_types += ((FU_FDV, fdivsqrt_ready, "FDiv"))

    val divq = Module(new BranchKillableQueue(new FuncUnitReq(xLen+1), 3))
    fdivsqrt_ready    := divq.io.empty
    divq.io.enq.valid := (exe_uop.valid && exe_uop.bits.fu_code_is(FU_FDV))
    divq.io.enq.bits  := exe_fp_req
    divq.io.brupdate  := io_brupdate
    divq.io.flush     := io_kill

    val fdivsqrt = Module(new FDivSqrtUnit)
    fdivsqrt.io.req <> divq.io.deq
    fdivsqrt.io.brupdate := io_brupdate
    fdivsqrt.io.kill     := io_kill
    fdivsqrt.io.fcsr_rm  := io_fcsr_rm

    val fdiv_resp = IO(Decoupled(new ExeUnitResp(xLen+1)))
    fdiv_resp <> fdivsqrt.io.resp
    Some(fdiv_resp)
  } else {
    assert(!(exe_uop.valid && exe_uop.bits.fu_code_is(FU_FDV)))
    None
  }

  val (io_fpiu_resp, io_dgen) = if (hasFpiu) {
    val fpiu_ready = Wire(Bool())
    fu_types += ((FU_F2I, fpiu_ready, "Fpiu"))

    val queue = Module(new BranchKillableQueue(new ExeUnitResp(xLen+1),
      entries = dfmaLatency + 6)) // TODO being overly conservative
    fpiu_ready               := RegNext(queue.io.count < 2.U)
    queue.io.enq.valid       := ( fpu.io.resp.valid &&
                                  fpu.io.resp.bits.uop.fu_code_is(FU_F2I) &&
                                 !fpu.io.resp.bits.uop.uses_stq) // STA means store data gen for floating point
    queue.io.enq.bits        := fpu.io.resp.bits
    queue.io.brupdate        := io_brupdate
    queue.io.flush           := io_kill
    assert(!(queue.io.enq.valid && !queue.io.enq.ready))

    val fpiu_resp = IO(Decoupled(new ExeUnitResp(xLen)))
    fpiu_resp <> queue.io.deq

    val dgen = IO(Valid(new MemGen))
    dgen.valid     := RegNext(exe_uop.valid && exe_uop.bits.uses_stq && !IsKilledByBranch(io_brupdate, exe_uop.bits))
    dgen.bits.uop  := RegNext(exe_uop.bits)
    dgen.bits.data := RegNext(ieee(exe_rs2_data))
    (Some(fpiu_resp), Some(dgen))
  } else {
    assert(!(exe_uop.valid && exe_uop.bits.fu_code_is(FU_F2I)))
    (None, None)
  }

  io_ready_fu_types := fu_types.map { case (code, ready, _) => Mux(ready, code, 0.U(FUC_SZ.W)) }.reduce(_|_)
}
