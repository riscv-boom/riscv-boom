//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
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

import scala.collection.mutable.ArrayBuffer

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.XLen
import freechips.rocketchip.tile

import FUConstants._
import boom.common._
import boom.ifu.GetPCFromFtqIO
import boom.util.{ImmGen, IsKilledByBranch, BranchKillableQueue}

/**
 * Response from Execution Unit. Bundles a MicroOp with data
 * TODO rename to something like MicroOpWithData
 *
 * @param data_width width of the data coming from the execution unit
 */
class ExeUnitResp(val data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomUOP
{
   val data = Bits(data_width.W)
   val fflags = new ValidIO(new FFlagsResp) // write fflags to ROB
}

/**
 * Floating Point flag response
 */
class FFlagsResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop = new MicroOp()
   val flags = Bits(tile.FPConstants.FLAGS_SZ.W)
}

/**
 * IO bundle for a Execution Unit.
 *
 * @param writes_irf does this exe unit need a integer regfile port
 * @param writes_ll_irf does this exe unit need a long latency integer regfile port
 * @param writes_frf does this exe unit need a FP regfile port
 * @param writes_ll_frf does this exe unit need a long latency FP regfile port
 * @param num_bypass_ports number of bypass ports for the exe unit
 * @param data_width width of the data coming out of the execution unit
 */
class ExecutionUnitIO(
   val writes_irf: Boolean,
   val writes_ll_irf: Boolean,
   val writes_frf: Boolean,
   val writes_ll_frf: Boolean,
   val num_bypass_ports: Int,
   val data_width: Int
   )(implicit p: Parameters) extends BoomBundle()(p)
{
   // describe which functional units we support (used by the issue window)
   val fu_types = Output(Bits(FUC_SZ.W))

   val req     = Flipped(new DecoupledIO(new FuncUnitReq(data_width)))

   val iresp    = if (writes_irf)    new DecoupledIO(new ExeUnitResp(data_width)) else null
   val fresp    = if (writes_frf)    new DecoupledIO(new ExeUnitResp(data_width)) else null
   val ll_iresp = if (writes_ll_irf) new DecoupledIO(new ExeUnitResp(data_width)) else null
   val ll_fresp = if (writes_ll_frf) new DecoupledIO(new ExeUnitResp(data_width)) else null

   val bypass   = Output(new BypassData(num_bypass_ports, data_width))
   val brinfo   = Input(new BrResolutionInfo())

   // only used by the branch unit
   val br_unit    = Output(new BranchUnitResp())
   val get_ftq_pc = Flipped(new GetPCFromFtqIO())
   val status     = Input(new freechips.rocketchip.rocket.MStatus())

   // only used by the fpu unit
   val fcsr_rm = Input(Bits(tile.FPConstants.RM_SZ.W))

   // only used by the mem unit
   val lsu_io = Flipped(new boom.lsu.LoadStoreUnitIO(decodeWidth))
   val dmem   = new boom.lsu.DCMemPortIO() // TODO move this out of ExecutionUnit
   val com_exception = Input(Bool())
}

/**
 * Abstract Top level Execution Unit that wraps lower level functional units to make a
 * multi function execution unit.
 *
 * @param reads_irf does this exe unit need a integer regfile port
 * @param writes_irf does this exe unit need a integer regfile port
 * @param reads_frf does this exe unit need a integer regfile port
 * @param writes_frf does this exe unit need a integer regfile port
 * @param writes_ll_irf does this exe unit need a integer regfile port
 * @param writes_ll_frf does this exe unit need a integer regfile port
 * @param num_bypass_stages number of bypass ports for the exe unit
 * @param data_width width of the data coming out of the exe unit
 * @param bypassable is the exe unit able to be bypassed
 * @param has_mem does the exe unit have a MemAddrCalcUnit
 * @param uses_csr_wport does the exe unit write to the CSRFile
 * @param has_br_unit does the exe unit have a branch unit
 * @param has_alu does the exe unit have a alu
 * @param has_fpu does the exe unit have a fpu
 * @param has_mul does the exe unit have a multiplier
 * @param has_div does the exe unit have a divider
 * @param has_fdiv does the exe unit have a FP divider
 * @param has_ifpu does the exe unit have a int to FP unit
 * @param has_fpiu does the exe unit have a FP to int unit
 */
abstract class ExecutionUnit( val reads_irf     : Boolean       = false,
                              val writes_irf    : Boolean       = false,
                              val reads_frf     : Boolean       = false,
                              val writes_frf    : Boolean       = false,
                              val writes_ll_irf : Boolean       = false,
                              val writes_ll_frf : Boolean       = false,
                              val num_bypass_stages: Int,
                              val data_width    : Int,
                              val bypassable    : Boolean       = false, // TODO make override def for code clarity
                              val has_mem       : Boolean       = false,
                              val uses_csr_wport: Boolean       = false,
                              val has_br_unit   : Boolean       = false,
                              val has_alu       : Boolean       = false,
                              val has_fpu       : Boolean       = false,
                              val has_mul       : Boolean       = false,
                              val has_div       : Boolean       = false,
                              val has_fdiv      : Boolean       = false,
                              val has_ifpu      : Boolean       = false,
                              val has_fpiu      : Boolean       = false
                            )(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new ExecutionUnitIO(writes_irf, writes_ll_irf, writes_frf, writes_ll_frf,
      num_bypass_stages, data_width))

   if (writes_irf)    { io.iresp.bits.fflags.valid    := false.B; assert(io.iresp.ready) }
   if (writes_ll_irf) { io.ll_iresp.bits.fflags.valid := false.B }
   if (writes_frf)    { io.fresp.bits.fflags.valid    := false.B; assert(io.fresp.ready) }
   if (writes_ll_frf) { io.ll_fresp.bits.fflags.valid := false.B }

   // TODO add "number of fflag ports", so we can properly account for FPU+Mem combinations
   def hasFFlags     : Boolean = has_fpu || has_fdiv

   require ((has_fpu || has_fdiv) ^ (has_alu || has_mul || has_mem || has_ifpu),
      "[execute] we no longer support mixing FP and Integer functional units in the same exe unit.")

   def supportedFuncUnits =
   {
      new SupportedFuncUnits(
         alu = has_alu,
         bru = has_br_unit,
         mem = has_mem,
         muld = has_mul || has_div,
         fpu = has_fpu,
         csr = uses_csr_wport,
         fdiv = has_fdiv,
         ifpu = has_ifpu)
   }
}

/**
 * ALU execution unit that can have a branch, alu, mul, div, int to FP,
 * and memory unit.
 *
 * @param has_br_unit does the exe unit have a branch unit
 * @param shares_csr_wport does the exe unit write to the CSRFile
 * @param has_alu does the exe unit have a alu
 * @param has_mul does the exe unit have a multiplier
 * @param has_div does the exe unit have a divider
 * @param has_ifpu does the exe unit have a int to FP unit
 * @param has_mem does the exe unit have a MemAddrCalcUnit
 */
class ALUExeUnit(
   has_br_unit     : Boolean = false,
   shares_csr_wport: Boolean = false,
   has_alu         : Boolean = true,
   has_mul         : Boolean = false,
   has_div         : Boolean = false,
   has_ifpu        : Boolean = false,
   has_mem         : Boolean = false)
   (implicit p: Parameters)
   extends ExecutionUnit(
      reads_irf  = true,
      writes_irf = has_alu || has_mul || has_div,
      writes_ll_irf = has_mem,
      writes_ll_frf = (has_ifpu || has_mem)
         && p(tile.TileKey).core.fpu != None,
      num_bypass_stages =
         if (has_alu && has_mul) 3 //TODO XXX p(tile.TileKey).core.imulLatency
         else if (has_alu) 1 else 0,
      data_width     = p(tile.XLen) + 1,
      bypassable     = has_alu,
      uses_csr_wport = shares_csr_wport,
      has_br_unit    = has_br_unit,
      has_alu        = has_alu,
      has_mul        = has_mul,
      has_div        = has_div,
      has_ifpu       = has_ifpu,
      has_mem        = has_mem)(p)
   with freechips.rocketchip.rocket.constants.MemoryOpConstants
{
   require(!(has_mem && has_ifpu),
      "TODO. Currently do not support AluMemExeUnit with FP")

   val out_str = new StringBuilder
   out_str.append("\n   ==ExeUnit==")
   if (has_alu)  out_str.append("\n     - ALU")
   if (has_mul)  out_str.append("\n     - Mul")
   if (has_div)  out_str.append("\n     - Div")
   if (has_ifpu) out_str.append("\n     - IFPU")
   if (has_mem)  out_str.append("\n     - Mem")
   override def toString: String = out_str.toString

   val div_busy  = WireInit(false.B)
   val ifpu_busy = WireInit(false.B)

   // The Functional Units --------------------
   val fu_units = ArrayBuffer[FunctionalUnit]()

   io.fu_types := Mux(has_alu.B, FU_ALU, 0.U) |
                  Mux(has_mul.B, FU_MUL, 0.U) |
                  Mux(!div_busy && has_div.B, FU_DIV, 0.U) |
                  Mux(shares_csr_wport.B, FU_CSR, 0.U) |
                  Mux(has_br_unit.B, FU_BRU, 0.U) |
                  Mux(!ifpu_busy && has_ifpu.B, FU_I2F, 0.U) |
                  Mux(has_mem.B, FU_MEM, 0.U)

   // ALU Unit -------------------------------
   var alu: ALUUnit = null
   if (has_alu)
   {
      alu = Module(new ALUUnit(is_branch_unit = has_br_unit,
                               num_stages = num_bypass_stages,
                               data_width = xLen))
      alu.io.req.valid         := io.req.valid &&
                                  (io.req.bits.uop.fu_code === FU_ALU ||
                                   io.req.bits.uop.fu_code === FU_BRU ||
                                   io.req.bits.uop.fu_code === FU_CSR)
      alu.io.req.bits.uop      := io.req.bits.uop
      alu.io.req.bits.kill     := io.req.bits.kill
      alu.io.req.bits.rs1_data := io.req.bits.rs1_data
      alu.io.req.bits.rs2_data := io.req.bits.rs2_data
      alu.io.req.bits.rs3_data := DontCare
      alu.io.resp.ready := DontCare
      alu.io.fcsr_rm := DontCare
      alu.io.brinfo <> io.brinfo
      alu.io.status := DontCare
      alu.io.get_ftq_pc := DontCare

      fu_units += alu

      // Bypassing only applies to ALU
      io.bypass <> alu.io.bypass

      // branch unit is embedded inside the ALU
      if (has_br_unit)
      {
         io.br_unit <> alu.io.br_unit
         alu.io.get_ftq_pc <> io.get_ftq_pc
         alu.io.status <> io.status
      }
      else
      {
         io.br_unit.brinfo.valid := false.B
      }
   }

   // Pipelined, IMul Unit ------------------
   var imul: PipelinedMulUnit = null
   if (has_mul)
   {
      imul = Module(new PipelinedMulUnit(imulLatency, xLen))
      imul.io <> DontCare
      imul.io.req.valid         := io.req.valid && io.req.bits.uop.fu_code_is(FU_MUL)
      imul.io.req.bits.uop      := io.req.bits.uop
      imul.io.req.bits.rs1_data := io.req.bits.rs1_data
      imul.io.req.bits.rs2_data := io.req.bits.rs2_data
      imul.io.req.bits.kill     := io.req.bits.kill
      imul.io.brinfo <> io.brinfo
      fu_units += imul
   }

   var ifpu: IntToFPUnit = null
   if (has_ifpu)
   {
      ifpu = Module(new IntToFPUnit(latency=intToFpLatency))
      ifpu.io.req        <> io.req
      ifpu.io.req.valid  := io.req.valid && io.req.bits.uop.fu_code_is(FU_I2F)
      ifpu.io.fcsr_rm    := io.fcsr_rm
      ifpu.io.brinfo     <> io.brinfo
      ifpu.io.status     := DontCare
      ifpu.io.get_ftq_pc := DontCare
      ifpu.io.resp.ready := DontCare

      // buffer up results since we share write-port on integer regfile.
      val queue = Module(new BranchKillableQueue(new ExeUnitResp(data_width),
         entries = intToFpLatency + 3)) // TODO being overly conservative
      queue.io.enq.valid       := ifpu.io.resp.valid
      queue.io.enq.bits.uop    := ifpu.io.resp.bits.uop
      queue.io.enq.bits.data   := ifpu.io.resp.bits.data
      queue.io.enq.bits.fflags := ifpu.io.resp.bits.fflags
      queue.io.brinfo := io.brinfo
      queue.io.flush := io.req.bits.kill

      io.ll_fresp <> queue.io.deq
      ifpu_busy := !(queue.io.empty)
      assert (queue.io.enq.ready)

   }

   // Div/Rem Unit -----------------------
   var div: DivUnit = null
   val div_resp_val = WireInit(false.B)
   if (has_div)
   {
      div = Module(new DivUnit(xLen))
      div.io <> DontCare
      div.io.req.valid           := io.req.valid && io.req.bits.uop.fu_code_is(FU_DIV) && has_div.B
      div.io.req.bits.uop        := io.req.bits.uop
      div.io.req.bits.rs1_data   := io.req.bits.rs1_data
      div.io.req.bits.rs2_data   := io.req.bits.rs2_data
      div.io.brinfo              := io.brinfo
      div.io.req.bits.kill       := io.req.bits.kill

      // share write port with the pipelined units
      div.io.resp.ready := !(fu_units.map(_.io.resp.valid).reduce(_|_))

      div_resp_val := div.io.resp.valid
      div_busy     := !div.io.req.ready ||
                      (io.req.valid && io.req.bits.uop.fu_code_is(FU_DIV))

      fu_units += div
   }

   // Mem Unit --------------------------
   if (has_mem)
   {
      require(!has_alu || usingUnifiedMemIntIQs)
      val maddrcalc = Module(new MemAddrCalcUnit)
      maddrcalc.io.req        <> io.req
      maddrcalc.io.req.valid  := io.req.valid && io.req.bits.uop.fu_code_is(FU_MEM)
      maddrcalc.io.brinfo     <> io.brinfo
      maddrcalc.io.status     := DontCare
      maddrcalc.io.get_ftq_pc := DontCare
      maddrcalc.io.fcsr_rm    := DontCare
      maddrcalc.io.resp.ready := DontCare
      io.bypass <> maddrcalc.io.bypass // TODO this is not where the bypassing should
                                       // occur from, is there any bypassing happening?!

      io.lsu_io.exe_resp.valid := maddrcalc.io.resp.valid
      io.lsu_io.exe_resp.bits  := maddrcalc.io.resp.bits

      // TODO get rid of com_exception and guard with an assert? Need to surpress within dc-shim.
      //   assert (!(io.com_exception && lsu.io.memreq_uop.is_load && lsu.io.memreq_val),
      //      "[execute] a valid load is returning while an exception is being thrown.")
      io.dmem.req.valid       := Mux(io.com_exception && io.lsu_io.memreq_uop.is_load,
                                     false.B,
                                     io.lsu_io.memreq_val)
      io.dmem.req.bits.addr  := io.lsu_io.memreq_addr
      io.dmem.req.bits.data  := io.lsu_io.memreq_wdata
      io.dmem.req.bits.uop   := io.lsu_io.memreq_uop
      io.dmem.req.bits.kill  := io.lsu_io.memreq_kill // load kill request sent to memory

      // I should be timing forwarding to coincide with dmem resps, so I'm not clobbering
      //anything....
      val memresp_val    = Mux(io.com_exception && io.dmem.resp.bits.uop.is_load, false.B,
                               io.lsu_io.forward_val || io.dmem.resp.valid)
      val memresp_rf_wen = (io.dmem.resp.valid &&
                           (io.dmem.resp.bits.uop.mem_cmd === M_XRD || io.dmem.resp.bits.uop.is_amo)) ||
                        io.lsu_io.forward_val // TODO should I refactor this to use is_load?
      val memresp_uop    = Mux(io.lsu_io.forward_val, io.lsu_io.forward_uop,
                                                      io.dmem.resp.bits.uop)

      val memresp_data = Mux(io.lsu_io.forward_val,
         io.lsu_io.forward_data, io.dmem.resp.bits.data_subword)

      io.lsu_io.memresp.valid := memresp_val
      io.lsu_io.memresp.bits  := memresp_uop

      // Hook up loads to the response
      io.ll_iresp.valid                 := RegNext(memresp_val
                                                && !IsKilledByBranch(io.brinfo, memresp_uop)
                                                && memresp_rf_wen
                                                && memresp_uop.dst_rtype === RT_FIX)
      io.ll_iresp.bits.uop              := RegNext(memresp_uop)
      io.ll_iresp.bits.uop.ctrl.rf_wen  := RegNext(memresp_rf_wen)
      io.ll_iresp.bits.data             := RegNext(memresp_data)

      if (usingFPU)
      {
         require(!has_alu, "Don't support this yet")
         io.ll_fresp.valid                 := RegNext(memresp_val
                                                   && !IsKilledByBranch(io.brinfo, memresp_uop)
                                                   && memresp_rf_wen
                                                   && memresp_uop.dst_rtype === RT_FLT)
         io.ll_fresp.bits.uop              := RegNext(memresp_uop)
         io.ll_fresp.bits.uop.ctrl.rf_wen  := RegNext(memresp_rf_wen)
         io.ll_fresp.bits.data             := RegNext(memresp_data)
      }

   }

   // Outputs (Write Port #0)  ---------------
   if (writes_irf)
   {
      io.iresp.valid    := fu_units.map(_.io.resp.valid).reduce(_|_)
      io.iresp.bits.uop := PriorityMux(fu_units.map(f =>
         (f.io.resp.valid, f.io.resp.bits.uop.asUInt))).asTypeOf(new MicroOp())
      io.iresp.bits.data:= PriorityMux(fu_units.map(f =>
         (f.io.resp.valid, f.io.resp.bits.data.asUInt))).asUInt

      // pulled out for critical path reasons
      // TODO: Does this make sense as part of the iresp bundle?
      if (has_alu)
      {
         io.iresp.bits.uop.csr_addr := ImmGen(alu.io.resp.bits.uop.imm_packed, IS_I).asUInt
         io.iresp.bits.uop.ctrl.csr_cmd := alu.io.resp.bits.uop.ctrl.csr_cmd
      }
   }

   assert ((PopCount(fu_units.map(_.io.resp.valid)) <= 1.U && !div_resp_val) ||
           (PopCount(fu_units.map(_.io.resp.valid)) <= 2.U && (div_resp_val)),
           "Multiple functional units are fighting over the write port.")
}

/**
 * FPU-only unit, with optional second write-port for ToInt micro-ops.
 *
 * @param has_fpu does the exe unit have a fpu
 * @param has_fdiv does the exe unit have a FP divider
 * @param has_fpiu does the exe unit have a FP to int unit
 */
class FPUExeUnit(
   has_fpu  : Boolean = true,
   has_fdiv : Boolean = false,
   has_fpiu : Boolean = false
   )
   (implicit p: Parameters)
   extends ExecutionUnit(
      reads_frf  = true,
      writes_frf = true,
      writes_ll_irf = has_fpiu,
      writes_irf = has_fpiu, // HACK: the "irf" port actually goes to the LSU fpu SDATAGen
      num_bypass_stages = 0,
      data_width = p(tile.TileKey).core.fpu.get.fLen + 1,
      bypassable = false,
      has_fpu  = has_fpu,
      has_fdiv = has_fdiv,
      has_fpiu = has_fpiu)(p) with tile.HasFPUParameters
{
   val out_str = new StringBuilder
   out_str.append("\n   ==ExeUnit==")
   if (has_fpu)  out_str.append("\n     - FPU (Latency: " + dfmaLatency + ")")
   if (has_fdiv) out_str.append("\n     - FDiv/FSqrt")
   if (has_fpiu) out_str.append("\n     - FPIU (writes to Integer RF)")

   val fdiv_busy = WireInit(false.B)
   val fpiu_busy = WireInit(false.B)

   // The Functional Units --------------------
   val fu_units = ArrayBuffer[FunctionalUnit]()

   io.fu_types := Mux(has_fpu.B, FU_FPU, 0.U) |
                  Mux(!fdiv_busy && has_fdiv.B, FU_FDV, 0.U) |
                  Mux(!fpiu_busy && has_fpiu.B, FU_F2I, 0.U)

   // FPU Unit -----------------------
   var fpu: FPUUnit = null
   val fpu_resp_val = WireInit(false.B)
   val fpu_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   fpu_resp_fflags.valid := false.B
   if (has_fpu)
   {
      fpu = Module(new FPUUnit())
      fpu.io.req.valid           := io.req.valid &&
                                    (io.req.bits.uop.fu_code_is(FU_FPU) ||
                                    io.req.bits.uop.fu_code_is(FU_F2I)) // TODO move to using a separate unit
      fpu.io.req.bits.uop        := io.req.bits.uop
      fpu.io.req.bits.rs1_data   := io.req.bits.rs1_data
      fpu.io.req.bits.rs2_data   := io.req.bits.rs2_data
      fpu.io.req.bits.rs3_data   := io.req.bits.rs3_data
      fpu.io.req.bits.kill       := io.req.bits.kill
      fpu.io.fcsr_rm             := io.fcsr_rm
      fpu.io.brinfo <> io.brinfo
      fpu.io.status := DontCare
      fpu.io.get_ftq_pc := DontCare
      fpu.io.resp.ready := DontCare
      fpu_resp_val := fpu.io.resp.valid
      fpu_resp_fflags := fpu.io.resp.bits.fflags
      fu_units += fpu
   }

   // FDiv/FSqrt Unit -----------------------
   var fdivsqrt: FDivSqrtUnit = null
   val fdiv_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   fdiv_resp_fflags := DontCare
   fdiv_resp_fflags.valid := false.B
   if (has_fdiv)
   {
      fdivsqrt = Module(new FDivSqrtUnit())
      fdivsqrt.io.req.valid         := io.req.valid && io.req.bits.uop.fu_code_is(FU_FDV)
      fdivsqrt.io.req.bits.uop      := io.req.bits.uop
      fdivsqrt.io.req.bits.rs1_data := io.req.bits.rs1_data
      fdivsqrt.io.req.bits.rs2_data := io.req.bits.rs2_data
      fdivsqrt.io.req.bits.rs3_data := DontCare
      fdivsqrt.io.req.bits.kill     := io.req.bits.kill
      fdivsqrt.io.fcsr_rm           := io.fcsr_rm
      fdivsqrt.io.brinfo <> io.brinfo
      fdivsqrt.io.status := DontCare
      fdivsqrt.io.get_ftq_pc := DontCare

      // share write port with the pipelined units
      fdivsqrt.io.resp.ready := !(fu_units.map(_.io.resp.valid).reduce(_|_)) // TODO PERF will get blocked by fpiu.

      fdiv_busy := !fdivsqrt.io.req.ready || (io.req.valid && io.req.bits.uop.fu_code_is(FU_FDV))

      fdiv_resp_fflags := fdivsqrt.io.resp.bits.fflags

      fu_units += fdivsqrt
   }

   // Outputs (Write Port #0)  ---------------

   io.fresp.valid    := fu_units.map(_.io.resp.valid).reduce(_|_) &&
                          !(fpu.io.resp.valid && fpu.io.resp.bits.uop.fu_code_is(FU_F2I))
   io.fresp.bits.uop := PriorityMux(fu_units.map(f => (f.io.resp.valid,
                                                         f.io.resp.bits.uop.asUInt))).asTypeOf(new MicroOp())
   io.fresp.bits.data:= PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.data.asUInt))).asUInt
   io.fresp.bits.fflags := Mux(fpu_resp_val, fpu_resp_fflags, fdiv_resp_fflags)

   // Outputs (Write Port #1) -- FpToInt Queuing Unit -----------------------

   if (has_fpiu) {
      // TODO instantiate our own fpiu; and remove it from fpu.scala.
      // buffer up results since we share write-port on integer regfile.
      val queue = Module(new BranchKillableQueue(new ExeUnitResp(data_width),
         entries = dfmaLatency + 3)) // TODO being overly conservative
      queue.io.enq.valid       := (fpu.io.resp.valid &&
                                   fpu.io.resp.bits.uop.fu_code_is(FU_F2I) &&
                                   fpu.io.resp.bits.uop.uopc =/= uopSTD)
      queue.io.enq.bits.uop    := fpu.io.resp.bits.uop
      queue.io.enq.bits.data   := fpu.io.resp.bits.data
      queue.io.enq.bits.fflags := fpu.io.resp.bits.fflags
      queue.io.brinfo          := io.brinfo
      queue.io.flush           := io.req.bits.kill
      io.ll_iresp <> queue.io.deq

      fpiu_busy := !(queue.io.empty)

      io.iresp.valid     := io.req.valid && io.req.bits.uop.uopc === uopSTD
      io.iresp.bits.uop  := io.req.bits.uop
      io.iresp.bits.data := ieee(io.req.bits.rs2_data)

      assert (queue.io.enq.ready) // If this backs up, we've miscalculated the size of the queue.
   }

   override def toString: String = out_str.toString
}
