//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
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

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import scala.collection.mutable.ArrayBuffer

import FUConstants._
import freechips.rocketchip.tile.XLen
import freechips.rocketchip.tile
import boom.common._
import boom.ifu.GetPCFromFtqIO
import boom.util.{ImmGen, IsKilledByBranch, BranchKillableQueue}

// TODO rename to something like MicroOpWithData
class ExeUnitResp(val data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomUOP
{
   val data = Bits(data_width.W)
   val fflags = new ValidIO(new FFlagsResp) // write fflags to ROB

   var writesToIRF = true // does this response unit plug into the integer regfile?
}

class FFlagsResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop = new MicroOp()
   val flags = Bits(tile.FPConstants.FLAGS_SZ.W)
}

class ExecutionUnitIO(
   val num_rf_read_ports: Int,
   val num_rf_write_ports: Int,
   val num_bypass_ports: Int,
   val data_width: Int
   )(implicit p: Parameters) extends BoomBundle()(p)
{
   // describe which functional units we support (used by the issue window)
   val fu_types = Output(Bits(FUC_SZ.W))

   val req     = Flipped(new DecoupledIO(new FuncUnitReq(data_width)))
   val resp    = Vec(num_rf_write_ports, new DecoupledIO(new ExeUnitResp(data_width)))
   val bypass  = Output(new BypassData(num_bypass_ports, data_width))
   val brinfo  = Input(new BrResolutionInfo())

   // only used by the branch unit
   val br_unit = Output(new BranchUnitResp())
   val get_ftq_pc = Flipped(new GetPCFromFtqIO())
   val status = Input(new freechips.rocketchip.rocket.MStatus())

   // only used by the fpu unit
   val fcsr_rm = Input(Bits(tile.FPConstants.RM_SZ.W))

   // only used by the mem unit
   val lsu_io = Flipped(new boom.lsu.LoadStoreUnitIO(decodeWidth))
   val dmem   = new boom.lsu.DCMemPortIO() // TODO move this out of ExecutionUnit
   val com_exception = Input(Bool())
}

abstract class ExecutionUnit(val num_rf_read_ports: Int
                            , val num_rf_write_ports: Int
                            , val num_bypass_stages: Int
                            , val data_width: Int
                            , val num_variable_write_ports: Int = 0
                            , var bypassable: Boolean           = false // TODO make override def for code clarity
                            , val is_mem_unit: Boolean          = false
                            , var uses_csr_wport: Boolean       = false
                            , var uses_iss_unit : Boolean       = true
                            ,     is_branch_unit: Boolean       = false
                            , val has_alu       : Boolean       = false
                            , val has_fpu       : Boolean       = false
                            , val has_mul       : Boolean       = false
                            , val has_div       : Boolean       = false
                            , val has_fdiv      : Boolean       = false
                            , val has_ifpu      : Boolean       = false
                            , val has_fpiu      : Boolean       = false
                            )(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new ExecutionUnitIO(num_rf_read_ports, num_rf_write_ports
                               , num_bypass_stages, data_width))

   io.resp.map(_.bits.fflags.valid := false.B)

   // TODO add "number of fflag ports", so we can properly account for FPU+Mem combinations
   def numBypassPorts: Int = num_bypass_stages
   def hasBranchUnit : Boolean = is_branch_unit
   def isBypassable  : Boolean = bypassable
   def hasFFlags     : Boolean = has_fpu || has_fdiv
   def usesFRF       : Boolean = (has_fpu || has_fdiv) && !(has_alu || has_mul)
   def usesIRF       : Boolean = !(has_fpu || has_fdiv) && (has_alu || has_mul || is_mem_unit || has_ifpu)

   require ((has_fpu || has_fdiv) ^ (has_alu || has_mul || is_mem_unit || has_ifpu),
      "[execute] we no longer support mixing FP and Integer functional units in the same exe unit.")

   def supportedFuncUnits =
   {
      new SupportedFuncUnits(
         alu = has_alu,
         bru = is_branch_unit,
         mem = is_mem_unit,
         muld = has_mul || has_div,
         fpu = has_fpu,
         csr = uses_csr_wport,
         fdiv = has_fdiv,
         ifpu = has_ifpu)
   }
}

class ALUExeUnit(
   is_branch_unit  : Boolean = false,
   shares_csr_wport: Boolean = false,
   has_alu         : Boolean = true,
   has_fpu         : Boolean = false,
   has_mul         : Boolean = false,
   has_div         : Boolean = false,
   has_fdiv        : Boolean = false,
   has_ifpu        : Boolean = false,
   use_slow_mul    : Boolean = false)
   (implicit p: Parameters)
   extends ExecutionUnit(
      num_rf_read_ports = if (has_fpu) 3 else 2,
      num_rf_write_ports = 1,
      num_bypass_stages =
         (if (has_fpu && has_alu) p(tile.TileKey).core.fpu.get.dfmaLatency
         else if (has_alu && has_mul && !use_slow_mul) 3 //TODO XXX p(tile.TileKey).core.imulLatency
         else if (has_alu) 1 else 0),
      data_width = if (has_fpu || has_fdiv) 65 else 64,
      bypassable = has_alu,
      is_mem_unit = false,
      uses_csr_wport = shares_csr_wport,
      is_branch_unit = is_branch_unit,
      has_alu  = has_alu,
      has_fpu  = has_fpu,
      has_mul  = has_mul,
      has_div  = has_div,
      has_fdiv = has_fdiv,
      has_ifpu = has_ifpu)(p)
{
   val has_muldiv = has_div || (has_mul && use_slow_mul)

   val out_str = new StringBuilder
   out_str.append("\n     ExeUnit--")
   if (has_alu) out_str.append("\n       - ALU")
   if (has_fpu) out_str.append("\n       - FPU (Latency: " + dfmaLatency + ")")
   if (has_mul && !use_slow_mul) out_str.append("\n       - Mul (pipelined)")
   if (has_div && has_mul && use_slow_mul) out_str.append("\n       - Mul/Div (unpipelined)")
   else if (has_mul && use_slow_mul) out_str.append("\n       - Mul (unpipelined)")
   else if (has_div) out_str.append("\n       - Div")
   if (has_fdiv) out_str.append("\n       - FDiv/FSqrt")
   if (has_ifpu) out_str.append("\n       - IFPU (for read port access)")

   override def toString: String = out_str.toString

   val muldiv_busy = WireInit(false.B)
   val fdiv_busy = WireInit(false.B)

   // The Functional Units --------------------
   val fu_units = ArrayBuffer[FunctionalUnit]()

   io.fu_types := FU_ALU |
                  Mux(has_fpu.B, FU_FPU, 0.U) |
                  Mux((has_mul && !use_slow_mul).B, FU_MUL, 0.U) |
                  (Mux(!muldiv_busy && (has_mul && use_slow_mul).B, FU_MUL, 0.U)) |
                  (Mux(!muldiv_busy && has_div.B, FU_DIV, 0.U)) |
                  (Mux(shares_csr_wport.B, FU_CSR, 0.U)) |
                  (Mux(is_branch_unit.B, FU_BRU, 0.U)) |
                  Mux(!fdiv_busy && has_fdiv.B, FU_FDV, 0.U)


   // ALU Unit -------------------------------
   var alu: ALUUnit = null
   if (has_alu)
   {
      alu = Module(new ALUUnit(is_branch_unit = is_branch_unit, num_stages = num_bypass_stages))
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
   }

   // branch unit is embedded inside the ALU
   if (is_branch_unit)
   {
      io.br_unit <> alu.io.br_unit
      alu.io.get_ftq_pc <> io.get_ftq_pc
      alu.io.status <> io.status
   }
   else
   {
      io.br_unit.brinfo.valid := false.B
   }

   if (has_alu) fu_units += alu

   // Pipelined, IMul Unit ------------------
   var imul: PipelinedMulUnit = null
   if (has_mul && !use_slow_mul)
   {
      imul = Module(new PipelinedMulUnit(imulLatency))
      imul.io <> DontCare
      imul.io.req.valid         := io.req.valid && io.req.bits.uop.fu_code_is(FU_MUL)
      imul.io.req.bits.uop      := io.req.bits.uop
      imul.io.req.bits.rs1_data := io.req.bits.rs1_data
      imul.io.req.bits.rs2_data := io.req.bits.rs2_data
      imul.io.req.bits.kill     := io.req.bits.kill
      imul.io.brinfo <> io.brinfo
      fu_units += imul
      if (has_fpu) require (imulLatency == dfmaLatency)
   }

   // FPU Unit -----------------------
   var fpu: FPUUnit = null
   val fpu_resp_val = WireInit(false.B)
   val fpu_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   fpu_resp_fflags.valid := false.B
   fpu_resp_fflags.bits := DontCare
   if (has_fpu)
   {
      fpu = Module(new FPUUnit())
      fpu.io.req.valid           := io.req.valid && io.req.bits.uop.fu_code_is(FU_FPU)
      fpu.io.req.bits.uop        := io.req.bits.uop
      fpu.io.req.bits.rs1_data   := io.req.bits.rs1_data
      fpu.io.req.bits.rs2_data   := io.req.bits.rs2_data
      fpu.io.req.bits.rs3_data   := io.req.bits.rs3_data
      fpu.io.req.bits.kill       := io.req.bits.kill
      fpu.io.fcsr_rm             := io.fcsr_rm
      fpu.io.brinfo <> io.brinfo
      fpu_resp_val := fpu.io.resp.valid
      fpu_resp_fflags := fpu.io.resp.bits.fflags
      fu_units += fpu
   }

   // Bypassing ------------------------------
   // (only the ALU is bypassable)

   if (has_alu) io.bypass <> alu.io.bypass

   // FDiv/FSqrt Unit -----------------------
   var fdivsqrt: FDivSqrtUnit = null
   val fdiv_resp_val = WireInit(false.B)
   val fdiv_resp_uop = Wire(new MicroOp())
   val fdiv_resp_data = Wire(Bits(65.W))
   val fdiv_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   fdiv_resp_uop := DontCare
   fdiv_resp_data := DontCare
   fdiv_resp_fflags.valid := false.B
   fdiv_resp_fflags.bits := DontCare
   if (has_fdiv)
   {
      fdivsqrt = Module(new FDivSqrtUnit())
      fdivsqrt.io.req.valid         := io.req.valid && io.req.bits.uop.fu_code_is(FU_FDV)
      fdivsqrt.io.req.bits.uop      := io.req.bits.uop
      fdivsqrt.io.req.bits.rs1_data := io.req.bits.rs1_data
      fdivsqrt.io.req.bits.rs2_data := io.req.bits.rs2_data
      fdivsqrt.io.req.bits.kill     := io.req.bits.kill
      fdivsqrt.io.fcsr_rm           := io.fcsr_rm
      fdivsqrt.io.brinfo <> io.brinfo

      // share write port with the pipelined units
      fdivsqrt.io.resp.ready := !(fu_units.map(_.io.resp.valid).reduce(_|_))

      fdiv_busy := !fdivsqrt.io.req.ready || (io.req.valid && io.req.bits.uop.fu_code_is(FU_FDV))

      fdiv_resp_val := fdivsqrt.io.resp.valid
      fdiv_resp_uop := fdivsqrt.io.resp.bits.uop
      fdiv_resp_data := fdivsqrt.io.resp.bits.data
      fdiv_resp_fflags := fdivsqrt.io.resp.bits.fflags

      fu_units += fdivsqrt
   }

   // Mul/Div/Rem Unit -----------------------
   var muldiv: MulDivUnit = null
   val muldiv_resp_val = WireInit(false.B)
   if (has_muldiv)
   {
      muldiv = Module(new MulDivUnit())
      muldiv.io <> DontCare
      muldiv.io.req.valid           := io.req.valid &&
                                       ((io.req.bits.uop.fu_code_is(FU_DIV) && has_div.B) ||
                                        (io.req.bits.uop.fu_code_is(FU_MUL) && (has_mul && use_slow_mul).B))
      muldiv.io.req.bits.uop        := io.req.bits.uop
      muldiv.io.req.bits.rs1_data   := io.req.bits.rs1_data
      muldiv.io.req.bits.rs2_data   := io.req.bits.rs2_data
      muldiv.io.brinfo              := io.brinfo
      muldiv.io.req.bits.kill       := io.req.bits.kill

      // share write port with the pipelined units
      muldiv.io.resp.ready := !(fu_units.map(_.io.resp.valid).reduce(_|_))

      muldiv_resp_val := muldiv.io.resp.valid
      muldiv_busy := !muldiv.io.req.ready ||
                     (io.req.valid && (io.req.bits.uop.fu_code_is(FU_DIV) ||
                                      (io.req.bits.uop.fu_code_is(FU_MUL) && (has_mul && use_slow_mul).B)))
      fu_units += muldiv
   }

   // Outputs (Write Port #0)  ---------------

   assert (io.resp(0).ready) // don'yet support back-pressuring this unit.

   io.resp(0).valid    := fu_units.map(_.io.resp.valid).reduce(_|_)
   io.resp(0).bits.uop := PriorityMux(fu_units.map(f => (f.io.resp.valid,
                                                   f.io.resp.bits.uop.asUInt))).asTypeOf(new MicroOp())
   io.resp(0).bits.data:= PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.data.asUInt))).asUInt
   // pulled out for critical path reasons
   if (has_alu) {
      io.resp(0).bits.uop.csr_addr := ImmGen(alu.io.resp.bits.uop.imm_packed, IS_I).asUInt
      io.resp(0).bits.uop.ctrl.csr_cmd := alu.io.resp.bits.uop.ctrl.csr_cmd
   }

   io.resp(0).bits.fflags := Mux(fpu_resp_val, fpu_resp_fflags, fdiv_resp_fflags)

   assert ((PopCount(fu_units.map(_.io.resp.valid)) <= 1.U && !muldiv_resp_val && !fdiv_resp_val) ||
          (PopCount(fu_units.map(_.io.resp.valid)) <= 2.U && (muldiv_resp_val || fdiv_resp_val)) ||
          (PopCount(fu_units.map(_.io.resp.valid)) <= 3.U && muldiv_resp_val && fdiv_resp_val)
      , "Multiple functional units are fighting over the write port.")
}


// FPU-only unit, with optional second write-port for ToInt micro-ops.
class FPUExeUnit(
   has_fpu  : Boolean = true,
   has_fdiv : Boolean = false,
   has_fpiu : Boolean = false
   )
   (implicit p: Parameters)
   extends ExecutionUnit(
      num_rf_read_ports = 3,
      num_rf_write_ports = 2, // one for FRF, oen for IRF
      num_bypass_stages = 0,
      data_width = 65,
      bypassable = false,
      has_alu  = false,
      has_fpu  = has_fpu,
      has_fdiv = has_fdiv,
      has_fpiu = has_fpiu)(p)
{
   val out_str = new StringBuilder
   out_str.append("\n     ExeUnit--")
   if (has_fpu)  out_str.append("\n       - FPU (Latency: " + dfmaLatency + ")")
   if (has_fdiv) out_str.append("\n       - FDiv/FSqrt")
   if (has_fpiu) out_str.append("\n       - FPIU (writes to Integer RF)")

   val fdiv_busy = WireInit(false.B)
   val fpiu_busy = WireInit(false.B)

   // The Functional Units --------------------
   val fu_units = ArrayBuffer[FunctionalUnit]()

   io.fu_types := Mux(has_fpu.B, FU_FPU, 0.U) |
                  Mux(!fdiv_busy && has_fdiv.B, FU_FDV, 0.U) |
                  Mux(!fpiu_busy && has_fpiu.B, FU_F2I, 0.U)


   io.resp(0).bits.writesToIRF = false
   io.resp(1).bits.writesToIRF = true
   //io.get_ftq_pc := DontCare

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
   val fdiv_resp_val = WireInit(false.B)
   val fdiv_resp_uop = Wire(new MicroOp())
   val fdiv_resp_data = Wire(Bits(65.W))
   val fdiv_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
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

      fdiv_resp_val := fdivsqrt.io.resp.valid
      fdiv_resp_uop := fdivsqrt.io.resp.bits.uop
      fdiv_resp_data := fdivsqrt.io.resp.bits.data
      fdiv_resp_fflags := fdivsqrt.io.resp.bits.fflags

      fu_units += fdivsqrt
   }


   // Outputs (Write Port #0)  ---------------

   io.resp(0).valid    := fu_units.map(_.io.resp.valid).reduce(_|_) &&
                          !(fpu.io.resp.valid && fpu.io.resp.bits.uop.fu_code_is(FU_F2I))
   io.resp(0).bits.uop := PriorityMux(fu_units.map(f => (f.io.resp.valid,
                                                         f.io.resp.bits.uop.asUInt))).asTypeOf(new MicroOp())
   io.resp(0).bits.data:= PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.data.asUInt))).asUInt
   io.resp(0).bits.fflags := Mux(fpu_resp_val, fpu_resp_fflags, fdiv_resp_fflags)

   // Outputs (Write Port #1) -- FpToInt Queuing Unit -----------------------

   // TODO instantiate our own fpiu; and remove it from fpu.scala.

   // buffer up results since we share write-port on integer regfile.
   val queue = Module(new BranchKillableQueue(new ExeUnitResp(data_width),
                                              entries = dfmaLatency + 3)) // TODO being overly conservative
   queue.io.enq.valid       := fpu.io.resp.valid && fpu.io.resp.bits.uop.fu_code_is(FU_F2I)
   queue.io.enq.bits.uop    := fpu.io.resp.bits.uop
   queue.io.enq.bits.data   := fpu.io.resp.bits.data
   queue.io.enq.bits.fflags := fpu.io.resp.bits.fflags
   queue.io.brinfo          := io.brinfo
   queue.io.flush           := io.req.bits.kill
   io.resp(1) <> queue.io.deq

   fpiu_busy := !(queue.io.empty)

   assert (queue.io.enq.ready) // If this backs up, we've miscalculated the size of the queue.

   override def toString: String = out_str.toString
}


class FDivSqrtExeUnit(implicit p: Parameters)
   extends ExecutionUnit(num_rf_read_ports = 2
                                       , num_rf_write_ports = 1
                                       , num_bypass_stages = 0
                                       , data_width = 65
                                       , num_variable_write_ports = 1
                                       , has_fdiv = true
                                       )
{
   val fdiv_busy = Wire(Bool())
   io.fu_types := Mux(!fdiv_busy, FU_FDV, 0.U)

   val fdivsqrt = Module(new FDivSqrtUnit())
   fdivsqrt.io.req <> io.req
   fdivsqrt.io.fcsr_rm    := io.fcsr_rm
   io.resp(0).valid       := fdivsqrt.io.resp.valid
   io.resp(0).bits.uop    := fdivsqrt.io.resp.bits.uop
   io.resp(0).bits.data   := fdivsqrt.io.resp.bits.data
   io.resp(0).bits.fflags := fdivsqrt.io.resp.bits.fflags
   fdivsqrt.io.brinfo <> io.brinfo
   io.bypass <> fdivsqrt.io.bypass

   fdiv_busy := !fdivsqrt.io.req.ready || io.req.valid

   override def toString: String =
      "\n     ExeUnit--" +
      "\n       - FDiv/FSqrt"
}


class IntToFPExeUnit(implicit p: Parameters) extends ExecutionUnit(
   has_ifpu = true,
   num_rf_read_ports = 2,
   num_rf_write_ports = 1,
   num_bypass_stages = 0,
   data_width = 65,
   // don't schedule uops from issue-window -- we're hard-hacking the datapath,
   // since the operand data comes from the IRF but writes back to the FRF.
   uses_iss_unit = false)
{
   val busy = WireInit(false.B)
   io.fu_types := Mux(!busy, FU_I2F, 0.U)
   io.resp(0).bits.writesToIRF = false

   val ifpu = Module(new IntToFPUnit(latency=intToFpLatency))
   ifpu.io.req <> io.req
   ifpu.io.fcsr_rm := io.fcsr_rm
   ifpu.io.brinfo <> io.brinfo
   ifpu.io.status := DontCare
   ifpu.io.get_ftq_pc := DontCare
   ifpu.io.resp.ready := DontCare
   io.bypass <> ifpu.io.bypass

   // buffer up results since we share write-port on integer regfile.
   val queue = Module(new BranchKillableQueue(new ExeUnitResp(data_width),
                                              entries = intToFpLatency + 3)) // TODO being overly conservative
   queue.io.enq.valid       := ifpu.io.resp.valid
   queue.io.enq.bits.uop    := ifpu.io.resp.bits.uop
   queue.io.enq.bits.data   := ifpu.io.resp.bits.data
   queue.io.enq.bits.fflags := ifpu.io.resp.bits.fflags
   queue.io.brinfo := io.brinfo
   queue.io.flush := io.req.bits.kill

   io.resp(0) <> queue.io.deq

   busy := !(queue.io.empty)


   assert (queue.io.enq.ready) // If this backs up, we've miscalculated the size of the queue.

   override def toString: String =
      "\n     ExeUnit--" +
      "\n       - IntToFP"
}


class MemExeUnit(implicit p: Parameters) extends ExecutionUnit(num_rf_read_ports = 2,
   num_rf_write_ports = 1,
   num_bypass_stages = 0,
   data_width = if(p(tile.TileKey).core.fpu.nonEmpty) 65 else p(tile.XLen),
   num_variable_write_ports = 1,
   bypassable = false,
   is_mem_unit = true)(p)
   with freechips.rocketchip.rocket.constants.MemoryOpConstants
{
   io.fu_types := FU_MEM

   // Perform address calculation
   val maddrcalc = Module(new MemAddrCalcUnit())
   maddrcalc.io.req <> io.req

   maddrcalc.io.brinfo <> io.brinfo
   maddrcalc.io.status := DontCare
   maddrcalc.io.get_ftq_pc := DontCare
   maddrcalc.io.fcsr_rm := DontCare
   maddrcalc.io.resp.ready := DontCare
   io.bypass <> maddrcalc.io.bypass  // TODO this is not where the bypassing should
                                     // occur from, is there any bypassing happening?!

   // enqueue addresses,st-data at the end of Execute
   io.lsu_io.exe_resp.valid := maddrcalc.io.resp.valid
   io.lsu_io.exe_resp.bits := maddrcalc.io.resp.bits


   // TODO get rid of com_exception and guard with an assert? Need to surpress within dc-shim.
//   assert (!(io.com_exception && lsu.io.memreq_uop.is_load && lsu.io.memreq_val),
//      "[execute] a valid load is returning while an exception is being thrown.")
   io.dmem.req.valid      := Mux(io.com_exception && io.lsu_io.memreq_uop.is_load,
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

   val memresp_data = Mux(io.lsu_io.forward_val, io.lsu_io.forward_data, io.dmem.resp.bits.data_subword)

   io.lsu_io.memresp.valid := memresp_val
   io.lsu_io.memresp.bits  := memresp_uop


   // Hook up loads to the response
   io.resp(0).valid                 := RegNext(memresp_val && !IsKilledByBranch(io.brinfo, memresp_uop))
   io.resp(0).bits.uop              := RegNext(memresp_uop)
   io.resp(0).bits.uop.ctrl.rf_wen  := RegNext(memresp_rf_wen)
   io.resp(0).bits.data             := RegNext(memresp_data)

   override def toString: String =
      "\n     ExeUnit--" +
      "\n       - Mem"
}

