//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Execution Units
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2013 Apr 27
//
// The issue window schedules micro-ops onto a specific execution pipeline
// A given execution pipeline may contain multiple functional units; one or more
// read ports, and one or more writeports.

package boom.exu

import Chisel._
import freechips.rocketchip.config.Parameters
import scala.collection.mutable.ArrayBuffer

import FUConstants._
import freechips.rocketchip.tile.XLen
import freechips.rocketchip.tile
import boom.common._
import boom.ifu.GetPCFromFtqIO
import boom.util.{ImmGen, BranchKillableQueue, HasBoomUOP, IsKilledByBranch}

// TODO rename to something like MicroOpWithData
class ExeUnitResp(data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
with HasBoomUOP
{
   val data  = Bits(width = data_width)
   val mask  = UInt(width = 128 / 8)
   val fflags = new ValidIO(new FFlagsResp) // write fflags to ROB

   var writesToIRF = true  // does this response unit plug into the integer regfile?
   var writesToVIQ = false // does this response unit plug into the vector issue slot?
   override def cloneType: this.type = new ExeUnitResp(data_width).asInstanceOf[this.type]
}

class FFlagsResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop = new MicroOp()
   val flags = Bits(width=tile.FPConstants.FLAGS_SZ)
}

class ExecutionUnitIO(
   val num_rf_read_ports: Int,
   val num_rf_write_ports: Int,
   val num_ll_write_ports: Int, // Write ports that don't connect straight back to the RF this execution unit uses
   val num_bypass_ports: Int,
   val data_width: Int
   )(implicit p: Parameters) extends BoomBundle()(p)
{
   // describe which functional units we support (used by the issue window)
   val fu_types = Bits(OUTPUT, FUC_SZ)

   val req     = (new DecoupledIO(new FuncUnitReq(data_width))).flip
   val resp    = Vec(num_rf_write_ports, new DecoupledIO(new ExeUnitResp(data_width)))
   val ll_resp = Vec(num_ll_write_ports, new DecoupledIO(new ExeUnitResp(data_width)))

   val bypass  = new BypassData(num_bypass_ports, data_width).asOutput
   val brinfo  = new BrResolutionInfo().asInput

   // only used by the branch unit
   val br_unit = new BranchUnitResp().asOutput
   val get_ftq_pc = new GetPCFromFtqIO().flip
   val status = new freechips.rocketchip.rocket.MStatus().asInput

   // only used by the fpu unit
   val fcsr_rm = Bits(INPUT, tile.FPConstants.RM_SZ)

   // only used by the mem unit
   val lsu_io = new boom.lsu.LoadStoreUnitIO(decodeWidth).flip
   val dmem   = new boom.lsu.DCMemPortIO() // TODO move this out of ExecutionUnit
   val com_exception = Bool(INPUT)
   val debug_tsc_reg = UInt(width=128.W).asInput

   val vl = UInt(width=VL_SZ.W).asInput
}

abstract class ExecutionUnit(val num_rf_read_ports: Int
                            , val num_rf_write_ports: Int
                            , val num_ll_write_ports: Int       = 0
                            , val num_bypass_stages: Int
                            , val data_width: Int
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
                            , val has_itov      : Boolean       = false
                            , val has_fpiu      : Boolean       = false
                            , val has_vfpu      : Boolean       = false
                            , val has_valu      : Boolean       = false
                            , val has_fpvu      : Boolean       = false
)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new ExecutionUnitIO(num_rf_read_ports, num_rf_write_ports, num_ll_write_ports
                               , num_bypass_stages, data_width))

   io.resp.map(_.bits.fflags.valid := Bool(false))

   // TODO add "number of fflag ports", so we can properly account for FPU+Mem combinations
   def numBypassPorts: Int = num_bypass_stages
   def hasBranchUnit : Boolean = is_branch_unit
   def isBypassable  : Boolean = bypassable
   def hasFFlags     : Boolean = has_fpu || has_fdiv
   def usesFRF       : Boolean = (has_fpu || has_fdiv) && !(has_alu || has_mul)
   def usesIRF       : Boolean = !(has_fpu || has_fdiv) && (has_alu || has_mul || is_mem_unit || has_ifpu)

   if (!has_vfpu && !has_valu)
   {
      require ((has_fpu || has_fdiv) ^ (has_alu || has_mul || is_mem_unit || has_ifpu),
         "[execute] we no longer support mixing FP and Integer functional units in the same exe unit.")
   }
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
         ifpu = has_ifpu,
         itov = has_itov,
         vfpu = has_vfpu,
         valu = has_valu
      )
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
   has_itov        : Boolean = true,
   use_slow_mul    : Boolean = false)
   (implicit p: Parameters)
   extends ExecutionUnit(
      num_rf_read_ports = if (has_fpu) 3 else 2,
      num_rf_write_ports = 1,
      num_ll_write_ports = if (has_itov) 1 else 0,
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
      has_ifpu = has_ifpu,
      has_itov = has_itov)(p)
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
   if (has_itov) out_str.append("\n       - IToV (for vinsert into vector)")

   override def toString: String = out_str.toString

   val muldiv_busy = Wire(init=Bool(false))
   val fdiv_busy = Wire(init=Bool(false))

   // The Functional Units --------------------
   val fu_units = ArrayBuffer[FunctionalUnit]()

   io.fu_types := FU_ALU |
                  Mux(Bool(has_fpu), FU_FPU, Bits(0)) |
                  Mux(Bool(has_mul && !use_slow_mul), FU_MUL, Bits(0)) |
                  (Mux(!muldiv_busy && Bool(has_mul && use_slow_mul), FU_MUL, Bits(0))) |
                  (Mux(!muldiv_busy && Bool(has_div), FU_DIV, Bits(0))) |
                  (Mux(Bool(shares_csr_wport), FU_CSR, Bits(0))) |
                  (Mux(Bool(is_branch_unit), FU_BRU, Bits(0))) |
                  Mux(!fdiv_busy && Bool(has_fdiv), FU_FDV, Bits(0) |
                  Mux(Bool(has_itov), FU_I2V, Bits(0)))


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
      alu.io.brinfo <> io.brinfo
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
      io.br_unit.brinfo.valid := Bool(false)
   }

   if (has_alu) fu_units += alu

   // Pipelined, IMul Unit ------------------
   var imul: PipelinedMulUnit = null
   if (has_mul && !use_slow_mul)
   {
      imul = Module(new PipelinedMulUnit(imulLatency))
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
   val fpu_resp_val = Wire(init=Bool(false))
   val fpu_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   fpu_resp_fflags.valid := Bool(false)
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
   val fdiv_resp_val = Wire(init=Bool(false))
   val fdiv_resp_uop = Wire(new MicroOp())
   val fdiv_resp_data = Wire(Bits(width=65))
   val fdiv_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   fdiv_resp_fflags.valid := Bool(false)
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
   val muldiv_resp_val = Wire(init = Bool(false))
   if (has_muldiv)
   {
      muldiv = Module(new MulDivUnit())
      muldiv.io.req.valid           := io.req.valid &&
                                       ((io.req.bits.uop.fu_code_is(FU_DIV) && Bool(has_div)) ||
                                        (io.req.bits.uop.fu_code_is(FU_MUL) && Bool(has_mul && use_slow_mul)))
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
                                      (io.req.bits.uop.fu_code_is(FU_MUL) && Bool(has_mul && use_slow_mul))))
      fu_units += muldiv
   }

   // Outputs (Write Port #0)  ---------------

   assert (io.resp(0).ready) // don'yet support back-pressuring this unit.

   io.resp(0).valid    := fu_units.map(_.io.resp.valid).reduce(_|_)
   io.resp(0).bits.uop := new MicroOp().fromBits(
                           PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.uop.asUInt))))
   io.resp(0).bits.data:= PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.data.asUInt))).asUInt
   // pulled out for critical path reasons
   if (has_alu) {
      io.resp(0).bits.uop.csr_addr := ImmGen(alu.io.resp.bits.uop.imm_packed, IS_I).asUInt
      io.resp(0).bits.uop.ctrl.csr_cmd := alu.io.resp.bits.uop.ctrl.csr_cmd
   }

   io.resp(0).bits.fflags := Mux(fpu_resp_val, fpu_resp_fflags, fdiv_resp_fflags)

   assert ((PopCount(fu_units.map(_.io.resp.valid)) <= UInt(1) && !muldiv_resp_val && !fdiv_resp_val) ||
          (PopCount(fu_units.map(_.io.resp.valid)) <= UInt(2) && (muldiv_resp_val || fdiv_resp_val)) ||
          (PopCount(fu_units.map(_.io.resp.valid)) <= UInt(3) && muldiv_resp_val && fdiv_resp_val)
      , "Multiple functional units are fighting over the write port.")

   if (has_itov) {
      io.ll_resp(0).bits.writesToIRF = false
      io.ll_resp(0).bits.writesToVIQ = true
      io.ll_resp(0).valid           := io.req.bits.uop.fu_code_is(FU_I2V) && io.req.valid
      io.ll_resp(0).bits.uop        := io.req.bits.uop
      when (io.req.bits.uop.lrs1_rtype === RT_FIX) {
         io.ll_resp(0).bits.uop.pdst := UInt(0)
         io.ll_resp(0).bits.data     := io.req.bits.rs1_data
      } .otherwise {
         io.ll_resp(0).bits.uop.pdst := UInt(1)
         io.ll_resp(0).bits.data     := io.req.bits.rs2_data
      }
   }
}

class VecFPUExeUnit(
   has_vfpu : Boolean = true,
   has_valu : Boolean = true
   )
   (implicit p: Parameters)
   extends ExecutionUnit(
      num_rf_read_ports = 3, // TODO_vec: add 4 for predication
      num_rf_write_ports = 1, // TODO_vec: Build mechanism for writes into IRF, this should get changed to 2 I think
      num_bypass_stages = 0,
      data_width = 128,
      bypassable = false,
      has_alu = false,
      has_valu = has_valu,
      has_vfpu = has_vfpu)(p) // TODO_vec: Add vmem, valu, vdiv, viu units
{
   val out_str = new StringBuilder
   out_str.append("\n     ExeUnit--")
   if (has_vfpu)  out_str.append("\n       - VECFPU (Latency: " + dfmaLatency + ")")
   if (has_valu)  out_str.append("\n       - VECALU")
   require (uses_iss_unit == true)
   // TODO_vec: Add div and toint stuff?

   val fu_units = ArrayBuffer[FunctionalUnit]()

   io.fu_types := Mux(Bool(has_vfpu), FU_VFPU, Bits(0)) |
                  Mux(Bool(has_valu), FU_VALU, Bits(0))// TODO_vec Add stuff for div, ving, etc
   io.resp(0).bits.writesToIRF = false
   //io.resp(1).bits.writesToIRF = true

   var vfpu: VFPUUnit = null
   val vfpu_resp_val = Wire(init=Bool(false))
   val vfpu_resp_fflags = Wire(new ValidIO(new FFlagsResp))
   vfpu_resp_fflags.valid := Bool(false)

   assert(has_vfpu, "The VecFPUExeUnit needs a vfpu");
   if (has_vfpu)
   {
      vfpu = Module(new VFPUUnit())

      vfpu.io.req.valid          := io.req.valid &&
                                    (io.req.bits.uop.fu_code_is(FU_VFPU))
      vfpu.io.req.bits.uop       := io.req.bits.uop
      vfpu.io.req.bits.rs1_data  := io.req.bits.rs1_data
      vfpu.io.req.bits.rs2_data  := io.req.bits.rs2_data
      vfpu.io.req.bits.rs3_data  := io.req.bits.rs3_data
      vfpu.io.req.bits.kill      := io.req.bits.kill
      vfpu.io.brinfo             <> io.brinfo

      vfpu_resp_val              := vfpu.io.resp.valid
      vfpu_resp_fflags           := vfpu.io.resp.bits.fflags
      fu_units += vfpu
   }
   var valu: VALUUnit = null
   val valu_resp_val = Wire(init=Bool(false))
   if (has_valu)
   {
      valu = Module(new VALUUnit(num_stages=p(tile.TileKey).core.fpu.get.dfmaLatency))
      valu.io.req.valid          := io.req.valid &&
                                    (io.req.bits.uop.fu_code_is(FU_VALU))
      valu.io.req.bits.kill      := io.req.bits.kill
      valu.io.req.bits.uop       := io.req.bits.uop
      valu.io.req.bits.rs1_data  := io.req.bits.rs1_data
      valu.io.req.bits.rs2_data  := io.req.bits.rs2_data
      valu.io.req.bits.rs3_data  := io.req.bits.rs3_data
      valu.io.brinfo             <> io.brinfo
      fu_units += valu
   }

   // Outputs
   val resp_uop = new MicroOp().fromBits(
      PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.uop.asUInt))))
   io.resp(0).valid         := fu_units.map(_.io.resp.valid).reduce(_|_)
   io.resp(0).bits.uop      := resp_uop
   io.resp(0).bits.data     := PriorityMux(fu_units.map(f =>(f.io.resp.valid, f.io.resp.bits.data.asUInt))).asUInt
   io.resp(0).bits.fflags   := vfpu_resp_fflags // TODO_vec add div flags here
   io.resp(0).bits.mask     := "b1111111111111111".U
   assert(!(valu.io.resp.valid && vfpu.io.resp.valid), "VALU and VFPU contending for write port")
   when (io.resp(0).valid) {
      printf("A functional unit in the vector exe unit has valid response\n");
      printf("%d %d %x\n", io.resp(0).bits.uop.uopc, io.resp(0).bits.uop.ldst, io.resp(0).bits.uop.inst)
   }
   override def toString: String = out_str.toString
}

// FPU-only unit, with optional second write-port for ToInt micro-ops.
class FPUExeUnit(
   has_fpu  : Boolean = true,
   has_fdiv : Boolean = false,
   has_fpiu : Boolean = false,
   has_fpvu : Boolean = false,
   usingVec : Boolean = false
   )
   (implicit p: Parameters)
   extends ExecutionUnit(
      num_rf_read_ports = 3,
      num_rf_write_ports = 1,
      num_ll_write_ports = if (usingVec) 2 else 1,  // +1 for FpToInt, +1 for FpToVec
      num_bypass_stages = 0,
      data_width = 65,
      bypassable = false,
      has_alu  = false,
      has_fpu  = has_fpu,
      has_fdiv = has_fdiv,
      has_fpiu = has_fpiu,
      has_fpvu = has_fpvu)(p)
{
   val out_str = new StringBuilder
   out_str.append("\n     ExeUnit--")
   if (has_fpu)  out_str.append("\n       - FPU (Latency: " + dfmaLatency + ")")
   if (has_fdiv) out_str.append("\n       - FDiv/FSqrt")
   if (has_fpiu) out_str.append("\n       - FPIU (writes to Integer RF)")
   if (has_fpvu) out_str.append("\n       - FPVU (writes to Vector issue slots)")

   val fdiv_busy = Wire(init=Bool(false))
   val fpiu_busy = Wire(init=Bool(false))
   val fpvu_busy = Wire(init=Bool(false))

   // The Functional Units --------------------
   val fu_units = ArrayBuffer[FunctionalUnit]()

   io.fu_types := Mux(Bool(has_fpu), FU_FPU, Bits(0)) |
                  Mux(!fdiv_busy && Bool(has_fdiv), FU_FDV, Bits(0)) |
                  Mux(!fpiu_busy && Bool(has_fpiu), FU_F2I, Bits(0)) |
                  Mux(!fpvu_busy && Bool(has_fpvu), FU_F2V, Bits(0))


   io.resp(0).bits.writesToIRF    = false
   io.ll_resp(0).bits.writesToIRF = true
   if (usingVec) {
      io.ll_resp(1).bits.writesToIRF = false
      io.ll_resp(1).bits.writesToVIQ = usingVec
   }
   // FPU Unit -----------------------
   var fpu: FPUUnit = null
   val fpu_resp_val = Wire(init=Bool(false))
   val fpu_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   fpu_resp_fflags.valid := Bool(false)
   if (has_fpu)
   {
      fpu = Module(new FPUUnit())
      fpu.io.req.valid           := io.req.valid &&
                                    (io.req.bits.uop.fu_code_is(FU_FPU) ||
                                     io.req.bits.uop.fu_code_is(FU_F2I)
                                    ) // TODO move to using a separate unit
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


   // FDiv/FSqrt Unit -----------------------
   var fdivsqrt: FDivSqrtUnit = null
   val fdiv_resp_val = Wire(init=Bool(false))
   val fdiv_resp_uop = Wire(new MicroOp())
   val fdiv_resp_data = Wire(Bits(width=65))
   val fdiv_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   fdiv_resp_fflags.valid := Bool(false)
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
                          !(fpu.io.resp.valid && (fpu.io.resp.bits.uop.fu_code_is(FU_F2I) || fpu.io.resp.bits.uop.fu_code_is(FU_F2V)))
   io.resp(0).bits.uop := new MicroOp().fromBits(
                           PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.uop.asUInt))))
   io.resp(0).bits.data:= PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.data.asUInt))).asUInt
   io.resp(0).bits.fflags := Mux(fpu_resp_val, fpu_resp_fflags, fdiv_resp_fflags)

   // Outputs (Write Port #1) -- FpToInt Queuing Unit -----------------------

   // TODO instantiate our own fpiu; and remove it from fpu.scala.

   // TODO_vec also instantiate our own fpvu, and remove it from fpu.scala

   // buffer up results since we share write-port on integer regfile.
   val queue = Module(new BranchKillableQueue(new ExeUnitResp(data_width), entries = dfmaLatency + 3)) // TODO being overly conservative
   queue.io.enq.valid       := fpu.io.resp.valid && fpu.io.resp.bits.uop.fu_code_is(FU_F2I)
   queue.io.enq.bits.uop    := fpu.io.resp.bits.uop
   queue.io.enq.bits.data   := fpu.io.resp.bits.data
   queue.io.enq.bits.fflags := fpu.io.resp.bits.fflags
   queue.io.brinfo          := io.brinfo
   queue.io.flush           := io.req.bits.kill
   io.ll_resp(0) <> queue.io.deq

   fpiu_busy := !(queue.io.empty)

   assert (queue.io.enq.ready) // If this backs up, we've miscalculated the size of the queue.

   if (usingVec) {
      io.ll_resp(1).valid         := io.req.valid && io.req.bits.uop.fu_code_is(FU_F2V)
      io.ll_resp(1).bits.uop      := io.req.bits.uop
      when (io.req.bits.uop.lrs1_rtype === RT_FLT) {
         io.ll_resp(1).bits.data     := io.req.bits.rs1_data
         io.ll_resp(1).bits.uop.pdst := UInt(0)
      } .elsewhen (io.req.bits.uop.lrs2_rtype === RT_FLT) {
         io.ll_resp(1).bits.data     := io.req.bits.rs2_data
         io.ll_resp(1).bits.uop.pdst := UInt(1)
      } .otherwise {
         io.ll_resp(1).bits.data     := io.req.bits.rs3_data
         io.ll_resp(1).bits.uop.pdst := UInt(2)
      }
   }

   override def toString: String = out_str.toString
}


class FDivSqrtExeUnit(implicit p: Parameters)
   extends ExecutionUnit(num_rf_read_ports = 2
                                       , num_rf_write_ports = 1
                                       , num_bypass_stages = 0
                                       , data_width = 65
                                       , has_fdiv = true
                                       )
{
   val fdiv_busy = Wire(Bool())
   io.fu_types := Mux(!fdiv_busy, FU_FDV, Bits(0))

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
   num_rf_write_ports = 0,
   num_ll_write_ports = 1,
   num_bypass_stages = 0,
   data_width = 65,
   // don't schedule uops from issue-window -- we're hard-hacking the datapath,
   // since the operand data comes from the IRF but writes back to the FRF.
   uses_iss_unit = false)
{
   val busy = Wire(init=Bool(false))
   io.fu_types := Mux(!busy, FU_I2F, Bits(0))
   io.ll_resp(0).bits.writesToIRF = false

   val ifpu = Module(new IntToFPUnit(latency=intToFpLatency))
   ifpu.io.req <> io.req
   ifpu.io.fcsr_rm := io.fcsr_rm
   ifpu.io.brinfo <> io.brinfo
   io.bypass <> ifpu.io.bypass

   // buffer up results since we share write-port on integer regfile.
   val queue = Module(new BranchKillableQueue(new ExeUnitResp(data_width), entries = intToFpLatency + 3)) // TODO being overly conservative
   queue.io.enq.valid       := ifpu.io.resp.valid
   queue.io.enq.bits.uop    := ifpu.io.resp.bits.uop
   queue.io.enq.bits.data   := ifpu.io.resp.bits.data
   queue.io.enq.bits.fflags := ifpu.io.resp.bits.fflags
   queue.io.brinfo := io.brinfo
   queue.io.flush := io.req.bits.kill

   io.ll_resp(0) <> queue.io.deq

   busy := !(queue.io.empty)


   assert (queue.io.enq.ready) // If this backs up, we've miscalculated the size of the queue.

   override def toString: String =
      "\n     ExeUnit--" +
      "\n       - IntToFP"
}


class MemExeUnit(implicit p: Parameters) extends ExecutionUnit(num_rf_read_ports = 2,
   num_rf_write_ports = 1,
   num_bypass_stages = 0,
   data_width = if (p(tile.TileKey).core.fpu.nonEmpty) 65 else p(tile.XLen),
   bypassable = false,
   is_mem_unit = true)(p)
   with freechips.rocketchip.rocket.constants.MemoryOpConstants
{

   // Perform address calculation
   val maddrcalc = Module(new MemAddrCalcUnit())
   maddrcalc.io.req <> io.req
   maddrcalc.io.vl  := io.vl
   assert(!(io.req.valid && io.req.bits.uop.vec_val && io.req.bits.uop.rate =/= UInt(1)),
      "Vector loads and stores through this unit proceed at rate 1")


   maddrcalc.io.brinfo <> io.brinfo
   io.bypass <> maddrcalc.io.bypass  // TODO this is not where the bypassing should occur from, is there any bypassing happening?!


   val to_lsu_vsta = Module(new BranchKillableQueue(new FuncUnitResp(128), 4))
   val to_lsu_resp = Module(new BranchKillableQueue(new FuncUnitResp(128), 8))

   assert(!(maddrcalc.io.resp.valid
      && !(maddrcalc.io.resp.bits.uop.vec_val && maddrcalc.io.resp.bits.uop.is_store)
      && !to_lsu_resp.io.enq.ready),
      "We do not support backpressure on this queue")

   to_lsu_resp.io.enq.valid := (maddrcalc.io.resp.valid
                             && !(maddrcalc.io.resp.bits.uop.vec_val && maddrcalc.io.resp.bits.uop.is_store))
   to_lsu_resp.io.enq.bits  := maddrcalc.io.resp.bits
   to_lsu_resp.io.brinfo    := io.brinfo
   to_lsu_resp.io.flush     := io.req.bits.kill

   io.lsu_io.exe_resp       <> to_lsu_resp.io.deq

   // Vector store addrs need to go through a separate queue to avoid blocking loads
   assert(!(maddrcalc.io.resp.valid && maddrcalc.io.resp.bits.uop.vec_val
      && maddrcalc.io.resp.bits.uop.is_store && !to_lsu_vsta.io.enq.ready),
      "We do not support backpressure on this queue")

   if (usingVec) {
      to_lsu_vsta.io.enq.valid := (maddrcalc.io.resp.valid
                                && maddrcalc.io.resp.bits.uop.vec_val
                                && maddrcalc.io.resp.bits.uop.is_store
                                && !IsKilledByBranch(io.brinfo, maddrcalc.io.resp.bits.uop))
      to_lsu_vsta.io.enq.bits  := maddrcalc.io.resp.bits
      to_lsu_vsta.io.brinfo    := io.brinfo
      to_lsu_vsta.io.flush     := io.req.bits.kill
      io.lsu_io.exe_vsta       <> to_lsu_vsta.io.deq
   } else {
      to_lsu_vsta.io.enq.valid := false.B
   }


   io.fu_types := Mux(to_lsu_resp.io.count < UInt(1), FU_MEM, UInt(0)) | Mux(to_lsu_vsta.io.count < UInt(1), FU_VSTA, UInt(0))
   // TODO_Vec: Tune queue suze and limit here


   // TODO get rid of com_exception and guard with an assert? Need to surpress within dc-shim.
//   assert (!(io.com_exception && lsu.io.memreq_uop.is_load && lsu.io.memreq_val),
//      "[execute] a valid load is returning while an exception is being thrown.")
   io.dmem.req.valid      := Mux(io.com_exception && io.lsu_io.memreq_uop.is_load,
                              Bool(false),
                              io.lsu_io.memreq_val)
   io.dmem.req.bits.addr  := io.lsu_io.memreq_addr
   io.dmem.req.bits.data  := io.lsu_io.memreq_wdata
   io.dmem.req.bits.uop   := io.lsu_io.memreq_uop
   io.dmem.req.bits.kill  := io.lsu_io.memreq_kill // load kill request sent to memory

   // I should be timing forwarding to coincide with dmem resps, so I'm not clobbering
   //anything....
   val memresp_val    = Mux(io.com_exception && io.dmem.resp.bits.uop.is_load, Bool(false),
                                                io.lsu_io.forward_val || io.dmem.resp.valid)
   val memresp_rf_wen = (io.dmem.resp.valid && (io.dmem.resp.bits.uop.mem_cmd === M_XRD || io.dmem.resp.bits.uop.is_amo)) ||  // TODO should I refactor this to use is_load?
                           io.lsu_io.forward_val
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
   io.resp(0).bits.mask             := RegNext(MuxLookup(memresp_uop.rd_vew, VEW_8, Array(
      VEW_8  -> "b1".U,
      VEW_16 -> "b11".U,
      VEW_32 -> "b1111".U,
      VEW_64 -> "b11111111".U)))
   override def toString: String =
      "\n     ExeUnit--" +
      "\n       - Mem"
}

