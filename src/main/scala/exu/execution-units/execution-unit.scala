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

import FUConstants._
import boom.common._
import boom.ifu.{GetPCFromFtqIO}
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
}

class MemExeUnit(
  val hasAGen          : Boolean       = false,
  val hasDGen          : Boolean       = false
)(implicit p: Parameters) extends ExecutionUnit("Mem") {
  val io_req = IO(Input(Valid(new FuncUnitReq(xLen))))

  val io_agen = if (hasAGen) {
    val loads_saturating = io_req.valid && io_req.bits.uop.uses_ldq && io_req.bits.uop.fu_code_is(FU_AGEN)
    val saturating_loads_counter = RegInit(0.U(5.W))
    when (loads_saturating) { saturating_loads_counter := saturating_loads_counter + 1.U }
      .otherwise { saturating_loads_counter := 0.U }
    val pause_mem = RegNext(loads_saturating) && saturating_loads_counter === ~(0.U(5.W))
    val load_ready = !pause_mem
    fu_types += ((FU_AGEN, load_ready, "AGen"))

    val sum = (io_req.bits.rs1_data.asSInt + io_req.bits.imm_data.asSInt).asUInt
    val ea_sign = Mux(sum(vaddrBits-1), ~sum(63,vaddrBits) === 0.U,
                                         sum(63,vaddrBits) =/= 0.U)
    val effective_address = Cat(ea_sign, sum(vaddrBits-1,0)).asUInt

    val agen = IO(Output(Valid(new MemGen)))
    agen.valid     := io_req.valid && io_req.bits.uop.fu_code_is(FU_AGEN)
    agen.bits.uop  := io_req.bits.uop
    agen.bits.data := Sext(effective_address, xLen)
    Some(agen)
  } else {
    assert(!(io_req.valid && io_req.bits.uop.fu_code_is(FU_AGEN)))
    None
  }

  val io_dgen = if (hasDGen) {
    fu_types += ((FU_DGEN, true.B, "DGen"))
    val dgen = IO(Output(Valid(new MemGen)))
    dgen.valid     := io_req.valid && io_req.bits.uop.fu_code_is(FU_DGEN)
    dgen.bits.data := io_req.bits.rs1_data
    dgen.bits.uop  := io_req.bits.uop
    Some(dgen)
  } else {
    assert(!(io_req.valid && io_req.bits.uop.fu_code_is(FU_DGEN)))
    None
  }

  io_ready_fu_types := fu_types.map { case (code, ready, _) => Mux(ready, code, 0.U(FUC_SZ.W)) }.reduce(_|_)


}
class IntExeUnit(
  val hasCSR           : Boolean       = false,
  val hasJmp           : Boolean       = false,
  val hasAlu           : Boolean       = false,
  val hasMul           : Boolean       = false,
  val hasDiv           : Boolean       = false,
  val hasIfpu          : Boolean       = false,
  val hasRocc          : Boolean       = false
)(implicit p: Parameters) extends ExecutionUnit("Int") {
  val alwaysBypassable = hasAlu && !hasMul

  val io_req = IO(Input(Valid(new FuncUnitReq(xLen))))


  val (io_alu_resp, io_brinfo, io_get_ftq_pc, io_csr, io_sfence) = if (hasAlu) {
    val alu_ready = WireInit(true.B)
    fu_types += ((FU_ALU, alu_ready, "ALU"))

    val alu = Module(new ALUUnit(isJmpUnit = hasJmp,
                                 dataWidth = xLen))
    val req_valid = (io_req.bits.uop.fu_code_is(FU_ALU) ||
      (if (hasJmp) io_req.bits.uop.fu_code_is(FU_JMP) else false.B) ||
      (if (hasCSR) io_req.bits.uop.fu_code_is(FU_CSR) else false.B)
    )
    alu.io.req.valid  := io_req.valid && req_valid && !io_req.bits.uop.is_rocc
    alu.io.req.bits   := io_req.bits
    alu.io.resp.ready := true.B
    alu.io.brupdate   := io_brupdate
    alu.io.kill       := io_kill

    val alu_resp = IO(Output(Valid(new ExeUnitResp(xLen))))
    alu_resp.valid := alu.io.resp.valid && !alu.io.resp.bits.uop.fu_code_is(FU_CSR)
    alu_resp.bits  := alu.io.resp.bits

    val brinfo = IO(Output(Valid(new BrResolutionInfo)))
    brinfo := alu.io.brinfo

    val get_ftq_pc = if (hasJmp) {
      fu_types += ((FU_JMP, alu_ready, "Jmp"))
      val g = IO(Flipped(new GetPCFromFtqIO))
      alu.io.get_ftq_pc <> g
      Some(g)
    } else {
      None
    }

    if (hasMul) {
      fu_types += ((FU_MUL, true.B, "IMul"))

      val imul = Module(new PipelinedMulUnit(imulLatency, xLen))
      require(imulLatency > 2)
      imul.io.req.valid  := io_req.valid && io_req.bits.uop.fu_code_is(FU_MUL)
      imul.io.req.bits   := io_req.bits
      imul.io.brupdate   := io_brupdate
      imul.io.kill       := io_kill
      imul.io.resp.ready := true.B

      // If the mul unit is going to write, block issue of a ALU op so the mul can take the write port
      val imul_block_alu = ShiftRegister(imul.io.req.valid, imulLatency-2)
      when (imul_block_alu) {
        alu_ready := false.B
      }

      when (imul.io.resp.valid) {
        alu_resp := imul.io.resp
        assert(!(alu.io.resp.valid && !alu.io.resp.bits.uop.fu_code_is(FU_CSR)))
      }
    } else {
      assert(!(io_req.valid && io_req.bits.uop.fu_code_is(FU_MUL)))
    }

    val (csr, sfence) = if (hasCSR) {
      fu_types += ((FU_CSR, true.B, "CSR"))
      val c = IO(Output(Valid(new CSRResp)))
      c.valid     := RegNext(alu.io.resp.valid && io_req.bits.uop.csr_cmd =/= CSR.N)
      c.bits.uop  := RegNext(alu.io.resp.bits.uop)
      c.bits.data := RegNext(alu.io.resp.bits.data)
      c.bits.addr := RegNext(io_req.bits.imm_data)

      val s = IO(Valid(new SFenceReq))
      s.valid    := RegNext(io_req.valid && io_req.bits.uop.is_sfence)
      s.bits.rs1 := RegNext(io_req.bits.uop.mem_size(0))
      s.bits.rs2 := RegNext(io_req.bits.uop.mem_size(1))
      s.bits.addr := RegNext(io_req.bits.rs1_data)
      s.bits.asid := RegNext(io_req.bits.rs2_data)
      (Some(c), Some(s))
    } else {
      assert(!(io_req.valid && io_req.bits.uop.fu_code_is(FU_CSR)))
      assert(!(io_req.valid && io_req.bits.uop.is_sfence))
      (None, None)
    }
    (Some(alu_resp), Some(brinfo), get_ftq_pc, csr, sfence)
  } else {
    assert(!(io_req.valid && io_req.bits.uop.fu_code_is(FU_ALU)))
    require(!hasJmp && !hasCSR)
    (None, None, None, None, None)
  }

  val (io_rocc_resp, io_rocc_core) = if (hasRocc) {
    val rocc_core = IO(new RoCCShimCoreIO)
    val rocc_resp = IO(Output(Valid(new ExeUnitResp(xLen))))
    val rocc = Module(new RoCCShim)
    rocc.io.req.valid         := io_req.valid && io_req.bits.uop.is_rocc
    rocc.io.req.bits          := io_req.bits
    rocc.io.brupdate          := io_brupdate // We should assert on this somewhere
    rocc.io.status            := io_status
    rocc.io.exception         := io_kill
    rocc_core                 <> rocc.io.core

    rocc_resp <> rocc.io.resp
    (Some(rocc_resp), Some(rocc_core))
  } else {
    assert(!(io_req.valid && io_req.bits.uop.is_rocc))
    (None, None)
  }

  val (io_ifpu_resp) = if (hasIfpu) {
    val ifpu_ready = Wire(Bool())
    fu_types += ((FU_I2F, ifpu_ready, "IFPU"))

    val ifpu = Module(new IntToFPUnit(latency=intToFpLatency))
    ifpu.io.req.valid  := io_req.valid && io_req.bits.uop.fu_code_is(FU_I2F)
    ifpu.io.req.bits   := io_req.bits
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
    assert(!(io_req.valid && io_req.bits.uop.fu_code_is(FU_I2F)))
    (None)
  }

  val (io_div_resp) = if (hasDiv) {
    val div_ready = Wire(Bool())
    fu_types += ((FU_DIV, div_ready, "IDiv"))

    val divq = Module(new BranchKillableQueue(new FuncUnitReq(xLen), 3))
    div_ready := divq.io.empty
    divq.io.enq.valid := (io_req.valid && io_req.bits.uop.fu_code_is(FU_DIV))
    divq.io.enq.bits  := io_req.bits
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
    assert(!(io_req.valid && io_req.bits.uop.fu_code_is(FU_DIV)))
    (None)
  }

  io_ready_fu_types := fu_types.map { case (code, ready, _) => Mux(ready, code, 0.U(FUC_SZ.W)) }.reduce(_|_)

}

class FPExeUnit(val hasFDiv: Boolean = false, val hasFpiu: Boolean = false)(implicit p: Parameters)
    extends ExecutionUnit("FP") with tile.HasFPUParameters
{
  val io_req     = IO(Input(Valid(new FuncUnitReq(xLen+1))))
  val fpu = Module(new FPUUnit)
  fu_types += ((FU_FPU, true.B, "FPU"))
  fpu.io.req.valid := io_req.valid && (
    io_req.bits.uop.fu_code_is(FU_FPU) || (if (hasFpiu) io_req.bits.uop.fu_code_is(FU_F2I) else false.B)
  )
  fpu.io.req.bits := io_req.bits
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
    divq.io.enq.valid := (io_req.valid && io_req.bits.uop.fu_code_is(FU_FDV))
    divq.io.enq.bits  := io_req.bits
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
    assert(!(io_req.valid && io_req.bits.uop.fu_code_is(FU_FDV)))
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
    dgen.valid     := RegNext(io_req.valid && io_req.bits.uop.uses_stq && !IsKilledByBranch(io_brupdate, io_req))
    dgen.bits.uop  := RegNext(io_req.bits.uop)
    dgen.bits.data := RegNext(ieee(io_req.bits.rs2_data))
    (Some(fpiu_resp), Some(dgen))
  } else {
    assert(!(io_req.valid && io_req.bits.uop.fu_code_is(FU_F2I)))
    (None, None)
  }

  io_ready_fu_types := fu_types.map { case (code, ready, _) => Mux(ready, code, 0.U(FUC_SZ.W)) }.reduce(_|_)
}

