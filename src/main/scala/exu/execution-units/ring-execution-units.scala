//******************************************************************************
// Copyright (c) 2015 - 2020, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Execution Unit Generator + Container Module for the Ring Microarchitecture
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import scala.collection.mutable.{ArrayBuffer}

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.rocket.{BP}
import freechips.rocketchip.tile

import boom.common._
import boom.util._
import boom.ifu.{GetPCFromFtqIO}

import FUConstants._

class RingExecutionUnits(implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle {
    //-------------------------
    // Common I/O
    val exe_reqs   = Vec(coreWidth, Flipped(DecoupledIO(new FuncUnitReq(xLen))))
    val exe_resps  = Output(Vec(coreWidth, Valid(new ExeUnitResp(xLen))))
    val ll_resps   = Output(Vec(coreWidth, Valid(new ExeUnitResp(xLen))))
    val ll_wakeups = Output(Vec( memWidth, Valid(UInt(ipregSz.W))))

    val brupdate   = Input(new BrUpdateInfo)
    val kill       = Input(Bool())

    val fu_avail   = Output(UInt(FUC_SZ.W))

    //-------------------------
    // Misc I/O

    // ALU branch resolution info
    val brinfos = Output(Vec(coreWidth, new BrResolutionInfo))

    // only used by the mem unit
    val lsu_io = Vec(memWidth, Flipped(new boom.lsu.LSUExeIO))
    val bp     = Input(Vec(nBreakpoints, new BP))

    // only used by the branch unit
    val jmp_brinfo = Output(new BrResolutionInfo)
    val get_ftq_pc = Flipped(new GetPCFromFtqIO)
    val status     = Input(new freechips.rocketchip.rocket.MStatus)

    // special jmp response port for predicate writeback
    val jmp_unit_resp = Output(Valid(new ExeUnitResp(xLen)))

    // only used by the CSR unit
    val csr_unit_resp = DecoupledIO(new FuncUnitResp(xLen))

    // only used by the rocc unit
    val rocc = if (usingRoCC) new RoCCShimCoreIO else null

    // only used by the fpu unit
    val fcsr_rm = if (usingFPU) Input(Bits(tile.FPConstants.RM_SZ.W)) else null

    // TODO move this out of ExecutionUnit
    val com_exception = Input(Bool())

    // fpu -> int writeback
    val from_fpu = Flipped(DecoupledIO(new ExeUnitResp(xLen+1)))

    // int -> fpu writeback
    val to_fpu = DecoupledIO(new ExeUnitResp(xLen+1))

    // load -> fpu writeback
    val ll_fresps = Vec(memWidth, DecoupledIO(new ExeUnitResp(xLen)))
  })

  //----------------------------------------------------------------------------------------------------
  // Instantiate the ExecutionUnits

  private val column_exe_units = ArrayBuffer[ExecutionUnit]()
  private val memory_exe_units = ArrayBuffer[ExecutionUnit]()
  private val unique_exe_units = ArrayBuffer[ExecutionUnit]()

  def shared_exe_units = memory_exe_units ++ unique_exe_units
  def exe_units        = column_exe_units ++ shared_exe_units

  //----------------------------------------------------------------------------------------------------
  // Getters

  def length = exe_units.length

  def apply(n: Int) = exe_units(n)

  def map[T](f: ExecutionUnit => T) = {
    exe_units.map(f)
  }

  def withFilter(f: ExecutionUnit => Boolean) = {
    exe_units.withFilter(f)
  }

  def foreach[U](f: ExecutionUnit => U) = {
    exe_units.foreach(f)
  }

  def zipWithIndex = {
    exe_units.zipWithIndex
  }

  def indexWhere(f: ExecutionUnit => Boolean) = {
    exe_units.indexWhere(f)
  }

  def count(f: ExecutionUnit => Boolean) = {
    exe_units.count(f)
  }

  def mem_units = {
    exe_units.filter(_.hasMem)
  }

  def csr_unit = {
    require (exe_units.count(_.hasCSR) == 1)
    exe_units.find(_.hasCSR).get
  }

  def ifpu_unit = {
    require (usingFPU)
    require (exe_units.count(_.hasIfpu) == 1)
    exe_units.find(_.hasIfpu).get
  }

  def fpiu_unit = {
    require (usingFPU)
    require (exe_units.count(_.hasFpiu) == 1)
    exe_units.find(_.hasFpiu).get
  }

  def rocc_unit = {
    require (usingRoCC)
    require (exe_units.count(_.hasRocc) == 1)
    exe_units.find(_.hasRocc).get
  }

  //----------------------------------------------------------------------------------------------------
  // Generate the units

  // Generate column ALUs
  for (w <- 0 until coreWidth) {
    val alu = Module(new ALUExeUnit)
    alu.suggestName("alu_" + w)
    column_exe_units += alu
  }

  // Generate memory access units
  for (w <- 0 until memWidth) {
    val mem_unit = Module(new ALUExeUnit(
      hasAlu = false,
      hasMem = true))
    mem_unit.suggestName("mem_unit_" + w)
    mem_unit.io.ll_iresp.ready := true.B
    memory_exe_units += mem_unit
  }

  // Jump unit
  val jmp_unit = Module(new ALUExeUnit(hasJmp = true))
  jmp_unit.suggestName("jmp_unit")
  unique_exe_units += jmp_unit

  // Put remaining (infrequently used) functional units in a single execution unit
  val misc_unit = Module(new ALUExeUnit(hasMul  = true,
                                        hasDiv  = true,
                                        hasCSR  = true,
                                        hasIfpu = usingFPU))
  misc_unit.suggestName("misc_unit")
  unique_exe_units += misc_unit

  // Hookup I/O common to all units
  for (eu <- exe_units) {
    eu.io.brupdate := io.brupdate
    eu.io.kill     := io.kill

    if (eu.writesIrf) eu.io.iresp.ready := true.B
  }

  //----------------------------------------------------------------------------------------------------
  // Generator string output

  val exeUnitsStr = new StringBuilder
  for (exe_unit <- exe_units) {
    exeUnitsStr.append(exe_unit.toString)
  }

  override def toString: String =
    (BoomCoreStringPrefix("===ExecutionUnits===") + "\n"
    + (BoomCoreStringPrefix(
         "==" + coreWidth + "-wide Machine==",
         "==" + coreWidth + " Issue==")
    ) + "\n"
    + exeUnitsStr.toString)

  //----------------------------------------------------------------------------------------------------
  // Pipeline Registers + Bypassing Logic

  val exe_valids = Wire(Vec(coreWidth, Bool()))
  val exe_uops   = Wire(Vec(coreWidth, new MicroOp))
  val prs1_data  = Wire(Vec(coreWidth, Bits(xLen.W)))
  val prs2_data  = Wire(Vec(coreWidth, Bits(xLen.W)))
  val ppred_data = Wire(Vec(coreWidth, Bool()))
  val exe_reqs   = Wire(Vec(coreWidth, Valid(new FuncUnitReq(xLen))))

  // uop registers
  for (w <- 0 until coreWidth) {
    val kill = io.kill || IsKilledByBranch(io.brupdate, io.exe_reqs(w).bits.uop)
    exe_valids(w) := RegNext(io.exe_reqs(w).valid && !kill)
    exe_uops(w)   := RegNext(GetNewUopAndBrMask(io.exe_reqs(w).bits.uop, io.brupdate))
  }

  // Operand data registers and bypassing
  for (w <- 0 until coreWidth) {
    val req = io.exe_reqs(w).bits
    val k = (coreWidth + w - 1) % coreWidth

    prs1_data(w)  := RegNext(Mux(req.uop.prs1_bypass_alu, column_exe_units(k).io.bypass(0).bits.data,
                             Mux(req.uop.prs1_bypass_mem.reduce(_||_),
                           Mux1H(req.uop.prs1_bypass_mem, mem_units.map(_.io.ll_iresp.bits.data)),
                             Mux(req.uop.prs1_bypass_gen, io.exe_resps(k).bits.data,
                                                          req.rs1_data))))
    prs2_data(w)  := RegNext(Mux(req.uop.prs2_bypass_alu, column_exe_units(k).io.bypass(0).bits.data,
                             Mux(req.uop.prs2_bypass_mem.reduce(_||_),
                           Mux1H(req.uop.prs2_bypass_mem, mem_units.map(_.io.ll_iresp.bits.data)),
                             Mux(req.uop.prs2_bypass_gen, io.exe_resps(k).bits.data,
                                                          req.rs2_data))))
    ppred_data(w) := RegNext(Mux(req.uop.ppred === jmp_unit.io.bypass(0).bits.uop.pdst && jmp_unit.io.bypass(0).valid, jmp_unit.io.bypass(0).bits.data(0),
                             Mux(req.uop.ppred === jmp_unit.io.iresp.bits.uop.pdst && jmp_unit.io.iresp.valid, jmp_unit.io.iresp.bits.data(0)    ,
                                           req.pred_data)))
  }

  // Setup requests
  for (w <- 0 until coreWidth) {
    exe_reqs(w)                := DontCare
    exe_reqs(w).valid          := exe_valids(w)
    exe_reqs(w).bits.uop       := exe_uops(w)
    exe_reqs(w).bits.rs1_data  := prs1_data(w)
    exe_reqs(w).bits.rs2_data  := prs2_data(w)
    exe_reqs(w).bits.pred_data := ppred_data(w)
  }

  //----------------------------------------------------------------------------------------------------
  // Req -> EU crossbar

  // Hookup column ALUs
  for (w <- 0 until coreWidth) {
    column_exe_units(w).io.req.bits  := exe_reqs(w).bits
    column_exe_units(w).io.req.valid := exe_reqs(w).valid && exe_reqs(w).bits.uop.eu_code(0)
  }

  // Hookup memory units (any number suppported)
  val mem_reqs = VecInit(exe_reqs.map(req => req.bits.uop.eu_code(1) && req.valid))
  val mem_sels = mem_reqs.scanLeft(1.U(memWidth.W)) ((s,r) => Mux(r, s << 1, s)(memWidth-1,0)).dropRight(1)
  val mem_gnts = Transpose(mem_sels).map(s => s & mem_reqs.asUInt)
  for ((mem,gnt) <- mem_units zip mem_gnts) {
    mem.io.req.bits  := Mux1H(gnt, exe_reqs.map(_.bits))
    mem.io.req.valid := gnt.orR
  }
  assert (PopCount(mem_reqs) <= memWidth.U, "[exe] too many requests to the memory units")

  // Hookup remaining shared units (FUs in this set should be unique)
  val unq_gnts = Transpose(VecInit(exe_reqs.map(req => req.bits.uop.eu_code(3,2) & Fill(2, req.valid))))
  for ((eu,gnt) <- unique_exe_units zip unq_gnts) {
    eu.io.req.bits  := Mux1H(gnt, exe_reqs.map(_.bits))
    eu.io.req.valid := gnt.orR

    assert (PopCount(gnt) <= 1.U, "[exe] multiple grants to a unique execution unit")
  }

  //----------------------------------------------------------------------------------------------------
  // EU -> Fast Resp crossbar

  val fast_eu_sels = Transpose(VecInit(Seq(VecInit(column_exe_units.map(_.io.iresp.valid)).asUInt) ++
    shared_exe_units.filter(_.writesIrf).map(eu => eu.io.iresp.bits.uop.column & Fill(coreWidth, eu.io.iresp.valid))))

  for (w <- 0 until coreWidth) {
    io.exe_resps(w).bits  := Mux1H(fast_eu_sels(w),
                                   Seq(column_exe_units(w).io.iresp.bits) ++
                                   shared_exe_units.filter(_.writesIrf).map(_.io.iresp.bits))
    io.exe_resps(w).valid := fast_eu_sels(w).orR

    assert (PopCount(fast_eu_sels(w)) <= 1.U, "[exe] writeback crossbar collision on port " + w)
  }

  //----------------------------------------------------------------------------------------------------
  // EU -> Slow (LL) Resp crossbar

  val fpiu_wb_req = if (usingFPU) Seq(io.from_fpu.bits.uop.column & Fill(coreWidth, io.from_fpu.valid)) else Seq()
  val fpiu_resp   = if (usingFPU) Seq(io.from_fpu.bits) else Seq()

  val slow_eu_reqs = Transpose(VecInit(shared_exe_units.filter(_.writesLlIrf).map(eu =>
    eu.io.ll_iresp.bits.uop.column & Fill(coreWidth, eu.io.ll_iresp.valid)) ++ fpiu_wb_req))
  val slow_eu_rdys = Transpose(VecInit(slow_eu_reqs.map(r => ~MaskAbove(r))))

  for (w <- 0 until coreWidth) {
    io.ll_resps(w).bits  := PriorityMux(slow_eu_reqs(w),
                                        shared_exe_units.filter(_.writesLlIrf).map(_.io.ll_iresp.bits) ++ fpiu_resp)
    io.ll_resps(w).valid := slow_eu_reqs(w).orR
  }

  for ((eu,rdy) <- shared_exe_units.filter(_.writesLlIrf) zip slow_eu_rdys) {
    eu.io.ll_iresp.ready := (rdy & eu.io.ll_iresp.bits.uop.column).orR
  }

  if (usingFPU) {
    io.from_fpu.ready := (slow_eu_rdys.last & io.from_fpu.bits.uop.column).orR
  } else {
    io.from_fpu.ready := DontCare
  }

  // Hookup the long latency wakeup ports for loads
  for (w <- 0 until memWidth) {
    io.ll_wakeups(w).bits  := mem_units(w).io.ll_iresp.bits.uop.pdst
    io.ll_wakeups(w).valid := mem_units(w).io.ll_iresp.valid
  }

  //----------------------------------------------------------------------------------------------------
  // Punch through misc I/O to core

  io.fu_avail := exe_units.foldLeft(0.U(FUC_SZ.W))((fu,eu) => fu | eu.io.fu_types)

  // Brinfo
  for (w <- 0 until coreWidth) {
    io.brinfos(w) := column_exe_units(w).io.brinfo
  }

  // Memory access units
  for ((mem_unit, w) <- mem_units.zipWithIndex) {
    mem_unit.io.lsu_io <> io.lsu_io(w)
    mem_unit.io.bp     := io.bp
    mem_unit.io.status := io.status
    mem_unit.io.com_exception := io.com_exception
  }

  // Jump unit
  io.jmp_brinfo := jmp_unit.io.brinfo
  jmp_unit.io.get_ftq_pc <> io.get_ftq_pc
  io.jmp_unit_resp.valid := jmp_unit.io.iresp.valid
  io.jmp_unit_resp.bits  := jmp_unit.io.iresp.bits

  // CSR unit
  io.csr_unit_resp <> csr_unit.io.iresp

  // FPU related
  if (usingFPU) {
    for (unit <- exe_units.filter(_.hasFcsr)) {
      unit.io.fcsr_rm := io.fcsr_rm
    }

    io.ll_fresps <> mem_units.map(_.io.ll_fresp)

    io.to_fpu <> ifpu_unit.io.ll_fresp
  } else {
     for (ll_fresp <- io.ll_fresps) {
      ll_fresp.bits  := DontCare
      ll_fresp.valid := DontCare
     }

     io.to_fpu.bits  := DontCare
     io.to_fpu.valid := DontCare
  }

  // RoCC unit
  if (usingRoCC) {
    rocc_unit.io.rocc <> io.rocc
    rocc_unit.io.com_exception := io.com_exception
  }
}
