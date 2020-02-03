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

import boom.common._
import boom.util._

class RingExecutionUnits(implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle {
    // I/O used by all units
    val exe_reqs  = Vec(coreWidth, Flipped(DecoupledIO(new FuncUnitReq(xLen))))
    val exe_resps = Output(Vec(coreWidth, Valid(new ExeUnitResp(xLen))))

    val brinfo    = Input(new BrResolutionInfo)
    val kill      = Input(Bool())

    // TODO get rid of this output
    val bypass = Output(new BypassData(coreWidth, dataWidth))

    // only used by the rocc unit
    val rocc = if (usingRoCC) new RoCCShimCoreIO else null

    // only used by the branch unit
    val br_unit    = Output(new BranchUnitResp)
    val get_ftq_pc = Flipped(new GetPCFromFtqIO)
    val status     = Input(new freechips.rocketchip.rocket.MStatus)

    // only used by the fpu unit
    val fcsr_rm = if (usingFPU) Input(Bits(tile.FPConstants.RM_SZ.W)) else null

    // only used by the mem unit
    val lsu_io = Vec(memWidth, Flipped(new boom.lsu.LSUExeIO))
    val bp     = Input(Vec(nBreakpoints, new BP))

    // TODO move this out of ExecutionUnit
    val com_exception = Input(Bool())
  })

  //----------------------------------------------------------------------------------------------------
  // Instantiate the ExecutionUnits

  private val column_exe_units = ArrayBuffer[ExecutionUnit]()
  private val shared_exe_units = ArrayBuffer[ExecutionUnit]()

  def exe_units = column_exe_units ++ shared_exe_units

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

  def br_unit = {
    require (exe_units.count(_.hasBrUnit) == 1)
    exe_units.find(_.hasBrUnit).get
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

  def br_unit_io = {
    require (exe_units.count(_.hasBrUnit) == 1)
    (exe_units.find(_.hasBrUnit).get).io.br_unit
  }

  def br_unit_idx = {
    exe_units.indexWhere(_.hasBrUnit)
  }

  def rocc_unit = {
    require (usingRoCC)
    require (exe_units.count(_.hasRocc) == 1)
    exe_units.find(_.hasRocc).get
  }

  def idiv_busy = {
    !exe_units.find(_.hasDiv).get.io.fu_types(4)
  }

  //----------------------------------------------------------------------------------------------------
  // Generate the units

  // Generate column ALUs
  for (w <- 0 until coreWidth) {
    column_exe_units += Module(new ALUExeUnit)
  }

  // Generate memory access units. Only 1 supported for now
  for (w <- 0 until memWidth) {
    val memExeUnit = Module(new ALUExeUnit(
      hasAlu = false,
      hasMem = true))

    memExeUnit.io.ll_iresp.ready := DontCare

    shared_exe_units += memExeUnit
  }

  // Branch unit
  shared_exe_units += Module(new ALUExeUnit(hasAlu = false, hasBrUnit = true))

  // Put remaining functional units in a shared execution unit
  shared_exe_units += Module(new ALUExeUnit(hasAlu  = false,
                                            hasMul  = true,
                                            hasDiv  = true,
                                            hasCSR  = true))

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
  // Req -> EU crossbar

  val xbarSize = shared_exe_units.length + 1
  val col_sels = Transpose(VecInit(io.exe_reqs.map(req => req.bits.uop.eu_code & Fill(xbarSize, req.valid))))

  // Hookup column units
  for (w <- 0 until coreWidth) {
    column_exe_units(w).io.req.bits  := io.exe_reqs(w).bits
    column_exe_units(w).io.req.valid := col_sels(0)(w)

    column_exe_units(w).io.brinfo    := io.brinfo
    column_exe_units(w).io.kill      := io.kill
  }

  // Hookup shared units
  for ((i,eu) <- (1 until xbarSize) zip shared_exe_units) {
    eu.io.req.bits  := Mux1H(col_sels(i), io.exe_reqs.map(_.bits))
    eu.io.req.valid := col_sels(i).orR

    eu.io.brinfo    := io.brinfo
    eu.io.kill      := io.kill
  }

  //----------------------------------------------------------------------------------------------------
  // EU -> Resp crossbar
  // TODO: This doesn't prevent collisions between scheduled operations (ALU etc.) and
  // long-latency operations (div & load miss). Possible to fix by backpressuring div unit and
  // refill buffers respectively, but a bit messy. The refill buffers are especially bad because
  // there's a pipe stage between them and the writeback crossbar.

  val eu_sels = Transpose(VecInit(Seq(VecInit(column_exe_units.map(_.io.iresp.valid)).asUInt) ++
    shared_exe_units.filter(_.writesIrf).map(eu => eu.io.iresp.bits.uop.dst_col & Fill(coreWidth, eu.io.iresp.valid))))

  for (w <- 0 until coreWidth) {
    io.exe_resps(w).bits  := Mux1H(eu_sels(w), Seq(column_exe_units(w).io.iresp.bits) ++ shared_exe_units.filter(_.writesIrf).map(_.io.iresp.bits))
    io.exe_resps(w).valid := eu_sels(w).orR
  }

  //----------------------------------------------------------------------------------------------------
  // Punch through misc unit I/O to core

  // ALU bypasses
  for (w <- 0 until coreWidth) {
    io.bypass((w + 1) % coreWidth) := column_units(w).io.bypass(0)
  }

  // Branch unit
  io.br_unit := br_unit_io
  io.get_ftq_pc <> br_unit.io.get_ftq_pc
  br_unit.io.status := io.status

  // Memory access units
  for ((mem_unit, w) <- mem_units.zipWithIndex) {
    mem_unit.io.lsu_io <> io.lsu_io(w)
    mem_unit.io.bp     := io.bp
    mem_unit.io.status := io.status
    mem_unit.io.com_exception := io.com_exception
  }

  // Core <-> FPU transfer units
  if (usingFPU) {
    for (unit <- exe_units.filter(_.hasFcsr)) {
      unit.io.fcsr_rm := io.fcsr_rm
    }
  }

  // RoCC unit
  if (usingRoCC) {
    rocc_unit.io.rocc <> io.rocc
    rocc_unit.io.com_exception := io.com_exception
  }
}
