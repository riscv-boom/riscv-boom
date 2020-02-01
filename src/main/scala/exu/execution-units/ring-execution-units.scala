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
  // I/O which is used by all units
  // Unit-specific I/O (e.g. rocc) can still be hooked up with the unit getter functions
  val io = IO(new BoomBundle {
    val exe_reqs  = Vec(coreWidth, Flipped(DecoupledIO(new FuncUnitReq(xLen))))
    val exe_resps = Output(Vec(coreWidth, Valid(new ExeUnitResp(xLen))))

    val brinfo    = Input(new BrResolutionInfo)
    val kill      = Input(Bool())
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

  def memory_units = {
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
}
