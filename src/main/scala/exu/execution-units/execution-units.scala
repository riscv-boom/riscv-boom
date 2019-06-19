//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISC-V Constructing the Execution Units
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import scala.collection.mutable.{ArrayBuffer}

import chisel3._

import freechips.rocketchip.config.{Parameters}

import boom.common._
import boom.util.{AddToStringPrefix}

/**
 * Top level class to wrap all execution units together into a "collection"
 *
 * @param fpu using a FPU?
 */
class ExecutionUnits(val fpu: Boolean)(implicit val p: Parameters) extends HasBoomCoreParameters
{
  val totalIssueWidth = issueParams.map(_.issueWidth).sum

  //*******************************
  // Instantiate the ExecutionUnits

  private val exe_units = ArrayBuffer[ExecutionUnit]()

  //*******************************
  // Act like a collection

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

  lazy val memory_unit = {
    require (exe_units.count(_.hasMem) == 1) // only one mem_unit supported
    exe_units.find(_.hasMem).get
  }

  lazy val br_unit = {
    require (exe_units.count(_.hasBrUnit) == 1)
    exe_units.find(_.hasBrUnit).get
  }

  lazy val csr_unit = {
    require (exe_units.count(_.hasCSR) == 1)
    exe_units.find(_.hasCSR).get
  }

  lazy val ifpu_unit = {
    require (usingFPU)
    require (exe_units.count(_.hasIfpu) == 1)
    exe_units.find(_.hasIfpu).get
  }

  lazy val fpiu_unit = {
    require (usingFPU)
    require (exe_units.count(_.hasFpiu) == 1)
    exe_units.find(_.hasFpiu).get
  }

  lazy val br_unit_io = {
    require (exe_units.count(_.hasBrUnit) == 1)
    (exe_units.find(_.hasBrUnit).get).io.br_unit
  }

  lazy val br_unit_idx = {
    exe_units.indexWhere(_.hasBrUnit)
  }

  lazy val rocc_unit = {
    require (usingRoCC)
    require (exe_units.count(_.hasRocc) == 1)
    exe_units.find(_.hasRocc).get
  }

  if (!fpu) {
    val int_width = issueParams.find(_.iqType == IQT_INT.litValue).get.issueWidth

    if (!usingUnifiedMemIntIQs) {
      val memExeUnit = Module(new ALUExeUnit(
        hasAlu = false,
        hasMem = true))

        memExeUnit.io.ll_iresp.ready := DontCare

        exe_units += memExeUnit
    }


    for (w <- 0 until int_width) {
      def is_nth(n: Int): Boolean = w == ((n) % int_width)
      val alu_exe_unit = Module(new ALUExeUnit(
        hasBrUnit      = is_nth(0),
        hasCSR         = is_nth(1),
        hasRocc        = is_nth(1) && usingRoCC,
        hasMul         = is_nth(2),
        hasDiv         = is_nth(3),
        hasIfpu        = is_nth(4) && usingFPU,
        hasMem         = usingUnifiedMemIntIQs))
      exe_units += alu_exe_unit
    }
  } else {
    val fp_width = issueParams.find(_.iqType == IQT_FP.litValue).get.issueWidth
    for (w <- 0 until fp_width) {
      val fpu_exe_unit = Module(new FPUExeUnit(hasFpu = true,
                                             hasFdiv = usingFDivSqrt && (w==0),
                                             hasFpiu = (w==0)))
      exe_units += fpu_exe_unit
    }
  }

  val exeUnitsStr = new StringBuilder
  for (exe_unit <- exe_units) {
    exeUnitsStr.append(exe_unit.toString)
  }

  override def toString: String =
    (AddToStringPrefix("===ExecutionUnits===")
    + "\n"
    + (if (!fpu) {
      AddToStringPrefix(
        "==" + Seq("One","Two","Three","Four")(coreWidth-1) + "-wide Machine==",
        "==" + Seq("Single","Dual","Triple","Quad","Five","Six")(totalIssueWidth-1) + " Issue==")
    } else {
      ""
    })
    + "\n"
    + exeUnitsStr.toString)

  require (exe_units.length != 0)
  if (!fpu) {
    // if this is for FPU units, we don't need a memory unit (or other integer units).
    require (exe_units.map(_.hasMem).reduce(_|_), "Datapath is missing a memory unit.")
    require (exe_units.map(_.hasMul).reduce(_|_), "Datapath is missing a multiplier.")
    require (exe_units.map(_.hasDiv).reduce(_|_), "Datapath is missing a divider.")
  } else {
    require (exe_units.map(_.hasFpu).reduce(_|_),
      "Datapath is missing a fpu (or has an fpu and shouldnt).")
  }

  val numIrfReaders       = exe_units.count(_.readsIrf)
  val numIrfReadPorts     = exe_units.count(_.readsIrf) * 2
  val numIrfWritePorts    = exe_units.count(_.writesIrf)
  val numLlIrfWritePorts  = exe_units.count(_.writesLlIrf)
  val numTotalBypassPorts = exe_units.withFilter(_.bypassable).map(_.numBypassStages).foldLeft(0)(_+_)

  val numFrfReaders       = exe_units.count(_.readsFrf)
  val numFrfReadPorts     = exe_units.count(_.readsFrf) * 3
  val numFrfWritePorts    = exe_units.count(_.writesFrf)
  val numLlFrfWritePorts  = exe_units.count(_.writesLlFrf)

  // The mem-unit will also bypass writes to readers in the RRD stage.
  // NOTE: This does NOT include the ll_wport
  val bypassable_write_port_mask = exe_units.withFilter(x => x.writesIrf).map(u => u.bypassable)
}
