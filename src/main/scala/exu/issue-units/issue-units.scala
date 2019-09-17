//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Units Collection Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import scala.collection.mutable.ArrayBuffer

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import boom.common._
import boom.exu.FUConstants._

/**
 * Top level class to wrap integer and memory issue units together
 * into a single "collection"
 *
 * @param numWakeupPorts number of wakeup ports per issue unit
 */
class IssueUnits(val numWakeupPorts: Int)(implicit val p: Parameters)
  extends HasBoomCoreParameters
  with IndexedSeq[IssueUnit]
{
  // issue unit collection
  private val issue_units = ArrayBuffer[IssueUnit]()

  /**
   * Amount of issue units in the collection
   *
   * @return amount of issue units in collection
   */
  def length = issue_units.length

  /**
   * Retrieve the nth issue unit in the collection
   *
   * @param n index of issue unit to retrieve
   * @return the IssueUnit of that index
   */
  def apply(n: Int): IssueUnit = issue_units(n)

  require (enableAgePriorityIssue) // unordered is currently unsupported.

  // create the issue units (note: this does not create the fp issue unit)
  for (issueParam <- issueParams.filter(_.iqType != IQT_FP.litValue)) {
    val issue_unit = Module(new IssueUnitCollapsing(issueParam, numWakeupPorts))

    // name the issue units
    if (issueParam.iqType == IQT_INT.litValue) {
      issue_unit.suggestName("int_issue_unit")
    } else if (issueParam.iqType == IQT_MEM.litValue) {
      issue_unit.suggestName("mem_issue_unit")
    }

    issue_units += issue_unit
  }

  /**
   * Get the memory issue queue
   *
   * @return the IssueUnit used for memory uops
   */
  def mem_iq = issue_units.find(_.iqType == IQT_MEM.litValue).get

  /**
   * Get the integer issue queue
   *
   * @return the IssueUnit used for integer uops
   */
  def int_iq = issue_units.find(_.iqType == IQT_INT.litValue).get
}
