//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
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
 * @param num_wakeup_ports number of wakeup ports per issue unit
 */
class IssueUnits(num_wakeup_ports: Int)(implicit val p: Parameters)
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
   for (issueParam <- issueParams.filter(_.iqType != IQT_FP.litValue))
   {
      val issueUnit = Module(new IssueUnitCollapsing(issueParam, num_wakeup_ports))

      // name the issue units
      if (issueParam.iqType == IQT_INT.litValue)
      {
        if (usingUnifiedMemIntIQs)
        {
          issueUnit.suggestName("intmem_issue_unit")
        }
        else
        {
          issueUnit.suggestName("int_issue_unit")
        }
      }
      else if (issueParam.iqType == IQT_MEM.litValue)
      {
        issueUnit.suggestName("mem_issue_unit")
      }

      issue_units += issueUnit
   }

   /**
    * Get the memory issue queue
    *
    * @return the IssueUnit used for memory uops
    */
   def mem_iq = if (usingUnifiedMemIntIQs)
   {
      // When using unified issue queues the IQT_INT handles everything
      issue_units.find(_.iqType == IQT_INT.litValue).get
   }
   else
   {
      issue_units.find(_.iqType == IQT_MEM.litValue).get
   }

   /**
    * Get the integer issue queue
    *
    * @return the IssueUnit used for integer uops
    */
   def int_iq = issue_units.find(_.iqType == IQT_INT.litValue).get
}
