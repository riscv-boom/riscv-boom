//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Units Logic
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
 * Top level class to wrap all issue units together into a "collection"
 *
 * @param num_wakeup_ports number of wakeup ports per issue unit
 */
class IssueUnits(num_wakeup_ports: Int)(implicit val p: Parameters)
   extends HasBoomCoreParameters
   with IndexedSeq[IssueUnit]
{
   //*******************************
   // Instantiate the IssueUnits

   private val iss_units = ArrayBuffer[IssueUnit]()

   //*******************************
   // Act like a collection

   def length = iss_units.length

   def apply(n: Int): IssueUnit = iss_units(n)

   //*******************************
   // Construct.

   require (enableAgePriorityIssue) // unordered is currently unsupported.

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

      iss_units += issueUnit
   }

   def mem_iq = if (usingUnifiedMemIntIQs)
   {
      // When using unified IQs the IQT_INT handles everything
      iss_units.find(_.iqType == IQT_INT.litValue).get
   }
   else
   {
      iss_units.find(_.iqType == IQT_MEM.litValue).get
   }

   def int_iq = iss_units.find(_.iqType == IQT_INT.litValue).get
}
