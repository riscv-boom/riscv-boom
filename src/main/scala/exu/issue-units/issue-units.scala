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

   for (issParam <- issueParams.filter(_.iqType != IQT_FP.litValue))
   {
      iss_units += Module(new IssueUnitCollasping(issParam, num_wakeup_ports))
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
