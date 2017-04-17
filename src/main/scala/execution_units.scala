//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISC-V Constructing the Execution Units
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom

import Chisel._
import config.Parameters

import scala.collection.mutable.ArrayBuffer

class ExecutionUnits(implicit val p: Parameters) extends HasBoomCoreParameters
{
   val totalIssueWidth = issueWidths.reduce(_+_)
   require (totalIssueWidth >= 2)

   println("\n   ~*** " + Seq("One","Two","Three","Four")(DECODE_WIDTH-1) + "-wide Machine ***~\n")
   println("    -== " + Seq("Single","Dual","Triple","Quad","Five","Six")(totalIssueWidth-1) + " Issue ==- \n")


   //*******************************
   // Instantiate the ExecutionUnits

   private val exe_units = ArrayBuffer[ExecutionUnit]()

   //*******************************
   // Act like a collection

   def length = exe_units.length

   def apply(n: Int) = exe_units(n)

   def map[T](f: ExecutionUnit => T) =
   {
      exe_units.map(f)
   }

   def withFilter(f: ExecutionUnit => Boolean) =
   {
      exe_units.withFilter(f)
   }

   def zipWithIndex =
   {
      exe_units.zipWithIndex
   }

   lazy val memory_unit =
   {
      require (exe_units.count(_.is_mem_unit) == 1) // only one mem_unit supported
      exe_units.find(_.is_mem_unit).get
   }

   lazy val br_unit =
   {
      require (exe_units.count(_.hasBranchUnit) == 1)
      exe_units.find(_.hasBranchUnit).get
   }

   lazy val csr_unit =
   {
      require (exe_units.count(_.uses_csr_wport) == 1)
      exe_units.find(_.uses_csr_wport).get
   }

   lazy val br_unit_io =
   {
      require (exe_units.count(_.hasBranchUnit) == 1)
      (exe_units.find(_.hasBranchUnit).get).io.br_unit
   }

   lazy val br_unit_idx =
   {
      exe_units.indexWhere(_.hasBranchUnit)
   }



   exe_units += Module(new MemExeUnit())
   exe_units += Module(new ALUExeUnit(is_branch_unit      = true
                                       , shares_csr_wport = true
                                       , has_mul          = true
                                       , use_slow_mul     = true // TODO
                                       , has_div          = true
                                       ))
   for (w <- 0 until issueWidths(1)-1) exe_units += Module(new ALUExeUnit())
   require (!usingFPU) // TODO BUG add back support for FP


   require (exe_units.length != 0)
   require (exe_units.map(_.is_mem_unit).reduce(_|_), "Datapath is missing a memory unit.")
   require (exe_units.map(_.has_mul).reduce(_|_), "Datapath is missing a multiplier.")
   require (exe_units.map(_.has_div).reduce(_|_), "Datapath is missing a divider.")
   require (exe_units.map(_.has_fpu).reduce(_|_) == usingFPU, "Datapath is missing a fpu (or has an fpu and shouldnt).")

   val num_rf_read_ports = exe_units.map(_.num_rf_read_ports).reduce[Int](_+_)
   val num_rf_write_ports = exe_units.map(_.num_rf_write_ports).reduce[Int](_+_)
   val num_total_bypass_ports = exe_units.withFilter(_.isBypassable).map(_.numBypassPorts).reduce[Int](_+_)
   val num_fast_wakeup_ports = exe_units.count(_.isBypassable)
   // TODO reduce the number of slow wakeup ports - currently have every write-port also be a slow-wakeup-port.
   val num_slow_wakeup_ports = num_rf_write_ports
   // The slow write ports to the regfile are variable latency, and thus can't be bypassed.
   // val num_slow_wakeup_ports = exe_units.map(_.num_variable_write_ports).reduce[Int](_+_)

   // TODO bug, this can return too many fflag ports,e.g., the FPU is shared with the mem unit and thus has two wb ports
   val num_fpu_ports = exe_units.withFilter(_.hasFFlags).map(_.num_rf_write_ports).foldLeft(0)(_+_)

   val num_wakeup_ports = num_slow_wakeup_ports + num_fast_wakeup_ports
   val rf_cost = (num_rf_read_ports+num_rf_write_ports)*(num_rf_read_ports+2*num_rf_write_ports)
}
