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
import freechips.rocketchip.config.Parameters

import scala.collection.mutable.ArrayBuffer

class ExecutionUnits(fpu: Boolean = false)(implicit val p: Parameters) extends HasBoomCoreParameters
{
   val totalIssueWidth = issueParams.map(_.issueWidth).sum
   if (!fpu) {
      println("\n   ~*** " + Seq("One","Two","Three","Four")(DECODE_WIDTH-1) + "-wide Machine ***~\n")
      println("    -== " + Seq("Single","Dual","Triple","Quad","Five","Six")(totalIssueWidth-1) + " Issue ==- \n")
   }


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

   def foreach[U](f: ExecutionUnit => U) =
   {
      exe_units.foreach(f)
   }

   def zipWithIndex =
   {
      exe_units.zipWithIndex
   }

   def indexWhere(f: ExecutionUnit => Boolean) =
   {
      exe_units.indexWhere(f)
   }

   def count(f: ExecutionUnit => Boolean) =
   {
      exe_units.count(f)
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

   lazy val ifpu_unit =
   {
      require (exe_units.count(_.has_ifpu) == 1)
      exe_units.find(_.has_ifpu).get
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



   if (!fpu) {
      val int_width = issueParams.find(_.iqType == IQT_INT.litValue).get.issueWidth
      exe_units += Module(new MemExeUnit())
      exe_units += Module(new ALUExeUnit(is_branch_unit      = true
                                          , shares_csr_wport = true
                                          , has_mul          = true
                                          , use_slow_mul     = false
                                          , has_div          = true
                                          , has_ifpu         = int_width==1
                                          ))
      for (w <- 0 until int_width-1) {
         val is_last = w == (int_width-2)
         exe_units += Module(new ALUExeUnit(has_ifpu = is_last))
      }
   } else {
      require (usingFPU)
      val fp_width = issueParams.find(_.iqType == IQT_FP.litValue).get.issueWidth
      require (fp_width <= 1) // TODO hacks to fix include uopSTD_fp needing a proper func unit.
      for (w <- 0 until fp_width) {
         exe_units += Module(new FPUExeUnit(has_fpu = true,
                                            has_fdiv = usingFDivSqrt && (w==0),
                                            has_fpiu = (w==0)))
      }
      exe_units += Module(new IntToFPExeUnit())
   }


   require (exe_units.length != 0)
   // if this is for FPU units, we don't need a memory unit (or other integer units)..
   require (exe_units.map(_.is_mem_unit).reduce(_|_) || fpu, "Datapath is missing a memory unit.")
   require (exe_units.map(_.has_mul).reduce(_|_) || fpu, "Datapath is missing a multiplier.")
   require (exe_units.map(_.has_div).reduce(_|_) || fpu, "Datapath is missing a divider.")
   require (exe_units.map(_.has_fpu).reduce(_|_) == usingFPU || !fpu, "Datapath is missing a fpu (or has an fpu and shouldnt).")

   val num_rf_read_ports = exe_units.map(_.num_rf_read_ports).reduce[Int](_+_)
   val num_rf_write_ports = exe_units.map(_.num_rf_write_ports).reduce[Int](_+_)
   val num_total_bypass_ports = exe_units.withFilter(_.isBypassable).map(_.numBypassPorts).foldLeft(0)(_+_)
//   val num_fast_wakeup_ports = exe_units.count(_.isBypassable)
   // TODO reduce the number of slow wakeup ports - currently have every write-port also be a slow-wakeup-port.
   // +1 is for FP->Int moves. TODO HACK move toint to share the mem port.
//   val num_slow_wakeup_ports = num_rf_write_ports + 1
   // The slow write ports to the regfile are variable latency, and thus can't be bypassed.
   // val num_slow_wakeup_ports = exe_units.map(_.num_variable_write_ports).reduce[Int](_+_)

   // TODO bug, this can return too many fflag ports,e.g., the FPU is shared with the mem unit and thus has two wb ports
   val num_fpu_ports = exe_units.withFilter(_.hasFFlags).map(_.num_rf_write_ports).foldLeft(0)(_+_)

   val bypassable_write_port_mask = {
      if (fpu)
      {
         // NOTE: hack for the long latency load pipe which is write_port(0) and doesn't support bypassing.
         val mask = Seq(false) ++ exe_units.withFilter(_.uses_iss_unit).map(_.isBypassable)
         require (!mask.reduce(_||_)) // don't support any bypassing in FP
         mask
      }
      else
      {
         exe_units.withFilter(_.usesIRF).map(_.isBypassable)
      }
   }

}
