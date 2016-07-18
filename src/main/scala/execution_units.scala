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
import cde.Parameters

import scala.collection.mutable.ArrayBuffer

class ExecutionUnits(implicit val p: Parameters) extends HasBoomCoreParameters
{
   if (DECODE_WIDTH == 1)      println("\n   ~*** One-wide Machine ***~\n")
   else if (DECODE_WIDTH == 2) println("\n   ~*** Two-wide Machine ***~\n")
   else if (DECODE_WIDTH == 4) println("\n   ~*** Four-wide Machine ***~\n")
   else                        println("\n ~*** Unknown Machine Width ***~\n")

   require (ISSUE_WIDTH <= 4)
   if (ISSUE_WIDTH == 1) println("    -== Single Issue ==- \n")
   if (ISSUE_WIDTH == 2) println("    -== Dual Issue ==- \n")
   if (ISSUE_WIDTH == 3) println("    -== Triple Issue ==- \n")
   if (ISSUE_WIDTH == 4) println("    -== Quad Issue ==- \n")


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

   lazy val br_unit_io =
   {
      require (exe_units.count(_.hasBranchUnit) == 1)
      (exe_units.find(_.hasBranchUnit).get).io.br_unit
   }

   lazy val br_unit_idx =
   {
      exe_units.indexWhere(_.hasBranchUnit)
   }


   if (ISSUE_WIDTH == 1)
   {
      exe_units += Module(new ALUMemExeUnit(is_branch_unit   = true
                                          , shares_csr_wport = true
                                          , fp_mem_support   = usingFPU
                                          , has_fpu          = usingFPU
                                          , has_mul          = true
                                          , has_div          = true
                                          , use_slow_mul     = false
                                          , has_fdiv         = usingFPU && usingFDivSqrt
                                          ))
   }
   else if (ISSUE_WIDTH == 2)
   {
      exe_units += Module(new ALUExeUnit(is_branch_unit      = true
                                          , shares_csr_wport = true
                                          , has_fpu          = usingFPU
                                          , has_mul          = true
                                          ))
      exe_units += Module(new ALUMemExeUnit(fp_mem_support   = usingFPU
                                          , has_div          = true
                                          , has_fdiv         = usingFPU && usingFDivSqrt
                                          ))
   }
   else if (ISSUE_WIDTH == 3)
   {
      exe_units += Module(new ALUExeUnit(is_branch_unit      = true
                                          , shares_csr_wport = true
                                          , has_fpu          = usingFPU
                                          , has_mul          = true
                                          ))
      exe_units += Module(new ALUExeUnit(has_div             = true
                                          , has_fdiv         = usingFPU && usingFDivSqrt
                                          ))
      exe_units += Module(new MemExeUnit())
   }
   else
   {
      require (ISSUE_WIDTH == 4)
      exe_units += Module(new ALUExeUnit(is_branch_unit      = false
                                          , shares_csr_wport = true
                                          , has_fpu          = usingFPU
                                          , has_mul          = true
                                          ))
      exe_units += Module(new ALUExeUnit(is_branch_unit      = true))
      exe_units += Module(new ALUExeUnit(has_div             = true
                                          , has_fdiv         = usingFPU && usingFDivSqrt
                                          ))
      exe_units += Module(new MemExeUnit())
   }

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
