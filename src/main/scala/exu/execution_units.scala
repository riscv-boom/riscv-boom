//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISC-V Constructing the Execution Units
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import Chisel._
import freechips.rocketchip.config.Parameters
import scala.collection.mutable.ArrayBuffer
import boom.common._

// TODO_Vec: Vec unit should include vfpu, vmem, viu, valu
class ExecutionUnits(fpu: Boolean = false, vec: Boolean = false)(implicit val p: Parameters) extends HasBoomCoreParameters
{
   val totalIssueWidth = issueParams.map(_.issueWidth).sum

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

   lazy val itov_unit =
   {
      require (exe_units.count(_.has_itov) == 1)
      exe_units.find(_.has_itov).get
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


   if (!fpu && !vec) {
      // Integer and memory execution units
      val int_width = issueParams.find(_.iqType == IQT_INT.litValue).get.issueWidth
      exe_units += Module(new MemExeUnit())
      exe_units += Module(new ALUExeUnit(is_branch_unit      = true
                                          , shares_csr_wport = true
                                          , has_mul          = true
                                          , use_slow_mul     = false
                                          , has_div          = true
                                          , has_ifpu         = int_width==1
                                          , has_itov         = int_width==1 && usingVec
      ))
      for (w <- 1 until int_width) {
         val is_last = w == (int_width-1)
         exe_units += Module(new ALUExeUnit(has_ifpu = is_last, has_itov = is_last && usingVec))
      }
   } else if (!vec) {
      // FP execution units
      require (usingFPU)
      val fp_width = issueParams.find(_.iqType == IQT_FP.litValue).get.issueWidth
      require (fp_width <= 1) // TODO hacks to fix include uopSTD_fp needing a proper func unit.
      for (w <- 0 until fp_width) {
         exe_units += Module(new FPUExeUnit(has_fpu = true,
                                            has_fdiv = usingFDivSqrt && (w==0),
                                            has_fpiu = (w==0),
                                            has_fpvu = (w==0),
                                            usingVec = usingVec
         ))
      }
      exe_units += Module(new IntToFPExeUnit()) // IT makes so much more sense for this to sit in integer exe units, but alas
   } else {
      // Vector execution units
      val vec_width = issueParams.find(_.iqType == IQT_VEC.litValue).get.issueWidth
      require (vec_width <= 1)
      for (w <- 0 until vec_width) {
         exe_units += Module(new VecFPUExeUnit(has_vfpu = true, has_valu = true));
      }
      // TODO_vec: Add vmem, viu, valu
   }

   val exe_units_str = new StringBuilder
   exe_units_str.append(
      if (!fpu && !vec) {
         ( "\n   ~*** " + Seq("One","Two","Three","Four")(decodeWidth-1) + "-wide Machine ***~\n"
         + "\n    -== " + Seq("Single","Dual","Triple","Quad","Five","Six")(totalIssueWidth-1) + " Issue ==- \n")
      }
   )
   for (exe_unit <- exe_units) {
      exe_units_str.append(exe_unit.toString)
   }
   override def toString: String =  exe_units_str.toString

   require (exe_units.length != 0)
   // if this is for FPU units, we don't need a memory unit (or other integer units)..
   require (exe_units.map(_.is_mem_unit).reduce(_|_) || fpu || vec, "Datapath is missing a memory unit.")
   require (exe_units.map(_.has_mul).reduce(_|_) || fpu || vec, "Datapath is missing a multiplier.")
   require (exe_units.map(_.has_div).reduce(_|_) || fpu || vec, "Datapath is missing a divider.")
   require (exe_units.map(_.has_fpu).reduce(_|_) == usingFPU || !fpu, "Datapath is missing a fpu (or has an fpu and shouldnt).")

   // TODO we really should tie write ports to register files
   val num_rf_read_ports = exe_units.map(_.num_rf_read_ports).reduce[Int](_+_)
   val num_rf_write_ports = exe_units.map(_.num_rf_write_ports).reduce[Int](_+_)
   val num_total_bypass_ports = exe_units.withFilter(_.isBypassable).map(_.numBypassPorts).foldLeft(0)(_+_)

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
      else if (vec)
      {
         val mask = exe_units.withFilter(_.uses_iss_unit).map(_.isBypassable)
         require (!mask.reduce(_||_)) // don't support any bypassing in VEC
         assert(mask.length == 1, "Vector regfile only has 1 write port!")
         mask
      }
      else
      {
         // The mem-unit will also bypass writes to readers in the RRD stage.
         exe_units.withFilter(_.usesIRF).map(x => x.isBypassable || x.is_mem_unit)
      }
   }

}
