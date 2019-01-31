//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// RISC-V Constructing the Execution Units
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import freechips.rocketchip.config.Parameters
import scala.collection.mutable.ArrayBuffer
import boom.common._

class ExecutionUnits(fpu: Boolean)(implicit val p: Parameters) extends HasBoomCoreParameters
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
      require (exe_units.count(_.has_mem) == 1) // only one mem_unit supported
      exe_units.find(_.has_mem).get
   }

   lazy val br_unit =
   {
      require (exe_units.count(_.has_br_unit) == 1)
      exe_units.find(_.has_br_unit).get
   }

   lazy val csr_unit =
   {
      require (exe_units.count(_.uses_csr_wport) == 1)
      exe_units.find(_.uses_csr_wport).get
   }

   lazy val ifpu_unit =
   {
      require (usingFPU)
      require (exe_units.count(_.has_ifpu) == 1)
      exe_units.find(_.has_ifpu).get
   }
   lazy val fpiu_unit =
   {
      require (usingFPU)
      require (exe_units.count(_.has_fpiu) == 1)
      exe_units.find(_.has_fpiu).get
   }

   lazy val br_unit_io =
   {
      require (exe_units.count(_.has_br_unit) == 1)
      (exe_units.find(_.has_br_unit).get).io.br_unit
   }

   lazy val br_unit_idx =
   {
      exe_units.indexWhere(_.has_br_unit)
   }

   if (!fpu)
   {
      val int_width = issueParams.find(_.iqType == IQT_INT.litValue).get.issueWidth
      val memExeUnit = Module(new ALUExeUnit(
         has_alu = false,
         has_mem = true))

      memExeUnit.io.status := DontCare
      memExeUnit.io.get_ftq_pc := DontCare
      memExeUnit.io.ll_iresp.ready := DontCare

      exe_units += memExeUnit

      val aluExeUnit = Module(new ALUExeUnit(has_br_unit      = true
         , shares_csr_wport = true
         , has_mul          = true
         , has_div          = true
         , has_ifpu         = usingFPU
      ))

      aluExeUnit.io.lsu_io := DontCare
      aluExeUnit.io.dmem := DontCare
      aluExeUnit.io.get_ftq_pc := DontCare

      exe_units += aluExeUnit

      for (w <- 0 until int_width-1) {
         val aluExeUnit = Module(new ALUExeUnit)

         aluExeUnit.io.dmem := DontCare
         aluExeUnit.io.lsu_io := DontCare
         aluExeUnit.io.get_ftq_pc := DontCare
         aluExeUnit.io.status := DontCare

         exe_units += aluExeUnit
      }
   }
   else
   {
      val fp_width = issueParams.find(_.iqType == IQT_FP.litValue).get.issueWidth
      require (fp_width == 1) // TODO hacks to fix include uopSTD_fp needing a proper func unit.
      for (w <- 0 until fp_width) {
         val fpuExeUnit = Module(new FPUExeUnit(has_fpu = true,
                                            has_fdiv = usingFDivSqrt && (w==0),
                                            has_fpiu = (w==0)))
         fpuExeUnit.io.status := DontCare
         fpuExeUnit.io.lsu_io := DontCare
         fpuExeUnit.io.dmem := DontCare
         fpuExeUnit.io.get_ftq_pc := DontCare

         exe_units += fpuExeUnit
      }
   }

   val exe_units_str = new StringBuilder
   exe_units_str.append(
      if (!fpu) {
         ( "\n   ~*** " + Seq("One","Two","Three","Four")(decodeWidth-1) + "-wide Machine ***~\n"
         + "\n    -== " + Seq("Single","Dual","Triple","Quad","Five","Six")(totalIssueWidth-1) + " Issue ==- \n")
      }
   )

   for (exe_unit <- exe_units) {
      exe_units_str.append(exe_unit.toString)
   }
   override def toString: String =  exe_units_str.toString

   require (exe_units.length != 0)
   if (!fpu)
   {
      // if this is for FPU units, we don't need a memory unit (or other integer units).
      require (exe_units.map(_.has_mem).reduce(_|_), "Datapath is missing a memory unit.")
      require (exe_units.map(_.has_mul).reduce(_|_), "Datapath is missing a multiplier.")
      require (exe_units.map(_.has_div).reduce(_|_), "Datapath is missing a divider.")
   }
   else
   {
      require (exe_units.map(_.has_fpu).reduce(_|_),
         "Datapath is missing a fpu (or has an fpu and shouldnt).")
   }

   val num_irf_readers        = exe_units.count(_.reads_irf)
   val num_irf_read_ports     = exe_units.count(_.reads_irf) * 2
   val num_irf_write_ports    = exe_units.count(_.writes_irf)
   val num_ll_irf_write_ports = exe_units.count(_.writes_ll_irf)
   val num_total_bypass_ports = exe_units.withFilter(_.bypassable).map(_.num_bypass_stages).foldLeft(0)(_+_)

   val num_frf_readers        = exe_units.count(_.reads_frf)
   val num_frf_read_ports     = exe_units.count(_.reads_frf) * 3
   val num_frf_write_ports    = exe_units.count(_.writes_frf)
   val num_ll_frf_write_ports = exe_units.count(_.writes_ll_frf)

   // The mem-unit will also bypass writes to readers in the RRD stage.
   // NOTE: This does NOT include the ll_wport
   val bypassable_write_port_mask = exe_units.withFilter(x => x.writes_irf).map(u => u.bypassable)

}
