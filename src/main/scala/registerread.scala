//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Register Read
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2012 Apr 29

// Handle the register read and bypass network for the OoO backend
// interfaces with the issue window on the enqueue side, and the execution
// pipelines on the dequeue side.

package boom

import Chisel._
import cde.Parameters

class RegisterReadIO(
   issue_width: Int,
   num_total_read_ports: Int,
   num_total_bypass_ports: Int,
   register_width: Int
)(implicit p: Parameters) extends  BoomBundle()(p)
{
   // issued micro-ops
   val iss_valids = Vec(issue_width, Bool(INPUT))
   val iss_uops   = Vec(issue_width, new MicroOp()).asInput

   // interface with register file's read ports
   val rf_read_ports = Vec(num_total_read_ports, new RegisterFileReadPortIO(PREG_SZ, register_width)).flip
//   val rf_read_ports = Vec(num_total_read_ports, new RegisterFileReadPortIO(PREG_SZ, register_width).flip)

   val bypass = new BypassData(num_total_bypass_ports, register_width).asInput

   // send micro-ops to the execution pipelines
   val exe_reqs = Vec(issue_width, (new DecoupledIO(new FuncUnitReq(register_width))))

   val kill   = Bool(INPUT)
   val brinfo = new BrResolutionInfo().asInput

   override def cloneType =
      new RegisterReadIO(issue_width, num_total_read_ports, num_total_bypass_ports, register_width
   )(p).asInstanceOf[this.type]
}


class RegisterRead(
   issue_width: Int,
   supported_units_array: Seq[SupportedFuncUnits],
   num_total_read_ports: Int,
   num_read_ports_array: Seq[Int],
                         // each exe_unit must tell us how many max
                         // operands it can accept (the sum should equal
                         // num_total_read_ports)
   num_total_bypass_ports: Int,
   register_width: Int
)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new RegisterReadIO(issue_width, num_total_read_ports, num_total_bypass_ports, register_width)

   val rrd_valids       = Wire(Vec(issue_width, Bool()))
   val rrd_uops         = Wire(Vec(issue_width, new MicroOp()))

   val exe_reg_valids   = Reg(init = Vec.fill(issue_width) { Bool(false) })
   val exe_reg_uops     = Reg(Vec(issue_width, new MicroOp()))
   val exe_reg_rs1_data = Reg(Vec(issue_width, Bits(width = register_width)))
   val exe_reg_rs2_data = Reg(Vec(issue_width, Bits(width = register_width)))
   val exe_reg_rs3_data = Reg(Vec(issue_width, Bits(width = register_width)))


   //-------------------------------------------------------------
   // hook up inputs

   for (w <- 0 until issue_width)
   {
      val rrd_decode_unit = Module(new RegisterReadDecode(supported_units_array(w)))
      rrd_decode_unit.io.iss_valid := io.iss_valids(w)
      rrd_decode_unit.io.iss_uop   := io.iss_uops(w)

      rrd_valids(w) := rrd_decode_unit.io.rrd_valid
      rrd_uops(w)   := rrd_decode_unit.io.rrd_uop
   }


   //-------------------------------------------------------------
   // read ports

   require (num_total_read_ports == num_read_ports_array.reduce(_+_))

   val regwidth = if (usingFPU) 65 else 64
   val rrd_rs1_data   = Wire(Vec(issue_width, Bits(width=regwidth)))
   val rrd_rs2_data   = Wire(Vec(issue_width, Bits(width=regwidth)))
   val rrd_rs3_data   = Wire(Vec(issue_width, Bits(width=regwidth)))

   var idx = 0 // index into flattened read_ports array
   for (w <- 0 until issue_width)
   {
      val num_read_ports = num_read_ports_array(w)

      val rs1_addr = rrd_uops(w).pop1
      val rs2_addr = rrd_uops(w).pop2
      val rs3_addr = rrd_uops(w).pop3

      if (num_read_ports > 0) io.rf_read_ports(idx+0).addr := rs1_addr
      if (num_read_ports > 1) io.rf_read_ports(idx+1).addr := rs2_addr
      if (num_read_ports > 2) io.rf_read_ports(idx+2).addr := rs3_addr

      if (num_read_ports > 0) rrd_rs1_data(w) := io.rf_read_ports(idx+0).data
      if (num_read_ports > 1) rrd_rs2_data(w) := io.rf_read_ports(idx+1).data
      if (num_read_ports > 2) rrd_rs3_data(w) := io.rf_read_ports(idx+2).data

      val rrd_kill = Mux(io.kill, Bool(true),
                     Mux(io.brinfo.valid && io.brinfo.mispredict
                                       , maskMatch(rrd_uops(w).br_mask, io.brinfo.mask)
                                       , Bool(false)))

      exe_reg_valids(w) := Mux(rrd_kill, Bool(false), rrd_valids(w))
      // TODO use only the valids signal, don't require us to set nullUop
      exe_reg_uops(w)   := Mux(rrd_kill, NullMicroOp, rrd_uops(w))

      exe_reg_uops(w).br_mask := GetNewBrMask(io.brinfo, rrd_uops(w))

      idx += num_read_ports
   }


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // BYPASS MUXES -----------------------------------------------
   // performed at the end of the register read stage

   // NOTES: this code is fairly hard-coded. Sorry.
   // ASSUMPTIONS:
   //    -rs3 is used for FPU ops which are NOT bypassed (so don't check
   //       them!).

   val bypassed_rs1_data = Wire(Vec(issue_width, Bits(width = register_width)))
   val bypassed_rs2_data = Wire(Vec(issue_width, Bits(width = register_width)))

   for (w <- 0 until issue_width)
   {
      val num_read_ports = num_read_ports_array(w)
      var rs1_cases = Array((Bool(false), Bits(0, register_width)))
      var rs2_cases = Array((Bool(false), Bits(0, register_width)))

      val pop1       = rrd_uops(w).pop1
      val lrs1_rtype = rrd_uops(w).lrs1_rtype
      val pop2       = rrd_uops(w).pop2
      val lrs2_rtype = rrd_uops(w).lrs2_rtype

      for (b <- 0 until io.bypass.getNumPorts)
      {
         // can't use "io.bypass.valid(b) since it would create a combinational loop on branch kills"
         rs1_cases ++= Array((io.bypass.valid(b) && (pop1 === io.bypass.uop(b).pdst) && io.bypass.uop(b).ctrl.rf_wen
            && (lrs1_rtype === RT_FIX || lrs1_rtype === RT_FLT) && (pop1 =/= UInt(0)), io.bypass.data(b)))
         rs2_cases ++= Array((io.bypass.valid(b) && (pop2 === io.bypass.uop(b).pdst) && io.bypass.uop(b).ctrl.rf_wen
            && (lrs2_rtype === RT_FIX || lrs2_rtype === RT_FLT) && (pop2 =/= UInt(0)), io.bypass.data(b)))
      }

      if (num_read_ports > 0) bypassed_rs1_data(w) := MuxCase(rrd_rs1_data(w), rs1_cases)
      if (num_read_ports > 1) bypassed_rs2_data(w) := MuxCase(rrd_rs2_data(w), rs2_cases)
   }


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Execute Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   for (w <- 0 until issue_width)
   {
      val num_read_ports = num_read_ports_array(w)
      if (num_read_ports > 0) exe_reg_rs1_data(w) := bypassed_rs1_data(w)
      if (num_read_ports > 1) exe_reg_rs2_data(w) := bypassed_rs2_data(w)
      if (num_read_ports > 2) exe_reg_rs3_data(w) := rrd_rs3_data(w)
      // ASSUMPTION: rs3 is FPU which is NOT bypassed
   }
   // TODO add assert to detect bypass conflicts on non-bypassable things
   // TODO add assert that checks bypassing to verify there isn't something it hits rs3

   //-------------------------------------------------------------
   // set outputs to execute pipelines
   for (w <- 0 until issue_width)
   {
      val num_read_ports = num_read_ports_array(w)

      io.exe_reqs(w).valid    := exe_reg_valids(w)
      io.exe_reqs(w).bits.uop := exe_reg_uops(w)
      if (num_read_ports > 0) io.exe_reqs(w).bits.rs1_data := exe_reg_rs1_data(w)
      if (num_read_ports > 1) io.exe_reqs(w).bits.rs2_data := exe_reg_rs2_data(w)
      if (num_read_ports > 2) io.exe_reqs(w).bits.rs3_data := exe_reg_rs3_data(w)
   }
}
