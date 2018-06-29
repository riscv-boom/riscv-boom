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

package boom.exu

import Chisel._
import freechips.rocketchip.config.Parameters
import boom.common._
import boom.util._


class VectorRegisterRead(
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
with Packing
{
   val reg_sz = log2Ceil(numVecRegFileRows)
   val io = IO(new RegisterReadIO(issue_width, num_total_read_ports, num_total_bypass_ports, register_width, reg_sz))

   val rrd_valids       = Wire(Vec(issue_width, Bool()))
   val rrd_uops         = Wire(Vec(issue_width, new MicroOp()))

   val exe_reg_valids   = Reg(init = Vec.fill(issue_width) { Bool(false) })
   val exe_reg_uops     = Reg(Vec(issue_width, new MicroOp()))
   val exe_reg_rs1_data = Reg(Vec(issue_width, Bits(width = register_width)))
   val exe_reg_rs2_data = Reg(Vec(issue_width, Bits(width = register_width)))
   val exe_reg_rs3_data = Reg(Vec(issue_width, Bits(width = register_width)))

   assert(num_total_bypass_ports == 0, "No bypassing supported")

   //-------------------------------------------------------------
   // hook up inputs

   for (w <- 0 until issue_width)
   {
      val rrd_decode_unit = Module(new RegisterReadDecode(supported_units_array(w)))
      rrd_decode_unit.io.iss_valid := io.iss_valids(w)
      rrd_decode_unit.io.iss_uop   := io.iss_uops(w)

      if (regreadLatency == 1) {
         rrd_valids(w) := RegNext(rrd_decode_unit.io.rrd_valid &&
                           !IsKilledByBranch(io.brinfo, rrd_decode_unit.io.rrd_uop))
         rrd_uops(w)   := RegNext(GetNewUopAndBrMask(rrd_decode_unit.io.rrd_uop, io.brinfo))
      } else {
         rrd_valids(w) := rrd_decode_unit.io.rrd_valid
         rrd_uops(w)   := rrd_decode_unit.io.rrd_uop
      }
   }


   //-------------------------------------------------------------
   // read ports

   require (num_total_read_ports == num_read_ports_array.reduce(_+_))

   val rrd_rs1_data   = Wire(Vec(issue_width, Bits(width=register_width)))
   val rrd_rs2_data   = Wire(Vec(issue_width, Bits(width=register_width)))
   val rrd_rs3_data   = Wire(Vec(issue_width, Bits(width=register_width)))



   var idx = 0 // index into flattened read_ports array
   for (w <- 0 until issue_width)
   {
      val num_read_ports = num_read_ports_array(w)

      // NOTE:
      // If rrdLatency==0, ISS and RRD are in same cycle so this "just works".
      // If rrdLatency==1, we need to send read address at end of ISS stage,
      //    in order to get read data back at end of RRD stage.
      require (regreadLatency == 1)

      val rs1_addr = Wire(UInt())
      val rs2_addr = Wire(UInt())
      val rs3_addr = Wire(UInt())
      rs1_addr := CalcVecRegAddr(
         io.iss_uops(w).rs1_vew,
         io.iss_uops(w).eidx,
         io.iss_uops(w).pop1,
         numVecPhysRegs)
      rs2_addr := CalcVecRegAddr(
         io.iss_uops(w).rs2_vew,
         io.iss_uops(w).eidx,
         io.iss_uops(w).pop2,
         numVecPhysRegs)
      rs3_addr := CalcVecRegAddr(
         io.iss_uops(w).rs3_vew,
         io.iss_uops(w).eidx,
         io.iss_uops(w).pop3,
         numVecPhysRegs)

      if (num_read_ports > 0) io.rf_read_ports(idx+0).addr := rs1_addr
      if (num_read_ports > 1) io.rf_read_ports(idx+1).addr := rs2_addr
      if (num_read_ports > 2) io.rf_read_ports(idx+2).addr := rs3_addr

      io.rf_read_ports(idx+0).enable := io.iss_uops(w).lrs1_rtype === RT_VEC
      io.rf_read_ports(idx+1).enable := io.iss_uops(w).lrs2_rtype === RT_VEC
      io.rf_read_ports(idx+2).enable := io.iss_uops(w).lrs3_rtype === RT_VEC

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
   // **** Execute Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   for (w <- 0 until issue_width)
   {
      val num_read_ports = num_read_ports_array(w)
      if (num_read_ports > 0) exe_reg_rs1_data(w) := rrd_rs1_data(w)
      if (num_read_ports > 1) exe_reg_rs2_data(w) := rrd_rs2_data(w)
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


      def fill_case(n: UInt, s: UInt): UInt = {
         MuxLookup(s, VEW_8, Array(
            VEW_8  -> fill_b(n),
            VEW_16 -> fill_h(n),
            VEW_32 -> fill_w(n),
            VEW_64 -> fill_d(n)))
      }
      when (exe_reg_uops(w).lrs1_rtype === RT_FLT) {
         io.exe_reqs(w).bits.rs1_data := fill_case(exe_reg_uops(w).rs1_data, exe_reg_uops(w).rs1_vew)
      }
      when (exe_reg_uops(w).lrs2_rtype === RT_FLT) {
         io.exe_reqs(w).bits.rs2_data := fill_case(exe_reg_uops(w).rs2_data, exe_reg_uops(w).rs2_vew)
      }
      when (exe_reg_uops(w).lrs3_rtype === RT_FLT) {
         io.exe_reqs(w).bits.rs3_data := fill_case(exe_reg_uops(w).rs3_data, exe_reg_uops(w).rs3_vew)
      }

   }
}
