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
   register_width: Int
)(implicit p: Parameters) extends BoomModule()(p)
with Packing
{
   val uop_invalid :: uop_rs1 :: uop_rs2 :: uop_rs3 :: uop_rp :: uop_done = Enum(UInt(), 6)

   val reg_sz = log2Ceil(numVecRegFileRows)
   val io = IO(new RegisterReadIO(issue_width=1, num_total_read_ports=3, num_total_bypass_ports=0, register_width, reg_sz))

   val rrd_valids       = Wire(Bool())
   val rrd_uops         = Wire(new MicroOp())

   val exe_reg_valids   = Reg(init = Bool(false) )
   val exe_reg_uops     = Reg(new MicroOp())
   val exe_reg_rs1_data = Reg(Bits(width = register_width))
   val exe_reg_rs2_data = Reg(Bits(width = register_width))
   val exe_reg_rs3_data = Reg(Bits(width = register_width))
   val exe_reg_rp_data  = Reg(Bits(width = register_width/8))

   assert(issue_width == 1, "Vector pipeline supports issue width of 1")


   //-------------------------------------------------------------
   // hook up inputs


   val rrd_decode_unit = Module(new RegisterReadDecode(supported_units_array(0)))
   rrd_decode_unit.io.iss_valid := io.iss_valids(0)
   rrd_decode_unit.io.iss_uop   := io.iss_uops(0)


   rrd_valids := RegNext(rrd_decode_unit.io.rrd_valid &&
      !IsKilledByBranch(io.brinfo, rrd_decode_unit.io.rrd_uop))
   rrd_uops   := RegNext(GetNewUopAndBrMask(rrd_decode_unit.io.rrd_uop, io.brinfo))



   //-------------------------------------------------------------
   // read ports



   val rrd_rs1_data   = Wire(Bits(width=register_width))
   val rrd_rs2_data   = Wire(Bits(width=register_width))
   val rrd_rs3_data   = Wire(Bits(width=register_width))
   val rrd_rp_data    = Wire(Bits(width=register_width/8))



   // NOTE:
   // If rrdLatency==0, ISS and RRD are in same cycle so this "just works".
   // If rrdLatency==1, we need to send read address at end of ISS stage,
   //    in order to get read data back at end of RRD stage.
   require (regreadLatency == 1)

   val rs1_addr = Wire(UInt())
   val rs2_addr = Wire(UInt())
   val rs3_addr = Wire(UInt())
   rs1_addr := CalcVecRegAddr(
      io.iss_uops(0).rs1_vew,
      io.iss_uops(0).eidx,
      io.iss_uops(0).pop1,
      numVecPhysRegs)
   rs2_addr := CalcVecRegAddr(
      io.iss_uops(0).rs2_vew,
      io.iss_uops(0).eidx,
      io.iss_uops(0).pop2,
      numVecPhysRegs)
   rs3_addr := CalcVecRegAddr(
      io.iss_uops(0).rs3_vew,
      io.iss_uops(0).eidx,
      io.iss_uops(0).pop3,
      numVecPhysRegs)

   io.rf_read_ports(0).addr := rs1_addr
   io.rf_read_ports(1).addr := rs2_addr
   io.rf_read_ports(2).addr := rs3_addr

   io.rf_read_ports(0).enable := io.iss_uops(0).lrs1_rtype === RT_VEC
   io.rf_read_ports(1).enable := io.iss_uops(0).lrs2_rtype === RT_VEC
   io.rf_read_ports(2).enable := io.iss_uops(0).lrs3_rtype === RT_VEC

   rrd_rs1_data := io.rf_read_ports(0).data
   rrd_rs2_data := io.rf_read_ports(1).data
   rrd_rs3_data := io.rf_read_ports(2).data

   val rrd_kill = Mux(io.kill, Bool(true),
      Mux(io.brinfo.valid && io.brinfo.mispredict
         , maskMatch(rrd_uops.br_mask, io.brinfo.mask)
         , Bool(false)))

   exe_reg_valids := Mux(rrd_kill, Bool(false), rrd_valids)
   // TODO use only the valids signal, don't require us to set nullUop
   exe_reg_uops   := Mux(rrd_kill, NullMicroOp, rrd_uops)

   exe_reg_uops.br_mask := GetNewBrMask(io.brinfo, rrd_uops)





   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Execute Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------


   exe_reg_rs1_data := rrd_rs1_data
   exe_reg_rs2_data := rrd_rs2_data
   exe_reg_rs3_data := rrd_rs3_data
   exe_reg_rp_data  := rrd_rp_data

   //-------------------------------------------------------------
   // set outputs to execute pipelines


   io.exe_reqs(0).valid    := exe_reg_valids
   io.exe_reqs(0).bits.uop := exe_reg_uops

   io.exe_reqs(0).bits.rs1_data := exe_reg_rs1_data
   io.exe_reqs(0).bits.rs2_data := exe_reg_rs2_data
   io.exe_reqs(0).bits.rs3_data := exe_reg_rs3_data


}
