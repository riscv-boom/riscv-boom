//**************************************************************************
// Execution Units
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2013 Apr 27
//
// The issue window schedules micro-ops onto a specific execution pipeline
// A given execution pipeline may contain multiple functional units; one or two
// read ports, and one or more writeports.


package BOOM
{

import Chisel._
import Node._

import FUCode._
import uncore.constants.MemoryOpConstants._
 
class ExeUnitResp extends Bundle()
{
   val uop = new MicroOp()
   val data = Bits(width = XPRLEN)  
   // TODO allow for muliple write ports?
}
 
class ExecutionUnitIo(num_rf_read_ports: Int, num_rf_write_ports: Int, num_bypass_ports: Int)(implicit conf: BOOMConfiguration) extends Bundle()
{
   // describe which functional units we support (used by the issue window)
   val fu_types = Bits(OUTPUT, FUC_SZ)

   val req     = (new DecoupledIO(new FuncUnitReq)).flip
   val resp    = Vec.fill(num_rf_write_ports) { (new DecoupledIO(new ExeUnitResp)) }
   val bypass  = new BypassData(num_bypass_ports).asOutput()

   val brinfo  = new BrResolutionInfo().asInput()
   
   // only used by the branch unit
   val br_unit = new BranchUnitResp().asOutput()
   val get_rob_pc = new Bundle 
   {
      val rob_idx = UInt(OUTPUT, ROB_ADDR_SZ) 
      val curr_pc = UInt(INPUT, XPRLEN)
      val next_val= Bool(INPUT)
      val next_pc = UInt(INPUT, XPRLEN)
   }

   // only used by the mem unit
   val lsu_io = new LoadStoreUnitIo(DECODE_WIDTH)
   val dmem   = new DCMemPortIo()(conf.rc.dcache)
   val com_handling_exc = Bool(INPUT)
   val ma_xcpt_val = Bool(OUTPUT)
   val ma_xcpt     = (new rocket.HellaCacheExceptions).asOutput
   val ma_xcpt_uop = new MicroOp().asOutput
}
 
abstract class ExecutionUnit(val num_rf_read_ports: Int, val num_rf_write_ports: Int, val num_bypass_stages: Int, var bypassable: Boolean = false, val is_mem_unit: Boolean = false, var uses_pcr_wport: Boolean = false, is_branch_unit: Boolean = false) (implicit conf: BOOMConfiguration) extends Module
{
   val io = new ExecutionUnitIo(num_rf_read_ports, num_rf_write_ports, num_bypass_stages)

   val uses_rf_wport = false

   if (!is_mem_unit)
   {
      io.ma_xcpt_val := Bool(false)
   }


   def get_num_bypass_ports: Int = num_bypass_stages
   def has_branch_unit     : Boolean = is_branch_unit
   def is_bypassable       : Boolean = bypassable
}


class ALUExeUnit(is_branch_unit: Boolean = false, shares_pcr_wport: Boolean = false)
                                       (implicit conf: BOOMConfiguration) 
                                       extends ExecutionUnit(num_rf_read_ports = 2
                                                            , num_rf_write_ports = 1
                                                            , num_bypass_stages = 2
                                                            , bypassable = true && ENABLE_ALU_BYPASSING
                                                            , is_mem_unit = false
                                                            , uses_pcr_wport = shares_pcr_wport
                                                            , is_branch_unit = is_branch_unit)
{
   io.fu_types := FU_ALU |
                  FU_CNTR |
                  (Mux(Bool(shares_pcr_wport), FU_PCR, Bits(0))) |
                  (Mux(Bool(is_branch_unit), FU_BRU, Bits(0)))


   val alu = Module(new ALUUnit(is_branch_unit = is_branch_unit))
   alu.io.req <> io.req
   io.resp(0) <> alu.io.resp

   alu.io.brinfo <> io.brinfo
   io.bypass <> alu.io.bypass

   // branch unit is embedded inside the ALU
   if (is_branch_unit)
   {
      io.br_unit <> alu.io.br_unit
      alu.io.get_rob_pc <> io.get_rob_pc
   }
   else
   {
      io.br_unit.brinfo.valid := Bool(false)
   }

   
}
 
 
class MulDExeUnit()(implicit conf: BOOMConfiguration) extends ExecutionUnit(num_rf_read_ports = 2, num_rf_write_ports = 1, num_bypass_stages = 0)
{
   val muldiv_busy = Bool()
   io.fu_types := Mux(!muldiv_busy, FU_MULD, Bits(0))

   val muldiv = Module(new MulDivUnit())
   muldiv.io.req <> io.req
   
   io.resp(0) <> muldiv.io.resp
   io.resp(0).ready := Bool(true)
   
   muldiv.io.brinfo <> io.brinfo
   io.bypass <> muldiv.io.bypass 

   muldiv_busy := !muldiv.io.req.ready || (io.req.valid) 
}
// TODO listed as FIFOs, but not using ready signal


class ALUMulDExeUnit(is_branch_unit: Boolean = false, shares_pcr_wport: Boolean = false)
                                       (implicit conf: BOOMConfiguration) 
                                       extends ExecutionUnit(num_rf_read_ports = 2, num_rf_write_ports = 1, num_bypass_stages = 2, bypassable = true && ENABLE_ALU_BYPASSING, is_mem_unit = false, uses_pcr_wport = shares_pcr_wport, is_branch_unit = is_branch_unit)
{
   val muldiv_busy = Bool()
   io.fu_types := (FU_ALU |
                  FU_CNTR | 
                  (Mux(!muldiv_busy, FU_MULD, Bits(0))) | 
                  (Mux(Bool(shares_pcr_wport), FU_PCR, Bits(0))) |
                  (Mux(Bool(is_branch_unit), FU_BRU, Bits(0))))


   // ALU Unit -------------------------------
   val alu = Module(new ALUUnit(is_branch_unit = is_branch_unit))
   alu.io.req.valid         := io.req.valid &&
                                    ((io.req.bits.uop.fu_code === FU_ALU) ||
                                     (io.req.bits.uop.fu_code === FU_BRU) ||
                                     (io.req.bits.uop.fu_code === FU_PCR) ||
                                     (io.req.bits.uop.fu_code === FU_CNTR))
   alu.io.req.bits.uop      := io.req.bits.uop
   alu.io.req.bits.kill     := io.req.bits.kill
   alu.io.req.bits.rs1_data := io.req.bits.rs1_data
   alu.io.req.bits.rs2_data := io.req.bits.rs2_data
 
   // branch unit is embedded inside the ALU
   if (is_branch_unit)
   {
      io.br_unit <> alu.io.br_unit
      alu.io.get_rob_pc <> io.get_rob_pc
   }
   else
   {
      io.br_unit.brinfo.valid := Bool(false)
   }


   // Mul/Div/Rem Unit -----------------------
   val muldiv = Module(new MulDivUnit())

   muldiv.io.req.valid           := io.req.valid && 
                                    (io.req.bits.uop.fu_code === FU_MULD)
   muldiv.io.req.bits.uop        := io.req.bits.uop
   muldiv.io.req.bits.rs1_data   := io.req.bits.rs1_data
   muldiv.io.req.bits.rs2_data   := io.req.bits.rs2_data
   muldiv.io.brinfo              := io.brinfo
   muldiv.io.req.bits.kill       := io.req.bits.kill

   muldiv.io.resp.ready := !alu.io.resp.valid // share write port with the ALU

   muldiv_busy := !muldiv.io.req.ready || (io.req.valid && io.req.bits.uop.fu_code === FU_MULD) 

   // Branch Resolution ------------------------

   alu.io.brinfo <> io.brinfo
   muldiv.io.brinfo <> io.brinfo

   // Bypassing --------------------------------
   // (only the ALU is bypassable)
   
   io.bypass <> alu.io.bypass

   // Outputs ----------------------------------
   // hook up responses

   io.resp(0).valid := alu.io.resp.valid || muldiv.io.resp.valid
   io.resp(0).bits.uop         := Mux(alu.io.resp.valid, alu.io.resp.bits.uop,
                                                         muldiv.io.resp.bits.uop)
   io.resp(0).bits.data        := Mux(alu.io.resp.valid, alu.io.resp.bits.data,
                                                         muldiv.io.resp.bits.data)
 
}


 

class MemExeUnit()(implicit conf: BOOMConfiguration) extends ExecutionUnit(num_rf_read_ports = 2, num_rf_write_ports = 1, num_bypass_stages = 0, bypassable = false, is_mem_unit = true)
{
   io.fu_types := FU_MEM

   // Perform address calculation
   val maddrcalc = Module(new MemAddrCalcUnit())
   maddrcalc.io.req <> io.req
   
   maddrcalc.io.brinfo <> io.brinfo 
   io.bypass <> maddrcalc.io.bypass  // TODO this is not where the bypassing should occur from
 
   io.ma_xcpt_val := maddrcalc.io.resp.bits.xcpt.toBits != Bits(0) && maddrcalc.io.resp.valid
   io.ma_xcpt     := maddrcalc.io.resp.bits.xcpt
   io.ma_xcpt_uop := maddrcalc.io.resp.bits.uop
    
   val lsu = Module(new LoadStoreUnit(DECODE_WIDTH))
 
   // TODO does this interface have to be so verbose? for the LSU connections
   lsu.io.dec_st_vals       := io.lsu_io.dec_st_vals
   lsu.io.dec_ld_vals       := io.lsu_io.dec_ld_vals
   lsu.io.dec_uops          := io.lsu_io.dec_uops

   
   lsu.io.commit_store_mask := io.lsu_io.commit_store_mask
   lsu.io.commit_load_mask  := io.lsu_io.commit_load_mask
    
   lsu.io.brinfo            := io.brinfo
   lsu.io.lsu_misspec       := io.lsu_io.lsu_misspec 
   lsu.io.exception         := io.lsu_io.exception
   lsu.io.nack              <> io.dmem.nack
   lsu.io.counters          <> io.lsu_io.counters
          
   io.lsu_io.new_ldq_idx := lsu.io.new_ldq_idx
   io.lsu_io.new_stq_idx := lsu.io.new_stq_idx
   io.lsu_io.laq_full := lsu.io.laq_full
   io.lsu_io.laq_empty := lsu.io.laq_empty
   io.lsu_io.stq_full := lsu.io.stq_full
   io.lsu_io.stq_empty := lsu.io.stq_empty
   io.lsu_io.lsu_clr_bsy_valid := lsu.io.lsu_clr_bsy_valid // HACK TODO need a better way to clear the busy bits in the ROB
   io.lsu_io.lsu_clr_bsy_rob_idx := lsu.io.lsu_clr_bsy_rob_idx // HACK TODO need a better way to clear the busy bits in the rob
   io.lsu_io.lsu_fencei_rdy := lsu.io.lsu_fencei_rdy 
   io.lsu_io.ldo_xcpt_val := lsu.io.ldo_xcpt_val
   io.lsu_io.ldo_xcpt_uop := lsu.io.ldo_xcpt_uop
   io.lsu_io.debug := lsu.io.debug

   // enqueue addresses,st-data at the end of Execute
   lsu.io.exe_resp <> maddrcalc.io.resp
      
   // HellaCache Req 
   lsu.io.dmem_req_ready := io.dmem.req.ready
   lsu.io.dmem_is_ordered:= io.dmem.ordered

   // TODO get rid of com_handling and guard with an assert?
   io.dmem.req.valid     := Mux(io.com_handling_exc && lsu.io.memreq_uop.is_load, Bool(false), 
                                                                              lsu.io.memreq_val)
   io.dmem.req.bits.addr  := lsu.io.memreq_addr
   io.dmem.req.bits.data  := lsu.io.memreq_wdata
   io.dmem.req.bits.uop   := lsu.io.memreq_uop
   io.dmem.req.bits.kill  := lsu.io.memreq_kill // load kill request sent to memory
   
   // I should be timing forwarding to coincide with dmem resps, so I'm not clobbering
   //anything....
   val memresp_val    = Mux(io.com_handling_exc && io.dmem.resp.bits.uop.is_load, Bool(false), 
                                                lsu.io.forward_val || io.dmem.resp.valid)

   val memresp_rf_wen = (io.dmem.resp.valid && (io.dmem.resp.bits.uop.mem_cmd === M_XRD)) || 
                           lsu.io.forward_val
   val memresp_uop    = Mux(lsu.io.forward_val, lsu.io.forward_uop,
                                                io.dmem.resp.bits.uop)
   val memresp_data   = Mux(lsu.io.forward_val, lsu.io.forward_data, 
                                                io.dmem.resp.bits.data)

   lsu.io.memresp_val := memresp_val
   lsu.io.memresp_uop := memresp_uop
   

   // Hook up loads to the response
   io.resp(0).valid := memresp_val
   io.resp(0).bits.uop := memresp_uop
   io.resp(0).bits.uop.pdst_rtype := RT_FIX
   io.resp(0).bits.uop.ctrl.rf_wen := memresp_rf_wen
   io.resp(0).bits.data := memresp_data

}
   
 
class ALUMulDMemExeUnit(is_branch_unit: Boolean = false, shares_pcr_wport: Boolean = false)(implicit conf: BOOMConfiguration) extends ExecutionUnit(num_rf_read_ports = 2, num_rf_write_ports = 2, num_bypass_stages = 2, bypassable = true && ENABLE_ALU_BYPASSING, is_mem_unit = true, uses_pcr_wport = shares_pcr_wport, is_branch_unit = is_branch_unit)
{
   val muldiv_busy = Bool()
   io.fu_types := (FU_ALU |
                  FU_CNTR | 
                  FU_MEM |
                  (Mux(!muldiv_busy, FU_MULD, Bits(0))) | 
                  (Mux(Bool(shares_pcr_wport), FU_PCR, Bits(0))) |
                  (Mux(Bool(is_branch_unit), FU_BRU, Bits(0))))

    
   val memresp_val = Bool()


   // ALU Unit -------------------------------
   val alu = Module(new ALUUnit(is_branch_unit = true))
   alu.io.req.valid         := io.req.valid &&
                                    ((io.req.bits.uop.fu_code === FU_ALU) ||
                                     (io.req.bits.uop.fu_code === FU_BRU) ||
                                     (io.req.bits.uop.fu_code === FU_PCR) ||
                                     (io.req.bits.uop.fu_code === FU_CNTR))
   alu.io.req.bits.uop      := io.req.bits.uop
   alu.io.req.bits.kill     := io.req.bits.kill
   alu.io.req.bits.rs1_data := io.req.bits.rs1_data
   alu.io.req.bits.rs2_data := io.req.bits.rs2_data
 
   // branch unit is embedded inside the ALU
   if (is_branch_unit)
   {
      io.br_unit <> alu.io.br_unit
      alu.io.get_rob_pc <> io.get_rob_pc
   }
   else
   {
      io.br_unit.brinfo.valid := Bool(false)
   }
 
   // Outputs ----------------------------------
   // hook up responses

   io.resp(0).ready := Bool(true) // did I make the -wi happy?
   io.resp(0) <> alu.io.resp
 

   // Mul/Div/Rem Unit -----------------------
   val muldiv = Module(new MulDivUnit())

   muldiv.io.req.valid           := io.req.valid && 
                                    (io.req.bits.uop.fu_code === FU_MULD)
   muldiv.io.req.bits.uop        := io.req.bits.uop
   muldiv.io.req.bits.rs1_data   := io.req.bits.rs1_data
   muldiv.io.req.bits.rs2_data   := io.req.bits.rs2_data
   muldiv.io.brinfo              := io.brinfo
   muldiv.io.req.bits.kill       := io.req.bits.kill

   muldiv.io.resp.ready := !memresp_val //share write port with the memory

   muldiv_busy := !muldiv.io.req.ready || (io.req.valid && io.req.bits.uop.fu_code === FU_MULD) 

   // Branch Resolution ------------------------

   alu.io.brinfo <> io.brinfo
   muldiv.io.brinfo <> io.brinfo

   // Bypassing --------------------------------
   // (only the ALU is bypassable)
   
   io.bypass <> alu.io.bypass

   // Perform address calculation
   val maddrcalc = Module(new MemAddrCalcUnit())
   maddrcalc.io.req <> io.req
   
   maddrcalc.io.brinfo <> io.brinfo 

   io.ma_xcpt_val := maddrcalc.io.resp.bits.xcpt.toBits != Bits(0) && maddrcalc.io.resp.valid
   io.ma_xcpt     := maddrcalc.io.resp.bits.xcpt
   io.ma_xcpt_uop := maddrcalc.io.resp.bits.uop
    
   val lsu = Module(new LoadStoreUnit(DECODE_WIDTH))
 
   lsu.io.dec_st_vals       := io.lsu_io.dec_st_vals
   lsu.io.dec_ld_vals       := io.lsu_io.dec_ld_vals
   lsu.io.dec_uops          := io.lsu_io.dec_uops

   
   lsu.io.commit_store_mask := io.lsu_io.commit_store_mask
   lsu.io.commit_load_mask  := io.lsu_io.commit_load_mask
    
   lsu.io.brinfo            := io.brinfo
   lsu.io.lsu_misspec       := io.lsu_io.lsu_misspec 
   lsu.io.exception         := io.lsu_io.exception
   lsu.io.nack              <> io.dmem.nack
   lsu.io.counters          <> io.lsu_io.counters
          
   io.lsu_io.new_ldq_idx := lsu.io.new_ldq_idx
   io.lsu_io.new_stq_idx := lsu.io.new_stq_idx
   io.lsu_io.laq_full := lsu.io.laq_full
   io.lsu_io.laq_empty := lsu.io.laq_empty
   io.lsu_io.stq_full := lsu.io.stq_full
   io.lsu_io.stq_empty := lsu.io.stq_empty
   io.lsu_io.lsu_clr_bsy_valid := lsu.io.lsu_clr_bsy_valid // HACK TODO need a better way to clear the busy bits in the ROB
   io.lsu_io.lsu_clr_bsy_rob_idx := lsu.io.lsu_clr_bsy_rob_idx // HACK TODO need a better way to clear the busy bits in the rob
   io.lsu_io.lsu_fencei_rdy := lsu.io.lsu_fencei_rdy 
   io.lsu_io.ldo_xcpt_val := lsu.io.ldo_xcpt_val
   io.lsu_io.ldo_xcpt_uop := lsu.io.ldo_xcpt_uop
   io.lsu_io.debug := lsu.io.debug

   // enqueue addresses,st-data at the end of Execute
   lsu.io.exe_resp <> maddrcalc.io.resp
      
   // HellaCache Req 
   lsu.io.dmem_req_ready := io.dmem.req.ready
   lsu.io.dmem_is_ordered:= io.dmem.ordered

   io.dmem.req.valid     := Mux(io.com_handling_exc && lsu.io.memreq_uop.is_load, Bool(false), 
                                                                              lsu.io.memreq_val)
   io.dmem.req.bits.addr  := lsu.io.memreq_addr
   io.dmem.req.bits.data  := lsu.io.memreq_wdata
   io.dmem.req.bits.uop   := lsu.io.memreq_uop
   io.dmem.req.bits.kill  := lsu.io.memreq_kill // load kill request sent to memory
   
   // I should be timing forwarding to coincide with dmem resps, so I'm not clobbering
   //anything....
   memresp_val    := Mux(io.com_handling_exc && io.dmem.resp.bits.uop.is_load, Bool(false), 
                                                lsu.io.forward_val || io.dmem.resp.valid)


   val memresp_rf_wen = (io.dmem.resp.valid && (io.dmem.resp.bits.uop.mem_cmd === M_XRD)) || 
                           lsu.io.forward_val
   val memresp_uop    = Mux(lsu.io.forward_val, lsu.io.forward_uop,
                                                io.dmem.resp.bits.uop)
   val memresp_data   = Mux(lsu.io.forward_val, lsu.io.forward_data, 
                                                io.dmem.resp.bits.data)

   lsu.io.memresp_val   := memresp_val
   lsu.io.memresp_uop   := memresp_uop
   

   // Hook up loads and multiplies to the 2nd write port
   io.resp(1).valid                := memresp_val || muldiv.io.resp.valid
   io.resp(1).bits.uop             := Mux(memresp_val, memresp_uop, muldiv.io.resp.bits.uop)
   io.resp(1).bits.uop.pdst_rtype  := RT_FIX // TODO why is this necessary? shouldn't memresp_uop already be giving us this?
   io.resp(1).bits.uop.ctrl.rf_wen := Mux(memresp_val, memresp_rf_wen, muldiv.io.resp.bits.uop.ctrl.rf_wen)
   io.resp(1).bits.data            := Mux(memresp_val, memresp_data, muldiv.io.resp.bits.data)
 
}
 
}
