//**************************************************************************
// RISCV Processor Register Read
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2012 Apr 29

// Handle the register read and bypass network for the OoO backend
// interfaces with the issue window on the enqueue side, and the execution
// pipelines on the dequeue side


package BOOM
{

import Chisel._
import Node._
import scala.collection.mutable.ArrayBuffer

import rocket.ALU._


//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

class RegisterRead(issue_width: Int
                  , num_read_ports: Int
                  , num_total_bypass_ports: Int
                  , register_width: Int
                  ) extends Module with BOOMCoreParameters
{
   val io = new BOOMCoreBundle
   {
      // issued micro-ops
      val iss_valids = Vec.fill(issue_width) { Bool(INPUT) }
      val iss_uops   = Vec.fill(issue_width) { new MicroOp().asInput() }

      // interface with register file's read ports
      val rf_read_ports = Vec.fill(num_read_ports) { new RegisterFileReadPortIO(PREG_SZ, register_width) }.flip

      val bypass = new BypassData(num_total_bypass_ports, register_width).asInput()

      // send micro-ops to the execution pipelines
      val exe_reqs = Vec.fill(issue_width) { (new DecoupledIO(new FuncUnitReq(register_width))) }


      val kill   = Bool(INPUT)
      val brinfo = new BrResolutionInfo().asInput
   }


   val rrd_valids       = Vec.fill(issue_width) { Bool() }
   val rrd_uops         = Vec.fill(issue_width) { new MicroOp() }

   val exe_reg_valids   = Vec.fill(issue_width) { Reg(init = Bool(false)) }
   val exe_reg_uops     = Vec.fill(issue_width) { Reg(outType = new MicroOp())  }
   val exe_reg_rs1_data = Vec.fill(issue_width) { Reg(outType = Bits(width = register_width))  }
   val exe_reg_rs2_data = Vec.fill(issue_width) { Reg(outType = Bits(width = register_width))  }


   //-------------------------------------------------------------
   // hook up inputs

   for (w <- 0 until issue_width)
   {
      val rrd_decode_unit = Module(new RegisterReadDecode)
      rrd_decode_unit.io.iss_valid := io.iss_valids(w)
      rrd_decode_unit.io.iss_uop   := io.iss_uops(w)

      rrd_valids(w) := rrd_decode_unit.io.rrd_valid
      rrd_uops(w)   := rrd_decode_unit.io.rrd_uop
   }



   //-------------------------------------------------------------
   // read ports


   val rrd_rs1_data   = Vec.fill(issue_width) { Bits() }
   val rrd_rs2_data   = Vec.fill(issue_width) { Bits() }

   for (w <- 0 until issue_width)
   {
      val i = w*2
      val rs1_addr = rrd_uops(w).pop1
      val rs2_addr = rrd_uops(w).pop2

      // TODO allow for execute pipelines to only use one register read port
      io.rf_read_ports(i+0).addr := rs1_addr
      io.rf_read_ports(i+1).addr := rs2_addr

      rrd_rs1_data(w) := io.rf_read_ports(i+0).data
      rrd_rs2_data(w) := io.rf_read_ports(i+1).data

      val rrd_kill = Mux(io.kill,       Bool(true),
                     Mux(io.brinfo.valid && io.brinfo.mispredict
                                       , maskMatch(rrd_uops(w).br_mask, io.brinfo.mask)
                                       , Bool(false)))

      exe_reg_valids(w) := Mux(rrd_kill, Bool(false), rrd_valids(w))
      // TODO use only the valids signal, don't require us to set nullUop
      exe_reg_uops(w)   := Mux(rrd_kill, NullMicroOp, rrd_uops(w))

      exe_reg_uops(w).br_mask := GetNewBrMask(io.brinfo, rrd_uops(w))
   }


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // BYPASS MUXES -----------------------------------------------
   // performed at the end of the register read stage

   val bypassed_rs1_data = Vec.fill(issue_width) { Bits(width = register_width) }
   val bypassed_rs2_data = Vec.fill(issue_width) { Bits(width = register_width) }

//   if (ENABLE_BYPASSING_NETWORK)
   if (true)
   {
      for (w <- 0 until issue_width)
      {
         var rs1_cases = Array((Bool(false), Bits(0, register_width)))
         var rs2_cases = Array((Bool(false), Bits(0, register_width)))

         val pop1       = rrd_uops(w).pop1
         val lrs1_rtype = rrd_uops(w).lrs1_rtype
         val pop2       = rrd_uops(w).pop2
         val lrs2_rtype = rrd_uops(w).lrs2_rtype

         for (b <- 0 until io.bypass.get_num_ports)
         {
            // can't use "io.bypass.valid(b) since it would create a combinational loop on branch kills"
            rs1_cases ++= Array((io.bypass.valid(b) && (pop1 === io.bypass.uop(b).pdst) && io.bypass.uop(b).ctrl.rf_wen && (lrs1_rtype === RT_FIX || lrs1_rtype === RT_FLT) && (pop1 != UInt(0)), io.bypass.data(b)))
            rs2_cases ++= Array((io.bypass.valid(b) && (pop2 === io.bypass.uop(b).pdst) && io.bypass.uop(b).ctrl.rf_wen && (lrs2_rtype === RT_FIX || lrs2_rtype === RT_FLT) && (pop2 != UInt(0)), io.bypass.data(b)))
         }

         bypassed_rs1_data(w) := MuxCase(rrd_rs1_data(w), rs1_cases)
         bypassed_rs2_data(w) := MuxCase(rrd_rs2_data(w), rs2_cases)
      }
   }
   else
   {
      bypassed_rs1_data := rrd_rs1_data
      bypassed_rs2_data := rrd_rs2_data
   }




   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Execute Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   exe_reg_rs1_data := bypassed_rs1_data
   exe_reg_rs2_data := bypassed_rs2_data


   //-------------------------------------------------------------
   // set outputs to execute pipelines
   for (w <- 0 until issue_width)
   {
      io.exe_reqs(w).valid    := exe_reg_valids(w)
      io.exe_reqs(w).bits.uop := exe_reg_uops(w)
      io.exe_reqs(w).bits.rs1_data := exe_reg_rs1_data(w)
      io.exe_reqs(w).bits.rs2_data := exe_reg_rs2_data(w)
   }


}

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

class RegisterReadDecode extends Module
{
   val io = new BOOMCoreBundle
   {
      val iss_valid = Bool(INPUT)
      val iss_uop   = new MicroOp().asInput()

      val rrd_valid = Bool(OUTPUT)
      val rrd_uop   = new MicroOp().asOutput()
   }

   // Issued Instruction
   val rrd_valid = io.iss_valid
   io.rrd_uop   := io.iss_uop


                             // br type
                             // |      use alu pipe              op1 sel   op2 sel
                             // |      |  use muldiv pipe        |         |         immsel       pcr fcn
                             // |      |  |  use mem pipe        |         |         |     rf wen |
   val rrd_csignals =        // |      |  |  |  alu fcn  wd/word?|         |         |     |      |
      rocket.DecodeLogic(    // |      |  |  |  |        |       |         |         |     |      |
                 io.rrd_uop.uopc,//    |  |  |  |        |       |         |         |     |      |
                           List(BR_N , Y, N, N, FN_ADD , DW_X  , OP1_X   , OP2_X   , IS_X, REN_0, rocket.CSR.N),
            Array(
               uopLD    -> List(BR_N , N, N, Y, FN_ADD , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_0, rocket.CSR.N),
               uopSTA   -> List(BR_N , N, N, Y, FN_ADD , DW_XPR, OP1_RS1 , OP2_IMM , IS_S, REN_0, rocket.CSR.N),
               uopSTD   -> List(BR_N , N, N, Y, FN_X   , DW_X  , OP1_RS1 , OP2_RS2 , IS_X, REN_0, rocket.CSR.N),

               uopAMO_AG-> List(BR_N , N, N, Y, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_X, REN_0, rocket.CSR.N),

               uopLUI   -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMM , IS_U, REN_1, rocket.CSR.N),

               uopADDI  -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
               uopANDI  -> List(BR_N , Y, N, N, FN_AND , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
               uopORI   -> List(BR_N , Y, N, N, FN_OR  , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
               uopXORI  -> List(BR_N , Y, N, N, FN_XOR , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
               uopSLTI  -> List(BR_N , Y, N, N, FN_SLT , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
               uopSLTIU -> List(BR_N , Y, N, N, FN_SLTU, DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
               uopSLLI  -> List(BR_N , Y, N, N, FN_SL  , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
               uopSRAI  -> List(BR_N , Y, N, N, FN_SRA , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
               uopSRLI  -> List(BR_N , Y, N, N, FN_SR  , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),

               uopADDIW -> List(BR_N , Y, N, N, FN_ADD , DW_32 , OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
               uopSLLIW -> List(BR_N , Y, N, N, FN_SL  , DW_32 , OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
               uopSRAIW -> List(BR_N , Y, N, N, FN_SRA , DW_32 , OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
               uopSRLIW -> List(BR_N , Y, N, N, FN_SR  , DW_32 , OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),

               uopADD   -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopSLL   -> List(BR_N , Y, N, N, FN_SL  , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopSUB   -> List(BR_N , Y, N, N, FN_SUB , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopSLT   -> List(BR_N , Y, N, N, FN_SLT , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopSLTU  -> List(BR_N , Y, N, N, FN_SLTU, DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopAND   -> List(BR_N , Y, N, N, FN_AND , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopOR    -> List(BR_N , Y, N, N, FN_OR  , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopXOR   -> List(BR_N , Y, N, N, FN_XOR , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopSRA   -> List(BR_N , Y, N, N, FN_SRA , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopSRL   -> List(BR_N , Y, N, N, FN_SR  , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),

               uopADDW  -> List(BR_N , Y, N, N, FN_ADD , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopSUBW  -> List(BR_N , Y, N, N, FN_SUB , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopSLLW  -> List(BR_N , Y, N, N, FN_SL  , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopSRAW  -> List(BR_N , Y, N, N, FN_SRA , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopSRLW  -> List(BR_N , Y, N, N, FN_SR  , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),

               uopMUL   -> List(BR_N , N, Y, N, FN_MUL,   DW_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,rocket.CSR.N),
               uopMULH  -> List(BR_N , N, Y, N, FN_MULH,  DW_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,rocket.CSR.N),
               uopMULHU -> List(BR_N , N, Y, N, FN_MULHU, DW_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,rocket.CSR.N),
               uopMULHSU-> List(BR_N , N, Y, N, FN_MULHSU,DW_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,rocket.CSR.N),
               uopMULW  -> List(BR_N , N, Y, N, FN_MUL,   DW_32 ,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,rocket.CSR.N),

               uopDIV   -> List(BR_N , N, Y, N, FN_DIV , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopDIVU  -> List(BR_N , N, Y, N, FN_DIVU, DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopREM   -> List(BR_N , N, Y, N, FN_REM , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopREMU  -> List(BR_N , N, Y, N, FN_REMU, DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopDIVW  -> List(BR_N , N, Y, N, FN_DIV , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopDIVUW -> List(BR_N , N, Y, N, FN_DIVU, DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopREMW  -> List(BR_N , N, Y, N, FN_REM , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
               uopREMUW -> List(BR_N , N, Y, N, FN_REMU, DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),

               uopBEQ   -> List(BR_EQ ,Y, N, N, FN_SUB , DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, rocket.CSR.N),
               uopBNE   -> List(BR_NE ,Y, N, N, FN_SUB , DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, rocket.CSR.N),
               uopBGE   -> List(BR_GE ,Y, N, N, FN_SLT , DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, rocket.CSR.N),
               uopBGEU  -> List(BR_GEU,Y, N, N, FN_SLTU, DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, rocket.CSR.N),
               uopBLT   -> List(BR_LT ,Y, N, N, FN_SLT , DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, rocket.CSR.N),
               uopBLTU  -> List(BR_LTU,Y, N, N, FN_SLTU, DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, rocket.CSR.N),

               uopJAL   -> List(BR_J , Y, N, N, FN_ADD , DW_XPR, OP1_PC  , OP2_FOUR, IS_J, REN_1, rocket.CSR.N),
               uopJALR  -> List(BR_JR, Y, N, N, FN_ADD , DW_XPR, OP1_PC  , OP2_FOUR, IS_I, REN_1, rocket.CSR.N),
               uopAUIPC -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_PC  , OP2_IMM , IS_U, REN_1, rocket.CSR.N),

               uopCSRRW -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_X, REN_1, rocket.CSR.W),
               uopCSRRS -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_X, REN_1, rocket.CSR.S),
               uopCSRRC -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_X, REN_1, rocket.CSR.C),

               uopCSRRWI-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_X, REN_1, rocket.CSR.W),
               uopCSRRSI-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_X, REN_1, rocket.CSR.S),
               uopCSRRCI-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_X, REN_1, rocket.CSR.C),

               // floating-point
               uopFMV_S_X->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMV_D_X->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMV_X_S->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMV_X_D->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFSGNJ_S->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFSGNJ_D->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N)
               ));

   val rrd_br_type :: rrd_use_alupipe :: rrd_use_muldivpipe :: rrd_use_mempipe :: rrd_op_fcn :: rrd_fcn_dw :: rrd_op1_sel :: rrd_op2_sel :: rrd_imm_sel :: (rrd_rf_wen: Bool) :: rrd_pcr_fcn :: Nil = rrd_csignals;

   require (rrd_op_fcn.getWidth == FN_SRA.getWidth)


   // rrd_use_alupipe is unused
   io.rrd_uop.ctrl.br_type := rrd_br_type
   io.rrd_uop.ctrl.rf_wen  := rrd_rf_wen
   io.rrd_uop.ctrl.op1_sel := rrd_op1_sel
   io.rrd_uop.ctrl.op2_sel := rrd_op2_sel
   io.rrd_uop.ctrl.imm_sel := rrd_imm_sel
   io.rrd_uop.ctrl.op_fcn  := rrd_op_fcn.toBits
   io.rrd_uop.ctrl.fcn_dw  := rrd_fcn_dw.toBool
   io.rrd_uop.ctrl.pcr_fcn := rrd_pcr_fcn
   io.rrd_uop.ctrl.is_load := io.rrd_uop.uopc === uopLD
   io.rrd_uop.ctrl.is_sta  := io.rrd_uop.uopc === uopSTA || io.rrd_uop.uopc === uopAMO_AG
   io.rrd_uop.ctrl.is_std  := io.rrd_uop.uopc === uopSTD


   //-------------------------------------------------------------
   // set outputs

   io.rrd_valid := rrd_valid

}


}
