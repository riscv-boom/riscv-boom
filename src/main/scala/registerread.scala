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
                  , num_total_read_ports: Int
                  , num_read_ports_array: ArrayBuffer[Int]
                                             // each exe_unit must tell us how many max
                                             // operands it can accept (the sum should equal
                                             // num_total_read_ports)
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
      val rf_read_ports = Vec.fill(num_total_read_ports) { new RegisterFileReadPortIO(PREG_SZ, register_width) }.flip

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
   val exe_reg_rs3_data = Vec.fill(issue_width) { Reg(outType = Bits(width = register_width))  }


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

   require (num_total_read_ports == num_read_ports_array.reduce(_+_))

   val rrd_rs1_data   = Vec.fill(issue_width) { Bits() }
   val rrd_rs2_data   = Vec.fill(issue_width) { Bits() }
   val rrd_rs3_data   = Vec.fill(issue_width) { Bits() }

   var idx = 0 // index into flattened read_ports array
   for (w <- 0 until issue_width)
   {
      val num_read_ports = num_read_ports_array(w)
      println ("Max Operands: " + num_read_ports)

      val rs1_addr = rrd_uops(w).pop1
      val rs2_addr = rrd_uops(w).pop2
      val rs3_addr = rrd_uops(w).pop3

      if (num_read_ports > 0) io.rf_read_ports(idx+0).addr := rs1_addr
      if (num_read_ports > 1) io.rf_read_ports(idx+1).addr := rs2_addr
      if (num_read_ports > 2) io.rf_read_ports(idx+2).addr := rs3_addr

      if (num_read_ports > 0) rrd_rs1_data(w) := io.rf_read_ports(idx+0).data
      if (num_read_ports > 1) rrd_rs2_data(w) := io.rf_read_ports(idx+1).data
      if (num_read_ports > 2) rrd_rs3_data(w) := io.rf_read_ports(idx+2).data

      val rrd_kill = Mux(io.kill,       Bool(true),
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

   val bypassed_rs1_data = Vec.fill(issue_width) { Bits(width = register_width) }
   val bypassed_rs2_data = Vec.fill(issue_width) { Bits(width = register_width) }

//   if (ENABLE_BYPASSING_NETWORK)
//   if (true)
//   {
      for (w <- 0 until issue_width)
      {
         val num_read_ports = num_read_ports_array(w)
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

         if (num_read_ports > 0) bypassed_rs1_data(w) := MuxCase(rrd_rs1_data(w), rs1_cases)
         if (num_read_ports > 1) bypassed_rs2_data(w) := MuxCase(rrd_rs2_data(w), rs2_cases)
      }
//   }
//   else
//   {
//      bypassed_rs1_data := rrd_rs1_data
//      bypassed_rs2_data := rrd_rs2_data
//   }




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
                             // |      |  use muldiv pipe        |         |         immsel       csr_cmd
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

               uopCSRRW -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_I, REN_1, rocket.CSR.W),
               uopCSRRS -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_I, REN_1, rocket.CSR.S),
               uopCSRRC -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_I, REN_1, rocket.CSR.C),

               uopCSRRWI-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_1, rocket.CSR.W),
               uopCSRRSI-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_1, rocket.CSR.S),
               uopCSRRCI-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_1, rocket.CSR.C),

               uopSYSTEM-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_0, rocket.CSR.I),

               // floating-point
               uopFCLASS_S->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCLASS_D->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

               uopFMV_S_X->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMV_D_X->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMV_X_S->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMV_X_D->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFSGNJ_S->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFSGNJ_D->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

               uopFCVT_S_D ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_D_S ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

               uopFCVT_S_W ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_S_WU->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_S_L ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_S_LU->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_D_W ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_D_WU->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_D_L ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_D_LU->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

               uopFCVT_W_S ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_WU_S->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_L_S ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_LU_S->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_W_D ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_WU_D->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_L_D ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFCVT_LU_D->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

               uopFEQ_S   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFLT_S   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFLE_S   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFEQ_D   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFLT_D   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFLE_D   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

               uopFMIN_S  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMAX_S  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMIN_D  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMAX_D  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

               uopFADD_S  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFSUB_S  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMUL_S  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFADD_D  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFSUB_D  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMUL_D  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

               uopFMADD_S ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMSUB_S ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFNMADD_S->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFNMSUB_S->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMADD_D ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFMSUB_D ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFNMADD_D->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
               uopFNMSUB_D->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N)
               ))

   val rrd_br_type :: rrd_use_alupipe :: rrd_use_muldivpipe :: rrd_use_mempipe :: rrd_op_fcn :: rrd_fcn_dw :: rrd_op1_sel :: rrd_op2_sel :: rrd_imm_sel :: (rrd_rf_wen: Bool) :: rrd_csr_cmd :: Nil = rrd_csignals;

   require (rrd_op_fcn.getWidth == FN_SRA.getWidth)


   // rrd_use_alupipe is unused
   io.rrd_uop.ctrl.br_type := rrd_br_type
   io.rrd_uop.ctrl.rf_wen  := rrd_rf_wen
   io.rrd_uop.ctrl.op1_sel := rrd_op1_sel
   io.rrd_uop.ctrl.op2_sel := rrd_op2_sel
   io.rrd_uop.ctrl.imm_sel := rrd_imm_sel
   io.rrd_uop.ctrl.op_fcn  := rrd_op_fcn.toBits
   io.rrd_uop.ctrl.fcn_dw  := rrd_fcn_dw.toBool
   io.rrd_uop.ctrl.is_load := io.rrd_uop.uopc === uopLD
   io.rrd_uop.ctrl.is_sta  := io.rrd_uop.uopc === uopSTA || io.rrd_uop.uopc === uopAMO_AG
   io.rrd_uop.ctrl.is_std  := io.rrd_uop.uopc === uopSTD

   val raddr1 = io.rrd_uop.pop1 // although renamed, it'll stay 0 if lrs1 = 0
   val csr_ren = (rrd_csr_cmd === rocket.CSR.S || rrd_csr_cmd === rocket.CSR.C) && raddr1 === UInt(0)
   io.rrd_uop.ctrl.csr_cmd := Mux(csr_ren, rocket.CSR.R, rrd_csr_cmd)

   //-------------------------------------------------------------
   // set outputs

   io.rrd_valid := rrd_valid

}


}
