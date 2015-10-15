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
// pipelines on the dequeue side


package boom
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
                  )(implicit p: Parameters) extends BoomModule()(p) 
{
   val io = new BoomBundle()(p)
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
      val rrd_decode_unit = Module(new RegisterReadDecode)
      rrd_decode_unit.io.iss_valid := io.iss_valids(w)
      rrd_decode_unit.io.iss_uop   := io.iss_uops(w)

      rrd_valids(w) := rrd_decode_unit.io.rrd_valid
      rrd_uops(w)   := rrd_decode_unit.io.rrd_uop
   }



   //-------------------------------------------------------------
   // read ports

   require (num_total_read_ports == num_read_ports_array.reduce(_+_))

   val rrd_rs1_data   = Wire(Vec(issue_width, Bits()))
   val rrd_rs2_data   = Wire(Vec(issue_width, Bits()))
   val rrd_rs3_data   = Wire(Vec(issue_width, Bits()))

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

         for (b <- 0 until io.bypass.getNumPorts)
         {
            // can't use "io.bypass.valid(b) since it would create a combinational loop on branch kills"
            rs1_cases ++= Array((io.bypass.valid(b) && (pop1 === io.bypass.uop(b).pdst) && io.bypass.uop(b).ctrl.rf_wen && (lrs1_rtype === RT_FIX || lrs1_rtype === RT_FLT) && (pop1 != UInt(0)), io.bypass.data(b)))
            rs2_cases ++= Array((io.bypass.valid(b) && (pop2 === io.bypass.uop(b).pdst) && io.bypass.uop(b).ctrl.rf_wen && (lrs2_rtype === RT_FIX || lrs2_rtype === RT_FLT) && (pop2 != UInt(0)), io.bypass.data(b)))
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

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

class RRdCtrlSigs(implicit p: Parameters) extends BoomBundle()(p)
{
   val br_type          = UInt(width = BR_N.getWidth)
   val use_alupipe      = Bool()
   val use_muldivpipe   = Bool()
   val use_mempipe      = Bool()
   val op_fcn      = Bits(width = SZ_ALU_FN)
   val fcn_dw      = Bool()
   val op1_sel     = UInt(width = OP1_X.getWidth)
   val op2_sel     = UInt(width = OP2_X.getWidth)
   val imm_sel     = UInt(width = IS_X.getWidth)
   val rf_wen      = Bool()
   val csr_cmd     = Bits(width = rocket.CSR.SZ)

   def decode(uopc: UInt, default: List[BitPat], table: Iterable[(BitPat, List[BitPat])]) =
   {
      val decoder = rocket.DecodeLogic(uopc, default, table)
      val sigs = Seq(br_type, use_alupipe, use_muldivpipe, use_mempipe, op_fcn,
                     fcn_dw, op1_sel, op2_sel, imm_sel, rf_wen, csr_cmd)
      sigs zip decoder map {case(s,d) => s := d}
      this
   }
}

class RegisterReadDecode(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new BoomBundle()(p)
   {
      val iss_valid = Bool(INPUT)
      val iss_uop   = new MicroOp().asInput()

      val rrd_valid = Bool(OUTPUT)
      val rrd_uop   = new MicroOp().asOutput()
   }

   // Issued Instruction
   val rrd_valid = io.iss_valid
   io.rrd_uop   := io.iss_uop

   val default: List[BitPat] =
                     List[BitPat](BR_N , Y, N, N, FN_ADD , DW_X  , OP1_X   , OP2_X   , IS_X, REN_0, rocket.CSR.N)
   val table: Array[(BitPat, List[BitPat])] =
              Array[(BitPat, List[BitPat])](
                               // br type
                               // |      use alu pipe              op1 sel   op2 sel
                               // |      |  use muldiv pipe        |         |         immsel       csr_cmd
                               // |      |  |  use mem pipe        |         |         |     rf wen |
                               // |      |  |  |  alu fcn  wd/word?|         |         |     |      |
                               // |      |  |  |  |        |       |         |         |     |      |
         BitPat(uopLD)    -> List(BR_N , N, N, Y, FN_ADD , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_0, rocket.CSR.N),
         BitPat(uopSTA)   -> List(BR_N , N, N, Y, FN_ADD , DW_XPR, OP1_RS1 , OP2_IMM , IS_S, REN_0, rocket.CSR.N),
         BitPat(uopSTD)   -> List(BR_N , N, N, Y, FN_X   , DW_X  , OP1_RS1 , OP2_RS2 , IS_X, REN_0, rocket.CSR.N),

         BitPat(uopAMO_AG)-> List(BR_N , N, N, Y, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_X, REN_0, rocket.CSR.N),

         BitPat(uopLUI)   -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMM , IS_U, REN_1, rocket.CSR.N),

         BitPat(uopADDI)  -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
         BitPat(uopANDI)  -> List(BR_N , Y, N, N, FN_AND , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
         BitPat(uopORI)   -> List(BR_N , Y, N, N, FN_OR  , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
         BitPat(uopXORI)  -> List(BR_N , Y, N, N, FN_XOR , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
         BitPat(uopSLTI)  -> List(BR_N , Y, N, N, FN_SLT , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
         BitPat(uopSLTIU) -> List(BR_N , Y, N, N, FN_SLTU, DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
         BitPat(uopSLLI)  -> List(BR_N , Y, N, N, FN_SL  , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
         BitPat(uopSRAI)  -> List(BR_N , Y, N, N, FN_SRA , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
         BitPat(uopSRLI)  -> List(BR_N , Y, N, N, FN_SR  , DW_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),

         BitPat(uopADDIW) -> List(BR_N , Y, N, N, FN_ADD , DW_32 , OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
         BitPat(uopSLLIW) -> List(BR_N , Y, N, N, FN_SL  , DW_32 , OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
         BitPat(uopSRAIW) -> List(BR_N , Y, N, N, FN_SRA , DW_32 , OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),
         BitPat(uopSRLIW) -> List(BR_N , Y, N, N, FN_SR  , DW_32 , OP1_RS1 , OP2_IMM , IS_I, REN_1, rocket.CSR.N),

         BitPat(uopADD)   -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopSLL)   -> List(BR_N , Y, N, N, FN_SL  , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopSUB)   -> List(BR_N , Y, N, N, FN_SUB , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopSLT)   -> List(BR_N , Y, N, N, FN_SLT , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopSLTU)  -> List(BR_N , Y, N, N, FN_SLTU, DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopAND)   -> List(BR_N , Y, N, N, FN_AND , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopOR)    -> List(BR_N , Y, N, N, FN_OR  , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopXOR)   -> List(BR_N , Y, N, N, FN_XOR , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopSRA)   -> List(BR_N , Y, N, N, FN_SRA , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopSRL)   -> List(BR_N , Y, N, N, FN_SR  , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),

         BitPat(uopADDW)  -> List(BR_N , Y, N, N, FN_ADD , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopSUBW)  -> List(BR_N , Y, N, N, FN_SUB , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopSLLW)  -> List(BR_N , Y, N, N, FN_SL  , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopSRAW)  -> List(BR_N , Y, N, N, FN_SRA , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopSRLW)  -> List(BR_N , Y, N, N, FN_SR  , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),

         BitPat(uopMUL)   -> List(BR_N , N, Y, N, FN_MUL,   DW_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,rocket.CSR.N),
         BitPat(uopMULH)  -> List(BR_N , N, Y, N, FN_MULH,  DW_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,rocket.CSR.N),
         BitPat(uopMULHU) -> List(BR_N , N, Y, N, FN_MULHU, DW_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,rocket.CSR.N),
         BitPat(uopMULHSU)-> List(BR_N , N, Y, N, FN_MULHSU,DW_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,rocket.CSR.N),
         BitPat(uopMULW)  -> List(BR_N , N, Y, N, FN_MUL,   DW_32 ,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,rocket.CSR.N),

         BitPat(uopDIV)   -> List(BR_N , N, Y, N, FN_DIV , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopDIVU)  -> List(BR_N , N, Y, N, FN_DIVU, DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopREM)   -> List(BR_N , N, Y, N, FN_REM , DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopREMU)  -> List(BR_N , N, Y, N, FN_REMU, DW_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopDIVW)  -> List(BR_N , N, Y, N, FN_DIV , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopDIVUW) -> List(BR_N , N, Y, N, FN_DIVU, DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopREMW)  -> List(BR_N , N, Y, N, FN_REM , DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopREMUW) -> List(BR_N , N, Y, N, FN_REMU, DW_32 , OP1_RS1 , OP2_RS2 , IS_X, REN_1, rocket.CSR.N),

         BitPat(uopBEQ)   -> List(BR_EQ ,Y, N, N, FN_SUB , DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, rocket.CSR.N),
         BitPat(uopBNE)   -> List(BR_NE ,Y, N, N, FN_SUB , DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, rocket.CSR.N),
         BitPat(uopBGE)   -> List(BR_GE ,Y, N, N, FN_SLT , DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, rocket.CSR.N),
         BitPat(uopBGEU)  -> List(BR_GEU,Y, N, N, FN_SLTU, DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, rocket.CSR.N),
         BitPat(uopBLT)   -> List(BR_LT ,Y, N, N, FN_SLT , DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, rocket.CSR.N),
         BitPat(uopBLTU)  -> List(BR_LTU,Y, N, N, FN_SLTU, DW_XPR, OP1_X   , OP2_X   , IS_B, REN_0, rocket.CSR.N),

         BitPat(uopJAL)   -> List(BR_J , Y, N, N, FN_ADD , DW_XPR, OP1_PC  , OP2_FOUR, IS_J, REN_1, rocket.CSR.N),
         BitPat(uopJALR)  -> List(BR_JR, Y, N, N, FN_ADD , DW_XPR, OP1_PC  , OP2_FOUR, IS_I, REN_1, rocket.CSR.N),
         BitPat(uopAUIPC) -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_PC  , OP2_IMM , IS_U, REN_1, rocket.CSR.N),

         BitPat(uopCSRRW) -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_I, REN_1, rocket.CSR.W),
         BitPat(uopCSRRS) -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_I, REN_1, rocket.CSR.S),
         BitPat(uopCSRRC) -> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_RS1 , OP2_ZERO, IS_I, REN_1, rocket.CSR.C),

         BitPat(uopCSRRWI)-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_1, rocket.CSR.W),
         BitPat(uopCSRRSI)-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_1, rocket.CSR.S),
         BitPat(uopCSRRCI)-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_1, rocket.CSR.C),

         BitPat(uopSYSTEM)-> List(BR_N , Y, N, N, FN_ADD , DW_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_0, rocket.CSR.I),

         // floating-point
         BitPat(uopFCLASS_S)->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCLASS_D)->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

         BitPat(uopFMV_S_X)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFMV_D_X)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFMV_X_S)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFMV_X_D)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFSGNJ_S)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFSGNJ_D)->List(BR_N , Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

         BitPat(uopFCVT_S_D) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_D_S) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

         BitPat(uopFCVT_S_W) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_S_WU)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_S_L) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_S_LU)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_D_W) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_D_WU)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_D_L) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_D_LU)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

         BitPat(uopFCVT_W_S) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_WU_S)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_L_S) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_LU_S)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_W_D) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_WU_D)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_L_D) ->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFCVT_LU_D)->List(BR_N,Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

         BitPat(uopFEQ_S)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFLT_S)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFLE_S)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFEQ_D)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFLT_D)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFLE_D)   ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

         BitPat(uopFMIN_S)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFMAX_S)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFMIN_D)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFMAX_D)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

         BitPat(uopFADD_S)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFSUB_S)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFMUL_S)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFADD_D)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFSUB_D)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFMUL_D)  ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),

         BitPat(uopFMADD_S) ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFMSUB_S) ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFNMADD_S)->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFNMSUB_S)->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFMADD_D) ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFMSUB_D) ->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFNMADD_D)->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N),
         BitPat(uopFNMSUB_D)->List(BR_N, Y, N, N, FN_X   , DW_X  , OP1_X   , OP2_X   , IS_X, REN_1, rocket.CSR.N)
         )

   val rrd_cs = Wire(new RRdCtrlSigs()).decode(io.rrd_uop.uopc, default, table)
   require (rrd_cs.op_fcn.getWidth == FN_SRA.getWidth)

   // rrd_use_alupipe is unused
   io.rrd_uop.ctrl.br_type := rrd_cs.br_type
   io.rrd_uop.ctrl.rf_wen  := rrd_cs.rf_wen
   io.rrd_uop.ctrl.op1_sel := rrd_cs.op1_sel
   io.rrd_uop.ctrl.op2_sel := rrd_cs.op2_sel
   io.rrd_uop.ctrl.imm_sel := rrd_cs.imm_sel
   io.rrd_uop.ctrl.op_fcn  := rrd_cs.op_fcn.toBits
   io.rrd_uop.ctrl.fcn_dw  := rrd_cs.fcn_dw.toBool
   io.rrd_uop.ctrl.is_load := io.rrd_uop.uopc === uopLD
   io.rrd_uop.ctrl.is_sta  := io.rrd_uop.uopc === uopSTA || io.rrd_uop.uopc === uopAMO_AG
   io.rrd_uop.ctrl.is_std  := io.rrd_uop.uopc === uopSTD || (io.rrd_uop.ctrl.is_sta && io.rrd_uop.lrs2_rtype != RT_X)

   when (io.rrd_uop.uopc === uopAMO_AG)
   {
      io.rrd_uop.imm_packed := UInt(0)
   }

   val raddr1 = io.rrd_uop.pop1 // although renamed, it'll stay 0 if lrs1 = 0
   val csr_ren = (rrd_cs.csr_cmd === rocket.CSR.S || rrd_cs.csr_cmd === rocket.CSR.C) && raddr1 === UInt(0)
   io.rrd_uop.ctrl.csr_cmd := Mux(csr_ren, rocket.CSR.R, rrd_cs.csr_cmd)

   //-------------------------------------------------------------
   // set outputs

   io.rrd_valid := rrd_valid

}


}
