//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Functional Units
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2013 Mar 10
//
// If regfile bypassing is disabled, then the functional unit must do its own
// bypassing in here on the WB stage (i.e., bypassing the io.resp.data)

// TODO: explore possibility of conditional IO fields?


package boom
{

import Chisel._
import Node._
import cde.Parameters

import rocket.ALU._
import rocket.Util._
import uncore.constants.MemoryOpConstants._


object FUCode
{
   // bit mask, since a given execution pipeline may support multiple functional units
   val FUC_SZ = 7
   val FU_X    = BitPat.DC(FUC_SZ)
   val FU_ALU  = Bits(  1, FUC_SZ)
   val FU_BRU  = Bits(  2, FUC_SZ)
   val FU_MEM  = Bits(  4, FUC_SZ)
   val FU_MUL  = Bits(  8, FUC_SZ)
   val FU_DIV  = Bits( 16, FUC_SZ)
   val FU_FPU  = Bits( 32, FUC_SZ)
   val FU_CSR  = Bits( 64, FUC_SZ)
}
import FUCode._

// TODO if a branch unit... how to add extra to IO in subclass?

class FunctionalUnitIo(num_stages: Int
                      , num_bypass_stages: Int
                      , data_width: Int
                      )(implicit p: Parameters) extends BoomBundle()(p)
{
   val req     = (new DecoupledIO(new FuncUnitReq(data_width))).flip
   val resp    = (new DecoupledIO(new FuncUnitResp(data_width)))

   val brinfo  = new BrResolutionInfo().asInput()

   val bypass  = new BypassData(num_bypass_stages, data_width).asOutput()

   val br_unit = new BranchUnitResp().asOutput

   // only used by the fpu unit
   val fcsr_rm = Bits(INPUT, rocket.FPConstants.RM_SZ)

   // only used by branch unit
   // TODO name this, so ROB can also instantiate it
   val get_rob_pc = new RobPCRequest().flip
   val get_pred = new GetPredictionInfo
}

class GetPredictionInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val br_tag = UInt(OUTPUT, BR_TAG_SZ)
   val info = new BranchPredictionResp().asInput
}

class FuncUnitReq(data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop = new MicroOp()

   val num_operands = 3

   val rs1_data = Bits(width = data_width)
   val rs2_data = Bits(width = data_width)
   val rs3_data = Bits(width = data_width) // only used for FMA units
//   val rs_data = Vec.fill(num_operands) {Bits(width=data_width)}
//   def rs1_data = rs_data(0)
//   def rs2_data = rs_data(1)
//   def rs3_data = rs_data(2)

   val kill = Bool() // kill everything

   override def cloneType = new FuncUnitReq(data_width)(p).asInstanceOf[this.type]
}

class FuncUnitResp(data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop = new MicroOp()
   val data = Bits(width = data_width)
   val fflags = new ValidIO(new FFlagsResp)
   val addr = UInt(width = vaddrBits+1) // only for maddr -> LSU
   val mxcpt = new ValidIO(Bits(width=rocket.Causes.all.max)) //only for maddr->LSU

   override def cloneType = new FuncUnitResp(data_width)(p).asInstanceOf[this.type]
}

class BypassData(num_bypass_ports: Int, data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val valid = Vec.fill(num_bypass_ports){ Bool() }
   val uop   = Vec.fill(num_bypass_ports){ new MicroOp() }
   val data  = Vec.fill(num_bypass_ports){ Bits(width = data_width) }

   def getNumPorts: Int = num_bypass_ports
   override def cloneType: this.type = new BypassData(num_bypass_ports, data_width).asInstanceOf[this.type]
}

class BranchUnitResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val take_pc         = Bool()
   val target          = UInt(width = vaddrBits+1)

   val pc              = UInt(width = vaddrBits+1) // TODO this isn't really a branch_unit thing

   val brinfo          = new BrResolutionInfo() // NOTE: delayed a cycle!
   val btb_update_valid= Bool() // TODO turn this into a directed bundle so we can fold this into btb_update?
   val btb_update      = new rocket.BTBUpdate
   val bht_update      = Valid(new rocket.BHTUpdate)
   val bpd_update      = Valid(new BpdUpdate)

   val xcpt            = Valid(new Exception)

   val debug_btb_pred  = Bool() // just for debug, did the BTB and BHT predict taken?
}

abstract class FunctionalUnit(is_pipelined: Boolean
                              , num_stages: Int
                              , num_bypass_stages: Int
                              , data_width: Int
                              , has_branch_unit: Boolean = false)
                              (implicit p: Parameters) extends BoomModule()(p)
{
   val io = new FunctionalUnitIo(num_stages, num_bypass_stages, data_width)
}


// Note: this helps track which uops get killed while in intermediate stages,
// but it is the job of the consumer to check for kills on the same cycle as consumption!!!
abstract class PipelinedFunctionalUnit(val num_stages: Int,
                                       val num_bypass_stages: Int,
                                       val earliest_bypass_stage: Int,
                                       val data_width: Int,
                                       is_branch_unit: Boolean = false
                                      )(implicit p: Parameters) extends FunctionalUnit(is_pipelined = true
                                                              , num_stages = num_stages
                                                              , num_bypass_stages = num_bypass_stages
                                                              , data_width = data_width
                                                              , has_branch_unit = is_branch_unit)(p)
{
   // pipelined functional unit is always ready
   io.req.ready := Bool(true)

   val r_valids = Reg(init = Vec.fill(num_stages) { Bool(false) })
//   val r_uops   = Vec.fill(num_stages) { Reg(outType =  new MicroOp()) }
   val r_uops   = Reg(Vec(num_stages, new MicroOp()))


   if (num_stages >= 1)
   {
      // handle incoming request
      r_valids(0) := io.req.valid && !IsKilledByBranch(io.brinfo, io.req.bits.uop) && !io.req.bits.kill
      r_uops(0)   := io.req.bits.uop
      r_uops(0).br_mask := GetNewBrMask(io.brinfo, io.req.bits.uop)

      // handle middle of the pipeline
      for (i <- 1 until num_stages)
      {
         r_valids(i) := r_valids(i-1) && !IsKilledByBranch(io.brinfo, r_uops(i-1)) && !io.req.bits.kill
         r_uops(i)   := r_uops(i-1)
         r_uops(i).br_mask := GetNewBrMask(io.brinfo, r_uops(i-1))

         if (num_bypass_stages != 0)// && i > earliest_bypass_stage)
         {
            io.bypass.uop(i-1) := r_uops(i-1)
         }
      }

      // handle outgoing (branch could still kill it)
      // consumer must also check for pipeline flushes (kills)
      io.resp.valid    := r_valids(num_stages-1) && !IsKilledByBranch(io.brinfo, r_uops(num_stages-1))
      io.resp.bits.uop := r_uops(num_stages-1)
      io.resp.bits.uop.br_mask := GetNewBrMask(io.brinfo, r_uops(num_stages-1))
   }
   else
   {
      require (num_stages == 0)
      // pass req straight through to response

      // valid doesn't check kill signals, let consumer deal with it.
      // The LSU already handles it and this hurts critical path.
      io.resp.valid    := io.req.valid && !IsKilledByBranch(io.brinfo, io.req.bits.uop)
      io.resp.bits.uop := io.req.bits.uop
      io.resp.bits.uop.br_mask := GetNewBrMask(io.brinfo, io.req.bits.uop)
   }

   // bypassing (TODO allow bypass vector to have a different size from num_stages)
   if (num_bypass_stages > 0 && earliest_bypass_stage == 0)
   {
      io.bypass.uop(0) := io.req.bits.uop

      for (i <- 1 until num_bypass_stages)
      {
         io.bypass.uop(i) := r_uops(i-1)

      }
   }


}

class ALUUnit(is_branch_unit: Boolean = false, num_stages: Int = 1)(implicit p: Parameters)
             extends PipelinedFunctionalUnit(num_stages = num_stages
                                            , num_bypass_stages = num_stages
                                            , earliest_bypass_stage = 0
                                            , data_width = 64  //xLen
                                            , is_branch_unit = is_branch_unit)(p)
{
   val uop = io.req.bits.uop

   // immediate generation
   val imm_xprlen = ImmGen(uop.imm_packed, uop.ctrl.imm_sel)

   // operand 1 select
   var op1_data: Bits = null
   if (is_branch_unit)
   {
      op1_data = Mux(io.req.bits.uop.ctrl.op1_sel.toUInt === OP1_RS1 , io.req.bits.rs1_data,
                 Mux(io.req.bits.uop.ctrl.op1_sel.toUInt === OP1_PC  , Sext(io.get_rob_pc.curr_pc, xLen),
                                                                       UInt(0)))
   }
   else
   {
      op1_data = Mux(io.req.bits.uop.ctrl.op1_sel.toUInt === OP1_RS1 , io.req.bits.rs1_data,
                                                                       UInt(0))
   }

   // operand 2 select
   val op2_data = Mux(io.req.bits.uop.ctrl.op2_sel === OP2_IMM,  Sext(imm_xprlen, xLen),
                  Mux(io.req.bits.uop.ctrl.op2_sel === OP2_IMMC, io.req.bits.uop.pop1(4,0),
                  Mux(io.req.bits.uop.ctrl.op2_sel === OP2_RS2 , io.req.bits.rs2_data,
                  Mux(io.req.bits.uop.ctrl.op2_sel === OP2_FOUR, UInt(4),
                                                                 UInt(0)))))

   val alu = Module(new rocket.ALU())

   alu.io.in1 := op1_data.toUInt
   alu.io.in2 := op2_data.toUInt
   alu.io.fn  := io.req.bits.uop.ctrl.op_fcn
   alu.io.dw  := io.req.bits.uop.ctrl.fcn_dw

   if (is_branch_unit)
   {
      val uop_pc_ = io.get_rob_pc.curr_pc

      // The Branch Unit redirects the PC immediately, but delays the mispredict
      // signal a cycle (for critical path reasons)

      // Did I just get killed by the previous cycle's branch?
      // Or by a flush pipeline?
      val killed = Wire(Bool())
      killed := Bool(false)
      when (io.req.bits.kill ||
            (io.brinfo.valid &&
               io.brinfo.mispredict &&
               maskMatch(io.brinfo.mask, io.req.bits.uop.br_mask)
            ))
      {
         killed := Bool(true)
      }

      val rs1 = io.req.bits.rs1_data
      val rs2 = io.req.bits.rs2_data
      val br_eq  = (rs1 === rs2)
      val br_ltu = (rs1.toUInt < rs2.toUInt)
      val br_lt  = (~(rs1(xLen-1) ^ rs2(xLen-1)) & br_ltu |
                      rs1(xLen-1) & ~rs2(xLen-1)).toBool

      val pc_plus4 = (uop_pc_ + UInt(4))(vaddrBits,0)

      val pc_sel = Lookup(io.req.bits.uop.ctrl.br_type, PC_PLUS4,
               Array(   BR_N  -> PC_PLUS4,
                        BR_NE -> Mux(!br_eq,  PC_BRJMP, PC_PLUS4),
                        BR_EQ -> Mux( br_eq,  PC_BRJMP, PC_PLUS4),
                        BR_GE -> Mux(!br_lt,  PC_BRJMP, PC_PLUS4),
                        BR_GEU-> Mux(!br_ltu, PC_BRJMP, PC_PLUS4),
                        BR_LT -> Mux( br_lt,  PC_BRJMP, PC_PLUS4),
                        BR_LTU-> Mux( br_ltu, PC_BRJMP, PC_PLUS4),
                        BR_J  -> PC_BRJMP,
                        BR_JR -> PC_JALR
                        ))

      val bj_addr = Wire(UInt())

      val is_taken = io.req.valid &&
                     !killed &&
                     uop.is_br_or_jmp &&
                     (pc_sel =/= PC_PLUS4)

      // "mispredict" means that a branch has been resolved and it must be killed
      val mispredict = Wire(Bool()); mispredict := Bool(false)

      val is_br          = io.req.valid && !killed && uop.is_br_or_jmp && !uop.is_jump
      val is_br_or_jalr  = io.req.valid && !killed && uop.is_br_or_jmp && !uop.is_jal

      // did the BTB predict a br or jmp incorrectly?
      // (do we need to reset its history and teach it a new target?)
      val btb_mispredict = Wire(Bool()); btb_mispredict := Bool(false)

      // did the bpd predict incorrectly (aka, should we correct its prediction?)
      val bpd_mispredict = Wire(Bool()); bpd_mispredict := Bool(false)

      when (is_br_or_jalr)
      {
         when (pc_sel === PC_JALR)
         {
            // only the BTB can predict JALRs (must also check it predicted taken)
            btb_mispredict := !io.get_rob_pc.next_val || (io.get_rob_pc.next_pc =/= bj_addr) ||
                              !io.get_pred.info.btb_resp.taken || !uop.br_prediction.btb_hit
            bpd_mispredict := Bool(false)
         }
         when (pc_sel === PC_PLUS4)
         {
            btb_mispredict := uop.br_prediction.btb_hit && io.get_pred.info.btb_resp.taken
            bpd_mispredict := uop.br_prediction.bpd_predict_taken
         }
         when (pc_sel === PC_BRJMP)
         {
            btb_mispredict := !uop.br_prediction.btb_hit ||
                              (uop.br_prediction.btb_hit && !io.get_pred.info.btb_resp.taken)
            bpd_mispredict := !uop.br_prediction.bpd_predict_taken
         }
      }

      when (is_br_or_jalr && pc_sel === PC_BRJMP && !mispredict && io.get_rob_pc.next_val)
      {
         assert (io.get_rob_pc.next_pc === bj_addr, "[FuncUnit] branch is taken to the wrong target.")
      }

      when (is_br_or_jalr)
      {
         when (pc_sel === PC_JALR)
         {
            mispredict := btb_mispredict
         }
         when (pc_sel === PC_PLUS4)
         {
            mispredict := Mux(uop.br_prediction.wasBTB, btb_mispredict, bpd_mispredict)
         }
         when (pc_sel === PC_BRJMP)
         {
            mispredict := Mux(uop.br_prediction.wasBTB, btb_mispredict, bpd_mispredict)
         }
      }

      io.br_unit.take_pc := mispredict
      io.br_unit.target := Mux(pc_sel === PC_PLUS4, pc_plus4, bj_addr)

      // note: jal doesn't allocate a branch-mask, so don't clear a br-mask bit
      // branch resolution delayed a cycle for critical path reasons
      io.br_unit.brinfo.valid      := Reg(next = io.req.valid && uop.is_br_or_jmp && !uop.is_jal && !killed)
      io.br_unit.brinfo.mispredict := Reg(next = mispredict)
      io.br_unit.brinfo.mask       := Reg(next = UInt(1) << uop.br_tag)
      io.br_unit.brinfo.exe_mask   := Reg(next = GetNewBrMask(io.brinfo, uop.br_mask))
      io.br_unit.brinfo.tag        := Reg(next = uop.br_tag)
      io.br_unit.brinfo.rob_idx    := Reg(next = uop.rob_idx)
      io.br_unit.brinfo.ldq_idx    := Reg(next = uop.ldq_idx)
      io.br_unit.brinfo.stq_idx    := Reg(next = uop.stq_idx)
      io.br_unit.brinfo.brob_idx   := Reg(next = io.get_rob_pc.curr_brob_idx)
      io.br_unit.brinfo.is_br      := Reg(next = is_br)
      io.br_unit.brinfo.is_jr      := Reg(next = pc_sel === PC_JALR)
      io.br_unit.brinfo.taken      := Reg(next = is_taken)

      // updates the BTB same cycle as PC redirect
      val lsb = log2Ceil(FETCH_WIDTH*coreInstBytes)

      // did a branch or jalr occur AND did we mispredict? AND was it taken? (i.e., should we update the BTB)
      val fetch_pc = ((uop_pc_ >> lsb) << lsb) + uop.fetch_pc_lob

      if (p(EnableBTBContainsBranches))
      {
         io.br_unit.btb_update_valid := is_br_or_jalr && mispredict && is_taken
         // update on all branches (but not jal/jalr)
         io.br_unit.bht_update.valid := is_br
      }
      else
      {
         io.br_unit.btb_update_valid := is_br_or_jalr && mispredict && uop.is_jump
         io.br_unit.bht_update.valid := Bool(false)
      }

      io.br_unit.btb_update.pc               := fetch_pc // tell the BTB which pc to tag check against
      io.br_unit.btb_update.br_pc            := uop_pc_
      io.br_unit.btb_update.target           := io.br_unit.target & SInt(-coreInstBytes)
      io.br_unit.btb_update.prediction.valid := io.get_pred.info.btb_resp_valid // did this branch's fetch packet have
                                                                                // a BTB hit in fetch?
      io.br_unit.btb_update.prediction.bits  := io.get_pred.info.btb_resp       // give the BTB back its BTBResp
      io.br_unit.btb_update.taken            := is_taken   // was this branch/jal/jalr "taken"
      io.br_unit.btb_update.isJump           := uop.is_jump
      io.br_unit.btb_update.isReturn         := uop.is_ret

      io.br_unit.bht_update.bits.taken            := is_taken   // was this branch "taken"
      io.br_unit.bht_update.bits.mispredict       := btb_mispredict     // need to reset the history in the BHT
                                                                        // that is updated only on BTB hits
      io.br_unit.bht_update.bits.prediction.valid := io.get_pred.info.btb_resp_valid // only update if hit in the BTB
      io.br_unit.bht_update.bits.prediction.bits  := io.get_pred.info.btb_resp
      io.br_unit.bht_update.bits.pc               := fetch_pc // what pc should the tag check be on?

      io.br_unit.bpd_update.valid                 := is_br
      io.br_unit.bpd_update.bits.taken            := is_taken
      io.br_unit.bpd_update.bits.mispredict       := mispredict
      io.br_unit.bpd_update.bits.bpd_mispredict   := bpd_mispredict
      io.br_unit.bpd_update.bits.pc               := fetch_pc
      io.br_unit.bpd_update.bits.br_pc            := uop_pc_
      io.br_unit.bpd_update.bits.history          := io.get_pred.info.bpd_resp.info.history

      // is the br_pc the last instruction in the fetch bundle?
      val is_last_inst = if (FETCH_WIDTH == 1) { Bool(true) }
                         else { ((uop_pc_ >> UInt(log2Up(coreInstBytes))) &
                                 Fill(log2Up(FETCH_WIDTH), Bits(1))) === UInt(FETCH_WIDTH-1) }
      io.br_unit.bpd_update.bits.new_pc_same_packet := !(is_taken) && !is_last_inst

      require (coreInstBytes == 4)


      // Branch/Jump Target Calculation
      // we can't push this through the ALU though, b/c jalr needs both PC+4 and rs1+offset

      def vaSign(a0: UInt, ea: Bits):Bool = {
         // efficient means to compress 64-bit VA into rc.as.vaddrBits+1 bits
         // (VA is bad if VA(rc.as.vaddrBits) =/= VA(rc.as.vaddrBits-1))
         val a = a0 >> vaddrBits-1
         val e = ea(vaddrBits,vaddrBits-1)
         Mux(a === UInt(0) || a === UInt(1), e =/= UInt(0),
         Mux(a === SInt(-1) || a === SInt(-2), e === SInt(-1),
            e(0)))
      }

      val bj_base = Mux(uop.uopc === uopJALR, io.req.bits.rs1_data, uop_pc_)
      val bj_offset = imm_xprlen(20,0).toSInt
      val bj64 = bj_base + bj_offset
      val bj_msb = Mux(uop.uopc === uopJALR, vaSign(io.req.bits.rs1_data, bj64), vaSign(uop_pc_, bj64))
      bj_addr := (Cat(bj_msb, bj64(vaddrBits-1,0)) & SInt(-2)).toUInt

      io.br_unit.pc             := uop_pc_
      io.br_unit.debug_btb_pred := io.get_pred.info.btb_resp_valid && io.get_pred.info.btb_resp.taken

      // handle misaligned branch/jmp targets
      io.br_unit.xcpt.valid     := bj_addr(1) && io.req.valid && mispredict && !killed
      io.br_unit.xcpt.bits.uop  := uop
      io.br_unit.xcpt.bits.cause:= rocket.Causes.misaligned_fetch
      // TODO is there a better way to get this information to the CSR file? maybe use brinfo.target?
      io.br_unit.xcpt.bits.badvaddr:= bj_addr
   }

   // Response
   // TODO add clock gate on resp bits from functional units
//   io.resp.bits.data := RegEnable(alu.io.out, io.req.valid)
//   val reg_data = Reg(outType = Bits(width = xLen))
//   reg_data := alu.io.out
//   io.resp.bits.data := reg_data

   val r_val  = Reg(init = Vec.fill(num_stages) { Bool(false) })
   val r_data = Reg(Vec(num_stages, Bits(xLen)))
   r_val (0) := io.req.valid
   r_data(0) := alu.io.out
   for (i <- 1 until num_stages)
   {
      r_val(i)  := r_val(i-1)
      r_data(i) := r_data(i-1)
   }
   io.resp.bits.data := r_data(num_stages-1)

   // Bypass
   // for the ALU, we can bypass same cycle as compute
   require (num_stages >= 1)
   require (num_bypass_stages >= 1)
   io.bypass.valid(0) := io.req.valid
   io.bypass.data (0) := alu.io.out
   for (i <- 1 until num_stages)
   {
      io.bypass.valid(i) := r_val(i-1)
      io.bypass.data (i) := r_data(i-1)
   }

   // Exceptions
   io.resp.bits.fflags.valid := Bool(false)
}


// passes in base+imm to calculate addresses, and passes store data, to the LSU
// for floating point, 65bit FP store-data needs to be decoded into 64bit FP form
class MemAddrCalcUnit(implicit p: Parameters) extends PipelinedFunctionalUnit(num_stages = 0
                                                     , num_bypass_stages = 0
                                                     , earliest_bypass_stage = 0
                                                     , data_width = 65 // TODO enable this only if FP is enabled?
                                                     , is_branch_unit = false)(p)
{
   // perform address calculation
   val sum = io.req.bits.rs1_data.toUInt + io.req.bits.uop.imm_packed(19,8).toSInt
   val ea_sign = Mux(sum(vaddrBits-1), ~sum(63,vaddrBits) === UInt(0),
                                        sum(63,vaddrBits) =/= UInt(0))
   val effective_address = Cat(ea_sign, sum(vaddrBits-1,0)).toUInt

   // compute store data
   // requires decoding 65-bit FP data
   val unrec_s = hardfloat.fNFromRecFN(8, 24, io.req.bits.rs2_data)
   val unrec_d = hardfloat.fNFromRecFN(11, 53, io.req.bits.rs2_data)
   val unrec_out = Mux(io.req.bits.uop.fp_single, Cat(Fill(32, unrec_s(31)), unrec_s), unrec_d)

   var store_data:Bits = null
   if (!usingFPU) store_data = io.req.bits.rs2_data
   else store_data = Mux(io.req.bits.uop.fp_val, unrec_out, io.req.bits.rs2_data)

   io.resp.bits.addr := effective_address
   io.resp.bits.data := store_data

   if (data_width > 63)
   {
      assert (!(io.req.valid && io.req.bits.uop.ctrl.is_std &&
         io.resp.bits.data(64).toBool === Bool(true)), "65th bit set in MemAddrCalcUnit.")
   }

   // Handle misaligned exceptions
   val typ = io.req.bits.uop.mem_typ
   val misaligned =
      (((typ === MT_H) || (typ === MT_HU)) && (effective_address(0) =/= Bits(0))) ||
      (((typ === MT_W) || (typ === MT_WU)) && (effective_address(1,0) =/= Bits(0))) ||
      ((typ === MT_D) && (effective_address(2,0) =/= Bits(0)))

   val ma_ld = io.req.valid && io.req.bits.uop.uopc === uopLD && misaligned
   val ma_st = io.req.valid && (io.req.bits.uop.uopc === uopSTA || io.req.bits.uop.uopc === uopAMO_AG) && misaligned

   io.resp.bits.mxcpt.valid := ma_ld || ma_st
   io.resp.bits.mxcpt.bits  := Mux(ma_ld, rocket.Causes.misaligned_load,
                                          rocket.Causes.misaligned_store)
   assert (!(ma_ld && ma_st), "Mutually-exclusive exceptions are firing.")
}



// currently, bypassing is unsupported!
// All FP instructions are padded out to the max latency unit for easy
// write-port scheduling.
class FPUUnit(implicit p: Parameters) extends PipelinedFunctionalUnit(num_stages = 3
                                            , num_bypass_stages = 0
                                            , earliest_bypass_stage = 0
                                            , data_width = 65)(p)
{
   val fpu = Module(new FPU())
   fpu.io.req <> io.req
   fpu.io.req.bits.fcsr_rm := io.fcsr_rm

   io.resp.bits.data              := fpu.io.resp.bits.data
   io.resp.bits.fflags.valid      := fpu.io.resp.bits.fflags.valid
   io.resp.bits.fflags.bits.uop   := io.resp.bits.uop
   io.resp.bits.fflags.bits.flags := fpu.io.resp.bits.fflags.bits.flags // kill me now
}



// unpipelined, can only hold a single MicroOp at a time
// assumes at least one register between request and response
abstract class UnPipelinedFunctionalUnit(implicit p: Parameters)
                                       extends FunctionalUnit(is_pipelined = false
                                                            , num_stages = 1
                                                            , num_bypass_stages = 0
                                                            , data_width = 64
                                                            , has_branch_unit = false)(p)
{
   val r_uop = Reg(outType = new MicroOp())

   val do_kill = Wire(Bool())
   do_kill := io.req.bits.kill // irrelevant default

   when (io.req.fire())
   {
      // update incoming uop
      do_kill := IsKilledByBranch(io.brinfo, io.req.bits.uop) || io.req.bits.kill
      r_uop := io.req.bits.uop
      r_uop.br_mask := GetNewBrMask(io.brinfo, io.req.bits.uop)
   }
   .otherwise
   {
      do_kill := IsKilledByBranch(io.brinfo, r_uop) || io.req.bits.kill
      r_uop.br_mask := GetNewBrMask(io.brinfo, r_uop)
   }


   // assumes at least one pipeline register between request and response
   io.resp.bits.uop := r_uop
}


class MulDivUnit(implicit p: Parameters) extends UnPipelinedFunctionalUnit()(p)
{
   val muldiv = Module(new rocket.MulDiv(width = xLen,
                                  unroll = if(usingFastMulDiv) 8 else 1,
                                  earlyOut = usingFastMulDiv))

   // request
   muldiv.io.req.valid    := io.req.valid && !this.do_kill
   muldiv.io.req.bits.dw  := io.req.bits.uop.ctrl.fcn_dw
   muldiv.io.req.bits.fn  := io.req.bits.uop.ctrl.op_fcn
   muldiv.io.req.bits.in1 := io.req.bits.rs1_data
   muldiv.io.req.bits.in2 := io.req.bits.rs2_data
   io.req.ready           := muldiv.io.req.ready

   // handle pipeline kills and branch misspeculations
   muldiv.io.kill         := this.do_kill

   // response
   io.resp.valid          := muldiv.io.resp.valid
   muldiv.io.resp.ready   := io.resp.ready
   io.resp.bits.data      := muldiv.io.resp.bits.data
}

class PipelinedMulUnit(num_stages: Int)(implicit p: Parameters)
      extends PipelinedFunctionalUnit (num_stages = num_stages
                                      , num_bypass_stages = 0
                                      , earliest_bypass_stage = 0
                                      , data_width = 64)(p)
{
   val imul = Module(new IMul(num_stages))
   // request
   imul.io.valid := io.req.valid
   imul.io.in0   := io.req.bits.rs1_data
   imul.io.in1   := io.req.bits.rs2_data
   imul.io.dw    := io.req.bits.uop.ctrl.fcn_dw
   imul.io.fn    := io.req.bits.uop.ctrl.op_fcn

   // response
   io.resp.bits.data      := imul.io.out
}

}

