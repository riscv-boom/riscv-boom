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

// TODO: explore possibility of conditional IO fields? if a branch unit... how to add extra to IO in subclass?

package boom

import Chisel._
import cde.Parameters

import rocket.ALU._
import util._
import uncore.constants.MemoryOpConstants._


object FUConstants
{
   // bit mask, since a given execution pipeline may support multiple functional units
   val FUC_SZ = 10
   val FU_X   = BitPat.DC(FUC_SZ)
   val FU_ALU = UInt(  1, FUC_SZ)
   val FU_BRU = UInt(  2, FUC_SZ)
   val FU_MEM = UInt(  4, FUC_SZ)
   val FU_MUL = UInt(  8, FUC_SZ)
   val FU_DIV = UInt( 16, FUC_SZ)
   val FU_CSR = UInt( 32, FUC_SZ)
   val FU_FPU = UInt( 64, FUC_SZ)
   val FU_FDV = UInt(128, FUC_SZ)
   val FU_I2F = UInt(256, FUC_SZ)
   val FU_F2I = UInt(512, FUC_SZ)
}
import FUConstants._

// tell the FUDecoders what units it needs to support
class SupportedFuncUnits(
   val alu: Boolean  = false,
   val bru: Boolean  = false,
   val mem: Boolean  = false,
   val muld: Boolean = false,
   val fpu: Boolean  = false,
   val csr: Boolean  = false,
   val fdiv: Boolean = false,
   val ifpu: Boolean = false)
{
}


class FunctionalUnitIo(num_stages: Int
                      , num_bypass_stages: Int
                      , data_width: Int
                      )(implicit p: Parameters) extends BoomBundle()(p)
{
   val req     = (new DecoupledIO(new FuncUnitReq(data_width))).flip
   val resp    = (new DecoupledIO(new FuncUnitResp(data_width)))

   val brinfo  = new BrResolutionInfo().asInput

   val bypass  = new BypassData(num_bypass_stages, data_width).asOutput

   val br_unit = new BranchUnitResp().asOutput

   // only used by the fpu unit
   val fcsr_rm = UInt(INPUT, rocket.FPConstants.RM_SZ)

   // only used by branch unit
   // TODO name this, so ROB can also instantiate it
   val get_rob_pc = new RobPCRequest().flip
   val get_pred = new GetPredictionInfo
   val status = new rocket.MStatus().asInput
}

class GetPredictionInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val br_tag = UInt(OUTPUT, BR_TAG_SZ)
//   val info = new BranchPredictionResp().asInput
   val info = new BranchPredInfo().asInput
}

class FuncUnitReq(data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop = new MicroOp()

   val num_operands = 3

   val rs1_data = UInt(width = data_width)
   val rs2_data = UInt(width = data_width)
   val rs3_data = UInt(width = data_width) // only used for FMA units

   val kill = Bool() // kill everything

   override def cloneType = new FuncUnitReq(data_width)(p).asInstanceOf[this.type]
}

class FuncUnitResp(data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop = new MicroOp()
   val data = UInt(width = data_width)
   val fflags = new ValidIO(new FFlagsResp)
   val addr = UInt(width = vaddrBits+1) // only for maddr -> LSU
   val mxcpt = new ValidIO(UInt(width=rocket.Causes.all.max)) //only for maddr->LSU

   override def cloneType = new FuncUnitResp(data_width)(p).asInstanceOf[this.type]
}

class BypassData(num_bypass_ports: Int, data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val valid = Vec(num_bypass_ports, Bool())
   val uop   = Vec(num_bypass_ports, new MicroOp())
   val data  = Vec(num_bypass_ports, UInt(width = data_width))

   def getNumPorts: Int = num_bypass_ports
   override def cloneType: this.type = new BypassData(num_bypass_ports, data_width).asInstanceOf[this.type]
}

class BrResolutionInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val valid      = Bool()
   val mispredict = Bool()
   val mask       = UInt(width = MAX_BR_COUNT) // the resolve mask
   val tag        = UInt(width = BR_TAG_SZ)    // the branch tag that was resolved
   val exe_mask   = UInt(width = MAX_BR_COUNT) // the br_mask of the actual branch uop
                                               // used to reset the dec_br_mask
   val rob_idx    = UInt(width = ROB_ADDR_SZ)
   val ldq_idx    = UInt(width = MEM_ADDR_SZ)  // track the "tail" of loads and stores, so we can
   val stq_idx    = UInt(width = MEM_ADDR_SZ)  // quickly reset the LSU on a mispredict
   val taken      = Bool()                     // which direction did the branch go?
   val is_jr      = Bool()

   // for stats
   val btb_made_pred  = Bool()
   val btb_mispredict = Bool()
   val bpd_made_pred  = Bool()
   val bpd_mispredict = Bool()
}

// for critical path reasons, some of the elements in this bundle may be delayed.
class BranchUnitResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val take_pc         = Bool()
   val target          = UInt(width = vaddrBitsExtended)

   val pc              = UInt(width = vaddrBitsExtended) // TODO this isn't really a branch_unit thing

   val brinfo          = new BrResolutionInfo()
   val btb_update      = Valid(new BTBsaUpdate)
   val bim_update      = Valid(new BimUpdate)
   val bpd_update      = Valid(new BpdUpdate)

   val xcpt            = Valid(new Exception)
}

abstract class FunctionalUnit(is_pipelined: Boolean
                              , num_stages: Int
                              , num_bypass_stages: Int
                              , data_width: Int
                              , has_branch_unit: Boolean = false)
                              (implicit p: Parameters) extends BoomModule()(p)
   with HasBoomCoreParameters
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
   // Pipelined functional unit is always ready.
   io.req.ready := Bool(true)


   if (num_stages > 0)
   {
      val r_valids = Reg(init = Vec.fill(num_stages) { Bool(false) })
      val r_uops   = Reg(Vec(num_stages, new MicroOp()))

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
   var op1_data: UInt = null
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
   val op2_data = Mux(io.req.bits.uop.ctrl.op2_sel === OP2_IMM,  Sext(imm_xprlen.toUInt, xLen),
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

      // Did I just get killed by the previous cycle's branch,
      // or by a flush pipeline?
      val killed = Wire(init=Bool(false))
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

      val pc_sel = MuxLookup(io.req.bits.uop.ctrl.br_type, PC_PLUS4,
               Seq  (   BR_N  -> PC_PLUS4,
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
      val mispredict = Wire(init = Bool(false))

      val is_br          = io.req.valid && !killed && uop.is_br_or_jmp && !uop.is_jump
      val is_br_or_jalr  = io.req.valid && !killed && uop.is_br_or_jmp && !uop.is_jal

      // did the BTB predict a br or jmp incorrectly?
      // (do we need to reset its history and teach it a new target?)
      val btb_mispredict = Wire(init = Bool(false))

      // did the bpd predict incorrectly (aka, should we correct its prediction?)
      val bpd_mispredict = Wire(init = Bool(false))

      // if b/j is taken, does it go to the wrong target?
      val wrong_taken_target = !io.get_rob_pc.next_val || (io.get_rob_pc.next_pc =/= bj_addr)


      if (DEBUG_PRINTF)
      {
         printf("  BR-UNIT: PC: 0x%x, Next: %d, 0x%x ,bj_addr: 0x%x\n",
            io.get_rob_pc.curr_pc, io.get_rob_pc.next_val, io.get_rob_pc.next_pc, bj_addr)
      }
      assert (!(io.req.valid && uop.is_jal && io.get_rob_pc.next_val && io.get_rob_pc.next_pc =/= bj_addr),
         "[func] JAL went to the wrong target.")

      when (is_br_or_jalr)
      {
         when (pc_sel === PC_JALR)
         {
            // only the BTB can predict JALRs (must also check it predicted taken)
            btb_mispredict := wrong_taken_target ||
                              !uop.br_prediction.btb_hit ||
                              !uop.br_prediction.btb_taken ||
                              io.status.debug // fun HACK to perform fence.i on JALRs in debug mode
//                              !io.get_pred.info.btb_resp.taken || XXX get info from get_Pred, not uop.br_pred
            bpd_mispredict := Bool(false)
         }
         when (pc_sel === PC_PLUS4)
         {
            btb_mispredict := uop.br_prediction.btb_hit && uop.br_prediction.btb_taken // TODO XXX io.get_pred.info.btb_resp.taken
            bpd_mispredict := uop.br_prediction.bpd_taken
         }
         when (pc_sel === PC_BRJMP)
         {
            btb_mispredict := wrong_taken_target ||
                              !uop.br_prediction.btb_hit ||
                              (uop.br_prediction.btb_hit && !uop.br_prediction.btb_taken) // TODO XXX !io.get_pred.info.btb_resp.taken)
            bpd_mispredict := !uop.br_prediction.bpd_taken
         }
      }

      when (is_br_or_jalr && pc_sel === PC_BRJMP && !mispredict && io.get_rob_pc.next_val)
      {
         when (io.get_rob_pc.next_pc =/= bj_addr)
         {
            printf ("[FuncUnit] Branch jumped to 0x%x, should have jumped to 0x%x.\n",
               io.get_rob_pc.next_pc, bj_addr)
         }
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
            mispredict :=
               Mux(uop.br_prediction.btb_blame,
                  btb_mispredict,
               Mux(uop.br_prediction.bpd_blame,
                  bpd_mispredict,
                  false.B)) // if neither BTB nor BPD predicted and it's not-taken, then no misprediction occurred.
         }
         when (pc_sel === PC_BRJMP)
         {
            mispredict :=
               Mux(uop.br_prediction.btb_blame,
                  btb_mispredict,
               Mux(uop.br_prediction.bpd_blame,
                  bpd_mispredict,
                  true.B)) // if neither BTB nor BPD predicted and it's taken, then a misprediction occurred.

         }
      }


      val br_unit =
         if (enableBrResolutionRegister) Reg(new BranchUnitResp)
         else Wire(new BranchUnitResp)



      br_unit.take_pc := mispredict
      val target = Mux(pc_sel === PC_PLUS4, pc_plus4, bj_addr)
      br_unit.target := target

      // Delay branch resolution a cycle for critical path reasons.
      // If the rest of "br_unit" is being registered too, then we don't need to
      // register "brinfo" here, since in that case we would be double counting.
      val brinfo =
         if (enableBrResolutionRegister) Wire(new BrResolutionInfo)
         else Reg(new BrResolutionInfo)

      // note: jal doesn't allocate a branch-mask, so don't clear a br-mask bit
      brinfo.valid          := io.req.valid && uop.is_br_or_jmp && !uop.is_jal && !killed
      brinfo.mispredict     := mispredict
      brinfo.mask           := UInt(1) << uop.br_tag
      brinfo.exe_mask       := GetNewBrMask(io.brinfo, uop.br_mask)
      brinfo.tag            := uop.br_tag
      brinfo.rob_idx        := uop.rob_idx
      brinfo.ldq_idx        := uop.ldq_idx
      brinfo.stq_idx        := uop.stq_idx
      brinfo.is_jr          := pc_sel === PC_JALR
      brinfo.taken          := is_taken
      brinfo.btb_mispredict := btb_mispredict
      brinfo.bpd_mispredict := bpd_mispredict
      brinfo.btb_made_pred  := uop.br_prediction.btb_blame
      brinfo.bpd_made_pred  := uop.br_prediction.bpd_blame

      br_unit.brinfo := brinfo

      // updates the BTB same cycle as PC redirect
      val lsb = log2Ceil(FETCH_WIDTH*coreInstBytes)

      // did a branch or jalr occur AND did we mispredict? AND was it taken? (i.e., should we update the BTB)
      val fetch_pc = ((uop_pc_ >> lsb) << lsb) + uop.fetch_pc_lob

      if (enableBTBContainsBranches)
      {
         br_unit.btb_update.valid := is_br_or_jalr && mispredict && is_taken && !uop.br_prediction.btb_hit
         // update on all branches (but not jal/jalr)
         br_unit.bim_update.valid := is_br && mispredict && uop.br_prediction.btb_hit
      }
      else
      {
         br_unit.btb_update.valid := is_br_or_jalr && mispredict && uop.is_jump
         br_unit.bim_update.valid := Bool(false)
      }

      br_unit.btb_update.bits.pc               := fetch_pc // tell the BTB which pc to tag check against
      br_unit.btb_update.bits.cfi_pc           := uop_pc_
      br_unit.btb_update.bits.target           := (target.toSInt & SInt(-coreInstBytes)).toUInt
//      br_unit.btb_update.prediction.valid := io.get_pred.info.btb_resp_valid // did this branch's fetch packet have
//                                                                             // a BTB hit in fetch?
//      br_unit.btb_update.prediction.bits  := io.get_pred.info.btb_resp       // give the BTB back its BTBResp
      br_unit.btb_update.bits.taken            := is_taken   // was this branch/jal/jalr "taken"
//      br_unit.btb_update.bits.is_jump          := uop.is_jump
//      br_unit.btb_update.bits.is_ret           := uop.is_ret
      br_unit.btb_update.bits.cfi_type         :=
			Mux(uop.is_jal, CfiType.jal,
			Mux(uop.is_jump && !uop.is_jal, CfiType.jalr,
				CfiType.branch))
      br_unit.btb_update.bits.bpd_type			  :=
			Mux(uop.is_ret, BpredType.ret,
			Mux(uop.is_call, BpredType.call,
			Mux(uop.is_jump, BpredType.jump,
				BpredType.branch)))

      br_unit.bim_update.bits.taken            := is_taken   // was this branch "taken"
      br_unit.bim_update.bits.bim_resp         := io.get_pred.info.bim_resp

      br_unit.bpd_update.valid                 := io.req.valid && uop.is_br_or_jmp &&
                                                  !uop.is_jal && !killed
      br_unit.bpd_update.bits.is_br            := is_br
      br_unit.bpd_update.bits.brob_idx         := io.get_rob_pc.curr_brob_idx
      br_unit.bpd_update.bits.taken            := is_taken
      br_unit.bpd_update.bits.mispredict       := mispredict
      br_unit.bpd_update.bits.bpd_predict_val  := uop.br_prediction.bpd_hit
      br_unit.bpd_update.bits.bpd_mispredict   := bpd_mispredict
      br_unit.bpd_update.bits.pc               := fetch_pc
      br_unit.bpd_update.bits.br_pc            := uop_pc_
      br_unit.bpd_update.bits.history_ptr      := io.get_pred.info.bpd_resp.history_ptr
      br_unit.bpd_update.bits.info             := io.get_pred.info.bpd_resp.info
      if (!ENABLE_VLHR)
      {
         br_unit.bpd_update.bits.history.get := io.get_pred.info.bpd_resp.history.get
         br_unit.bpd_update.bits.history_u.get := io.get_pred.info.bpd_resp.history_u.get
      }

      // is the br_pc the last instruction in the fetch bundle?
      val is_last_inst = if (FETCH_WIDTH == 1) { Bool(true) }
                         else { ((uop_pc_ >> UInt(log2Up(coreInstBytes))) &
                                 Fill(log2Up(FETCH_WIDTH), UInt(1))) === UInt(FETCH_WIDTH-1) }
      br_unit.bpd_update.bits.new_pc_same_packet := !(is_taken) && !is_last_inst

      require (coreInstBytes == 4)


      // Branch/Jump Target Calculation
      // we can't push this through the ALU though, b/c jalr needs both PC+4 and rs1+offset

      def vaSign(a0: UInt, ea: UInt):Bool = {
         // efficient means to compress 64-bit VA into rc.as.vaddrBits+1 bits
         // (VA is bad if VA(rc.as.vaddrBits) =/= VA(rc.as.vaddrBits-1))
         val a = a0 >> vaddrBits-1
         val e = ea(vaddrBits,vaddrBits-1)
         Mux(a === UInt(0) || a === UInt(1), e =/= UInt(0),
         Mux(a.toSInt === SInt(-1) || a.toSInt === SInt(-2), e.toSInt === SInt(-1),
            e(0)))
      }

      val bj_base = Mux(uop.uopc === uopJALR, io.req.bits.rs1_data, uop_pc_)
      val bj_offset = imm_xprlen(20,0).toSInt
      val bj64 = (bj_base.toSInt + bj_offset).toUInt
      val bj_msb = Mux(uop.uopc === uopJALR, vaSign(io.req.bits.rs1_data, bj64.toUInt), vaSign(uop_pc_, bj64.toUInt))
      bj_addr := (Cat(bj_msb, bj64(vaddrBits-1,0)).toSInt & SInt(-2)).toUInt

      br_unit.pc             := uop_pc_

      // handle misaligned branch/jmp targets
      // TODO BUG only trip xcpt if taken to bj_addr
      br_unit.xcpt.valid     := bj_addr(1) && io.req.valid && mispredict && !killed
      br_unit.xcpt.bits.uop  := uop
      br_unit.xcpt.bits.cause:= UInt(rocket.Causes.misaligned_fetch)
      // TODO is there a better way to get this information to the CSR file? maybe use brinfo.target?
      br_unit.xcpt.bits.badvaddr:= bj_addr

      io.br_unit := br_unit
   }

   // Response
   // TODO add clock gate on resp bits from functional units
//   io.resp.bits.data := RegEnable(alu.io.out, io.req.valid)
//   val reg_data = Reg(outType = Bits(width = xLen))
//   reg_data := alu.io.out
//   io.resp.bits.data := reg_data

   val r_val  = Reg(init = Vec.fill(num_stages) { Bool(false) })
   val r_data = Reg(Vec(num_stages, UInt(width=xLen)))
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
   val sum = (io.req.bits.rs1_data.toSInt + io.req.bits.uop.imm_packed(19,8).toSInt).toUInt
   val ea_sign = Mux(sum(vaddrBits-1), ~sum(63,vaddrBits) === UInt(0),
                                        sum(63,vaddrBits) =/= UInt(0))
   val effective_address = Cat(ea_sign, sum(vaddrBits-1,0)).toUInt

   // compute store data
   // requires decoding 65-bit FP data
   // TODO remove this recoding -- since this will only handle Int micro-ops with split regfiles.
   val unrec_s = hardfloat.fNFromRecFN(8, 24, io.req.bits.rs2_data)
   val unrec_d = hardfloat.fNFromRecFN(11, 53, io.req.bits.rs2_data)
   val unrec_out = Mux(io.req.bits.uop.fp_single, Cat(Fill(32, unrec_s(31)), unrec_s), unrec_d)

   var store_data:UInt = null
   if (!usingFPU) store_data = io.req.bits.rs2_data
   else store_data = Mux(io.req.bits.uop.fp_val, unrec_out, io.req.bits.rs2_data)

   io.resp.bits.addr := effective_address
   io.resp.bits.data := store_data

   if (data_width > 63)
   {
      assert (!(io.req.valid && io.req.bits.uop.ctrl.is_std &&
         io.resp.bits.data(64).toBool === Bool(true)), "65th bit set in MemAddrCalcUnit.")

      assert (!(io.req.valid && io.req.bits.uop.ctrl.is_std && io.req.bits.uop.fp_val),
         "FP store-data should now be going through a different unit.")
   }

   // Handle misaligned exceptions
   val typ = io.req.bits.uop.mem_typ
   val misaligned =
      (((typ === rocket.MT_H) || (typ === rocket.MT_HU)) && (effective_address(0) =/= UInt(0))) ||
      (((typ === rocket.MT_W) || (typ === rocket.MT_WU)) && (effective_address(1,0) =/= UInt(0))) ||
      ((typ ===  rocket.MT_D) && (effective_address(2,0) =/= UInt(0)))

   val ma_ld = io.req.valid && io.req.bits.uop.uopc === uopLD && misaligned
   val ma_st = io.req.valid && (io.req.bits.uop.uopc === uopSTA || io.req.bits.uop.uopc === uopAMO_AG) && misaligned

   io.resp.bits.mxcpt.valid := ma_ld || ma_st
   io.resp.bits.mxcpt.bits  := Mux(ma_ld, UInt(rocket.Causes.misaligned_load),
                                          UInt(rocket.Causes.misaligned_store))
   assert (!(ma_ld && ma_st), "Mutually-exclusive exceptions are firing.")
}



// currently, bypassing is unsupported!
// All FP instructions are padded out to the max latency unit for easy
// write-port scheduling.
class FPUUnit(implicit p: Parameters) extends PipelinedFunctionalUnit(
   num_stages = p(rocket.FPUKey).get.dfmaLatency,
   num_bypass_stages = 0,
   earliest_bypass_stage = 0,
   data_width = 65)(p)
{
   val fpu = Module(new FPU())
   fpu.io.req <> io.req
   fpu.io.req.bits.fcsr_rm := io.fcsr_rm

   io.resp.bits.data              := fpu.io.resp.bits.data
   io.resp.bits.fflags.valid      := fpu.io.resp.bits.fflags.valid
   io.resp.bits.fflags.bits.uop   := io.resp.bits.uop
   io.resp.bits.fflags.bits.flags := fpu.io.resp.bits.fflags.bits.flags // kill me now
}


class IntToFPUnit(implicit p: Parameters) extends PipelinedFunctionalUnit(
   num_stages = p(BoomKey).intToFpLatency,
   num_bypass_stages = 0,
   earliest_bypass_stage = 0,
   data_width = 65)(p)
{
   val fp_decoder = Module(new UOPCodeFPUDecoder) // TODO use a simpler decoder
   val io_req = io.req.bits
   fp_decoder.io.uopc := io_req.uop.uopc
   val fp_ctrl = fp_decoder.io.sigs
   val fp_rm = Mux(ImmGenRm(io_req.uop.imm_packed) === Bits(7), io.fcsr_rm, ImmGenRm(io_req.uop.imm_packed))
   val req = Wire(new rocket.FPInput)
   req := fp_ctrl
   req.rm := fp_rm
   req.in1 := io_req.rs1_data
   req.in2 := io_req.rs2_data
   req.typ := ImmGenTyp(io_req.uop.imm_packed)

   assert (!(io.req.valid && fp_ctrl.fromint && req.in1(64).toBool),
      "[func] IntToFP integer input has 65th high-order bit set!")

   assert (!(io.req.valid && !fp_ctrl.fromint),
      "[func] Only support fromInt micro-ops.")

   val ifpu = Module(new rocket.IntToFP(intToFpLatency))
   ifpu.io.in.valid := io.req.valid
   ifpu.io.in.bits := req

   io.resp.bits.data              := ifpu.io.out.bits.data
   io.resp.bits.fflags.valid      := ifpu.io.out.valid
   io.resp.bits.fflags.bits.uop   := io.resp.bits.uop
   io.resp.bits.fflags.bits.flags := ifpu.io.out.bits.exc
}



// Iterative/unpipelined, can only hold a single MicroOp at a time TODO allow up to N micro-ops simultaneously.
// assumes at least one register between request and response
abstract class IterativeFunctionalUnit(implicit p: Parameters)
                                       extends FunctionalUnit(is_pipelined = false
                                                            , num_stages = 1
                                                            , num_bypass_stages = 0
                                                            , data_width = 64
                                                            , has_branch_unit = false)(p)
{
   val r_uop = Reg(new MicroOp())

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


class MulDivUnit(implicit p: Parameters) extends IterativeFunctionalUnit()(p)
{
   val muldiv = Module(new rocket.MulDiv(
      p(rocket.MulDivKey).getOrElse(rocket.MulDivConfig()),
      width = xLen))

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
   io.resp.valid          := muldiv.io.resp.valid && !this.do_kill
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

