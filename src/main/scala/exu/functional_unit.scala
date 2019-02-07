//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Functional Units
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// If regfile bypassing is disabled, then the functional unit must do its own
// bypassing in here on the WB stage (i.e., bypassing the io.resp.data)
//
// TODO: explore possibility of conditional IO fields? if a branch unit... how to add extra to IO in subclass?

package boom.exu

import chisel3._
import chisel3.util._
import chisel3.experimental.chiselName

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.ALU._
import freechips.rocketchip.util._
import freechips.rocketchip.tile
import freechips.rocketchip.rocket.PipelinedMultiplier

import boom.bpu.{BpredType, BranchPredInfo, BoomBTBUpdate}
import boom.common._
import boom.ifu._
import boom.util._

object FUConstants
{
   // bit mask, since a given execution pipeline may support multiple functional units
   val FUC_SZ = 10
   val FU_X   = BitPat.dontCare(FUC_SZ)
   val FU_ALU =   1.U(FUC_SZ.W)
   val FU_BRU =   2.U(FUC_SZ.W)
   val FU_MEM =   4.U(FUC_SZ.W)
   val FU_MUL =   8.U(FUC_SZ.W)
   val FU_DIV =  16.U(FUC_SZ.W)
   val FU_CSR =  32.U(FUC_SZ.W)
   val FU_FPU =  64.U(FUC_SZ.W)
   val FU_FDV = 128.U(FUC_SZ.W)
   val FU_I2F = 256.U(FUC_SZ.W)
   val FU_F2I = 512.U(FUC_SZ.W)
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

class FunctionalUnitIo(
   val num_stages: Int,
   val num_bypass_stages: Int,
   val data_width: Int
   )(implicit p: Parameters) extends BoomBundle()(p)
{
   val req     = Flipped(new DecoupledIO(new FuncUnitReq(data_width)))
   val resp    = (new DecoupledIO(new FuncUnitResp(data_width)))

   val brinfo  = Input(new BrResolutionInfo())

   val bypass  = Output(new BypassData(num_bypass_stages, data_width))

   val br_unit = Output(new BranchUnitResp())

   // only used by the fpu unit
   val fcsr_rm = Input(UInt(tile.FPConstants.RM_SZ.W))

   // only used by branch unit
   val get_ftq_pc = Flipped(new GetPCFromFtqIO())
   val status = Input(new freechips.rocketchip.rocket.MStatus())
}

class GetPredictionInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val br_tag = Output(UInt(BR_TAG_SZ.W))
//   val info = new BranchPredictionResp().asInput
   val info = Input(new BranchPredInfo())
}

class FuncUnitReq(val data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomUOP
{
   val num_operands = 3

   val rs1_data = UInt(data_width.W)
   val rs2_data = UInt(data_width.W)
   val rs3_data = UInt(data_width.W) // only used for FMA units

   val kill = Bool() // kill everything

}

class FuncUnitResp(val data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomUOP
{
   val data = UInt(data_width.W)
   val fflags = new ValidIO(new FFlagsResp)
   val addr = UInt((vaddrBits+1).W) // only for maddr -> LSU
   val mxcpt = new ValidIO(UInt((freechips.rocketchip.rocket.Causes.all.max+2).W)) //only for maddr->LSU
   val sfence = Valid(new freechips.rocketchip.rocket.SFenceReq) // only for mcalc

}

class BypassData(val num_bypass_ports: Int, val data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val valid = Vec(num_bypass_ports, Bool())
   val uop   = Vec(num_bypass_ports, new MicroOp())
   val data  = Vec(num_bypass_ports, UInt(data_width.W))

   def getNumPorts: Int = num_bypass_ports
}

class BrResolutionInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val valid      = Bool()
   val mispredict = Bool()
   val mask       = UInt(MAX_BR_COUNT.W) // the resolve mask
   val tag        = UInt(BR_TAG_SZ.W)    // the branch tag that was resolved
   val exe_mask   = UInt(MAX_BR_COUNT.W) // the br_mask of the actual branch uop
                                               // used to reset the dec_br_mask
   val pc_lob     = UInt(log2Ceil(fetchWidth*coreInstBytes).W)
   val ftq_idx    = UInt(ftqSz.W)
   val rob_idx    = UInt(ROB_ADDR_SZ.W)
   val ldq_idx    = UInt(MEM_ADDR_SZ.W)  // track the "tail" of loads and stores, so we can
   val stq_idx    = UInt(MEM_ADDR_SZ.W)  // quickly reset the LSU on a mispredict
   val taken      = Bool()                     // which direction did the branch go?
   val is_jr      = Bool() // TODO remove use cfi_type instead
   val cfi_type   = CfiType()

   def getCfiIdx = pc_lob >> log2Ceil(coreInstBytes)

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
   val target          = UInt(vaddrBitsExtended.W) // TODO XXX REMOVE this -- use FTQ to redirect instead

   val pc              = UInt(vaddrBitsExtended.W) // TODO this isn't really a branch_unit thing

   val brinfo          = new BrResolutionInfo()
   val btb_update      = Valid(new BoomBTBUpdate)

   val xcpt            = Valid(new Exception)
}

abstract class FunctionalUnit(is_pipelined: Boolean
                              , num_stages: Int
                              , num_bypass_stages: Int
                              , data_width: Int
                              , has_branch_unit: Boolean = false)
                              (implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new FunctionalUnitIo(num_stages, num_bypass_stages, data_width))
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
   io.req.ready := true.B

   if (num_stages > 0)
   {
      val r_valids = RegInit(VecInit(Seq.fill(num_stages) { false.B }))
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

@chiselName
class ALUUnit(is_branch_unit: Boolean = false, num_stages: Int = 1, data_width: Int)(implicit p: Parameters)
             extends PipelinedFunctionalUnit(num_stages = num_stages
                                            , num_bypass_stages = num_stages
                                            , earliest_bypass_stage = 0
                                            , data_width = data_width
                                            , is_branch_unit = is_branch_unit)(p)
{
   val uop = io.req.bits.uop

   // immediate generation
   val imm_xprlen = ImmGen(uop.imm_packed, uop.ctrl.imm_sel)

   // operand 1 select
   var op1_data: UInt = null
   if (is_branch_unit)
   {
      val curr_pc = (AlignPCToBoundary(io.get_ftq_pc.fetch_pc, icBlockBytes)
                   + io.req.bits.uop.pc_lob
                   - Mux(io.req.bits.uop.edge_inst, 2.U, 0.U))
      op1_data = Mux(io.req.bits.uop.ctrl.op1_sel.asUInt === OP1_RS1 , io.req.bits.rs1_data,
                 Mux(io.req.bits.uop.ctrl.op1_sel.asUInt === OP1_PC  , Sext(curr_pc, xLen),
                                                                       0.U))
   }
   else
   {
      op1_data = Mux(io.req.bits.uop.ctrl.op1_sel.asUInt === OP1_RS1 , io.req.bits.rs1_data,
                                                                       0.U)
   }

   // operand 2 select
   val op2_data = Mux(io.req.bits.uop.ctrl.op2_sel === OP2_IMM,  Sext(imm_xprlen.asUInt, xLen),
                  Mux(io.req.bits.uop.ctrl.op2_sel === OP2_IMMC, io.req.bits.uop.pop1(4,0),
                  Mux(io.req.bits.uop.ctrl.op2_sel === OP2_RS2 , io.req.bits.rs2_data,
                  Mux(io.req.bits.uop.ctrl.op2_sel === OP2_NEXT, Mux(io.req.bits.uop.is_rvc, 2.U, 4.U),
                                                                 0.U))))

   val alu = Module(new freechips.rocketchip.rocket.ALU())

   alu.io.in1 := op1_data.asUInt
   alu.io.in2 := op2_data.asUInt
   alu.io.fn  := io.req.bits.uop.ctrl.op_fcn
   alu.io.dw  := io.req.bits.uop.ctrl.fcn_dw

   if (is_branch_unit)
   {
      val uop_pc_ = (AlignPCToBoundary(io.get_ftq_pc.fetch_pc, icBlockBytes)
                   + io.req.bits.uop.pc_lob
                   - Mux(io.req.bits.uop.edge_inst, 2.U, 0.U))
      // The Branch Unit redirects the PC immediately, but delays the mispredict
      // signal a cycle (for critical path reasons)

      // Did I just get killed by the previous cycle's branch,
      // or by a flush pipeline?
      val killed = WireInit(false.B)
      when (io.req.bits.kill ||
            (io.brinfo.valid &&
               io.brinfo.mispredict &&
               maskMatch(io.brinfo.mask, io.req.bits.uop.br_mask)
            ))
      {
         killed := true.B
      }

      val rs1 = io.req.bits.rs1_data
      val rs2 = io.req.bits.rs2_data
      val br_eq  = (rs1 === rs2)
      val br_ltu = (rs1.asUInt < rs2.asUInt)
      val br_lt  = (~(rs1(xLen-1) ^ rs2(xLen-1)) & br_ltu |
                      rs1(xLen-1) & ~rs2(xLen-1)).toBool

      val pc_plus4 = (uop_pc_ + Mux(io.req.bits.uop.is_rvc, 2.U, 4.U))(vaddrBitsExtended-1,0)

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

      val bj_addr = Wire(UInt(vaddrBitsExtended.W))

      val is_taken = io.req.valid &&
                     !killed &&
                     uop.is_br_or_jmp &&
                     (pc_sel =/= PC_PLUS4)

      // "mispredict" means that a branch has been resolved and it must be killed
      val mispredict = WireInit(false.B)

      val is_br          = io.req.valid && !killed && uop.is_br_or_jmp && !uop.is_jump
      val is_br_or_jalr  = io.req.valid && !killed && uop.is_br_or_jmp && !uop.is_jal

      // did the BTB predict a br or jmp incorrectly?
      // (do we need to reset its history and teach it a new target?)
      val btb_mispredict = WireInit(false.B)

      // did the bpd predict incorrectly (aka, should we correct its prediction?)
      val bpd_mispredict = WireInit(false.B)

      // if b/j is taken, does it go to the wrong target?
      val wrong_taken_target = !io.get_ftq_pc.next_val || (io.get_ftq_pc.next_pc =/= bj_addr)

      if (DEBUG_PRINTF)
      {
         printf("  BR-UNIT: PC: 0x%x+%x, Next: %d, 0x%x ,bj_addr: 0x%x\n",
            io.get_ftq_pc.fetch_pc, io.req.bits.uop.pc_lob, io.get_ftq_pc.next_val, io.get_ftq_pc.next_pc, bj_addr)
      }
      when (io.req.valid && uop.is_jal && io.get_ftq_pc.next_val && io.get_ftq_pc.next_pc =/= bj_addr) 
      {
         printf("[func] JAL went to the wrong target [curr: 0x%x+%x next: 0x%x, target: 0x%x]",
            io.get_ftq_pc.fetch_pc, io.req.bits.uop.pc_lob, io.get_ftq_pc.next_pc, bj_addr)
      }
      assert (!(io.req.valid && uop.is_jal && io.get_ftq_pc.next_val && io.get_ftq_pc.next_pc =/= bj_addr),
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
            bpd_mispredict := false.B
         }
         when (pc_sel === PC_PLUS4)
         {
            btb_mispredict := uop.br_prediction.btb_hit && uop.br_prediction.btb_taken
            bpd_mispredict := uop.br_prediction.bpd_taken
         }
         when (pc_sel === PC_BRJMP)
         {
            btb_mispredict := wrong_taken_target ||
                              !uop.br_prediction.btb_hit ||
                              (uop.br_prediction.btb_hit && !uop.br_prediction.btb_taken)
            bpd_mispredict := !uop.br_prediction.bpd_taken
         }
      }

      when (is_br_or_jalr && pc_sel === PC_BRJMP && !mispredict && io.get_ftq_pc.next_val)
      {
         // ignore misaligned issues -- we'll catch that elsewhere as an exception.
         when (io.get_ftq_pc.next_pc(vaddrBits-1, log2Ceil(coreInstBytes)) =/=
               bj_addr(vaddrBits-1, log2Ceil(coreInstBytes)))
         {
            printf ("[FuncUnit] Branch jumped to 0x%x, should have jumped to 0x%x.\n",
               io.get_ftq_pc.next_pc, bj_addr)
         }
         assert (io.get_ftq_pc.next_pc(vaddrBits-1, log2Ceil(coreInstBytes)) ===
                 bj_addr(vaddrBits-1, log2Ceil(coreInstBytes)),
                 "[FuncUnit] branch is taken to the wrong target.")
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
      brinfo.mask           := 1.U << uop.br_tag
      brinfo.exe_mask       := GetNewBrMask(io.brinfo, uop.br_mask)
      brinfo.tag            := uop.br_tag
      brinfo.ftq_idx        := uop.ftq_idx
      brinfo.pc_lob         := uop.pc_lob
      brinfo.rob_idx        := uop.rob_idx
      brinfo.ldq_idx        := uop.ldq_idx
      brinfo.stq_idx        := uop.stq_idx
      brinfo.is_jr          := pc_sel === PC_JALR
      brinfo.cfi_type       := Mux(uop.is_jal, CfiType.jal,
                               Mux(pc_sel === PC_JALR, CfiType.jalr,
                               Mux(uop.is_br_or_jmp, CfiType.branch, CfiType.none)))
      brinfo.taken          := is_taken
      brinfo.btb_mispredict := btb_mispredict
      brinfo.bpd_mispredict := bpd_mispredict
      brinfo.btb_made_pred  := uop.br_prediction.btb_blame
      brinfo.bpd_made_pred  := uop.br_prediction.bpd_blame

      br_unit.brinfo := brinfo

      // updates the BTB same cycle as PC redirect
      val lsb = log2Ceil(fetchWidth*coreInstBytes)

      // did a branch or jalr occur AND did we mispredict? AND was it taken? (i.e., should we update the BTB)

      if (enableBTBContainsBranches)
      {
         br_unit.btb_update.valid := is_br_or_jalr && mispredict && is_taken && !uop.br_prediction.btb_hit
      }
      else
      {
         br_unit.btb_update.valid := is_br_or_jalr && mispredict && uop.is_jump
      }

      br_unit.btb_update.bits.pc      := io.get_ftq_pc.fetch_pc// tell the BTB which pc to tag check against
      br_unit.btb_update.bits.cfi_idx := Mux(io.req.bits.uop.edge_inst,
                                             0.U,
                                             (uop_pc_ >> log2Ceil(coreInstBytes)))
      br_unit.btb_update.bits.target  := (target.asSInt & (-coreInstBytes).S).asUInt
      br_unit.btb_update.bits.taken   := is_taken   // was this branch/jal/jalr "taken"
      br_unit.btb_update.bits.cfi_type :=
            Mux(uop.is_jal, CfiType.jal,
            Mux(uop.is_jump && !uop.is_jal, CfiType.jalr,
                CfiType.branch))
      br_unit.btb_update.bits.bpd_type :=
            Mux(uop.is_ret, BpredType.ret,
            Mux(uop.is_call, BpredType.call,
            Mux(uop.is_jump, BpredType.jump,
                BpredType.branch)))

      // Branch/Jump Target Calculation
      // we can't push this through the ALU though, b/c jalr needs both PC+4 and rs1+offset

      def encodeVirtualAddress(a0: UInt, ea: UInt) = if (vaddrBitsExtended == vaddrBits)
      {
        ea
      }
      else
      {
         // Efficient means to compress 64-bit VA into vaddrBits+1 bits.
         // (VA is bad if VA(vaddrBits) != VA(vaddrBits-1)).
         val a = a0.asSInt >> vaddrBits
         val msb = Mux(a === 0.S || a === -1.S, ea(vaddrBits), !ea(vaddrBits-1))
         Cat(msb, ea(vaddrBits-1,0))
      }

      val target_base = Mux(uop.uopc === uopJALR, io.req.bits.rs1_data.asSInt, uop_pc_.asSInt)
      val target_offset = imm_xprlen(20,0).asSInt
      val targetXlen = Wire(UInt(xLen.W))
      targetXlen  := (target_base + target_offset).asUInt

      bj_addr := (encodeVirtualAddress(targetXlen, targetXlen).asSInt & -2.S).asUInt

      br_unit.pc := uop_pc_

      // handle misaligned branch/jmp targets
      br_unit.xcpt.valid     := bj_addr(1) && !usingCompressed.B &&
                                io.req.valid && is_taken && !killed
      br_unit.xcpt.bits.uop  := uop
      br_unit.xcpt.bits.cause:= freechips.rocketchip.rocket.Causes.misaligned_fetch.U
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

   val r_val  = RegInit(VecInit(Seq.fill(num_stages) { false.B }))
   val r_data = Reg(Vec(num_stages, UInt(xLen.W)))
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
   io.resp.bits.fflags.valid := false.B
}

// passes in base+imm to calculate addresses, and passes store data, to the LSU
// for floating point, 65bit FP store-data needs to be decoded into 64bit FP form
class MemAddrCalcUnit(implicit p: Parameters)
   extends PipelinedFunctionalUnit(
      num_stages = 0,
      num_bypass_stages = 0,
      earliest_bypass_stage = 0,
      data_width = 65, // TODO enable this only if FP is enabled?
      is_branch_unit = false)(p)
   with freechips.rocketchip.rocket.constants.MemoryOpConstants
   with freechips.rocketchip.rocket.constants.ScalarOpConstants
{
   // perform address calculation
   val sum = (io.req.bits.rs1_data.asSInt + io.req.bits.uop.imm_packed(19,8).asSInt).asUInt
   val ea_sign = Mux(sum(vaddrBits-1), ~sum(63,vaddrBits) === 0.U,
                                        sum(63,vaddrBits) =/= 0.U)
   val effective_address = Cat(ea_sign, sum(vaddrBits-1,0)).asUInt

   val store_data = io.req.bits.rs2_data

   io.resp.bits.addr := effective_address
   io.resp.bits.data := store_data

   if (data_width > 63)
   {
      assert (!(io.req.valid && io.req.bits.uop.ctrl.is_std &&
         io.resp.bits.data(64).toBool === true.B), "65th bit set in MemAddrCalcUnit.")

      assert (!(io.req.valid && io.req.bits.uop.ctrl.is_std && io.req.bits.uop.fp_val),
         "FP store-data should now be going through a different unit.")
   }

   assert (!(io.req.bits.uop.fp_val && io.req.valid && io.req.bits.uop.uopc =/=
           uopLD && io.req.bits.uop.uopc =/= uopSTA),
           "[maddrcalc] assert we never get store data in here.")

   // Handle misaligned exceptions
   val typ = io.req.bits.uop.mem_typ
   val misaligned =
      (((typ === MT_H) || (typ === MT_HU)) && (effective_address(0) =/= 0.U)) ||
      (((typ === MT_W) || (typ === MT_WU)) && (effective_address(1,0) =/= 0.U)) ||
      ((typ ===  MT_D) && (effective_address(2,0) =/= 0.U))

   val ma_ld = io.req.valid && io.req.bits.uop.uopc === uopLD && misaligned
   val ma_st = io.req.valid && (io.req.bits.uop.uopc === uopSTA || io.req.bits.uop.uopc === uopAMO_AG) && misaligned

   io.resp.bits.mxcpt.valid := ma_ld || ma_st
   io.resp.bits.mxcpt.bits  := Mux(ma_ld, freechips.rocketchip.rocket.Causes.misaligned_load.U,
                                          freechips.rocketchip.rocket.Causes.misaligned_store.U)
   assert (!(ma_ld && ma_st), "Mutually-exclusive exceptions are firing.")

   io.resp.bits.sfence.valid := io.req.valid && io.req.bits.uop.mem_cmd === M_SFENCE
   io.resp.bits.sfence.bits.rs1 := typ(0)
   io.resp.bits.sfence.bits.rs2 := typ(1)
   io.resp.bits.sfence.bits.addr := io.req.bits.rs1_data
   io.resp.bits.sfence.bits.asid := io.req.bits.rs2_data

}


// currently, bypassing is unsupported!
// All FP instructions are padded out to the max latency unit for easy
// write-port scheduling.
class FPUUnit(implicit p: Parameters) extends PipelinedFunctionalUnit(
   num_stages = p(tile.TileKey).core.fpu.get.dfmaLatency,
   num_bypass_stages = 0,
   earliest_bypass_stage = 0,
   data_width = 65)(p)
{
   val fpu = Module(new FPU())
   fpu.io.req.valid         := io.req.valid
   fpu.io.req.bits.uop      := io.req.bits.uop
   fpu.io.req.bits.rs1_data := io.req.bits.rs1_data
   fpu.io.req.bits.rs2_data := io.req.bits.rs2_data
   fpu.io.req.bits.rs3_data := io.req.bits.rs3_data
   fpu.io.req.bits.fcsr_rm  := io.fcsr_rm

   io.resp.bits.data              := fpu.io.resp.bits.data
   io.resp.bits.fflags.valid      := fpu.io.resp.bits.fflags.valid
   io.resp.bits.fflags.bits.uop   := io.resp.bits.uop
   io.resp.bits.fflags.bits.flags := fpu.io.resp.bits.fflags.bits.flags // kill me now
}

class IntToFPUnit(latency: Int)(implicit p: Parameters) extends PipelinedFunctionalUnit(
   num_stages = latency,
   num_bypass_stages = 0,
   earliest_bypass_stage = 0,
   data_width = 65)(p)
   with tile.HasFPUParameters
{
   val fp_decoder = Module(new UOPCodeFPUDecoder) // TODO use a simpler decoder
   val io_req = io.req.bits
   fp_decoder.io.uopc := io_req.uop.uopc
   val fp_ctrl = fp_decoder.io.sigs
   val fp_rm = Mux(ImmGenRm(io_req.uop.imm_packed) === 7.U, io.fcsr_rm, ImmGenRm(io_req.uop.imm_packed))
   val req = Wire(new tile.FPInput)
   val tag = !fp_ctrl.singleIn

   req <> fp_ctrl

   req.rm := fp_rm
   req.in1 := unbox(io_req.rs1_data, tag, None)
   req.in2 := unbox(io_req.rs2_data, tag, None)
   req.in3 := DontCare
   req.typ := ImmGenTyp(io_req.uop.imm_packed)
   req.fmaCmd := DontCare

   assert (!(io.req.valid && fp_ctrl.fromint && req.in1(xLen).toBool),
      "[func] IntToFP integer input has 65th high-order bit set!")

   assert (!(io.req.valid && !fp_ctrl.fromint),
      "[func] Only support fromInt micro-ops.")

   val ifpu = Module(new tile.IntToFP(intToFpLatency))
   ifpu.io.in.valid := io.req.valid
   ifpu.io.in.bits := req
   ifpu.io.in.bits.in1 := io_req.rs1_data
   val out_double = Pipe(io.req.valid, !fp_ctrl.singleOut, intToFpLatency).bits

//   io.resp.bits.data              := box(ifpu.io.out.bits.data, !io.resp.bits.uop.fp_single)
   io.resp.bits.data              := box(ifpu.io.out.bits.data, out_double)
   io.resp.bits.fflags.valid      := ifpu.io.out.valid
   io.resp.bits.fflags.bits.uop   := io.resp.bits.uop
   io.resp.bits.fflags.bits.flags := ifpu.io.out.bits.exc
}


// Iterative/unpipelined, can only hold a single MicroOp at a time TODO allow up to N micro-ops simultaneously.
// assumes at least one register between request and response
abstract class IterativeFunctionalUnit(data_width: Int)(implicit p: Parameters)
                                       extends FunctionalUnit(is_pipelined = false
                                                            , num_stages = 1
                                                            , num_bypass_stages = 0
                                                            , data_width = data_width
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

class DivUnit(data_width: Int)(implicit p: Parameters) extends IterativeFunctionalUnit(data_width)(p)
{

   // We don't use the iterative multiply functionality here.
   // Instead we use the PipelinedMultiplier
   val div = Module(new freechips.rocketchip.rocket.MulDiv(mulDivParams, width = data_width))

   // request
   div.io.req.valid    := io.req.valid && !this.do_kill
   div.io.req.bits.dw  := io.req.bits.uop.ctrl.fcn_dw
   div.io.req.bits.fn  := io.req.bits.uop.ctrl.op_fcn
   div.io.req.bits.in1 := io.req.bits.rs1_data
   div.io.req.bits.in2 := io.req.bits.rs2_data
   div.io.req.bits.tag := DontCare
   io.req.ready        := div.io.req.ready

   // handle pipeline kills and branch misspeculations
   div.io.kill         := this.do_kill

   // response
   io.resp.valid       := div.io.resp.valid && !this.do_kill
   div.io.resp.ready   := io.resp.ready
   io.resp.bits.data   := div.io.resp.bits.data
}

class PipelinedMulUnit(num_stages: Int, data_width: Int)(implicit p: Parameters)
      extends PipelinedFunctionalUnit (num_stages = num_stages
                                      , num_bypass_stages = 0
                                      , earliest_bypass_stage = 0
                                      , data_width = data_width)(p)
{
   val imul = Module(new PipelinedMultiplier(xLen, num_stages))
   // request
   imul.io.req.valid    := io.req.valid
   imul.io.req.bits.fn  := io.req.bits.uop.ctrl.op_fcn
   imul.io.req.bits.dw  := io.req.bits.uop.ctrl.fcn_dw
   imul.io.req.bits.in1 := io.req.bits.rs1_data
   imul.io.req.bits.in2 := io.req.bits.rs2_data
   imul.io.req.bits.tag := DontCare
   // response
   io.resp.bits.data    := imul.io.resp.bits.data
}
