//**************************************************************************
// Functional Units
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2013 Mar 10
//
// If regfile bypassing is disabled, then the functional unit must do its own
// bypassing in here on the WB stage (i.e., bypassing the io.resp.data)

// QUESTIONS
//    is there a way to have a plug-able output bundle?  (adder_out...)
//
//    is there a way to pass in the FU logic itself? or do I have to wrap all possible instances?


package BOOM
{

import Chisel._
import Node._

import rocket.ALU._
import rocket.Util._
import rocket.BuildFPU
import uncore.constants.MemoryOpConstants._


object FUCode
{
   // bit mask, since a given execution pipeline may support multiple functional units
   val FUC_SZ = 8
   val FU_X    = Bits("b???????", FUC_SZ)
   val FU_ALU  = Bits( 1, FUC_SZ)
   val FU_BRU  = Bits( 2, FUC_SZ)
   val FU_CNTR = Bits( 4, FUC_SZ)
   val FU_PCR  = Bits( 8, FUC_SZ)
   val FU_MULD = Bits(16, FUC_SZ)
   val FU_MEM  = Bits(32, FUC_SZ)
   val FU_FPU  = Bits(64, FUC_SZ)
}
import FUCode._

// TODO if a branch unit... how to add extra to IO in subclass?

class FunctionalUnitIo(num_stages: Int
                      , num_bypass_stages: Int
                      , data_width: Int
                      ) extends BOOMCoreBundle
{
   val req     = (new DecoupledIO(new FuncUnitReq(data_width))).flip
   val resp    = (new DecoupledIO(new FuncUnitResp(data_width)))

   val brinfo  = new BrResolutionInfo().asInput()

   val bypass  = new BypassData(num_bypass_stages, data_width).asOutput()

   val br_unit = new BranchUnitResp().asOutput

   // only used by the fpu unit
   val fcsr_rm = Bits(INPUT, rocket.FPConstants.RM_SZ)

   val get_rob_pc = new Bundle
   {
      val rob_idx = UInt(OUTPUT, ROB_ADDR_SZ)
      val curr_pc = UInt(INPUT, xprLen)
      val next_val= Bool(INPUT)
      val next_pc = UInt(INPUT, xprLen)
   }
}

class FuncUnitReq(data_width: Int) extends BOOMCoreBundle
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

   override def clone = new FuncUnitReq(data_width).asInstanceOf[this.type]
}

class FuncUnitResp(data_width: Int) extends BOOMCoreBundle
{
   val uop = new MicroOp()
   val data = Bits(width = data_width)
   val xcpt = (new rocket.HellaCacheExceptions)
   val exc  = Bits(width = 5) // FP-only TODO consolidate exception bits?

   override def clone = new FuncUnitResp(data_width).asInstanceOf[this.type]
}

class BypassData(num_bypass_ports: Int, data_width: Int) extends BOOMCoreBundle
{
   val valid = Vec.fill(num_bypass_ports){ Bool() }
   val uop   = Vec.fill(num_bypass_ports){ new MicroOp() }
   val data  = Vec.fill(num_bypass_ports){ Bits(width = data_width) }

   def get_num_ports: Int = num_bypass_ports
}

class BranchUnitResp extends BOOMCoreBundle
{
   val take_pc         = Bool()
   val target          = UInt(width = xprLen)
   val taken           = Bool()

   val pc              = UInt(width = xprLen) // TODO this isn't really a branch_unit thing

   val brinfo          = new BrResolutionInfo() // NOTE: delayed a cycle!
   val btb_update_valid= Bool() // TODO turn this into a directed bundle so we can fold this into btb_update?
   val btb_update      = new rocket.BTBUpdate
   val bht_update      = Valid(new rocket.BHTUpdate)

   val debug_btb_pred  = Bool() // just for debug, did the BTB and BHT predict taken?
}

abstract class FunctionalUnit(is_pipelined: Boolean
                              , num_stages: Int
                              , num_bypass_stages: Int
                              , data_width: Int
                              , has_branch_unit: Boolean = false)
                              extends Module
{
   val io = new FunctionalUnitIo(num_stages, num_bypass_stages, data_width)
//   val data_width = params(XPRLEN)
}


// Note: this helps track which uops get killed while in intermediate stages,
// but it is the job of the consumer to check for kills on the same cycle as consumption!!!
abstract class PipelinedFunctionalUnit(val num_stages: Int,
                                       val num_bypass_stages: Int,
                                       val earliest_bypass_stage: Int,
                                       val data_width: Int,
                                       is_branch_unit: Boolean = false
                                      ) extends FunctionalUnit(is_pipelined = true
                                                              , num_stages = num_stages
                                                              , num_bypass_stages = num_bypass_stages
                                                              , data_width = data_width
                                                              , has_branch_unit = is_branch_unit)
{
   // pipelined functional unit is always ready
   io.req.ready := Bool(true)

   val r_valids = Vec.fill(num_stages) { Reg(init = Bool(false)) }
   val r_uops   = Vec.fill(num_stages) { Reg(outType =  new MicroOp()) }


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
      io.resp.valid    := r_valids(num_stages-1) && !IsKilledByBranch(io.brinfo, r_uops(num_stages-1)) // && !io.req.bits.kill
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

class ALUUnit(is_branch_unit: Boolean = false, num_stages: Int = 1)
             extends PipelinedFunctionalUnit(num_stages = num_stages
                                            , num_bypass_stages = num_stages
                                            , earliest_bypass_stage = 0
                                            , data_width = 64  //xprLen
                                            , is_branch_unit = is_branch_unit)
             with BOOMCoreParameters
{
   val uop = io.req.bits.uop

   // immediate generation
   val imm_xprlen = ImmGen(uop.imm_packed, uop.ctrl.imm_sel)

   // operand 1 select
   var op1_data: Bits = null
   if (is_branch_unit)
   {
      //io.get_rob_pc.rob_idx := uop.rob_idx
      op1_data = Mux(io.req.bits.uop.ctrl.op1_sel.toUInt === OP1_RS1 , io.req.bits.rs1_data,
                 Mux(io.req.bits.uop.ctrl.op1_sel.toUInt === OP1_PC  , io.get_rob_pc.curr_pc,
                                                                       UInt(0)))
   }
   else
   {
      op1_data = Mux(io.req.bits.uop.ctrl.op1_sel.toUInt === OP1_RS1 , io.req.bits.rs1_data,
                                                                       UInt(0))
   }

   // operand 2 select
   val op2_data = Mux(io.req.bits.uop.ctrl.op2_sel === OP2_IMM,  Sext(imm_xprlen, xprLen),
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
      val killed = Bool()
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
      val br_lt  = (~(rs1(xprLen-1) ^ rs2(xprLen-1)) & br_ltu |
                      rs1(xprLen-1) & ~rs2(xprLen-1)).toBool

      val pc_plus4 = (uop_pc_ + UInt(4))(xprLen-1,0)

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

      val bj_addr = UInt()
//      val jreg_target = bj_addr

      io.br_unit.taken := io.req.valid &&
                          !killed &&
                          uop.is_br_or_jmp &&
                          (pc_sel != PC_PLUS4)

      // JAL is taken in the front-end, so it should never mispredict
      val mispredict = io.req.valid &&
                       !killed &&
                       uop.is_br_or_jmp &&
                       !(uop.is_jal) && // TODO XXX is this the proper way to do this? can we remove more JAL stuff from the branch unit? jal should just be a NOP, except it needs the PC for wb
                                        // TODO treat a JAL with rd=x0 differently, squash away in decode.
//                       ((io.br_unit.taken ^ uop.btb_pred_taken) || // BTB was wrong this assumes BTB doesn't say "taken" for PC+4
                       (// BTB was wrong
                          (pc_sel === PC_PLUS4 && (uop.btb_hit && uop.btb_resp.taken)) ||
                          (pc_sel != PC_PLUS4 && !(uop.btb_hit && uop.btb_resp.taken)) ||
                          (pc_sel === PC_JALR && (!io.get_rob_pc.next_val || (io.get_rob_pc.next_pc != bj_addr))) // BTB was right, but wrong target for JALR
                       )

      // TODO assert is there a way to verify the branch prediction jumped to the correct address?
      //val bad_jmp_target_error = io.req.valid && uop.is_br_or_jmp && !uop.is_jump && (bj_addr != ???)
      //assert (!(br_bad_jmp_target_error), "Branch jumped to the wrong target address.")


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

      // updates the BTB same cycle as PC redirect
      val lsb = log2Ceil(FETCH_WIDTH*coreInstBytes)
      // TODO remove jal from redirecting the PC? it should NEVER "mispredict!"

      // did a branch or jalr occur AND did we mispredict? AND was it taken? (i.e., should we update the BTB)
      io.br_unit.btb_update_valid            := io.req.valid && uop.is_br_or_jmp && !uop.is_jal && mispredict && io.br_unit.taken && !killed
      io.br_unit.btb_update.pc               := ((uop_pc_ >> lsb) << lsb) + uop.fetch_pc_lob // what pc should the tag check be on?
      io.br_unit.btb_update.br_pc            := uop_pc_
      io.br_unit.btb_update.target           := io.br_unit.target & SInt(-coreInstBytes) //bj_addr // what should the target be on the tag hit?
      io.br_unit.btb_update.prediction.valid := uop.btb_resp_valid // did this branch's fetch packet  have a BTB hit in fetch?
      io.br_unit.btb_update.prediction.bits  := uop.btb_resp       // give the BTB back its BTBResp
      io.br_unit.btb_update.taken            := io.br_unit.taken   // was this branch "taken"
      io.br_unit.btb_update.isJump           := uop.is_jump
      io.br_unit.btb_update.isReturn         := uop.is_ret

      io.br_unit.bht_update.valid                 := io.req.valid && uop.is_br_or_jmp && !uop.is_jump && !killed // update on all branches
      io.br_unit.bht_update.bits.taken            := io.br_unit.taken   // was this branch "taken"
      io.br_unit.bht_update.bits.mispredict       := mispredict
      io.br_unit.bht_update.bits.prediction.valid := uop.btb_resp_valid
      io.br_unit.bht_update.bits.prediction.bits  := uop.btb_resp
      io.br_unit.bht_update.bits.pc               := ((uop_pc_ >> lsb) << lsb) + uop.fetch_pc_lob // what pc should the tag check be on?


      // Branch/Jump Target Calculation
      // we can't push this through the ALU though, b/c jalr needs both PC+4 and rs1+offset

      def vaSign(a0: UInt, ea: Bits) = {
         // efficient means to compress 64-bit VA into rc.as.vaddrBits+1 bits
         // (VA is bad if VA(rc.as.vaddrBits) != VA(rc.as.vaddrBits-1))
         val a = a0 >> vaddrBits-1
         val e = ea(vaddrBits,vaddrBits-1)
         Mux(a === UInt(0) || a === UInt(1), e != UInt(0),
         Mux(a === SInt(-1) || a === SInt(-2), e === SInt(-1),
            e(0)))
      }

      val bj_base = Mux(uop.uopc === uopJALR, io.req.bits.rs1_data, uop_pc_)
      val bj_offset = imm_xprlen(20,0).toSInt
      val bj64 = bj_base + bj_offset
      val bj_msb = Mux(uop.uopc === uopJALR, vaSign(io.req.bits.rs1_data, bj64), vaSign(uop_pc_, bj64))
      bj_addr := Cat(bj_msb, bj64(vaddrBits-1,0))

      io.br_unit.pc             := uop_pc_
      io.br_unit.debug_btb_pred := uop.btb_resp_valid && uop.btb_resp.taken
   }

   // Response
   // TODO add clock gate on resp bits from functional units
//   io.resp.bits.data := RegEnable(alu.io.out, io.req.valid)
//   val reg_data = Reg(outType = Bits(width = xprLen))
//   reg_data := alu.io.out
//   io.resp.bits.data := reg_data

   val r_val  = Vec.fill(num_stages) { Reg(init = Bool(false)) }
   val r_data = Vec.fill(num_stages) { Reg(Bits(xprLen)) }
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
   io.resp.bits.xcpt.ma.ld := Bool(false)
   io.resp.bits.xcpt.ma.st := Bool(false)
   io.resp.bits.xcpt.pf.ld := Bool(false)
   io.resp.bits.xcpt.pf.st := Bool(false)
}


// passes in base+imm to calculate addresses, and passes store data, to the LSU
// for floating point, 65bit FP store-data needs to be decoded into 64bit FP form
class MemAddrCalcUnit extends PipelinedFunctionalUnit(num_stages = 0
                                                     , num_bypass_stages = 0
                                                     , earliest_bypass_stage = 0
                                                     , data_width = 65 // TODO enable this only if FP is enabled?
                                                     , is_branch_unit = false) with BOOMCoreParameters
{
   // perform address calculation
   val alu = Module(new rocket.ALU())

   alu.io.in1 := io.req.bits.rs1_data.toUInt
   alu.io.in2 := Mux(io.req.bits.uop.ctrl.op2_sel === OP2_ZERO,
                     SInt(0),
                     io.req.bits.uop.imm_packed(19,8).toSInt) //uses special packed imm format

   alu.io.fn  := FN_ADD
   alu.io.dw  := DW_XPR

   val adder_out = alu.io.adder_out
   val ea_sign = Mux(adder_out(vaddrBits-1), ~adder_out(63,vaddrBits) === UInt(0),
                                                    adder_out(63,vaddrBits) != UInt(0))
   val effective_address = Cat(ea_sign, adder_out(vaddrBits-1,0)).toUInt

   // compute store data
   // requires decoding 65-bit FP data
   val unrec_s = hardfloat.recodedFloatNToFloatN(io.req.bits.rs2_data, 23, 9)
   val unrec_d = hardfloat.recodedFloatNToFloatN(io.req.bits.rs2_data, 52, 12)
   val unrec_out = Mux(io.req.bits.uop.fp_single, Cat(Fill(32, unrec_s(31)), unrec_s), unrec_d)

   var store_data:Bits = null
   if (params(BuildFPU).isEmpty)
      store_data = io.req.bits.rs2_data
   else
      store_data = Mux(io.req.bits.uop.fp_val, unrec_out
                                             , io.req.bits.rs2_data)

   // TODO only use one register read port
   io.resp.bits.data := Mux(io.req.bits.uop.ctrl.is_std, store_data
                                                       , effective_address)

   if (data_width > 63)
      assert (io.resp.bits.data(64).toBool === Bool(false), "65th bit set in MemCalcAddrUnit.")

   // Handle misaligned exceptions
   val typ = io.req.bits.uop.mem_typ
   val misaligned =
      (((typ === MT_H) || (typ === MT_HU)) && (effective_address(0) != Bits(0))) ||
      (((typ === MT_W) || (typ === MT_WU)) && (effective_address(1,0) != Bits(0))) ||
      ((typ === MT_D) && (effective_address(2,0) != Bits(0)))

   val ma_ld = io.req.valid && io.req.bits.uop.uopc === uopLD && misaligned
   val ma_st = io.req.valid && (io.req.bits.uop.uopc === uopSTA || io.req.bits.uop.uopc === uopAMO_AG) && misaligned

   io.resp.bits.xcpt.ma.ld := ma_ld
   io.resp.bits.xcpt.ma.st := ma_st
   io.resp.bits.xcpt.pf.ld := Bool(false)
   io.resp.bits.xcpt.pf.st := Bool(false)
}



// currently, bypassing is unsupported!
// All FP instructions are padded out to the max latency unit for easy
// write-port scheduling.
class FPUUnit extends PipelinedFunctionalUnit(num_stages = 3
                                            , num_bypass_stages = 0
                                            , earliest_bypass_stage = 0
                                            , data_width = 65)
              with BOOMCoreParameters
{
//   val uop = io.req.bits.uop

   val fpu = Module(new FPU())

   // TODO just straight up hook up the req fpu.io.req <> io.req ?
   fpu.io.valid   := io.req.valid
   fpu.io.uop     := io.req.bits.uop
   fpu.io.in1     := io.req.bits.rs1_data
   fpu.io.in2     := io.req.bits.rs2_data
   fpu.io.in3     := io.req.bits.rs3_data
   fpu.io.fcsr_rm := io.fcsr_rm


   // Response
//   val reg_data = Reg(Bits(width=65))
//   reg_data := fpu.io.resp.bits.data
//   io.resp.bits.data := reg_data
//   io.resp <> fpu.io.resp
   // TODO how close can I match these I/Os?
   io.resp.bits.data := fpu.io.resp.bits.data
   io.resp.bits.exc := fpu.io.resp.bits.exc
}




// unpipelined, can only hold a single MicroOp at a time
// assumes at least one register between request and response
abstract class UnPipelinedFunctionalUnit
                                       extends FunctionalUnit(is_pipelined = false
                                                            , num_stages = 1
                                                            , num_bypass_stages = 0
                                                            , data_width = 64
                                                            , has_branch_unit = false) with BOOMCoreParameters
{
   val r_uop = Reg(outType = new MicroOp())

   val do_kill = Bool()
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


class MulDivUnit extends UnPipelinedFunctionalUnit with BOOMCoreParameters
{
   val muldiv = Module(new rocket.MulDiv(mulUnroll = if (fastMulDiv) 8 else 1, earlyOut = fastMulDiv))

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

   // exceptions
   io.resp.bits.xcpt.ma.ld := Bool(false)
   io.resp.bits.xcpt.ma.st := Bool(false)
   io.resp.bits.xcpt.pf.ld := Bool(false)
   io.resp.bits.xcpt.pf.st := Bool(false)
}

}

