//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
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

package boom.v3.exu

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._
import freechips.rocketchip.tile
import freechips.rocketchip.rocket.{PipelinedMultiplier,BP,BreakpointUnit,Causes,CSR}

import boom.v3.common._
import boom.v3.ifu._
import boom.v3.util._

/**t
 * Functional unit constants
 */
object FUConstants
{
  // bit mask, since a given execution pipeline may support multiple functional units
  val FUC_SZ = 13
  val FU_X   = BitPat.dontCare(FUC_SZ)
  val FU_ALU =   1.U(FUC_SZ.W)
  val FU_JMP =   2.U(FUC_SZ.W)
  val FU_MEM =   4.U(FUC_SZ.W)
  val FU_MUL =   8.U(FUC_SZ.W)
  val FU_DIV =  16.U(FUC_SZ.W)
  val FU_CSR =  32.U(FUC_SZ.W)
  val FU_FPU =  64.U(FUC_SZ.W)
  val FU_FDV = 128.U(FUC_SZ.W)
  val FU_I2F = 256.U(FUC_SZ.W)
  val FU_F2I = 512.U(FUC_SZ.W)
  val FU_MULE2N = 1024.U(FUC_SZ.W)
  val FU_MULE3N = 2048.U(FUC_SZ.W)
  val FU_MULE5N = 4096.U(FUC_SZ.W)

  // FP stores generate data through FP F2I, and generate address through MemAddrCalc
  val FU_F2IMEM = 516.U(FUC_SZ.W)
}
import FUConstants._

/**
 * Class to tell the FUDecoders what units it needs to support
 *
 * @param alu support alu unit?
 * @param bru support br unit?
 * @param mem support mem unit?
 * @param muld support multiple div unit?
 * @param fpu support FP unit?
 * @param csr support csr writing unit?
 * @param fdiv support FP div unit?
 * @param ifpu support int to FP unit?
 */
class SupportedFuncUnits(
  val alu: Boolean  = false,
  val jmp: Boolean  = false,
  val mem: Boolean  = false,
  val muld: Boolean = false,
  val fpu: Boolean  = false,
  val csr: Boolean  = false,
  val fdiv: Boolean = false,
  val ifpu: Boolean = false)
{
}


/**
 * Bundle for signals sent to the functional unit
 *
 * @param dataWidth width of the data sent to the functional unit
 */
class FuncUnitReq(val dataWidth: Int)(implicit p: Parameters) extends BoomBundle
  with HasBoomUOP
{
  val numOperands = 3

  val rs1_data = UInt(dataWidth.W)
  val rs2_data = UInt(dataWidth.W)
  val rs3_data = UInt(dataWidth.W) // only used for FMA units
  val pred_data = Bool()

  val kill = Bool() // kill everything
}

/**
 * Bundle for the signals sent out of the function unit
 *
 * @param dataWidth data sent from the functional unit
 */
class FuncUnitResp(val dataWidth: Int)(implicit p: Parameters) extends BoomBundle
  with HasBoomUOP
{
  val predicated = Bool() // Was this response from a predicated-off instruction
  val data = UInt(dataWidth.W)
  val fflags = new ValidIO(new FFlagsResp)
  val addr = UInt((vaddrBits+1).W) // only for maddr -> LSU
  val mxcpt = new ValidIO(UInt((freechips.rocketchip.rocket.Causes.all.max+2).W)) //only for maddr->LSU
  val sfence = Valid(new freechips.rocketchip.rocket.SFenceReq) // only for mcalc
}

/**
 * Branch resolution information given from the branch unit
 */
class BrResolutionInfo(implicit p: Parameters) extends BoomBundle
{
  val uop        = new MicroOp
  val valid      = Bool()
  val mispredict = Bool()
  val taken      = Bool()                     // which direction did the branch go?
  val cfi_type   = UInt(CFI_SZ.W)

  // Info for recalculating the pc for this branch
  val pc_sel     = UInt(2.W)

  val jalr_target = UInt(vaddrBitsExtended.W)
  val target_offset = SInt()
}

class BrUpdateInfo(implicit p: Parameters) extends BoomBundle
{
  // On the first cycle we get masks to kill registers
  val b1 = new BrUpdateMasks
  // On the second cycle we get indices to reset pointers
  val b2 = new BrResolutionInfo
}

class BrUpdateMasks(implicit p: Parameters) extends BoomBundle
{
  val resolve_mask = UInt(maxBrCount.W)
  val mispredict_mask = UInt(maxBrCount.W)
}


/**
 * Abstract top level functional unit class that wraps a lower level hand made functional unit
 *
 * @param isPipelined is the functional unit pipelined?
 * @param numStages how many pipeline stages does the functional unit have
 * @param numBypassStages how many bypass stages does the function unit have
 * @param dataWidth width of the data being operated on in the functional unit
 * @param hasBranchUnit does this functional unit have a branch unit?
 */
abstract class FunctionalUnit(
  val isPipelined: Boolean,
  val numStages: Int,
  val numBypassStages: Int,
  val dataWidth: Int,
  val isJmpUnit: Boolean = false,
  val isAluUnit: Boolean = false,
  val isMemAddrCalcUnit: Boolean = false,
  val needsFcsr: Boolean = false)
  (implicit p: Parameters) extends BoomModule
{
  val io = IO(new Bundle {
    val req    = Flipped(new DecoupledIO(new FuncUnitReq(dataWidth)))
    val resp   = (new DecoupledIO(new FuncUnitResp(dataWidth)))

    val brupdate = Input(new BrUpdateInfo())

    val bypass = Output(Vec(numBypassStages, Valid(new ExeUnitResp(dataWidth))))

    // only used by the fpu unit
    val fcsr_rm = if (needsFcsr) Input(UInt(tile.FPConstants.RM_SZ.W)) else null

    // only used by branch unit
    val brinfo     = if (isAluUnit) Output(new BrResolutionInfo()) else null
    val get_ftq_pc = if (isJmpUnit) Flipped(new GetPCFromFtqIO()) else null
    val status     = if (isMemAddrCalcUnit) Input(new freechips.rocketchip.rocket.MStatus()) else null

    // only used by memaddr calc unit
    val bp = if (isMemAddrCalcUnit) Input(Vec(nBreakpoints, new BP)) else null
    val mcontext = if (isMemAddrCalcUnit) Input(UInt(coreParams.mcontextWidth.W)) else null
    val scontext = if (isMemAddrCalcUnit) Input(UInt(coreParams.scontextWidth.W)) else null

  })

  io.bypass.foreach { b => b.valid := false.B; b.bits := DontCare }

  io.resp.valid := false.B
  io.resp.bits := DontCare

  if (isJmpUnit) {
    io.get_ftq_pc.ftq_idx := DontCare
  }
}

/**
 * Abstract top level pipelined functional unit
 *
 * Note: this helps track which uops get killed while in intermediate stages,
 * but it is the job of the consumer to check for kills on the same cycle as consumption!!!
 *
 * @param numStages how many pipeline stages does the functional unit have
 * @param numBypassStages how many bypass stages does the function unit have
 * @param earliestBypassStage first stage that you can start bypassing from
 * @param dataWidth width of the data being operated on in the functional unit
 * @param hasBranchUnit does this functional unit have a branch unit?
 */
abstract class PipelinedFunctionalUnit(
  numStages: Int,
  numBypassStages: Int,
  earliestBypassStage: Int,
  dataWidth: Int,
  isJmpUnit: Boolean = false,
  isAluUnit: Boolean = false,
  isMemAddrCalcUnit: Boolean = false,
  needsFcsr: Boolean = false
  )(implicit p: Parameters) extends FunctionalUnit(
    isPipelined = true,
    numStages = numStages,
    numBypassStages = numBypassStages,
    dataWidth = dataWidth,
    isJmpUnit = isJmpUnit,
    isAluUnit = isAluUnit,
    isMemAddrCalcUnit = isMemAddrCalcUnit,
    needsFcsr = needsFcsr)
{
  // Pipelined functional unit is always ready.
  io.req.ready := true.B

  if (numStages > 0) {
    val r_valids = RegInit(VecInit(Seq.fill(numStages) { false.B }))
    val r_uops   = Reg(Vec(numStages, new MicroOp()))

    // handle incoming request
    r_valids(0) := io.req.valid && !IsKilledByBranch(io.brupdate, io.req.bits.uop) && !io.req.bits.kill
    r_uops(0)   := io.req.bits.uop
    r_uops(0).br_mask := GetNewBrMask(io.brupdate, io.req.bits.uop)

    // handle middle of the pipeline
    for (i <- 1 until numStages) {
      r_valids(i) := r_valids(i-1) && !IsKilledByBranch(io.brupdate, r_uops(i-1)) && !io.req.bits.kill
      r_uops(i)   := r_uops(i-1)
      r_uops(i).br_mask := GetNewBrMask(io.brupdate, r_uops(i-1))

      if (numBypassStages > 0) {
        io.bypass(i-1).bits.uop := r_uops(i-1)
      }
    }

    // handle outgoing (branch could still kill it)
    // consumer must also check for pipeline flushes (kills)
    io.resp.valid    := r_valids(numStages-1) && !IsKilledByBranch(io.brupdate, r_uops(numStages-1))
    io.resp.bits.predicated := false.B
    io.resp.bits.uop := r_uops(numStages-1)
    io.resp.bits.uop.br_mask := GetNewBrMask(io.brupdate, r_uops(numStages-1))

    // bypassing (TODO allow bypass vector to have a different size from numStages)
    if (numBypassStages > 0 && earliestBypassStage == 0) {
      io.bypass(0).bits.uop := io.req.bits.uop

      for (i <- 1 until numBypassStages) {
        io.bypass(i).bits.uop := r_uops(i-1)
      }
    }
  } else {
    require (numStages == 0)
    // pass req straight through to response

    // valid doesn't check kill signals, let consumer deal with it.
    // The LSU already handles it and this hurts critical path.
    io.resp.valid    := io.req.valid && !IsKilledByBranch(io.brupdate, io.req.bits.uop)
    io.resp.bits.predicated := false.B
    io.resp.bits.uop := io.req.bits.uop
    io.resp.bits.uop.br_mask := GetNewBrMask(io.brupdate, io.req.bits.uop)
  }
}

/**
 * Functional unit that wraps RocketChips ALU
 *
 * @param isBranchUnit is this a branch unit?
 * @param numStages how many pipeline stages does the functional unit have
 * @param dataWidth width of the data being operated on in the functional unit
 */
class ALUUnit(isJmpUnit: Boolean = false, numStages: Int = 1, dataWidth: Int)(implicit p: Parameters)
  extends PipelinedFunctionalUnit(
    numStages = numStages,
    numBypassStages = numStages,
    isAluUnit = true,
    earliestBypassStage = 0,
    dataWidth = dataWidth,
    isJmpUnit = isJmpUnit)
  with boom.v3.ifu.HasBoomFrontendParameters
{
  val uop = io.req.bits.uop

  // immediate generation
  val imm_xprlen = ImmGen(uop.imm_packed, uop.ctrl.imm_sel)

  // operand 1 select
  var op1_data: UInt = null
  if (isJmpUnit) {
    // Get the uop PC for jumps
    val block_pc = AlignPCToBoundary(io.get_ftq_pc.pc, icBlockBytes)
    val uop_pc = (block_pc | uop.pc_lob) - Mux(uop.edge_inst, 2.U, 0.U)

    op1_data = Mux(uop.ctrl.op1_sel.asUInt === OP1_RS1 , io.req.bits.rs1_data,
               Mux(uop.ctrl.op1_sel.asUInt === OP1_PC  , Sext(uop_pc, xLen),
                                                         0.U))
  } else {
    op1_data = Mux(uop.ctrl.op1_sel.asUInt === OP1_RS1 , io.req.bits.rs1_data,
                                                         0.U)
  }

  // operand 2 select
  val op2_data = Mux(uop.ctrl.op2_sel === OP2_IMM,  Sext(imm_xprlen.asUInt, xLen),
                 Mux(uop.ctrl.op2_sel === OP2_IMMC, io.req.bits.uop.prs1(4,0),
                 Mux(uop.ctrl.op2_sel === OP2_RS2 , io.req.bits.rs2_data,
                 Mux(uop.ctrl.op2_sel === OP2_NEXT, Mux(uop.is_rvc, 2.U, 4.U),
                                                    0.U))))

  val alu = Module(new freechips.rocketchip.rocket.ALU())

  alu.io.in1 := op1_data.asUInt
  alu.io.in2 := op2_data.asUInt
  alu.io.fn  := uop.ctrl.op_fcn
  alu.io.dw  := uop.ctrl.fcn_dw


  // Did I just get killed by the previous cycle's branch,
  // or by a flush pipeline?
  val killed = WireInit(false.B)
  when (io.req.bits.kill || IsKilledByBranch(io.brupdate, uop)) {
    killed := true.B
  }

  val rs1 = io.req.bits.rs1_data
  val rs2 = io.req.bits.rs2_data
  val br_eq  = (rs1 === rs2)
  val br_ltu = (rs1.asUInt < rs2.asUInt)
  val br_lt  = (~(rs1(xLen-1) ^ rs2(xLen-1)) & br_ltu |
                rs1(xLen-1) & ~rs2(xLen-1)).asBool

  val pc_sel = MuxLookup(uop.ctrl.br_type, PC_PLUS4)(
                 Seq(   BR_N   -> PC_PLUS4,
                        BR_NE  -> Mux(!br_eq,  PC_BRJMP, PC_PLUS4),
                        BR_EQ  -> Mux( br_eq,  PC_BRJMP, PC_PLUS4),
                        BR_GE  -> Mux(!br_lt,  PC_BRJMP, PC_PLUS4),
                        BR_GEU -> Mux(!br_ltu, PC_BRJMP, PC_PLUS4),
                        BR_LT  -> Mux( br_lt,  PC_BRJMP, PC_PLUS4),
                        BR_LTU -> Mux( br_ltu, PC_BRJMP, PC_PLUS4),
                        BR_J   -> PC_BRJMP,
                        BR_JR  -> PC_JALR
                        ))

  val is_taken = io.req.valid &&
                   !killed &&
                   (uop.is_br || uop.is_jalr || uop.is_jal) &&
                   (pc_sel =/= PC_PLUS4)

  // "mispredict" means that a branch has been resolved and it must be killed
  val mispredict = WireInit(false.B)

  val is_br          = io.req.valid && !killed && uop.is_br && !uop.is_sfb
  val is_jal         = io.req.valid && !killed && uop.is_jal
  val is_jalr        = io.req.valid && !killed && uop.is_jalr

  when (is_br || is_jalr) {
    if (!isJmpUnit) {
      assert (pc_sel =/= PC_JALR)
    }
    when (pc_sel === PC_PLUS4) {
      mispredict := uop.taken
    }
    when (pc_sel === PC_BRJMP) {
      mispredict := !uop.taken
    }
  }

  val brinfo = Wire(new BrResolutionInfo)

  // note: jal doesn't allocate a branch-mask, so don't clear a br-mask bit
  brinfo.valid          := is_br || is_jalr
  brinfo.mispredict     := mispredict
  brinfo.uop            := uop
  brinfo.cfi_type       := Mux(is_jalr, CFI_JALR,
                           Mux(is_br  , CFI_BR, CFI_X))
  brinfo.taken          := is_taken
  brinfo.pc_sel         := pc_sel

  brinfo.jalr_target    := DontCare


  // Branch/Jump Target Calculation
  // For jumps we read the FTQ, and can calculate the target
  // For branches we emit the offset for the core to redirect if necessary
  val target_offset = imm_xprlen(20,0).asSInt
  brinfo.jalr_target := DontCare
  if (isJmpUnit) {
    def encodeVirtualAddress(a0: UInt, ea: UInt) = if (vaddrBitsExtended == vaddrBits) {
      ea
    } else {
      // Efficient means to compress 64-bit VA into vaddrBits+1 bits.
      // (VA is bad if VA(vaddrBits) != VA(vaddrBits-1)).
      val a = a0.asSInt >> vaddrBits
      val msb = Mux(a === 0.S || a === -1.S, ea(vaddrBits), !ea(vaddrBits-1))
      Cat(msb, ea(vaddrBits-1,0))
    }


    val jalr_target_base = io.req.bits.rs1_data.asSInt
    val jalr_target_xlen = Wire(UInt(xLen.W))
    jalr_target_xlen := (jalr_target_base + target_offset).asUInt
    val jalr_target = (encodeVirtualAddress(jalr_target_xlen, jalr_target_xlen).asSInt & -2.S).asUInt

    brinfo.jalr_target := jalr_target
    val cfi_idx = ((uop.pc_lob ^ Mux(io.get_ftq_pc.entry.start_bank === 1.U, 1.U << log2Ceil(bankBytes), 0.U)))(log2Ceil(fetchWidth),1)

    when (pc_sel === PC_JALR) {
      mispredict := !io.get_ftq_pc.next_val ||
                    (io.get_ftq_pc.next_pc =/= jalr_target) ||
                    !io.get_ftq_pc.entry.cfi_idx.valid ||
                    (io.get_ftq_pc.entry.cfi_idx.bits =/= cfi_idx)
    }
  }

  brinfo.target_offset := target_offset


  io.brinfo := brinfo



// Response
// TODO add clock gate on resp bits from functional units
//   io.resp.bits.data := RegEnable(alu.io.out, io.req.valid)
//   val reg_data = Reg(outType = Bits(width = xLen))
//   reg_data := alu.io.out
//   io.resp.bits.data := reg_data

  val r_val  = RegInit(VecInit(Seq.fill(numStages) { false.B }))
  val r_data = Reg(Vec(numStages, UInt(xLen.W)))
  val r_pred = Reg(Vec(numStages, Bool()))
  val alu_out = Mux(io.req.bits.uop.is_sfb_shadow && io.req.bits.pred_data,
    Mux(io.req.bits.uop.ldst_is_rs1, io.req.bits.rs1_data, io.req.bits.rs2_data),
    Mux(io.req.bits.uop.uopc === uopMOV, io.req.bits.rs2_data, alu.io.out))
  r_val (0) := io.req.valid
  r_data(0) := Mux(io.req.bits.uop.is_sfb_br, pc_sel === PC_BRJMP, alu_out)
  r_pred(0) := io.req.bits.uop.is_sfb_shadow && io.req.bits.pred_data
  for (i <- 1 until numStages) {
    r_val(i)  := r_val(i-1)
    r_data(i) := r_data(i-1)
    r_pred(i) := r_pred(i-1)
  }
  io.resp.bits.data := r_data(numStages-1)
  io.resp.bits.predicated := r_pred(numStages-1)
  // Bypass
  // for the ALU, we can bypass same cycle as compute
  require (numStages >= 1)
  require (numBypassStages >= 1)
  io.bypass(0).valid := io.req.valid
  io.bypass(0).bits.data := Mux(io.req.bits.uop.is_sfb_br, pc_sel === PC_BRJMP, alu_out)
  for (i <- 1 until numStages) {
    io.bypass(i).valid := r_val(i-1)
    io.bypass(i).bits.data := r_data(i-1)
  }

  // Exceptions
  io.resp.bits.fflags.valid := false.B
}

/**
 * Functional unit that passes in base+imm to calculate addresses, and passes store data
 * to the LSU.
 * For floating point, 65bit FP store-data needs to be decoded into 64bit FP form
 */
class MemAddrCalcUnit(implicit p: Parameters)
  extends PipelinedFunctionalUnit(
    numStages = 0,
    numBypassStages = 0,
    earliestBypassStage = 0,
    dataWidth = 65, // TODO enable this only if FP is enabled?
    isMemAddrCalcUnit = true)
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

  if (dataWidth > 63) {
    assert (!(io.req.valid && io.req.bits.uop.ctrl.is_std &&
      io.resp.bits.data(64).asBool === true.B), "65th bit set in MemAddrCalcUnit.")

    assert (!(io.req.valid && io.req.bits.uop.ctrl.is_std && io.req.bits.uop.fp_val),
      "FP store-data should now be going through a different unit.")
  }

  assert (!(io.req.bits.uop.fp_val && io.req.valid && io.req.bits.uop.uopc =/=
          uopLD && io.req.bits.uop.uopc =/= uopSTA),
          "[maddrcalc] assert we never get store data in here.")

  // Handle misaligned exceptions
  val size = io.req.bits.uop.mem_size
  val misaligned =
    (size === 1.U && (effective_address(0) =/= 0.U)) ||
    (size === 2.U && (effective_address(1,0) =/= 0.U)) ||
    (size === 3.U && (effective_address(2,0) =/= 0.U))

  val bkptu = Module(new BreakpointUnit(nBreakpoints))
  bkptu.io.status   := io.status
  bkptu.io.bp       := io.bp
  bkptu.io.pc       := DontCare
  bkptu.io.ea       := effective_address
  bkptu.io.mcontext := io.mcontext
  bkptu.io.scontext := io.scontext

  val ma_ld  = io.req.valid && io.req.bits.uop.uopc === uopLD && misaligned
  val ma_st  = io.req.valid && (io.req.bits.uop.uopc === uopSTA || io.req.bits.uop.uopc === uopAMO_AG) && misaligned
  val dbg_bp = io.req.valid && ((io.req.bits.uop.uopc === uopLD  && bkptu.io.debug_ld) ||
                                (io.req.bits.uop.uopc === uopSTA && bkptu.io.debug_st))
  val bp     = io.req.valid && ((io.req.bits.uop.uopc === uopLD  && bkptu.io.xcpt_ld) ||
                                (io.req.bits.uop.uopc === uopSTA && bkptu.io.xcpt_st))

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))
  val (xcpt_val, xcpt_cause) = checkExceptions(List(
    (ma_ld,  (Causes.misaligned_load).U),
    (ma_st,  (Causes.misaligned_store).U),
    (dbg_bp, (CSR.debugTriggerCause).U),
    (bp,     (Causes.breakpoint).U)))

  io.resp.bits.mxcpt.valid := xcpt_val
  io.resp.bits.mxcpt.bits  := xcpt_cause
  assert (!(ma_ld && ma_st), "Mutually-exclusive exceptions are firing.")

  io.resp.bits.sfence.valid := io.req.valid && io.req.bits.uop.mem_cmd === M_SFENCE
  io.resp.bits.sfence.bits.rs1 := io.req.bits.uop.mem_size(0)
  io.resp.bits.sfence.bits.rs2 := io.req.bits.uop.mem_size(1)
  io.resp.bits.sfence.bits.addr := io.req.bits.rs1_data
  io.resp.bits.sfence.bits.asid := io.req.bits.rs2_data
}


/**
 * Functional unit to wrap lower level FPU
 *
 * Currently, bypassing is unsupported!
 * All FP instructions are padded out to the max latency unit for easy
 * write-port scheduling.
 */
class FPUUnit(implicit p: Parameters)
  extends PipelinedFunctionalUnit(
    numStages = p(tile.TileKey).core.fpu.get.dfmaLatency,
    numBypassStages = 0,
    earliestBypassStage = 0,
    dataWidth = 65,
    needsFcsr = true)
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

/**
 * Int to FP conversion functional unit
 *
 * @param latency the amount of stages to delay by
 */
class IntToFPUnit(latency: Int)(implicit p: Parameters)
  extends PipelinedFunctionalUnit(
    numStages = latency,
    numBypassStages = 0,
    earliestBypassStage = 0,
    dataWidth = 65,
    needsFcsr = true)
  with tile.HasFPUParameters
{
  val fp_decoder = Module(new UOPCodeFPUDecoder) // TODO use a simpler decoder
  val io_req = io.req.bits
  fp_decoder.io.uopc := io_req.uop.uopc
  val fp_ctrl = fp_decoder.io.sigs
  val fp_rm = Mux(ImmGenRm(io_req.uop.imm_packed) === 7.U, io.fcsr_rm, ImmGenRm(io_req.uop.imm_packed))
  val req = Wire(new tile.FPInput)
  val tag = fp_ctrl.typeTagIn

  req.viewAsSupertype(new tile.FPUCtrlSigs) := fp_ctrl

  req.rm := fp_rm
  req.in1 := unbox(io_req.rs1_data, tag, None)
  req.in2 := unbox(io_req.rs2_data, tag, None)
  req.in3 := DontCare
  req.typ := ImmGenTyp(io_req.uop.imm_packed)
  req.fmt := DontCare // FIXME: this may not be the right thing to do here
  req.fmaCmd := DontCare

  assert (!(io.req.valid && fp_ctrl.fromint && req.in1(xLen).asBool),
    "[func] IntToFP integer input has 65th high-order bit set!")

  assert (!(io.req.valid && !fp_ctrl.fromint),
    "[func] Only support fromInt micro-ops.")

  val ifpu = Module(new tile.IntToFP(intToFpLatency))
  ifpu.io.in.valid := io.req.valid
  ifpu.io.in.bits := req
  ifpu.io.in.bits.in1 := io_req.rs1_data
  val out_double = Pipe(io.req.valid, fp_ctrl.typeTagOut === D, intToFpLatency).bits

//io.resp.bits.data              := box(ifpu.io.out.bits.data, !io.resp.bits.uop.fp_single)
  io.resp.bits.data              := box(ifpu.io.out.bits.data, out_double)
  io.resp.bits.fflags.valid      := ifpu.io.out.valid
  io.resp.bits.fflags.bits.uop   := io.resp.bits.uop
  io.resp.bits.fflags.bits.flags := ifpu.io.out.bits.exc
}

/**
 * Iterative/unpipelined functional unit, can only hold a single MicroOp at a time
 * assumes at least one register between request and response
 *
 * TODO allow up to N micro-ops simultaneously.
 *
 * @param dataWidth width of the data to be passed into the functional unit
 */
abstract class IterativeFunctionalUnit(dataWidth: Int)(implicit p: Parameters)
  extends FunctionalUnit(
    isPipelined = false,
    numStages = 1,
    numBypassStages = 0,
    dataWidth = dataWidth)
{
  val r_uop = Reg(new MicroOp())

  val do_kill = Wire(Bool())
  do_kill := io.req.bits.kill // irrelevant default

  when (io.req.fire) {
    // update incoming uop
    do_kill := IsKilledByBranch(io.brupdate, io.req.bits.uop) || io.req.bits.kill
    r_uop := io.req.bits.uop
    r_uop.br_mask := GetNewBrMask(io.brupdate, io.req.bits.uop)
  } .otherwise {
    do_kill := IsKilledByBranch(io.brupdate, r_uop) || io.req.bits.kill
    r_uop.br_mask := GetNewBrMask(io.brupdate, r_uop)
  }

  // assumes at least one pipeline register between request and response
  io.resp.bits.uop := r_uop
}

/**
 * Divide functional unit.
 *
 * @param dataWidth data to be passed into the functional unit
 */
class DivUnit(dataWidth: Int)(implicit p: Parameters)
  extends IterativeFunctionalUnit(dataWidth)
{

  // We don't use the iterative multiply functionality here.
  // Instead we use the PipelinedMultiplier
  val div = Module(new freechips.rocketchip.rocket.MulDiv(mulDivParams, width = dataWidth))

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

/**
 * Pipelined multiplier functional unit that wraps around the RocketChip pipelined multiplier
 *
 * @param numStages number of pipeline stages
 * @param dataWidth size of the data being passed into the functional unit
 */
class PipelinedMulUnit(numStages: Int, dataWidth: Int)(implicit p: Parameters)
  extends PipelinedFunctionalUnit(
    numStages = numStages,
    numBypassStages = 0,
    earliestBypassStage = 0,
    dataWidth = dataWidth)
{
  val imul = Module(new PipelinedMultiplier(xLen, numStages))
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

class FoldedMule2NUnit(dataWidth: Int)(implicit p: Parameters)
  extends FunctionalUnit(
    isPipelined = false,
    numStages = 1,
    numBypassStages = 0,
    dataWidth = dataWidth)
{
  require(xLen == 64, "[mule2n] BOOM MULE2N implementation assumes RV64.")

  val prodWidth  = 2 * xLen
  val chunkWidth = xLen / 2

  val sIdle :: sCalc0 :: sCalc1 :: sResp :: Nil = Enum(4)
  val state = RegInit(sIdle)

  val r_uop          = Reg(new MicroOp())
  val sign_r         = RegInit(false.B)
  val chunk1_valid_r = RegInit(false.B)
  val multiplicand_r = Reg(UInt(xLen.W))
  val chunk0_r       = Reg(UInt(chunkWidth.W))
  val chunk1_r       = Reg(UInt(chunkWidth.W))
  val chunk0_high_r  = RegInit(false.B)
  val chunk1_high_r  = RegInit(false.B)
  val acc_sum_r      = RegInit(0.U(prodWidth.W))
  val acc_carry_r    = RegInit(0.U(prodWidth.W))
  val result_r       = RegInit(0.U(xLen.W))

  private def csa(a: UInt, b: UInt, c: UInt): (UInt, UInt) = {
    val width = a.getWidth
    val sum   = Wire(UInt(width.W))
    val carry = Wire(UInt(width.W))
    sum   := a ^ b ^ c
    carry := (((a & b) | (a & c) | (b & c)) << 1)(width - 1, 0)
    (sum, carry)
  }

  private def absSigned(value: UInt): UInt = {
    Mux(value(xLen - 1), (~value).asUInt + 1.U, value)
  }

  private def shiftChunk(value: UInt, upperHalf: Bool): UInt = {
    val shifted = Wire(UInt(prodWidth.W))
    shifted := Mux(upperHalf, (value << chunkWidth)(prodWidth - 1, 0), value)
    shifted
  }

  val req_a = io.req.bits.rs1_data(xLen - 1, 0)
  val req_b = io.req.bits.rs2_data(xLen - 1, 0)

  val a_abs = absSigned(req_a)
  val b_abs = absSigned(req_b)

  val a_low_nonzero  = a_abs(chunkWidth - 1, 0) =/= 0.U
  val a_high_nonzero = a_abs(xLen - 1, chunkWidth) =/= 0.U
  val b_low_nonzero  = b_abs(chunkWidth - 1, 0) =/= 0.U
  val b_high_nonzero = b_abs(xLen - 1, chunkWidth) =/= 0.U

  val a_single_chunk = !(a_low_nonzero && a_high_nonzero)
  val b_single_chunk = !(b_low_nonzero && b_high_nonzero)

  val choose_a_as_chunk =
    Mux(a_single_chunk =/= b_single_chunk, a_single_chunk, PopCount(a_abs) <= PopCount(b_abs))

  val chunk_seed        = Mux(choose_a_as_chunk, a_abs, b_abs)
  val multiplicand_seed = Mux(choose_a_as_chunk, b_abs, a_abs)
  val sign_seed         = req_a(xLen - 1) ^ req_b(xLen - 1)

  val chunk_low_seed    = chunk_seed(chunkWidth - 1, 0)
  val chunk_high_seed   = chunk_seed(xLen - 1, chunkWidth)
  val low_nonzero_seed  = chunk_low_seed =/= 0.U
  val high_nonzero_seed = chunk_high_seed =/= 0.U

  val choose_high_first =
    high_nonzero_seed &&
    (!low_nonzero_seed || (PopCount(chunk_high_seed) < PopCount(chunk_low_seed)))

  val chunk0_seed       = Mux(choose_high_first, chunk_high_seed, chunk_low_seed)
  val chunk1_seed       = Mux(choose_high_first, chunk_low_seed, chunk_high_seed)
  val chunk0_high_seed  = choose_high_first
  val chunk1_high_seed  = !choose_high_first
  val chunk1_valid_seed = low_nonzero_seed && high_nonzero_seed

  val calc0_active = state === sCalc0
  val calc1_active = state === sCalc1
  val current_chunk = Mux(calc1_active, chunk1_r, chunk0_r)
  val current_high  = Mux(calc1_active, chunk1_high_r, chunk0_high_r)
  val gated_chunk   = Mux(calc0_active || calc1_active, current_chunk, 0.U(chunkWidth.W))

  val pp_lines = Wire(Vec(chunkWidth, UInt(prodWidth.W)))
  for (i <- 0 until chunkWidth) {
    pp_lines(i) := Mux(gated_chunk(i), (Cat(0.U(xLen.W), multiplicand_r) << i)(prodWidth - 1, 0), 0.U(prodWidth.W))
  }

  val ppm_sum   = Wire(Vec(chunkWidth + 1, UInt(prodWidth.W)))
  val ppm_carry = Wire(Vec(chunkWidth + 1, UInt(prodWidth.W)))
  ppm_sum(0)   := 0.U
  ppm_carry(0) := 0.U

  for (i <- 0 until chunkWidth) {
    val stage = csa(ppm_sum(i), ppm_carry(i), pp_lines(i))
    ppm_sum(i + 1)   := stage._1
    ppm_carry(i + 1) := stage._2
  }

  val chunk_sum_shifted   = shiftChunk(ppm_sum(chunkWidth), current_high)
  val chunk_carry_shifted = shiftChunk(ppm_carry(chunkWidth), current_high)
  val feedback0           = csa(acc_sum_r, acc_carry_r, chunk_sum_shifted)
  val feedback1           = csa(feedback0._1, feedback0._2, chunk_carry_shifted)
  val final_product       = feedback1._1 + feedback1._2
  val final_mag_result    = final_product(xLen - 1, 0)
  val final_result        = Mux(sign_r, (~final_mag_result).asUInt + 1.U, final_mag_result)

  when (state =/= sIdle) {
    r_uop.br_mask := GetNewBrMask(io.brupdate, r_uop)
  }

  val do_kill = (state =/= sIdle) && (IsKilledByBranch(io.brupdate, r_uop) || io.req.bits.kill)

  io.req.ready := state === sIdle
  io.resp.valid := state === sResp && !do_kill
  io.resp.bits.predicated := false.B
  io.resp.bits.data := result_r
  io.resp.bits.fflags.valid := false.B
  io.resp.bits.uop := r_uop
  io.resp.bits.uop.br_mask := GetNewBrMask(io.brupdate, r_uop)

  when (do_kill) {
    state := sIdle
    acc_sum_r := 0.U
    acc_carry_r := 0.U
  } .otherwise {
    switch (state) {
      is (sIdle) {
        when (io.req.fire) {
          r_uop := io.req.bits.uop
          r_uop.br_mask := GetNewBrMask(io.brupdate, io.req.bits.uop)
          sign_r := sign_seed
          multiplicand_r := multiplicand_seed
          chunk0_r := chunk0_seed
          chunk1_r := chunk1_seed
          chunk0_high_r := chunk0_high_seed
          chunk1_high_r := chunk1_high_seed
          chunk1_valid_r := chunk1_valid_seed
          acc_sum_r := 0.U
          acc_carry_r := 0.U

          when (a_abs === 0.U || b_abs === 0.U) {
            result_r := 0.U
            state := sResp
          } .otherwise {
            state := sCalc0
          }
        }
      }

      is (sCalc0) {
        when (chunk1_valid_r) {
          acc_sum_r := feedback1._1
          acc_carry_r := feedback1._2
          state := sCalc1
        } .otherwise {
          result_r := final_result
          state := sResp
        }
      }

      is (sCalc1) {
        result_r := final_result
        state := sResp
      }

      is (sResp) {
        when (io.resp.ready) {
          state := sIdle
        }
      }
    }
  }
}

class FoldedMule3NUnit(dataWidth: Int)(implicit p: Parameters)
  extends FunctionalUnit(
    isPipelined = false,
    numStages = 1,
    numBypassStages = 0,
    dataWidth = dataWidth)
{
  require(xLen == 64, "[mule3n] BOOM MULE3N implementation assumes RV64.")

  val prodWidth      = 2 * xLen
  val chunkWidth     = 22
  val highChunkShift = 44
  val shiftWidth     = log2Ceil(prodWidth)

  val sIdle :: sCalc0 :: sCalc1 :: sCalc2 :: sResp :: Nil = Enum(5)
  val state = RegInit(sIdle)

  val r_uop          = Reg(new MicroOp())
  val sign_r         = RegInit(false.B)
  val chunk1_valid_r = RegInit(false.B)
  val chunk2_valid_r = RegInit(false.B)
  val multiplicand_r = Reg(UInt(xLen.W))
  val chunk0_r       = Reg(UInt(chunkWidth.W))
  val chunk1_r       = Reg(UInt(chunkWidth.W))
  val chunk2_r       = Reg(UInt(chunkWidth.W))
  val chunk0_shift_r = Reg(UInt(shiftWidth.W))
  val chunk1_shift_r = Reg(UInt(shiftWidth.W))
  val chunk2_shift_r = Reg(UInt(shiftWidth.W))
  val acc_sum_r      = RegInit(0.U(prodWidth.W))
  val acc_carry_r    = RegInit(0.U(prodWidth.W))
  val result_r       = RegInit(0.U(xLen.W))

  private def csa(a: UInt, b: UInt, c: UInt): (UInt, UInt) = {
    val width = a.getWidth
    val sum   = Wire(UInt(width.W))
    val carry = Wire(UInt(width.W))
    sum   := a ^ b ^ c
    carry := (((a & b) | (a & c) | (b & c)) << 1)(width - 1, 0)
    (sum, carry)
  }

  private def absSigned(value: UInt): UInt = {
    Mux(value(xLen - 1), (~value).asUInt + 1.U, value)
  }

  private def chunkComesFirst(weightA: UInt, shiftA: UInt, weightB: UInt, shiftB: UInt): Bool = {
    (weightA < weightB) || ((weightA === weightB) && (shiftA < shiftB))
  }

  val req_a = io.req.bits.rs1_data(xLen - 1, 0)
  val req_b = io.req.bits.rs2_data(xLen - 1, 0)

  val a_abs = absSigned(req_a)
  val b_abs = absSigned(req_b)

  val a_low_nonzero  = a_abs(21, 0) =/= 0.U
  val a_mid_nonzero  = a_abs(43, 22) =/= 0.U
  val a_high_nonzero = a_abs(63, 44) =/= 0.U
  val b_low_nonzero  = b_abs(21, 0) =/= 0.U
  val b_mid_nonzero  = b_abs(43, 22) =/= 0.U
  val b_high_nonzero = b_abs(63, 44) =/= 0.U

  val a_chunk_count = a_low_nonzero.asUInt +& a_mid_nonzero.asUInt +& a_high_nonzero.asUInt
  val b_chunk_count = b_low_nonzero.asUInt +& b_mid_nonzero.asUInt +& b_high_nonzero.asUInt

  val choose_a_as_chunk =
    Mux(a_chunk_count =/= b_chunk_count, a_chunk_count < b_chunk_count, PopCount(a_abs) <= PopCount(b_abs))

  val chunk_seed        = Mux(choose_a_as_chunk, a_abs, b_abs)
  val multiplicand_seed = Mux(choose_a_as_chunk, b_abs, a_abs)
  val sign_seed         = req_a(xLen - 1) ^ req_b(xLen - 1)

  val chunk_low_seed  = chunk_seed(21, 0)
  val chunk_mid_seed  = chunk_seed(43, 22)
  val chunk_high_seed = Cat(0.U(2.W), chunk_seed(63, 44))

  val low_nonzero_seed  = chunk_low_seed =/= 0.U
  val mid_nonzero_seed  = chunk_mid_seed =/= 0.U
  val high_nonzero_seed = chunk_high_seed =/= 0.U

  val low_weight  = Mux(low_nonzero_seed, PopCount(chunk_low_seed), 63.U(6.W))
  val mid_weight  = Mux(mid_nonzero_seed, PopCount(chunk_mid_seed), 63.U(6.W))
  val high_weight = Mux(high_nonzero_seed, PopCount(chunk_high_seed), 63.U(6.W))

  val shiftLow  = 0.U(shiftWidth.W)
  val shiftMid  = 22.U(shiftWidth.W)
  val shiftHigh = highChunkShift.U(shiftWidth.W)

  val swap01 = !chunkComesFirst(low_weight, shiftLow, mid_weight, shiftMid)
  val stage0_chunk0  = Mux(swap01, chunk_mid_seed, chunk_low_seed)
  val stage0_chunk1  = Mux(swap01, chunk_low_seed, chunk_mid_seed)
  val stage0_weight0 = Mux(swap01, mid_weight, low_weight)
  val stage0_weight1 = Mux(swap01, low_weight, mid_weight)
  val stage0_shift0  = Mux(swap01, shiftMid, shiftLow)
  val stage0_shift1  = Mux(swap01, shiftLow, shiftMid)
  val stage0_valid0  = Mux(swap01, mid_nonzero_seed, low_nonzero_seed)
  val stage0_valid1  = Mux(swap01, low_nonzero_seed, mid_nonzero_seed)

  val swap12 = !chunkComesFirst(stage0_weight1, stage0_shift1, high_weight, shiftHigh)
  val stage1_chunk1  = Mux(swap12, chunk_high_seed, stage0_chunk1)
  val stage1_chunk2  = Mux(swap12, stage0_chunk1, chunk_high_seed)
  val stage1_weight1 = Mux(swap12, high_weight, stage0_weight1)
  val stage1_weight2 = Mux(swap12, stage0_weight1, high_weight)
  val stage1_shift1  = Mux(swap12, shiftHigh, stage0_shift1)
  val stage1_shift2  = Mux(swap12, stage0_shift1, shiftHigh)
  val stage1_valid1  = Mux(swap12, high_nonzero_seed, stage0_valid1)
  val stage1_valid2  = Mux(swap12, stage0_valid1, high_nonzero_seed)

  val swap01b = !chunkComesFirst(stage0_weight0, stage0_shift0, stage1_weight1, stage1_shift1)
  val chunk0_seed       = Mux(swap01b, stage1_chunk1, stage0_chunk0)
  val chunk1_seed       = Mux(swap01b, stage0_chunk0, stage1_chunk1)
  val chunk2_seed       = stage1_chunk2
  val chunk0_shift_seed = Mux(swap01b, stage1_shift1, stage0_shift0)
  val chunk1_shift_seed = Mux(swap01b, stage0_shift0, stage1_shift1)
  val chunk2_shift_seed = stage1_shift2
  val chunk0_valid_seed = Mux(swap01b, stage1_valid1, stage0_valid0)
  val chunk1_valid_seed = Mux(swap01b, stage0_valid0, stage1_valid1)
  val chunk2_valid_seed = stage1_valid2

  val calc0_active = state === sCalc0
  val calc1_active = state === sCalc1
  val calc2_active = state === sCalc2
  val current_chunk = Mux(calc2_active, chunk2_r, Mux(calc1_active, chunk1_r, chunk0_r))
  val current_shift = Mux(calc2_active, chunk2_shift_r, Mux(calc1_active, chunk1_shift_r, chunk0_shift_r))
  val gated_chunk   = Mux(calc0_active || calc1_active || calc2_active, current_chunk, 0.U(chunkWidth.W))

  val pp_lines = Wire(Vec(chunkWidth, UInt(prodWidth.W)))
  for (i <- 0 until chunkWidth) {
    pp_lines(i) := Mux(gated_chunk(i), (Cat(0.U(xLen.W), multiplicand_r) << i)(prodWidth - 1, 0), 0.U(prodWidth.W))
  }

  val ppm_sum   = Wire(Vec(chunkWidth + 1, UInt(prodWidth.W)))
  val ppm_carry = Wire(Vec(chunkWidth + 1, UInt(prodWidth.W)))
  ppm_sum(0)   := 0.U
  ppm_carry(0) := 0.U

  for (i <- 0 until chunkWidth) {
    val stage = csa(ppm_sum(i), ppm_carry(i), pp_lines(i))
    ppm_sum(i + 1)   := stage._1
    ppm_carry(i + 1) := stage._2
  }

  val chunk_sum_shifted   = (ppm_sum(chunkWidth) << current_shift)(prodWidth - 1, 0)
  val chunk_carry_shifted = (ppm_carry(chunkWidth) << current_shift)(prodWidth - 1, 0)
  val feedback0           = csa(acc_sum_r, acc_carry_r, chunk_sum_shifted)
  val feedback1           = csa(feedback0._1, feedback0._2, chunk_carry_shifted)
  val final_product       = feedback1._1 + feedback1._2
  val final_mag_result    = final_product(xLen - 1, 0)
  val final_result        = Mux(sign_r, (~final_mag_result).asUInt + 1.U, final_mag_result)

  when (state =/= sIdle) {
    r_uop.br_mask := GetNewBrMask(io.brupdate, r_uop)
  }

  val do_kill = (state =/= sIdle) && (IsKilledByBranch(io.brupdate, r_uop) || io.req.bits.kill)

  io.req.ready := state === sIdle
  io.resp.valid := state === sResp && !do_kill
  io.resp.bits.predicated := false.B
  io.resp.bits.data := result_r
  io.resp.bits.fflags.valid := false.B
  io.resp.bits.uop := r_uop
  io.resp.bits.uop.br_mask := GetNewBrMask(io.brupdate, r_uop)

  when (do_kill) {
    state := sIdle
    acc_sum_r := 0.U
    acc_carry_r := 0.U
  } .otherwise {
    switch (state) {
      is (sIdle) {
        when (io.req.fire) {
          r_uop := io.req.bits.uop
          r_uop.br_mask := GetNewBrMask(io.brupdate, io.req.bits.uop)
          sign_r := sign_seed
          multiplicand_r := multiplicand_seed
          chunk0_r := chunk0_seed
          chunk1_r := chunk1_seed
          chunk2_r := chunk2_seed
          chunk0_shift_r := chunk0_shift_seed
          chunk1_shift_r := chunk1_shift_seed
          chunk2_shift_r := chunk2_shift_seed
          chunk1_valid_r := chunk1_valid_seed
          chunk2_valid_r := chunk2_valid_seed
          acc_sum_r := 0.U
          acc_carry_r := 0.U

          when (a_abs === 0.U || b_abs === 0.U || !chunk0_valid_seed) {
            result_r := 0.U
            state := sResp
          } .otherwise {
            state := sCalc0
          }
        }
      }

      is (sCalc0) {
        when (chunk1_valid_r) {
          acc_sum_r := feedback1._1
          acc_carry_r := feedback1._2
          state := sCalc1
        } .otherwise {
          result_r := final_result
          state := sResp
        }
      }

      is (sCalc1) {
        when (chunk2_valid_r) {
          acc_sum_r := feedback1._1
          acc_carry_r := feedback1._2
          state := sCalc2
        } .otherwise {
          result_r := final_result
          state := sResp
        }
      }

      is (sCalc2) {
        result_r := final_result
        state := sResp
      }

      is (sResp) {
        when (io.resp.ready) {
          state := sIdle
        }
      }
    }
  }
}

class FoldedMule5NUnit(dataWidth: Int)(implicit p: Parameters)
  extends FunctionalUnit(
    isPipelined = false,
    numStages = 1,
    numBypassStages = 0,
    dataWidth = dataWidth)
{
  require(xLen == 64, "[mule5n] BOOM MULE5N implementation assumes RV64.")

  val prodWidth      = 2 * xLen
  val chunkWidth     = 13
  val highChunkShift = 52
  val shiftWidth     = log2Ceil(prodWidth)

  val sIdle :: sCalc0 :: sCalc1 :: sCalc2 :: sCalc3 :: sCalc4 :: sResp :: Nil = Enum(7)
  val state = RegInit(sIdle)

  val r_uop          = Reg(new MicroOp())
  val sign_r         = RegInit(false.B)
  val multiplicand_r = Reg(UInt(xLen.W))
  val chunk0_r       = Reg(UInt(chunkWidth.W))
  val chunk1_r       = Reg(UInt(chunkWidth.W))
  val chunk2_r       = Reg(UInt(chunkWidth.W))
  val chunk3_r       = Reg(UInt(chunkWidth.W))
  val chunk4_r       = Reg(UInt(chunkWidth.W))
  val chunk0_shift_r = Reg(UInt(shiftWidth.W))
  val chunk1_shift_r = Reg(UInt(shiftWidth.W))
  val chunk2_shift_r = Reg(UInt(shiftWidth.W))
  val chunk3_shift_r = Reg(UInt(shiftWidth.W))
  val chunk4_shift_r = Reg(UInt(shiftWidth.W))
  val chunk1_valid_r = RegInit(false.B)
  val chunk2_valid_r = RegInit(false.B)
  val chunk3_valid_r = RegInit(false.B)
  val chunk4_valid_r = RegInit(false.B)
  val acc_sum_r      = RegInit(0.U(prodWidth.W))
  val acc_carry_r    = RegInit(0.U(prodWidth.W))
  val result_r       = RegInit(0.U(xLen.W))

  private def csa(a: UInt, b: UInt, c: UInt): (UInt, UInt) = {
    val width = a.getWidth
    val sum   = Wire(UInt(width.W))
    val carry = Wire(UInt(width.W))
    sum   := a ^ b ^ c
    carry := (((a & b) | (a & c) | (b & c)) << 1)(width - 1, 0)
    (sum, carry)
  }

  private def absSigned(value: UInt): UInt = {
    Mux(value(xLen - 1), (~value).asUInt + 1.U, value)
  }

  private def chunkComesFirst(weightA: UInt, shiftA: UInt, weightB: UInt, shiftB: UInt): Bool = {
    (weightA < weightB) || ((weightA === weightB) && (shiftA < shiftB))
  }

  private def sortPair(
    chunkA: UInt, shiftA: UInt, weightA: UInt, validA: Bool,
    chunkB: UInt, shiftB: UInt, weightB: UInt, validB: Bool
  ): (UInt, UInt, UInt, Bool, UInt, UInt, UInt, Bool) = {
    val swap = !chunkComesFirst(weightA, shiftA, weightB, shiftB)
    (
      Mux(swap, chunkB, chunkA),
      Mux(swap, shiftB, shiftA),
      Mux(swap, weightB, weightA),
      Mux(swap, validB, validA),
      Mux(swap, chunkA, chunkB),
      Mux(swap, shiftA, shiftB),
      Mux(swap, weightA, weightB),
      Mux(swap, validA, validB)
    )
  }

  val req_a = io.req.bits.rs1_data(xLen - 1, 0)
  val req_b = io.req.bits.rs2_data(xLen - 1, 0)

  val a_abs = absSigned(req_a)
  val b_abs = absSigned(req_b)

  val a_chunk0_nonzero = a_abs(12, 0) =/= 0.U
  val a_chunk1_nonzero = a_abs(25, 13) =/= 0.U
  val a_chunk2_nonzero = a_abs(38, 26) =/= 0.U
  val a_chunk3_nonzero = a_abs(51, 39) =/= 0.U
  val a_chunk4_nonzero = a_abs(63, 52) =/= 0.U
  val b_chunk0_nonzero = b_abs(12, 0) =/= 0.U
  val b_chunk1_nonzero = b_abs(25, 13) =/= 0.U
  val b_chunk2_nonzero = b_abs(38, 26) =/= 0.U
  val b_chunk3_nonzero = b_abs(51, 39) =/= 0.U
  val b_chunk4_nonzero = b_abs(63, 52) =/= 0.U

  val a_chunk_count = a_chunk0_nonzero.asUInt +& a_chunk1_nonzero.asUInt +& a_chunk2_nonzero.asUInt +& a_chunk3_nonzero.asUInt +& a_chunk4_nonzero.asUInt
  val b_chunk_count = b_chunk0_nonzero.asUInt +& b_chunk1_nonzero.asUInt +& b_chunk2_nonzero.asUInt +& b_chunk3_nonzero.asUInt +& b_chunk4_nonzero.asUInt

  val choose_a_as_chunk =
    Mux(a_chunk_count =/= b_chunk_count, a_chunk_count < b_chunk_count, PopCount(a_abs) <= PopCount(b_abs))

  val chunk_seed        = Mux(choose_a_as_chunk, a_abs, b_abs)
  val multiplicand_seed = Mux(choose_a_as_chunk, b_abs, a_abs)
  val sign_seed         = req_a(xLen - 1) ^ req_b(xLen - 1)

  val chunkSeed0 = chunk_seed(12, 0)
  val chunkSeed1 = chunk_seed(25, 13)
  val chunkSeed2 = chunk_seed(38, 26)
  val chunkSeed3 = chunk_seed(51, 39)
  val chunkSeed4 = Cat(0.U(1.W), chunk_seed(63, 52))

  val valid0  = chunkSeed0 =/= 0.U
  val valid1  = chunkSeed1 =/= 0.U
  val valid2  = chunkSeed2 =/= 0.U
  val valid3  = chunkSeed3 =/= 0.U
  val valid4  = chunk_seed(63, 52) =/= 0.U
  val weight0 = Mux(valid0, PopCount(chunkSeed0), 63.U(6.W))
  val weight1 = Mux(valid1, PopCount(chunkSeed1), 63.U(6.W))
  val weight2 = Mux(valid2, PopCount(chunkSeed2), 63.U(6.W))
  val weight3 = Mux(valid3, PopCount(chunkSeed3), 63.U(6.W))
  val weight4 = Mux(valid4, PopCount(chunkSeed4), 63.U(6.W))

  val shift0 = 0.U(shiftWidth.W)
  val shift1 = 13.U(shiftWidth.W)
  val shift2 = 26.U(shiftWidth.W)
  val shift3 = 39.U(shiftWidth.W)
  val shift4 = highChunkShift.U(shiftWidth.W)

  val (s10c0, s10s0, s10w0, s10v0, s10c1, s10s1, s10w1, s10v1) =
    sortPair(chunkSeed0, shift0, weight0, valid0, chunkSeed1, shift1, weight1, valid1)
  val (s10c2, s10s2, s10w2, s10v2, s10c3, s10s3, s10w3, s10v3) =
    sortPair(chunkSeed2, shift2, weight2, valid2, chunkSeed3, shift3, weight3, valid3)

  val (s11c1, s11s1, s11w1, s11v1, s11c2, s11s2, s11w2, s11v2) =
    sortPair(s10c1, s10s1, s10w1, s10v1, s10c2, s10s2, s10w2, s10v2)
  val (s11c3, s11s3, s11w3, s11v3, s11c4, s11s4, s11w4, s11v4) =
    sortPair(s10c3, s10s3, s10w3, s10v3, chunkSeed4, shift4, weight4, valid4)

  val (s12c0, s12s0, s12w0, s12v0, s12c1, s12s1, s12w1, s12v1) =
    sortPair(s10c0, s10s0, s10w0, s10v0, s11c1, s11s1, s11w1, s11v1)
  val (s12c2, s12s2, s12w2, s12v2, s12c3, s12s3, s12w3, s12v3) =
    sortPair(s11c2, s11s2, s11w2, s11v2, s11c3, s11s3, s11w3, s11v3)

  val (s13c1, s13s1, s13w1, s13v1, s13c2, s13s2, s13w2, s13v2) =
    sortPair(s12c1, s12s1, s12w1, s12v1, s12c2, s12s2, s12w2, s12v2)
  val (s13c3, s13s3, s13w3, s13v3, s13c4, s13s4, s13w4, s13v4) =
    sortPair(s12c3, s12s3, s12w3, s12v3, s11c4, s11s4, s11w4, s11v4)

  val (s14c0, s14s0, _, s14v0, s14c1, s14s1, _, s14v1) =
    sortPair(s12c0, s12s0, s12w0, s12v0, s13c1, s13s1, s13w1, s13v1)
  val (s14c2, s14s2, _, s14v2, s14c3, s14s3, _, s14v3) =
    sortPair(s13c2, s13s2, s13w2, s13v2, s13c3, s13s3, s13w3, s13v3)

  val (chunk0_seed, chunk0_shift_seed, _, chunk0_valid_seed, chunk1_seed, chunk1_shift_seed, _, chunk1_valid_seed) =
    sortPair(s14c0, s14s0, Mux(s14v0, PopCount(s14c0), 63.U(6.W)), s14v0,
             s14c1, s14s1, Mux(s14v1, PopCount(s14c1), 63.U(6.W)), s14v1)
  val (chunk2_seed, chunk2_shift_seed, _, chunk2_valid_seed, chunk3_seed, chunk3_shift_seed, _, chunk3_valid_seed) =
    sortPair(s14c2, s14s2, Mux(s14v2, PopCount(s14c2), 63.U(6.W)), s14v2,
             s14c3, s14s3, Mux(s14v3, PopCount(s14c3), 63.U(6.W)), s14v3)
  val chunk4_seed       = s13c4
  val chunk4_shift_seed = s13s4
  val chunk4_valid_seed = s13v4

  val calc0_active = state === sCalc0
  val calc1_active = state === sCalc1
  val calc2_active = state === sCalc2
  val calc3_active = state === sCalc3
  val calc4_active = state === sCalc4

  val currentChunk = Mux(calc4_active, chunk4_r,
                     Mux(calc3_active, chunk3_r,
                     Mux(calc2_active, chunk2_r,
                     Mux(calc1_active, chunk1_r, chunk0_r))))
  val currentShift = Mux(calc4_active, chunk4_shift_r,
                     Mux(calc3_active, chunk3_shift_r,
                     Mux(calc2_active, chunk2_shift_r,
                     Mux(calc1_active, chunk1_shift_r, chunk0_shift_r))))
  val gatedChunk = Mux(calc0_active || calc1_active || calc2_active || calc3_active || calc4_active,
                   currentChunk, 0.U(chunkWidth.W))

  val pp_lines = Wire(Vec(chunkWidth, UInt(prodWidth.W)))
  for (i <- 0 until chunkWidth) {
    pp_lines(i) := Mux(gatedChunk(i), (Cat(0.U(xLen.W), multiplicand_r) << i)(prodWidth - 1, 0), 0.U(prodWidth.W))
  }

  val ppm_sum   = Wire(Vec(chunkWidth + 1, UInt(prodWidth.W)))
  val ppm_carry = Wire(Vec(chunkWidth + 1, UInt(prodWidth.W)))
  ppm_sum(0)   := 0.U
  ppm_carry(0) := 0.U

  for (i <- 0 until chunkWidth) {
    val stage = csa(ppm_sum(i), ppm_carry(i), pp_lines(i))
    ppm_sum(i + 1)   := stage._1
    ppm_carry(i + 1) := stage._2
  }

  val chunk_sum_shifted   = (ppm_sum(chunkWidth) << currentShift)(prodWidth - 1, 0)
  val chunk_carry_shifted = (ppm_carry(chunkWidth) << currentShift)(prodWidth - 1, 0)
  val feedback0           = csa(acc_sum_r, acc_carry_r, chunk_sum_shifted)
  val feedback1           = csa(feedback0._1, feedback0._2, chunk_carry_shifted)
  val final_product       = feedback1._1 + feedback1._2
  val final_mag_result    = final_product(xLen - 1, 0)
  val final_result        = Mux(sign_r, (~final_mag_result).asUInt + 1.U, final_mag_result)

  when (state =/= sIdle) {
    r_uop.br_mask := GetNewBrMask(io.brupdate, r_uop)
  }

  val do_kill = (state =/= sIdle) && (IsKilledByBranch(io.brupdate, r_uop) || io.req.bits.kill)

  io.req.ready := state === sIdle
  io.resp.valid := state === sResp && !do_kill
  io.resp.bits.predicated := false.B
  io.resp.bits.data := result_r
  io.resp.bits.fflags.valid := false.B
  io.resp.bits.uop := r_uop
  io.resp.bits.uop.br_mask := GetNewBrMask(io.brupdate, r_uop)

  when (do_kill) {
    state := sIdle
    acc_sum_r := 0.U
    acc_carry_r := 0.U
  } .otherwise {
    switch (state) {
      is (sIdle) {
        when (io.req.fire) {
          r_uop := io.req.bits.uop
          r_uop.br_mask := GetNewBrMask(io.brupdate, io.req.bits.uop)
          sign_r := sign_seed
          multiplicand_r := multiplicand_seed
          chunk0_r := chunk0_seed
          chunk1_r := chunk1_seed
          chunk2_r := chunk2_seed
          chunk3_r := chunk3_seed
          chunk4_r := chunk4_seed
          chunk0_shift_r := chunk0_shift_seed
          chunk1_shift_r := chunk1_shift_seed
          chunk2_shift_r := chunk2_shift_seed
          chunk3_shift_r := chunk3_shift_seed
          chunk4_shift_r := chunk4_shift_seed
          chunk1_valid_r := chunk1_valid_seed
          chunk2_valid_r := chunk2_valid_seed
          chunk3_valid_r := chunk3_valid_seed
          chunk4_valid_r := chunk4_valid_seed
          acc_sum_r := 0.U
          acc_carry_r := 0.U

          when (a_abs === 0.U || b_abs === 0.U || !chunk0_valid_seed) {
            result_r := 0.U
            state := sResp
          } .otherwise {
            state := sCalc0
          }
        }
      }

      is (sCalc0) {
        when (chunk1_valid_r) {
          acc_sum_r := feedback1._1
          acc_carry_r := feedback1._2
          state := sCalc1
        } .otherwise {
          result_r := final_result
          state := sResp
        }
      }

      is (sCalc1) {
        when (chunk2_valid_r) {
          acc_sum_r := feedback1._1
          acc_carry_r := feedback1._2
          state := sCalc2
        } .otherwise {
          result_r := final_result
          state := sResp
        }
      }

      is (sCalc2) {
        when (chunk3_valid_r) {
          acc_sum_r := feedback1._1
          acc_carry_r := feedback1._2
          state := sCalc3
        } .otherwise {
          result_r := final_result
          state := sResp
        }
      }

      is (sCalc3) {
        when (chunk4_valid_r) {
          acc_sum_r := feedback1._1
          acc_carry_r := feedback1._2
          state := sCalc4
        } .otherwise {
          result_r := final_result
          state := sResp
        }
      }

      is (sCalc4) {
        result_r := final_result
        state := sResp
      }

      is (sResp) {
        when (io.resp.ready) {
          state := sIdle
        }
      }
    }
  }
}
