//******************************************************************************
// Copyright (c) 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Branch Target Buffer (abstract class)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Provide an abstract class for branch target buffers. Currently, can choose
// between a set-associative BTB or a "dense" BTB design

package boom.bpu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import boom.common._
import boom.exu._

import freechips.rocketchip.util.Str

//------------------------------------------------------------------------------
// Parameters and Traits
//------------------------------------------------------------------------------

case class BoomBTBParameters(
   btbsa: Boolean = false,
   densebtb: Boolean = true,
   nSets: Int    = 512,
   nBanks: Int   = 2,
   nWays: Int    = 4,
   nRAS: Int     = 16,
   tagSz: Int    = 13,
   offsetSz: Int = 13,

   // Extra knobs
   bypassCalls: Boolean = true,
   rasCheckForEmpty: Boolean = true,
   numBufferEntries: Int = 8
)

trait HasBoomBTBParameters extends HasBoomCoreParameters
{
   val btbParams = boomParams.btb
   val nSets = btbParams.nSets
   val nBanks = btbParams.nBanks
   val nWays = btbParams.nWays
   val nRAS = btbParams.nRAS
   val tag_sz = btbParams.tagSz
   val offset_sz = btbParams.offsetSz
   val idx_sz = log2Ceil(nSets)
   val bypassCalls = btbParams.bypassCalls
   val rasCheckForEmpty = btbParams.rasCheckForEmpty
   val num_buff_entries = btbParams.numBufferEntries
}

// Which predictor should we listen to?
object BpredType
{
   def SZ = 3
   def apply() = UInt(SZ.W)
   def branch = 0.U
   def jump = 1.U
   def ret =  (2+1).U
   def call = (4+1).U

   def isAlwaysTaken(typ: UInt): Bool = typ(0)
   def isReturn(typ: UInt): Bool = typ(1)
   def isCall(typ: UInt): Bool = typ(2)
   def isJump(typ: UInt): Bool = typ === jump
   def isBranch(typ: UInt): Bool = typ === branch
}

//------------------------------------------------------------------------------
// Module Interface
//------------------------------------------------------------------------------

abstract class BoomBTBBundle(implicit val p: Parameters) extends freechips.rocketchip.util.ParameterizedBundle()(p)
  with HasBoomBTBParameters

//  - "cfi_idx" is the low-order PC bits of the predicted branch (after
//     shifting off the lowest log(inst_bytes) bits off).
//  - "mask" provides a mask of valid instructions (instructions are
//     masked off by the predicted taken branch from the BTB).
class BoomBTBResp(implicit p: Parameters) extends BoomBTBBundle()(p)
{
   val taken     = Bool()   // is BTB predicting a taken cfi?
   val target    = UInt(vaddrBits.W) // what target are we predicting?
   val mask      = UInt(fetchWidth.W) // mask of valid instructions.
   val cfi_idx   = UInt(log2Ceil(fetchWidth).W) // where is cfi we are predicting?
   val bpd_type  = BpredType() // which predictor should we use?
   val cfi_type  = CfiType()  // what type of instruction is this?
   val fetch_pc  = UInt(vaddrBits.W) // the PC we're predicting on (start of the fetch packet).

   val bim_resp  = Valid(new BimResp) // Output from the bimodal table. Valid if prediction provided.
}

// BTB update occurs during branch resolution (and only on a mispredict that's taken).
//  - "pc" is what future fetch PCs will tag match against.
//  - "cfi_pc" is the PC of the branch instruction.
class BoomBTBUpdate(implicit p: Parameters) extends BoomBTBBundle()(p)
{
   val pc = UInt(vaddrBits.W)
   val target = UInt(vaddrBits.W)
   val taken = Bool()
   val cfi_pc = UInt(vaddrBits.W)
   val bpd_type = BpredType()
   val cfi_type = CfiType()
}

class RasUpdate(implicit p: Parameters) extends BoomBTBBundle()(p)
{
   val is_call = Bool()
   val is_ret = Bool()
   val return_addr = UInt(vaddrBits.W)
}

class PCReq(implicit p: Parameters) extends BoomBTBBundle()(p)
{
   val addr = UInt(vaddrBitsExtended.W)
}

//------------------------------------------------------------------------------
// Return Address Stack
//------------------------------------------------------------------------------

class RAS(nras: Int, coreInstBytes: Int)
{
   def push(addr: UInt): Unit =
   {
      when (count < nras.U) { count := count + 1.U }
      val nextPos = Mux(isPow2(nras).B || pos < (nras-1).U, pos+1.U, 0.U)
      stack(nextPos) := addr >> log2Ceil(coreInstBytes)
      pos := nextPos
   }
   def peek: UInt = Cat(stack(pos), 0.U(log2Ceil(coreInstBytes).W))
   def pop(): Unit = when (!isEmpty)
   {
      count := count - 1.U
      pos := Mux(isPow2(nras).B || pos > 0.U, pos-1.U, (nras-1).U)
   }
   //def clear(): Unit = count := 0.U
   def isEmpty: Bool = count === 0.U

   private val count = Reg(UInt(log2Ceil(nras+1).W))
   private val pos = Reg(UInt(log2Ceil(nras).W))
   private val stack = Reg(Vec(nras, UInt()))
}

//------------------------------------------------------------------------------
// BoomBTB
//------------------------------------------------------------------------------

abstract class BoomBTB(implicit p: Parameters) extends BoomModule()(p) with HasBoomBTBParameters
{
   val io = IO(new Bundle
   {
      // req.valid is false if stalling (aka, we won't read and use BTB results, on cycle S1).
      // req.bits.addr is available on cycle S0.
      // resp is expected on cycle S2.
      val req = Flipped(Valid(new PCReq))

      // resp is valid if there is a BTB hit.
      val resp = Valid(new BoomBTBResp)

      // the PC we're predicting on (start of the fetch packet).
      // Pass this to the BPD.
      //val s1_pc  = UInt(width = vaddrBits)

      // supress S1 (so next cycle S2 is not valid).
      val flush = Input(Bool())

      val btb_update = Flipped(Valid(new BoomBTBUpdate))
      val bim_update = Flipped(Valid(new BimUpdate))
      val ras_update = Flipped(Valid(new RasUpdate))

      // HACK: prevent BTB updating/predicting during program load.
      // Easier to diff against spike which doesn't run debug mode.
      val status_debug = Input(Bool())
   })

   override val compileOptions = chisel3.core.ExplicitCompileOptions.NotStrict.copy(explicitInvalidate = true)
}

object BoomBTB
{
   def apply(boomParams: BoomCoreParams)
      (implicit p: Parameters): BoomBTB =
   {
      val boomParams: BoomCoreParams = p(freechips.rocketchip.tile.TileKey).core.asInstanceOf[BoomCoreParams]
      var btb: BoomBTB = null

      if (boomParams.btb.btbsa)
      {
         btb = Module(new BTBsa())
      }
      else if (boomParams.btb.densebtb)
      {
         btb = Module(new DenseBTB())
      }
      btb
   }
}
