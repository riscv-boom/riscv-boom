//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Branch Target Buffer and Return Address Stack (abstract class)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Provide an abstract class for branch target buffers. Currently, can choose
// between a set-associative BTB or a "dense" BTB design (or if not using the BTB
// a "null" BTB)

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

/**
 * BTB parameters that BOOM uses
 *
 * @param btbsa enable a set associative BTB
 * @param densebtb enable a more complicated BTB
 * @param nSets number of sets
 * @param nBanks number of banks
 * @param nWays number of ways
 * @param nRAS number of RAS entries
 * @param tagSz size of the BTB tags
 * @param offsetSz offset size for the DenseBTB
 * @param bypassCalls bypass RAS update to the BTB resp target
 * @param rasCheckForEmpty check if RAS is empty before reading
 */
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
)

/**
 * Trait implementing creating parameters
 */
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
}

/**
 * Factory object to determining the type of branch
 */
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

/**
 * Abstract class to allow bundles to have the Boom BTB parameters
 */
abstract class BoomBTBBundle(implicit val p: Parameters) extends freechips.rocketchip.util.ParameterizedBundle()(p)
  with HasBoomBTBParameters

/**
 * The response packet sent back from the BTB
 */
class BoomBTBResp(implicit p: Parameters) extends BoomBTBBundle()(p)
{
   val taken     = Bool()   // is BTB predicting a taken cfi?
   val target    = UInt(vaddrBits.W) // what target are we predicting?

   // a mask of valid instructions (instructions are
   //   masked off by the predicted taken branch from the BTB).
   val mask      = UInt(fetchWidth.W) // mask of valid instructions.

   // the low-order PC bits of the predicted branch (after
   //   shifting off the lowest log(inst_bytes) bits off).
   val cfi_idx   = UInt(log2Ceil(fetchWidth).W) // where is cfi we are predicting?
   val bpd_type  = BpredType() // which predictor should we use?
   val cfi_type  = CfiType()  // what type of instruction is this?
   val fetch_pc  = UInt(vaddrBits.W) // the PC we're predicting on (start of the fetch packet).

   val bim_resp  = Valid(new BimResp) // Output from the bimodal table. Valid if prediction provided.
}

/**
 * The incoming packet to update the BTB state. BTB update occurs during branch resolution
 * (and only on a mispredict that's taken).
 */
class BoomBTBUpdate(implicit p: Parameters) extends BoomBTBBundle()(p)
{
   // what future fetch PCs will tag match against.
   val pc = UInt(vaddrBits.W)
   val target = UInt(vaddrBits.W)
   val taken = Bool()

   // the offset of the PC of the branch
   val cfi_idx = UInt(log2Ceil(fetchWidth).W)
   val bpd_type = BpredType()
   val cfi_type = CfiType()
}

/**
 * RAS update packet
 */
class RasUpdate(implicit p: Parameters) extends BoomBTBBundle()(p)
{
   val is_call = Bool()
   val is_ret = Bool()
   val return_addr = UInt(vaddrBits.W)
}

/**
 * PC address request packet
 */
class PCReq(implicit p: Parameters) extends BoomBTBBundle()(p)
{
   val addr = UInt(vaddrBitsExtended.W)
}

//------------------------------------------------------------------------------
// Return Address Stack
//------------------------------------------------------------------------------

/**
 * Return Address Stack (RAS) class used to keep track of where to return to
 * at the end of a function call.
 *
 * @param nras number of RAS entries
 * @param coreInstBytes amount of bytes per instruction
 */
class RAS(nras: Int, coreInstBytes: Int)
{
   /**
    * Push an address onto the RAS
    *
    * @param addr the address to push onto the stack
    */
   def push(addr: UInt): Unit =
   {
      when (count < nras.U) { count := count + 1.U }
      val nextPos = Mux(isPow2(nras).B || pos < (nras-1).U, pos+1.U, 0.U)
      stack(nextPos) := addr >> log2Ceil(coreInstBytes)
      pos := nextPos
   }

   /**
    * Look at the address at the top of the stack
    */
   def peek: UInt = Cat(stack(pos), 0.U(log2Ceil(coreInstBytes).W))

   /**
    * Pop the top entry of the stack
    */
   def pop(): Unit = when (!isEmpty)
   {
      count := count - 1.U
      pos := Mux(isPow2(nras).B || pos > 0.U, pos-1.U, (nras-1).U)
   }

   /**
    * Check if the stack is empty
    */
   def isEmpty: Bool = count === 0.U

   //def clear(): Unit = count := 0.U

   private val count = Reg(UInt(log2Ceil(nras+1).W))
   private val pos = Reg(UInt(log2Ceil(nras).W))
   private val stack = Reg(Vec(nras, UInt()))
}

//------------------------------------------------------------------------------
// BoomBTB
//------------------------------------------------------------------------------

/**
 * Abstract top level branch target buffer class. Exposes the necessary i/o for different
 * branch target buffers to be instantiated into BOOM.
 */
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
}

/**
 * A Null BTB that mades no predictions
 */
class NullBTB(implicit p: Parameters) extends BoomBTB
{
  io.resp.valid := false.B

  override def toString: String = "   ==Null BTB=="
}

/**
 * Factory object to create a branch target buffer (BTB)
 */
object BoomBTB
{
   /**
    * Create a specific type of branch target buffer (BTB) based on the parameters specified
    *
    * @param boomParams general boom core parameters that determine the BTB
    * @return a BoomBTB instance determined by the input parameters
    */
   def apply(boomParams: BoomCoreParams)(implicit p: Parameters): BoomBTB =
   {
      val boomParams: BoomCoreParams = p(freechips.rocketchip.tile.TileKey).core.asInstanceOf[BoomCoreParams]

      val enableBTBPredictor = boomParams.enableBTB

      var btb: BoomBTB = null

      // a BTB must be defined with the enable flag
      require(!enableBTBPredictor || (Seq(boomParams.btb.btbsa, boomParams.btb.densebtb).count(_ == true) == 1))

      // select BTB based on parameters
      if (enableBTBPredictor)
      {
        if (boomParams.btb.btbsa)
        {
          btb = Module(new BTBsa())
        }
        else if (boomParams.btb.densebtb)
        {
          btb = Module(new DenseBTB())
        }
      }
      else
      {
        btb = Module(new NullBTB())
      }

      btb
   }
}
