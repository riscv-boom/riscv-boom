package BOOM
{

import Chisel._
import Node._
import rocket._

case object DecodeWidth extends Field[Int]
case object DispatchWidth extends Field[Int]
case object IssueWidth extends Field[Int]
case object NumRobEntries extends Field[Int]
case object NumIntIssueSlotEntries extends Field[Int]
case object NumLsuEntries extends Field[Int]
case object NumPhysRegisters extends Field[Int]
case object MaxBrCount extends Field[Int]
case object FetchBufferSz extends Field[Int]
case object EnableFetchBufferFlowThrough extends Field[Boolean]
case object EnableBTB extends Field[Boolean]
case object EnableUarchCounters extends Field[Boolean]
case object EnablePrefetching extends Field[Boolean]
case object EnableCommitMapTable extends Field[Boolean]

abstract trait BOOMCoreParameters extends rocket.CoreParameters
{
   require(xLen == 64)

   //************************************
   // Superscalar Widths
   val FETCH_WIDTH      = params(FetchWidth)       // number of insts we can fetch
   val DECODE_WIDTH     = params(DecodeWidth)
   val DISPATCH_WIDTH   = params(DispatchWidth) // number of insts put into the IssueWindow
   val ISSUE_WIDTH      = params(IssueWidth)
   val COMMIT_WIDTH     = params(RetireWidth)

   require (DECODE_WIDTH == COMMIT_WIDTH)
   require (FETCH_WIDTH == 1 || FETCH_WIDTH == 2)
   require (DECODE_WIDTH <= FETCH_WIDTH)

   //************************************
   // Data Structure Sizes
   val NUM_ROB_ENTRIES          = params(NumRobEntries)     // number of ROB entries (e.g., 32 entries for R10k)
   val INTEGER_ISSUE_SLOT_COUNT = params(NumIntIssueSlotEntries)
   val NUM_LSU_ENTRIES          = params(NumLsuEntries)     // number of LD/ST entries
   val MAX_BR_COUNT             = params(MaxBrCount)        // number of branches we can speculate simultaneously
   val PHYS_REG_COUNT           = params(NumPhysRegisters)  // size of the unified, physical register file
   val FETCH_BUFFER_SZ          = params(FetchBufferSz)     // number of instructions that stored between fetch&decode

   //************************************
   // Pipelining

   //************************************
   // Load/Store Unit

   //************************************
   // Extra Knobs and Features
   val ENABLE_REGFILE_BYPASSING  = true  // bypass regfile write ports to read ports
   val MAX_WAKEUP_DELAY = 3              // unused

   //************************************
   // Implicitly calculated constants
   val NUM_ROB_ROWS      = NUM_ROB_ENTRIES/DECODE_WIDTH
   val ROB_ADDR_SZ       = log2Up(NUM_ROB_ENTRIES)
   // the f-registers are mapped into the space above the x-registers
   val LOGICAL_REG_COUNT = if (params(BuildFPU).isEmpty) 32 else 64 
   val LREG_SZ           = log2Up(LOGICAL_REG_COUNT)
   val PREG_SZ           = log2Up(PHYS_REG_COUNT)
   val MEM_ADDR_SZ       = log2Up(NUM_LSU_ENTRIES)
   val MAX_ST_COUNT      = (1 << MEM_ADDR_SZ)
   val MAX_LD_COUNT      = (1 << MEM_ADDR_SZ)
   val BR_TAG_SZ         = log2Up(MAX_BR_COUNT)

   require (PHYS_REG_COUNT >= (LOGICAL_REG_COUNT + DECODE_WIDTH))
   require (MAX_BR_COUNT >=2)
   require (NUM_ROB_ROWS % 2 == 0)
   require (NUM_ROB_ENTRIES % DECODE_WIDTH == 0)
   require (isPow2(NUM_LSU_ENTRIES))

   //************************************
   // Non-BOOM parameters

   val corePAddrBits = params(uncore.PAddrBits)

   val fastMulDiv = params(FastMulDiv)

}


}
