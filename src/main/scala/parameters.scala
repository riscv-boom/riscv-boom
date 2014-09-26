package BOOM
{

import Chisel._
import Node._
import rocket._

case object FetchWidth extends Field[Int]
case object DecodeWidth extends Field[Int]
case object DispatchWidth extends Field[Int]
case object IssueWidth extends Field[Int]
case object EnableFetchBufferFlowThrough extends Field[Boolean]

abstract trait BOOMCoreParameters extends rocket.CoreParameters {
   require(xprLen == 64)
   require(params(UseVM) == false)
   
   //************************************
   // Superscalar Widths
   val FETCH_WIDTH      = params(FetchWidth)       // number of insts we can fetch
   val DECODE_WIDTH     = params(DecodeWidth)
   val DISPATCH_WIDTH   = params(DispatchWidth) // number of insts put into the IssueWindow
   val ISSUE_WIDTH      = params(IssueWidth)
   val COMMIT_WIDTH     = params(RetireWidth); require (DECODE_WIDTH == COMMIT_WIDTH)
  
   //************************************
   // Pipelining
   val ENABLE_FETCH_BUFFER_FLOW_THROUGH = params(EnableFetchBufferFlowThrough)
  
   //************************************
   // Extra Knobs and Features
   val ENABLE_PREFETCHING        = false
   val ENABLE_BTB                = true
   val ENABLE_REGFILE_BYPASSING  = true  // bypass regfile write ports to read ports
   val ENABLE_COMMIT_MAP_TABLE   = false // track the committed rename state; allows
    
   val IC_NUM_SETS = 128
   val IC_NUM_WAYS = 2
   val DC_NUM_SETS = 128
   val DC_NUM_WAYS = 2
   val DC_NUM_MSHR = 2    // secondary miss handler
                                      // for single-cycle resets.

   val BTB_NUM_ENTRIES = 32
   val INTEGER_ISSUE_SLOT_COUNT = 12
   val NUM_LSU_ENTRIES          = 8 // number of LD/ST entries
   val NUM_ROB_ENTRIES          = 20 // number of ROB entries (e.g., 32 entries for R10k)
   val NUM_ROB_ROWS             = NUM_ROB_ENTRIES/DECODE_WIDTH
   val ROB_ADDR_SZ = log2Up(NUM_ROB_ENTRIES)
   
   val MAX_WAKEUP_DELAY = 3 // unused
   val FETCH_BUFFER_SZ = 4 // number of instructions that can be stored between fetch + decode
   
   // size of the unified, physical register file
   val PHYS_REG_COUNT = 50; require(PHYS_REG_COUNT >= (32 + DECODE_WIDTH))

   // if pipeline goes idle, throw error
   // otherwise, reset pipeline and restart TODO on this feature
   val ON_IDLE_THROW_ERROR = true

   val MAX_BR_COUNT = 8   // number of branches we can speculate simultaneously
   require(MAX_BR_COUNT >=2)

   // Implicitly calculated constants
   val LOGICAL_REG_COUNT = 32
   val LREG_SZ           = log2Up(LOGICAL_REG_COUNT)
   val PREG_SZ           = log2Up(PHYS_REG_COUNT)
   val MEM_ADDR_SZ       = log2Up(NUM_LSU_ENTRIES)
   val MAX_ST_COUNT      = (1 << MEM_ADDR_SZ)
   val MAX_LD_COUNT      = (1 << MEM_ADDR_SZ)
   val BR_TAG_SZ         = log2Up(MAX_BR_COUNT)
                          
   require (NUM_ROB_ROWS % 2 == 0)
   require (NUM_ROB_ENTRIES % DECODE_WIDTH == 0)
   require (isPow2(NUM_LSU_ENTRIES))
 

   val vaddrBits = params(uncore.VAddrBits)
   val fastMulDiv = params(FastMulDiv)

   require(FETCH_WIDTH == 1 || FETCH_WIDTH == 2)
   require(DECODE_WIDTH <= FETCH_WIDTH)
}


}
