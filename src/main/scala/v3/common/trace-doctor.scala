//******************************************************************************
// TraceDoctor oracle trace port (ported from EECS-NTNU TraceDoctor / TEA)
//------------------------------------------------------------------------------
// A wide, payload-defined trace vector emitted on commit-stage state changes.
// Consumed by the FireSim TraceDoctor bridge; the bit layout is a contract
// with the host-side oracle worker (see core.scala packing block).
//******************************************************************************

package boom.v3.common

import chisel3._

class BoomTraceDoctorIO(val traceWidth: Int) extends Bundle {
  val valid = Bool()
  val bits  = UInt(traceWidth.W)
}
