package BOOM
{
import Chisel._
import Node._


case class BOOMConfiguration(rc: rocket.RocketConfiguration,
                             iwc: IssueConfig
                            )
{
   require (rc.xprlen == 64)
}
 
case class IssueConfig() {
   // TODO
   // Keep these in sync with riscv-boom/consts.scala
//   val optype_bits       = 8
//   val issue_width       = 2 // DISPATCH_WIDTH
//   val fu_count          = 3 // EXECUTE_WIDTH
//   val wb_width          = 3 // NUM_WAKEUP_PORTS
//   val buffer_width      = 8
//   val max_age           = 1
//   val tag_bits          = 5 // PREG_SZ
//   val buffer_width_bits = 8
}

}
