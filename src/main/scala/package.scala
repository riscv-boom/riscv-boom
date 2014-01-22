
//package BOOM
//import BOOM.constants._
//
//import Chisel._
//import scala.math._
//
////TODO: When compiler bug SI-5604 is fixed in 2.10, change object Constants to 
////      package object rocket and remove import Constants._'s from other files
//object Constants extends
//   BOOMProcConstants with
//   LoadStoreUnitConstants with
//   BrPredConstants with
//   ScalarOpConstants with
//   Common.constants.ExcCauseConstants with 
//   InterruptConstants with
//   RocketDcacheConstants with
//   uncore.constants.MemoryOpConstants with
//   uncore.constants.MemoryInterfaceConstants
//{
//  val START_ADDR = 0x2000
//}



package object BOOM extends 
   BOOM.constants.BOOMProcConstants with
   BOOM.constants.LoadStoreUnitConstants with
   BOOM.constants.BrPredConstants with
   BOOM.constants.ScalarOpConstants with
   BOOM.constants.ExcCauseConstants with 
//   InterruptConstants with
//   RocketDcacheConstants with
//  BOOM.constants.ScalarOpConstants with
   BOOM.constants.InterruptConstants with
   BOOM.constants.RISCVConstants 
//  BOOM.constants.VectorOpConstants
{
   val START_ADDR = 0x2000
}
