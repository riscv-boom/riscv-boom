package boom.common

import chisel3._
import chisel3.util.{RRArbiter, Queue}

import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._

import boom.lsu._

/*
 * Different from normal HasLazyRoCC because we create a new FPU
 */
trait HasBoomLazyRoCC extends CanHaveBoomPTW { this: BaseTile =>
   val roccs = p(BuildRoCC).map(_(p))

   roccs.map(_.atlNode).foreach { atl => tlMasterXbar.node :=* atl }
   roccs.map(_.tlNode).foreach { tl => tlOtherMastersNode :=* tl }

   nPTWPorts += roccs.map(_.nPTWPorts).foldLeft(0)(_+_)
   nDCachePorts += roccs.size
}

trait HasBoomLazyRoCCModule extends CanHaveBoomPTWModule
      with HasCoreParameters { this: BoomTileModuleImp =>

  val fcsr_rm = Wire(UInt(freechips.rocketchip.tile.FPConstants.RM_SZ.W))
  val (respArb, cmdRouter) = if(outer.roccs.size > 0) {
    val respArb = Module(new RRArbiter(new RoCCResponse()(outer.p), outer.roccs.size))
    val cmdRouter = Module(new RoccCommandRouter(outer.roccs.map(_.opcodes))(outer.p))
    outer.roccs.zipWithIndex.foreach { case (rocc, i) =>
      ptwPorts ++= rocc.module.io.ptw
      rocc.module.io.cmd <> cmdRouter.io.out(i)
      val dcIF = Module(new SimpleHellaCacheIF()(outer.p))
      dcIF.io.requestor <> rocc.module.io.mem
      dcachePorts += dcIF.io.cache
      respArb.io.in(i) <> Queue(rocc.module.io.resp)
    }
    // Create this FPU just for RoCC
    val nFPUPorts = outer.roccs.filter(_.usesFPU).size
    if (nFPUPorts > 0)
    {
       require(usingFPU)
       val fpuOpt = outer.tileParams.core.fpu.map(params => Module(new FPU(params)(outer.p)))
       fpuOpt foreach { fpu =>
          // This FPU does not get CPU requests
          fpu.io := DontCare
          fpu.io.fcsr_rm := fcsr_rm
          fpu.io.dmem_resp_val := false.B
          fpu.io.valid := false.B
          fpu.io.killx := false.B
          fpu.io.killm := false.B

          val fpArb = Module(new InOrderArbiter(new FPInput()(outer.p), new FPResult()(outer.p), nFPUPorts))
          val fp_rocc_ios = outer.roccs.filter(_.usesFPU).map(_.module.io)
          fpArb.io.in_req <> fp_rocc_ios.map(_.fpu_req)
          fp_rocc_ios.zip(fpArb.io.in_resp).foreach {
             case (rocc, arb) => rocc.fpu_resp <> arb
          }
          fpu.io.cp_req <> fpArb.io.out_req
          fpArb.io.out_resp <> fpu.io.cp_resp
       }
    }
    (Some(respArb), Some(cmdRouter))
  } else {
    (None, None)
  }
}
