//******************************************************************************
// Copyright (c) 2017 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.common

import chisel3._
import chisel3.util.{RRArbiter, Queue}

import scala.collection.mutable.{ListBuffer}

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{LogicalTreeNode }
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem.{RocketCrossingParams}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

import boom.exu._
import boom.ifu._
import boom.lsu._
import boom.util.{BoomCoreStringPrefix}

/**
 * BOOM tile parameter class used in configurations
 *
 * @param core BOOM core params
 * @param icache i$ params
 * @param dcache d$ params
 * @param btb btb params
 * @param dataScratchpadBytes ...
 * @param trace ...
 * @param hcfOnUncorrectable ...
 * @param name name of tile
 * @param hartId hardware thread id
 * @param blockerCtrlAddr ...
 * @param boundaryBuffers ...
 */
case class BoomTileParams(
  core: BoomCoreParams = BoomCoreParams(),
  icache: Option[ICacheParams] = Some(ICacheParams()),
  dcache: Option[DCacheParams] = Some(DCacheParams()),
  btb: Option[BTBParams] = Some(BTBParams()),
  dataScratchpadBytes: Int = 0,
  trace: Boolean = false,
  name: Option[String] = Some("boom_tile"),
  hartId: Int = 0,
  beuAddr: Option[BigInt] = None,
  blockerCtrlAddr: Option[BigInt] = None,
  boundaryBuffers: Boolean = false, // if synthesized with hierarchical PnR, cut feed-throughs?
  dromajoParams: Option[DromajoParams] = None
  ) extends TileParams
{
  require(icache.isDefined)
  require(dcache.isDefined)
}

/**
 * BOOM tile
 *
 * @param boomParams BOOM tile params
 * @param crossing ...
 */
class BoomTile(
  val boomParams: BoomTileParams,
  crossing: ClockCrossingType,
  lookup: LookupByHartIdImpl,
  q: Parameters,
  logicalTreeNode: LogicalTreeNode)
  extends BaseTile(boomParams, crossing, lookup, q)
  with SinksExternalInterrupts
  with SourcesExternalNotifications
{

  // Private constructor ensures altered LazyModule.p is used implicitly
  def this(params: BoomTileParams, crossing: RocketCrossingParams, lookup: LookupByHartIdImpl, logicalTreeNode: LogicalTreeNode)(implicit p: Parameters) =
    this(params.copy(dromajoParams = Some(DromajoParams(Some(p(BootROMParams)), p(ExtMem), p(CLINTKey), p(PLICKey)))), crossing.crossingType, lookup, p, logicalTreeNode)

  val intOutwardNode = IntIdentityNode()
  val slaveNode = TLIdentityNode()
  val masterNode = visibilityNode

  val dtim_adapter = tileParams.dcache.flatMap { d => d.scratch.map(s =>
    LazyModule(new ScratchpadSlavePort(AddressSet.misaligned(s, d.dataScratchpadBytes-1),
                                       xBytes,
                                       tileParams.core.useAtomics && !tileParams.core.useAtomicsOnlyForIO)))
  }
  dtim_adapter.foreach(lm => connectTLSlave(lm.node, xBytes))

  val bus_error_unit = boomParams.beuAddr map { a =>
    val beu = LazyModule(new BusErrorUnit(new L1BusErrors, BusErrorUnitParams(a), logicalTreeNode))
    intOutwardNode := beu.intNode
    connectTLSlave(beu.node, xBytes)
    beu
  }

  val tile_master_blocker =
    tileParams.blockerCtrlAddr
      .map(BasicBusBlockerParams(_, xBytes, masterPortBeatBytes, deadlock = true))
      .map(bp => LazyModule(new BasicBusBlocker(bp)))

  tile_master_blocker.foreach(lm => connectTLSlave(lm.controlNode, xBytes))

  // TODO: this doesn't block other masters, e.g. RoCCs
  tlOtherMastersNode := tile_master_blocker.map { _.node := tlMasterXbar.node } getOrElse { tlMasterXbar.node }
  masterNode :=* tlOtherMastersNode
  DisableMonitors { implicit p => tlSlaveXbar.node :*= slaveNode }

  val dtimProperty = dtim_adapter.map(d => Map(
    "sifive,dtim" -> d.device.asProperty)).getOrElse(Nil)

  val itimProperty = tileParams.icache.flatMap(_.itimAddr.map(i => Map(
    "sifive,itim" -> frontend.icache.device.asProperty))).getOrElse(Nil)

  val cpuDevice: SimpleDevice = new SimpleDevice("cpu", Seq("ucb-bar,boom0", "riscv")) {
    override def parent = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++
                        cpuProperties ++
                        nextLevelCacheProperty ++
                        tileProperties ++
                        dtimProperty ++
                        itimProperty)
    }
  }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceAddress(hartId))
  }

  override def makeMasterBoundaryBuffers(implicit p: Parameters) = {
    if (!boomParams.boundaryBuffers) super.makeMasterBoundaryBuffers
    else TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1))
  }

  override def makeSlaveBoundaryBuffers(implicit p: Parameters) = {
    if (!boomParams.boundaryBuffers) super.makeSlaveBoundaryBuffers
    else TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none)
  }

  override lazy val module = new BoomTileModuleImp(this)

  // DCache
  lazy val dcache: BoomNonBlockingDCache = LazyModule(new BoomNonBlockingDCache(hartId))
  val dCacheTap = TLIdentityNode()
  tlMasterXbar.node := dCacheTap := dcache.node


  // Frontend/ICache
  val frontend = LazyModule(new BoomFrontend(tileParams.icache.get, hartId))
  tlMasterXbar.node := frontend.masterNode

  // ROCC
  val roccs = p(BuildRoCC).map(_(p))
  roccs.map(_.atlNode).foreach { atl => tlMasterXbar.node :=* atl }
  roccs.map(_.tlNode).foreach { tl => tlOtherMastersNode :=* tl }
}

/**
 * BOOM tile implementation
 *
 * @param outer top level BOOM tile
 */
class BoomTileModuleImp(outer: BoomTile) extends BaseTileModuleImp(outer){

  Annotated.params(this, outer.boomParams)

  val core = Module(new BoomCore()(outer.p))
  val lsu  = Module(new LSU()(outer.p, outer.dcache.module.edge))

  val ptwPorts         = ListBuffer(lsu.io.ptw, outer.frontend.module.io.ptw, core.io.ptw_tlb)

  val hellaCachePorts  = ListBuffer[HellaCacheIO]()

  outer.reportWFI(None) // TODO: actually report this?

  outer.decodeCoreInterrupts(core.io.interrupts) // Decode the interrupt vector

  outer.bus_error_unit.foreach { beu =>
    core.io.interrupts.buserror.get := beu.module.io.interrupt
    beu.module.io.errors.dcache := outer.dcache.module.io.errors
    beu.module.io.errors.icache := outer.frontend.module.io.errors
  }

  // Pass through various external constants and reports
  outer.traceSourceNode.bundle <> core.io.trace
  outer.bpwatchSourceNode.bundle <> DontCare // core.io.bpwatch
  core.io.hartid := constants.hartid
  outer.dcache.module.io.hartid := constants.hartid
  outer.frontend.module.io.hartid := constants.hartid
  outer.frontend.module.io.reset_vector := constants.reset_vector

  // Connect the core pipeline to other intra-tile modules
  outer.frontend.module.io.cpu <> core.io.ifu
  core.io.lsu <> lsu.io.core

  //fpuOpt foreach { fpu => core.io.fpu <> fpu.io } RocketFpu - not needed in boom
  core.io.rocc := DontCare
  core.io.reset_vector := DontCare

  if (outer.roccs.size > 0) {
    val (respArb, cmdRouter) = {
      val respArb = Module(new RRArbiter(new RoCCResponse()(outer.p), outer.roccs.size))
      val cmdRouter = Module(new RoccCommandRouter(outer.roccs.map(_.opcodes))(outer.p))
      outer.roccs.zipWithIndex.foreach { case (rocc, i) =>
        ptwPorts ++= rocc.module.io.ptw
        rocc.module.io.cmd <> cmdRouter.io.out(i)
        val dcIF = Module(new SimpleHellaCacheIF()(outer.p))
        dcIF.io.requestor <> rocc.module.io.mem
        hellaCachePorts += dcIF.io.cache
        respArb.io.in(i) <> Queue(rocc.module.io.resp)
      }
      // Create this FPU just for RoCC
      val nFPUPorts = outer.roccs.filter(_.usesFPU).size
      if (nFPUPorts > 0) {
        val fpuOpt = outer.tileParams.core.fpu.map(params => Module(new freechips.rocketchip.tile.FPU(params)(outer.p)))
        // TODO: Check this FPU works properly
        fpuOpt foreach { fpu =>
          // This FPU does not get CPU requests
          fpu.io := DontCare
          fpu.io.fcsr_rm := core.io.fcsr_rm
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
      (respArb, cmdRouter)
    }

    cmdRouter.io.in <> core.io.rocc.cmd
    outer.roccs.foreach(_.module.io.exception := core.io.rocc.exception)
    core.io.rocc.resp <> respArb.io.out
    core.io.rocc.busy <> (cmdRouter.io.busy || outer.roccs.map(_.module.io.busy).reduce(_||_))
    core.io.rocc.interrupt := outer.roccs.map(_.module.io.interrupt).reduce(_||_)
  }

  // PTW
  val ptw  = Module(new PTW(ptwPorts.length)(outer.dcache.node.edges.out(0), outer.p))
  core.io.ptw <> ptw.io.dpath
  ptw.io.requestor <> ptwPorts
  hellaCachePorts += ptw.io.mem

   // LSU IO
  val hellaCacheArb = Module(new HellaCacheArbiter(hellaCachePorts.length)(outer.p))
  hellaCacheArb.io.requestor <> hellaCachePorts
  lsu.io.hellacache <> hellaCacheArb.io.mem
  outer.dcache.module.io.lsu <> lsu.io.dmem

  // Generate a descriptive string
  val frontendStr = outer.frontend.module.toString
  val coreStr = core.toString
  val boomTileStr =
    (BoomCoreStringPrefix(s"======BOOM Tile ${p(TileKey).hartId} Params======") + "\n"
    + frontendStr
    + coreStr + "\n")

  override def toString: String = boomTileStr

  print(boomTileStr)
}
