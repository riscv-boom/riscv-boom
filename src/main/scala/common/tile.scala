//******************************************************************************
// Copyright (c) 2017 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

package boom.common

import chisel3._

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{LogicalModuleTree, RocketLogicalTreeNode}
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem.RocketCrossingParams
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
    boundaryBuffers: Boolean = false // if synthesized with hierarchical PnR, cut feed-throughs?
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
    q: Parameters)
    extends BaseTile(boomParams, crossing, lookup, q)
    with SinksExternalInterrupts
    with SourcesExternalNotifications
    with HasBoomLazyRoCC  // implies CanHaveSharedFPU with CanHavePTW with HasHellaCache
    with CanHaveBoomPTW
    with HasBoomHellaCache
    with HasBoomICacheFrontend
{

  // Private constructor ensures altered LazyModule.p is used implicitly
  def this(params: BoomTileParams, crossing: RocketCrossingParams, lookup: LookupByHartIdImpl)(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)


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
    val beu = LazyModule(new BusErrorUnit(new L1BusErrors, BusErrorUnitParams(a)))
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

  nDCachePorts += 1 /*core */ + (dtim_adapter.isDefined).toInt

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

  override lazy val module = new BoomTileModuleImp(this)

  override def makeMasterBoundaryBuffers(implicit p: Parameters) = {
    if (!boomParams.boundaryBuffers) super.makeMasterBoundaryBuffers
    else TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1))
  }

  override def makeSlaveBoundaryBuffers(implicit p: Parameters) = {
    if (!boomParams.boundaryBuffers) super.makeSlaveBoundaryBuffers
    else TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none)
  }

  val fakeRocketParams = RocketTileParams(
    dcache = boomParams.dcache,
    hartId = boomParams.hartId,
    name   = boomParams.name,
    btb    = boomParams.btb,
    core = RocketCoreParams(
      bootFreqHz          = boomParams.core.bootFreqHz,
      useVM               = boomParams.core.useVM,
      useUser             = boomParams.core.useUser,
      useDebug            = boomParams.core.useDebug,
      useAtomics          = boomParams.core.useAtomics,
      useAtomicsOnlyForIO = boomParams.core.useAtomicsOnlyForIO,
      useCompressed       = boomParams.core.useCompressed,
      useSCIE             = boomParams.core.useSCIE,
      mulDiv              = boomParams.core.mulDiv,
      fpu                 = boomParams.core.fpu,
      nLocalInterrupts    = boomParams.core.nLocalInterrupts,
      nPMPs               = boomParams.core.nPMPs,
      nBreakpoints        = boomParams.core.nBreakpoints,
      nPerfCounters       = boomParams.core.nPerfCounters,
      haveBasicCounters   = boomParams.core.haveBasicCounters,
      misaWritable        = boomParams.core.misaWritable,
      haveCFlush          = boomParams.core.haveCFlush,
      nL2TLBEntries       = boomParams.core.nL2TLBEntries,
      mtvecInit           = boomParams.core.mtvecInit,
      mtvecWritable       = boomParams.core.mtvecWritable
    )
  )
  val rocketLogicalTree: RocketLogicalTreeNode = new RocketLogicalTreeNode(cpuDevice, fakeRocketParams, dtim_adapter, p(XLen), iCacheLogicalTreeNode)

}

/**
 * BOOM tile implicit
 *
 * @param outer top level BOOM tile
 */
class BoomTileModuleImp(outer: BoomTile) extends BaseTileModuleImp(outer)
    with HasBoomLazyRoCCModule
    with CanHaveBoomPTWModule
    with HasBoomHellaCacheModule
    with HasBoomICacheFrontendModule
{
  Annotated.params(this, outer.boomParams)

  val core = Module(new BoomCore()(outer.p, outer.dcache.module.edge))

  // Observe the Tilelink Channel C traffic leaving the L1D (writeback/releases).
  val tl_c = outer.dCacheTap.out(0)._1.c
  core.io.release.valid := tl_c.fire()
  core.io.release.bits.address := tl_c.bits.address

  // Report unrecoverable error conditions; for now the only cause is cache ECC errors
  outer.reportHalt(List(outer.frontend.module.io.errors, outer.dcache.module.io.errors))

  // Report when the tile has ceased to retire instructions; for now the only cause is clock gating
  //outer.reportCease(outer.boomParams.core.clockGate.option(
  //  !outer.dcache.module.io.cpu.clock_enabled &&
  //  !outer.frontend.module.io.cpu.clock_enabled &&
  //  !ptw.io.dpath.clock_enabled &&
  //  core.io.cease)) // clock-gating is not supported

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
  dcachePorts += core.io.dmem // TODO outer.dcachePorts += () => module.core.io.dmem ??
  //fpuOpt foreach { fpu => core.io.fpu <> fpu.io } RocketFpu - not needed in boom
  core.io.ptw <> ptw.io.dpath
  fcsr_rm := core.io.fcsr_rm
  core.io.rocc := DontCare
  core.io.reset_vector := DontCare


  if (outer.roccs.size > 0)
  {
     cmdRouter.get.io.in <> core.io.rocc.cmd
     outer.roccs.foreach(_.module.io.exception := core.io.rocc.exception)
     core.io.rocc.resp <> respArb.get.io.out
     core.io.rocc.busy <> (cmdRouter.get.io.busy || outer.roccs.map(_.module.io.busy).reduce(_||_))
     core.io.rocc.interrupt := outer.roccs.map(_.module.io.interrupt).reduce(_||_)
  }


  outer.dtim_adapter.foreach { lm => dcachePorts += lm.module.io.dmem }

  // TODO eliminate this redundancy
  val h = dcachePorts.size
  val c = core.dcacheArbPorts
  val o = outer.nDCachePorts
  require(h == c, s"port list size was $h, core expected $c")
  require(h == o, s"port list size was $h, outer counted $o")
  // TODO figure out how to move the below into their respective mix-ins
  dcacheArb.io.requestor <> dcachePorts
  ptwPorts += core.io.ptw_tlb
  ptw.io.requestor <> ptwPorts

  val frontendStr = outer.frontend.module.toString
  val coreStr = core.toString
  val boomTileStr =
    (BoomCoreStringPrefix(s"======BOOM Tile ${p(TileKey).hartId} Params======") + "\n"
    + frontendStr
    + coreStr + "\n")

  override def toString: String = boomTileStr

  print(boomTileStr)
}
