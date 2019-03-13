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
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

import boom.exu._
import boom.ifu._
import boom.lsu._

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
    name: Option[String] = Some("tile"),
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
    crossing: ClockCrossingType)
  (implicit p: Parameters) extends BaseTile(boomParams, crossing)(p)
    with SinksExternalInterrupts
    with SourcesExternalNotifications
    //with HasLazyRoCC  // implies CanHaveSharedFPU with CanHavePTW with HasHellaCache
    with CanHaveBoomPTW
    with HasBoomHellaCache
    with HasBoomICacheFrontend
{

  val intOutwardNode = IntIdentityNode()
  val slaveNode = TLIdentityNode()
  val masterNode = TLIdentityNode()

  val dtim_adapter = tileParams.dcache.flatMap { d => d.scratch.map(s =>
    LazyModule(new ScratchpadSlavePort(AddressSet(s, d.dataScratchpadBytes-1),
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

  val cpuDevice = new SimpleDevice("cpu", Seq("ucb-bar,boom0", "riscv")) {
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

    override def getOMComponents(resourceBindingsMap: ResourceBindingsMap): Seq[OMComponent] = {
      val cores = getOMRocketCores(resourceBindingsMap)
      cores
    }

    def getOMICacheFromBindings(resourceBindingsMap: ResourceBindingsMap): Option[OMICache] = {
      boomParams.icache.map(i => frontend.icache.device.getOMComponents(resourceBindingsMap) match {
        case Seq() => throw new IllegalArgumentException
        case Seq(h) => h.asInstanceOf[OMICache]
        case _ => throw new IllegalArgumentException
      })
    }

    def getOMDCacheFromBindings(dCacheParams: DCacheParams, resourceBindingsMap: ResourceBindingsMap): Option[OMDCache] = {
      val omDTIM: Option[OMDCache] = dtim_adapter.map(_.device.getMemory(dCacheParams, resourceBindingsMap))
      val omDCache: Option[OMDCache] = tileParams.dcache.filterNot(_.scratch.isDefined).map(OMCaches.dcache(_, None))

      require(!(omDTIM.isDefined && omDCache.isDefined))

      omDTIM.orElse(omDCache)
    }

    def getOMRocketCores(resourceBindingsMap: ResourceBindingsMap): Seq[OMRocketCore] = {
      val coreParams = rocketCoreParams(boomParams.core)

      val omICache = getOMICacheFromBindings(resourceBindingsMap)

      val omDCache = boomParams.dcache.flatMap{ getOMDCacheFromBindings(_, resourceBindingsMap)}

      Seq(OMRocketCore(
        isa = OMISA.rocketISA(coreParams, xLen),
        mulDiv =  coreParams.mulDiv.map{ md => OMMulDiv.makeOMI(md, xLen)},
        fpu = coreParams.fpu.map{f => OMFPU(fLen = f.fLen)},
        performanceMonitor = PerformanceMonitor.permon(coreParams),
        pmp = OMPMP.pmp(coreParams),
        documentationName = "TODO",
        hartIds = Seq(hartId),
        hasVectoredInterrupts = true,
        interruptLatency = 4,
        nLocalInterrupts = coreParams.nLocalInterrupts,
        nBreakpoints = coreParams.nBreakpoints,
        branchPredictor = boomParams.btb.map(OMBTB.makeOMI),
        dcache = omDCache,
        icache = omICache
      ))
    }

    def rocketCoreParams(params: BoomCoreParams): RocketCoreParams = {
      RocketCoreParams(
        bootFreqHz = params.bootFreqHz,
        useVM = params.useVM,
        useUser = params.useUser,
        useDebug = params.useDebug,
        useAtomics = params.useAtomics,
        useAtomicsOnlyForIO = params.useAtomicsOnlyForIO,
        useCompressed = params.useCompressed,
        useSCIE = params.useSCIE,
        mulDiv = params.mulDiv,
        fpu = params.fpu,
        nLocalInterrupts = params.nLocalInterrupts,
        nPMPs = params.nPMPs,
        nBreakpoints = params.nBreakpoints,
        nPerfCounters = params.nPerfCounters,
        haveBasicCounters = params.haveBasicCounters,
        misaWritable = params.misaWritable,
        haveCFlush = params.haveCFlush,
        nL2TLBEntries = params.nL2TLBEntries,
        mtvecInit = params.mtvecInit,
        mtvecWritable = params.mtvecWritable
      )
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
}

/**
 * BOOM tile implicit
 *
 * @param outer top level BOOM tile
 */
class BoomTileModuleImp(outer: BoomTile) extends BaseTileModuleImp(outer)
    //with HasFpuOpt
    //with HasLazyRoCCModule
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
  core.io.hartid := constants.hartid
  outer.dcache.module.io.hartid := constants.hartid
  outer.frontend.module.io.hartid := constants.hartid
  outer.frontend.module.io.reset_vector := constants.reset_vector

  // Connect the core pipeline to other intra-tile modules
  outer.frontend.module.io.cpu <> core.io.ifu
  dcachePorts += core.io.dmem // TODO outer.dcachePorts += () => module.core.io.dmem ??
  //fpuOpt foreach { fpu => core.io.fpu <> fpu.io } RocketFpu - not needed in boom
  core.io.ptw := DontCare
  if (usingPTW)
  {
    core.io.ptw <> ptw.get.io.dpath
  }
  core.io.rocc := DontCare
  core.io.fpu := DontCare
  core.io.reset_vector := DontCare

  // Connect the coprocessor interface
  //if (outer.roccs.size > 0) {
  //  cmdRouter.get.io.in <> core.io.rocc.cmd
  //  outer.roccs.foreach(_.module.io.exception := core.io.rocc.exception)
  //  core.io.rocc.resp <> respArb.get.io.out
  //  core.io.rocc.busy <> (cmdRouter.get.io.busy || outer.roccs.map(_.module.io.busy).reduce(_ || _))
  //  core.io.rocc.interrupt := outer.roccs.map(_.module.io.interrupt).reduce(_ || _)
  //} // rocc is not supported

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
  core.io.ptw_tlb := DontCare
  if (usingPTW)
  {
    ptw.get.io.requestor <> ptwPorts
  }
  val frontendStr = outer.frontend.module.toString
  ElaborationArtefacts.add(
    """core.config""",
    frontendStr + core.toString + "\n"
  )
  print(outer.frontend.module.toString + core.toString + "\n")
}
