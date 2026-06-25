//******************************************************************************
// Copyright (c) 2017 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.v3.common

import chisel3._
import chisel3.util.{RRArbiter, Queue}

import scala.collection.mutable.{ListBuffer}

import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem.{RocketCrossingParams}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

import boom.v3.exu._
import boom.v3.ifu._
import boom.v3.lsu._
import boom.v3.util.{BoomCoreStringPrefix}
import freechips.rocketchip.prci.ClockSinkParameters


case class BoomTileAttachParams(
  tileParams: BoomTileParams,
  crossingParams: RocketCrossingParams
) extends CanAttachTile {
  type TileType = BoomTile
  val lookup = PriorityMuxHartIdFromSeq(Seq(tileParams))
}


/**
 * BOOM tile parameter class used in configurations
 *
 */
case class BoomTileParams(
  core: BoomCoreParams = BoomCoreParams(),
  icache: Option[ICacheParams] = Some(ICacheParams()),
  dcache: Option[DCacheParams] = Some(DCacheParams()),
  btb: Option[BTBParams] = Some(BTBParams()),
  name: Option[String] = Some("boom_tile"),
  tileId: Int = 0
) extends InstantiableTileParams[BoomTile]
{
  require(icache.isDefined)
  require(dcache.isDefined)
  def instantiate(crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters): BoomTile = {
    new BoomTile(this, crossing, lookup)
  }
  val beuAddr: Option[BigInt] = None
  val blockerCtrlAddr: Option[BigInt] = None
  val boundaryBuffers: Boolean = false // if synthesized with hierarchical PnR, cut feed-throughs?
  val clockSinkParams: ClockSinkParameters = ClockSinkParameters()
  val baseName = name.getOrElse("boom_tile")
  val uniqueName = s"${baseName}_$tileId"
}

/**
 * BOOM tile
 *
 */
class BoomTile private(
  val boomParams: BoomTileParams,
  crossing: ClockCrossingType,
  lookup: LookupByHartIdImpl,
  q: Parameters)
  extends BaseTile(boomParams, crossing, lookup, q)
  with SinksExternalInterrupts
  with SourcesExternalNotifications
{

  // Private constructor ensures altered LazyModule.p is used implicitly
  def this(params: BoomTileParams, crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)

  val intOutwardNode = None
  val masterNode = TLIdentityNode()
  val slaveNode = TLIdentityNode()

  // Connect slaveNode to the slave crossbar (needed for MMIO devices inside the tile)
  DisableMonitors { implicit p => tlSlaveXbar.node :*= slaveNode }

  val tile_master_blocker =
    tileParams.blockerCtrlAddr
      .map(BasicBusBlockerParams(_, xBytes, masterPortBeatBytes, deadlock = true))
      .map(bp => LazyModule(new BasicBusBlocker(bp)))

  tile_master_blocker.foreach(lm => connectTLSlave(lm.controlNode, xBytes))

  // TODO: this doesn't block other masters, e.g. RoCCs
  tlOtherMastersNode := tile_master_blocker.map { _.node := tlMasterXbar.node } getOrElse { tlMasterXbar.node }
  masterNode :=* tlOtherMastersNode

  val cpuDevice: SimpleDevice = new SimpleDevice("cpu", Seq("ucb-bar,boom0", "riscv")) {
    override def parent = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++
                        cpuProperties ++
                        nextLevelCacheProperty ++
                        tileProperties)
    }
  }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceAddress(tileId))
  }

  override def makeMasterBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = crossing match {
    case _: RationalCrossing =>
      if (!boomParams.boundaryBuffers) TLBuffer(BufferParams.none)
      else TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1))
    case _ => TLBuffer(BufferParams.none)
  }

  override def makeSlaveBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = crossing match {
    case _: RationalCrossing =>
      if (!boomParams.boundaryBuffers) TLBuffer(BufferParams.none)
      else TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none)
    case _ => TLBuffer(BufferParams.none)
  }

  override lazy val module = new BoomTileModuleImp(this)

  // DCache
  lazy val dcache: BoomNonBlockingDCache = LazyModule(new BoomNonBlockingDCache(tileId))
  val dCacheTap = TLIdentityNode()
  tlMasterXbar.node := dCacheTap := TLWidthWidget(tileParams.dcache.get.rowBits/8) := visibilityNode := dcache.node


  // Frontend/ICache
  val frontend = LazyModule(new BoomFrontend(tileParams.icache.get, tileId))
  frontend.resetVectorSinkNode := resetVectorNexusNode
  tlMasterXbar.node := TLWidthWidget(tileParams.icache.get.rowBits/8) := frontend.masterNode

  // require(tileParams.dcache.get.rowBits == tileParams.icache.get.rowBits)

  // ROCC
  val roccs = p(BuildRoCC).map(_(p))
  roccs.map(_.atlNode).foreach { atl => tlMasterXbar.node :=* atl }
  roccs.map(_.tlNode).foreach { tl => tlOtherMastersNode :=* tl }

  // TMA Performance Counter MMIO Device (optional)
  val perfCounterDevice = if (boomParams.core.enableTMACounters) {
    val params = BoomPerfCounterParams(address = 0x10030000L + tileId * 0x1000L)
    val dev = LazyModule(new BoomPerfCounterDevice(params, xBytes))
    connectTLSlave(dev.node, xBytes)
    Some(dev)
  } else None

  // BundleBridge sink for L2 performance counters (wired from subsystem via diplomacy)
  val l2PerfCounterSinkNode: Option[BundleBridgeSink[Vec[UInt]]] = if (boomParams.core.enableTMACounters) {
    Some(BundleBridgeSink[Vec[UInt]](Some(() => Vec(BoomPerfCounterConsts.L2_NUM_COUNTERS, UInt(64.W))))) // sized to L2_NUM_COUNTERS (18)
  } else None
}

/**
 * BOOM tile implementation
 *
 * @param outer top level BOOM tile
 */
class BoomTileModuleImp(outer: BoomTile) extends BaseTileModuleImp(outer){

  val core = Module(new BoomCore()(outer.p))
  val lsu  = Module(new LSU()(outer.p, outer.dcache.module.edge))

  val ptwPorts         = ListBuffer(lsu.io.ptw, outer.frontend.module.io.ptw, core.io.ptw_tlb)

  val hellaCachePorts  = ListBuffer[HellaCacheIO]()

  outer.reportWFI(None) // TODO: actually report this?

  outer.decodeCoreInterrupts(core.io.interrupts) // Decode the interrupt vector

  // Pass through various external constants and reports
  outer.traceSourceNode.bundle <> core.io.trace
  outer.bpwatchSourceNode.bundle <> DontCare // core.io.bpwatch
  core.io.hartid := outer.hartIdSinkNode.bundle

  // Connect the core pipeline to other intra-tile modules
  outer.frontend.module.io.cpu <> core.io.ifu
  core.io.lsu <> lsu.io.core

  //fpuOpt foreach { fpu => core.io.fpu <> fpu.io } RocketFpu - not needed in boom
  core.io.rocc := DontCare

  // RoCC
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
      // first keep fpu ios unconnected
      val fp_ios = outer.roccs.map(r => {
        val roccio = r.module.io
        roccio.fpu_req.ready := true.B
        roccio.fpu_resp.valid := false.B
        roccio.fpu_resp.bits := DontCare
      })
      // Create this FPU just for RoCC
      val nFPUPorts = outer.roccs.filter(_.usesFPU).size
      if (nFPUPorts > 0) {
        val fpuOpt = outer.tileParams.core.fpu.map(params => Module(new freechips.rocketchip.tile.FPU(params)(outer.p)))
        // TODO: Check this FPU works properly
        fpuOpt foreach { fpu =>
          // This FPU does not get CPU requests
          fpu.io := DontCare
          fpu.io.fcsr_rm := core.io.fcsr_rm
          fpu.io.ll_resp_val := false.B
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
  ptw.io.requestor <> ptwPorts.toSeq
  ptw.io.mem +=: hellaCachePorts

   // LSU IO
  val hellaCacheArb = Module(new HellaCacheArbiter(hellaCachePorts.length)(outer.p))
  hellaCacheArb.io.requestor <> hellaCachePorts.toSeq
  lsu.io.hellacache <> hellaCacheArb.io.mem
  outer.dcache.module.io.lsu <> lsu.io.dmem

  // L2 performance counters received via BundleBridge diplomacy from subsystem
  val l2PerfCounters = outer.l2PerfCounterSinkNode.map(_.bundle)

  // TMA Performance Counter MMIO wiring
  outer.perfCounterDevice.foreach { dev =>
    core.io.tma_counters.foreach { ctrs =>
      dev.module.io.counters := ctrs
      // Override inline L2 counter slots (75-91) with actual L2 counter values
      l2PerfCounters.foreach { l2ctrs =>
        for (i <- 0 until BoomPerfCounterConsts.L2_INLINE_NUM_COUNTERS) {
          dev.module.io.counters(BoomPerfCounterConsts.CORE_NUM_COUNTERS + BoomPerfCounterConsts.MEM_ORDER_NUM_COUNTERS + BoomPerfCounterConsts.DATA_DEP_NUM_COUNTERS + i) := l2ctrs(i)
        }
        // l2_demand_miss_pending: appended at end (index 109) to avoid shifting existing indices
        dev.module.io.counters(BoomPerfCounterConsts.NUM_COUNTERS - 1) := l2ctrs(BoomPerfCounterConsts.L2_INLINE_NUM_COUNTERS)
      }
    }
  }

  // DPI-C counter dump at simulation end (gated by +dump-tma-counters plusarg)
  // Only instantiated when enableTMASimDump is true (Verilator/VCS only).
  // Do NOT enable for FPGA, FireSim, or ASIC flows — DPI-C is unsupported.
  if (outer.boomParams.core.enableTMASimDump) {
    core.io.tma_counters.foreach { ctrs =>
      val dump = Module(new SimTMACounterDump(BoomPerfCounterConsts.NUM_COUNTERS, outer.tileId))
      dump.io.clock := clock
      dump.io.reset := reset.asBool
      dump.io.counters := ctrs
      // Override inline L2 counter slots with actual values
      l2PerfCounters.foreach { l2ctrs =>
        for (i <- 0 until BoomPerfCounterConsts.L2_INLINE_NUM_COUNTERS) {
          dump.io.counters(BoomPerfCounterConsts.CORE_NUM_COUNTERS + BoomPerfCounterConsts.MEM_ORDER_NUM_COUNTERS + BoomPerfCounterConsts.DATA_DEP_NUM_COUNTERS + i) := l2ctrs(i)
        }
        // l2_demand_miss_pending: appended at end (index 109)
        dump.io.counters(BoomPerfCounterConsts.NUM_COUNTERS - 1) := l2ctrs(BoomPerfCounterConsts.L2_INLINE_NUM_COUNTERS)
      }
    }
  }

  // Generate a descriptive string
  val frontendStr = outer.frontend.module.toString
  val coreStr = core.toString
  val boomTileStr =
    (BoomCoreStringPrefix(s"======BOOM Tile ${outer.tileId} Params======") + "\n"
    + frontendStr
    + coreStr + "\n")

  override def toString: String = boomTileStr

  print(boomTileStr)
}
