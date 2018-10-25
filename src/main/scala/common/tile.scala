// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package boom.common

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import boom.exu._
import boom.ifu._
import boom.lsu._

case class BoomTileParams(
    core: BoomCoreParams = BoomCoreParams(),
    icache: Option[ICacheParams] = Some(ICacheParams()),
    dcache: Option[DCacheParams] = Some(DCacheParams()),
    btb: Option[BTBParams] = Some(BTBParams()),
    dataScratchpadBytes: Int = 0,
    trace: Boolean = false,
    hcfOnUncorrectable: Boolean = false,
    name: Option[String] = Some("tile"),
    hartId: Int = 0,
    blockerCtrlAddr: Option[BigInt] = None,
    boundaryBuffers: Boolean = false // if synthesized with hierarchical PnR, cut feed-throughs?
    ) extends TileParams {
  require(icache.isDefined)
  require(dcache.isDefined)
}

class BoomTile(
    val boomParams: BoomTileParams,
    crossing: ClockCrossingType)
  (implicit p: Parameters) extends BaseTile(boomParams, crossing)(p)
    with HasExternalInterrupts
    //with HasLazyRoCC  // implies CanHaveSharedFPU with CanHavePTW with HasHellaCache
    with CanHaveBoomPTW
    with HasBoomHellaCache
    with HasBoomICacheFrontend {

  val intOutwardNode = IntIdentityNode()
  val slaveNode = TLIdentityNode()
  val masterNode = TLIdentityNode()

  val dtim_adapter = tileParams.dcache.flatMap { d => d.scratch.map(s =>
    LazyModule(new ScratchpadSlavePort(AddressSet(s, d.dataScratchpadBytes-1), xBytes, tileParams.core.useAtomics && !tileParams.core.useAtomicsOnlyForIO)))
  }
  dtim_adapter.foreach(lm => connectTLSlave(lm.node, xBytes))

  val bus_error_unit = tileParams.core.tileControlAddr map { a =>
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

  def findScratchpadFromICache: Option[AddressSet] = dtim_adapter.map { s =>
    val finalNode = frontend.masterNode.edges.out.head.manager.managers.find(_.nodePath.last == s.node)
    require (finalNode.isDefined, "Could not find the scratch pad; not reachable via icache?")
    require (finalNode.get.address.size == 1, "Scratchpad address space was fragmented!")
    finalNode.get.address(0)
  }

  nDCachePorts += 1 /*core */ + (dtim_adapter.isDefined).toInt

  val dtimProperty = dtim_adapter.map(d => Map(
    "sifive,dtim" -> d.device.asProperty)).getOrElse(Nil)

  val itimProperty = tileParams.icache.flatMap(_.itimAddr.map(i => Map(
    "sifive,itim" -> frontend.icache.device.asProperty))).getOrElse(Nil)

  val cpuDevice = new SimpleDevice("cpu", Seq("ucb-bar,boom0", "riscv")) {
    override def parent = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ cpuProperties ++ nextLevelCacheProperty ++ tileProperties ++ dtimProperty ++ itimProperty)
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

class BoomTileModuleImp(outer: BoomTile) extends BaseTileModuleImp(outer)
    //with HasLazyRoCCModule[BoomTile]
    with CanHaveBoomPTWModule
    with HasBoomHellaCacheModule
    with HasBoomICacheFrontendModule {
  Annotated.params(this, outer.boomParams)

  val core = Module(new BoomCore()(outer.p, outer.dcache.module.edge))

  //val fpuOpt = outer.tileParams.core.fpu.map(params => Module(new FPU(params)(outer.p))) RocketFpu - not needed in boom

  // Observe the Tilelink Channel C traffic leaving the L1D (writeback/releases).
  val tl_c = outer.dCacheTap.out(0)._1.c
  core.io.release.valid := tl_c.fire()
  core.io.release.bits.address := tl_c.bits.address

  val uncorrectable = RegInit(Bool(false))
  val halt_and_catch_fire = outer.boomParams.hcfOnUncorrectable.option(IO(Bool(OUTPUT)))

   outer.dtim_adapter.foreach { lm => {
      val dc_port = Wire(new SecureHellaCacheIO()(outer.p))
      dc_port <> lm.module.io.dmem
      dcachePorts += dc_port
   }
   }

  outer.bus_error_unit.foreach { lm =>
    lm.module.io.errors.dcache := outer.dcache.module.io.errors
    lm.module.io.errors.icache := outer.frontend.module.io.errors
  }

  outer.decodeCoreInterrupts(core.io.interrupts) // Decode the interrupt vector
  outer.bus_error_unit.foreach { beu => core.io.interrupts.buserror.get := beu.module.io.interrupt }
  core.io.hartid := constants.hartid // Pass through the hartid
  trace.foreach { _ := core.io.trace }
  halt_and_catch_fire.foreach { _ := uncorrectable }
  outer.frontend.module.io.cpu <> core.io.ifu
  outer.frontend.module.io.reset_vector := constants.reset_vector
  outer.frontend.module.io.hartid := constants.hartid
  outer.dcache.module.io.hartid := constants.hartid
  dcachePorts += core.io.dmem // TODO outer.dcachePorts += () => module.core.io.dmem ??
  //fpuOpt foreach { fpu => core.io.fpu <> fpu.io } RocketFpu - not needed in boom
  core.io.ptw <> ptw.io.dpath
  //roccCore.cmd <> core.io.rocc.cmd
  //roccCore.exception := core.io.rocc.exception
  //core.io.rocc.resp <> roccCore.resp
  //core.io.rocc.busy := roccCore.busy
  //core.io.rocc.interrupt := roccCore.interrupt

  when(!uncorrectable) { uncorrectable :=
    List(outer.frontend.module.io.errors, outer.dcache.module.io.errors)
      .flatMap { e => e.uncorrectable.map(_.valid) }
      .reduceOption(_||_)
      .getOrElse(false.B)
  }

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
  ElaborationArtefacts.add(
    """core.config""",
    frontendStr + core.toString + "\n"
  )
  print(outer.frontend.module.toString + core.toString + "\n")
}
