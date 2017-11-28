// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package boom

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.coreplex._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

case class BoomTileParams(
    core: BoomCoreParams = BoomCoreParams(),
    icache: Option[ICacheParams] = Some(ICacheParams()),
    dcache: Option[DCacheParams] = Some(DCacheParams()),
    rocc: Seq[RoCCParams] = Nil,
    btb: Option[BTBParams] = Some(BTBParams()),
    dataScratchpadBytes: Int = 0,
    trace: Boolean = false,
    hcfOnUncorrectable: Boolean = false,
    name: Option[String] = Some("tile"),
    hartid: Int = 0,
    blockerCtrlAddr: Option[BigInt] = None,
    boundaryBuffers: Boolean = false // if synthesized with hierarchical PnR, cut feed-throughs?
    ) extends TileParams {
  require(icache.isDefined)
  require(dcache.isDefined)
}


class BoomTile(val boomParams: BoomTileParams)(implicit p: Parameters) 
   extends freechips.rocketchip.tile.HartedTile(boomParams, boomParams.hartid)(p)
   with HasExternalInterrupts
   with HasLazyRoCC  // implies CanHaveSharedFPU with CanHavePTW with HasHellaCache
   with CanHaveBoomScratchpad { // implies CanHavePTW with HasHellaCache with HasICacheFrontend

  nDCachePorts += 1 // core TODO dcachePorts += () => module.core.io.dmem ??

  private def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))
  private def ofStr(x: String) = Seq(ResourceString(x))
  private def ofRef(x: Device) = Seq(ResourceReference(x.label))

  val cpuDevice = new Device {
    def describe(resources: ResourceBindings): Description = {
      val block =  p(CacheBlockBytes)
      val m = if (boomParams.core.mulDiv.nonEmpty) "m" else ""
      val a = if (boomParams.core.useAtomics) "a" else ""
      val f = if (boomParams.core.fpu.nonEmpty) "f" else ""
      val d = if (boomParams.core.fpu.nonEmpty && p(XLen) > 32) "d" else ""
      val c = if (boomParams.core.useCompressed) "c" else ""
      val isa = s"rv${p(XLen)}i$m$a$f$d$c"

      val dcache = boomParams.dcache.filter(!_.scratch.isDefined).map(d => Map(
        "d-cache-block-size"   -> ofInt(block),
        "d-cache-sets"         -> ofInt(d.nSets),
        "d-cache-size"         -> ofInt(d.nSets * d.nWays * block))).getOrElse(Map())

      val dtim = scratch.map(d => Map(
        "sifive,dtim"          -> ofRef(d.device))).getOrElse(Map())

      val itim = if (frontend.icache.slaveNode.edges.in.isEmpty) Map() else Map(
        "sifive,itim"          -> ofRef(frontend.icache.device))

      val icache = boomParams.icache.map(i => Map(
        "i-cache-block-size"   -> ofInt(block),
        "i-cache-sets"         -> ofInt(i.nSets),
        "i-cache-size"         -> ofInt(i.nSets * i.nWays * block))).getOrElse(Map())

      val dtlb = boomParams.dcache.filter(_ => boomParams.core.useVM).map(d => Map(
        "d-tlb-size"           -> ofInt(d.nTLBEntries),
        "d-tlb-sets"           -> ofInt(1))).getOrElse(Map())

      val itlb = boomParams.icache.filter(_ => boomParams.core.useVM).map(i => Map(
        "i-tlb-size"           -> ofInt(i.nTLBEntries),
        "i-tlb-sets"           -> ofInt(1))).getOrElse(Map())

      val mmu = if (!boomParams.core.useVM) Map() else Map(
        "tlb-split" -> Nil,
        "mmu-type"  -> ofStr(p(PgLevels) match {
          case 2 => "riscv,sv32"
          case 3 => "riscv,sv39"
          case 4 => "riscv,sv48"
      }))

      // Find all the caches
      val outer = masterNode.edges.out
        .flatMap(_.manager.managers)
        .filter(_.supportsAcquireB)
        .flatMap(_.resources.headOption)
        .map(_.owner.label)
        .distinct
      val nextlevel: Option[(String, Seq[ResourceValue])] =
        if (outer.isEmpty) None else
        Some("next-level-cache" -> outer.map(l => ResourceReference(l)).toList)

      Description(s"cpus/cpu@${hartid}", Map(
        "reg"                  -> resources("reg").map(_.value),
        "device_type"          -> ofStr("cpu"),
        "compatible"           -> Seq(ResourceString("sifive,rocket0"), ResourceString("riscv")),
        "status"               -> ofStr("okay"),
        "clock-frequency"      -> Seq(ResourceInt(boomParams.core.bootFreqHz)),
        "riscv,isa"            -> ofStr(isa))
        ++ dcache ++ icache ++ nextlevel ++ mmu ++ itlb ++ dtlb ++ dtim ++itim)
    }
  }
  val intcDevice = new Device {
    def describe(resources: ResourceBindings): Description = {
      Description(s"cpus/cpu@${hartid}/interrupt-controller", Map(
        "compatible"           -> ofStr("riscv,cpu-intc"),
        "interrupt-controller" -> Nil,
        "#interrupt-cells"     -> ofInt(1)))
    }
  }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceInt(BigInt(hartid)))
    Resource(intcDevice, "reg").bind(ResourceInt(BigInt(hartid)))

    intNode.edges.in.flatMap(_.source.sources).map { case s =>
      for (i <- s.range.start until s.range.end) {
       csrIntMap.lift(i).foreach { j =>
          s.resources.foreach { r =>
            r.bind(intcDevice, ResourceInt(j))
          }
        }
      }
    }
  }

  override lazy val module = new BoomTileModule(this)
}

class BoomTileBundle(outer: BoomTile) extends BaseTileBundle(outer)
    with HasExternalInterruptsBundle
    with CanHaveBoomScratchpadBundle
    with CanHaltAndCatchFire {
  val halt_and_catch_fire = outer.boomParams.hcfOnUncorrectable.option(Bool(OUTPUT))
}

class BoomTileModule(outer: BoomTile) extends BaseTileModule(outer, () => new BoomTileBundle(outer))
    with HasExternalInterruptsModule
    with HasLazyRoCCModule
    with CanHaveBoomScratchpadModule {

  val core = Module(new BoomCore()(outer.p, outer.dcache.module.edge))
  val uncorrectable = RegInit(Bool(false))

  decodeCoreInterrupts(core.io.interrupts) // Decode the interrupt vector
  core.io.hartid := io.hartid // Pass through the hartid
  io.trace.foreach { _ := core.io.trace }
  io.halt_and_catch_fire.foreach { _ := uncorrectable }
  outer.frontend.module.io.cpu <> core.io.imem
  outer.frontend.module.io.reset_vector := io.reset_vector
  outer.frontend.module.io.hartid := io.hartid
  outer.dcache.module.io.hartid := io.hartid
  dcachePorts += core.io.dmem // TODO outer.dcachePorts += () => module.core.io.dmem ??
  fpuOpt foreach { fpu => core.io.fpu <> fpu.io }
  core.io.ptw <> ptw.io.dpath
  roccCore.cmd <> core.io.rocc.cmd
  roccCore.exception := core.io.rocc.exception
  core.io.rocc.resp <> roccCore.resp
  core.io.rocc.busy := roccCore.busy
  core.io.rocc.interrupt := roccCore.interrupt

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
}

class BoomTileWrapperBundle[+L <: BoomTileWrapper](_outer: L) extends BaseTileBundle(_outer)
    with CanHaltAndCatchFire {
  val halt_and_catch_fire = _outer.boom.module.io.halt_and_catch_fire.map(_.cloneType)
}

class BoomTileWrapper(
    params: BoomTileParams,
    val crossing: CoreplexClockCrossing)
    (implicit p: Parameters) extends BaseTile(params) with HasCrossing {

  val boom = LazyModule(new BoomTile(params))

  // The buffers needed to cut feed-through paths are microarchitecture specific, so belong here
  val masterBuffer = LazyModule(new TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1)))
  val masterNode: TLOutwardNode = crossing match {
    case _: AsynchronousCrossing => boom.masterNode
    case SynchronousCrossing(b) =>
      require (!params.boundaryBuffers || (b.depth >= 1 && !b.flow && !b.pipe), "Buffer misconfiguration creates feed-through paths")
      boom.masterNode
    case RationalCrossing(dir) =>
      require (dir != SlowToFast, "Misconfiguration? Core slower than fabric")
      if (params.boundaryBuffers) {
        masterBuffer.node :=* boom.masterNode
      } else {
        boom.masterNode
      }
  }

  val slaveBuffer  = LazyModule(new TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none))
  val slaveNode: TLInwardNode = crossing match {
    case _: SynchronousCrossing  => boom.slaveNode // requirement already checked
    case _: AsynchronousCrossing => boom.slaveNode
    case _: RationalCrossing =>
      if (params.boundaryBuffers) {
        DisableMonitors { implicit p => boom.slaveNode :*= slaveBuffer.node }
      } else {
        boom.slaveNode
      }
  }

  val intXbar = LazyModule(new IntXbar)
  boom.intNode := intXbar.intnode

  override lazy val module = new BaseTileModule(this, () => new BoomTileWrapperBundle(this)) {
    // signals that do not change based on crossing type:
    boom.module.io.hartid := io.hartid
    boom.module.io.reset_vector := io.reset_vector
    io.trace.foreach { _ := boom.module.io.trace.get }
    io.halt_and_catch_fire.foreach { _ := boom.module.io.halt_and_catch_fire.get }
  }
}
