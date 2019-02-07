//******************************************************************************
// Copyright (c) 2018 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

package boom.system

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.devices.debug.{HasPeripheryDebug, HasPeripheryDebugModuleImp}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.amba.axi4._

case object BoomTilesKey extends Field[Seq[boom.common.BoomTileParams]](Nil)

trait HasBoomTiles extends HasTiles
    with CanHavePeripheryPLIC
    with CanHavePeripheryCLINT
    with HasPeripheryDebug { this: BaseSubsystem =>
  val module: HasBoomTilesModuleImp

  protected val boomTileParams = p(BoomTilesKey)
  private val crossings = perTileOrGlobalSetting(p(RocketCrossingKey), boomTileParams.size)

  // Make a tile and wire its nodes into the system,
  // according to the specified type of clock crossing.
  // Note that we also inject new nodes into the tile itself,
  // also based on the crossing type.
  val boomTiles = boomTileParams.zip(crossings).map { case (tp, crossing) =>
    val boomCore = LazyModule(new boom.common.BoomTile(tp, crossing.crossingType)(augmentedTileParameters(tp)))
      .suggestName(tp.name)

    connectMasterPortsToSBus(boomCore, crossing)
    connectSlavePortsToCBus(boomCore, crossing)
    connectInterrupts(boomCore, Some(debug), clintOpt, plicOpt)

    boomCore
  }
}

trait HasBoomTilesModuleImp extends HasTilesModuleImp
    with HasPeripheryDebugModuleImp
{
  val outer: HasBoomTiles
}

class BoomSubsystem(implicit p: Parameters) extends BaseSubsystem
    with HasBoomTiles
{
  val tiles = boomTiles
  override lazy val module = new BoomSubsystemModule(this)
}

class BoomSubsystemModule[+L <: BoomSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
    with HasBoomTilesModuleImp
{
  tile_inputs.zip(outer.hartIdList).foreach { case(wire, i) =>
    wire.clock := clock
    wire.reset := reset
    wire.hartid := i.U
    wire.reset_vector := global_reset_vector
  }
}

///// Adds a port to the system intended to master an AXI4 DRAM controller that supports a large physical address size

trait CanHaveMisalignedMasterAXI4MemPort { this: BaseSubsystem =>
  val module: CanHaveMisalignedMasterAXI4MemPortModuleImp
  val nMemoryChannels: Int
  private val memPortParamsOpt = p(ExtMem)
  private val portName = "misaligned_axi4"
  private val device = new MemoryDevice

  require(nMemoryChannels == 0 || memPortParamsOpt.isDefined,
    s"Cannot have $nMemoryChannels with no memory port!")

  val memAXI4Node = AXI4SlaveNode(Seq.tabulate(nMemoryChannels) { channel =>
    val params = memPortParamsOpt.get

    AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address       = AddressSet.misaligned(params.base, params.size),
        resources     = device.reg,
        regionType    = RegionType.UNCACHED, // cacheable
        executable    = true,
        supportsWrite = TransferSizes(1, cacheBlockBytes),
        supportsRead  = TransferSizes(1, cacheBlockBytes),
        interleavedId = Some(0))), // slave does not interleave read responses
      beatBytes = params.beatBytes)
  })

  memPortParamsOpt.foreach { params =>
    memBuses.map { m =>
       memAXI4Node := m.toDRAMController(Some(portName)) {
        (AXI4UserYanker() := AXI4IdIndexer(params.idBits) := TLToAXI4())
      }
    }
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait CanHaveMisalignedMasterAXI4MemPortModuleImp extends LazyModuleImp
{
  val outer: CanHaveMisalignedMasterAXI4MemPort

  val mem_axi4 = IO(HeterogeneousBag.fromNode(outer.memAXI4Node.in))
  (mem_axi4 zip outer.memAXI4Node.in).foreach { case (io, (bundle, _)) => io <> bundle }

  def connectSimAXIMem() {
    (mem_axi4 zip outer.memAXI4Node.in).foreach { case (io, (_, edge)) =>
      // setting the max size for simulated memory to be 256MB
      val mem = LazyModule(new SimAXIMem(edge, size = 0x10000000))
      Module(mem.module).io.axi4.head <> io
    }
  }
}
