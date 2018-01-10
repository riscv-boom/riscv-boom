// See LICENSE.SiFive for license details.

package boom.system

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.devices.debug.{HasPeripheryDebug, HasPeripheryDebugModuleImp}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.coreplex._


case object BoomTilesKey extends Field[Seq[boom.BoomTileParams]](Nil)

trait HasBoomTiles extends HasTiles
    with HasPeripheryBus
    with HasPeripheryPLIC
    with HasPeripheryClint
    with HasPeripheryDebug {
  val module: HasBoomTilesModuleImp

  protected val boomTileParams = p(BoomTilesKey)
  private val NumBoomTiles = boomTileParams.size
  private val crossingParams = p(RocketCrossingKey)
  private val crossings = crossingParams.size match {
    case 1 => List.fill(NumBoomTiles) { crossingParams.head }
    case NumBoomTiles => crossingParams
    case _ => throw new Exception("RocketCrossingKey.size must == 1 or == BoomTilesKey.size")
  }
  private val crossingTuples = boomTileParams.zip(crossings)

  // Make a tile and wire its nodes into the system,
  // according to the specified type of clock crossing.
  // Note that we also inject new nodes into the tile itself,
  // also based on the crossing type.
  val boomTiles = crossingTuples.map { case (tp, crossing) =>
    // For legacy reasons, it is convenient to store some state
    // in the global Parameters about the specific tile being built now
    val boomCore = LazyModule(new boom.BoomTile(tp, crossing.crossingType)(p.alterPartial {
        case TileKey => tp
        case BuildRoCC => tp.rocc
        case SharedMemoryTLEdge => sharedMemoryTLEdge
      })
    ).suggestName(tp.name)

    // Connect the master ports of the tile to the system bus

    def tileMasterBuffering: TLOutwardNode = boomCore {
      // The buffers needed to cut feed-through paths are microarchitecture specific, so belong here
      val masterBuffer = LazyModule(new TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1)))
      crossing.crossingType match {
        case _: AsynchronousCrossing => boomCore.masterNode
        case SynchronousCrossing(b) =>
          require (!tp.boundaryBuffers || (b.depth >= 1 && !b.flow && !b.pipe), "Buffer misconfiguration creates feed-through paths")
          boomCore.masterNode
        case RationalCrossing(dir) =>
          require (dir != SlowToFast, "Misconfiguration? Core slower than fabric")
          if (tp.boundaryBuffers) {
            masterBuffer.node :=* boomCore.masterNode
          } else {
            boomCore.masterNode
          }
      }
    }

    sbus.fromTile(tp.name) { implicit p => crossing.master.adapt(this)(boomCore.crossTLOut :=* tileMasterBuffering) }

    // Connect the slave ports of the tile to the periphery bus

    def tileSlaveBuffering: TLInwardNode = boomCore {
      val slaveBuffer  = LazyModule(new TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none))
      crossing.crossingType match {
        case RationalCrossing(_) if (tp.boundaryBuffers) => boomCore.slaveNode :*= slaveBuffer.node
        case _ => boomCore.slaveNode
      }
    }

    pbus.toTile(tp.name) { implicit p => crossing.slave.adapt(this)( DisableMonitors { implicit p =>
      tileSlaveBuffering :*= boomCore.crossTLIn
    })}

    // Handle all the different types of interrupts crossing to or from the tile:
    // 1. Debug interrupt is definitely asynchronous in all cases.
    // 2. The CLINT and PLIC output interrupts are synchronous to the periphery clock,
    //    so might need to be synchronized depending on the Tile's crossing type.
    // 3. Local Interrupts are required to already be synchronous to the tile clock.
    // 4. Interrupts coming out of the tile are sent to the PLIC,
    //    so might need to be synchronized depending on the Tile's crossing type.
    // NOTE: The order of calls to := matters! They must match how interrupts
    //       are decoded from rocket.intNode inside the tile.

    // 1. always async crossing for debug
    boomCore.intInwardNode := boomCore { IntSyncCrossingSink(3) } := debug.intnode

    // 2. clint+plic conditionally crossing
    val periphIntNode = boomCore.intInwardNode :=* boomCore.crossIntIn
    periphIntNode := clint.intnode                   // msip+mtip
    periphIntNode := plic.intnode                    // meip
    if (tp.core.useVM) periphIntNode := plic.intnode // seip

    // 3. local interrupts  never cross 
    // rocket.intInwardNode is wired up externally     // lip

    // 4. conditional crossing from core to PLIC
    FlipRendering { implicit p =>
      plic.intnode :=* boomCore.crossIntOut :=* boomCore.intOutwardNode
    }

    boomCore
  }
}

trait HasBoomTilesModuleImp extends HasTilesModuleImp
    with HasPeripheryDebugModuleImp {
  val outer: HasBoomTiles
}

class BoomCoreplex(implicit p: Parameters) extends BaseCoreplex
    with HasBoomTiles {
  val tiles = boomTiles
  override lazy val module = new BoomCoreplexModule(this)
}

class BoomCoreplexModule[+L <: BoomCoreplex](_outer: L) extends BaseCoreplexModule(_outer)
    with HasBoomTilesModuleImp {
  tile_inputs.zip(outer.hartIdList).foreach { case(wire, i) =>
    wire.clock := clock
    wire.reset := reset
    wire.hartid := UInt(i)
    wire.reset_vector := global_reset_vector
  }
}
