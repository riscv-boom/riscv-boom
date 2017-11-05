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

  protected val tileParams = p(BoomTilesKey)
  private val NumBoomTiles = tileParams.size
  private val crossingParams = p(RocketCrossingKey)
  private val crossings = crossingParams.size match {
    case 1 => List.fill(NumBoomTiles) { crossingParams.head }
    case NumBoomTiles => crossingParams
    case _ => throw new Exception("RocketCrossingKey.size must == 1 or == BoomTilesKey.size")
  }
  private val crossingTuples = localIntNodes.zip(tileParams).zip(crossings)

  // Make a wrapper for each tile that will wire it to coreplex devices and crossbars,
  // according to the specified type of clock crossing.
  val tiles: Seq[BaseTile] = crossingTuples.map { case ((lip, tp), crossing) =>
    // For legacy reasons, it is convenient to store some state
    // in the global Parameters about the specific tile being built now
    val wrapper = LazyModule(new boom.BoomTileWrapper(
      params = tp,
      crossing = crossing.crossingType
      )(p.alterPartial {
        case TileKey => tp
        case BuildRoCC => tp.rocc
        case SharedMemoryTLEdge => sharedMemoryTLEdge
        case RocketCrossingKey => List(crossing)
      })
    ).suggestName(tp.name)

    // Connect the master ports of the tile to the system bus
    sbus.fromTile(tp.name) { implicit p => crossing.master.adapt(this)(wrapper.crossTLOut :=* wrapper.masterNode) }

    // Connect the slave ports of the tile to the periphery bus
    pbus.toTile(tp.name) { implicit p => crossing.slave.adapt(this)(wrapper.slaveNode :*= wrapper.crossTLIn) }

    // Handle all the different types of interrupts crossing to or from the tile:
    // 1. Debug interrupt is definitely asynchronous in all cases.
    // 2. The CLINT and PLIC output interrupts are synchronous to the periphery clock,
    //    so might need to be synchronized depending on the Tile's crossing type.
    // 3. Local Interrupts are required to already be synchronous to the tile clock.
    // 4. Interrupts coming out of the tile are sent to the PLIC,
    //    so might need to be synchronized depending on the Tile's crossing type.
    // NOTE: The order of calls to := matters! They must match how interrupts
    //       are decoded from rocket.intNode inside the tile.

    wrapper.intXbar.intnode := wrapper { IntSyncCrossingSink(3) } := debug.intnode // 1. always async crossign

    // 2. clint+plic conditionak crossing
    val periphIntNode = SourceCardinality { implicit p => wrapper.intXbar.intnode :=? wrapper.crossIntIn }
    periphIntNode := clint.intnode                   // msip+mtip
    periphIntNode := plic.intnode                    // meip
    if (tp.core.useVM) periphIntNode := plic.intnode // seip

    lip.foreach { wrapper.intXbar.intnode := _ } // 3. lip never crosses

    // From core to PLIC
    wrapper.boom.intOutputNode.foreach { i =>              // 4. conditional crossing
      FlipRendering { implicit p => SourceCardinality { implicit p =>
        plic.intnode :=? wrapper.crossIntOut :=? i
      } }
    }

    wrapper
  }
}

trait HasBoomTilesModuleImp extends HasTilesModuleImp
    with HasPeripheryDebugModuleImp {
  val outer: HasBoomTiles
}

class BoomCoreplex(implicit p: Parameters) extends BaseCoreplex
    with HasBoomTiles {
  override lazy val module = new BoomCoreplexModule(this)
}

class BoomCoreplexModule[+L <: BoomCoreplex](_outer: L) extends BaseCoreplexModule(_outer)
    with HasBoomTilesModuleImp
