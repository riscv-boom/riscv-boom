//******************************************************************************
// Copyright (c) 2018 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

package boom.system

import chisel3._
import chisel3.internal.sourceinfo.{SourceInfo}

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.devices.debug.{HasPeripheryDebug, HasPeripheryDebugModuleImp}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.tile._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.amba.axi4._

case object BoomTilesKey extends Field[Seq[boom.common.BoomTileParams]](Nil)

trait HasBoomTiles extends HasTiles
  with CanHavePeripheryPLIC
  with CanHavePeripheryCLINT
  with HasPeripheryDebug
{ this: BaseSubsystem =>

  val module: HasBoomTilesModuleImp

  protected val boomTileParams = p(BoomTilesKey)
  private val crossings = perTileOrGlobalSetting(p(RocketCrossingKey), boomTileParams.size)

  // Make a tile and wire its nodes into the system,
  //   according to the specified type of clock crossing.
  //   Note that we also inject new nodes into the tile itself,
  //   also based on the crossing type.
  val boomTiles = boomTileParams.zip(crossings).map { case (tp, crossing) =>
    val boomCore = LazyModule(
      new boom.common.BoomTile(tp, crossing, PriorityMuxHartIdFromSeq(boomTileParams))).suggestName(tp.name)

    connectMasterPortsToSBus(boomCore, crossing)
    connectSlavePortsToCBus(boomCore, crossing)
    connectInterrupts(boomCore, Some(debug), clintOpt, plicOpt)

    boomCore
  }

  boomTiles.map {
    r =>
      def treeNode: RocketTileLogicalTreeNode = new RocketTileLogicalTreeNode(r.rocketLogicalTree.getOMInterruptTargets)
      LogicalModuleTree.add(logicalTreeNode, r.rocketLogicalTree)
  }

  def coreMonitorBundles = (boomTiles map { t =>
    t.module.core.coreMonitorBundle
  }).toList

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
  override lazy val module = new BoomSubsystemModuleImp(this)
  def getOMInterruptDevice(resourceBindingsMap: ResourceBindingsMap): Seq[OMInterrupt] = Nil
}

class BoomSubsystemModuleImp[+L <: BoomSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
  with HasResetVectorWire
  with HasBoomTilesModuleImp
{
  tile_inputs.zip(outer.hartIdList).foreach { case(wire, i) =>
    wire.hartid := i.U
    wire.reset_vector := global_reset_vector
  }
}

