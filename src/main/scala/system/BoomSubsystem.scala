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

import boom.common.{BoomTile}

case object BoomTilesKey extends Field[Seq[boom.common.BoomTileParams]](Nil)
case object BoomCrossingKey extends Field[Seq[RocketCrossingParams]](List(RocketCrossingParams()))

trait HasBoomTiles extends HasTiles
  with CanHavePeripheryPLIC
  with CanHavePeripheryCLINT
  with HasPeripheryDebug
{ this: BaseSubsystem =>

  val module: HasBoomTilesModuleImp

  protected val boomTileParams = p(BoomTilesKey)
  // crossing can either be per tile or global (aka only 1 crossing specified)
  private val crossings = perTileOrGlobalSetting(p(BoomCrossingKey), boomTileParams.size)

  // Make a tile and wire its nodes into the system,
  //   according to the specified type of clock crossing.
  //   Note that we also inject new nodes into the tile itself,
  //   also based on the crossing type.
  val boomTiles = boomTileParams.zip(crossings).map { case (tp, crossing) =>
    val boom = LazyModule(new BoomTile(tp, crossing, PriorityMuxHartIdFromSeq(boomTileParams)))

    connectMasterPortsToSBus(boom, crossing)
    connectSlavePortsToCBus(boom, crossing)

    def treeNode: RocketTileLogicalTreeNode = new RocketTileLogicalTreeNode(boom.rocketLogicalTree.getOMInterruptTargets)
    LogicalModuleTree.add(logicalTreeNode, boom.rocketLogicalTree)

    boom
  }

  // connect interrupts based on the order of harts
  boomTiles.sortWith(_.tileParams.hartId < _.tileParams.hartId).map {
    b => connectInterrupts(b, Some(debug), clintOpt, plicOpt)
  }

  def coreMonitorBundles = (boomTiles map { t => t.module.core.coreMonitorBundle }).toList
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

