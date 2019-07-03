//******************************************************************************
// Copyright (c) 2018 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

package boom.lsu

import scala.collection.mutable.ListBuffer

import chisel3._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.rocket.{DCache, HellaCache, HellaCacheArbiter, HellaCacheIO, NonBlockingDCache, PTW}
import freechips.rocketchip.tile.{BaseTile, HasTileParameters}
import freechips.rocketchip.tilelink.TLIdentityNode


trait HasBoomLSU { this: BaseTile =>
  val module: HasBoomLSUModule
  implicit val p: Parameters
  var nHellaCachePorts = 0
  lazy val dcache: BoomNonBlockingDCache = LazyModule(new BoomNonBlockingDCache(hartId))

  val dCacheTap = TLIdentityNode()
  tlMasterXbar.node := dCacheTap := dcache.node
}

trait HasBoomLSUModule
{
  val outer: HasBoomLSU with HasTileParameters
  implicit val p: Parameters
  val hellaCachePorts = ListBuffer[HellaCacheIO]()
  val hellaCacheArb = Module(new HellaCacheArbiter(outer.nHellaCachePorts)(outer.p))
  // outer.dcache.module.io.cpu <> dcacheArb.io.mem
  require(outer.nHellaCachePorts == 1, "Only support PTW for now")
  val lsu = Module(new LSU()(p, outer.dcache.module.edge))
  lsu.io.hellacache <> hellaCacheArb.io.mem
  outer.dcache.module.io.lsu <> lsu.io.dmem
}


// /**
//  * Top level mixin to construct a tile with a BOOM HellaCache.
//  */
// trait HasBoomHellaCache { this: BaseTile =>
//   val module: HasBoomHellaCacheModule
//   implicit val p: Parameters
//   var nDCachePorts = 0
//   lazy val dcache: HellaCache = LazyModule(
//     if (tileParams.dcache.get.nMSHRs == 0)
//     {
//       new DCache(hartId, crossing)
//     }
//     else
//     {
//       new NonBlockingDCache(hartId)
//     })

//   //tlMasterXbar.node := dcache.node
//   val dCacheTap = TLIdentityNode()
//   tlMasterXbar.node := dCacheTap := dcache.node
// }

// /**
//  * Mixin to construct a tile with a BOOM HellaCache.
//  */
// trait HasBoomHellaCacheModule
// {
//   val outer: HasBoomHellaCache with HasTileParameters
//   implicit val p: Parameters
//   val dcachePorts = ListBuffer[HellaCacheIO]()
//   val dcacheArb = Module(new HellaCacheArbiter(outer.nDCachePorts)(outer.p))
//   outer.dcache.module.io.cpu <> dcacheArb.io.mem
// }

/**
 * Top level mixin to construct a tile with a BOOM PTW.
 */
trait CanHaveBoomPTW extends HasTileParameters with HasBoomLSU { this: BaseTile =>
  val module: CanHaveBoomPTWModule
  var nPTWPorts = 2
  nHellaCachePorts += (if (usingPTW) 1 else 0)
}

/**
 * Mixin to construct a tile with a BOOM PTW.
 */
trait CanHaveBoomPTWModule extends HasBoomLSUModule
{
  val outer: CanHaveBoomPTW
  val ptwPorts = ListBuffer.empty[freechips.rocketchip.rocket.TLBPTWIO]
  for (ptwio <- lsu.io.ptw) {
    ptwPorts += ptwio
  }
  val ptw = Module(new PTW(outer.nPTWPorts)(outer.dcache.node.edges.out(0), outer.p))
  ptw.io <> DontCare // Is overridden below if PTW is connected
  if (outer.usingPTW)
  {
    hellaCachePorts += ptw.io.mem
  }
}

/**
 * Bundle to monitor cache data writebacks/releases for memory ordering.
 */
class ReleaseInfo(implicit p: Parameters) extends boom.common.BoomBundle()(p)
{
   val address = UInt(corePAddrBits.W)
}
