package boom.lsu

import Chisel._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.rocket.{DCache, HellaCache, HellaCacheArbiter, HellaCacheIO, NonBlockingDCache, PTW}
import freechips.rocketchip.subsystem.RocketCrossingKey
import freechips.rocketchip.tile.{BaseTile, HasTileParameters}
import freechips.rocketchip.tilelink.TLIdentityNode
import scala.collection.mutable.ListBuffer


/** Mix-ins for constructing tiles that have a HellaCache */

trait HasBoomHellaCache { this: BaseTile =>
  val module: HasBoomHellaCacheModule
  implicit val p: Parameters
  def findScratchpadFromICache: Option[AddressSet]
  var nDCachePorts = 0
  val dcache: HellaCache = LazyModule(
    if(tileParams.dcache.get.nMSHRs == 0) {
      new DCache(hartId, findScratchpadFromICache _, p(RocketCrossingKey).head.knownRatio)
    } else { new NonBlockingDCache(hartId) })

  //tlMasterXbar.node := dcache.node
  val dCacheTap = TLIdentityNode()
  tlMasterXbar.node := dCacheTap := dcache.node
}


trait HasBoomHellaCacheModule {
  val outer: HasBoomHellaCache
  val dcachePorts = ListBuffer[HellaCacheIO]()
  val dcacheArb = Module(new HellaCacheArbiter(outer.nDCachePorts)(outer.p))
  outer.dcache.module.io.cpu <> dcacheArb.io.mem
}


/** Mix-ins for constructing tiles that might have a PTW */
trait CanHaveBoomPTW extends HasTileParameters with HasBoomHellaCache { this: BaseTile =>
  val module: CanHaveBoomPTWModule
  var nPTWPorts = 1
  nDCachePorts += (if (usingPTW) 1 else 0)
}


trait CanHaveBoomPTWModule extends HasBoomHellaCacheModule {
  val outer: CanHaveBoomPTW
  val ptwPorts = ListBuffer(outer.dcache.module.io.ptw)
  val ptw = Module(new PTW(outer.nPTWPorts)(outer.dcache.node.edges.out(0), outer.p))
  if (outer.usingPTW)
    dcachePorts += ptw.io.mem
}


/** Monitor cache data writebacks/releases for memory ordering. */

class ReleaseInfo(implicit p: Parameters) extends boom.common.BoomBundle()(p)
{
   val address = UInt(width = corePAddrBits)
}

