// See LICENSE.SiFive for license details.

package boom

import freechips.rocketchip.rocket._

/** Mix-ins for constructing tiles that have optional scratchpads */
trait CanHaveBoomScratchpad extends CanHaveScratchpad with HasBoomICacheFrontend {
  val module: CanHaveBoomScratchpadModule
}

trait CanHaveBoomScratchpadBundle extends CanHaveScratchpadBundle with HasBoomICacheFrontendBundle {
  val outer: CanHaveBoomScratchpad
}

trait CanHaveBoomScratchpadModule extends CanHaveScratchpadModule with HasBoomICacheFrontendModule {
  val outer: CanHaveBoomScratchpad
  val io: CanHaveBoomScratchpadBundle
}
