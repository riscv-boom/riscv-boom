// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package boom

import freechips.rocketchip.rocket._

/** Mix-ins for constructing tiles that have an ICache-based pipeline frontend */
trait HasBoomICacheFrontend extends HasICacheFrontend {
  val module: HasBoomICacheFrontendModule
  nPTWPorts += 1 // boom -- needs an extra PTW port for its LSU.
}

trait HasBoomICacheFrontendBundle extends HasICacheFrontendBundle {
  val outer: HasBoomICacheFrontend
}

trait HasBoomICacheFrontendModule extends HasICacheFrontendModule {
  val outer: HasBoomICacheFrontend
}
