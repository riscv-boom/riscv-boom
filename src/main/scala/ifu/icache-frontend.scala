// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package boom

import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

/** Mix-ins for constructing tiles that have an ICache-based pipeline frontend */
trait HasBoomICacheFrontend extends HasICacheFrontend { this: BaseTile =>
  val module: HasBoomICacheFrontendModule
  nPTWPorts += 1 // boom -- needs an extra PTW port for its LSU.
}

trait HasBoomICacheFrontendModule extends HasICacheFrontendModule {
  val outer: HasBoomICacheFrontend
}
