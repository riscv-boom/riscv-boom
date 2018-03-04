// See LICENSE.SiFive for license details.

package boom.system

import freechips.rocketchip.subsystem.RocketTilesKey
import freechips.rocketchip.tile.XLen
import freechips.rocketchip.util.GeneratorApp
import freechips.rocketchip.system.{TestGeneration, RegressionTestSuite}

import scala.collection.mutable.LinkedHashSet

// Any BOOM-specific tests can go here to override default rocket-chip behavior.
object BoomTestSuites {
  import freechips.rocketchip.system.DefaultTestSuites._
  
  // We do not currently support breakpoints, so override the rv64mi and its descendents. 
  val rv64miNames = rv32miNames
  val rv64mi = new freechips.rocketchip.system.AssemblyTestSuite("rv64mi", rv64miNames)(_)
  val rv64i = List(rv64ui, rv64si, rv64mi)
  val rv64pi = List(rv64ui, rv64mi)
}

/** A Generator for platforms containing Rocket Subsystems */
object Generator extends GeneratorApp {

  val rv64RegrTestNames = LinkedHashSet(
        "rv64ud-v-fcvt",
        "rv64ud-p-fdiv",
        "rv64ud-v-fadd",
        "rv64uf-v-fadd",
        "rv64um-v-mul",
        // "rv64mi-p-breakpoint", TODO: breakpoints not implemented yet
        // "rv64uc-v-rvc", TODO: RVC not implemented yet
        "rv64ud-v-structural",
        "rv64si-p-wfi",
        "rv64um-v-divw",
        "rv64ua-v-lrsc",
        "rv64ui-v-fence_i",
        "rv64ud-v-fcvt_w",
        "rv64uf-v-fmin",
        "rv64ui-v-sb",
        "rv64ua-v-amomax_d",
        "rv64ud-v-move",
        "rv64ud-v-fclass",
        "rv64ua-v-amoand_d",
        "rv64ua-v-amoxor_d",
        "rv64si-p-sbreak",
        "rv64ud-v-fmadd",
        "rv64uf-v-ldst",
        "rv64um-v-mulh",
        "rv64si-p-dirty")

  val rv32RegrTestNames = LinkedHashSet(
      "rv32mi-p-ma_addr",
      "rv32mi-p-csr",
      "rv32ui-p-sh",
      "rv32ui-p-lh",
      "rv32uc-p-rvc",
      "rv32mi-p-sbreak",
      "rv32ui-p-sll")

  override def addTestSuites {
    import freechips.rocketchip.system.DefaultTestSuites._
    val xlen = params(XLen)
    // TODO: for now only generate tests for the first core in the first subsystem
    val tileParams = params(BoomTilesKey).head
    val coreParams = tileParams.core
    val vm = coreParams.useVM
    val env = if (vm) List("p","v") else List("p")
    coreParams.fpu foreach { case cfg =>
      if (xlen == 32) {
        TestGeneration.addSuites(env.map(rv32uf))
      } else {
        TestGeneration.addSuite(rv32udBenchmarks)
        TestGeneration.addSuites(env.map(rv64uf))
        TestGeneration.addSuites(env.map(rv64ud))
        if (cfg.divSqrt) {
          TestGeneration.addSuites(env.map(rv64uf))
          TestGeneration.addSuites(env.map(rv64ud))
        }
      }
    }
    if (coreParams.useAtomics) {
      if (tileParams.dcache.flatMap(_.scratch).isEmpty)
        TestGeneration.addSuites(env.map(if (xlen == 64) rv64ua else rv32ua))
      else
        TestGeneration.addSuites(env.map(if (xlen == 64) rv64uaSansLRSC else rv32uaSansLRSC))
    }
    if (coreParams.useCompressed) TestGeneration.addSuites(env.map(if (xlen == 64) rv64uc else rv32uc))


    // Include our BOOM-specific overrides.
    val (rvi, rvu) =
      if (xlen == 64) ((if (vm) BoomTestSuites.rv64i else BoomTestSuites.rv64pi), rv64u)
      else            ((if (vm) rv32i else rv32pi), rv32u)

    TestGeneration.addSuites(rvi.map(_("p")))
    TestGeneration.addSuites((if (vm) List("v") else List()).flatMap(env => rvu.map(_(env))))
    TestGeneration.addSuite(benchmarks)
    TestGeneration.addSuite(new RegressionTestSuite(if (xlen == 64) rv64RegrTestNames else rv32RegrTestNames))
  }

  val longName = names.topModuleProject + "." + names.configs
  generateFirrtl
  generateAnno
  generateTestSuiteMakefrags
  generateROMs
  generateArtefacts
}
