//******************************************************************************
// Copyright (c) 2017 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

package boom.system

import scala.collection.mutable.{LinkedHashSet}

import freechips.rocketchip.subsystem.{RocketTilesKey}
import freechips.rocketchip.tile.{XLen}
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.util.{GeneratorApp}
import freechips.rocketchip.system.{TestGeneration, RegressionTestSuite}

/**
 * A set of pre-chosen regression tests
 */
object RegressionTestSuites
{
  val rv64RegrTestNames = LinkedHashSet(
    "rv64ud-v-fcvt",
    "rv64ud-p-fdiv",
    "rv64ud-v-fadd",
    "rv64uf-v-fadd",
    "rv64um-v-mul",
    "rv64mi-p-breakpoint",
    "rv64uc-v-rvc",
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
}

/**
 * Helper functions to add BOOM or Rocket tests
 */
object TestSuiteHelper
{
  import freechips.rocketchip.system.DefaultTestSuites._
  import RegressionTestSuites._

  /**
  * Add BOOM tests (asm, bmark, regression)
  */
  def addBoomTestSuites(implicit p: Parameters) = {
    val xlen = p(XLen)
    p(BoomTilesKey).find(_.hartId == 0).map { tileParams =>
      val coreParams = tileParams.core
      val vm = coreParams.useVM
      val env = if (vm) List("p","v") else List("p")
      coreParams.fpu foreach { case cfg =>
        if (xlen == 32) {
          TestGeneration.addSuites(env.map(rv32uf))
          if (cfg.fLen >= 64) {
            TestGeneration.addSuites(env.map(rv32ud))
          }
        } else if (cfg.fLen >= 64) {
          TestGeneration.addSuites(env.map(rv64ud))
          TestGeneration.addSuites(env.map(rv64uf))
          TestGeneration.addSuite(rv32udBenchmarks)
        }
      }
      if (coreParams.useAtomics) {
        if (tileParams.dcache.flatMap(_.scratch).isEmpty) {
          TestGeneration.addSuites(env.map(if (xlen == 64) rv64ua else rv32ua))
        } else {
          TestGeneration.addSuites(env.map(if (xlen == 64) rv64uaSansLRSC else rv32uaSansLRSC))
        }
      }
      if (coreParams.useCompressed) TestGeneration.addSuites(env.map(if (xlen == 64) rv64uc else rv32uc))

      // Include our BOOM-specific overrides.
      val (rvi, rvu) =
        if (xlen == 64) ((if (vm) BoomTestSuites.rv64i else BoomTestSuites.rv64pi), rv64u)
        else            ((if (vm) rv32i else rv32pi), rv32u)

      TestGeneration.addSuites(rvi.map(_("p")))
      TestGeneration.addSuites(rvu.map(_("p")))
      TestGeneration.addSuites((if (vm) List("v") else List()).flatMap(env => rvu.map(_(env))))
      TestGeneration.addSuite(benchmarks)
      rv64RegrTestNames -= "rv64mi-p-breakpoint" // TODO: breakpoints not implemented yet
      TestGeneration.addSuite(new RegressionTestSuite(if (xlen == 64) rv64RegrTestNames else rv32RegrTestNames))
    }
  }

  /**
  * Add Rocket tests (asm, bmark, regression)
  */
  def addRocketTestSuites(implicit p: Parameters) = {
    val xlen = p(XLen)
    p(RocketTilesKey).find(_.hartId == 0).map { tileParams =>
      val coreParams = tileParams.core
      val vm = coreParams.useVM
      val env = if (vm) List("p","v") else List("p")
      coreParams.fpu foreach { case cfg =>
        if (xlen == 32) {
          TestGeneration.addSuites(env.map(rv32uf))
          if (cfg.fLen >= 64)
            TestGeneration.addSuites(env.map(rv32ud))
        } else {
          TestGeneration.addSuite(rv32udBenchmarks)
          TestGeneration.addSuites(env.map(rv64uf))
          if (cfg.fLen >= 64)
            TestGeneration.addSuites(env.map(rv64ud))
        }
      }
      if (coreParams.useAtomics) {
        if (tileParams.dcache.flatMap(_.scratch).isEmpty)
          TestGeneration.addSuites(env.map(if (xlen == 64) rv64ua else rv32ua))
        else
          TestGeneration.addSuites(env.map(if (xlen == 64) rv64uaSansLRSC else rv32uaSansLRSC))
      }
      if (coreParams.useCompressed) TestGeneration.addSuites(env.map(if (xlen == 64) rv64uc else rv32uc))
      val (rvi, rvu) =
        if (xlen == 64) ((if (vm) rv64i else rv64pi), rv64u)
        else            ((if (vm) rv32i else rv32pi), rv32u)

      TestGeneration.addSuites(rvi.map(_("p")))
      TestGeneration.addSuites((if (vm) List("v") else List()).flatMap(env => rvu.map(_(env))))
      TestGeneration.addSuite(benchmarks)
      TestGeneration.addSuite(new RegressionTestSuite(if (xlen == 64) rv64RegrTestNames else rv32RegrTestNames))
    }
  }
}

/**
 * A generator for platforms containing BOOM and Rocket systems.
 */
object Generator extends GeneratorApp
{
  // add unique test suites
  override def addTestSuites {
    implicit val p: Parameters = params
    TestSuiteHelper.addRocketTestSuites
    TestSuiteHelper.addBoomTestSuites
  }

  // specify the name that the generator outputs files as
  val longName = names.topModuleProject + "." + names.topModuleClass + "." + names.configs

  // generate files
  generateFirrtl
  generateAnno
  generateTestSuiteMakefrags
  generateROMs
  generateArtefacts
}
