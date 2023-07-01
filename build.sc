// import Mill dependency
import mill._
import mill.define.Sources
import mill.modules.Util
import mill.scalalib.TestModule.ScalaTest
import $ivy.`com.lihaoyi::mill-contrib-bloop:`

import scalalib._
// support BSP
import mill.bsp._

object boom extends ScalaModule { m =>
  override def millSourcePath = os.pwd
  override def scalaVersion = "2.13.10"
  override def scalacOptions = Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
    "-P:chiselplugin:genBundleElements"
  )
  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.5.6",
    ivy"edu.berkeley.cs::rocketchip:1.6.0",
	ivy"ch.epfl.scala::bloop-config:1.5.5"

  )
  override def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:3.5.6",
  )
}
