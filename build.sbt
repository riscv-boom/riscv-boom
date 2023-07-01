import sbt.project

ThisBuild / scalaVersion     := "2.13.10"
ThisBuild / version          := "3.0"
ThisBuild / organization     := "ucb-bar"

val chiselVersion = "3.6.0"

lazy val root = (project in file("."))
  .settings(
    name := "boom",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "rocketchip" % "1.6.0",
      "org.chipsalliance" %% "cde" % "0.1.2-4-384c06",
	  "ch.epfl.scala" %% "bloop-config" % "1.5.5"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-P:chiselplugin:genBundleElements"
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
	resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.mavenLocal
)

)
