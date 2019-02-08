organization := "edu.berkeley.cs"

version := "1.0"

name := "boom"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-Xsource:2.11")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

lazy val root = (project in file(".")).enablePlugins(ScalaUnidocPlugin)
