organization := "edu.berkeley.cs"

version := "1.0"

name := "boom"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-Xsource:2.11")

libraryDependencies += "edu.berkeley.cs" %% "chisel-iotesters" % "1.2+"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
