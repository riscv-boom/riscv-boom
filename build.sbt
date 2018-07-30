organization := "edu.berkeley.cs"

version := "1.1"

name := "boom"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-deprecation","-unchecked","-Xsource:2.11")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
