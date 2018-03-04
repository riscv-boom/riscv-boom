organization := "edu.berkeley.cs"

version := "1.0"

name := "boom"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value) ++
			Seq("edu.berkeley.cs" %% "chisel-iotesters" % "latest.release")

