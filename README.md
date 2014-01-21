Source repository for the Berkeley Out-of-Order RISC-V processor (BOOM).

To use this processor, include this repo as a git submodule and add it as to
your chip's build.scala as a Project, e.g.  lazy val boom = Project("boom",
file("boom"), settings = buildSettings)

BOOM depends on the Chisel project, make sure this library's jars are
installed. It also depends on Rocket source code, so make sure Rocket is in 
the path too.
