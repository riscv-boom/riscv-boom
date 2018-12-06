The Berkeley Out-of-Order RISC-V Processor
================================================

The Berkeley Out-of-Order Machine (BOOM) is a synthesizable and parameterizable open source RV64G RISC-V core written in the 
[Chisel](https://chisel.eecs.berkeley.edu/) hardware construction language. While BOOM is primarily ASIC optimized, it is also usable on FPGAs. 
We support the FireSim flow to run BOOM at 90+ MHz on FPGAs on Amazon EC2 F1. Created at the University of California,
Berkeley in the [Berkeley Architecture Research](https://bar.eecs.berkeley.edu/) group, its focus is to create a high 
performance, synthesizable, and parameterizable core for architecture research. 

Feature | BOOM
--- | ---
ISA | RISC-V (RV64G)
Synthesizable |√
FPGA |√
Parameterized |√
Floating Point (IEEE 754-2008) |√
Atomic Memory Op Support |√
Caches |√
Viritual Memory |√
Boots Linux |√
Privileged Arch v1.11 |√
External Debug |√

**Google group:** (https://groups.google.com/forum/#!forum/riscv-boom)

For documentation on BOOM visit the BOOM website (www.boom-core.org/).


### Important!

This repository is **NOT A SELF-RUNNING** repository. To instantiate a BOOM core, please use the
boom-template SoC generator found in the git repository (https://github.com/riscv-boom/boom-template).

Note: you **MUST** build the riscv-tools as described to build the correct version. A copy of
riscv-tools you have built yourself previously may be out of date! Likewise, the `master` branch of
risv-tools may be running ahead and may also not work!


### Requirements

You must set the $RISCV environment variable to where you would like the RISC-V toolchain to be
installed. You must also add $RISCV/bin to your $PATH.

The instructions below will walk you through installing the RISC-V toolchain. If you run into
problems, go to the README in [riscv-tools](https://github.com/riscv/riscv-tools) for additional
information.

### Code Organization

The Chisel source code is found in `src/main/scala`:

 * bpu - branch predictor unit
 * exu - execute/core unit
 * ifu - instruction fetch unit
 * lsu - load/store/memory unit
 * common - configs, bundle, and tile definitions
 * system - Non-core system-level infrastructure
 * util - utilities
 

### Directions

To build a BOOM Verilator emulator and its corresponding RISC-V toolchain, and run BOOM through a
couple of simple tests:

````
   $ git clone https://github.com/riscv-boom/boom-template.git
   $ cd boom-template
   $ ./scripts/init-submodules.sh
   $ ./scripts/build-tools.sh 
   $ cd verisim
   $ make CONFIG=BoomConfig
   $ make run CONFIG=BoomConfig
````

Note: the above `build-tools.sh` script builds a specific commit of the risv-tools that BOOM is
compatible with. Building your own riscv-tools copy *may* produce an incompatible version (there is
too much development churn in risv-tools currently!). The `build-tools.sh` will also build a RV64G
toolchain -- the default riscv-tool build scripts produce an incompatible RV64GC toolchain.

There are many BOOM configurations to choose from (and modify!). In fact, the `CONFIG` variable
defaults to `BoomConfig`, so it is not necessary to pass a `CONFIG` option.

 
### Installing the RISC-V Toolchain

First, set the $RISCV environment variable (to where you want the toolchain to be installed). You 
will also need to add $RISCV/bin to your $PATH.

````
   $ git clone https://github.com/riscv-boom/boom-template.git
   $ cd boom-template
   $ ./scripts/build-tools.sh 
````

That's it. But read on for some more information about what's going on behind the scenes.

By default, `riscv-tools/build.sh` builds a RV64GC compiler. Therefore, We need to change that as
BOOM does not support the RVC extension.

````
   $ export RISCV=/path/to/install/riscv/toolchain
   $ export PATH="${PATH}:$RISCV/bin"
   $ git clone https://github.com/riscv-boom/boom-template.git
   $ cd boom-template/rocket-chip
   $ git submodule update --init
   $ cd riscv-tools
   $ git submodule update --init --recursive
   $ cp build.sh build-rv64g.sh
   $ vim build-rv64g.sh
````

Modify the `riscv-gnu-toolchain` and `riscv-isa-sim` entries to specify rv64imafd as the ISA we want
to build:

````
build_project riscv-isa-sim --prefix=$RISCV --with-fesvr=$RISCV --with-isa=rv64imafd
build_project riscv-gnu-toolchain --prefix=$RISCV --with-arch=rv64imafd
````

Now we can build the riscv-tools within (boom-template/rocket-chip/riscv-tools):

````
   $ ./build-rv64g.sh
````

For more detailed information on the toolchain, visit 
[the riscv-tools repository](https://github.com/riscv/riscv-tools).


## Using the gem5 O3 Pipeline Viewer with BOOM

The O3 Pipeline Viewer is an out-of-order pipeline viewer included in the
gem5 suite. BOOM is capable of generating traces compatible with the
pipeline viewer, which is useful for understanding what causes
pipeline stalls and flushes.

To generate gem5 compatible traces, first set O3PIPEVIEW_PRINTF in
`boom/src/main/scala/consts.scala` to true:

````
val O3PIPEVIEW_PRINTF = true  // dump trace for O3PipeView from gem5
````

Rebuild and rerun BOOM. You should find the traces (*.out) in 
emulator/output/. To generate the visualization run:

````
   $ boom/util/pipeview-helper.py -f <TRACE_FILE> > cleaned_trace.out
   $ path_to_gem5/util/o3-pipeview.py --color --store_completions -o pipeview.out cleaned_trace.out
````

You can view the visualization by running:
````
   $ less -r pipeview.out
````
For more details (and to download o3-pipeview.py), visit the [gem5 wiki](http://www.m5sim.org/Visualization).



## More Info

* **The Design Spec** - https://github.com/riscv-boom/riscv-boom-doc
* **Google group:** - https://groups.google.com/forum/#!forum/riscv-boom
* **Chisel Learning Journey** - https://github.com/librecores/riscv-sodor/wiki/Chisel-Learning-Journey
* **Tech Report** - The Berkeley Out-of-Order Machine (BOOM): An Industry-Competitive, Synthesizable, Parameterized RISC-V Processor (https://www.eecs.berkeley.edu/Pubs/TechRpts/2015/EECS-2015-167.html)
* **CARRV Workshop Report** - BOOM v2: an open-source out-of-order RISC-V core (https://www2.eecs.berkeley.edu/Pubs/TechRpts/2017/EECS-2017-157.html)
* **Slides** - RISC_V Workshop #3 (https://riscv.org/wp-content/uploads/2016/01/Wed1345-RISCV-Workshop-3-BOOM.pdf)
* **Video** - RISC-V Workshop #3 (https://www.youtube.com/watch?v=JuJDPbzWpR0)
* **Slides** - RISC-V Workshop #2 (https://riscv.org/wp-content/uploads/2015/06/riscv-boom-workshop-june2015.pdf)
* **Video** - RISC-V Workshop #2 (https://www.youtube.com/watch?v=z8UInbiQbdA)


### Disclaimer!

The RISC-V privileged ISA, platform, and Debug specs are still in flux. BOOM will do its best to
stay up-to-date with it!

BOOM is a work-in-progress and remains in active development.


## FAQ

*Help! BOOM isn't working.*

First verify the software is not an issue. Run spike first:

````
# Verify it works on spike.
spike --isa=rv64imafd my_program

# Then we can run on BOOM.
./emulator-freechips.rocketchip.system-SmallBoomConfig my_program 
````

Also verify the riscv-tools you built is the one pointed to within 
the boom-template/rocket-chip/riscv-tools repository. Otherwise a version mismatch can easily occur!

*Master branch is broken! How do I get a working BOOM?*

The [boom-template](https://github.com/riscv-boom/boom-template) super-repo should
always be pointing to a working boom/rocket-chip/riscv-tools combination. The
`master` branch of riscv-boom may run ahead though. Ideally, `master` should never be
broken, but it may be somewhat unstable as development continues. For more
stability, I recommend using one of the tagged
[releases](https://github.com/riscv-boom/riscv-boom/releases).
