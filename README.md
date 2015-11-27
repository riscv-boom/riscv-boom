The Berkeley Out-of-Order RISC-V Processor
================================================

This is the source repository for the Berkeley Out-of-Order RV64G RISC-V processor (BOOM), 
written in the [Chisel hardware construction language](http://chisel.eecs.berkeley.edu).  
BOOM is a synthesizable core that targets ASIC processes. It can run on an FPGA (50 MHz on a zc706), 
but optimizing it to be an FPGA soft-core is a non-goal.

This repository only covers the source code to a "BOOM Tile"! The uncore and
surrounding infrastructure for instantiating BOOM can be found at in the 
[Rocket chip](https://github.com/ucb-bar/rocket-chip) repository.


 Feature | BOOM
--- | ---
ISA | RISC-V (RV64G)
Synthesizable |√
FPGA |√
Parameterized |√
Floating Point (IEE 754-2008) |√
Atomic Memory Op Support |√
Caches |√
Viritual Memory |√
Linux |√


**Requirements**

This repository is **NOT A SELF-RUNNING** repository. To instantiate a BOOM core, please use the Rocket chip generator found in the rocket-chip git repository (https://github.com/ucb-bar/rocket-chip).

BOOM depends on the Chisel project. It also depends on Rocket, uncore, and
junction source codes.

**Directions**

To check-out the Rocket chip SoC, build a BOOM C++ emulator, and run BOOM through a couple of simple tests:

````
   git clone git@github.com:ucb-bar/rocket-chip.git
   cd rocket-chip
   git checkout boom
   cd emulator; make run CONFIG=BOOMCPPConfig
````

**More Info**

* **Tech Report** The Berkeley Out-of-Order Machine (BOOM): An Industry-Competitive, Synthesizable, Parameterized RISC-V Processor (https://www.eecs.berkeley.edu/Pubs/TechRpts/2015/EECS-2015-167.html)
* **Slides** RISC-V Workshop #2 (http://riscv.org/workshop-jun2015/riscv-boom-workshop-june2015.pdf)
* **Video** RISC-V Workshop #2 (https://www.youtube.com/watch?v=z8UInbiQbdA)
