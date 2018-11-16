Introduction & Overview {#sec:introduction}
=======================

The goal of this document is to describe the design and implementation
of the Berkeley Out–of–Order Machine (BOOM).

BOOM is heavily inspired by the MIPS R10k and the Alpha 21264
out–of–order processors[@alpha21264; @mipsr10k]. Like the R10k and the
21264, BOOM is a unified physical register file design (also known as
“explicit register renaming").

The source code to BOOM can be found at
(<https://ucb-bar.github.io/riscv-boom>).

The BOOM Pipeline
-----------------

Commentary on design decisions and justifications can be found in
paragraphs like this one.

Conceptually, BOOM is broken up into 10 stages: [*Fetch, Decode,
Register Rename, Dispatch, Issue, Register Read, Execute, Memory,
Writeback,*]{} and [*Commit*]{}. However, many of those stages are
combined in the current implementation, yielding [*six*]{} stages:
[*Fetch, Decode/Rename/Dispatch, Issue/RegisterRead, Execute,
Memory,*]{} and [*Writeback*]{} ([*Commit*]{} occurs asynchronously, so
I’m not counting that as part of the “pipeline").

> Fetch
>
> :   Instructions are [*fetched*]{} from the Instruction Memory and
>     pushed into a FIFO queue, known as the [*fetch buffer*]{}.[^1]
>
> Decode
>
> :   [*Decode*]{} pulls instructions out of the [*fetch buffer*]{} and
>     generates the appropriate “micro-op" to place into the
>     pipeline.[^2]
>
> Rename
>
> :   The ISA, or “logical", register specifiers are then [*renamed*]{}
>     into “physical" register specifiers.
>
> Dispatch
>
> :   The micro-op is then [*dispatched*]{}, or written, into the
>     [*Issue Window*]{}.
>
> Issue
>
> :   Micro-ops sitting in the [*Issue Window*]{} wait until all of
>     their operands are ready, and are then [*issued*]{}.[^3] This is
>     the beginning of the out–of–order piece of the pipeline.
>
> RF Read
>
> :   Issued micro-ops first [*read*]{} their operands from the unified
>     physical register file (or from the bypass network)...
>
> Execute
>
> :   ... and then enter the [*Execute*]{} stage where the functional
>     units reside. Issued memory operations perform their address
>     calculations in the [*Execute*]{} stage, and then store the
>     calculated addresses in the Load/Store Unit which resides in the
>     [*Memory*]{} stage.
>
> Memory
>
> :   The Load/Store Unit consists of three queues: a Load Address Queue
>     (LAQ), a Store Address Queue (SAQ), and a Store Data Queue (SDQ).
>     Loads are fired to memory when their address is present in the
>     LAQ. Stores are fired to memory at [*Commit*]{} time (and
>     naturally, stores cannot be [*committed*]{} until both their
>     address and data have been placed in the SAQ and SDQ).
>
> Writeback
>
> :   ALU operations and load operations are [*written*]{} back to the
>     physical register file.
>
> Commit
>
> :   The Reorder Buffer, or ROB, tracks the status of each instruction
>     in the pipeline. When the head of the ROB is not-busy, the ROB
>     [*commits*]{} the instruction. For stores, the ROB signals to the
>     store at the head of the Store Queue that it can now write its
>     data to memory.
>
BOOM supports full branch speculation and branch prediction. Each
instruction, no matter where it is in the pipeline, is accompanied by a
branch tag that marks which branches the instruction is “speculated
under". A mispredicted branch requires killing all instructions that
depended on that branch. When a branch instructions passes through
[*Rename*]{}, copies of the [*Register Rename Table*]{} and the [*Free
List*]{} are made. On a mispredict, the saved processor state is
restored.

Although Figure \[fig:boom\_stages\] shows a simplified pipeline, BOOM
implements the RV64G and privileged ISAs, which includes single- and
double-precision floating point, atomic memory support, and page-based
virtual memory.

The RISC-V ISA
--------------

BOOM implements the RV64G variant of the RISC-V ISA. This includes the
MAFD extensions and the privileged specification (multiply/divide, AMOs,
load-reserve/store-conditional, single- and double-precision IEEE
754-2008 floating point). More information about the RISC-V ISA can be
found at (<http://riscv.org>).

RISC-V provides the following features which make it easy to target with
high-performance designs:

> This greatly simplifies the Load/Store Unit, which does not need to
> have loads snoop other loads nor does coherence traffic need to snoop
> the LSU, as required by sequential consistency.
>
> The fp status register does not need to be renamed, nor can FP
> instructions throw exceptions themselves.
>
> All integer ALU operations exhibit no side-effects, save the writing
> of the destination register. This prevents the need to rename
> additional condition state.
>
> Although predication can lower the branch predictor complexity of
> small designs, it greatly complicates OoO pipelines, including the
> addition of a third read port for integer operations.
>
> Even JAL requires specifying an explicit . This simplifies rename
> logic, which prevents either the need to know the instruction first
> before accessing the rename tables, or it prevents adding more ports
> to remove the instruction decode off the critical path.
>
> This allows decode and rename to proceed in parallel.

BOOM (currently) does not implement the “C" compressed extension nor the
“V" vector extension.

The  Hardware Construction Language
-----------------------------------

BOOM is implemented in the  hardware construction language. More
information about  can be found at (<http://chisel.eecs.berkeley.edu>).

Quick-start
-----------

To build a BOOM C++ emulator and run BOOM through a couple of simple
tests:\
`$` `git clone https://github.com/ucb-bar/rocket-chip.git`

`$` `cd rocket-chip`

`$` `git checkout boom`

`$` `git submodule update --init`

`$` `cd emulator; make run CONFIG`=`BOOMConfig`

this assumes you have already installed the riscv-tools toolchain. If
not, visit (<https://github.com/riscv/riscv-tools>).

The BOOM Repository
-------------------

The BOOM repository holds the source code to the BOOM core; it is not a
full processor and thus is **NOT A SELF-RUNNING** repository. To
instantiate a BOOM core, the Rocket chip generator found in the
rocket-chip git repository must be used
(<https://github.com/ucb-bar/rocket-chip>), which provides the caches,
uncore, and other needed infrastructure to support a full processor.

The BOOM source code can be found in `boom/src/main/scala`.

The code structure is shown below:

> -   `boom/src/main/scala`/
>
>     -   bpd\_pipeline.scala [ branch prediction stage.]{}
>
>     -   brpredictor.scala [ abstract branch predictor.]{}
>
>     -   configs.scala [ BOOM configurations. ]{}
>
>     -   consts.scala [ constant definitions. ]{}
>
>     -   core.scala [ the top-level of the processor core.]{}
>
>     -   dcacheshim.scala [ the shim between the the core and the
>         dcache.]{}
>
>     -   decode.scala [ decode stage.]{}
>
>     -   execute.scala [ high-level execution units (made up of
>         FUs).]{}
>
>     -   fpu.scala [ floating point unit.]{}
>
>     -   functional\_unit.scala [ low-level functional units.]{}
>
>     -   gshare.scala [ gshare branch predictor.]{}
>
>     -   imul.scala [ integer multiplier.]{}
>
>     -   issue\_ageordered.scala [ age-ordered (collasping-queue) issue
>         window implementation.]{}
>
>     -   issue.scala [ abstract issue window.]{}
>
>     -   issue\_slot.scala [ An issue window slot.]{}
>
>     -   issue\_unordered.scala [ un-ordered issue window
>         implementation.]{}
>
>     -   lsu.scala [ load/store unit.]{}
>
>     -   package.scala [ ]{}
>
>     -   parameters.scala [ knobs/parameters.]{}
>
>     -   prefetcher.scala [ data prefetcher.]{}
>
>     -   regfile.scala [ register file.]{}
>
>     -   registerread.scala [ registerRead stage and bypassing.]{}
>
>     -   rename.scala [ register renaming logic.]{}
>
>     -   rob.scala [ re-order buffer.]{}
>
>     -   tile.scala [ top-level tile.]{}
>
>     -   util.scala [ utility code.]{}
>
The Rocket-chip Repository Layout
---------------------------------

As BOOM is just a core, an entire SoC infrastructure must be provided.
BOOM was developed to use the open-source Rocket-chip SoC generator
(<https://github.com/ucb-bar/rocket-chip>). The Rocket-chip generator
can instantiate a wide range of SoC designs, including cache-coherent
multi-tile designs, cores with and without accelerators, and chips with
or without a last-level shared cache.

To manage the wide array of actively developed projects that encompass
Rocket-chip, the Rocket-chip repository makes heavy use of git
submodules. The directory structure of the Rocket-chip repository is
shown below.

> -   `rocket-chip`/
>
>     -   boom/ [ Git submodule of the  source code for the BOOM
>         core.]{}
>
>     -   chisel [ The source code to the [Chisel]{} language itself.]{}
>
>     -   firrtl [ The source code to the [FIRRTL]{} project.]{}
>
>     -   csrc/ [ Utility C/C++ source code.]{}
>
>     -   emulator/ [ Verilator simulation tools and support.]{}
>
>         -   generated-src/[ Auto-generated Verilog code.]{}
>
>         -   Makefile [ Makefile for Verilator simulation.]{}
>
>         -   output/[ Output files from Verilator simulation runs.]{}
>
>     -   riscv-tools/[ Git submodule that points to the RISC-V
>         toolchain.]{}
>
>         -   riscv-tests/ [ Source code for benchmarks and tests.]{}
>
>             -   riscv-bmarks/ [ Benchmarks written in C.]{}
>
>             -   riscv-tests/ [ Tests written in assembly.]{}
>
>     -   Makefrag [ The high-level Makefile fragment.]{}
>
>     -   src/ [  source code for rocket-chip.]{}
>
>         -   rocket/ [ Git submodule of the  source code for the Rocket
>             core (used as a library of processor components).]{}
>
>         -   junctions/ [ Git submodule of the  source code for the
>             uncore and off-chip network.]{}
>
>         -   uncore/ [ Git submodule of the  source code for the uncore
>             components (including LLC).]{}
>
>     -   sbt/ [/Scala voodoo.]{}
>
>     -   vsim/ [ The ASIC Verilog simulation and build directories. ]{}
>
### The Rocket Core - a Library of Processor Components! {#sec:rocket}

Rocket is a 5-stage in-order core that implements the RV64G ISA and
page-based virtual memory. The original design purpose of the Rocket
core was to enable architectural research into vector co-processors by
serving as the scalar [*Control Processor*]{}. Some of that work can be
found at (<http://hwacha.org>).[@hwacha]

Rocket has been taped out at least thirteen times in three different
commercial processes, and has been successfully demonstrated to reach
over 1.65 GHz in IBM 45 nm SOI.[@riscv_nature] As its namesake suggests,
Rocket is the baseline core for the Rocket-chip SoC generator. As
discussed earlier, BOOM is instantiated by replacing a Rocket tile with
a BOOM tile.

However, from BOOM’s point of view, Rocket can also be thought of as a
“Library of Processor Components." There are a number of modules created
for Rocket that are also used by BOOM - the functional units, the
caches, the translation look-aside buffers, the page table walker, and
more. Thus, throughout this document you will find references to these
Rocket components and descriptions on how they fit into BOOM.

The source code to Rocket can be found at
(<https://github.com/ucb-bar/rocket>).[@rocket]

[^1]: While the fetch buffer is N-entries deep, it can instantly read
    out the first instruction on the front of the FIFO. Put another way,
    instructions don’t need to spend N cycles moving their way through
    the [*fetch buffer*]{} if there are no instructions in front of
    them.

[^2]: Because RISC-V is a RISC ISA, currently all instructions generate
    only a single micro-op. More details on how store micro-ops are
    handled can be found in Chapter \[chapter:memory\].

[^3]: More precisely, uops that are ready assert their request, and the
    issue scheduler chooses which uops to issue that cycle.
