The BOOM Pipeline
====================================

Commentary on design decisions and justifications can be found in
paragraphs like this one.

Conceptually, BOOM is broken up into 10 stages: **Fetch**, **Decode**,
**Register Rename**, **Dispatch**, **Issue**, **Register Read**, **Execute**, **Memory**,
**Writeback** and **Commit**. However, many of those stages are
combined in the current implementation, yielding **six** stages:
**Fetch**, **Decode/Rename/Dispatch**, **Issue/RegisterRead**, **Execute**,
**Memory** and **Writeback** (**Commit** occurs asynchronously, so
I’m not counting that as part of the “pipeline").

Fetch

    Instructions are **fetched** from the Instruction Memory and
    pushed into a FIFO queue, known as the **fetch buffer**.[^1]

Decode

    **Decode** pulls instructions out of the **fetch buffer** and
    generates the appropriate “micro-op" to place into the
    pipeline.[^2]

Rename

    The ISA, or “logical", register specifiers are then **renamed**
    into “physical" register specifiers.

Dispatch

    The micro-op is then **dispatched**, or written, into the
    **Issue Window**.

Issue

    Micro-ops sitting in the **Issue Window** wait until all of
    their operands are ready, and are then **issued**.[^3] This is
    the beginning of the out–of–order piece of the pipeline.

RF Read

    Issued micro-ops first **read** their operands from the unified
    physical register file (or from the bypass network)...

Execute

    ... and then enter the **Execute** stage where the functional
    units reside. Issued memory operations perform their address
    calculations in the **Execute** stage, and then store the
    calculated addresses in the Load/Store Unit which resides in the
    **Memory** stage.

Memory

    The Load/Store Unit consists of three queues: a Load Address Queue
    (LAQ), a Store Address Queue (SAQ), and a Store Data Queue (SDQ).
    Loads are fired to memory when their address is present in the
    LAQ. Stores are fired to memory at **Commit** time (and
    naturally, stores cannot be **committed** until both their
    address and data have been placed in the SAQ and SDQ).

Writeback

    ALU operations and load operations are **written** back to the
    physical register file.

Commit

    The Reorder Buffer, or ROB, tracks the status of each instruction
    in the pipeline. When the head of the ROB is not-busy, the ROB
    **commits** the instruction. For stores, the ROB signals to the
    store at the head of the Store Queue that it can now write its
    data to memory.

BOOM supports full branch speculation and branch prediction. Each
instruction, no matter where it is in the pipeline, is accompanied by a
branch tag that marks which branches the instruction is “speculated
under". A mispredicted branch requires killing all instructions that
depended on that branch. When a branch instructions passes through
**Rename**, copies of the **Register Rename Table** and the **Free
List** are made. On a mispredict, the saved processor state is
restored.

Although Figure \[fig:boom\_stages\] shows a simplified pipeline, BOOM
implements the RV64G and privileged ISAs, which includes single- and
double-precision floating point, atomic memory support, and page-based
virtual memory.

[^1]: While the fetch buffer is N-entries deep, it can instantly read
    out the first instruction on the front of the FIFO. Put another way,
    instructions don’t need to spend N cycles moving their way through
    the *fetch buffer* if there are no instructions in front of
    them.

[^2]: Because RISC-V is a RISC ISA, currently all instructions generate
    only a single micro-op. More details on how store micro-ops are
    handled can be found in Chapter \[chapter:memory\].

[^3]: More precisely, uops that are ready assert their request, and the
    issue scheduler chooses which uops to issue that cycle.
