Terminology
===========

Fetch Packet
    A bundle returned by the Front-end which contains
    some set of consecutive instructions with a mask
    denoting which instructions are valid, amongst
    other meta-data related to instruction fetch and
    branch prediction. The **Fetch PC** will point
    to the first valid instruction in the
    **Fetch Packet**, as it is the PC used by the
    Front End to fetch the **Fetch Packet**.

Fetch PC
    The PC corresponding to the head of a
    **Fetch Packet** instruction group. 

Out-of-Order (OOO)
    Refers to the microarchitecture of a core in which
    instructions are re-ordered according to their 
    dependencies.

Fetch Buffer
    Buffer that holds **Fetch Packets** that are sent to the
    **Back-end**.

TAGE Predictor
    A high performance branch predictor. For more information
    read the paper "A case for (partially) tagged geometric history length predictors".

GShare Predictor
    A simplier branch predictor that uses a global history to index into a set of
    counters.

Bi-Modal Table (BIM)
    A counter table.

Micro-Op
    Element sent throughout the pipeline holding information about the type of
    micro-op, its PC, pointers to the FTQ, ROB, LDQ, STQs, and more.

Speculative Execution
    A microarchitectural technique that allows the processor to run-ahead during
    long latency events. For ex, during a branch, a guess is made and the processor
    follows a particular branch, speculating that it will complete the following
    instructions. However, if the branch was mispredicted, then the misspeculated 
    state must be reverted.

Front-end
    The Fetch and Branch Prediction portions of the pipeline that fetch instructions
    from the i-cache.

Back-end
    The stages starting from **Dispatch** to **Writeback**. Here instructions
    are executed, dependencies resolved, branches resolved, etc.

Fetch Boundry
    The bytes at the end of a i-cache response that might be half of an instruction
    used in RVC.

Fetch Target Queue
    Queue used to track the branch prediction information for inflight **Micro-Ops**.
    This is dequeued once all instructions in its **Fetch Packet** entry are 
    committed.

Next-Line Predictor (NLP)
    Consists of a Branch Target Buffer (BTB),
    Return Address Stack (RAS) and Bi-Modal Table (BIM).
    This is used to make quick predictions to redirect the **Front-end**

Backing predictor (BPD)
    Slower but more complicated predictor used to track longer
    histories. In BOOM you can have multiple different types of 
    a **Backing predictor** (TAGE, GShare...).

Branch Target Buffer (BTB)
    Tagged entry table in which a PC is used to find a matching
    target. Thus, if there is a hit, the specified target is used
    to redirect the pipeline.

Return Address Stack (RAS)
    Stack used to track function calls. It is pushed with a PC
    on a JAL or JALR and popped during a RET.
    
Fetch Width
    The amount of instructions retrieved from the i-cache from the
    **Front-end** of the processor.

Global History Register (GHR)
    A register holding the last N taken/not taken results of branches
    in the processor.

Rename Snapshots
    Saved state used to reset the pipeline to a correct state after a
    misspeculation or other redirecting event.

Rename Map Table
    Holds the mapping from logical ISA registers (x0-x31) to physical
    registers. Must be restored on a mis-speculation.

Busy Table
    Table indicating the status of a physical register. Once a physical
    register is ready, then the **Micro0Op** can procede through the
    rest of the pipeline (issued).

Free List
    A list of all physical registers that are currently not used. This is
    used to inform the **Rename** stage.
