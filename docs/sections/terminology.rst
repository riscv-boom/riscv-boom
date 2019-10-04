Terminology
===========

This terminology page contains terms/concepts that are unique to the BOOM core that may/may not match with other
out-of-order terminology.

.. glossary::

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

    Fetch Buffer
        Buffer that holds **Fetch Packets** that are sent to the
        **Back-end**.

    TAGE Predictor
        A high performance branch predictor. For more information
        read the paper "A case for (partially) tagged geometric history length predictors".

    GShare Predictor
        A simpler branch predictor that uses a global history to index into a set of
        counters.

    Bi-Modal Table (BIM)
        A counter table.

    Micro-Op (UOP)
        Element sent throughout the pipeline holding information about the type of
        Micro-Op, its PC, pointers to the FTQ, ROB, LDQ, STQs, and more.

    Front-end
        The Fetch and Branch Prediction portions of the pipeline that fetch instructions
        from the i-cache.

    Back-end
        The stages starting from **Dispatch** to **Writeback**. Here instructions
        are executed, dependencies resolved, branches resolved, etc.

    Fetch Boundary
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
        in the processor. However, in BOOM, each bit does not correspond to a
        bit of history. Instead this is a hashed history.

    Rename Snapshots
        Saved state used to reset the pipeline to a correct state after a
        misspeculation or other redirecting event.

    Issue Scheduler
        <TODO: ADD DESCRIPTION HERE>

    Branch Unit

    Branch Rename Snapshot

    Execution Unit

    Functional Unit
