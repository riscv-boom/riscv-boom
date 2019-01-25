Terminology
===========

Program Counter (PC)
    A weird, terrible, anachronistic term to describe
    the address in memory of an instruction. 

Fetch Packet
    A bundle returned by the front-end which contains
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
