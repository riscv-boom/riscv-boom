Terminology
===========

* fetch packet - A bundle returned by the front-end which contains some set of consecutive instructions with a mask denoting which instructions are valid, amongst other meta-data related to instruction fetch and branch prediction. The {\em Fetch PC} will point to the first valid instruction in the {\em fetch packet}, as it is the PC used by the Front End to fetch the {\em fetch packet}.
* fetch PC - The PC corresponding to the head of a {\em fetch packet} instruction group. 
* PC - A.k.a, the Program Counter.  A weird, terrible, anachronistic term to describe the address in memory of an instruction. 
