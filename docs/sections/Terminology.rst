\chapter{Terminology}

\TODO{To be filled in as needed.}

\TODO{come up with a better format...}

\

\


{\bf fetch packet} - A bundle returned by the front-end which contains some set of consecutive instructions with a mask denoting which instructions are valid, amongst other meta-data related to instruction fetch and branch prediction. The {\em Fetch PC} will point to the first valid instruction in the {\em fetch packet}, as it is the PC used by the Front End to fetch the {\em fetch packet}.

\

{\bf fetch PC} - The PC corresponding to the head of a {\em fetch packet} instruction group. 

{\bf PC} - A.k.a, the {\em Program Counter}.  A weird, terrible, anachronistic term to describe the address in memory of an instruction. 