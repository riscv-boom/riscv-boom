Pipeline Visualization
======================

“Pipevew" is a useful diagnostic and visualization tool for seeing how
instructions are scheduled on an out-of-order pipeline.

Pipeview displays every fetched instruction and shows when it is
fetched, decoded, renamed, dispatched, issued, completed, and committed
(“retired"). It shows both committed and misspeculated instructions. It
also shows when stores were successfully acknowledged by the memory
system (“store-completion"). It is useful for programmers who wish to
see how their code is performing and for architects to see which
bottlenecks are constricting machine performance.

::

    -(       33320000) 0x00000018a0.0 sw a0, 220(gp)     
    [......f.d...i...c.........r.............]-(       33320000) 0x00000018a4.0 blt s1, a2, pc + 52
    [.......f.d..i.c............r............]-(       33320000) 0x00000018a8.0 slliw a5, a2, 2    
    [.......f.d...i...c.........r............]-(       33320000) 0x00000018ac.0 addw a5, a5, a2    
    [........f.d...i...c.........r...........]-(       33320000) 0x00000018b0.0 addiw a5, a5, -3   
    [........f.d..i.c............r...........]-(       33320000) 0x00000018b4.0 mv a0, a2          
    [.........f.d..i.c............r..........]-(       33320000) 0x00000018b8.0 li a1, 3           
    [.........f.d...i...c.........r..........]-(       33320000) 0x00000018bc.0 addi a2, s0, -184  
    [..........f.di...c............r..s......]-(       33320000) 0x00000018c0.0 sw a5, -184(s0)    
    [..........f.d...i...c.........r.........]-(       33320000) 0x00000018c4.0 jal pc - 0x1284    
    [...........f.d.i.c.............r........]-(       33320000) 0x0000000640.0 addiw a0, a0, 2    
    [...........f.d..i.c............r........]-(       33320000) 0x0000000644.0 addw a1, a0, a1    
    [............f.d.i..c............r...s...]-(       33320000) 0x0000000648.0 sw a1, 0(a2)       
    [............f.d......i...c......r.......]-(       33320000) 0x000000064c.0 ret                
    [.............f.d..i..c...........r......]-(       33320000) 0x00000018c8.0 lw a2, -188(s0)    
    [.............f.d......i...c......r......]-(       33320000) 0x00000018cc.0 addiw a2, a2, 1    
    [..............f.d..i.....c........r..s..]-(       33320000) 0x00000018d0.0 sw a2, -188(s0)    
    [..............f.d......i...c......r.....]-(       33320000) 0x00000018d4.0 bge s1, a2, pc - 44
    [...............f.d..i..c...........r....]-(       33320000) 0x00000018d8.0 lw a3, -184(s0)    
    [...............f.d...i..c..........r....]-(       33320000) 0x00000018dc.0 ld a0, -200(s0)    
    [................f.di...c............r...]-(       33320000) 0x00000018e0.0 addi a1, gp, 256   
    [................f.d.i...c...........r...]-(       33320000) 0x00000018e4.0 jal pc - 0x1294    
    [.................f.d...i.c...........r..]-(       33320000) 0x0000000650.0 addiw a6, a2, 5    
    [.................f.d....i...c........r..]-(       33320000) 0x0000000654.0 mv a5, a6          

[code:pipeview]

To display the text-based pipeline visualizations, BOOM generates traces
compatible with the O3 Pipeline Viewer included in the gem5 simulator
suite.:raw-latex:`\cite{gem5}`

To enable pipeline visualization, first set ``O``\ 3PIPEVIEW\_PRINTF in
``b``\ oom/src/main/scala/consts.scala to ``t``\ rue:

``val O3PIPEVIEW_PRINTF ``\ =\ `` true  // dump trace for O3PipeView from gem5``

Rebuild and rerun BOOM. You should find the traces (\*.out) in
emulator/output/. To generate the visualization, first download and
install gem5:raw-latex:`\cite{gem5}`, and then run:

``$`` ``boom/util/pipeview-helper.py -f <TRACE_FILE> > clean_trace.out``

``$``
``gem5/util/o3-pipeview.py --color --store_completions -o pipeview.out clean_trace.out``

You can view the visualization by running:

``$`` ``less -r pipeview.out``

To learn more about o3-pipeview.py and to download gem5 visit
http://www.m5sim.org/Visualization.
