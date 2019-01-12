Parameterization
================

BOOM Parameters
---------------

General Parameters
~~~~~~~~~~~~~~~~~~

Listing :numref:`general-boom-params` lists of most of the top-level parameters that you can manipulate for a BOOM core.
This is taken from :code:`src/main/scala/common/parameters.scala`.

.. _general-boom-params:
.. code-block:: scala 
    :caption: General BOOM Parameters 

    fetchWidth: Int = ?,
    decodeWidth: Int = ?,
    numRobEntries: Int = ?,
    issueParams: Seq[IssueParams] = Seq(
        IssueParams(issueWidth=?, numEntries=?, iqType=?),
        IssueParams(issueWidth=?, numEntries=?, iqType=?),
        IssueParams(issueWidth=?, numEntries=?, iqType=?)),
    numLsuEntries: Int = ?,
    numIntPhysRegisters: Int = ?,
    numFpPhysRegisters: Int = ?,
    enableCustomRf: Boolean = ?,
    enableCustomRfModel: Boolean = ?,
    maxBrCount: Int = ?,
    fetchBufferSz: Int = ?,
    enableAgePriorityIssue: Boolean = ?,
    enablePrefetching: Boolean = ?,
    enableBrResolutionRegister: Boolean = ?,
    enableCommitMapTable: Boolean = ?,
    enableBTBContainsBranches: Boolean = ?,
    enableBranchPredictor: Boolean = ?,
    enableBpdUModeOnly: Boolean = ?,
    enableBpdUSModeHistory: Boolean = ?,
    useAtomicsOnlyForIO: Boolean = ?,
    ftq: FtqParameters = FtqParameters(),
    btb: BoomBTBParameters = BoomBTBParameters(),
    bim: BimParameters = BimParameters(),
    tage: Option[TageParameters] = ?,
    gshare: Option[GShareParameters] = ?,
    bpdBaseOnly: Option[BaseOnlyParameters] = ?,
    bpdRandom: Option[RandomBpdParameters] = ?,
    intToFpLatency: Int = ?,
    imulLatency: Int = ?,
    fetchLatency: Int = ?,
    renameLatency: Int = ?,
    regreadLatency: Int = ?,
    nPerfCounters: Int = ?,
    bootFreqHz: BigInt = ?,
    fpu: Option[FPUParams] = Some(FPUParams()),
    usingFPU: Boolean = ?,
    haveBasicCounters: Boolean = ?,
    misaWritable: Boolean = ?,
    mtvecInit: Option[BigInt] = ?,
    mtvecWritable: Boolean = ?,
    mulDiv: Option[freechips.rocketchip.rocket.MulDivParams] = Some(MulDivParams()),
    nBreakpoints: Int = ?,
    nL2TLBEntries: Int = ?,
    nLocalInterrupts: Int = ?,
    tileControlAddr: Option[BigInt] = ?,
    useAtomics: Boolean = ?,
    useDebug: Boolean = ?,
    useUser: Boolean = ?,
    useVM: Boolean = ?

Other Parameters
------------------------

You can also manipulate other parameters such as Rocket Chip parameters, Uncore, BTB, BIM,
BPU, and more! To start exploring look at the default configs given in :code:`src/main/scala/common/configs.scala`
and :code:`src/main/scala/common/Configs.scala` to get a sample of
BOOM configurations.

