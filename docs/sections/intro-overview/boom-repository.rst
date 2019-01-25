The BOOM Repository
====================================

The BOOM repository holds the source code to the BOOM core; it is not a
full processor and thus is **NOT A SELF-RUNNING** repository. To
instantiate a BOOM core, the Rocket-Chip generator found in the
rocket-chip git repository must be used
https://github.com/freechipsproject/rocket-chip, which provides the caches,
uncore, and other needed infrastructure to support a full processor.

The BOOM source code can be found in :code:`boom/src/main/scala`.

The code structure is shown below:

* :code:`boom/src/main/scala/`

  * :code:`bpu/`

    * :code:`2bc-table.scala`
    * :code:`base-only.scala`
    * :code:`bim.scala`
    * :code:`bpd-pipeline.scala`
    * :code:`brpredictor.scala`
    * :code:`btb-sa.scala`
    * :code:`btb.scala`
    * :code:`dense-btb.scala`
    * :code:`gshare.scala`
    * :code:`tage.scala`
    * :code:`tage-table.scala`

  * :code:`common/`

    * :code:`configs`
    * :code:`consts`
    * :code:`microop`
    * :code:`package`
    * :code:`parameters`
    * :code:`tile`
    * :code:`types`

  * :code:`exu/`

    * :code:`core.scala`
    * :code:`decode.scala`
    * :code:`execute.scala`
    * :code:`execution_units.scala`
    * :code:`fdiv.scala`
    * :code:`fppipeline.scala`
    * :code:`fpu.scala`
    * :code:`fudecode.scala`
    * :code:`functional_unit.scala`
    * :code:`imul.scala`
    * :code:`issue_ageordered.scala`
    * :code:`issue.scala`
    * :code:`issue_slot.scala`
    * :code:`issue_unordered.scala`
    * :code:`regfile-custom.scala`
    * :code:`regfile.scala`
    * :code:`registerread.scala`
    * :code:`rename-busytable.scala`
    * :code:`rename-freelist.scala`
    * :code:`rename-maptable.scala`
    * :code:`rename.scala`
    * :code:`rob.scala`

  * :code:`ifu/`

    * :code:`branchchecker.scala`
    * :code:`fetchbuffer.scala`
    * :code:`fetchmonitor.scala`
    * :code:`fetch.scala`
    * :code:`fetchtargetqueue.scala`
    * :code:`frontend.scala`
    * :code:`icache.scala`
  
  * :code:`lsu/`

    * :code:`dcacheshim.scala`
    * :code:`lsu.scala`
    * :code:`types.scala`

  * :code:`system/`

    * :code:`BoomSubsystem.scala`
    * :code:`BoomTestSuites.scala`
    * :code:`Configs.scala`
    * :code:`ExampleBoomSystem.scala`
    * :code:`Generator.scala`
    * :code:`TestHarness.scala`

  * :code:`util/`

    * :code:`elastic-reg.scala`
    * :code:`elastic-sram.scala`
    * :code:`seqmem-transformable.scala`
    * :code:`util.scala`
