The BOOM Repository
====================================

The BOOM repository holds the source code to the BOOM core; it is not a
full processor and thus is **NOT A SELF-RUNNING** repository. To
instantiate a BOOM core, the Rocket-Chip generator found in the
rocket-chip git repository must be used
https://github.com/freechipsproject/rocket-chip, which provides the caches,
uncore, and other needed infrastructure to support a full processor.

The BOOM core source code can be found in ``boom/src/main/scala``.

The core code structure is shown below:

* ``boom/src/main/scala/``
  * ``bpu/`` - branch predictor unit
  * ``common/`` - configs, bundles, and tile definitions
  * ``exu/`` - execute/core unit
  * ``ifu/`` - instruction fetch unit
  * ``lsu/`` - load/store/memory unit
  * ``system/`` - non-core system-level infrastructure
  * ``util/`` - utilities
