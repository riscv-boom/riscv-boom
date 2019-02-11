Quick-start
===========

The best way to get started with the BOOM core is to use the BOOM project template located in the
main `GitHub organization <https://github.com/riscv-boom/boom-template>`__. There you will find the main steps
to setup your environment, build, and run the BOOM core on a C++ emulator. Here is a selected set of steps
from that repositories README:

.. _quick-start-code:
.. code-block:: bash
    :caption: Quick-Start Code

    # Download the template and setup environment
    git clone https://github.com/riscv-boom/boom-template.git
    cd boom-template
    ./scripts/init-submodules.sh

    # You may want to add the following two lines to your shell profile
    export RISCV=/path/to/install/dir
    export PATH=$RISCV/bin:$PATH

    cd boom-template
    ./scripts/build-tools.sh

    cd verisim
    make run

Note: :numref:`quick-start-code` assumes you don't have riscv-tools toolchain installed.
It will pull and build the toolchain for you.
