Quick-start
====================================

To build a BOOM C++ emulator and run BOOM through a couple of simple
tests:

```

git clone https://github.com/ucb-bar/rocket-chip.git
cd rocket-chip
git checkout boom
git submodule update --init
cd emulator
make run CONFIG=BOOMConfig

```

Note: This assumes you have already installed the riscv-tools toolchain. If
not, visit (<https://github.com/riscv/riscv-tools>).


