The RISC-V ISA
==============

BOOM implements the RV64GC variant of the RISC-V ISA. This includes the
MAFDC extensions and the privileged specification (multiply/divide, AMOs,
load-reserve/store-conditional, single- and double-precision IEEE
754-2008 floating point). More information about the RISC-V ISA can be
found at http://riscv.org.

RISC-V provides the following features which make it easy to target with
high-performance designs:

* **Relaxed memory model**

    * This greatly simplifies the Load/Store Unit, which does not need to
      have loads snoop other loads nor does coherence traffic need to snoop
      the LSU, as required by sequential consistency.

* **Accrued Floating Point (FP) exception flags**

    * The FP status register does not need to be renamed, nor can FP
      instructions throw exceptions themselves.

* **No integer side-effects**

    * All integer ALU operations exhibit no side-effects, other than the writing
      of the destination register. This prevents the need to rename
      additional condition state.

* **No cmov or predication**

    * Although predication can lower the branch predictor complexity of
      small designs, it greatly complicates OoO pipelines, including the
      addition of a third read port for integer operations.

* **No implicit register specifiers**

    * Even JAL requires specifying an explicit register. This simplifies rename
      logic, which prevents either the need to know the instruction first
      before accessing the rename tables, or it prevents adding more ports
      to remove the instruction decode off the critical path.

* **Registers rs1, rs2, rs3, rd are always in the same place**

    * This allows decode and rename to proceed in parallel.

BOOM (currently) does not implement the proposed “V" vector extension.
