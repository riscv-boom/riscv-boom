The RISC-V ISA
====================================

BOOM implements the RV64G variant of the RISC-V ISA. This includes the
MAFD extensions and the privileged specification (multiply/divide, AMOs,
load-reserve/store-conditional, single- and double-precision IEEE
754-2008 floating point). More information about the RISC-V ISA can be
found at http://riscv.org.

RISC-V provides the following features which make it easy to target with
high-performance designs:

* This greatly simplifies the Load/Store Unit, which does not need to
  have loads snoop other loads nor does coherence traffic need to snoop
  the LSU, as required by sequential consistency.
    
* The fp status register does not need to be renamed, nor can FP
  instructions throw exceptions themselves.

* All integer ALU operations exhibit no side-effects, save the writing
  of the destination register. This prevents the need to rename
  additional condition state.

* Although predication can lower the branch predictor complexity of
  small designs, it greatly complicates OoO pipelines, including the
  addition of a third read port for integer operations.

* Even JAL requires specifying an explicit . This simplifies rename
  logic, which prevents either the need to know the instruction first
  before accessing the rename tables, or it prevents adding more ports
  to remove the instruction decode off the critical path.

* This allows decode and rename to proceed in parallel.

BOOM (currently) does not implement the “C" compressed extension nor the
“V" vector extension.


