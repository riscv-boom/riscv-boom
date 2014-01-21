import Common._
import Chisel._

package BOOM 
{
  class IssueBuffer(implicit conf: BOOMConfiguration) extends Module {
    // Contains a single entry in the issue buffer.
    class Entry(implicit conf: BOOMConfiguration) extends Module {
      class IO(implicit conf: BOOMConfiguration) extends Bundle {
        val op_out   = new MicroOp().asOutput()
        val op_in    = new MicroOp().asInput()
	val type_in  = Bits(INPUT,  conf.iwc.optype_bits)
	val type_out = Bits(OUTPUT, conf.iwc.optype_bits)
        val op_used  = Bool(OUTPUT)
        val op_ready = Bool(OUTPUT)
        val read_op  = Bool(INPUT)
        val clear    = Bool(INPUT)
        val wu_wb    = Vec.fill(conf.iwc.wb_width){ new MicroOpI() }
        val br_in    = new BrResolutionInfo().asInput
      }
      val io = new IO()
     
      // Stolen from the old issue unit
      val null_ctrl_signals = new CtrlSignals()
      null_ctrl_signals.br_type     := BR_N
      null_ctrl_signals.rf_wen      := Bool(false)
      null_ctrl_signals.pcr_fcn     := PCR_N
      null_ctrl_signals.is_load     := Bool(false)
      null_ctrl_signals.is_sta      := Bool(false)
      null_ctrl_signals.is_std      := Bool(false)

      val null_uop = new MicroOp()
      null_uop.valid := Bool(false)
      null_uop.uopc := uopNOP
      null_uop.inst := BUBBLE
      null_uop.pc   := UInt(0)
      null_uop.ctrl := null_ctrl_signals
      null_uop.is_br_or_jmp:= Bool(false)

      // Store the opcode internally as a register
      val op_reg = Reg(new MicroOp()) 
      op_reg    := op_reg
      io.op_out := op_reg
      when (io.read_op) { op_reg := io.op_in }

      // We also need to store the type field
      val type_reg = Reg(Bits(INPUT, conf.iwc.optype_bits)) 
      type_reg    := type_reg
      io.type_out := type_reg
      when (io.read_op) { type_reg := io.type_in }

      // This determines if there's actually an MicroOp in this entry.
      val used_reg = Reg(init = Bool(false))
      used_reg    := used_reg
      io.op_used  := used_reg
      when (io.clear)   { used_reg := Bool(false) }
      when (io.read_op) { used_reg := Bool(true)  }

      // Defer the wakeup logic to some user-specific code that
      // handles MicroOp wakeups
      val wu = Module(new MicroOpWakeup())
      wu.io.op_in.op     := op_reg
      wu.io.op_in.valid  := used_reg
      wu.io.op_in.optype := type_reg
      for (i <- 0 until conf.iwc.wb_width) { wu.io.wu_wb(i) := io.wu_wb(i) }

      // This signal is asserted whenever the current uOP in the
      // register is OK to be issued.
      io.op_ready := used_reg & wu.io.op_ready

      // This tracks the branch kill signal and checks if the current
      // uOP should be killed
      val br = Module(new MicroOpKill())
      br.io.op_in := wu.io.op_out.op
      br.io.br_in := io.br_in
      br.io.w_en  := io.read_op
      when (br.io.kill) { used_reg := Bool(false) }

      // Whenever we're not reading in a value from outside then just
      // write the results of our combinational logic back to the
      // register
      when (!io.read_op) { op_reg := br.io.op_out }
      when (io.clear)    { op_reg := null_uop }
    }

    class IO(implicit conf: BOOMConfiguration) extends Bundle {
      val ops_in   = Vec.fill(conf.iwc.issue_width){ new MicroOpI() }
      val ops_out  = Vec.fill(conf.iwc.fu_count){ new MicroOpO() }
      val fu_types = Vec.fill(conf.iwc.fu_count){ Bits(INPUT, conf.iwc.optype_bits) }
      val wu_wb    = Vec.fill(conf.iwc.wb_width){ new MicroOpI() }
      val br_in    = new BrResolutionInfo().asInput
      val full     = new Bool().asOutput()
      val num_free = Bits(OUTPUT, conf.iwc.buffer_width_bits)
      val flush    = new Bool().asInput()
    }
    val io = new IO()
     
    // Stolen from the old issue unit
    val null_ctrl_signals = new CtrlSignals()
    null_ctrl_signals.br_type     := BR_N
    null_ctrl_signals.rf_wen      := Bool(false)
    null_ctrl_signals.pcr_fcn     := PCR_N
    null_ctrl_signals.is_load     := Bool(false)
    null_ctrl_signals.is_sta      := Bool(false)
    null_ctrl_signals.is_std      := Bool(false)

    val null_uop = new MicroOp()
    null_uop.valid := Bool(false)
    null_uop.uopc := uopNOP
    null_uop.inst := BUBBLE
    null_uop.pc   := UInt(0)
    null_uop.ctrl := null_ctrl_signals
    null_uop.is_br_or_jmp:= Bool(false)

    // Give default values to every output port
    for (i <- 0 until conf.iwc.issue_width) {
      io.ops_in(i).ready := Bool(false)
    }
    for (i <- 0 until conf.iwc.fu_count) {
      io.ops_out(i).valid  := Bool(false)
      io.ops_out(i).op     := null_uop
      io.ops_out(i).optype := UInt(0)
    }

    // The actual issue buffer lives here
    val buf = Array.fill(conf.iwc.buffer_width){ new Entry() }
    for (i <- 0 until conf.iwc.buffer_width) {
      buf(i).io.op_in   := null_uop
      buf(i).io.type_in := UInt(0)
      buf(i).io.read_op := Bool(false)
      buf(i).io.clear   := Bool(false)
      buf(i).io.br_in   := io.br_in
      for  (j <- 0 until conf.iwc.wb_width) {
        buf(i).io.wu_wb(j) := io.wu_wb(j);
      }
    }

    // This contains a virtual free signal that's masked out when an
    // issue slot uses writes into one of the buffer entries.  This
    // ensures that all MicroOps are only issued once.
    val to_issue = Array.fill(conf.iwc.buffer_width){ Bool(INPUT) }
    for (bi <- 0 until conf.iwc.buffer_width) {
      to_issue(bi) = !buf(bi).io.op_used
    }

    // Scans every issue buffer entry to see which is availiable to
    // issue into.
    for (ii <- 0 until conf.iwc.issue_width) {
      var prev_issued = Bool(false)
      var issued = Bool(false)
      for (bi <- 0 until conf.iwc.buffer_width) {
        when (to_issue(bi) & !issued & io.ops_in(ii).valid) {
          buf(bi).io.op_in    := io.ops_in(ii).op
          buf(bi).io.type_in  := io.ops_in(ii).optype
          buf(bi).io.read_op  := Bool(true)
          io.ops_in(ii).ready := Bool(true)
        }

        prev_issued = issued
        issued = (issued | to_issue(bi)) & io.ops_in(ii).valid
        to_issue(bi) = to_issue(bi) & !(!prev_issued & issued)
      }
    }

    // This contains a virtual valid signal that's masked out when a
    // functional unit uses the value in one of the buffer entries.
    // This ensures that all MicroOps are only processed once.
    val to_proc = Array.fill(conf.iwc.buffer_width){ Bool(INPUT) }
    for (bi <- 0 until conf.iwc.buffer_width) {
      to_proc(bi) = buf(bi).io.op_ready
    }

    // Scans every issue buffer entry for a valid instruction,
    // "processing" it by simply pushing it out on every clock cycle
    for (fi <- 0 until conf.iwc.fu_count) {
      var prev_proced = Bool(false)
      var proced = Bool(false)

      for (bi <- 0 until conf.iwc.buffer_width) {
        // This is true when it's legal to issue the given instruction
        // to a particular functional unit
        val matched = (io.fu_types(fi) & buf(bi).io.type_out) != UInt(0)

        when (to_proc(bi) & !proced & io.ops_out(fi).ready & matched) {
          io.ops_out(fi).op     := buf(bi).io.op_out
          io.ops_out(fi).optype := buf(bi).io.type_out
          buf(bi).io.clear      := Bool(true)
          io.ops_out(fi).valid  := Bool(true)
        }

        prev_proced = proced
        proced = proced | (to_proc(bi) & matched)
//        proced = (proced | to_proc(bi)) & matched & io.ops_out(fi).ready
        to_proc(bi) = to_proc(bi) & !(!prev_proced & proced)
      }
    }

    // Finally, produce a signal that's true whenever there's an
    // element anywhere in this buffer.  This is used be the test
    // harness in order to drain all the live MicroOps out of the
    // processor, which is used to enforce consistancy.
    io.full := Bool(false)
    for (bi <- 0 until conf.iwc.buffer_width) {
      when (buf(bi).io.op_used) {
        io.full := Bool(true)
      }
    }

    // Count the number of free buffer slots.  The decode/dispatch
    // unit can only issue entire bundles at once, this count ensures
    // that there's enough free entries that all of them can be
    // issued.  The logic above assures that all the instructions will
    // get stored if there's space for them.
    val empty_buffers = Vec.fill(conf.iwc.buffer_width) { Bool() }
    for (bi <- 0 until conf.iwc.buffer_width) {
      empty_buffers(bi) := !buf(bi).io.op_used
    }
    io.num_free := PopCount(empty_buffers.toBits)

    // Supports pipeline flushes
    for (bi <- 0 until conf.iwc.buffer_width) {
      when (io.flush) {
        buf(bi).io.clear := Bool(true)
      }
    }
  }
}
