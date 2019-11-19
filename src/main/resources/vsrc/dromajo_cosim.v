`define INST_LEN 32
`define HARTID_LEN 32

import "DPI-C" function int dromajo_init(
    input string binary_file,
    input string bootrom_file,
    input string reset_vector,
    input string dtb_file,
    input string mmio_start,
    input string mmio_end,
    input string plic_base,
    input string plic_size,
    input string clint_base,
    input string clint_size
);

import "DPI-C" function int dromajo_step(
    input int     hartid,
    input longint dut_pc,
    input int     dut_insn,
    input longint dut_wdata,
    input longint mstatus,
    input bit     check
);

import "DPI-C" function void dromajo_raise_trap(
    input int     hartid,
    input longint cause
);

module DromajoCosimBlackBox
    #(parameter COMMIT_WIDTH, XLEN, BOOTROM_FILE, RESET_VECTOR, MMIO_START, MMIO_END, PLIC_BASE, PLIC_SIZE, CLINT_BASE, CLINT_SIZE)
(
    input clock,
    input reset,

    input [          (COMMIT_WIDTH) - 1:0] valid  ,
    input [           (`HARTID_LEN) - 1:0] hartid ,
    input [     (XLEN*COMMIT_WIDTH) - 1:0] pc     ,
    input [(`INST_LEN*COMMIT_WIDTH) - 1:0] inst   ,
    input [     (XLEN*COMMIT_WIDTH) - 1:0] wdata  ,
    input [     (XLEN*COMMIT_WIDTH) - 1:0] mstatus,
    input [          (COMMIT_WIDTH) - 1:0] check  ,

    input           int_xcpt,
    input [XLEN - 1:0] cause
);
    string __binary_file, __dtb_file, __mmio_start, __mmio_end;
    int __itr, __fail;

    initial begin
        // need binary file or dromajo will error
        if (!$value$plusargs("drj_binary_file=%s", __binary_file)) begin
            __binary_file = "";
        end
        // optional dtb param
        if (!$value$plusargs("drj_dtb_file=%s", __dtb_file)) begin
            __dtb_file = "";
        end
        __fail = dromajo_init(
            __binary_file,
            BOOTROM_FILE,
            RESET_VECTOR,
            __dtb_file,
            MMIO_START,
            MMIO_END,
            PLIC_BASE,
            PLIC_SIZE,
            CLINT_BASE,
            CLINT_SIZE);
        if (__fail != 0) begin
            $display("FAIL: Dromajo Simulation Failed");
            $fatal;
        end
    end

    always @(posedge clock) begin
        if (!reset) begin
            for (__itr=0; __itr<COMMIT_WIDTH; __itr=__itr+1) begin
                if (valid[__itr]) begin
                    __fail = dromajo_step(
                        hartid,
                        pc[((__itr+1)*XLEN - 1)-:XLEN],
                        inst[((__itr+1)*`INST_LEN - 1)-:`INST_LEN],
                        wdata[((__itr+1)*XLEN - 1)-:XLEN],
                        mstatus[((__itr+1)*XLEN - 1)-:XLEN],
                        check[__itr]);
                    if (__fail != 0) begin
                        $display("FAIL: Dromajo Simulation Failed with exit code: %d", __fail);
                        $fatal;
                    end
                end
            end

            if (int_xcpt) begin
                dromajo_raise_trap(
                    hartid,
                    cause
                );
            end
        end
    end

endmodule
