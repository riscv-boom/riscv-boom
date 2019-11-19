#include <vpi_user.h>
#include <svdpi.h>

#include <stdio.h>

#include "dromajo.h"

#define MAX_ARGS 20
#define MAX_STR_LEN 24

dromajo_t *dromajo = 0;

extern "C" int dromajo_init(
    char* binary_file,
    char* bootrom_file,
    char* reset_vector,
    char* dtb_file,
    char* mmio_start,
    char* mmio_end,
    char* plic_base,
    char* plic_size,
    char* clint_base,
    char* clint_size)
{
    // setup arguments
    char *local_argv[MAX_ARGS];
    char local_argc = 0;
    char mmio_range[MAX_STR_LEN] = "";
    char plic_params[MAX_STR_LEN] = "";
    char clint_params[MAX_STR_LEN] = "";

    local_argv[local_argc] = (char*)"./dromajo";
    local_argc += 1;
    local_argv[local_argc] = (char*)"--compact_bootrom";
    local_argc += 1;
    local_argv[local_argc] = (char*)"--custom_extension";
    local_argc += 1;
    local_argv[local_argc] = (char*)"--reset_vector";
    local_argc += 1;
    local_argv[local_argc] = (char*)reset_vector;
    local_argc += 1;
    local_argv[local_argc] = (char*)"--bootrom";
    local_argc += 1;
    local_argv[local_argc] = (char*)bootrom_file;
    local_argc += 1;
    local_argv[local_argc] = (char*)"--mmio_range";
    local_argc += 1;
    strcat(mmio_range, (char*)mmio_start);
    strcat(mmio_range, ":");
    strcat(mmio_range, (char*)mmio_end);
    local_argv[local_argc] = (char*)mmio_range;
    local_argc += 1;
    local_argv[local_argc] = (char*)"--plic";
    local_argc += 1;
    strcat(plic_params, (char*)plic_base);
    strcat(plic_params, ":");
    strcat(plic_params, (char*)plic_size);
    local_argv[local_argc] = (char*)plic_params;
    local_argc += 1;
    local_argv[local_argc] = (char*)"--clint";
    local_argc += 1;
    strcat(clint_params, (char*)clint_base);
    strcat(clint_params, ":");
    strcat(clint_params, (char*)clint_size);
    local_argv[local_argc] = (char*)clint_params;
    local_argc += 1;

    if (strlen(dtb_file) != 0) {
        local_argv[local_argc] = (char*)"--dtb";
        local_argc += 1;
        local_argv[local_argc] = (char*)dtb_file;
        local_argc += 1;
    }
    if (strlen(binary_file) != 0) {
        local_argv[local_argc] = (char*)binary_file;
        local_argc += 1;
    }

    if (MAX_ARGS < local_argc) {
        printf("[DEBUG] Too many arguments\n");
        exit(1);
    }

    dromajo = new dromajo_t(local_argc, local_argv);
    if (!(dromajo->valid_state())) {
        printf("[DEBUG] Failed Dromajo initialization\n");
        return 1;
    }

    return 0;
}

extern "C" int dromajo_step(
    int      hartid,
    uint64_t dut_pc,
    uint32_t dut_insn,
    uint64_t dut_wdata,
    uint64_t mstatus,
    bool     check)
{
    return dromajo->step(hartid, dut_pc, dut_insn, dut_wdata, mstatus, check);
}

extern "C" void dromajo_raise_trap(
    int     hartid,
    int64_t cause)
{
    dromajo->raise_trap(hartid, cause);
}
