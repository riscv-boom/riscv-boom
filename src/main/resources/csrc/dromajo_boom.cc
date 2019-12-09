#include <vpi_user.h>
#include <svdpi.h>

#include <stdio.h>
#include <unistd.h>
#include <getopt.h>

#include "dromajo_wrapper.h"
#include <fesvr/htif.h>

#define MAX_ARGS 20
#define MAX_STR_LEN 24

dromajo_t *dromajo = 0;

int parse_args(
    int argc,
    char *argv[])
{
    // adapted from htif parse_arguments
    const char* error_str =
        "[DRJ_ERR] Please setup simulation arguments correctly\n"
        "[EMULATOR OPTION]... [VERILOG PLUSARG]... [HOST OPTION]... BINARY [TARGET OPTION]...";

    optind = 0; // reset optind as HTIF may run getopt _after_ others
    while (1) {
      static struct option long_options[] = { HTIF_LONG_OPTIONS };
      int option_index = 0;
      int c = getopt_long(argc, argv, "-h", long_options, &option_index);

      if (c == -1) break;

      retry:

      switch (c) {
        case 'h':
          throw std::invalid_argument(error_str);
        case HTIF_LONG_OPTIONS_OPTIND:
          break;
        case HTIF_LONG_OPTIONS_OPTIND + 1:
          break;
        case HTIF_LONG_OPTIONS_OPTIND + 2:
          break;
        case HTIF_LONG_OPTIONS_OPTIND + 3:
          break;
        case '?':
            break;
        case 1: {
          std::string arg = optarg;
          if (arg == "+rfb") {
            c = HTIF_LONG_OPTIONS_OPTIND;
            optarg = nullptr;
          }
          else if (arg.find("+rfb=") == 0) {
            c = HTIF_LONG_OPTIONS_OPTIND;
            optarg = optarg + 5;
          }
          else if (arg.find("+disk=") == 0) {
            c = HTIF_LONG_OPTIONS_OPTIND + 1;
            optarg = optarg + 6;
          }
          else if (arg.find("+signature=") == 0) {
            c = HTIF_LONG_OPTIONS_OPTIND + 2;
            optarg = optarg + 11;
          }
          else if (arg.find("+chroot=") == 0) {
            c = HTIF_LONG_OPTIONS_OPTIND + 3;
            optarg = optarg + 8;
          }
          else if (arg.find("+permissive-off") == 0) {
            if (opterr)
              throw std::invalid_argument(error_str);
            opterr = 1;
            break;
          }
          else if (arg.find("+permissive") == 0) {
            if (!opterr)
              throw std::invalid_argument(error_str);
            opterr = 0;
            break;
          }
          else {
            if (!opterr)
              break;
            else {
              optind--;
              goto done_processing;
            }
          }
          goto retry;
        }
      }
    }

    done_processing:

    // expects the next argument to be the binary (otherwise error)
    if (optind >= argc) {
      printf(error_str);
      exit(1);
    }

    return optind;
}

extern "C" int dromajo_init(
    char* bootrom_file,
    char* reset_vector,
    char* dtb_file,
    char* mmio_start,
    char* mmio_end,
    char* plic_base,
    char* plic_size,
    char* clint_base,
    char* clint_size,
    char* mem_size)
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
    local_argv[local_argc] = (char*)"--memory_size";
    local_argc += 1;
    local_argv[local_argc] = (char*)mem_size;
    local_argc += 1;

    if (strlen(dtb_file) != 0) {
        local_argv[local_argc] = (char*)"--dtb";
        local_argc += 1;
        local_argv[local_argc] = (char*)dtb_file;
        local_argc += 1;
    }

    // get the binary from the input emulator arguments
    s_vpi_vlog_info info;
    if (!vpi_get_vlog_info(&info)) {
        printf("[DRJ_ERR] Failed getting VPI Information\n");
        exit(1);
    }
    int bin_idx = parse_args(info.argc, info.argv);
    local_argv[local_argc] = info.argv[bin_idx];
    local_argc += 1;

    if (MAX_ARGS < local_argc) {
        printf("[DRJ_ERR] Too many arguments\n");
        exit(1);
    }

    dromajo = new dromajo_t(local_argc, local_argv);
    if (!(dromajo->valid_state())) {
        printf("[DRJ_ERR] Failed Dromajo initialization\n");
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
