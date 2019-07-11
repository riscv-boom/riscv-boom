#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define MEMSTART 0x80000000
#define MEMSIZE  0x10000000

#define size_mask(data, memsize) (data >> ((1 << memsize) << 3) << ((1 << memsize) << 3))

void handle_store(int8_t* mem, uint64_t* mem_history, char* mem_val,
                  int64_t tsc, int64_t addr, int64_t stdata, int memsize) {
  memcpy(mem + addr - MEMSTART, &stdata, 1 << memsize);
  for (int i = 0; i < (1 << memsize); i++) {
    uint64_t arr_addr = addr - MEMSTART + i;
    mem_history[arr_addr] = tsc;
    mem_val[arr_addr] = 'Y';
  }
}

int main(int argc, char* argv) {
  int8_t* mem = malloc(MEMSIZE);
  char* mem_val = malloc(MEMSIZE);
  memset(mem_val, 'N', MEMSIZE);
  uint64_t* mem_history = calloc(MEMSIZE, sizeof(uint64_t));
  uint64_t lct = 0;

  char *lpt = NULL;
  size_t size;
  while (getline(&lpt, &size, stdin) != -1) {
    char *line = lpt;
    if (strncmp(line, "MT", 2)) {
      continue;
    }
    line = line + 3;
    uint64_t tsc = strtol(line, &line, 16);
    int uopc = strtol(line, &line, 16);
    int memsize = strtol(line, &line, 16);
    uint64_t addr = strtol(line, &line, 16);
    uint64_t stdata = strtol(line, &line, 16);
    uint64_t wbdata = strtol(line, &line, 16);

    //printf("%lu %d %d %lx %lx %lx\n", tsc, uopc, memsize, addr, stdata, wbdata);

    if (addr > MEMSTART && addr < MEMSTART + MEMSIZE) {
      switch (uopc) {
      case 1 :
        // Loads
        if (strncmp(mem + addr - MEMSTART, (char*)&wbdata, 1 << memsize)) {

          printf("%lx Load differs %d %lx != %lx at %lx\n",
                 tsc,
                 memsize,
                 size_mask(wbdata, memsize),
                 size_mask(*(uint64_t*)(mem + addr - MEMSTART), memsize),
                 addr);
          if (strncmp(mem_val + addr - MEMSTART, "YYYYYYYY", 1 << memsize) == 0) {
            printf("reading from valid memory\n");
            exit(0);
          } else {
            handle_store(mem, mem_history, mem_val,
                         0, addr, wbdata, memsize);
          }
        } else {
          printf("%lx Load matches %d %lx == %lx at %lx\n",
                 tsc,
                 memsize,
                 size_mask(wbdata, memsize),
                 size_mask(*(uint64_t*)(mem + addr - MEMSTART), memsize),
                 addr);
        }
        break;
      case 2 :
        // Stores
        handle_store(mem, mem_history, mem_val,
                     tsc, addr, stdata, memsize);
        break;
      default :
        printf("bad opcode %d\n", uopc);
        exit(0);
      }
    }
    lct++;
  }
  printf("success\n");
}
