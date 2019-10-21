#define BTB_SIZE 16384

#include <stdint.h>
#include <stdio.h>

uint64_t btb_addrs[BTB_SIZE];
uint64_t btb_targs[BTB_SIZE];
uint8_t btb_is_br[BTB_SIZE];
uint8_t btb_is_jal[BTB_SIZE];
uint32_t entry_point;

extern "C" void initialize_btb()
{
  for (int i = 0; i < BTB_SIZE; i++)
    btb_addrs[i] = 0;
  entry_point = 0;
}

extern "C" void predict_target(uint64_t ip, uint8_t *valid, uint64_t *target, uint8_t *is_br, uint8_t *is_jal)
{
  for (int i = 0; i < BTB_SIZE; i++) {
    if (btb_addrs[i] == ip) {
      *target = btb_targs[i];
      *is_br = btb_is_br[i];
      *is_jal = btb_is_jal[i];
      *valid = 1;
      return;
    }
  }
  *valid = 0;
}

extern "C" void update_btb(uint64_t ip, uint64_t target, uint8_t is_br, uint8_t is_jal)
{
  for (int i = 0; i < BTB_SIZE; i++) {
    if (btb_addrs[i] == ip) {
      btb_targs[i] = target;
      btb_is_br[i] = is_br;
      btb_is_jal[i] = is_jal;
      return;
    }
  }
  btb_addrs[entry_point] = ip;
  btb_targs[entry_point] = target;
  btb_is_br[entry_point] = is_br;
  btb_is_jal[entry_point] = is_jal;
  entry_point++;
}
