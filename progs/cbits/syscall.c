#include "syscall.h"

task_id_t hos_current_task()
{
  return syscall(HOS_CURRENT_TASK, 0, 0, 0, 0, 0);
}

addr_space_t hos_current_address_space(task_id_t tId)
{
  return syscall(HOS_CURRENT_ADDRESS_SPACE, tId, 0, 0, 0, 0);
}

hos_status_t hos_add_mapping(addr_space_t aRef, uintptr_t vStart, uintptr_t vEnd, mem_mapping_t *mapping)
{
  return syscall(HOS_ADD_MAPPING, aRef, vStart, vEnd, (hos_word_t)mapping, 0);
}

hos_status_t hos_switch_to_address_space(task_id_t tId, addr_space_t aRef)
{
  return syscall(HOS_SWITCH_TO_ADDRESS_SPACE, tId, aRef, 0, 0, 0);
}

void hos_close_address_space(addr_space_t aRef)
{
  syscall(HOS_CLOSE_ADDRESS_SPACE, aRef, 0, 0, 0, 0);
}

void hos_debug_log(const char *ptr)
{
  syscall(HOS_DEBUG_LOG, (hos_word_t) ptr, 0, 0, 0, 0);
}

#define NIBBLE(x, i) hexChars[((x) >> i) & 0xf]
void hos_debug_log_hex(uint64_t i)
{
  static char hexChars[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};
  static char buf[17] = {0};
  buf[0] = NIBBLE(i, 60);
  buf[1] = NIBBLE(i, 56);
  buf[2] = NIBBLE(i, 52);
  buf[3] = NIBBLE(i, 48);
  buf[4] = NIBBLE(i, 44);
  buf[5] = NIBBLE(i, 40);
  buf[6] = NIBBLE(i, 36);
  buf[7] = NIBBLE(i, 32);
  buf[8] = NIBBLE(i, 28);
  buf[9] = NIBBLE(i, 24);
  buf[10] = NIBBLE(i, 20);
  buf[11] = NIBBLE(i, 16);
  buf[12] = NIBBLE(i, 12);
  buf[13] = NIBBLE(i, 8);
  buf[14] = NIBBLE(i, 4);
  buf[15] = NIBBLE(i, 0);
  buf[16] = '\0';
  hos_debug_log(buf);
}

void jhc_error(char *ptr)
{
  hos_debug_log(ptr);
}

#if TARGET == x86_64
hos_word_t x64_syscall(hos_word_t a1, hos_word_t a2, hos_word_t a3, hos_word_t i, hos_word_t a4, hos_word_t a5);
hos_word_t syscall(hos_word_t i, hos_word_t a1, hos_word_t a2, hos_word_t a3, hos_word_t a4, hos_word_t a5)
{
  x64_syscall(a1, a2, a3, i, a4, a5);
}
#endif
