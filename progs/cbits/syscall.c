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
