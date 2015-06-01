#ifndef __syscall_H__
#define __syscall_H__

#include "hos.h"
#include <stdint.h>

#if HOS_IS_64_BIT
typedef uint64_t hos_word_t;
#elif HOS_IS_32_BIT
typedef uint32_t hos_word_t;
#endif

/* HOS syscall numbers */

/* Sectior 0 - debug log */
#define HOS_DEBUG_LOG 0

/* Section 1 - Address space management */
#define HOS_CURRENT_ADDRESS_SPACE 0x001
#define HOS_ADD_MAPPING 0x002
#define HOS_DELETE_MAPPING 0x003
#define HOS_CLOSE_ADDRESS_SPACE 0x004
#define HOS_SWITCH_TO_ADDRESS_SPACE 0x005

/* Section 4 - Process management */
#define HOS_KILL_PROCESS 0x400
#define HOS_CURRENT_TASK 0x401

typedef hos_word_t hos_status_t;

#if HOS_IS_64_BIT
#define HOS_ERROR 0xffffffffffffffffUL
#endif

#define HOS_ERR_INSUFFICIENT_PRIVILEGES HOS_ERROR
#define HOS_ERR_NO_SUCH_ADDRESS_SPACE (HOS_ERROR - 1)
#define HOS_ERR_NO_SUCH_TASK (HOS_ERROR - 2)

hos_word_t syscall(hos_word_t i, hos_word_t arg1, hos_word_t arg2, hos_word_t arg3, hos_word_t arg4, hos_word_t arg5);

/* Types */

typedef uint32_t addr_space_t;
typedef uint32_t task_id_t;

#define PERMS_PRIVILEGED_RW 0x1
#define PERMS_PRIVILEGED_RO 0x0
#define PERMS_USERSPACE_RW 0x3
#define PERMS_USERSPACE_RO 0x2
typedef uint8_t mem_permissions_t;

typedef struct mem_mapping_t {
  #define MAP_ALLOCATE_ON_DEMAND 1
  #define MAP_ALLOCATE_IMMEDIATELY 2
  #define MAP_FROM_PHYSICAL 3
  uint8_t mapping_type;
  mem_permissions_t perms;
  union {
    uint64_t phys_base;

    #define MAP_ALIGNMENT_UNALIGNED 0
    uint64_t alignment;
  };
} __attribute__((aligned)) mem_mapping_t;

task_id_t hos_current_task();
addr_space_t hos_current_address_space(task_id_t taskId);
hos_status_t hos_add_mapping(addr_space_t aRef, uintptr_t vStart, uintptr_t vEnd, mem_mapping_t *mapping);
hos_status_t hos_switch_to_address_space(task_id_t tId, addr_space_t aRef);
void hos_close_address_space(addr_space_t aRef);
void hos_debug_log(const char *ptr);

#endif
