#include "hos.h"

#include "stdio.h"

void *memcpy(void *dest, const void *src, size_t n)
{
    char *dp = dest;
    const char *sp = src;
    while (n--)
        *dp++ = *sp++;
    return dest;
}

void *memset(void *s, int c, unsigned long n)
{
  unsigned char* p=s;
  while(n--)
    *p++ = (unsigned char)c;
  return s;
}

void abort()
{
  asm("syscall" : : "A"(0x400));
}

void* sbrk(ptrdiff_t space_to_add)
{
  static void *heap_end = (void *) HEAP_START;
  ptrdiff_t aligned_space_to_add = (space_to_add + EXEC_PAGESIZE - 1) & ~(EXEC_PAGESIZE - 1);

  if ( space_to_add > 0 ) {
    task_id_t tId = hos_current_task();
    addr_space_t aRef = hos_current_address_space(tId);
    mem_mapping_t mapping = {MAP_ALLOCATE_ON_DEMAND, PERMS_USERSPACE_RW, 0};
    hos_add_mapping(aRef, heap_end, heap_end + aligned_space_to_add, &mapping);
    hos_switch_to_address_space(tId, aRef);
    hos_close_address_space(aRef);
    heap_end += aligned_space_to_add;
  }
  return HEAP_START;
}

int fprintf(FILE *f, const char *fmt, ...)
{
  return 0;
}

size_t fwrite(const void * ptr, size_t size, size_t nitems, FILE * stream)
{
  return 0;
}


extern int _bss_start, _bss_end;
void hos_init_clear_bss()
{
  memset(&_bss_start, 0, ((uintptr_t) &_bss_end) - ((uintptr_t) &_bss_start));
}
