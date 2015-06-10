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

  if ( aligned_space_to_add > 0 ) {
    uint64_t curAddrSpaceRef = 0xffffffff;
    mem_mapping_t mapping = {0, 0, 0};
    mapping.mapping_type = MAP_ALLOCATE_ON_DEMAND;
    mapping.perms = PERMS_USERSPACE_RW;
    hos_add_mapping((uint64_t) curAddrSpaceRef, heap_end, heap_end + aligned_space_to_add, (mem_mapping_t *) &mapping);
    //     for(;;);
    heap_end += aligned_space_to_add;
  }
  return (void *) (((uintptr_t) heap_end) - aligned_space_to_add);
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
