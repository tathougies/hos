#include "memory.h"
#include "internal.h"
#include "allocator.h"
#include "rts/cdefs.h"
#include "rts/gc.h"
#include "rts/gc_jgc_internal.h"

typedef struct s_cache s_cache_t;
typedef struct s_megablock s_megablock_t;
extern int kernel_heap_start;

void *buddy_page_alloc(mem_allocator_t *a);
void buddy_page_free(mem_allocator_t *a, void *ptr);

DECL_MEMPOOL_TYPE(s_cache_t);
MEMPOOL_IMPL(s_cache_t);
POOL_T(s_cache_t) s_cache_pool = POOL_INITIALIZER(s_cache_t);

DECL_MEMPOOL_TYPE(s_megablock_t);
MEMPOOL_IMPL(s_megablock_t);
POOL_T(s_megablock_t) s_megablock_pool = POOL_INITIALIZER(s_megablock_t);

mem_allocator_t buddy_page_allocator = {buddy_page_alloc, buddy_page_free, NULL};

hp_t jhc_heap = HEAP_INITIALIZER(&kernel_heap_start);

DECL_MAPPING_ALLOCATOR(&buddy_page_allocator, &jhc_heap, heap_page_allocator);

DECL_POOL_ALLOCATOR(&s_cache_pool, &heap_page_allocator, s_cache_t, s_cache_allocator);
DECL_POOL_ALLOCATOR(&s_megablock_pool, &heap_page_allocator, s_megablock_t, s_megablock_allocator);

void *buddy_page_alloc(mem_allocator_t *a)
{
  return (void *) alloc_from_regions(ARCH_PAGE_SIZE);
}

void buddy_page_free(mem_allocator_t *a, void *ptr)
{
  free_from_regions((uintptr_t) ptr);
}

void *memset(void *s, int c, unsigned long n)
{
  unsigned char* p=s;
  while(n--)
    *p++ = (unsigned char)c;
  return s;
}

void *memcpy(void *dest, const void *src, size_t n)
{
    char *dp = dest;
    const char *sp = src;
    while (n--)
        *dp++ = *sp++;
    return dest;
}

void *ext_page_aligned_alloc(size_t sz)
{
  uintptr_t phys_page = alloc_from_regions(sz);
  uintptr_t ret = 0;
  size_t mapped_size, aligned_sz;

  if (sz < ARCH_PAGE_SIZE) {
    klog("page aligned alloc less than size");
    klog_hex(sz);
  }

  aligned_sz = (1UL << ARCH_LOWEST_SET_BIT(phys_page));
  assert(aligned_sz >= sz);

  for (mapped_size = 0; mapped_size < aligned_sz; mapped_size += ARCH_PAGE_SIZE) {
    uintptr_t virt_page = heap_next_page(&jhc_heap);
    if (!ret) ret = virt_page;
    arch_map_page(&buddy_page_allocator, virt_page, phys_page);
    phys_page += ARCH_PAGE_SIZE;
  }
  return (void *) ret;
}

void *ext_page_aligned_realloc(void *ret, size_t sz)
{
  if (!ret) {
    return ext_page_aligned_alloc (sz);
  } else {
    abort();
    /* void *new_page = (void *) alloc_from_regions(sz); */
    /* return NULL; */
  }
}

void ext_free(void *ptr)
{
  free_from_regions((uintptr_t) ptr);
}

struct s_megablock *ext_alloc_megablock()
{
  klog("ext_alloc_megablock ");
  return (struct s_megablock *) MEM_ALLOC(&s_megablock_allocator);
}

struct s_cache *ext_alloc_cache()
{
  klog("ext_alloc_cache ");
  return (struct s_cache *) MEM_ALLOC(&s_cache_allocator);
}

