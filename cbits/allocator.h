#ifndef __allocator_H__
#define __allocator_H__

#include "memory.h"
#include "rb_tree.h"

typedef struct buddy_region_t {
  RB_TREE_ENTRY(buddy_region_t) link;
  uintptr_t region_start;
} buddy_region_t;

#define REGION_MAX_SIZE_BIT (ARCH_WORD_WIDTH - ARCH_PAGE_WIDTH)
#define REGION_COUNT (REGION_MAX_SIZE_BIT - ARCH_PAGE_WIDTH)

typedef struct regions_t {
  buddy_region_t *regions[REGION_COUNT];
} regions_t;

void add_free_region(mem_allocator_t *allocator, regions_t *regions, uintptr_t base_addr, uint64_t length);
void add_used_region(mem_allocator_t *allocator, regions_t *regions, uintptr_t base_addr, uint64_t length);

uintptr_t alloc_from_regions(size_t sz);
void free_from_regions(uintptr_t ptr);

#endif
