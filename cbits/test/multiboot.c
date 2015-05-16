#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "multiboot.h"
#include "allocator.h"

DECL_MEMPOOL_TYPE(buddy_region_t);
DECL_MALLOC_ALLOCATOR(stdalloc, POOL_PAGE_T(buddy_region_t));
//MEMPOOL_IMPL(buddy_region_t);

POOL_T(buddy_region_t) buddy_pool = POOL_INITIALIZER(buddy_region_t);
DECL_POOL_ALLOCATOR(&buddy_pool, &stdalloc, buddy_region_t, region_alloc);
//DECL_MALLOC_ALLOCATOR(region_alloc, buddy_region_t);

void print_rb_tree(int indent, buddy_region_t *root)
{
  int i = 0;
  for (;i < indent; ++i) printf(" ");
  if (!root)
    printf("LEAF\n");
  else {
    printf("0x%llx (%s)\n", RB_TREE_KEY(root, region_start), RB_TREE_IS_BLACK(root)?"black":"red");
    print_rb_tree(indent + 1, RB_TREE_LEFT(root, link));
    print_rb_tree(indent + 1, RB_TREE_RIGHT(root, link));
  }
}

void print_regions(regions_t *regions)
{
  int i = 0;
  for (; i < (REGION_MAX_SIZE_BIT - ARCH_PAGE_WIDTH); ++i) {
    printf ("Region %d\n", i);
    printf("----------\n");
    print_rb_tree(0, regions->regions[i]);
    printf("\n");
  }
}

int main(int argc, char **argv)
{
  regions_t regions = {0};

  assert(ARCH_LOWEST_SET_BIT(0x1000) == 12);
  assert(ARCH_LOWEST_SET_BIT(0x2000) == 13);
  assert(ARCH_LOWEST_SET_BIT(0x4000) == 14);
  assert(ARCH_LOWEST_SET_BIT(0x8000) == 15);
  assert(ARCH_LOWEST_SET_BIT(0x10000) == 16);
  assert(ARCH_LOWEST_SET_BIT(0x20000) == 17);

  assert(ARCH_HIGHEST_SET_BIT(0x3000) == 13);

  printf("lowest clear bit %d %d %d\n", ARCH_LOWEST_CLEAR_BIT(0x3), ARCH_LOWEST_SET_BIT(0x3), ARCH_HIGHEST_SET_BIT(0x3));
  assert(ARCH_LOWEST_CLEAR_BIT(0x3) == 2);
  assert(ARCH_LOWEST_CLEAR_BIT(0x1) == 1);
  assert(ARCH_LOWEST_CLEAR_BIT(0) == 0);

  //add_free_region(&region_alloc, &regions, 0, 0x9fc00);
  add_free_region(&region_alloc, &regions, 0, 0x9fc00);
  add_free_region(&region_alloc, &regions, 0x11f000, 0x7ed1000); 
		  //  add_free_region(&region_alloc, &regions, 0x5000, 0xfff000);

  print_regions(&regions);

  printf("Deleting\n");
  add_used_region(&region_alloc, &regions, 0x11f000, 0x1000);
  add_used_region(&region_alloc, &regions, 0x120000, 0x1000);
  add_used_region(&region_alloc, &regions, 0x121000, 0x1000);
  add_used_region(&region_alloc, &regions, 0x122000, 0x1000);
  add_used_region(&region_alloc, &regions, 0x123000, 0x1000);
  add_used_region(&region_alloc, &regions, 0x124000, 0x1000);
  add_used_region(&region_alloc, &regions, 0x125000, 0x1000);
  add_used_region(&region_alloc, &regions, 0x126000, 0x1000);

  print_regions(&regions);

  add_used_region(&region_alloc, &regions, 0x7c00000, 0x200000);

  return 0;
}
