#include "arch.h"
#include "multiboot.h"
#include "allocator.h"
#ifndef KERNEL
#include "stdlib.h"
#include "assert.h"
#else
#include "internal.h"
#endif

#define BS_PAGE_LIST_SIZE 4096
#ifdef KERNEL
#define KERNEL_SIZE ((uintptr_t) &kernel_size)
#else
#define KERNEL_SIZE 0x0badbeef
#endif

#define MAX_MODULE_COUNT 32

typedef struct {
  uintptr_t cur_base;
  struct multiboot_memory_map *mmap;
  size_t mmap_length;
} bootstrap_allocator_user_info_t;

typedef struct {
  uint32_t mod_phys_start, mod_phys_end;
  char mod_cmdline[120];
} __attribute__((packed)) mboot_module_info_t;

struct multiboot_info *g_mboot_hdr_ptr;
extern int kernel_size;
extern mem_allocator_t buddy_page_allocator;
regions_t g_buddy_regions;

hp_t phys_memory_management_heap = HEAP_INITIALIZER(ARCH_PHYS_MEMORY_MANAGEMENT_HEAP_BASE);

uintptr_t g_pages_allocated[BS_PAGE_LIST_SIZE];
uint32_t g_pages_allocated_count = 0;

mboot_module_info_t g_mboot_modules[MAX_MODULE_COUNT];
int g_module_count;

DECL_MEMPOOL_TYPE(buddy_region_t);
DECL_RB_TREE(buddy_region_t, uintptr_t);
void add_free_region(mem_allocator_t *allocator, regions_t *regions, uintptr_t base_addr, uint64_t length);

#define COMPARE_UINTPTR(x, y) ((x < y) ? -1 : ((x > y) ? 1 : 0))
RB_TREE_IMPL(buddy_region_t, uintptr_t, COMPARE_UINTPTR, region_start, link);
MEMPOOL_IMPL(buddy_region_t);

POOL_T(buddy_region_t) g_buddy_pool = POOL_INITIALIZER(buddy_region_t);
extern mem_allocator_t null_allocator;

uintptr_t temp_pages[4];
#define TEMP_PAGES_COUNT (sizeof(temp_pages) / sizeof(temp_pages[0]))

uintptr_t _alloc_from_regions(mem_allocator_t *temp_page_allocator, regions_t *regions, size_t sz);
#ifdef KERNEL
void write_serial(char c);
#endif
void klog(const char *c)
{
#ifdef KERNEL
  /* static char *vBuf = (char *) REAL_MODE_ADDR(0xb8000UL); */
  /* for (;*c != 0; ++c, vBuf += 2) { */
  /*   vBuf[1] = '\x0C'; */
  /*   vBuf[0] = *c; */
  /* } */
  for (; *c != 0; ++c) {
    write_serial(*c);
  }
#else
  printf("klog: %s\n", c);
#endif
}

#define NIBBLE(x, i) hexChars[((x) >> i) & 0xf]
void klog_hex(uint64_t i)
{
#ifdef KERNEL
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
  klog (buf);
#else
  printf("klog_hex: %llx\n", i);
#endif
}

void *bootstrap_alloc(mem_allocator_t *alloc)
{
  bootstrap_allocator_user_info_t *bs_alloc_info = (bootstrap_allocator_user_info_t *) alloc->user_data;
  struct multiboot_memory_map *mmap = bs_alloc_info->mmap;
  size_t mmap_parsed_length = 0;
  while ( mmap_parsed_length < bs_alloc_info->mmap_length ) {

    /* Now we want to see if there's enough space at cur_base to allocate a new page. If not, then continue until we find a free region */
    if ( mmap->base_addr <= bs_alloc_info->cur_base && bs_alloc_info->cur_base <= (mmap->base_addr + mmap->length) ) {
      if ( mmap->type == MULTIBOOT_MEMORY_MAP_TYPE_FREE && (mmap->base_addr + mmap->length - bs_alloc_info->cur_base) <= ARCH_PAGE_SIZE ) {
	break;
      } else {
	/* Otherwise find the next free region... */
	mmap_parsed_length += mmap->size + 4;
	mmap = (struct multiboot_memory_map *) (((uintptr_t) mmap) + mmap->size + 4);
	while ( mmap_parsed_length < bs_alloc_info->mmap_length ) {
	  if ( mmap->type == MULTIBOOT_MEMORY_MAP_TYPE_FREE ) {
	    bs_alloc_info->cur_base = mmap->base_addr;
	    break;
	  }
	  mmap_parsed_length += mmap->size + 4;
	  mmap = (struct multiboot_memory_map *) (((uintptr_t) mmap) + mmap->size + 4);
	}
	break;
      }
    }

    mmap_parsed_length += mmap->size + 4;
    mmap = (struct multiboot_memory_map *) (((uintptr_t) mmap) + mmap->size + 4);
  }


  if ( bs_alloc_info->cur_base >= 0x100000 && bs_alloc_info->cur_base < (KERNEL_SIZE + 0x100000) ) {
    bs_alloc_info->cur_base = (KERNEL_SIZE + 0x100000 + ARCH_PAGE_SIZE - 1) & 0xfffffffffffff000;
    return bootstrap_alloc(alloc);
  } else {
    uint32_t i;
    for ( i = 0; i < g_module_count; ++i ) {
      if ( bs_alloc_info->cur_base >= g_mboot_modules[i].mod_phys_start && bs_alloc_info->cur_base < g_mboot_modules[i].mod_phys_end ) {
	bs_alloc_info->cur_base = PAGE_ADDR(((uintptr_t) (g_mboot_modules[i].mod_phys_end + ARCH_PAGE_SIZE - 1)));
	return bootstrap_alloc(alloc);
      }
    }
    /* Now add bs_alloc_info->cur_base to our allocated page list */
    g_pages_allocated[g_pages_allocated_count++] = bs_alloc_info->cur_base;
    bs_alloc_info->cur_base += ARCH_PAGE_SIZE;
    return (void *) (bs_alloc_info->cur_base - ARCH_PAGE_SIZE);
  }
}

void bootstrap_free(mem_allocator_t *alloc, void *free)
{
  klog("Bootstrap free called ");
}

/*
 * initialize_regions
 *
 * Given a multiboot information structure and a pointer to a regions structure,
 * mark the free regions as available in the regions structure.
 */
int initialize_regions(struct multiboot_info *mboot, regions_t *regions)
{
  struct multiboot_memory_map *mmap = (struct multiboot_memory_map *) REAL_MODE_ADDR((uintptr_t) mboot->mmap_addr);
  size_t mmap_parsed_length = 0;

  uint32_t i;

  /* Set up our *physical* page allocator */
  bootstrap_allocator_user_info_t bs_alloc_info = {0, mmap, mboot->mmap_length};
  mem_allocator_t bootstrap_allocator = {bootstrap_alloc,
					 bootstrap_free,
					 &bs_alloc_info};

  DECL_MAPPING_ALLOCATOR(&bootstrap_allocator, &phys_memory_management_heap, mapping_alloc);

  DECL_POOL_ALLOCATOR(&g_buddy_pool, &mapping_alloc, buddy_region_t, bootstrap_buddy_allocator);

  klog("Initializing regions! ");
  memset(regions, 0, sizeof(*regions));

  if ( (mboot->flags & MBI_FLAG_MMAP) == 0 ) {
    klog(" No map :(");
    return 1;
  }

  /* We're also going to allocate space for the MISC pages */
  arch_map_page(&bootstrap_allocator, ARCH_MISC_PAGE_ADDR_1, 0);
  arch_map_page(&bootstrap_allocator, ARCH_MISC_PAGE_ADDR_2, 0);
  arch_map_page(&bootstrap_allocator, ARCH_MISC_PAGE_ADDR_3, 0);
  arch_map_page(&bootstrap_allocator, ARCH_MISC_PAGE_ADDR_4, 0);
  arch_map_page(&bootstrap_allocator, ARCH_MISC_PAGE_ADDR_5, 0);
  arch_map_page(&bootstrap_allocator, ARCH_MISC_PAGE_ADDR_6, 0);
  arch_map_page(&bootstrap_allocator, ARCH_MISC_PAGE_ADDR_7, 0);
  arch_map_page(&bootstrap_allocator, ARCH_MISC_PAGE_ADDR_8, 0);

  /* We also want to make sure that we have enough temp pages allocated for us */
  for ( i = 0; i < TEMP_PAGES_COUNT; ++i )
    temp_pages[i] = (uintptr_t) MEM_ALLOC(&bootstrap_allocator);

  while ( mmap_parsed_length < mboot->mmap_length ) {
    if ( mmap->type == MULTIBOOT_MEMORY_MAP_TYPE_FREE ) {
      /* if ( mmap->base_addr <= 0x100000 && (mmap->base_addr + mmap->length) > 0x100000 ) { */
      /* 	if ( (0x100000 - mmap->base_addr) > 0 ) { */
      /* 	  add_free_region(&bootstrap_buddy_allocator, regions, 0x100000, 0x100000 - mmap->base_addr); */
      /* 	} */
      /* } */
      /* if ( mmap->base_addr <= (0x100000 + KERNEL_SIZE) && (mmap->base_addr + mmap->length) > (0x100000 + KERNEL_SIZE) ) { */
      /* 	if ( ((0x100000 + KERNEL_SIZE) - (mmap->base_addr + mmap->length)) > 0 ) { */
      /* 	  add_free_region(&bootstrap_buddy_allocator, regions, 0x100000 + KERNEL_SIZE, (mmap->base_addr + mmap->length) - (0x100000 + KERNEL_SIZE)); */
      /* 	} */
      /* } */
      /* if ( (mmap->base_addr < 0x100000 && (mmap->base_addr + mmap->length) <= 0x100000) || */
      /* 	   mmap->base_addr >= (0x100000 + KERNEL_SIZE) ) { */
	add_free_region(&bootstrap_buddy_allocator, regions, mmap->base_addr, mmap->length);
	/* } */
    }
    mmap_parsed_length += mmap->size + 4;
    mmap = (struct multiboot_memory_map *) (((uintptr_t) mmap) + mmap->size + 4);
  }

  /* Now, go through all the bootstrap allocated pages and mark them as used. Also, we should mark used all the kernel and module pages */
  klog("Fixing up mmap after bootstrap... ");

  /* First the kernel */
  add_used_region(&bootstrap_buddy_allocator, regions, 0x100000, PAGE_ADDR((KERNEL_SIZE + ARCH_PAGE_SIZE - 1)));

  /* Then the modules */
  for ( i = 0; i < g_module_count; ++i )
    add_used_region(&bootstrap_buddy_allocator, regions, g_mboot_modules[i].mod_phys_start, PAGE_ADDR(((uintptr_t) (g_mboot_modules[i].mod_phys_end - g_mboot_modules[i].mod_phys_start + ARCH_PAGE_SIZE - 1))));

  for ( i = 0; i < g_pages_allocated_count; ++i ) {
    add_used_region(&bootstrap_buddy_allocator, regions, g_pages_allocated[i], ARCH_PAGE_SIZE);
  }

  klog("Done with bootstrap ");
  return 0;
}

void copy_modules(struct multiboot_info *mboot, mboot_module_info_t *module_info, int *module_count)
{
  uint32_t i = 0;
  struct multiboot_module *mods = (struct multiboot_module *) REAL_MODE_ADDR(((uintptr_t) mboot->mods_addr));
  if ( mboot->flags & MBI_FLAG_MODS == 0 )
    return; /* No modules */
  for ( ; i < mboot->mods_count && i < MAX_MODULE_COUNT; ++i) {
    char *mod_cmdline = REAL_MODE_ADDR(((uintptr_t) mods[i].string));
    module_info[i].mod_phys_start = mods[i].mod_start;
    module_info[i].mod_phys_end = mods[i].mod_end;

    strncpy(module_info[i].mod_cmdline, mod_cmdline, 120);
    (*module_count)++;
  }
}

void bootstrap_kernel()
{
  g_mboot_hdr_ptr = REAL_MODE_ADDR(g_mboot_hdr_ptr);
  g_module_count = 0;
  copy_modules(g_mboot_hdr_ptr, g_mboot_modules, &g_module_count);
  klog("Have ");
  klog_hex(g_module_count);
  klog(" modules");
  initialize_regions(g_mboot_hdr_ptr, &g_buddy_regions);

  /* We're also going to go ahead and map the first multiboot module completely into memory starting at 0x400000 */

  if ( g_module_count >= 1) {
    uint64_t cur_phys_addr = g_mboot_modules[0].mod_phys_start;
    uint64_t cur_virt_addr = 0x400000;

    while ( cur_phys_addr < (g_mboot_modules[0].mod_phys_end + ARCH_PAGE_SIZE - 1) ) {
      arch_map_page(&buddy_page_allocator, cur_virt_addr, cur_phys_addr);
      arch_mark_user(cur_virt_addr);
      cur_virt_addr += ARCH_PAGE_SIZE;
      cur_phys_addr += ARCH_PAGE_SIZE;
    }
  }
}

#define ALIGN(addr, size) (addr & ~(size - 1))

void add_free_region(mem_allocator_t *allocator, regions_t *regions, uintptr_t base_addr, uint64_t length)
{
  while (length >= ARCH_PAGE_SIZE) {
    uintptr_t aligned_base = ALIGN(base_addr + ARCH_PAGE_SIZE - 1, ARCH_PAGE_SIZE),
      length_after_alignment = ALIGN(length - (aligned_base - base_addr), ARCH_PAGE_SIZE),
      new_region_length, buddy_region_base, buddy_addr;
    uint8_t region;
    buddy_region_t *buddy_region;
    printf("add free region @ %lx with %lx\n", base_addr, length);
    printf("add free region (aligned) @ %lx with %lx\n", aligned_base, length_after_alignment);

    if ( length_after_alignment < ARCH_PAGE_SIZE ) break;

    /* See which region this needs to go in. Basically, we align base_addr to the next highest page_width, and adjust length. */
    if ( aligned_base == 0 )
      region = ARCH_WORD_WIDTH;
    else
      region = MIN(ARCH_LOWEST_SET_BIT(aligned_base), REGION_MAX_SIZE_BIT);
    printf("Using base %p %d %d %d\n", aligned_base, region, ARCH_LOWEST_SET_BIT(aligned_base) - 1, ARCH_LOWEST_SET_BIT(aligned_base));
    region = MIN(ARCH_LOWEST_SET_BIT(length_after_alignment), region);
    printf("using length %d\n", region);
    region = MIN((region - ARCH_PAGE_WIDTH), REGION_COUNT - 1);
    printf("adding %p to region %d (%llx -> MIN(%d, %d))\n", aligned_base, region, length_after_alignment, ARCH_LOWEST_SET_BIT(base_addr) - 1, ARCH_LOWEST_SET_BIT(length_after_alignment) - 1);
    assert(region >= 0 && region < (REGION_MAX_SIZE_BIT - ARCH_PAGE_WIDTH));

    /* Now check if our neighbor could also be in this region. If they are, then merge. */
    new_region_length = 1 << (region + ARCH_PAGE_WIDTH);
    printf("The new length will be %llx\n", new_region_length);
    assert(new_region_length <= length);

    buddy_region_base = aligned_base & ~((new_region_length << 1) - 1);
    buddy_addr = buddy_region_base == aligned_base ? (buddy_region_base + new_region_length) : buddy_region_base;
    printf("Our buddy region will be %llx\n", buddy_addr);

    buddy_region = RB_TREE_LOOKUP(buddy_region_t, regions->regions[region], buddy_addr);

    if ( buddy_region ) {
      uintptr_t buddy_region_length = new_region_length << 1;
      buddy_region = NULL;

      do {
	uintptr_t new_buddy_region_base;
	printf("The region is being combined with it's buddy. The new region will be %d and have length %lx and base %lx\n", region + 1, buddy_region_length, buddy_region_base);

	if (buddy_region) MEM_FREE(allocator, buddy_region);

	buddy_region = RB_TREE_DELETE(buddy_region_t, &regions->regions[region], buddy_addr);
	assert(buddy_region);
	assert(buddy_region->region_start == buddy_addr);
	memset(&buddy_region->link, 0, sizeof(buddy_region->link));
	buddy_region->region_start = buddy_region_base;
	region = region + 1;

	new_buddy_region_base = buddy_region_base & ~((buddy_region_length << 1) - 1);
	buddy_addr = buddy_region_base == new_buddy_region_base ? (new_buddy_region_base + new_region_length) : new_buddy_region_base;
	buddy_region_base = new_buddy_region_base;
	buddy_region_length = buddy_region_length << 1;
      } while (RB_TREE_LOOKUP(buddy_region_t, regions->regions[region], buddy_addr));
      printf("Removed buddy will continue with insert at %llx\n", buddy_region->region_start);
      RB_TREE_INSERT(buddy_region_t, &regions->regions[region], buddy_region);
    } else {
      /* Now, let's allocate ourselves a buddy_region_t, and add it to regions */
      printf("The new region has length %llx\n", new_region_length);
      if ( RB_TREE_LOOKUP(buddy_region_t, regions->regions[region], aligned_base) ) {
	printf("The new region was already marked free\n");
      } else {
	buddy_region = (buddy_region_t *) MEM_ALLOC(allocator);
	memset(buddy_region, 0, sizeof(buddy_region_t));
	buddy_region->region_start = aligned_base;
	RB_TREE_INSERT(buddy_region_t, &regions->regions[region], buddy_region);
      }
    }
    base_addr += new_region_length;
    length -= new_region_length;
  }
}

#ifdef KERNEL
void print_rb_tree(int indent, buddy_region_t *root) {
  int i = 0;
  klog("prbtree");
  for (;i < indent; ++i) klog(" ");
  if (root) {
    klog_hex(root->region_start);
    print_rb_tree(indent + 1, RB_TREE_LEFT(root, link));
    print_rb_tree(indent + 1, RB_TREE_RIGHT(root, link));
  }
}
#endif

int buddy_rb_assert(buddy_region_t *root)
{
  int lh, rh;

  if ( !root )
    return 1; /* The empty tree is always good! */
  else {
    buddy_region_t *left = RB_TREE_LEFT(root, link),
      *right = RB_TREE_RIGHT(root, link);
    int left_black_height, right_black_height;

    assert(RB_TREE_LEFT(root, link) != root);
    assert(RB_TREE_RIGHT(root, link) != root);

    /* Children of red nodes must always be black */
    assert((RB_TREE_IS_RED(root) && !RB_TREE_IS_RED(left)) || RB_TREE_IS_BLACK(root));
    assert((RB_TREE_IS_RED(root) && !RB_TREE_IS_RED(right)) || RB_TREE_IS_BLACK(root));

    /* Binary tree condition, left key must always be <= root key, and right key must be >= */
    assert(!left || RB_TREE_KEY(left, region_start) <= RB_TREE_KEY(root, region_start));
    assert(!right || RB_TREE_KEY(right, region_start) >= RB_TREE_KEY(root, region_start));

    left_black_height = buddy_rb_assert(left);
    right_black_height = buddy_rb_assert(right);

    assert(left_black_height == right_black_height);

    return RB_TREE_IS_RED(root) ? left_black_height : left_black_height + 1;
  }
}

void mark_page_used(mem_allocator_t *allocator, regions_t *regions, uintptr_t base_addr)
{
  /* Scan the regions from highest to lowest, searching for base_addr's entry in that region.
   * If we find it, delete it and split the region into the next region, or if there is no next region, return
   */
  buddy_region_t *deleted_region = NULL;
  int32_t i;
  printf("Marking page used %llx\n", base_addr);
  for ( i = REGION_COUNT - 1; i >= 0; --i ) {
    uintptr_t base_addr_in_this_region = 1;
    base_addr_in_this_region <<= i + ARCH_PAGE_WIDTH;
    base_addr_in_this_region -= 1;
    base_addr_in_this_region = base_addr & ~base_addr_in_this_region;
    if ( deleted_region ) {
      uintptr_t split_buddy_region_start = deleted_region->region_start + (1 << (i + ARCH_PAGE_WIDTH));
      printf("Spilling %llx into region %d (buddy is %llx)\n", deleted_region->region_start, i, split_buddy_region_start);
      /* This region is from the arena above and needs to be split in this region */
      if (!RB_TREE_LOOKUP(buddy_region_t, regions->regions[i], deleted_region->region_start)) {
	printf("inserting region base\n");
	if (base_addr == 0x7c00000 && i == 6) {
	  klog("l1--");

	  // print_rb_tree(0, regions->regions[i]);
	}
	RB_TREE_INSERT(buddy_region_t, &regions->regions[i], deleted_region);
      }
      printf("check split\n");
      if (!RB_TREE_LOOKUP(buddy_region_t, regions->regions[i], split_buddy_region_start)) {
	printf("inserting split buddy\n");
	//if (split_buddy_region_start) print_rb_tree(0, regions->regions[i]);
	buddy_region_t *split_buddy = (buddy_region_t *)MEM_ALLOC(allocator);
	memset(split_buddy, 0, sizeof(buddy_region_t));
	split_buddy->region_start = split_buddy_region_start;
	RB_TREE_INSERT(buddy_region_t, &regions->regions[i], split_buddy);
	printf("Done insert\n");
      }
    }

    /* Now, attempt to find base_addr in this region... */
    printf("In region %d, base addr is %llx, mask is %llx\n", i, base_addr_in_this_region,  (uintptr_t) ~((1UL << (i + ARCH_PAGE_WIDTH)) - 1));
    printf("Deleting %p...\n", base_addr_in_this_region);
    deleted_region = RB_TREE_DELETE(buddy_region_t, &regions->regions[i], base_addr_in_this_region);
    if ( deleted_region )
      memset(&deleted_region->link, 0, sizeof(deleted_region->link));
    printf("deleted\n");
  }
  if (deleted_region) MEM_FREE(allocator, deleted_region);
}

void add_used_region(mem_allocator_t *allocator, regions_t *regions, uintptr_t base_addr, uint64_t length)
{
  /* First, we need to iterate over all the pages that this could go in. Then we called mark_used_page for each one */
  uintptr_t aligned_base_addr = PAGE_ADDR(base_addr),
    aligned_length = length + (base_addr - aligned_base_addr);
  aligned_length = PAGE_ADDR(aligned_length + ARCH_PAGE_SIZE - 1);
  for ( base_addr = aligned_base_addr; (base_addr - aligned_base_addr) < aligned_length; base_addr += ARCH_PAGE_SIZE )
    mark_page_used(allocator, regions, base_addr);
}

uintptr_t alloc_from_regions(size_t sz)
{
  /* The region allocator may trigger more page allocations. */
  /* To prevent an infinite loop, we're going to give the region allocator a
   * fake allocator, which uses four pages we have marked out for this region.
   *
   * Since the pages are mapped to the right virtual address, we later copy the pages to a
   * new physical page, and then remap them to the correct addresses.
   *
   * To do this, we layer a recording allocator on top of the mapping allocator that
   * allocates virtual addresses for the heap.
   *
   * The mapping allocator uses a new allocator which dishes out pages from our cache;
   */
  /* temp pages contains the physical addresses of the pages to allocate */
  DECL_STATIC_PAGE_ALLOCATOR(temp_pages, TEMP_PAGES_COUNT, NULL, temp_page_allocator);
  uintptr_t ret = _alloc_from_regions(&temp_page_allocator, &g_buddy_regions, sz);
  return ret;
}

uintptr_t _alloc_from_regions(mem_allocator_t *temp_page_allocator, regions_t *regions, size_t sz)
{
  uintptr_t pages_used[TEMP_PAGES_COUNT];
  uint32_t i;
  DECL_MAPPING_ALLOCATOR(temp_page_allocator, &phys_memory_management_heap, temp_mapping_allocator);
  DECL_RECORDING_ALLOCATOR(&temp_mapping_allocator, pages_used, TEMP_PAGES_COUNT, NULL, buddy_page_allocator);
  DECL_POOL_ALLOCATOR(&g_buddy_pool, &buddy_page_allocator, buddy_region_t, buddy_region_allocator);
  size_t aligned_sz = PAGE_ADDR((sz + ARCH_PAGE_SIZE - 1)), new_alignment;
  int region;
  uintptr_t base_addr = 0;

  if (aligned_sz == 0) return 0;

  /* Now we need to re-align size to the next highest power of two */
  new_alignment = (1UL << ARCH_HIGHEST_SET_BIT(aligned_sz));
  aligned_sz = ALIGN((aligned_sz + new_alignment - 1), new_alignment);

  region = MIN(ARCH_HIGHEST_SET_BIT(aligned_sz), REGION_MAX_SIZE_BIT);
  region = MIN((region - ARCH_PAGE_WIDTH), REGION_COUNT - 1);
  if ( (1UL << (region + ARCH_PAGE_WIDTH)) < sz ) {
    klog("alloc from regions ");
    klog_hex(aligned_sz);
    klog(" ");
    klog_hex(region);
    klog(" ");
    klog_hex((1UL << (region + ARCH_PAGE_WIDTH)) );
  }
  assert((1UL << (region + ARCH_PAGE_WIDTH)) >= sz);
  sz = 1UL << (region + ARCH_PAGE_WIDTH);

  for ( ; region < REGION_COUNT; region ++ ) {
    /* Check if there's something in this region that we could use */
    if ( regions->regions[region] ) {
      base_addr = regions->regions[region]->region_start;
      break;
    }
  }
  if ( region == REGION_COUNT )
    return 0;

  /* Now we marked the page as used, using the new buddy allocator */
  add_used_region(&buddy_region_allocator, &g_buddy_regions, base_addr, sz);

  /* Now, let's see which pages were actually used */
  /* This assertion checks that all allocated pages were indeed mapped to physical pages. If they weren't, the
   * paging code took a static physical page for one of its page tables, which we don't yet handle, and can't
   * be fixed here. */
  assert(static_allocator_get_used_count(temp_page_allocator, TEMP_PAGES_COUNT) == recording_allocator_get_used_count(&buddy_page_allocator, TEMP_PAGES_COUNT));
  for (i = 0; i < recording_allocator_get_used_count(&buddy_page_allocator, TEMP_PAGES_COUNT); ++ i) {
    uintptr_t static_phys_page = arch_get_phys_page(pages_used[i]),
      new_phys_page = _alloc_from_regions(temp_page_allocator, regions, ARCH_PAGE_SIZE);
    /* now, we're going to remap this page into kernel memory. This operation should not allocate, so we pass the null_allocator */
    klog("copying pages..... ! ! ! !");
    arch_map_page(&null_allocator, ARCH_MISC_PAGE_ADDR_1, static_phys_page);
    memcpy((void *) ARCH_MISC_PAGE_ADDR_1, (void *) pages_used[i], ARCH_PAGE_SIZE);
    arch_unmap_page(ARCH_MISC_PAGE_ADDR_1);
  }

  return base_addr;
}

void free_from_regions(uintptr_t ptr)
{
}

void jhc_init_msg()
{
  klog("jhc init ");
}

void jhc_hs_init_msg()
{
  klog("jhc hs init ");
}

void amain_msg()
{
  klog("amain init ");
}

void arch_unmap_init_task()
{
  if ( g_module_count >= 1 ) {
    uint64_t cur_virt_addr = 0x400000;
    while ( (cur_virt_addr - 0x400000) < (g_mboot_modules[0].mod_phys_end - g_mboot_modules[0].mod_phys_start) ) {
      arch_unmap_page(cur_virt_addr);
      cur_virt_addr += ARCH_PAGE_SIZE;
    }
  }
}
