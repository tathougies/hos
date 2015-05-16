#include "internal.h"
#include "memory.h"

void *null_allocator_alloc(mem_allocator_t *alloc);
void null_allocator_free(mem_allocator_t *alloc, void *ptr);
mem_allocator_t null_allocator = {null_allocator_alloc, null_allocator_free, NULL};

void *null_allocator_alloc(mem_allocator_t *alloc)
{
  klog("Null allocator called");
  assert(0);
  return NULL;
}

void null_allocator_free(mem_allocator_t *alloc, void *ptr)
{
  klog("Null allocator (free) called");
  assert(0);
}

/* Pool internal functions */
void pool_mark_used(uint8_t *free_ptrs_bmap, size_t i, size_t entry_count)
{
  if ( i < entry_count )
    free_ptrs_bmap[i / 8] |= 1 << (i % 8);
}

void pool_mark_free(uint8_t *free_ptrs_bmap, size_t i, size_t entry_count)
{
  if ( i < entry_count )
    free_ptrs_bmap[i / 8] &= ~(1 << (i % 8));
}

void *pool_find_free_ptr(size_t *pool_index, uint8_t *free_ptrs_bmap, uintptr_t entries, size_t entry_count, size_t entry_size)
{
  size_t i, lowest_bit;
  for ( i = 0; i < ROUND_UP(entry_count, 8) && free_ptrs_bmap[i] == 0xff; ++i );
  if ( i == ROUND_UP(entry_count, 8)) /* we ran out to the end */
    return NULL;
  else { /* We found a valid option (well maybe)! */
    size_t lowest_bit = ARCH_LOWEST_CLEAR_BIT(free_ptrs_bmap[i]);
    if ( (i * 8 + lowest_bit) >= entry_count )
      return NULL; /* This position goes past the end, so no dice */
    else {
      size_t found_index = i * 8 + lowest_bit;
      uintptr_t entry = entries + found_index * entry_size;
      if (pool_index) *pool_index = found_index;
      return (void *) entry;
    }
  }
}

uintptr_t heap_next_page(hp_t *hp)
{
  uintptr_t ret = hp->next_page;
  hp->next_page += ARCH_PAGE_SIZE;
  return ret;
}

void *mapping_allocator_alloc(mem_allocator_t *alloc)
{
  mapping_allocator_user_info_t *user_info = (mapping_allocator_user_info_t *) alloc->user_data;

  uintptr_t virt_ret = heap_next_page(user_info->virt_mem_heap),
    phys_ret = (uintptr_t) MEM_ALLOC(user_info->phys_page_allocator);


  arch_map_page(user_info->phys_page_allocator, virt_ret, phys_ret);

  return (void *) virt_ret;
}

void mapping_allocator_free(mem_allocator_t *alloc, void *ptr)
{
  /* TODO */
}

void *static_allocator_alloc(mem_allocator_t *alloc)
{
  static_allocator_user_info_t *user_info = (static_allocator_user_info_t *) alloc->user_data;
  if ( user_info->ptr_count ) {
    void *ret = (void *) *user_info->ptrs;
    --user_info->ptr_count;
    ++user_info->ptrs;
    return ret;
  } else if ( user_info->call_on_no_more ) {
    return user_info->call_on_no_more(alloc);
  } else
    return NULL;
}

void static_allocator_free(mem_allocator_t *alloc, void *ptr)
{
  /* TODO */
}

void* recording_allocator_alloc(mem_allocator_t *alloc)
{
  recording_allocator_user_info_t *user_info = (recording_allocator_user_info_t *) alloc->user_data;
  void *new_ptr = MEM_ALLOC(user_info->underlying);

  if ( user_info->spaces_left ) {
    *user_info->addrs_used = (uintptr_t) new_ptr;
    ++user_info->addrs_used;
    --user_info->spaces_left;
  } else if ( user_info->call_on_no_space ) {
    user_info->call_on_no_space(alloc, new_ptr);
  }

  return new_ptr;
}

void recording_allocator_free(mem_allocator_t *alloc, void *ptr)
{
  /* TODO */
}
