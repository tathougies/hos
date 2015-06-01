#ifndef __memory_H__
#define __memory_H__

#include "sys/queue.h"
#include "rb_tree.h"
#include "util.h"
#include "arch.h"

/* A way to allocate memory */
typedef struct mem_allocator_t {
  void *(*alloc)(struct mem_allocator_t *);
  void(*free)(struct mem_allocator_t *, void*);
  void *user_data;
} mem_allocator_t;

#define MEM_ALLOC(allocator) (allocator)->alloc((allocator))
#define MEM_FREE(allocator, p) (allocator)->free((allocator), p)

#define DECL_MALLOC_ALLOCATOR(name, for_type)		\
  void *name ## _alloc(mem_allocator_t *a) {		\
    void *ret = malloc(sizeof(for_type));		\
    memset(ret, 0, sizeof(for_type));			\
    return ret;						\
  }							\
  void name ## _free(mem_allocator_t *a, void *p) {	\
    free(p);						\
  }							\
  mem_allocator_t name = {name ## _alloc, name ## _free, NULL};

/* A way to allocate pages.
 *
 * Used to change the strategy with which we allocate pages as the memory management
 * layer develops.
 */
typedef struct page_allocator_t
{
  uintptr_t(*allocate_page)(struct page_allocator_t*);
  void(*free_page)(struct page_allocator_t*, uintptr_t);
  void *user_data;
} page_allocator_t;

/* Pool allocators */

#define POOL_PAGE_T(for_type) for_type ## _pool_page
#define POOL_T(for_type) for_type ## _pool

#define SZ(for_type, mem) SIZEOF_MEMBER(struct POOL_PAGE_T(for_type), mem)
#define POOL_ENTRIES(for_type) (((ARCH_PAGE_SIZE - sizeof(struct POOL_PAGE_T(for_type) *) * 2) * 8)/(sizeof(for_type) + 1))

/* pool_page_t
 *
 * Splay finger heap data structure
 *
 * left, right - left and right branches
 * has_free - true if any of our descendants have free entries
 * has_uneven - true if any of our descendants are uneven (they have only one of their children filled)
 * node_count - The number of pages below us in the hierarchy
 * free_ptrs_bmap - bitmap of all entries which are free
 * entries - array of free entries
 */
#define DECL_MEMPOOL_TYPE(for_type)					\
  struct POOL_PAGE_T(for_type)  {					\
    LIST_ENTRY(POOL_PAGE_T(for_type)) link;				\
    uint8_t free_ptrs_bmap[ROUND_UP(POOL_ENTRIES(for_type), 8)];	\
    for_type entries[POOL_ENTRIES(for_type)];				\
  } __attribute__((aligned));						\
  typedef struct POOL_PAGE_T(for_type) POOL_PAGE_T(for_type);		\
  typedef LIST_HEAD(POOL_T(for_type), POOL_PAGE_T(for_type)) POOL_T(for_type); \
									\
  for_type *pool_alloc_ ## for_type (POOL_T(for_type) *pool, mem_allocator_t *allocator); \
  void pool_dealloc_ ## for_type (POOL_T(for_type) *pool, mem_allocator_t *allocator, for_type *ptr); \
  void *pool_allocator_alloc_ ## for_type(mem_allocator_t *allocator); \
  void pool_allocator_free_ ## for_type(mem_allocator_t *alloc, void *ptr);
void klog_hex(uint64_t i);
void klog(const char *k);
#define MEMPOOL_IMPL(for_type)			\
  for_type *pool_alloc_ ## for_type (POOL_T(for_type) *pool, mem_allocator_t *allocator) { \
    for_type *ret = NULL;						\
    POOL_PAGE_T(for_type) *page = LIST_FIRST(pool);			\
    assert(sizeof(POOL_T(for_type)) <= ARCH_PAGE_SIZE);			\
    if ( page ) {							\
      size_t pool_index;						\
      ret = pool_find_free_ptr(&pool_index, page->free_ptrs_bmap,	\
			       (uintptr_t) page->entries,		\
			       sizeof(page->entries) / sizeof(page->entries[0]), \
			       P(page->entries + 1) - P(page->entries)); \
      pool_mark_used(page->free_ptrs_bmap, pool_index, sizeof(page->entries) / sizeof(page->entries[0])); \
      if ( !pool_find_free_ptr(NULL, page->free_ptrs_bmap,		\
			       (uintptr_t) page->entries,		\
			       sizeof(page->entries) / sizeof(page->entries[0]), \
			       P(page->entries + 1) - P(page->entries)) ) { \
	/* If we have no more free pointers, pop this page from the list */ \
	LIST_REMOVE(page, link);					\
	memset(&page->link, 0, sizeof(page->link));			\
      }									\
      return ret;							\
    } else {								\
      /* There is no space left, so we need to allocate a new page */	\
      POOL_PAGE_T(for_type) *new_page = MEM_ALLOC(allocator);		\
      if ( !new_page )							\
	die("Out of memory!");						\
      LIST_INSERT_HEAD(pool, new_page, link);				\
      return pool_alloc_ ## for_type (pool, allocator);			\
    }									\
  }									\
									\
  void pool_dealloc_ ## for_type (POOL_T(for_type) *pool, mem_allocator_t *allocator, for_type *ptr) { \
    POOL_PAGE_T(for_type) *page = (POOL_PAGE_T(for_type) *)PAGE_ADDR(ptr); \
    size_t entry_phys_size = P(page->entries + 1) - P(page->entries);	\
    size_t entry_offset = P(ptr) - P(page->entries);			\
    uintptr_t entry_index = entry_offset / entry_phys_size;		\
    int was_full = !pool_find_free_ptr(NULL, page->free_ptrs_bmap,	\
					(uintptr_t) page->entries,	\
					sizeof(page->entries) / sizeof(page->entries[0]), \
					P(page->entries + 1) - P(page->entries)); \
    int i = 0;								\
    pool_mark_free(page->free_ptrs_bmap, entry_index, sizeof(page->entries) / sizeof(page->entries[0])); \
    memset(ptr, 0, sizeof(for_type));					\
    if ( was_full ) {							\
      LIST_INSERT_HEAD(pool, page, link);				\
    }									\
    return;								\
  }									\
									\
  void *pool_allocator_alloc_ ## for_type(mem_allocator_t *allocator) { \
    pool_allocator_user_data_t *data = (pool_allocator_user_data_t *) allocator->user_data; \
    return (void *) POOL_ALLOC(data->pool, data->allocator, for_type);	\
  }									\
									\
  void pool_allocator_free_ ## for_type(mem_allocator_t *allocator, void *ptr) { \
    pool_allocator_user_data_t *data = (pool_allocator_user_data_t *) allocator->user_data; \
    POOL_DEALLOC(data->pool, data->allocator, for_type, ptr);		\
  }

#define POOL_INITIALIZER(for_type) LIST_HEAD_INITIALIZER(for_type)
#define POOL_ALLOC(pool, allocator, for_type) pool_alloc_ ## for_type (pool, allocator)
#define POOL_DEALLOC(pool, allocator, for_type, ptr) pool_dealloc_ ## for_type (pool, allocator, ptr)
#define DECL_POOL_ALLOCATOR(pool, allocator, for_type, name)		\
  pool_allocator_user_data_t name ## _user_data = {(void *) pool, allocator}; \
  mem_allocator_t name = {pool_allocator_alloc_ ## for_type, pool_allocator_free_ ## for_type, (void *) & name ## _user_data}

/* Pool internal-use only */
typedef struct {
  void *pool;
  mem_allocator_t *allocator;
} pool_allocator_user_data_t;

void pool_mark_used(uint8_t *free_ptrs_bmap, size_t i, size_t entry_count);
void pool_mark_free(uint8_t *free_ptrs_bmap, size_t i, size_t entry_count);
void *pool_find_free_ptr(size_t *i, uint8_t *free_ptrs_bmap, uintptr_t entries, size_t entry_count, size_t entry_size);

/* Heaps */
typedef struct {
  uintptr_t base;
  uintptr_t next_page;
} hp_t;

#define HEAP_INITIALIZER(base) { base, base }

uintptr_t heap_next_page(hp_t *heap);

/* Mapping allocator */
typedef struct {
  mem_allocator_t *phys_page_allocator;
  hp_t *virt_mem_heap;
} mapping_allocator_user_info_t;

void *mapping_allocator_alloc(mem_allocator_t *alloc);
void mapping_allocator_free(mem_allocator_t *alloc, void *to_free);

#define DECL_MAPPING_ALLOCATOR(phys_page_allocator, virt_mem_heap, alloc_name) \
  mapping_allocator_user_info_t alloc_name ## _info = { (phys_page_allocator), (virt_mem_heap) }; \
  mem_allocator_t alloc_name = { mapping_allocator_alloc,		\
				 mapping_allocator_free,		\
				 &alloc_name ## _info }

/* Recording allocator */
typedef struct {
  uintptr_t *addrs_used;
  uint32_t spaces_left;
  void(*call_on_no_space)(mem_allocator_t*, void *);
  mem_allocator_t *underlying;
} recording_allocator_user_info_t;

void *recording_allocator_alloc(mem_allocator_t *alloc);
void recording_allocator_free(mem_allocator_t *alloc, void *to_free);

#define recording_allocator_get_used_count(alloc, n) ((n) - ((recording_allocator_user_info_t *) (alloc)->user_data)->spaces_left)

#define DECL_RECORDING_ALLOCATOR(underlying, addrs_used, spaces_left, call_on_no_space, name) \
  recording_allocator_user_info_t name ## _info = { (addrs_used), (spaces_left), (call_on_no_space), (underlying) }; \
  mem_allocator_t name = { recording_allocator_alloc, recording_allocator_free, &name ## _info }

/* Static allocator */
typedef struct {
  uintptr_t *ptrs;
  uint32_t ptr_count;
  void*(*call_on_no_more)(mem_allocator_t*);
} static_allocator_user_info_t;

void *static_allocator_alloc(mem_allocator_t *alloc);
void static_allocator_free(mem_allocator_t *alloc, void *to_free);

#define static_allocator_get_used_count(alloc, n) (n - ((static_allocator_user_info_t *) (alloc)->user_data)->ptr_count)

#define DECL_STATIC_PAGE_ALLOCATOR(ptrs, count, call_on_no_more, name)	\
  static_allocator_user_info_t name ## _info = {ptrs, count, call_on_no_more}; \
  mem_allocator_t name = {static_allocator_alloc, static_allocator_free, &name ## _info }

/* Arch specific */

void arch_map_page(mem_allocator_t *allocator, uintptr_t virt, uintptr_t phys);
uintptr_t arch_get_phys_page(uintptr_t virt);
uintptr_t arch_unmap_page(uintptr_t virt);
void arch_mark_user(uintptr_t virt);
void arch_unmap_init_task();

#endif

