#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "memory.h"

#include "test/test.h"

#define TEST_SIZE 10000

typedef struct my_list_t {
  char c;
  struct my_list_t *next;
} my_list_t;

DECL_MEMPOOL_TYPE(my_list_t);
DECL_MALLOC_ALLOCATOR(stdalloc, POOL_PAGE_T(my_list_t));

MEMPOOL_IMPL(my_list_t);

void all_pointers_unique()
{
  /* Test: all allocated pointers are unique */
  int i;
  POOL_T(my_list_t) list_pool = POOL_INITIALIZER(my_list_t);
  my_list_t **ptrs = malloc(sizeof(my_list_t*) * TEST_SIZE);

  for (i = 0; i < TEST_SIZE; ++i)
    ptrs[i] = POOL_ALLOC(&list_pool, &stdalloc, my_list_t);

  test_monotonic("allocated ptrs", (uintptr_t *) ptrs, TEST_SIZE);
}

void mark_free_and_used()
{
  /* Test: pool_mark_used and pool_mark_free work correctly */
  static const uint8_t exp_bmp[10] = {0x3, 0x22, 0, 0, 0,
				      0, 0, 0, 0, 8};
  uint8_t bmp[10];
  size_t i;
  void *ptr;

  memset(bmp, 0, sizeof(bmp));

  ptr = pool_find_free_ptr(&i, bmp, 0xdead0000, 80, 1);
  TEST_EQUAL("find_free_ptr returned pointer", "%p", ptr, (void *) 0xdead0000);
  TEST_EQUAL("find_free_ptr returned index", "%zu", i, ((size_t) 0));

  pool_mark_used(bmp, 0, 80);
  pool_mark_used(bmp, 1, 80);
  TEST_EQUAL("free/used bitmap entry", "0x%x", bmp[0], 3);

  ptr = pool_find_free_ptr(&i, bmp, 0xdead0000, 80, 1);
  TEST_EQUAL("find_free_ptr returned pointer", "%p", ptr, (void *) 0xdead0002);
  TEST_EQUAL("find_free_ptr returned index", "%zu", i, ((size_t) 2));

  pool_mark_used(bmp, 9, 80);
  TEST_EQUAL("free/used bitmap (byte 2)", "0x%x", bmp[1], 0x2);
  pool_mark_used(bmp, 13, 80);
  TEST_EQUAL("free/used bitmap (byte 2)", "0x%x", bmp[1], 0x22);
  pool_mark_used(bmp, 80, 80);
  pool_mark_used(bmp, 75, 80);
  TEST_ARRAY_EQUAL(uint8_t, "0x%x", "free/used bitmaps", bmp, exp_bmp, 10);

  pool_mark_free(bmp, 0, 80);
  TEST_EQUAL("free/used bitmap entry", "0x%x", bmp[0], 2);
  pool_mark_free(bmp, 1, 80);
  TEST_EQUAL("free/used bitmap entry", "0x%x", bmp[0], 0);

  pool_mark_free(bmp, 13, 80);
  TEST_EQUAL("free/used bitmap entry", "0x%x", bmp[1], 2);
  pool_mark_free(bmp, 9, 80);
  TEST_EQUAL("free/used bitmap entry", "0x%x", bmp[1], 0);
}

void test_case1()
{
  uint8_t bitmap = {0xbf, 0xff};
}

int main(int argc, char **argv)
{
  assert(ARCH_LOWEST_CLEAR_BIT(0) == 0);
  assert(ARCH_LOWEST_CLEAR_BIT(1) == 1);
  assert(ARCH_LOWEST_CLEAR_BIT(2) == 0);
  assert(ARCH_LOWEST_CLEAR_BIT(3) == 2);
  assert(ARCH_LOWEST_CLEAR_BIT(7) == 3);
  assert(ARCH_LOWEST_CLEAR_BIT(15) == 4);
  assert(ARCH_LOWEST_CLEAR_BIT(31) == 5);
  assert(ARCH_LOWEST_CLEAR_BIT(63) == 6);
  assert(ARCH_LOWEST_CLEAR_BIT(127) == 7);

  printf("test %d %d\n", ARCH_LOWEST_SET_BIT(0xbf), ARCH_HIGHEST_SET_BIT(0xbf));
  assert(ARCH_LOWEST_CLEAR_BIT(0xbf) == 6);

  mark_free_and_used();
  all_pointers_unique();

  test_case1();
}
