#include <stdio.h>
#include <stdlib.h>

#include "test/test.h"

int kernel_heap_start;

void die(const char *x)
{
  fprintf(stderr, "die(): %s\n", x);
  exit(1);
}

static int compare_uintptr(void *a, void *b) {
  uintptr_t _a = *(uintptr_t *)a, _b = *(uintptr_t *)b;
  if ( _a < _b ) return -1;
  else if ( _a > _b ) return 1;
  else return 0;
}

void test_monotonic(const char *what, uintptr_t *ptrs, size_t nel)
{
  size_t i;
  uintptr_t last;

  qsort(ptrs, nel, sizeof(uintptr_t), compare_uintptr);

  for ( i = 0, last = 0; i < nel && ptrs[i] > last; last = ptrs[i++] );
  if ( i != nel ) {
    fprintf(stderr, "test_monotonic: %s is not monotonic at position %zu!\n", what, i);
    fprintf(stderr, "  the array looks like {");
    PRINT_AROUND_ARRAY(ptrs, i, nel, 2, "0x%zx");
    fprintf(stderr, "}\n");
    exit(1);
  }
}
