
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

#define PRINT_AROUND_ARRAY(a, i, nel, ctxt, fmt)			\
  do {									\
    int start = MAX(0, ((int) i) - (int) ctxt);				\
    int end = MIN((int) nel, ((int)i) + (int)ctxt);			\
    int ix;								\
    if (start != 0) fprintf(stderr, "... ");				\
    for ( ix = start; ix < end; ++ix) {					\
      fprintf(stderr, " %s" fmt "%s", ix == i ? ">>" : "", a[ix], ix == i ? "<<" : ""); \
    }									\
    if (end != nel ) fprintf(stderr, " ...");				\
  } while (0);

#define TEST_ARRAY_EQUAL(type, fmt, what, act, exp, nel)	\
  do {								\
    int i = 0;							\
    for (; i < nel; ++i) {					\
      type act_el = act[i], exp_el = exp[i];			\
      if (act_el != exp_el) {						\
        fprintf(stderr, "TEST_ARRAY_EQUAL: " what " are not equal at position %d\n", i); \
	fprintf(stderr, "   Expected: {");				\
	PRINT_AROUND_ARRAY(exp, i, nel, 2, fmt);			\
	fprintf(stderr, "}\n   Actual: {");				\
	PRINT_AROUND_ARRAY(act, i, nel, 2, fmt);			\
	fprintf(stderr, "}\n");						\
	exit(1);							\
      }									\
    }									\
  } while (0)

#define TEST_EQUAL(what, fmt, act, exp)					\
  if ((act) != (exp)) {							\
    fprintf(stderr, "TEST_EQUAL:%s:%d:" what " is not what is expected\n", __FILE__, __LINE__); \
    fprintf(stderr, "   Expected: " fmt "\n", (exp));			\
    fprintf(stderr, "   Actual: " fmt "\n", (act));			\
    exit(1);								\
  }

#define VA_ARGS(...) , ## __VA_ARGS__
#define GENERIC_ASSERT(cond, what, ...)		   \
  if (!(cond)) {				   \
    fprintf(stderr, "GENERIC_ASSERT:%s:%d:" what "\n", __FILE__, __LINE__ VA_ARGS(__VA_ARGS__)); \
    exit(1);					   \
  }

void test_monotonic(const char *what, uintptr_t *ptrs, size_t nel);
