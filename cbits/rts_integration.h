#ifndef __rts_integration_H__
#define __rts_integration_H__

#include "arch.h"
#include "internal.h"
#include "rts/gc_jgc_internal.h"

void *ext_page_aligned_alloc(size_t sz);
void *ext_page_aligned_realloc(void *ret, size_t sz);
void ext_free(void *ptr);
struct s_megablock *ext_alloc_megablock();
struct s_cache *ext_alloc_cache();
void arch_invalidate_page(uintptr_t ptr);

void jhc_case_fell_off(int line);
void jhc_error(char *s);

inline void jhc_utf8_putchar(int c) { };
void jhc_exit();
inline void ext_halt() { for (;;); };
inline uintptr_t ptrToWord(void *p) { return (uintptr_t) p; }
inline void *wordToPtr(uintptr_t p) { return (void *) p; }

inline double frexp(double x, int *exp)
{
  return __builtin_frexp(x, exp);
};
inline float frexpf(float x, int *exp)
{
  return __builtin_frexpf(x, exp);
};
inline double ldexp(double x, int exp)
{
  return __builtin_ldexp(x, exp);
};
inline double ceil(double x)
{
  return __builtin_ceil(x);
};
inline double log(double x)
{
  return __builtin_log(x);
};


#endif
