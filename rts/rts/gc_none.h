#ifndef GC_NONE_H
#define GC_NONE_H

#include <stddef.h>
#include "rts/cdefs.h"

#define jhc_malloc_sanity(p,t) (1)

#if _JHC_GC == _JHC_GC_BOEHM

#include "rts/profile.h"

#include <gc/gc.h>

#define jhc_malloc GC_malloc
#define jhc_malloc_atomic GC_malloc_atomic

#elif _JHC_GC == _JHC_GC_NONE

#if _JHC_DEBUG
void *A_MALLOC jhc_malloc_debug(size_t n, int line, int atomic);
#define jhc_malloc(n) jhc_malloc_debug(n,__LINE__,0)
#define jhc_malloc_atomic(n) jhc_malloc_debug(n,__LINE__,1)
#else
void *A_MALLOC jhc_malloc(size_t n);
void *A_MALLOC jhc_malloc_atomic(size_t n);
#endif

#endif

#endif
