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

#endif
