#ifndef RTS_PROFILE_H
#define RTS_PROFILE_H

#ifndef _JHC_BAREBONES
#include <stdio.h>
#endif
#include "rts/cdefs.h"

#ifndef JHC_VALGRIND
#define JHC_VALGRIND 0
#endif

#ifndef JHC_MEM_ANNOTATE
#define JHC_MEM_ANNOTATE 0
#endif

#ifndef _JHC_PROFILE
#define _JHC_PROFILE 0
#endif

#if JHC_VALGRIND
#include <valgrind/valgrind.h>
#include <valgrind/memcheck.h>
#else
#define VALGRIND_MAKE_MEM_UNDEFINED(x,y) \
    do { } while (0)
#define VALGRIND_MAKE_MEM_DEFINED(x,y) \
    do { } while (0)
#define VALGRIND_MAKE_MEM_NOACCESS(x,y) \
    do { } while (0)
#define VALGRIND_PRINTF(...) \
    do { } while (0)
#endif

#ifndef _JHC_BAREBONES
void A_UNUSED profile_print_header(FILE *file, char *value_unit);
void A_COLD jhc_print_profile(void);
#endif

#if _JHC_PROFILE
struct profile_stack;
extern struct profile_stack gc_alloc_time;
extern struct profile_stack gc_gc_time;
void jhc_profile_push(struct profile_stack *ps);
void jhc_profile_pop(struct profile_stack *ps);
#define profile_push(x) jhc_profile_push(x)
#define profile_pop(x)  jhc_profile_pop(x)
#else
#define profile_push(x)          do { } while(0)
#define profile_pop(x)           do { } while(0)
#define alloc_count(x,y)         do { } while(0)
#define print_alloc_size_stats() do { } while(0)
#endif

#if JHC_STATUS > 1
#define debugf(...) fprintf(stderr,__VA_ARGS__)
#else
#define debugf(...) do { } while (0)
#endif

#endif
