#ifndef __hos_H__
#define __hos_H__

#include <stdint.h>
#include <stddef.h>

#define LACKS_TIME_H 1
#define LACKS_STRIGS_H 1
#define LACKS_STRING_H 1
#define LACKS_FCNTL_H 1
#define LACKS_UNISTD_H 1
#define HAVE_MMAP 0
#define NO_MALLOC_STATS 1
#define MALLOC_FAILURE_ACTION abort()

#if TARGET == i386 || TARGET == x86_64
#define EXEC_PAGESIZE 0x1000
#endif

#if TARGET == x86_64
/* We have tons of space on a 64-bit architecture, so just start the heap at the 512th gigabyte */
#define HEAP_START 0x8000000000
#define HOS_IS_64_BIT 1
#elif TARGET == i386
#define HOS_IS_32_BIT
#endif

typedef uintptr_t size_t;

/* Some shims for string.h */
//void *memcpy(void *dest, const void *src, size_t n);
//void *memset(void *s, int c, unsigned long n);
void abort();
void hos_init_clear_bss();

/* JHC integration */
inline static int jhc_utf8_putchar(int ch)
{ return 0; }

inline static void jhc_exit(int n)
{ abort(); }

inline static void jhc_case_fell_off(int n)
{ abort() ; }

inline void *word_to_ptr(uintptr_t i)
{
  return (void*) i;
}

inline uintptr_t ptr_to_word(void *i)
{
  return (uintptr_t) i;
}

#endif
