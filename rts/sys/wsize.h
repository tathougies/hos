#ifndef WSIZE_H
#define WSIZE_H

/*
 * wsize.h
 * define appropriate __WORDSIZE and __BYTE_ORDER macros
 *
 * always use operating systems headers rather than checking for architectures
 * when possible. if adding new cases. Checking the CPU type should be a last
 * resort.
 *
 */

#include <limits.h>

#ifdef __linux__
#include<endian.h>
#endif

#ifndef __LITTLE_ENDIAN
#define	__LITTLE_ENDIAN	1234
#endif
#ifndef __BIG_ENDIAN
#define	__BIG_ENDIAN	4321
#endif
#ifndef __PDP_ENDIAN
#define	__PDP_ENDIAN	3412
#endif

#ifndef __BYTE_ORDER
#ifdef _BIG_ENDIAN
#define __BYTE_ORDER __BIG_ENDIAN
#elif defined(__BIG_ENDIAN__)
#define __BYTE_ORDER __BIG_ENDIAN
#elif defined(_LITTLE_ENDIAN)
#define __BYTE_ORDER __LITTLE_ENDIAN
#elif defined(__LITTLE_ENDIAN__)
#define __BYTE_ORDER __LITTLE_ENDIAN
#elif defined(__i386__)
#define __BYTE_ORDER __LITTLE_ENDIAN
#else
#error Could not determine Byte Order
#endif
#endif

#ifndef __WORDSIZE
#ifdef __SIZEOF_POINTER__
#define __WORDSIZE (CHAR_BIT*__SIZEOF_POINTER__)
#elif defined(__i386__)
#define __WORDSIZE 32
#elif defined(__x86_64__)
#define __WORDSIZE 64
#else
#error Could not determine bitsize
#endif
#endif

#ifdef TEST_WSIZE
#include <stdio.h>
int
main(int argc, char *argv[])
{
    printf("__WORDSIZE:   %i\n", __WORDSIZE);
    printf("__BYTE_ORDER: %i\n", __BYTE_ORDER);
    return 0;
}
#endif

#endif
