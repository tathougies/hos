#ifndef __util_H__
#define __util_H__

#include <stdint.h>
#include <stdbool.h>

#define SIZEOF_MEMBER(type_name, member) sizeof(((type_name *) 0)->member)
#define ROUND_UP(x, divisor) ((x + divisor - 1) / divisor)

#define TRUE 1
#define FALSE 0

#define A_PACKED __attribute__((packed))
#define P(p) ((uintptr_t) (p))

#endif
