#ifndef BITARRAY_H
#define BITARRAY_H

#include <limits.h>
#include <stdbool.h>

typedef unsigned long bitarray_t;

#define BITS_PER_UNIT (bitarray_t)(CHAR_BIT*sizeof(bitarray_t))
#define BITARRAY_SIZE(bits) (((bits) + (BITS_PER_UNIT - 1)) / BITS_PER_UNIT)
#define BITARRAY_SIZE_IN_BYTES(bits) (sizeof(bitarray_t)*BITARRAY_SIZE(bits))

#define WHICH_BIT(bit)  \
    (1UL << ((((bitarray_t)(bit)) % BITS_PER_UNIT)))

#define OFFSET_IN_ARRAY(array,bit) \
    (((bitarray_t *)(array))[((bitarray_t)(bit)) / BITS_PER_UNIT])

#define BIT_IS_SET(array,bit)  \
    (OFFSET_IN_ARRAY(array,bit) & WHICH_BIT(bit))

#define BIT_IS_UNSET(array,bit) \
    (!(BIT_IS_SET(array,bit)))

#define BIT_SET(array,bit) \
    (OFFSET_IN_ARRAY(array,bit) |= WHICH_BIT(bit))

#define BIT_UNSET(array,bit) \
    (OFFSET_IN_ARRAY(array,bit) &= ~WHICH_BIT(bit))

#define BIT_TOGGLE(array,bit) \
    (OFFSET_IN_ARRAY(array,bit) ^= WHICH_BIT(bit))

#define BIT_COPY(dest,src,bit)  \
    do { BIT_IS_SET((src),(bit)) ? BIT_SET((dest),(bit)) : BIT_UNSET((dest),(bit)) } while(0)

#define BIT_VALUE(array,bit) \
    (BIT_IS_SET((array),(bit)) ? true : false)

#define BIT_SET_VALUE(array,bit,value) \
    do { (value) ? BIT_SET((array),(bit)) : BIT_UNSET((array),(bit)) } while(0)

#endif
