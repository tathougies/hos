/* HsFFI.h for jhc */

#ifndef _JHC_HSFFI_H
#define _JHC_HSFFI_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

typedef int32_t HsInt;
typedef int8_t  HsInt8;
typedef int16_t HsInt16;
typedef int32_t HsInt32;
typedef int64_t HsInt64;

typedef uint32_t HsWord;
typedef uint8_t  HsWord8;
typedef uint16_t HsWord16;
typedef uint32_t HsWord32;
typedef uint64_t HsWord64;

typedef wchar_t HsChar;
typedef bool HsBool;

typedef double HsDouble;
typedef float HsFloat;

typedef void *HsPtr;
typedef void (*HsFunPtr)(void);
typedef void *HsStablePtr;

#define HS_BOOL_FALSE 0
#define HS_BOOL_TRUE 1

void hs_init (int *argc, char **argv[]);
void hs_exit (void);
void hs_set_argv(int argc, char *argv[]);
void hs_perform_gc(void);
void hs_free_stable_ptr(HsStablePtr sp);
void hs_free_fun_ptr(HsFunPtr fp);

#endif
