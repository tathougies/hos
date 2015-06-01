#ifndef __stdio_H__
#define __stdio_H__

#include "stdlib.h"

typedef void *FILE;
#define stderr NULL

int fprintf(FILE *f, const char *fmt, ...);
size_t fwrite(const void * ptr, size_t size, size_t nitems, FILE * stream);

#endif
