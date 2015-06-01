#ifndef __stdlib_H__
#define __stdlib_H__

#include <stddef.h>
#undef abort
#undef assert

#include "syscall.h"

#define assert(x) do {							\
    if ( !(x) ) { hos_debug_log("Assert failed at " __FILE__); }	\
  } while (0)

#endif
