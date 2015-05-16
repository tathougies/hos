#ifndef RTS_SUPPORT_H
#define RTS_SUPPORT_H

#include <setjmp.h>
#include "rts/cdefs.h"

extern jmp_buf jhc_uncaught;
A_UNUSED extern char *jhc_options_os;
A_UNUSED extern char *jhc_options_arch;
extern int jhc_argc;
extern char **jhc_argv;
extern char *jhc_progname;

extern char jhc_c_compile[];
extern char jhc_command[];
extern char jhc_version[];

void A_NORETURN A_UNUSED A_COLD jhc_exit(int n);
void A_NORETURN A_UNUSED A_COLD jhc_error(char *s);
void A_NORETURN A_UNUSED A_COLD jhc_case_fell_off(int n);

#define jhc_setjmp(jb) setjmp(*(jb))
#define jhc_longjmp(jb) longjmp(*(jb),1)

#define prim_umaxbound(t) ((t)~((t)0))
#define prim_maxbound(t) ((t)(~((t)1 << (sizeof(t)*CHAR_BIT - 1))))
#define prim_minbound(t) ((t)(((t)1 << (sizeof(t)*CHAR_BIT - 1))))

#endif
