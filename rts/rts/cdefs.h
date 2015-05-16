#ifndef RTS_CDEFS_H
#define RTS_CDEFS_H

// GNU attributes
#if !defined(__predict_true)
#ifdef __GNUC__
#  define __predict_true(exp)     __builtin_expect(!!(exp), 1)
#  define __predict_false(exp)    __builtin_expect(!!(exp), 0)
#else
#  define __predict_true(exp)     (exp)
#  define __predict_false(exp)    (exp)
#endif
#endif

#ifdef __GNUC__
#define A_ALIGNED  __attribute__ ((aligned))
#define A_CONST    __attribute__ ((const))
#define A_MALLOC   __attribute__ ((malloc))
#define A_MAYALIAS __attribute__ ((__may_alias__))
#define A_NORETURN __attribute__ ((noreturn))
#define A_PURE     __attribute__ ((pure))
#define A_UNUSED   __attribute__ ((unused))
#ifdef __i386__
#define A_REGPARM __attribute__ ((fastcall))
#else
#define A_REGPARM
#endif
#define A_STD    A_REGPARM

#else
#define A_ALIGNED
#define A_CONST
#define A_MALLOC
#define A_MAYALIAS
#define A_NORETURN
#define A_PURE
#define A_UNUSED
#define A_STD
#endif

// these should be enabled with newer versions of gcc
#define A_HOT
#define A_COLD
#define A_FALIGNED

#endif
