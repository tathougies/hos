#ifndef JHC_RTS_H
#define JHC_RTS_H

#include "rts/profile.h"
#include "rts/gc.h"

struct sptr;
struct wptr;
struct fptr;

// we use dummy structs here so the compiler will catch any attempt
// to use one type in anothers place
typedef struct sptr *sptr_t;
typedef struct sptr *wptr_t;
typedef struct fptr *fptr_t;
typedef uintptr_t     what_t;

typedef struct node {
        fptr_t head;
        sptr_t rest[];
} A_MAYALIAS node_t;

typedef struct dnode {
        what_t what;
        sptr_t rest[];
} A_MAYALIAS dnode_t;

#define P_WHNF  0x0
#define P_LAZY  0x1
#define P_VALUE 0x2
#define P_FUNC  0x3

#define IS_LAZY(x)     (bool)(((uintptr_t)(x)) & 0x1)
#define IS_PTR(x)      (bool)(!(((uintptr_t)(x)) & 0x2))

#define FROM_SPTR(x)   (typeof (x))((uintptr_t)(x) & ~0x3)  // remove a ptype from a smart pointer
#define GET_PTYPE(x)   ((uintptr_t)(x) & 0x3)               // return the ptype associated with a smart pointer
#define TO_SPTR(t,x)   (typeof (x))((uintptr_t)(x) | (t))   // attach a ptype to a smart pointer
#define TO_SPTR_C(t,x) (typeof (x))((uintptr_t)(x) + (t))   // attach a ptype to a smart pointer, suitable for use by constant initialializers

#define GETHEAD(x)   (NODEP(x)->head)
#define NODEP(x)     ((node_t *)(x))
#define DNODEP(x)    ((dnode_t *)(x))

#define MKLAZY(fn)    TO_SPTR(P_LAZY,(sptr_t)fn)
#define MKLAZY_C(fn)  TO_SPTR_C(P_LAZY,(sptr_t)fn)
#define TO_FPTR(fn)   TO_SPTR_C(P_FUNC,(fptr_t)fn)

#define RAW_SET_F(n)   ((wptr_t)(((intptr_t)(n) << 2) | P_VALUE))
#define RAW_SET_UF(n)  ((wptr_t)(((uintptr_t)(n) << 2) | P_VALUE))
#define RAW_GET_F(n)   ((intptr_t)(n) >> 2)
#define RAW_GET_UF(n)  ((uintptr_t)(n) >> 2)

#define RAW_SET_16(w)  (wptr_t)(((uintptr_t)(w) << 16) | P_VALUE)
#define RAW_GET_16(n)  ((intptr_t)(n) >> 16)
#define RAW_GET_U16(n) ((uintptr_t)(n) >> 16)

// demote is always safe, we must only promote when we know the argument is a WHNF
#define PROMOTE(n)   ((wptr_t)(n))
#define DEMOTE(n)    ((sptr_t)(n))

#define FETCH_TAG(x)      RAW_GET_U16(IS_PTR(x) ? FETCH_MEM_TAG(x) : (what_t)(x))
#define FETCH_RAW_TAG(x)  RAW_GET_U16(x)
#define SET_RAW_TAG(x)    RAW_SET_16(x)
#define FETCH_MEM_TAG(x)  (DNODEP(x)->what)
#define SET_MEM_TAG(x,v)  (DNODEP(x)->what = (what_t)RAW_SET_16(v))

#define BLACK_HOLE TO_FPTR(0xDEADBEE0)

wptr_t A_STD
#if _JHC_GC == _JHC_GC_JGC
eval(gc_t gc, sptr_t s);
#else
eval(sptr_t s);
#endif

// both promote and demote evaluate to nothing when debugging is not enabled
// otherwise, they check that their arguments are in the correct form.
#if _JHC_DEBUG
wptr_t A_STD promote(sptr_t s);
sptr_t A_STD demote(wptr_t s);
void   A_STD update(void *, wptr_t);
#else
#define promote(x) PROMOTE(x)
#define demote(x) DEMOTE(x)
inline static void update(void *t, wptr_t n)
{
        GETHEAD(t) = (fptr_t)n;
}
#endif

#if _JHC_DEBUG && _JHC_GC == _JHC_GC_JGC
bool jhc_valid_whnf(wptr_t s);
bool jhc_valid_lazy(sptr_t s);
#else
#define jhc_valid_whnf(x) true
#define jhc_valid_lazy(x) true
#endif

#endif

/*
 * Detail:
 * http://communities.mentor.com/community/cs/archives/arm-gnu/msg01904.html
 */
#ifdef _JHC_ARM_STAY_IN_THUMB_MODE
#define SET_THUMB_BIT(fn)    TO_SPTR(0x1,(sptr_t)fn)
#else
#define SET_THUMB_BIT(fn)    (fn)
#endif
