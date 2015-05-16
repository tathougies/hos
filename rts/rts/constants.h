#ifndef RTS_CONSTANTS_H
#define RTS_CONSTANTS_H
/* these constants are shared between jhc-prim and the rts. */

// Normal memory block.
#define SLAB_FLAG_NONE         0

// Each element has a finalizer-list as its second word.
#define SLAB_FLAG_FINALIZER    1

// In addition to whatever other finalization is done, 'free' should be called
// on the first word of each entry.
#define SLAB_FLAG_FREE         2

// Finalizers should be delayed until entire slab is freed up and individually
// freed members need not be kept track of.
#define SLAB_FLAG_DELAY        4

// A global finalizer exists for this slab
#define SLAB_GLOBAL_FINALIZER  8

// slab is a monolith, should be 'free'd when done with and not returned to
// cache.
#define SLAB_MONOLITH          16

// virtual flags are never set in a cache but are used internally to keep track
// of things.

// virtual flag to indicate location is a value
#define SLAB_VIRTUAL_VALUE     256

// virtual flag to indicate location has a special intererpretation.
#define SLAB_VIRTUAL_SPECIAL   512

// virtual flag to indication location is a constant.
#define SLAB_VIRTUAL_CONSTANT  1024

// virtual flag to indication location has been freed. (for debugging)
#define SLAB_VIRTUAL_FREED     2048

// virtual flag to indication location is lazy.
#define SLAB_VIRTUAL_LAZY      4096

// virtual flag to indication location is func.
#define SLAB_VIRTUAL_FUNC      8192

#endif
