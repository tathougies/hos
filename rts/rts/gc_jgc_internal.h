#ifndef GC_JGC_INTERNAL_H
#define GC_JGC_INTERNAL_H

#include "rts/gc_jgc.h"
#include "sys/bitarray.h"
#include "sys/queue.h"

#if _JHC_GC == _JHC_GC_JGC

struct s_arena {
        struct s_megablock *current_megablock;
        SLIST_HEAD(, s_block) free_blocks;
        unsigned block_used;
        unsigned block_threshold;
        SLIST_HEAD(, s_cache) caches;
        SLIST_HEAD(, s_block) monolithic_blocks;
        SLIST_HEAD(, s_megablock) megablocks;
        unsigned number_gcs;    // number of garbage collections
        unsigned number_allocs; // number of allocations since last garbage collection
};

struct s_megablock {
        void *base;
        unsigned next_free;
        SLIST_ENTRY(s_megablock) next;
};

struct s_block {
        SLIST_ENTRY(s_block) link;
        unsigned char flags;  // defined in rts/constants.h
        unsigned char color;  // offset in words to first entry.
        union {
                // A normal block.
                struct {
                        unsigned char num_ptrs;
                        unsigned char size;
                        unsigned short num_free;
                        unsigned short next_free;
                } pi;
                // A monolithic block.
                struct {
                        unsigned num_ptrs;
                } m;
        } u;
        bitarray_t used[];
};

struct s_cache {
        SLIST_ENTRY(s_cache) next;
        SLIST_HEAD(, s_block) blocks;
        SLIST_HEAD(, s_block) full_blocks;
        unsigned char color;
        unsigned char size;
        unsigned char num_ptrs;
        unsigned char flags;
        unsigned short num_entries;
        struct s_arena *arena;
#if _JHC_PROFILE
        unsigned allocations;
#endif
};
#endif
#endif
