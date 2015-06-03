#ifndef __arch_H__
#define __arch_H__

#include <stdint.h>
#include <stddef.h>

#if TARGET == i386 || TARGET == x86_64

#define ARCH_PAGE_WIDTH 12
#define ARCH_PAGE_SIZE 4096

#if TARGET == x86_64
#define ARCH_WORD_WIDTH 64
#define REAL_MODE_ADDR(x) ((typeof(x)) (((uintptr_t) x) + 0xffffff7f80000000UL))

#define UPTR(x) ((uintptr_t) (x))
#define CUR_PT_ADDR(pml4_index, pdpt_index, pdt_index) ((uintptr_t *) (UPTR(0xffffff8000000000UL) + (UPTR(pml4_index) * 0x40000000UL) + (UPTR(pdpt_index) * 0x200000UL) + (UPTR(pdt_index) * 0x1000UL)))
#define CUR_PDT_ADDR(pml4_index, pdpt_index) CUR_PT_ADDR(511, pml4_index, pdpt_index)
#define CUR_PDPT_ADDR(pml4_index) CUR_PDT_ADDR(511, pml4_index)
#define CUR_PML4_ADDR CUR_PDPT_ADDR(511)

#define PT_ENTRY(x) (((x) >> 12) & 0x1ff)
#define PDT_ENTRY(x) (((x) >> 21) & 0x1ff)
#define PDPT_ENTRY(x) (((x) >> 30) & 0x1ff)
#define PML4_ENTRY(x) (((x) >> 39) & 0x1ff)

/* 128 MB is reserved for the tree structures that we use to manage physical memory */
#define ARCH_PHYS_MEMORY_MANAGEMENT_HEAP_BASE 0xffffff7f40000000

/* We reserve 8 virtual addresses for the miscellaneous copying among pages */
#define ARCH_MISC_PAGE_ADDR_1 (0xffffff7f7ffff000)
#define ARCH_MISC_PAGE_ADDR_2 (ARCH_MISC_PAGE_ADDR_1 - 0x1000)
#define ARCH_MISC_PAGE_ADDR_3 (ARCH_MISC_PAGE_ADDR_2 - 0x1000)
#define ARCH_MISC_PAGE_ADDR_4 (ARCH_MISC_PAGE_ADDR_3 - 0x1000)
#define ARCH_MISC_PAGE_ADDR_5 (ARCH_MISC_PAGE_ADDR_4 - 0x1000)
#define ARCH_MISC_PAGE_ADDR_6 (ARCH_MISC_PAGE_ADDR_5 - 0x1000)
#define ARCH_MISC_PAGE_ADDR_7 (ARCH_MISC_PAGE_ADDR_6 - 0x1000)
#define ARCH_MISC_PAGE_ADDR_8 (ARCH_MISC_PAGE_ADDR_7 - 0x1000)

#define ARCH_INVALIDATE_PAGE(x) do {		\
    asm("invlpg (%%rax)" : :  "a"(x));		\
  } while(0)

inline uint64_t x64ReadCR2() {
  uint64_t ret;
  asm ( "mov %%cr2, %%rax" : "=a"(ret) );
  return ret;
}

inline void x64WriteCR3(uint64_t cr3) {
  asm ( "mov %%rax, %%cr3" : : "a"(cr3));
}

void setupSysCalls();
extern int curUserSpaceState;
extern int kernelState;
extern uint64_t x64TrapErrorCode;
int x64SwitchToUserspace(void *newState, void *oldState);

void trap0();
void trap1();
void trap2();
void trap3();
void trap4();
void trap5();
void trap6();
void trap7();
void trap8();
void trap9();
void trap10();
void trap11();
void trap12();
void trap13();
void trap14();
void trap15();
void trap16();
void trap17();
void trap18();
void trap19();
void trap20();
void trap21();
void trap22();
void trap23();
void trap24();
void trap25();
void trap26();
void trap27();
void trap28();
void trap29();
void trap30();
void trap31();
extern int tssArea;
extern int kernelTmpStack;
extern int kernelTmpStack_top;
extern int kernelFaultStack;
extern int kernelFaultStack_top;
extern int tssArea;
extern int iopb;
#ifdef COMPILING_HS_KERNEL
extern int g_module_count;
extern int g_mboot_modules; /* Wrong type just for Haskell import */
#endif

inline void loadIdt(void *idtP) { asm ("lidt (%%rax)" : : "a"(idtP)); };

#else
#define ARCH_WORD_WIDTH 32
#endif

#ifndef NULL
#define NULL ((void *) 0)
#endif


#define ARCH_LOWEST_CLEAR_BIT(b) (__builtin_ffsll((unsigned long long)(~b)) - 1)
#define ARCH_LOWEST_SET_BIT(b) (__builtin_ffsll(b) - 1)
#define ARCH_HIGHEST_SET_BIT(b) ((b) == 0 ? ARCH_WORD_WIDTH : 63 - __builtin_clzll((unsigned long long) (b)))
#endif

#define PAGE_OFFSET_MASK (ARCH_PAGE_SIZE - 1)
#define PAGE_ADDR(a) (typeof(a))(((uintptr_t) (a)) & ~PAGE_OFFSET_MASK)

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

#endif
