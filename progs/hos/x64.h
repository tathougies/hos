#ifndef __user_x64_H__
#define __user_x64_H__

static inline uint32_t arch_in_32(uint16_t port) {
  uint32_t ret;
  asm("inl %%dx, %%eax" : "=a"(ret) : "d" (port));
  return ret;
}

static inline void arch_out_32(uint16_t port, uint32_t l) {
  asm("outl %%eax, %%dx" : : "a"(l), "d"(port));
}

#endif
