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

static inline uint16_t arch_in_16(uint16_t port) {
  uint16_t ret;
  asm("inw %%dx, %%ax" : "=a"(ret) : "d" (port));
  return ret;
}

static inline void arch_out_16(uint16_t port, uint16_t l) {
  asm("outw %%ax, %%dx" : : "a"(l), "d"(port));
}

static inline uint8_t arch_in_8(uint16_t port) {
  uint8_t ret;
  asm("inb %%dx, %%al" : "=a"(ret) : "d" (port));
  return ret;
}

static inline void arch_out_8(uint16_t port, uint8_t l) {
  asm("outb %%al, %%dx" : : "a"(l), "d"(port));
}

#endif
