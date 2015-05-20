#include "syscall.h"

void syscall(int i)
{
  asm ("syscall" : : "a"(i));
}
