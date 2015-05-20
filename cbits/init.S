# Main init process entry
.global _start
_start:	nop
	nop
	nop
	movq %rax, 0
	syscall
