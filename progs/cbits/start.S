.global _start
_start:
	.set HOS_DEBUG_LOG, 0
	.set HOS_CURRENT_ADDRESS_SPACE, 1
	.set HOS_ADD_MAPPING, 2
	.set HOS_DELETE_MAPPING, 3
	.set HOS_CLOSE_ADDRESS_SPACE, 4
	.set HOS_SWITCH_TO_ADDRESS_SPACE, 5
	.set HOS_KILL_PROCESS, 0x400
	.set HOS_CURRENT_PROCESS, 0x401
	movq $HOS_CURRENT_ADDRESS_SPACE, %rax
	xorq %rdi, %rdi # We want the current address space for task id 0
	syscall

	mov %rax, %rdi # Address space is returned in %rax
	mov $HOS_ADD_MAPPING, %rax
	movq $0xfffffe8000000000, %rsi
	movq $0xffffff0000000000, %rdx
	movq $mappingInfo, %r8
	movb $0x1, (%r8) # allocate on demand
	movb $0x3, 1(%r8) # userspace readwrite

	syscall

	# Now switch the address space copy for our own address space
	movq $HOS_SWITCH_TO_ADDRESS_SPACE, %rax
	mov %rdi, %rsi
	mov $0, %rdi # Switch our own adress space (task id 0)
	syscall

	# Now delete the address space reference
	movq $HOS_CLOSE_ADDRESS_SPACE, %rax
	mov %rsi, %rdi
	syscall

	# Now that we have the mapping in place set the stack pointer
	mov $5, %rsi
	movabsq $0xffffff0000000000, %rsp

	movabsq $xmmWord, %rax
	movups (%rax), %xmm0
	movups (%rax), %xmm1
	movups (%rax), %xmm2
	movups (%rax), %xmm3
	movups (%rax), %xmm4
	movups (%rax), %xmm5
	movups (%rax), %xmm6
	movups (%rax), %xmm7
	movups (%rax), %xmm8
	movups (%rax), %xmm9
	movups (%rax), %xmm10
	movups (%rax), %xmm11
	movups (%rax), %xmm12
	movups (%rax), %xmm13
	movups (%rax), %xmm14
	movups (%rax), %xmm15

	movabsq $0xf0f0f0f0f0f0f001, %rax
	movabsq $0xf2f0f0f2f2f0f022, %rbx
	movabsq $0x3ff0f4f2f1f1f023, %rcx
	movabsq $0x3f000003f1f4f024, %rdx
	movabsq $0x3f000003f1f4f025, %rsi
	movabsq $0xfffffffffffffff5, %rdi
	movabsq $0xffffffff00000006, %rbp
	movabsq $0x0707070707070707, %r8
	movabsq $0x808080808080808a, %r9
	movabsq $0xdeadbeefdeadbeeb, %r10
	movabsq $0x111111111111111c, %r11
	movabsq $0xdd00dd00dd00dddd, %r12
	movabsq $0xeefbeefbeefbeefe, %r13
	movabsq $0xffffff0000000fff, %r14
	movabsq $0x1f1f1f1f1f1f1f11, %r15

	call hos_init_clear_bss
	call jhc_alloc_init
	call jhc_hs_init
	call _amain

	# Now we say bye bye to the world...
	movq $HOS_CURRENT_PROCESS, %rax
	syscall
	mov %rax, %rdi
	movq $HOS_KILL_PROCESS, %rax
	syscall

xmmWord:	.quad 0x0202020201010101
	.quad 0x0303030304040404

mappingInfo:	.skip 0x2

x64_syscall:
	.global x64_syscall
	# the c signature for amd64 is arg1, arg2, arg3, syscall_no, arg4, arg5,
	# so we just have to move the number into rax
	mov %rcx, %rax
	syscall
	ret
