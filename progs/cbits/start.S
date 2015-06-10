.global _start
_start:
	.set HOS_DEBUG_LOG, 0
	.set HOS_CURRENT_ADDRESS_SPACE, 1
	.set HOS_ADD_MAPPING, 2
	.set HOS_DELETE_MAPPING, 3
	.set HOS_CLOSE_ADDRESS_SPACE, 4
	.set HOS_SWITCH_TO_ADDRESS_SPACE, 5
	.set HOS_KILL_TASK, 0x400
	.set HOS_CURRENT_TASK, 0x401
	.align 0x1000

	mov $HOS_ADD_MAPPING, %rax
	movq $0xffffffff, %rdi
	movq $0xfffffe8000000000, %rsi
	movq $0xffffff0000000000, %rdx
	movq $mappingInfo, %r8
	movb $0x1, (%r8) # allocate on demand
	movb $0x3, 1(%r8) # userspace readwrite
	syscall

	# Now that we have the mapping in place set the stack pointer
	movabsq $0xffffff0000000000, %rsp
	pushq %rax
	pushq %rbx

	call hos_init_clear_bss

	call jhc_alloc_init
	call jhc_hs_init
	call _amain

	# Now we say bye bye to the world...
	movq $HOS_CURRENT_TASK, %rax
	syscall
	mov %rax, %rdi
	movq $HOS_KILL_TASK, %rax
	syscall

failedToKill:
	jmp failedToKill


x64_syscall:
	.global x64_syscall
	# the c signature for amd64 is arg1, arg2, arg3, syscall_no, arg4, arg5,
	# so we just have to move the number into rax
	mov %rcx, %rax
	syscall
	ret

mappingInfo:
	.section .data
	.skip 0x2
