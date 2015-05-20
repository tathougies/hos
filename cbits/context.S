# Contains assembler code for context switching. Called directly from Haskell-land.
.section .data
xmmNextState:	.quad 0
xmmPrevState:	.quad 0

.section .bss
curUserSpaceState:
	.skip 216 # 18 registers + 512-bit fxsave area + 1 quad word reason
		  # indicator
kernelState:
	.skip 208 # 18 registers  + 512-bit fxsave area
.global kernelState
.global curUserSpaceState

.align 16
.global kernelTmpStack
.global kernelTmpStack_top
.global kernelFaultStack
.global kernelFaultStack_top
kernelTmpStack:
	.skip 2048
kernelTmpStack_top:
kernelFaultStack:
	.skip 4096
kernelFaultStack_top:

.section .text
# We are called here from an interrupt. The interrupt number is on the stack,
# and after it, the stuff for the iret instruction.
#
# We're going first check to see if we're being called from userspace or from
# within the kernel. If from within the kernel, and the interrupt number corresponds
# to a fault, we will call the C instrumentation functions to report the error.
#
# If from userspace, then we will switch to the Haskell stack, and fake a "return"
# value for the interrupt number being called.
serviceISR:
	.set SYSCALL_REASON, 1
	.set TRAP_REASON, 2
	.set IRQ_REASON, 3

	pushq %rax
	pushq %rbx

	movabsq $curUserSpaceState, %rax

	# Load the interrupt/trap number into rbx
	# If bit 8 is set, then this is an IRQ. Otherwise, it's a trap
	movq 0x10(%rsp), %rbx
	test $0x100, %rbx
	jz itsATrap
	movq $IRQ_REASON, (%rax)
	jmp continueServiceISR
itsATrap:
	movq $TRAP_REASON, (%rax)
continueServiceISR:
	addq $0x8, %rax
	popq %rbx

	# now save user space state
	movq %rbx, (RBX_I * 8)(%rax)
	movq %rcx, (RCX_I * 8)(%rax)
	movq %rdx, (RDX_I * 8)(%rax)
	movq %rax, %rbx
	popq %rax
	movq %rax, (RAX_I * 8)(%rbx)

	movq %rsi, (RSI_I * 8)(%rbx)
	movq %rdi, (RDI_I * 8)(%rbx)
	movq %r8, (R8_I * 8)(%rbx)
	movq %r9, (R9_I * 8)(%rbx)
	movq %r10, (R10_I * 8)(%rbx)
	movq %r11, (R11_I * 8)(%rbx)
	movq %r12, (R12_I * 8)(%rbx)
	movq %r13, (R13_I * 8)(%rbx)
	movq %r14, (R14_I * 8)(%rbx)
	movq %r15, (R15_I * 8)(%rbx)

	# Since we came from a syscall, rcx contains the rip to return to, and
	# r11 the old rflags
	movq %rcx, (RIP_I * 8)(%rbx)
	movq %rsp, (RSP_I * 8)(%rbx)
	movq %rbp, (RBP_I * 8)(%rbx)
	movq %r11, (RFLAGS_I * 8)(%rbx)

	# TODO save xmm state

	# Pop the interrupt number from rax and call restoreKernelState
	popq %rax
	jmp restoreKernelState

# We can switch contexts by loading the temp userspace stack,
# changing the registers, and then far returning to userspace.
#
# We also will save the current rsp value. When an interrupt is received, the
# CPU will set rsp to the ISR stack pointer. This stack is only used to service
# ISRs. The serviceISR function will switch stacks to the Haskell stack, and
# that's the one that will be used to execute code in kernel-land.
#
# Haskell never returns an interrupt (IRQ or INT instruction) will set the correct RSP
# for processing
.global x64SwitchToUserspace
x64SwitchToUserspace:
	.set RAX_I, 0
	.set RBX_I, 1
	.set RCX_I, 2
	.set RDX_I, 3
	.set RSI_I, 4
	.set RDI_I, 5
	.set R8_I,  6
	.set R9_I,  7
	.set R10_I, 8
	.set R11_I, 9
	.set R12_I, 10
	.set R13_I, 11
	.set R14_I, 12
	.set R15_I, 13
	.set RIP_I, 14
	.set RSP_I, 15
	.set RBP_I, 16
	.set RFLAGS_I, 17

	.set FXSAVE_I, 18
	# Assumptions: Called when the kernel wants to restore the state of a
	# userspace task and give control back to userspace.
	# Called using x86_64 abi with two arguments, so RDI points to a struct
	# containing the userspace state and RSI points to a place where
	# we should save the current state.

	# First save all the registers in the %RSI structure
	movq %rax, (RAX_I * 8)(%rsi)
	movq %rbx, (RBX_I * 8)(%rsi)
	movq %rcx, (RCX_I * 8)(%rsi)
	movq %rdx, (RDX_I * 8)(%rsi)
	movq %rsi, (RSI_I * 8)(%rsi)
	movq %rdi, (RDI_I * 8)(%rsi)
	movq %r8, (R8_I * 8)(%rsi)
	movq %r9, (R9_I * 8)(%rsi)
	movq %r10, (R10_I * 8)(%rsi)
	movq %r11, (R11_I * 8)(%rsi)
	movq %r12, (R12_I * 8)(%rsi)
	movq %r13, (R13_I * 8)(%rsi)
	movq %r14, (R14_I * 8)(%rsi)
	movq %r15, (R15_I * 8)(%rsi)
	# We skip rip, since we don't really need to ever return to *this* function
	movq %rsp, (RSP_I * 8)(%rsi)
	movq %rbp, (RBP_I * 8)(%rsi)
	# We skip rflags, since the caller expects this to be convoluted after a
	# function call

	# Now save xmm state
	# leaq (XMM_I * 8)(%rsi), %rax
	# fxsave (%rax)

	# We're going to use rax to store the means by which we will exit
	# userspace
	mov (%rdi), %rax

	# Now restore the user state
	addq $0x8, %rdi
	movq (RAX_I * 8)(%rdi), %rax
	movq (RBX_I * 8)(%rdi), %rbx
	movq (RCX_I * 8)(%rdi), %rcx
	movq (RDX_I * 8)(%rdi), %rdx
	movq (RSI_I * 8)(%rdi), %rsi
	# skip rdi for now
	movq (R8_I * 8)(%rdi), %r8
	movq (R9_I * 8)(%rdi), %r9
	movq (R10_I * 8)(%rdi), %r10
	movq (R11_I * 8)(%rdi), %r11
	movq (R12_I * 8)(%rdi), %r12
	movq (R13_I * 8)(%rdi), %r13
	movq (R14_I * 8)(%rdi), %r14
	movq (R15_I * 8)(%rdi), %r15
	# skip rsp and rip
	movq (RBP_I * 8)(%rdi), %rbp
	# skip rflags

	# Now restore segment selectors to the userspace ones
	mov $0x1b, %rax
	mov %ax, %ds
	mov %ax, %es
	mov %ax, %fs
	mov %ax, %gs

	# Now we're going to determine why we entered kernel space to begin
	# with, and enter userspace accordingly.
	#
	# We either entered kernel space because of a syscall, trap (exception), or irq
	cmp $SYSCALL_REASON, %rax
	je ret_syscall
	cmp $TRAP_REASON, %rax
	je ret_trap
	cmp $IRQ_REASON, %rax
	je ret_irq

	# If we don't know why we entered userspace, give up on life...
	cli

ret_syscall:
	# We were called by syscall, so we should use the sysret mechanism to
	# return quickly. We just need to set rcx to the old rip and r11 to the
	# old rflags. We also need to restore the userspace stack and rdi
	movq (RSP_I * 8)(%rdi), %rsp
	movq (RIP_I * 8)(%rdi), %rcx
	movq (RFLAGS_I * 8)(%rdi), %r11
	movq (RDI_I * 8)(%rdi), %rdi
	sysretq
ret_irq:
ret_trap:
	# We're going to iret from this now. This will set the stack back to the
	# normal kernel stack. We will need to push the old rip, the userspace
	# cs, the old rflags, the userspace rsp, and the userspace ss
	pushq (RIP_I * 8)(%rdi)
	pushq $0x23 # Userspace CS
	pushq (RFLAGS_I * 8)(%rdi)
	pushq (RSP_I * 8)(%rdi)
	pushq $0x1b # Userspace SS
	movq (RDI_I * 8)(%rdi), %rdi
	iret

serviceSyscall:
	# First, we're going to store the state that we should return to in
	# curUserSpaceState. This will be read by the haskell interrupt/syscall
	# handler and saved as the current task context. Also in this structure
	# is a field indicating how we entered kernel space. This determines how
	# exactly we switch back into userspace.
	pushq %rax # Save rax for now
	movabsq $curUserSpaceState, %rax

	# The first quad word indicates the reason we entered userspace
	movq $SYSCALL_REASON, (%rax)
	addq $0x8, %rax

	# now save user space state
	movq %rbx, (RBX_I * 8)(%rax)
	movq %rcx, (RCX_I * 8)(%rax)
	movq %rdx, (RDX_I * 8)(%rax)
	movq %rax, %rbx
	popq %rax
	movq %rax, (RAX_I * 8)(%rbx)

	movq %rsi, (RSI_I * 8)(%rbx)
	movq %rdi, (RDI_I * 8)(%rbx)
	movq %r8, (R8_I * 8)(%rbx)
	movq %r9, (R9_I * 8)(%rbx)
	movq %r10, (R10_I * 8)(%rbx)
	movq %r11, (R11_I * 8)(%rbx)
	movq %r12, (R12_I * 8)(%rbx)
	movq %r13, (R13_I * 8)(%rbx)
	movq %r14, (R14_I * 8)(%rbx)
	movq %r15, (R15_I * 8)(%rbx)

	# Since we came from a syscall, rcx contains the rip to return to, and
	# r11 the old rflags
	movq %rcx, (RIP_I * 8)(%rbx)
	movq %rsp, (RSP_I * 8)(%rbx)
	movq %rbp, (RBP_I * 8)(%rbx)
	movq %r11, (RFLAGS_I * 8)(%rbx)

	# TODO save xmm state

	# Now, we're going to restore the kernel state, which will involve switching
	# to the kernel stack. Once on that stack, we can simply do a ret to
	# return back to the calling X86_64-arch-specific interrupt/syscall
	# routine.
	#
	# This routine will inspect the values of the rax (return value)
	# register to determine what transferred control to kernel land. The
	# return value 256 indicates a syscall from userspace. We set rax here,
	# and then fall through to restoreKernelState, which does not set rax
	mov $0x100, %rax
restoreKernelState:
	movabsq $kernelState, %rdi
	movq (RBX_I * 8)(%rdi), %rbx
	movq (RCX_I * 8)(%rdi), %rcx
	movq (RDX_I * 8)(%rdi), %rdx
	movq (RSI_I * 8)(%rdi), %rsi
	# skip rdi for now
	movq (R8_I * 8)(%rdi), %r8
	movq (R9_I * 8)(%rdi), %r9
	movq (R10_I * 8)(%rdi), %r10
	movq (R11_I * 8)(%rdi), %r11
	movq (R12_I * 8)(%rdi), %r12
	movq (R13_I * 8)(%rdi), %r13
	movq (R14_I * 8)(%rdi), %r14
	movq (R15_I * 8)(%rdi), %r15
	# skip rip
	movq (RSP_I * 8)(%rdi), %rsp
	movq (RBP_I * 8)(%rdi), %rbp
	# rflags doesn't matter, since we'll be simulating a return from a
	# function

	# TODO restore kernel xmm state

	# Now we're on the kernel stack, so we can issue a ret to return to the
	# kernel, returning whatever we got
	ret

.global setupSysCalls
setupSysCalls:
	.set STAR_MSR, 0xC0000081
	.set LSTAR_MSR, 0xC0000082
	.set SFMASK_MSR, 0xC0000084
	.set EFER_MSR, 0xC0000080
	# Sets up the syscall/sysret mechanism.
	#
	# We load the star register with the following information:
	#    - SYSCALL selector = 8 (implies CS = 8 and SS = 16 on entry to kernel-land)
	#    - SYSRET selector = 16 (implies CS = 32 and SS = 24 on return to
	#       userspace)
	#    - 32-bit target EIP is unused, so set to 0
	movq $STAR_MSR, %rcx
	movl $0x0, %eax
	movl $0x00130008, %edx
	wrmsr

	# The LSTAR register contains the address of the entry point for the
	# syscall instruction. The serviceSyscall function will handle the
	# call and adapt it to Haskell appropriately
	movq $LSTAR_MSR, %rcx
	movabsq $serviceSyscall, %rax
	mov %rax, %rdx
	shr $32, %rdx
	wrmsr

	# The SFMASK register tells us which rflags bits should be cleared on a
	# syscall.
	#
	# We clear the following bits:
	#  - trap flag (bit 8)
	#  - interrupt flag (bit 9)
	#  - IOPL (bits 12 and 13)
	#  - direction flag (bit 10)
	#  - alignment check (bit 18)
	#  - nested task bit (bit 14)
	# This means we set the following SFMASK bits:
	#  8, 9, 10, 12, 13, 14, 18
	movq $SFMASK_MSR, %rcx
	movl $0x47700, %eax
	movl $0, %edx
	wrmsr

	# Now we want to enable the syscall mechanism.
	# This involves setting bit 0 of the EFER MSR
	movq $EFER_MSR, %rcx
	rdmsr
	orq $0x1, %rax
	wrmsr
	ret

	.global trap0
	.global trap1
	.global trap2
	.global trap3
	.global trap4
	.global trap5
	.global trap6
	.global trap7
	.global trap8
	.global trap9
	.global trap10
	.global trap11
	.global trap12
	.global trap13
	.global trap14
	.global trap15
	.global trap16
	.global trap17
	.global trap18
	.global trap19
	.global trap20
	.global trap21
	.global trap22
	.global trap23
	.global trap24
	.global trap25
	.global trap26
	.global trap27
	.global trap28
	.global trap29
	.global trap30
	.global trap31
trap0:
	pushq $0
	jmp serviceISR
trap1:
	pushq $1
	jmp serviceISR
trap2:
	pushq $2
	jmp serviceISR
trap3:
	pushq $3
	jmp serviceISR
trap4:
	pushq $4
	jmp serviceISR
trap5:
	pushq $5
	jmp serviceISR
trap6:
	pushq $6
	jmp serviceISR
trap7:
	pushq $7
	jmp serviceISR
trap8:
	pushq $8
	jmp serviceISR
trap9:
	pushq $9
	jmp serviceISR
trap10:
	pushq $10
	jmp serviceISR
trap11:
	pushq $11
	jmp serviceISR
trap12:
	pushq $12
	jmp serviceISR
trap13:
	pushq $13
	jmp serviceISR
trap14:
	pushq $14
	jmp serviceISR
trap15:
	pushq $15
	jmp serviceISR
trap16:
	pushq $16
	jmp serviceISR
trap17:
	pushq $17
	jmp serviceISR
trap18:
	pushq $18
	jmp serviceISR
trap19:
	pushq $19
	jmp serviceISR
trap20:
	pushq $20
	jmp serviceISR
trap21:
	pushq $21
	jmp serviceISR
trap22:
	pushq $22
	jmp serviceISR
trap23:
	pushq $23
	jmp serviceISR
trap24:
	pushq $24
	jmp serviceISR
trap25:
	pushq $25
	jmp serviceISR
trap26:
	pushq $26
	jmp serviceISR
trap27:
	pushq $27
	jmp serviceISR
trap28:
	pushq $28
	jmp serviceISR
trap29:
	pushq $29
	jmp serviceISR
trap30:
	pushq $30
	jmp serviceISR
trap31:
	pushq $31
	jmp serviceISR
