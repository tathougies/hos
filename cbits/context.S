# Contains assembler code for context switching. Called directly from Haskell-land.
.section .data
xmmNextState:	.quad 0
xmmPrevState:	.quad 0

.section .bss
	.align 16
	.skip 0x8 # Ensure that the reason is 8-byte aligned, but not 16-byte aligned, which means the sse save area will be 16 byte aligned...
curUserSpaceState:
	.skip 0x298 # 1quad word reason + 18 registers + 512-byte fxsave area
		  # indicator
kernelState:
	.skip 0x290 # 18 registers  + 512-byte fxsave area
.global kernelState
.global curUserSpaceState

.align 16
.global kernelTmpStack
.global kernelTmpStack_top
.global kernelFaultStack
.global kernelFaultStack_top
kernelFaultStack:
	.skip 8192
kernelFaultStack_top:
kernelTmpStack:
	.skip 8192
kernelTmpStack_top:

	.set IST1, tssArea + 0x24

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

	.set PUSHES_ERROR_CODE_FLAG, 0x100
	.set IS_TRAP_FLAG, 0x80

	.set USERSPACE_CODE_SELECTOR, 0x23
	.set USERSPACE_DATA_SELECTOR, 0x1b
	cli # Clear the interrupt flag
	pushq %rax
	pushq %rbx
	pushq %rcx

	mov $0x10, %ax
	mov %ax, %ds
	mov %ax, %es
	mov %ax, %fs
	mov %ax, %gs
	mov %ax, %ss

	# We set the interrupts to use the new fault stack so that we can
	# detect if the error was in the kernel or not
	movabsq $IST1, %rbx
	movq (%rbx), %rcx # Save the old IST1 to check if the interrupt was from
                          # within the kernel
	movabsq $kernelFaultStack_top, %rax
	mov %rax, (%rbx)

	# Load the interrupt/trap number into rbx
	# If bit 7 is set, then this is an IRQ. Otherwise, it's a trap
	movq 0x18(%rsp), %rbx
	test $IS_TRAP_FLAG, %rbx
	jz itsATrap
	movq $IRQ_REASON, %rax
	jmp continueServiceISR
itsATrap:
	# If it's a trap, check if it may be the kernel by checking the IST1
	# entry
	movabsq $kernelFaultStack_top, %rax
	cmpq %rax, %rcx
	je inKernelFault
	movq $TRAP_REASON, %rax
continueServiceISR:
	movabsq $curUserSpaceState, %rcx
	movq %rax, (%rcx)
	# Now check if we're dealing with an error code or not
	test $PUSHES_ERROR_CODE_FLAG, %rbx
	mov $0x20, %rbx # Since rbx, rcx, and rax, and the interrupt number are on the stack, the exception information is
	# 32 off the stack
	jz noErrorCode # If there is no error code, continue with the ISR
	mov 0x20(%rsp), %rbx # Save the error code
	movabsq $x64TrapErrorCode, %rcx
	movq %rbx, (%rcx)
	mov $0x28, %rbx # We want to skip the error code
noErrorCode:
	movabsq $(curUserSpaceState + 8), %rax
	# Now, save the rip and eflags from the stack
	mov (%rsp, %rbx, 1), %rcx
	mov %rcx, (RIP_I * 8)(%rax)
	mov 0x10(%rsp, %rbx, 1), %rcx
	orq $(1 << 9), %rcx # Interrupt flags
	mov %rcx, (RFLAGS_I * 8)(%rax)

	# Now, check if this was from userspace or kernel land, and set the
	# stack approrpriately
	mov 0x8(%rsp, %rbx, 1), %rcx
	cmp $USERSPACE_CODE_SELECTOR, %rcx
	jz fromUserspace

	# We're coming from the kernel, so rsp is simply the %rsp + %rbx + 24
	leaq 0x18(%rsp, %rbx, 1), %rbx
	jmp doneWithStack

fromUserspace:
	movq 0x18(%rsp, %rbx, 1), %rbx
doneWithStack:
	movq %rbx, (RSP_I * 8)(%rax)
	popq %rcx
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

	movq %rbp, (RBP_I * 8)(%rbx)

	# Now, save SSE state
	fxsave SSE_OFFSET(%rbx)

	# Pop the interrupt number from rax and call restoreKernelState
	popq %rax
	# We don't pass along information on the error pointer to Haskell
	# The x64-specific code already knows which ones produced an error
	# code or not
	andq $(0xff | IS_TRAP_FLAG), %rax
	jmp restoreKernelState

inKernelFault:
	# Test if the RTS can handle this fault. Nothing here yet...
continueInKernelFault:
	popq %rcx
	popq %rbx
	movabsq $(kernelState + 8), %rax
	movq %rbx, (RBX_I*8)(%rax)
	popq %rax

	# Save kernel state
	movabsq $(kernelState + 8), %rbx
	movq %rax, (RAX_I*8)(%rbx)
	movq %rcx, (RCX_I*8)(%rbx)
	movq %rdx, (RDX_I*8)(%rbx)
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

	# Load %rdi with the trap number
	popq %rdi

	movq %rsp, (RSP_I * 8)(%rbx)
	movq %rbp, (RBP_I * 8)(%rbx)

	# Check if we pushed an error code, and if so, put it in %rsi
	movq $0x0, %rsi
	test $PUSHES_ERROR_CODE_FLAG, %rdi
	je continueWithPanic
	popq %rsi
continueWithPanic:
	movq (%rsp), %rdx # RIP
	movq 0x10(%rsp), %rcx # EFLAGS
	callq report_kernel_panic

callNMPanic:
	movabsq $report_sse_panic, %rax
	callq *%rax

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
	.set SSE_OFFSET, 0x90

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

	# We're going to use rax to store the means by which we will exit
	# userspace
	mov (%rdi), %rax

	# Now restore the user state
	addq $0x8, %rdi
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

	# Now, restore the sse state...
	fxrstor SSE_OFFSET(%rdi)

	# Now restore segment selectors to the userspace ones
	mov $0x1b, %rbx
	mov %bx, %ds
	mov %bx, %es
	mov %bx, %fs
	mov %bx, %gs

	movq (RBX_I * 8)(%rdi), %rbx

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

	# If we don't know why we entered from userspace, give up on life...
	cli
	hlt

ret_syscall:
	# We were called by syscall, so we should use the sysret mechanism to
	# return quickly. We just need to set rcx to the old rip and r11 to the
	# old rflags. We also need to restore the userspace stack and rdi

	# We also want to restore the IST1 entry to simply be the normal tmp stack
	movabsq $kernelTmpStack_top, %rax
	movabsq $IST1, %rcx
	movq %rax, (%rcx)

	movq (RAX_I * 8)(%rdi), %rax
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

	# We also want to restore the IST1 entry to simply be the normal tmp
	# stack
	pushq %rbx
	movabsq $IST1, %rax
	movabsq $kernelTmpStack_top, %rbx
	movq %rbx, (%rax)
	popq %rbx

	mov $0x1b, %ax
	mov %ax, %ds
	mov %ax, %es
	mov %ax, %fs
	mov %ax, %gs

	movq (RAX_I * 8)(%rdi), %rax
	pushq $0x1b # Userspace SS
	pushq (RSP_I * 8)(%rdi)
	pushq (RFLAGS_I * 8)(%rdi)
	pushq $0x23 # Userspace CS
	pushq (RIP_I * 8)(%rdi)
	movq (RDI_I * 8)(%rdi), %rdi
	iretq

serviceSyscall:
	# First, we're going to store the state that we should return to in
	# curUserSpaceState. Since this is a syscall. The calling program
	# is responsible for storing any register that is not rbp, rbx, or
	# r12-r15. This means we're free to mess around with the other
	# registers, so we're not as strict about saving things as in serviceISR.
	#
	# This will be read by the haskell interrupt/syscall
	# handler and saved as the current task context. Also in this structure
	# is a field indicating how we entered kernel space. This determines how
	# exactly we switch back into userspace.

	# We're going to save the rsp pointer first so we can get a new stack
	movabsq $curUserSpaceState, %r10
	movq $SYSCALL_REASON, (%r10)

	# Now save the registers
	addq $0x8, %r10
	movq %rsp, (RSP_I * 8)(%r10)

	# We only need to save rbp, rbx, r12, r13, r14, and r15 to conform to
	# the ABI

	mov %rbp, (RBP_I * 8)(%r10)
	mov %rbx, (RBX_I * 8)(%r10)
	mov %r12, (R12_I * 8)(%r10)
	mov %r13, (R13_I * 8)(%r10)
	mov %r14, (R14_I * 8)(%r10)
	mov %r15, (R15_I * 8)(%r10)

	# We also have to save rax, rdi, rsi, rdx, r8, and r9 to pass in kernel
	# arguments
	mov %rax, (RAX_I * 8)(%r10)
	mov %rdi, (RDI_I * 8)(%r10)
	mov %rsi, (RSI_I * 8)(%r10)
	mov %rdx, (RDX_I * 8)(%r10)
	mov %r8, (R8_I * 8)(%r10)
	mov %r9, (R9_I * 8)(%r10)

	# Since we came from a syscall, rcx contains the rip to return to, and
	# r11 the old rflags
	movq %rcx, (RIP_I * 8)(%r10)
	orq $(1 << 9), %r11 # Set interrupt flag
	movq %r11, (RFLAGS_I * 8)(%r10)

	# Now save the SSE state
	fxsave SSE_OFFSET(%r10)

	# We set the interrupts to use the new fault stack so that we can
	# detect if the error was in the kernel or not
	movabsq $kernelFaultStack_top, %r11
	movabsq $IST1, %r10
	movq %r11, (%r10)

	# Now, we're going to restore the kernel state, which will involve switching
	# to the kernel stack. Once on that stack, we can simply do a ret to
	# return back to the calling X86_64-arch-specific interrupt/syscall
	# routine.
	#
	# This routine will inspect the values of the rax (return value)
	# register to determine what transferred control to kernel land. The
	# return value 256 indicates a syscall from userspace. We set rax here,
	# and then fall through to restoreKernelState, which does not set rax
	mov $0x10, %ax
	mov %ax, %ds
	mov %ax, %es
	mov %ax, %fs
	mov %ax, %gs

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
	pushq $(8 | PUSHES_ERROR_CODE_FLAG)
	jmp serviceISR
trap9:
	pushq $9
	jmp serviceISR
trap10:
	pushq $(10 | PUSHES_ERROR_CODE_FLAG)
	jmp serviceISR
trap11:
	pushq $(11 | PUSHES_ERROR_CODE_FLAG)
	jmp serviceISR
trap12:
	pushq $(12 | PUSHES_ERROR_CODE_FLAG)
	jmp serviceISR
trap13:
	pushq $(13| PUSHES_ERROR_CODE_FLAG)
	jmp serviceISR
trap14:
	pushq $(14 | PUSHES_ERROR_CODE_FLAG)
	jmp serviceISR
trap15:
	pushq $15
	jmp serviceISR
trap16:
	pushq $16
	jmp serviceISR
trap17:
	pushq $(17 | PUSHES_ERROR_CODE_FLAG)
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

	.global irq0
	.global irq1
	.global irq2
	.global irq3
	.global irq4
	.global irq5
	.global irq6
	.global irq7
	.global irq8
	.global irq9
	.global irq10
	.global irq11
	.global irq12
	.global irq13
	.global irq14
	.global irq15
irq0:
	pushq $0x80
	jmp serviceISR
irq1:
	pushq $0x81
	jmp serviceISR
irq2:
	pushq $0x82
	jmp serviceISR
irq3:
	pushq $0x83
	jmp serviceISR
irq4:
	pushq $0x84
	jmp serviceISR
irq5:
	pushq $0x85
	jmp serviceISR
irq6:
	pushq $0x86
	jmp serviceISR
irq7:
	pushq $0x87
	jmp serviceISR
irq8:
	pushq $0x88
	jmp serviceISR
irq9:
	pushq $0x89
	jmp serviceISR
irq10:
	pushq $0x8A
	jmp serviceISR
irq11:
	pushq $0x8B
	jmp serviceISR
irq12:
	pushq $0x8C
	jmp serviceISR
irq13:
	pushq $0x8D
	jmp serviceISR
irq14:
	pushq $0x8E
	jmp serviceISR
irq15:
	pushq $0x8F
	jmp serviceISR

	.align 8
x64TrapErrorCode:
	.global x64TrapErrorCode
	.quad 0
