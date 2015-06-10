# Declare constants used for creating a multiboot header.
.set ALIGN,    1<<0             # align loaded modules on page boundaries
.set MEMINFO,  1<<1             # provide memory map
.set AOUT_KLUDGE, 0x10000
.set FLAGS,    ALIGN | MEMINFO | AOUT_KLUDGE # this is the Multiboot 'flag' field
.set MAGIC,    0x1BADB002       # 'magic number' lets bootloader find the header
.set CHECKSUM, -(MAGIC + FLAGS) # checksum of above, to prove we are multiboot
.set KERNEL_LOAD_BASE, 0x100000
# Declare a header as in the Multiboot Standard. We put this into a special
# section so we can force the header to be in the start of the final program.
# You don't need to understand all these details as it is just magic values that
# is documented in the multiboot standard. The bootloader will search for this
# magic sequence and recognize us as a multiboot kernel.
.section .multiboot
.align 4
.long MAGIC
.long FLAGS
.long CHECKSUM
.long 0x100000
.long 0x100000
.long _before_bss_mb
.long _end_mb
.long _start_mb

# Currently the stack pointer register (esp) points at anything and using it may
# cause massive harm. Instead, we'll provide our own stack. We will allocate
# room for a small temporary stack by creating a symbol at the bottom of it,
# then allocating 16384 bytes for it, and finally creating a symbol at the top.
.section .bootstrap_stack, "aw", @nobits
stack_unmap:
.global stack_unmap
.align 4096
.skip 4096 # This page will be unmapped
stack_bottom:
.global stack_bottom
.skip 16384 # 16 KiB
.skip 16384 # 16 KiB
stack_top:
.global stack_top

.section .page_tables, "aw", @nobits
page_tables_base:
.set PML4, . - page_tables_base
	.skip 0x1000
.set PDPT, . - page_tables_base
	.skip 0x1000
.set PDT, . - page_tables_base
	.skip 0x1000
.set PT, . - page_tables_base
	.skip 0x1000
.set IDENTITY_PDPT, . - page_tables_base
	.skip 0x1000
.set IDENTITY_PDT, . - page_tables_base
	.skip 0x1000
.set IDENTITY_PT, . - page_tables_base
	.skip 0x1000
.set IDENTITY_PT2, . - page_tables_base
	.skip 0x1000
.set PAGE_TABLE_SZ, . - page_tables_base
end_page_tables:

.set TSS_OFFS, . - page_tables_base
.global tssArea
tssArea: .skip 0x6C
.set IOPB, . - tssArea
.set IOPB_OFFS, . - page_tables_base
iopb:
	 .skip 8193 # We need one extra byte at the end of the iopb
.set TSS_SIZE, . - tssArea

gdt64:
.set GDT_OFFS, . - page_tables_base
.set nullS, . - gdt64
	.skip 8
.set codeS, . - gdt64
	.skip 8
.set dataS, . - gdt64
	.skip 8
.set userDataS, . - gdt64
	.skip 8
.set userCodeS, . - gdt64
	.skip 8
.set tssS, . - gdt64
	.skip 16
.set GDT_SIZE, . - gdt64 - 1
gdt_pointer:
.set GDT_POINTER_OFFS, . - page_tables_base
	.short 0
	.quad 0

# The linker script specifies _start as the entry point to the kernel and the
# bootloader will jump to this position once the kernel has been loaded. It
# doesn't make sense to return from this function as the bootloader is gone.
.section .text
.global _start
.global initialize_regions
.global g_mboot_hdr_ptr
.global g_buddy_regions
.type _start, @function
_start:
	.code32
	movl $bootstrap_stack_mb, %esp
	cli
	# Now, we're going to set up long mode paging, identity mapping the whole kernel to 0xffffffff00000000
	# We're also going to identity map all the first 2 GB to 0xffffffff80000000
	call _zeroPageTables
	call _setupGdt
	call _setPageTableEntries

	# Set up gdt pointer
	movl $(page_tables_base_mb + GDT_POINTER_OFFS), %ecx
	movw $GDT_SIZE, (%ecx)
	movl $(page_tables_base_mb + GDT_OFFS), 2(%ecx)

	# Set up page tables
	movl $(page_tables_base_mb + PML4), %eax
	movl %eax, %cr3

	# Enable PAE paging
	movl %cr4, %eax
	orl $(1 << 5), %eax
	movl %eax, %cr4

	# Now switch to long mode
	mov $0xc0000080, %ecx
	rdmsr
	orl $(1 << 8), %eax
	wrmsr

	mov %cr0, %eax
	orl $((1 << 31) | (1 << 0)), %eax
	mov %eax, %cr0

	# Now we're in compatibility mode
	mov $(page_tables_base_mb+GDT_POINTER_OFFS), %eax
	lgdt (page_tables_base_mb + GDT_POINTER_OFFS)
	pushl $codeS
	pushl $longModeTrampoline_mb
	lret
.global _longModeTrampoline
_longModeTrampoline:
	.code64
	movabsq $_longModeStart, %rax
	jmpq *%rax
_longModeStart:
	# Now replace all the data selectors
	mov $dataS, %ax
	mov %ax, %ds
	mov %ax, %es
	mov %ax, %gs
	mov %ax, %fs

	# To set up a stack, we simply set the esp register to point to the top of
	# our stack (as it grows downwards).
	movabsq $stack_top, %rsp
	mov %ax, %ss
	movabsq $g_mboot_hdr_ptr, %rax
	movq %rbx, (%rax) # Store the multboot header pointer somewhere

	# clang is smart and likes vectorized operations (we do too!), but this means that sse will need to be enabled
	movq %cr4, %rax
	orq $0x600, %rax
	movq %rax, %cr4

	movq %cr0, %rax
	andq $(~0x4), %rax # Disable x87 emulation
	orq $0x2, %rax # Enable monitor coprocessor bit
	movq %rax, %cr0

	# Now, the gdtr contains a stale gdt pointer. We'd like the gdtr to point to the correct higher-half address
	movabsq $gdt_pointer, %rax
	movw $GDT_SIZE, (%rax)
	movabsq $gdt64, %rcx
	movq %rcx, 2(%rax)
	lgdt (%rax)

	# Now, we can load the task register with our one and only TSS segment
	movw $tssS, %ax
	ltr %ax

	# We're also going to initialize the serial port for kernel output, so that
	# the kernel run-time system and the Haskell kernel have different means of
	# communicating
	.set COM1, 0x3f8
	movb $0x00, %al
	mov $(COM1 + 1), %dx
	out %al, %dx # Disable interrupts
	movb $0x80, %al
	mov $(COM1 + 3), %dx
	out %al, %dx # Enable DLAB (baud rate divisor)
	movb $0x01, %al
	mov $(COM1 + 0), %dx
	outb %al, %dx # lo byte for 115200 baud
	movb $0x00, %al
	mov $(COM1 + 1), %dx
	outb %al, %dx # hi byte
	movb $0x03, %al
	mov $(COM1 + 3), %dx
	outb %al, %dx # 8-bits, no parity, one stop bit
	movb $0xc7, %al
	mov $(COM1 + 2), %dx
	outb %al, %dx # Clear 14-byte FIFO
	movb $0x0b, %al
	mov $(COM1 + 4), %dx
	outb %al, %dx # Enable RTS/DSR
	movb $0x0, %al
	mov $(COM1 + 1), %dx
	outb %al, %dx # Disable interrupts

	# Now we're going to call the initialize_regions function with the multiboot header
	call bootstrap_kernel
	# At this point we have all the structures we need to initialize the jhc garbage collector.
	# The garbage collector is initialized by calling jhc_alloc_init in gc_jgc.c
	call jhc_alloc_init
	# We're now ready to initialize the jhc haskell runtime. jhc creates a function for us called jhc_hs_init, which initializes its garbage collection structures. We should call that now.

	call jhc_hs_init
	call _amain

	# In case the function returns, we'll want to put the computer into an
	# infinite loop. To do that, we use the clear interrupt ('cli') instruction
	# to disable interrupts, the halt instruction ('hlt') to stop the CPU until
	# the next interrupt arrives, and jumping to the halt instruction if it ever
	# continues execution, just to be safe. We will create a local label rather
	# than real symbol and jump to there endlessly.
	xorq %rdi, %rdi
	call jhc_exit
	cli
	hlt
.Lhang:
	jmp .Lhang

# Set the size of the _start symbol to the current location '.' minus its start.
# This is useful when debugging or when you implement call tracing.
.size _start, . - _start
.code32
.global _zeroPageTables
_zeroPageTables:
	movl $(page_tables_base_mb), %edi
	movl $(PAGE_TABLE_SZ >> 2), %ecx
	xor %eax, %eax
	rep stosl
	ret
.size _zeroPageTables, . - _zeroPageTables

_setPageTableEntries:
	# Set PML4 to point to pdpt at last 512 GB of ram
	mov $(page_tables_base_mb + PML4), %eax
	movl $(page_tables_base_mb + PDPT + 0x3), %ecx
	movl %ecx, 0xff0(%eax)
	movl $(page_tables_base_mb + IDENTITY_PDPT + 0x3), %ecx
	movl %ecx, (%eax)
	movl %eax, %ecx
	addl $0x3, %ecx
	movl %ecx, 0xff8(%eax)

	# Set PDPT to map second to last 3 GB of second to last 512 gb to kernel space
	mov $(page_tables_base_mb + PDPT), %eax
	mov $(page_tables_base_mb + PDT + 3), %ecx
	movl %ecx, 0xfe0(%eax) # The kernel PDT gets 1 GB

	# Set last entries of PDPT to identity map first 2 GB of RAM
	mov $0x83, %ecx
	movl %ecx, 0xff0(%eax)
	mov $0x40000083, %ecx
	movl %ecx, 0xff8(%eax)

	# Set up PT in PDT
	mov $(page_tables_base_mb + PDT), %eax
	mov $(page_tables_base_mb + PT + 3), %ecx
	movl %ecx, (%eax)

	# Now, identity map some of the kernel (for the switch to paging...)
	mov $(page_tables_base_mb + IDENTITY_PDPT), %eax
	movl $(page_tables_base_mb + IDENTITY_PDT + 3), (%eax)

	mov $(page_tables_base_mb + IDENTITY_PDT), %eax
	movl $(page_tables_base_mb + IDENTITY_PT + 3), (%eax)
	# IDENTITY_PT2 maps the range 2-4MB. We need it because the kernel is greater than 1mb.
	# It comes right after IDENTITY_PT, so our identity mapping loop just continues
	movl $(page_tables_base_mb + IDENTITY_PT2 + 3), 0x8(%eax)

	# Now, we need to map the entirety of kernel space into the 2 mb page table we have set up...
	mov $(page_tables_base_mb + PT), %eax
	mov $(page_tables_base_mb + IDENTITY_PT + 0x800), %edi
	movl $(KERNEL_LOAD_BASE | 3), %ecx
_mapKernelPage:
	movl %ecx, %edx
	andl $0xfffff000, %edx
	subl $KERNEL_LOAD_BASE, %edx # edx now contains the length already mapped
	cmpl $kernel_size, %edx
	jge _setPageTableEntries_done # if we've ran beyond the end of the kernel, we're done

	movl %ecx, (%eax)
	movl %ecx, (%edi)

	addl $0x1000, %ecx
	addl $0x8, %eax
	addl $0x8, %edi
	jmp _mapKernelPage
_setPageTableEntries_done:
	ret

_setupGdt:
	mov $(page_tables_base_mb + GDT_OFFS), %ecx
	# Null segment
	movl $0, (%ecx)
	movl $0, 4(%ecx)

	# Code segment
	movl $0, 0x8(%ecx)
	movl $0x00209800, 0xC(%ecx)

	# Data segment
	movl $0, 0x10(%ecx)
	movl $0x00009200, 0x14(%ecx)

	# Userspace code segment
	movl $0, 0x20(%ecx)
	movl $0x0020F800, 0x24(%ecx)

	# Userspace data segment
	movl $0, 0x18(%ecx)
	movl $0x0000F200, 0x1c(%ecx)

	# TSS Segment

	# First 16-bits is segment limit which is 8 kb(8192)
	movw $TSS_SIZE, 0x28(%ecx)
	movl $(page_tables_base_mb + TSS_OFFS), %eax
	subl $0x100000, %eax
	movw %ax, 0x2a(%ecx)
	# Mov bits 16 - 23 into the next byte
	shr $0x10, %eax
	movb %al, 0x2c(%ecx)
	# Segment is present, and the type is a 64-bit tss (9)
	movb $0x89, 0x2d(%ecx)
	# upper bits of segment limit are 0
	movb $0x0, 0x2e(%ecx)
	# bits 24 - 31 are in the next byte
	movb %ah, 0x2f(%ecx)
	# Now move the upper 32-bits of the tss offset into the next 32-bits
	movl $0xffffff7f, 0x30(%ecx)
	# The highest 32-bits are reserved
	xor %eax, %eax
	movl %eax, 0x34(%ecx)

	# Remember to clear the tss segment too
	movl $(page_tables_base_mb + TSS_OFFS), %edi
	movl $((IOPB_OFFS - TSS_OFFS) >> 2), %ecx
	# eax is 0 from above
	rep stosl

	movl $8193, %ecx
	mov $0xff, %ax
	rep stosb

	# Set the IOPB base ptr correctly
	movl IOPB, %eax
	movl %eax, (page_tables_base_mb + TSS_OFFS + 0x68)

	ret

write_serial:
	.code64
	.global write_serial
	push %rdx
	push %rax
	mov $(0x3f8 + 5), %dx
wait:	inb %dx, %al
	testb $0x20, %al
	jz wait
	mov $0x3f8, %dx
	mov %rdi, %rax
	outb %al, %dx
	pop %rax
	pop %rdx
	ret
