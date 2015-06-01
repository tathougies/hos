/* The bootloader will look at this image and start execution at the symbol
   designated as the entry point. */
ENTRY(_start)
OUTPUT_FORMAT(x86_64-pc-elf)
/*OUTPUT_FORMAT(i386-pc-elf)*/

/* Tell where the various sections of the object files will be put in the final
   kernel image. */
SECTIONS
{
	/* Begin putting sections at 1 MiB, a conventional place for kernels to be
	   loaded at by the bootloader. */
	. = 0xffffff7f00000000;
	_kernel_base = .;

	/* First put the multiboot header, as it is required to be put very early
	   early in the image or the bootloader won't recognize the file format.
	   Next we'll put the .text section. */
	.text BLOCK(4K) : ALIGN(4K)
	{
		*(.multiboot)
		*(.text)
	}

	/* Read-only data. */
	.rodata BLOCK(4K) : ALIGN(4K)
	{
		*(.rodata)
	}

	/* Read-write data (initialized) */
	.data BLOCK(4K) : ALIGN(4K)
	{
		*(.data)
	}
	/* Read-write data (uninitialized) and stack */
	.bss BLOCK(4K) : ALIGN(4K)
	{
		_before_bss = .;
		*(.page_tables)
		*(COMMON)
		*(.bss)
		. = ALIGN(4k);
		*(.bootstrap_stack)
		_bootstrap_stack = .;
	}
        _end = .;
	_before_bss_mb = _before_bss - _kernel_base + 0x100000;
	_end_mb = _end - _kernel_base + 0x100000;
	_start_mb = _start - _kernel_base + 0x100000;
	page_tables_base_mb = _before_bss_mb;
	kernel_size = _end - _kernel_base;
	bootstrap_stack_mb = _bootstrap_stack - _kernel_base + 0x100000;
	longModeTrampoline_mb = _longModeTrampoline - _kernel_base + 0x100000;
        kernel_heap_start = ALIGN(4k);
	/* The compiler may produce other sections, by default it will put them in
	   a segment with the same name. Simply add stuff here as needed. */
}
