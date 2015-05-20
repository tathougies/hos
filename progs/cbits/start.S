.global _start
_start:
	call jhc_hs_init
	call _amain
	xor %rcx, %rcx
	syscall
