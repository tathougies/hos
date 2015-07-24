include ../build.opts
HOS_SOURCES=main.hs Hos/Types.hs Hos/Task.hs Hos/Arch/X64.hs Hos/Arch/X64/Interrupt.hs Hos/Arch/Types.hs Hos/CBits.hs Hos/Memory.hs Hos/Privileges.hs Hos/SysCall.hs Hos/IPC.hs Data/IntervalMap.hs
JHC_OPTS=-fbang-patterns $(JHC_COMMON)

.PHONY: test

../build/hos.o: $(HOS_SOURCES) ../common/hos-common-0.0.1.hl
	$(JHC) -C $(HOS_SOURCES) -o ../build/hos.c $(JHC_OPTS) -fcpp -I../cbits -I../rts -fffi -ftype-families -DKERNEL=1 -phos-common -L../common
	$(CROSSCOMPILE_CLANG) ../build/hos.c -S -I../rts -I../cbits $(CFLAGS) -o ../build/hos.S -D _JHC_BAREBONES=1 -DCOMPILING_HS_KERNEL=1 2>../build/hos-clang-output
	$(CROSSCOMPILE_AS) -c ../build/hos.S -o ../build/hos.o

test:
