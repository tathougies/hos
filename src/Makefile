include ../build.opts
HOS_SOURCES=main.hs Hos/Types.hs Hos/Task.hs Hos/Arch/X64.hs Hos/Arch/Types.hs Hos/CBits.hs
JHC_OPTS=-fbang-patterns $(JHC_COMMON)

.PHONY: test

../build/hos.o: $(HOS_SOURCES)
	$(JHC) -C $(HOS_SOURCES) -o ../build/hos.c $(JHC_OPTS) -fcpp -I../cbits -I../rts -fffi -ftype-families
	$(CROSSCOMPILE_CLANG) ../build/hos.c -S -I../rts -I../cbits $(CFLAGS) -o ../build/hos.S
	$(CROSSCOMPILE_AS) -c ../build/hos.S -o ../build/hos.o

test:
