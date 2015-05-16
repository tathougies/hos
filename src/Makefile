include ../build.opts
HOS_SOURCES=test.hs
JHC_OPTS=-fbang-patterns

.PHONY: test

../build/hos.o: $(HOS_SOURCES)
	$(JHC) -C $(HOS_SOURCES) -o ../build/hos.c $(JHC_OPTS)
	$(CROSSCOMPILE_CLANG) ../build/hos.c -S -I../rts -I../cbits $(CFLAGS) -o ../build/hos.S
	$(CROSSCOMPILE_AS) -c ../build/hos.S -o ../build/hos.o

test:
