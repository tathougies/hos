include ../build.opts
RTS_SOURCES=rts.c gc_jgc.c

.PHONY: all test

all: ../build/librts.a

../build/librts.a: $(addprefix ../build/rts/, $(RTS_SOURCES:.c=.o))
	$(CROSSCOMPILE_AR) rcs ../build/librts.a $(addprefix ../build/rts/, $(RTS_SOURCES:.c=.o))

../build/rts/%.s: %.c
	mkdir -p ../build/rts
	$(CROSSCOMPILE_CLANG) $< -S -I . -I../cbits/ $(CFLAGS) -o $@

%.o: %.s
	mkdir -p ../build/rts
	$(CROSSCOMPILE_AS) -c $< -o $@

../build/depends/rts/%.P: %.c
	mkdir -p ../build/rts/
	mkdir -p ../build/depends/rts
	(printf ../build/rts/; $(CROSSCOMPILE_CLANG) -MM $< -I.) > $@

test:

include $(addprefix ../build/depends/rts/, $(RTS_SOURCES:.c=.P))
