include ../build.opts
RTS_SOURCES=rts.c gc_jgc.c
RTS_USER_SOURCES=$(RTS_SOURCES) stableptr.c

.PHONY: all test

all: ../build/librts.a ../build/librts-user.a

../build/librts.a: $(addprefix ../build/rts/, $(RTS_SOURCES:.c=.o))
	$(CROSSCOMPILE_AR) rcs ../build/librts.a $(addprefix ../build/rts/, $(RTS_SOURCES:.c=.o))

../build/librts-user.a: $(addprefix ../build/rts-user/, $(RTS_USER_SOURCES:.c=.o))
	$(CROSSCOMPILE_AR) rcs ../build/librts-user.a $(addprefix ../build/rts-user/, $(RTS_USER_SOURCES:.c=.o))

../build/rts/%.s: %.c
	mkdir -p ../build/rts
	$(CROSSCOMPILE_CLANG) $< -S -I . -I../cbits/ $(CFLAGS) -o $@ -D _JHC_BAREBONES=1

../build/rts-user/%.s: %.c
	mkdir -p ../build/rts-user
	$(CROSSCOMPILE_CLANG) $< -S -I . -I../cbits/ $(CFLAGS) -o $@ -I../progs/cbits

%.o: %.s
	mkdir -p ../build/rts
	mkdir -p ../build/rts-user
	$(CROSSCOMPILE_AS) -c $< -o $@

../build/depends/rts/%.P: %.c
	mkdir -p ../build/rts/
	mkdir -p ../build/depends/rts
	(printf ../build/rts/; $(CROSSCOMPILE_CLANG) -MM $< -I.) > $@

../build/depends/rts-user/%.P: %.c
	mkdir -p ../build/rts-user/
	mkdir -p ../build/depends/rts-user
	(printf ../build/rts-user/; $(CROSSCOMPILE_CLANG) -MM $< -I.) > $@

test:

include $(addprefix ../build/depends/rts/, $(RTS_SOURCES:.c=.P))
include $(addprefix ../build/depends/rts-user/, $(RTS_USER_SOURCES:.c=.P))
