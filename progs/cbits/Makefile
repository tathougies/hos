include ../../build.opts
CBITS_C_SOURCES=syscall.c
CBITS_AS_SOURCES=start.S

CBITS_OBJECTS=$(addprefix ../../build/cbits-user/, $(CBITS_C_SOURCES:.c=.o) $(CBITS_AS_SOURCES:.S=.o))

.PHONY: all

all: ../../build/libcbits-user.a

../../build/libcbits-user.a: $(CBITS_OBJECTS)
	$(CROSSCOMPILE_AR) rcs ../../build/libcbits-user.a $(CBITS_OBJECTS)

../../build/cbits-user/%.S: %.c
	mkdir -p ../../build/cbits-user
	$(CROSSCOMPILE_CLANG) $< -S -I . -I ../../cbits -I ../../rts $(CFLAGS_USER) -o $@

../../build/cbits-user/%.o: ../../build/cbits-user/%.S
	mkdir -p ../../build/cbits-user
	$(CROSSCOMPILE_AS) -c $< -o $@

../../build/cbits-user/start.o: start.S
	mkdir -p ../../build/cbits-user
	$(CROSSCOMPILE_AS) -c start.S -o ../../build/cbits-user/start.o

../../build/depends/cbits-user/%.P: %.c
	mkdir -p ../../build/cbits-user
	mkdir -p ../../build/depends/cbits-user
	(printf ../../build/cbits-user/; $(CROSSCOMPILE_CLANG) -MM $< -I . -I ../../cbits -I ../../rts) > $@

include $(addprefix ../../build/depends/cbits-user/, $(CBITS_C_SOURCES:.c=.P))
