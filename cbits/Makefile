include ../build.opts
CBITS_C_SOURCES=memory.c multiboot.c rts_integration.c arch.c
CBITS_AS_SOURCES=boot.S
TEST_PROGRAMS=multiboot pool rbtree

CBITS_OBJECTS=$(addprefix ../build/cbits/, $(CBITS_C_SOURCES:.c=.o) $(CBITS_AS_SOURCES:.S=.o))

.PHONY: all test

all: ../build/libcbits.a

../build/libcbits.a: $(CBITS_OBJECTS)
	$(CROSSCOMPILE_AR) rcs ../build/libcbits.a $(CBITS_OBJECTS)

../build/cbits/%.S: %.c
	mkdir -p ../build/cbits
	$(CROSSCOMPILE_CLANG) $< -S -I . -I ../rts $(CFLAGS) -o $@

../build/cbits/%.o: ../build/cbits/%.S
	mkdir -p ../build/cbits
	$(CROSSCOMPILE_AS) -c $< -o $@

../build/depends/cbits/%.P: %.c
	mkdir -p ../build/cbits
	mkdir -p ../build/depends/cbits
	(printf ../build/cbits/; $(CROSSCOMPILE_CLANG) -MM $< -I. -I../rts) > $@

include $(addprefix ../build/depends/cbits/, $(CBITS_C_SOURCES:.c=.P))
../build/cbits/boot.o: boot.S
	mkdir -p ../build/cbits
	$(CROSSCOMPILE_AS) -c boot.S -o ../build/cbits/boot.o

test: $(addsuffix .passed, $(addprefix ../build/test/, $(TEST_PROGRAMS)))

../build/test/%.passed: test/%.c
	@echo "Test '$<'"
	@echo "  Building..."
	mkdir -p ../build/test
	clang -o $@-test $< test/shims.c $(CBITS_C_SOURCES) -g3 -I . -I ../rts $(TEST_CFLAGS) -Wl,-no_pie
	@echo "  Running..."
	$@-test
	touch $@

include $(addprefix ../build/depends/cbits/test/, $(addsuffix .P-test, $(TEST_PROGRAMS)))
../build/depends/cbits/test/%.P-test: test/%.c
	mkdir -p ../build/depends/cbits/test
	(printf ../build/test/; $(CROSSCOMPILE_CLANG) -MM $< -MT `basename $< .c`.passed -I. -I../rts) > $@
