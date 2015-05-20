include ../../build.opts

.PHONY: all clean

all: ../../build/progs/$(PROG_NAME).elf

../../build/progs/$(PROG_NAME).elf: $(PROG_HS_FILES) ../../build/libcbits-user.a ../../build/librts.a
	mkdir -p ../../build/progs/
	$(JHC) -C $(PROG_HS_FILES) -o ../../build/progs/$(PROG_NAME).c -fcpp -I../cbits -I../../cbits -I../../rts -fffi
	$(CROSSCOMPILE_CLANG) ../../build/progs/$(PROG_NAME).c -S -I ../cbits -I ../../cbits -I ../../rts $(CFLAGS_USER) -o ../../build/progs/$(PROG_NAME).S
	$(CROSSCOMPILE_AS) -c ../../build/progs/$(PROG_NAME).S -o ../../build/progs/$(PROG_NAME).o
	$(CROSSCOMPILE_LD) ../../build/progs/$(PROG_NAME).o -o ../../build/progs/$(PROG_NAME).elf -lrts -lcbits-user -T../link-user.ld -L../../build
