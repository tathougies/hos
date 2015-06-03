include ../../build.opts

.PHONY: all clean

all: ../../build/progs/$(PROG_NAME).elf

../../build/progs/$(PROG_NAME).elf: $(PROG_HS_FILES) ../../build/libcbits-user.a ../../build/librts.a ../hos/hos-0.0.1.hl
	mkdir -p ../../build/progs/
	$(JHC) -C $(PROG_HS_FILES) -o ../../build/progs/$(PROG_NAME).c -fcpp -I../cbits -I../../cbits -I../../rts -fffi -phos -L../hos
	$(CROSSCOMPILE_CLANG) ../../build/progs/$(PROG_NAME).c -S -I ../cbits -I ../../cbits -I ../../rts $(CFLAGS_USER) $(PROG_CFLAGS) -o ../../build/progs/$(PROG_NAME).S
	$(CROSSCOMPILE_AS) -c ../../build/progs/$(PROG_NAME).S -o ../../build/progs/$(PROG_NAME).o
	$(CROSSCOMPILE_LD) ../../build/progs/$(PROG_NAME).o -o ../../build/progs/$(PROG_NAME).elf -lrts-user -lcbits-user -T../link-user.ld -L../../build --nmagic
