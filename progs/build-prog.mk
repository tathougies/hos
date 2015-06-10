include ../../build.opts

.PHONY: all clean

all: ../../build/progs/$(PROG_NAME).elf

../../build/progs/$(PROG_NAME).c: $(PROG_HS_FILES) ../hos/hos-0.0.1.hl ../../common/hos-common-0.0.1.hl
	mkdir -p ../../build/progs/
	$(JHC) -C $(PROG_HS_FILES) -o ../../build/progs/$(PROG_NAME).c -fcpp -I../cbits -I../../cbits -I../../rts -fffi -phos -phos-common -L../hos -L../../common -L../../build/prog-libs $(PROG_JHC_FLAGS)

../../build/progs/$(PROG_NAME).elf: ../../build/progs/$(PROG_NAME).c ../../build/libcbits-user.a ../../build/librts.a
	mkdir -p ../../build/progs/
	$(CROSSCOMPILE_CLANG) ../../build/progs/$(PROG_NAME).c -S -I ../cbits -I ../../cbits -I ../../rts -I ../hos $(CFLAGS_USER) $(PROG_CFLAGS) -o ../../build/progs/$(PROG_NAME).S 2>../../build/progs/$(PROG_NAME).clang-log
	$(CROSSCOMPILE_AS) -c ../../build/progs/$(PROG_NAME).S -o ../../build/progs/$(PROG_NAME).o
	$(CROSSCOMPILE_LD) ../../build/progs/$(PROG_NAME).o -o ../../build/progs/$(PROG_NAME).elf -lrts-user -lcbits-user -T../link-user.ld -L../../build --nmagic
	cp ../../build/progs/$(PROG_NAME).elf ../../build/progs/$(PROG_NAME)-full.elf
	$(CROSSCOMPILE_STRIP) ../../build/progs/$(PROG_NAME).elf

