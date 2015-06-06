include build.opts

.PHONY: all clean common rts hos cbits progs test util

all: rts common hos cbits util progs hos.iso

hos.iso: rts hos cbits kernel.bin kernel.elf
	@echo "Building ISO image..."
	cp kernel.bin cd/hos.bin
	cp build/progs/init.elf build/progs/storage.elf cd/
	cp build/hos.bundle cd
	${XORRISO} -as mkisofs -o hos.iso -b isolinux/isolinux.bin -c isolinux/boot.cat -no-emul-boot -boot-load-size 4 -boot-info-table cd

kernel.bin: build/libcbits.a build/librts.a build/hos.o linker.ld
	$(CROSSCOMPILE_LD) -T linker.ld build/hos.o -lrts -lcbits -Lbuild -o kernel.bin -Map=kernel.symbols

kernel.elf: build/libcbits.a build/librts.a build/hos.o linker-elf.ld
	$(CROSSCOMPILE_LD) -T linker-elf.ld build/hos.o -lrts -lcbits -Lbuild -o kernel.elf

progs:
	make -C progs

cbits:
	make -C cbits

rts:
	make -C rts

common:
	make -C common

hos:
	make -C src

util:
	make -C util


test:
	make -C cbits test
	make -C rts test
	make -C src test

clean:
	rm -rf build
	make -C util clean
