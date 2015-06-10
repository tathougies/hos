all: ../../build/prog-libs/$(PROG_NAME)-0.0.1.hl ../../build/progs/$(PROG_NAME).elf
include ../../build.opts

../../build/prog-libs/$(PROG_NAME)-0.0.1.hl: $(PROG_LIB_HS_FILES)
	mkdir -p ../../build/prog-libs
	jhc --build-hl=$(PROG_NAME).yaml -DTARGET=$(TARGET) -L../../common -L../hos -fcpp -fffi && cp $(PROG_NAME)-0.0.1.hl ../../build/prog-libs/
