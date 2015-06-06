.PHONY: all

all: hos-common-0.0.1.hl

hos-common-0.0.1.hl: Data/Elf.hs Data/Binary.hs Hos/Common/Types.hs Hos/Common/Bundle.hs
	jhc --build-hl=common.yaml

clean:
	rm *.hl
