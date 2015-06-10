.PHONY: all

include ../../build.opts

all: hos-0.0.1.hl

hos-0.0.1.hl: Hos/User/SysCall.hs Hos/User/IO.hs Hos/User/IPC.hs
	jhc  -DTARGET=$(TARGET) --build-hl=hos.yaml -L../../common -fcpp -fffi

clean:
	rm *.hl
