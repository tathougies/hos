.PHONY: all

all: ../bin/hos-build-bundle

../bin/hos-build-bundle: hos-build-bundle.hs
	mkdir -p ../bin
	ghc --make hos-build-bundle.hs -o ../bin/hos-build-bundle -i../../common/
