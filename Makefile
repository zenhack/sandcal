hs_src := $(shell find src -type f -name '*.hs')

all: sandcal
run: all
	./sandcal
dev: all
	spk dev
pack: sandcal.spk

.build-hs: cabal.project $(wildcard *.cabal) $(hs_src)
	cabal v2-build
	@# Create a sentinel file, so we can depend on this without
	@# having to specify the path the binary, which is deep under
	@# dist-newstyle.
	touch .build-hs
sandcal: .build-hs
	find dist-newstyle -type f -executable -name sandcal -exec cp \{} ./ \;
	strip $@
sandcal.spk: all
	spk pack $@

.PHONY: all run dev pack
