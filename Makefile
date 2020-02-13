elm_src := $(shell find ui -type f -name '*.elm')
hs_src := $(shell find server -type f -name '*.hs')

all: ui.js sandcal
run: all
	./sandcal
dev: all
	spk dev

ui.js: elm.json $(elm_src)
	elm make --optimize ui/Main.elm --output $@

.build-hs: cabal.project $(wildcard *.cabal) $(hs_src)
	cabal v2-build
	@# Create a sentinel file, so we can depend on this without
	@# having to specify the path the binary, which is deep under
	@# dist-newstyle.
	touch .build-hs
sandcal: .build-hs
	find dist-newstyle -type f -executable -name sandcal -exec cp \{} ./ \;
	strip $@

.PHONY: all run dev
