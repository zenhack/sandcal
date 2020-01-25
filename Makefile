elm_src := $(shell find ui/src -type f -name '*.elm')
hs_src := $(shell find src -type f -name '*.hs')

all: ui/ui.js sandcal
run: all
	./sandcal
dev: all
	spk dev

ui/ui.js: ui/elm.json $(elm_src)
	cd ui; elm make --optimize src/Main.elm --output ui.js

.build-hs: cabal.project $(wildcard *.cabal) $(hs_src)
	cabal new-build
	@# Create a sentinel file, so we can depend on this without
	@# having to specify the path the binary, which is deep under
	@# dist-newstyle.
	touch .build-hs
sandcal: .build-hs
	find dist-newstyle -type f -executable -name sandcal -exec cp \{} ./ \;
	strip $@

.PHONY: all run dev
