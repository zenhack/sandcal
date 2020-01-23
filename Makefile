elm_src := $(shell find ui/src -type f -name '*.elm')
hs_src := $(shell find src -type f -name '*.hs')

all: ui/ui.js .build-hs
run: all
	cabal v2-run sandcal

ui/ui.js: ui/elm.json $(elm_src)
	cd ui; elm make --debug src/Main.hs --output $@

.build-hs: cabal.project $(wildcard *.cabal) $(hs_src)
	cabal new-build
	@# Create a sentinel file, so we can depend on this without
	@# having to specify the path the binary, which is deep under
	@# dist-newstyle.
	touch .build-hs

.PHONY: all run
