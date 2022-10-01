hs_src := $(shell find src exe -type f -name '*.hs')

all: sandcal ui/bundle.min.js
run: all
	./sandcal
dev: all
	spk dev
check:
	cabal run sandcal:test:tests
pack: sandcal.spk

.build-hs: cabal.project $(wildcard *.cabal) $(hs_src)
	cabal build all
	@# Create a sentinel file, so we can depend on this without
	@# having to specify the path the binary, which is deep under
	@# dist-newstyle.
	touch .build-hs
sandcal: .build-hs
	find dist-newstyle -type f -executable -name sandcal -exec cp \{} ./ \;
	strip $@
sandcal.spk: all
	spk pack $@

gen_elm_files := \
	ui/gen/GenAccessors.elm \
	ui/gen/GenTz.elm

elm_files := $(shell find ui/src/ -type f -name '*.elm') $(gen_elm_files)

clean:
	rm -rf ui/elm-stuff
	rm -rf ui/gen
	rm -f ui/bundle.js
	rm -f ui/bundle.min.js
	rm -f .build-hs

ui/gen/GenTz.elm: .build-hs
	cabal run gen-elm
ui/gen/GenAccessors.elm: ui/gen-accessors.py
	cd ui && python gen-accessors.py

ui/elm.js: $(elm_files)
	cd ui && elm make src/Main.elm --output=elm.js
ui/bundle.min.js: ui/bundle.js
	(cd ui && uglifyjs --compress --mangle) < $< > $@
ui/bundle.js: ui/elm.js ui/src/entry.js
	cat ui/elm.js ui/src/entry.js > $@

.PHONY: all clean run dev pack check
