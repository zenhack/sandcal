hs_src := $(shell find src exe -type f -name '*.hs')

all: sandcal ui/bundle.min.js
run: all
	./sandcal
dev: all
	spk dev
check:
	cabal v2-run sandcal:test:tests
pack: sandcal.spk

.build-hs: cabal.project $(wildcard *.cabal) $(hs_src)
	cabal v2-build all
	@# Create a sentinel file, so we can depend on this without
	@# having to specify the path the binary, which is deep under
	@# dist-newstyle.
	touch .build-hs
sandcal: .build-hs
	find dist-newstyle -type f -executable -name sandcal -exec cp \{} ./ \;
	strip $@
sandcal.spk: all
	spk pack $@

gen_elm_files :=
#gen_ocaml_files := \
#	ui/src/gen_tz.ml

# TODO: avoid listing gen_elm_files twice if they've already been generated.
elm_files := $(shell find ui/src/ -type f -name '*.elm') $(gen_elm_files)

clean:
	cd ui && bsb -clean-world
	rm -f ui/bundle.js
	rm -f ui/bundle.min.js
	rm -f .build-hs

$(gen_ocaml_files): .build-hs
	cabal v2-run gen-caml

ui/elm.js: $(elm_files)
	cd ui && elm make src/Main.elm --output=elm.js
ui/bundle.min.js: ui/bundle.js
	(cd ui && uglifyjs --compress --mangle) < $< > $@
ui/bundle.js: ui/elm.js ui/src/entry.js
	cat ui/elm.js ui/src/entry.js > $@

.PHONY: all clean run dev pack check
