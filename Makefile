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

gen_ocaml_files := \
	ui/src/gen_tz.ml

# TODO: avoid listing gen_ocaml_files twice if they've already been generated.
ocaml_files := $(shell find ui/src/ -type f -name '*.ml') $(gen_ocaml_files)
bs_files := $(ocaml_files:.ml=.bs.js)

clean:
	cd ui && bsb -clean-world
	rm -f ui/bundle.js
	rm -f ui/bundle.min.js
	rm -f .build-ml

$(gen_ocaml_files): .build-hs
	cabal v2-run gen-caml

.build-ml: $(ocaml_files)
	cd ui && npx bsb -make-world
	@# Conceptually the output here is $(bs_files), but specifying that
	@# reuslts in this rule being run multiple times for reasons I(zenhack)
	@# don't fully understand, so we use a sentinel file instead:
	touch .build-ml
ui/bundle.min.js: ui/bundle.js
	(cd ui && npx uglifyjs --compress --mangle) < $< > $@
ui/bundle.js: .build-ml ui/src/entry.js ui/package.json ui/rollup.config.js
	cd ui && npx rollup --config

.PHONY: all clean run dev pack check
