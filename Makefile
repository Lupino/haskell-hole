PLATFORM ?= musl64
STRIP = strip
PKG = haskell-hole
SYSTEM = linux
EXT =

ifeq ($(PLATFORM),aarch64-multiplatform-musl)
STRIP = aarch64-linux-gnu-strip
COMPILER ?= ghc9141
else
ifeq ($(PLATFORM),muslpi)
STRIP = armv6l-unknown-linux-musleabihf-strip
COMPILER ?= ghc884
else
ifeq ($(PLATFORM),mingwW64)
STRIP = x86_64-w64-mingw32-strip
COMPILER ?= ghc9141
EXT = .exe
SYSTEM = windows
else
COMPILER ?= ghc9141
endif
endif
endif

OUT = hole holed

BUNDLE_BIN = dist/bundle/bin
BUNDLE_LIB = dist/bundle/lib/hole
BUNDLE_EXEC_PATH = @executable_path/../lib/hole
BUNDLE = dylibbundler -b -d $(BUNDLE_LIB) -p '$(BUNDLE_EXEC_PATH)' -of
BUNDLE_BINS = $(foreach var,$(OUT),dist/bundle/bin/$(var))

all: package

dist/$(PLATFORM):
	mkdir -p $@

dist/$(PLATFORM)/%: dist/$(PLATFORM)
	nix-build -A projectCross.$(PLATFORM).hsPkgs.$(PKG).components.exes.$(shell basename $@ $(EXT)) --argstr compiler-nix-name $(COMPILER) # --arg enableProfiling true
	cp -f result/bin/$(shell basename $@) $@
	chmod +w $@
	nix-shell --run "$(STRIP) -s $@" --argstr compiler-nix-name $(COMPILER) --arg crossPlatforms "ps: with ps; [$(PLATFORM)]"
	chmod -w $@

hole:
	make dist/$(PLATFORM)/$@$(EXT)

holed:
	make dist/$(PLATFORM)/$@$(EXT)

package: $(OUT)
	cd dist/$(PLATFORM) && tar cjvf ../hole-$(SYSTEM)-$(PLATFORM).tar.bz2 hole*

dist/bundle/bin/%: bin/%
	@mkdir -p dist/bundle/bin
	@mkdir -p dist/bundle/lib/hole
	cp $< $@
	nix-shell -p macdylibbundler --run "$(BUNDLE) -x $@"
	echo sudo xattr -d com.apple.quarantine $< >> dist/bundle/install.sh

macos-build:
	stack install --local-bin-path bin

macos-install:
	@mkdir -p dist/bundle
	echo '#!/usr/bin/env bash' > dist/bundle/install.sh

macos-bundle: macos-install macos-build $(BUNDLE_BINS)
	cd dist/bundle && find lib -type f | while read F; do echo sudo xattr -d com.apple.quarantine $$F >> install.sh; done
	chmod +x dist/bundle/install.sh
	cd dist/bundle && tar cjvf ../hole-macos-bundle.tar.bz2 .

update-sha256:
	gawk -f nix/update-sha256.awk cabal.project > nix/sha256map.nix

clean:
	rm -rf dist

help:
	@echo make PLATFORM=muslpi
	@echo make PLATFORM=musl64
	@echo make PLATFORM=aarch64-multiplatform-musl
	@echo make PLATFORM=mingwW64
	@echo make macos-bundle
	@echo make clean
	@echo make update-sha256
