PLATFORM ?= musl64
STRIP = strip
PKG ?= haskell-hole
COMPILER = ghc8105

ifeq ($(PLATFORM),aarch64-multiplatform-musl)
STRIP = aarch64-unknown-linux-musl-strip
else
ifeq ($(PLATFORM),muslpi)
STRIP = armv6l-unknown-linux-musleabihf-strip
COMPILER = ghc884
else

endif

endif

all: package

dist/$(PLATFORM):
	mkdir -p $@

dist/$(PLATFORM)/%: dist/$(PLATFORM)
	nix-build -A projectCross.$(PLATFORM).hsPkgs.$(PKG).components.exes.$(shell basename $@) --argstr compiler-nix-name $(COMPILER)
	cp -f result/bin/$(shell basename $@) $@
	chmod +w $@
	nix-shell --run "$(STRIP) -s $@" --argstr compiler-nix-name $(COMPILER) --arg crossPlatforms "ps: with ps; [$(PLATFORM)]"
	chmod -w $@

hole: dist/$(PLATFORM)/hole
holed: dist/$(PLATFORM)/holed

package: hole holed
	cd dist/$(PLATFORM) && tar cjvf ../hole-linux-$(PLATFORM).tar.bz2 hole*

clean:
	rm -rf dist

help:
	@echo make PLATFORM=muslpi
	@echo make PLATFORM=musl64
	@echo make PLATFORM=aarch64-multiplatform-musl
