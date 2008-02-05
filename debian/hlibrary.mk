#!/usr/bin/make -f

# Experimental CDBS rule file for building haskell library packages v0.2
# TODO: Get this included in a package (haskell-devscripts?) and have
# it installed somewhere under /usr/share/cdbs/
# Fix any remaining issues with this.  Feedback is welcome!

CABAL_PACKAGE = $(shell cat *.cabal |\
 perl -ne 'if (/^name:\s*(.*)$$/i) {$$_ = $$1; tr/A-Z/a-z/; print; exit 0;}')

clean::
	test ! -e setup-bin || ./setup-bin clean
	rm -rf dist dist-ghc6 dist-hugs setup-bin Setup.hi Setup.ho Setup.o .*config*

setup-bin::
	if test ! -e Setup.lhs -a ! -e Setup.hs; then echo "No setup script found!"; exit 1; fi
	for setup in Setup.lhs Setup.hs; do if test -e $$setup; then ghc6 -package Cabal $$setup -o setup-bin; exit 0; fi; done

dist-ghc6: setup-bin
	./setup-bin configure --ghc --prefix=/usr/lib/haskell-packages/ghc6 --enable-library-profiling -v2
	mv dist dist-ghc6

build/libghc6-$(CABAL_PACKAGE)-dev:: dist-ghc6
	mv dist-ghc6 dist
	./setup-bin build
	mv dist dist-ghc6

build/libghc6-$(CABAL_PACKAGE)-prof:: dist-ghc6
	mv dist-ghc6 dist
	./setup-bin build
	mv dist dist-ghc6

build/libghc6-$(CABAL_PACKAGE)-doc:: dist-ghc6
	mv dist-ghc6 dist
	./setup-bin haddock
	mv dist dist-ghc6

dist-hugs: setup-bin
	./setup-bin configure --hugs --prefix=/usr -v2
	mv dist dist-hugs

build/libhugs-$(CABAL_PACKAGE):: dist-hugs
	mv dist-hugs dist
	./setup-bin build
	mv dist dist-hugs

binary-install/libghc6-$(CABAL_PACKAGE)-dev:: setup-bin
	mv dist-ghc6 dist
	./setup-bin copy --destdir=debian/libghc6-$(CABAL_PACKAGE)-dev
	dh_haskell_prep -plibghc6-$(CABAL_PACKAGE)-dev
	cp dist/installed-pkg-config debian/libghc6-$(CABAL_PACKAGE)-dev/usr/lib/haskell-packages/ghc6/lib/*/
	rm -rf debian/libghc6-$(CABAL_PACKAGE)-dev/usr/lib/haskell-packages/ghc6/share/
	mv dist dist-ghc6

binary-fixup/libghc6-$(CABAL_PACKAGE)-dev:: binary-install/libghc6-$(CABAL_PACKAGE)-dev setup-bin
	find debian/libghc6-$(CABAL_PACKAGE)-dev/usr/lib/haskell-packages/ghc6/lib/ -name "*_p.a" -exec rm '{}' ';'
	find debian/libghc6-$(CABAL_PACKAGE)-dev/usr/lib/haskell-packages/ghc6/lib/ -name "*.p_hi" -exec rm '{}' ';'

binary-install/libghc6-$(CABAL_PACKAGE)-prof:: setup-bin
	mv dist-ghc6 dist
	./setup-bin copy --destdir=debian/libghc6-$(CABAL_PACKAGE)-prof
	dh_haskell_prep -plibghc6-$(CABAL_PACKAGE)-prof
# Work around #460558
	sed -i 's/ghc6-prof-prof/ghc6-prof/' debian/libghc6-$(CABAL_PACKAGE)-prof.substvars
	cp dist/installed-pkg-config debian/libghc6-$(CABAL_PACKAGE)-dev/usr/lib/haskell-packages/ghc6/lib/*/
	rm -rf debian/libghc6-$(CABAL_PACKAGE)-prof/usr/lib/haskell-packages/ghc6/share/
	mv dist dist-ghc6

binary-fixup/libghc6-$(CABAL_PACKAGE)-prof:: binary-install/libghc6-$(CABAL_PACKAGE)-prof setup-bin
	find debian/libghc6-$(CABAL_PACKAGE)-prof/usr/lib/haskell-packages/ghc6/lib/ -name "*[^p].a" -exec rm '{}' ';'
	find debian/libghc6-$(CABAL_PACKAGE)-prof/usr/lib/haskell-packages/ghc6/lib/ -name "*.o" -exec rm '{}' ';'
	find debian/libghc6-$(CABAL_PACKAGE)-prof/usr/lib/haskell-packages/ghc6/lib/ -name "*.hi" -exec rm '{}' ';'

binary-install/libghc6-$(CABAL_PACKAGE)-doc:: setup-bin
	mv dist-ghc6 dist
	mkdir -p debian/libghc6-$(CABAL_PACKAGE)-doc/usr/share/doc/libghc6-$(CABAL_PACKAGE)-doc/html/
	cp -r dist/doc/html/*/* debian/libghc6-$(CABAL_PACKAGE)-doc/usr/share/doc/*/html/
	mv dist dist-ghc6

binary-install/libhugs-$(CABAL_PACKAGE):: setup-bin
	mv dist-hugs dist
	./setup-bin copy --destdir=debian/libhugs-$(CABAL_PACKAGE)
	rm -rf debian/libhugs-$(CABAL_PACKAGE)/usr/share/doc/
	dh_haskell_prep -plibhugs-$(CABAL_PACKAGE)
	mv dist dist-hugs

include /usr/share/cdbs/1/rules/debhelper.mk
