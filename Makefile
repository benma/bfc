build:
	cabal-dev build
install-prof:
	cabal-dev --extra-config-file=cabal-dev.config install
build-prof:
	cabal-dev build # template haskell requires two-phase compiling according to http://www.haskell.org/ghc/docs/6.8.1/html/users_guide/template-haskell.html
	cabal-dev build --ghc-options="-osuf p_o -prof -auto-all -caf-all"
	echo "run program with ./dist/build/bfc/ +RTS -h"
build-prof2:
	cabal-dev build --ghc-options="-osuf p_o -prof -auto-all -caf-all"
show-prof:
	hp2ps -c bfc.hp
	exo-open bfc.ps
