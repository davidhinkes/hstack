all: build

configure:
	cabal configure --enable-tests || exit 1

build:
	cabal build || exit 1

test:
	cabal test

dist:
	cabal 
