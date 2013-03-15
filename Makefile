all: build

configure:
	cabal configure --enable-tests

build:
	cabal build

test:
	cabal test

dist:
	cabal dist

clean:
	cabal clean
