.PHONY: all build configure clean

all: build

build: configure
	cabal build

configure:
	cabal update
	cabal configure

clean:
	cabal clean
