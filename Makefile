all: build

build:
	cabal update
	cabal build

clean:
	cabal clean
