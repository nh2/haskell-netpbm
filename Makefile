# Intended for library developer use only

.PHONY: config
config:
	cabal configure --disable-library-profiling --disable-shared --enable-tests --enable-benchmarks

.PHONY: build
build: config
	cabal build

.PHONY: test
test: build
	dist/build/tests/tests

.PHONY: benchmark
benchmark: build
	dist/build/bench/bench
