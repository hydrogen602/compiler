.PHONY: build xclean clean run docs

SRCS = $(shell find src -type f -name '*.hs')

GENERATED = src/Lexer.hs src/Grammar.hs

export PATH := /opt/homebrew/opt/llvm@11/bin:$(PATH)
export LDFLAGS := -L/opt/homebrew/opt/llvm@11/lib
export CPPFLAGS := -I/usr/homebrew/opt/llvm@11/include
SHELL := env PATH=$(PATH) /bin/bash

# Configs
BUILD_DIR = build
EXE_NAME = ${BUILD_DIR}/main

# Compiling

run: ${EXE_NAME}
	@./${EXE_NAME}

docs:
	cabal haddock

# Note: changing code to produce an executable and then trying to link it is dumb

${EXE_NAME}: ${SRCS} ${GENERATED} test_basic.idk
	cabal run exe:compiler -- -i test_basic.idk -o $@

# Haskell building & other setup

build: ${SRCS} ${GENERATED}
	cabal build exe:compiler

src/Lexer.hs: Lexer.x
	alex Lexer.x --outfile=$@

src/Grammar.hs: Grammar.y src/Token.hs src/Lexer.hs
	happy Grammar.y --outfile=$@

# cleanup

clean:
	cabal clean
	rm -f *.ll *.o
	rm -f build/*
	$(MAKE) -C libc clean

xclean: clean
	rm -f ${GENERATED} ${EXE_NAME}
	$(MAKE) -C libc xclean
