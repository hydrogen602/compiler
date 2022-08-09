.PHONY: build xclean clean run

SRCS = $(shell find src -type f -name '*.hs')

GENERATED = src/Lexer.hs src/Grammar.hs

export PATH := /opt/homebrew/opt/llvm@11/bin:$(PATH)
export LDFLAGS := -L/opt/homebrew/opt/llvm@11/lib
export CPPFLAGS := -I/usr/homebrew/opt/llvm@11/include
SHELL := env PATH=$(PATH) /bin/bash

LIB = libc/libc.a

# Configs
BUILD_DIR = build
EXE_NAME = ${BUILD_DIR}/main

# Compiling

run: ${EXE_NAME}
	@./${EXE_NAME}

# out.ll: ${SRCS} ${GENERATED}
# 	cabal run exe:compiler -- -i test_basic.idk -o out.ll -c

${BUILD_DIR}/out.o: ${SRCS} ${GENERATED} test_basic.idk
	cabal run exe:compiler -- -i test_basic.idk -o $@

${EXE_NAME}: ${BUILD_DIR}/out.o ${LIB}
	clang ${BUILD_DIR}/out.o ${LIB} -o $@


# Haskell building & other setup

${LIB}:
	$(MAKE) -C libc

# build: ${SRCS} ${GENERATED}
# 	cabal build exe:compiler

src/Lexer.hs: Lexer.x
	alex Lexer.x --outfile=$@

src/Grammar.hs: Grammar.y src/Token.hs src/Lexer.hs
	happy Grammar.y --outfile=$@

# cleanup

clean:
	cabal clean
	rm -f *.ll *.o
	$(MAKE) -C libc clean

xclean: clean
	rm -f ${GENERATED} ${EXE_NAME}
	$(MAKE) -C libc xclean
