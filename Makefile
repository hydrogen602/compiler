.PHONY: build xclean clean run lib

SRCS = $(shell find src -type f -name '*.hs')

GENERATED = Lexer.hs Grammar.hs

export PATH := /opt/homebrew/opt/llvm@11/bin:$(PATH)
export LDFLAGS := -L/opt/homebrew/opt/llvm@11/lib
export CPPFLAGS := -I/usr/homebrew/opt/llvm@11/include
SHELL := env PATH=$(PATH) /bin/bash


EXTRAS = $(shell find libc -type f -name '*.o')

# Compiling

run: a.out
	./a.out

out.ll: ${SRCS} ${GENERATED}
	cabal run exe:compiler -- -i test_basic.idk -o out.ll

out.o: out.ll
	llc out.ll -filetype=obj

a.out: out.o lib
	@# I can't figure out llvm-link
	ld out.o ${EXTRAS} -lSystem -L$(shell xcode-select -p)/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib/


# Haskell building & other setup

lib:
	$(MAKE) -C libc

build: ${SRCS} ${GENERATED}
	cabal build exe:compiler

Lexer.hs: Lexer.x
	alex Lexer.x --outfile=src/Lexer.hs

Grammar.hs: Grammar.y src/Token.hs src/Lexer.hs
	happy Grammar.y --outfile=src/Grammar.hs

# cleanup

clean:
	cabal clean
	rm -f *.ll *.o 
	$(MAKE) -C libc clean

xclean: clean
	rm -f ${GENERATED}
