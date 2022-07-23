.PHONY: build xclean run

SRCS = $(shell find app -type f -name '*.hs')

GENERATED = Lexer.hs Grammar.hs

export PATH := /opt/homebrew/opt/llvm@11/bin:$(PATH)
export LDFLAGS := -L/opt/homebrew/opt/llvm@11/lib
export CPPFLAGS := -I/usr/homebrew/opt/llvm@11/include

run: ${SRCS} ${GENERATED}
	cabal run compiler -- -i test.idk

build: ${SRCS} ${GENERATED}
	cabal build compiler

Lexer.hs: Lexer.x
	alex Lexer.x --outfile=app/Lexer.hs

Grammar.hs: Grammar.y app/Token.hs app/Lexer.hs
	happy Grammar.y --outfile=app/Grammar.hs

xclean: clean
	rm -f ${GENERATED}
