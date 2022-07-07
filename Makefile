.PHONY: all clean xclean run


SRCS = $(shell find . -type f -name '*.hs')

GENERATED = Lexer.hs Grammar.hs

export PATH := /opt/homebrew/opt/llvm@11/bin:$(PATH)
# export PATH="/opt/homebrew/opt/llvm@11/bin:$PATH"

# all: Main
# 	@echo "Running Main"
# 	@./Main

run: Main
	./Main -i test.idk

Main: ${SRCS} ${GENERATED}
	ghc Main.hs -hidir build -odir build

Lexer.hs: Lexer.x
	alex Lexer.x

Grammar.hs: Grammar.y Token.hs Lexer.hs
	happy Grammar.y

Grammar: Grammar.hs
	ghc Grammar.hs

clean:
	rm -rf Main build/*

xclean: clean
	rm -f ${GENERATED}
