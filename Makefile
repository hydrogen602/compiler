.PHONY: all clean

SRCS = $(wildcard *.hs)

GENERATED = Lexer.hs Grammar.hs

# all: Main
# 	@echo "Running Main"
# 	@./Main

Main: ${SRCS} ${GENERATED}
	ghc Main.hs

Lexer.hs: Lexer.x
	alex Lexer.x

Grammar.hs: Grammar.y Token.hs Lexer.hs
	happy Grammar.y

Grammar: Grammar.hs
	ghc Grammar.hs

clean:
	rm -f *.o *.hi Main 

xclean: clean
	rm -f ${GENERATED}