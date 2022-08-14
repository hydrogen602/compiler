.PHONY: test_c test_my_lang test_py all

all: test_py test_c test_my_lang

test_py:
	python3 fibR.py > py.out

test_c: fibR_c
	time ./fibR_c > c.out

test_my_lang: fibR
	time ./fibR > my_lang.out

# Compiling
fibR_c:
	clang fibR.c -o fibR_c

fibR:
	cd .. && cabal run exe:compiler -- -i comparison/fibR.idk -o comparison/fibR