# C Flags
CPPFLAGS = -Wall -pedantic -g
CC = clang
LDLIBS = -lm

# Env
export PATH := /opt/homebrew/opt/llvm@11/bin:$(PATH)
SHELL := env PATH=$(PATH) /bin/bash

# Files
SRCS := $(wildcard *.c)
OBJS = $(SRCS:.c=.o)

.PHONY: clean xclean symbols

libc.a: ${OBJS}
	llvm-ar r libc.a ${OBJS}

symbols:
	llvm-nm libc.a

$(objects): %.o: %.c

clean:
	rm -f *.o

xclean: clean
	rm -f *.a