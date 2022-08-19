# C Flags
CFLAGS = -Wall -pedantic -g -DSANITY_CHECK
CC = clang
LDLIBS = -lm

# MEM_DEBUG will set the pre-processor macro MDEBUG, which adds
# debugging printfs to track allocation/deallocation of memory
ifdef MEM_DEBUG
	CFLAGS := -DMDEBUG ${CFLAGS}
endif

# Env
export PATH := /opt/homebrew/opt/llvm@11/bin:$(PATH)
SHELL := env PATH=$(PATH) /bin/bash

# Files - FIXME: replace wildcard with find
BUILD_DIR := build
SRCS := $(wildcard *.c)
OBJS := $(SRCS:%.c=$(BUILD_DIR)/%.o)


.PHONY: clean xclean symbols

libc.a: ${OBJS}
	llvm-ar r libc.a ${OBJS}

symbols:
	llvm-nm libc.a

$(BUILD_DIR)/%.o: %.c $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR):
	mkdir $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)/*

xclean: clean
	rm -f *.a