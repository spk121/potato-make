# Test makefile with .POSIX in wrong position (should fail)

CC = gcc

.POSIX:

all: test
	$(CC) -o test test.c
