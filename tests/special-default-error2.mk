# Test makefile with .DEFAULT without commands (should fail)

CC = gcc

.DEFAULT:

all: test
	$(CC) -o test test.o
