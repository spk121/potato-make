.POSIX
# Test makefile with .POSIX as first non-comment line

CC = gcc
CFLAGS = -g -O2

all: test.o
	$(CC) -o test test.o

test.o: test.c
	$(CC) $(CFLAGS) -c test.c
