# Test makefile with .DEFAULT with prerequisites (should fail)

CC = gcc

.DEFAULT: main.c
	$(CC) -c $<

all: test
	$(CC) -o test test.o
