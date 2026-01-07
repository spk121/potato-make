# Example Makefile for a simple C project
CC = gcc
CFLAGS = -Wall -g

all: hello

hello: hello.o
	$(CC) -o $@ $^

.c.o:
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f hello hello.o
