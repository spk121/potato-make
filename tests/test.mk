# Simple test makefile
CC = gcc
CFLAGS = -g -O2

all: foo

foo: foo.o bar.o
$(CC) -o $@ $^

.c.o:
$(CC) $(CFLAGS) -c $<

clean:
rm -f foo foo.o bar.o
