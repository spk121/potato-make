.POSIX
# Comprehensive test of all special targets

CC = gcc
CFLAGS = -Wall -O2

.PHONY: all clean test install
.IGNORE: clean

all: prog test

prog: main.o utils.o
	$(CC) -o prog main.o utils.o

.c.o:
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f prog *.o
	rm -f nonexistent.file

test: prog
	./prog

.IGNORE:

install: prog
	cp prog /usr/local/bin
	mkdir -p /tmp/test || true

.DEFAULT:
	@echo "Using default rule for $<"
	$(CC) -c $<
