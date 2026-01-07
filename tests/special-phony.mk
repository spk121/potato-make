# Test makefile with .PHONY targets

.PHONY: all clean

all: prog
	@echo "Building all"

prog: main.o
	gcc -o prog main.o

main.o: main.c
	gcc -c main.c

clean:
	rm -f prog main.o

.PHONY: install test

install: prog
	cp prog /usr/local/bin

test: prog
	./prog
