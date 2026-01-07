# Test makefile with .DEFAULT rule

CC = gcc

all: prog

prog: main.o utils.o
	$(CC) -o prog main.o utils.o

.DEFAULT:
	@echo "Building $< with default rule"
	$(CC) -c $<
