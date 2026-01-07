# Test makefile with .IGNORE targets

.IGNORE: clean distclean

all: prog
	gcc -o prog main.c

clean:
	rm -f prog main.o
	rm -f nonexistent.file

distclean: clean
	rm -f config.log

.IGNORE:

test:
	false
	echo "This should still run"
