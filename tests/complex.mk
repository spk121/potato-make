# Complex test makefile
CC = gcc
CXX = g++
CFLAGS = -Wall -O2
CXXFLAGS = $(CFLAGS) -std=c++11
LDFLAGS = -lm

OBJS = main.o utils.o helper.o

.PHONY: all clean install

all: myapp

myapp: $(OBJS)
	$(CXX) -o $@ $^ $(LDFLAGS)

main.o: main.cpp
	$(CXX) $(CXXFLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<

.cpp.o:
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -f myapp $(OBJS)

install: myapp
	cp myapp /usr/local/bin/
