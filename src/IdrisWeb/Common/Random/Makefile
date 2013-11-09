.PHONY : all

rand_c.o : rand_c.c rand_c.h
	gcc -c rand_c.c

rand_c.so : rand_c.c
	gcc - fPIC -o rand_c.so -shared rand_c.c

all : rand_c.o rand_c.so
