CFLAGS=$(shell pkg-config --cflags python3)
LIBS=$(shell pkg-config --libs python3)

all: mypython.$(SOEXT)

mypython.$(SOEXT): mypython.c
	gcc -shared -o $@ $(CFLAGS) $< $(LIBS)

