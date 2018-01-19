SWIPL=swipl
LD=$(SWIPL)-ld
LDSOFLAGS=-Wall -shared -O2
CC=gcc
MAKE=make
ARCH=$(shell $(SWIPL) -arch)
PACKSODIR=lib/$(ARCH)
FFI4PL=lib/$(ARCH)/ffi4pl.$(SOEXT)
LIBS=-lffi
CFLAGS=-shared -fPIC
TESTSO=	test/test_struct.$(SOEXT) \
	test/test_union.$(SOEXT) \
	test/test_enum.$(SOEXT)

all:	$(FFI4PL)

$(FFI4PL): c/ffi4pl.c c/cmemory.c Makefile
	mkdir -p $(PACKSODIR)
	$(LD) $(LDSOFLAGS) -o $@ c/ffi4pl.c $(LIBS)

test/test.$(SOEXT): test/test.c
	$(CC) $(CFLAGS) -o $@ $<
test/test_struct.$(SOEXT): test/test_struct.c
	$(CC) $(CFLAGS) -o $@ $<
test/test_union.$(SOEXT): test/test_union.c
	$(CC) $(CFLAGS) -o $@ $<
test/test_enum.$(SOEXT): test/test_enum.c
	$(CC) $(CFLAGS) -o $@ $<

tags:
	etags c/*.[ch]

check:	$(TESTSO)
	$(SWIPL) -q -g test_cmem -t halt test/test_cmem.pl
	$(SWIPL) -q -g test_enum -t halt test/test_enum.pl
	$(SWIPL) -q -g test_struct -t halt test/test_struct.pl
	$(SWIPL) -q -g test_union -t halt test/test_union.pl
	$(SWIPL) -q -g test_libc -t halt test/test_libc.pl

install::

clean:
	rm -f *~
	rm -f test/*.$(SOEXT)

distclean: clean
	rm -f $(FFI4PL)

