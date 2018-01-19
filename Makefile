SWIPL=swipl
LD=$(SWIPL)-ld
LDSOFLAGS=-Wall -shared -O2
CC=gcc
MAKE=make
ARCH=$(shell $(SWIPL) -arch)
PACKSODIR=lib/$(ARCH)
C4PL=lib/$(ARCH)/cinvoke4pl.so
CIFLAGS=-Icinvoke-1.0/lib
LIBS=-Lcinvoke-1.0/lib -lcinvoke
CILIB=cinvoke-1.0/lib/libcinvoke.a
CFLAGS=-shared -fPIC
SOEXT=so
TESTSO=	test/test_struct.$(SOEXT) \
	test/test_union.$(SOEXT) \
	test/test_enum.$(SOEXT)

all:	$(C4PL)

$(C4PL): c/cinvoke4pl.c c/cmemory.c Makefile $(CILIB)
	mkdir -p $(PACKSODIR)
	$(LD) $(LDSOFLAGS) $(CIFLAGS) -o $@ c/cinvoke4pl.c $(LIBS)

$(CILIB): cinvoke-1.0/Makefile
	$(MAKE) -C cinvoke-1.0

cinvoke-1.0/Makefile: cinvoke-1.0/Makefile.templ
	(cd cinvoke-1.0 && ./configure.pl)

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
	$(SWIPL) -q -g test_libc -t halt test/test_libc.pl
	$(SWIPL) -q -g test_struct -t halt test/test_struct.pl
	$(SWIPL) -q -g test_union -t halt test/test_union.pl

install::

clean:
	rm -f $(C4PL) *~
	rm -f test/*.$(SOEXT)
	if [ -r cinvoke-1.0/Makefile ]; then $(MAKE) -C cinvoke-1.0 clean; fi

distclean: clean
	rm -f $(C4PL) *~
	(cd cinvoke-1.0 && ./configure.pl --distclean)

