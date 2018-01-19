SWIPL=swipl
CC=gcc
MAKE=make
ARCH=$(shell $(SWIPL) -arch)
C4PL=lib/$(ARCH)/cinvoke4pl.so
CIFLAGS=-Icinvoke-1.0/lib
LIBS=-Lcinvoke-1.0/lib -lcinvoke
CILIB=cinvoke-1.0/lib/libcinvoke.a
CFLAGS=-shared -fPIC
SO=so

all: $(C4PL) test/test_struct.$(SO) test/test_union.$(SO) test/test_enum.$(SO)

$(C4PL): c/cinvoke4pl.c c/c_memory.c Makefile $(CILIB)
	mkdir -p lib/$(ARCH)
	$(SWIPL)-ld -Wall -g -shared $(CIFLAGS) -o $@ $< $(LIBS)

$(CILIB): cinvoke-1.0/Makefile
	$(MAKE) -C cinvoke-1.0

cinvoke-1.0/Makefile: cinvoke-1.0/Makefile.templ
	(cd cinvoke-1.0 && ./configure.pl)

test/test.$(SO): test/test.c
	$(CC) $(CFLAGS) -o $@ $<
test/test_struct.$(SO): test/test_struct.c
	$(CC) $(CFLAGS) -o $@ $<
test/test_union.$(SO): test/test_union.c
	$(CC) $(CFLAGS) -o $@ $<
test/test_enum.$(SO): test/test_enum.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(C4PL) *~
	rm test/*.$(SO)
	$(MAKE) -C cinvoke-1.0 clean

distclean: clean
	rm -f $(C4PL) *~
	(cd cinvoke-1.0 && ./configure.pl --distclean)

tags:
	etags c/*.[ch]

check:
	$(SWIPL) -q -g test_cmem -t halt test/test_cmem.pl
	$(SWIPL) -q -g test_enum -t halt test/test_enum.pl
	$(SWIPL) -q -g test_libc -t halt test/test_libc.pl
	$(SWIPL) -q -g test_struct -t halt test/test_struct.pl
	$(SWIPL) -q -g test_union -t halt test/test_union.pl
