SWIPL=swipl
LD=$(SWIPL)-ld
LDSOFLAGS=-Wall -shared -O2 -gdwarf-2 -g3
CC=gcc
MAKE=make
ARCH=$(shell $(SWIPL) -arch)
PACKSODIR=lib/$(ARCH)
FFI4PL=lib/$(ARCH)/ffi4pl.$(SOEXT)
LIBS=-lffi
CFLAGS=-shared -fPIC
TESTSO=	test/test_struct.$(SOEXT) \
	test/test_union.$(SOEXT) \
	test/test_enum.$(SOEXT) \
	test/test_funcptr.$(SOEXT)

all:	env $(FFI4PL)

ifeq ($(SOEXT),)
env::
	@echo "Please use . buildenv.sh to setup the environment"
	@exit 1
else
env::
endif

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
test/test_funcptr.$(SOEXT): test/test_funcptr.c
	$(CC) $(CFLAGS) -o $@ $<

tags:
	etags c/*.[ch]

check:	$(TESTSO)
	$(SWIPL) -q -g test_cmem -t halt test/test_cmem.pl
	$(SWIPL) -q -g test_enum -t halt test/test_enum.pl
	$(SWIPL) -q -g test_struct -t halt test/test_struct.pl
	$(SWIPL) -q -g test_union -t halt test/test_union.pl
	$(SWIPL) -q -g test_funcptr -t halt test/test_funcptr.pl
	$(SWIPL) -q -g test_qsort -t halt test/test_qsort.pl
	$(SWIPL) -q -g test_libc -t halt test/test_libc.pl

install::

clean:
	rm -f *~
	rm -f test/*.$(SOEXT)

distclean: clean
	rm -f $(FFI4PL)

