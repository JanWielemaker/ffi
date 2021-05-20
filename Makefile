LD=$(SWIPL)-ld
LDSOFLAGS=-Wall -shared -gdwarf-2 -g3
CC=gcc
MAKE=make
PACKSODIR=lib/$(SWIARCH)
FFI4PL=lib/$(SWIARCH)/ffi4pl.$(SOEXT)
LIBS=-lffi
CFLAGS=-shared -fPIC
TESTS=test_mode test_marshall test_bool test_enum test_struct test_union test_funcptr test_ccallback
TESTSO=$(addprefix test/$(SWIARCH)/, $(addsuffix .$(SOEXT), $(TESTS)))

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

test/$(SWIARCH)/%.$(SOEXT): test/%.c
	mkdir -p test/$(SWIARCH)
	$(CC) $(CFLAGS) -o $@ $<

$(TESTSO): env


tags:
	etags c/*.[ch]

check:	$(TESTSO)
	$(SWIPL) -q -g test_cmem -t halt test/test_cmem.pl
	$(SWIPL) -q -g test_mode -t halt test/test_mode.pl
	$(SWIPL) -q -g test_marshall -t halt test/test_marshall.pl
	$(SWIPL) -q -g test_bool -t halt test/test_bool.pl
	$(SWIPL) -q -g test_enum -t halt test/test_enum.pl
	$(SWIPL) -q -g test_struct -t halt test/test_struct.pl
	$(SWIPL) -q -g test_union -t halt test/test_union.pl
	$(SWIPL) -q -g test_funcptr -t halt test/test_funcptr.pl
	$(SWIPL) -q -g test_qsort -t halt test/test_qsort.pl
	$(SWIPL) -q -g test_libc -t halt test/test_libc.pl
	$(SWIPL) -q -g test_ccallback -t halt test/test_ccallback.pl

install::

clean:
	rm -f *~
	rm -f test/*.$(SOEXT)

distclean: clean
	rm -f $(FFI4PL)

