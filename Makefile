LD=$(SWIPL)-ld
LDSOFLAGS=-Wall -shared -gdwarf-2 -g3
CC=gcc
MAKE=make
PACKSODIR=lib/$(SWIARCH)
FFI4PL=lib/$(SWIARCH)/ffi4pl.$(SOEXT)
LIBS=-lffi
CFLAGS=-shared -fPIC
TESTS=test_mode test_marshall test_bool test_enum test_struct test_union    \
      test_funcptr test_ccallback test_array
TESTSO=$(addprefix test/$(SWIARCH)/, $(addsuffix .$(SOEXT), $(TESTS)))
TESTFLAGS=-q -t halt -f none

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
	$(SWIPL) $(TESTFLAGS) -g test_cmem test/test_cmem.pl
	$(SWIPL) $(TESTFLAGS) -g test_mode test/test_mode.pl
	$(SWIPL) $(TESTFLAGS) -g test_marshall test/test_marshall.pl
	$(SWIPL) $(TESTFLAGS) -g test_bool test/test_bool.pl
	$(SWIPL) $(TESTFLAGS) -g test_enum test/test_enum.pl
	$(SWIPL) $(TESTFLAGS) -g test_struct test/test_struct.pl
	$(SWIPL) $(TESTFLAGS) -g test_union test/test_union.pl
	$(SWIPL) $(TESTFLAGS) -g test_funcptr test/test_funcptr.pl
	$(SWIPL) $(TESTFLAGS) -g test_qsort test/test_qsort.pl
	$(SWIPL) $(TESTFLAGS) -g test_libc test/test_libc.pl
	$(SWIPL) $(TESTFLAGS) -g test_ccallback test/test_ccallback.pl
	$(SWIPL) $(TESTFLAGS) -g test_array test/test_array.pl

install::

clean:
	rm -f *~
	rm -f test/*.$(SOEXT)

distclean: clean
	rm -f $(FFI4PL)

