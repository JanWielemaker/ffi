SWIPL=swipl
ARCH=$(shell swipl -arch)
C4PL=lib/$(ARCH)/cinvoke4pl.so
CIFLAGS=-Icinvoke-1.0/lib
LIBS=-Lcinvoke-1.0/lib -lcinvoke

all: $(C4PL) test/test_struct.so test/test_union.so test/test_enum.so

$(C4PL): c/cinvoke4pl.c c/c_memory.c Makefile
	mkdir -p lib/$(ARCH)
	swipl-ld -Wall -g -shared $(CIFLAGS) -o $@ $< $(LIBS)

test/test.so: test/test.c
	gcc -shared -fPIC -o $@ $<
test/test_struct.so: test/test_struct.c
	gcc -shared -fPIC -o $@ $<
test/test_union.so: test/test_union.c
	gcc -shared -fPIC -o $@ $<
test/test_enum.so: test/test_enum.c
	gcc -shared -fPIC -o $@ $<

clean:
	rm -f $(C4PL) *~

tags:
	etags c/*.[ch]

check:
	swipl -q -g test_cmem -t halt test/test_cmem.pl
	swipl -q -g test_enum -t halt test/test_enum.pl
	swipl -q -g test_libc -t halt test/test_libc.pl
	swipl -q -g test_struct -t halt test/test_struct.pl
	swipl -q -g test_union -t halt test/test_union.pl
