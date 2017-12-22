SWIPL=swipl
ARCH=$(shell swipl -arch)
C4PL=lib/$(ARCH)/cinvoke4pl.so
CIFLAGS=-Icinvoke-1.0/lib
LIBS=-Lcinvoke-1.0/lib -lcinvoke

all: $(C4PL)
$(C4PL): c/cinvoke4pl.c c/c_memory.c Makefile
	mkdir -p lib/$(ARCH)
	swipl-ld -g -shared $(CIFLAGS) -o $@ $< $(LIBS)

clean:
	rm -f $(C4PL) *~

tags:
	etags c/*.[ch]
