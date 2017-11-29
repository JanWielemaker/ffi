SWIPL=swipl
ARCH=$(shell swipl -arch)
DC4PL=lib/$(ARCH)/dc4pl.so
CIFLAGS=-Idyncall/dynload -Idyncall/dyncall
DYNLIBS=dyncall/dyncall/libdyncall_s.a \
	dyncall/dynload/libdynload_s.a

all: $(DC4PL)
$(DC4PL): c/dc4pl.c Makefile
	mkdir -p lib/$(ARCH)
	swipl-ld -O2 -shared $(CIFLAGS) -o $@ $< $(DYNLIBS)
