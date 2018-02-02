CFLAGS=$(shell pkg-config --cflags python3)
LIBS=$(shell pkg-config --libs python3)

all: env mypython.$(SOEXT)

ifeq ($(SOEXT),)
env::
	@echo "Please use . buildenv.sh to setup the environment"
	@exit 1
else
env::
endif

mypython.$(SOEXT): mypython.c
	gcc -fPIC -shared -o $@ $(CFLAGS) $< $(LIBS)

