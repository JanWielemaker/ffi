# Dynamic calling C from Prolog

This package deals with calling C   functions from shared objects (DLLs)
from Prolog without writing wrappers. There   are several libraries that
allows for creating an argument vector for  a C function dynamically and
call      the      function.       One        such       library      is
[libffi](https://sourceware.org/libffi/).

Giving such a  library,  calling  C   functions  with  arithmetic  types
(integers, floats) is easy, but we  still   have  to  deal with structs,
unions, enums and the fact  that  most   C  libraries  do not define the
target type directly but use some `typedef` that binds the concrete type
depending on the platform. This package parses  the C header file (`.h`)
or even a concrete C source file   to extract the actual function return
and parameter types and all type definition   that are involved in these
types. In addition it allows fetching  constants defined using `#define`
macros.

Below is an example making the POSIX statfs() function available.

```{prolog}
:- use_module(library(ffi)).

:- c_import("#include <sys/vfs.h>",
            [ libc ],
            [ statfs(string, -struct(statfs), [int])
            ]).

statfs(File, FsStat) :-
    statfs(File, FsStat, Status),
    posix_status(Status, statfs, file, File).
```

The returned pointer may be handed to   c_load/2  to extract fields. For
example, to get the number  of  available   blocks  on  the current file
system we can make the  call  below.  Note   that  we  did  not make any
declarations about the structure or the involved integer types.

```{prolog}
?- statfs(".", FsStat), c_load(FsStat[f_bavail], A).
FsStat = <C struct statfs[1]>(0x55a2c9d5bae0),
A = 66197957.
```

## Installation

  - Make sure [libffi](https://sourceware.org/libffi/) is installed.
    This is available as package on various systems.
  - Clone this repo
  - Make a link from the `.../swipl/pack/ffi` to the cloned directory
  - Run in Prolog: `?- pack_rebuild(ffi).`

### Windows installation

  - DDLs can be cross-compiled using Linux and MinGW using
    `Makefile.mingw`.  Please check `Makefile.mingw`.
  - Precompiled versions, including precompiled DLLs for the tests
    and available from
    https://github.com/JanWielemaker/ffi/wiki/files/ffi4swipl-windows.zip
  - Requires SWI-Prolog 7.7.10 or later.

### Mac installation

  - MacOS (at least Mojave 10.14.3) includes an outdated version of libffi
  - A more recent version must be installed with Homebrew or MacpPort.
  - With home brew the command is 
  ```{prolog}
  brew install libffi
  ```
  - Go to the `ffi` pack directory
  - Execute the shell command
  ```{bash}
  export C_INCLUDE_PATH=/usr/local/Cellar/libffi/3.2.1/lib/libffi-3.2.1/include
  ```
  - Modify the Makefile file as follows: 
    - replace the lines
      ```{bash}
      LD=$(SWIPL)-ld
      LDSOFLAGS=-Wall -shared -O2 -gdwarf-2 -g3
      CC=gcc
      ```
      with the line
      ```{bash}
      LDSOFLAGS += -Wall -shared -O2 -gdwarf-2 -g3 -L/usr/local/opt/libffi/lib/ ${SWISOLIB}
      ```
    - replace the line
      ```{bash}
      CFLAGS=-shared -fPIC
      ```
      with the line
      ```{bash}
      CFLAGS += -shared -fPIC -I/usr/local/Cellar/libffi/3.2.1/lib/libffi-3.2.1/include
      ```
  - Modify `configure.ac`: replace the line
  `AC_CHECK_HEADERS(ffi.h ffi/ffi.h)`
  with 
  `AC_CHECK_HEADERS(ffi.h)`
  - Run 
  ```{bash}
autoconf
source configure
make
  ```


## Documentation

There is not yet public documentation.   With all proper tools installed
(Prolog, LaTeX) and a Linux machine you should be able to type `make` in
the `doc` directory to build the (incomplete) documentation.

Here is a [PDF
version](https://github.com/JanWielemaker/ffi/wiki/files/ffi.pdf),
created at Feb 18, 2018

## Status

  - Portability
    - Tested on Linux (Fedora 26 and Ubuntu 17.10), MacOSX and Windows
      using MinGW. Mainly limited by [libffi](https://sourceware.org/libffi/).

  - Functionality
    - All major foreseen features are implemented.
    - It is likely that high-level conversions such as between a Prolog
      list and C array will be added.  Such conversions are easily
      written in Prolog though.

  - API Stability
    - Overall the API is not likely to change much.  Possibly the
      `*` as prefix operator will be changed to postfix.  One may
      wish to write `*(Type)` to avoid ambiguity.

  - Documentation
    - First incomplete draft.  Please also see the directories
      `test` and `examples` for concrete code examples.  Notably
      `examples/python` provides a rudimentary Python interface.

  - Testing
    - Only basic functionality is tested.  Many more combinatins of
      types and different ways to define them need to be tested.


