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
            [ statfs(+string, -struct(statfs), [-int])
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
