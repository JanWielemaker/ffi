:- use_module(library(apply_macros)).
:- use_module(library(statistics)).

:- use_module('../prolog/cinvoke').
:- use_module('../prolog/cerror').
:- use_module('../prolog/ctokens').
:- use_module('../prolog/cparser').
:- use_module('../prolog/cdecls').

cpp_const('_STAT_VER').

:- c_import("#include <sys/types.h>
             #include <sys/stat.h>
             #include <unistd.h>",
            [ libc ],
            [ '__xstat'(+int,+string,-struct(stat),[-int])
            ]).

:- c_import("#include <sys/vfs.h>",
            [ libc ],
            [ statfs(+string, -struct(statfs), [-int])
            ]).

:- c_import("#include <math.h>",
            [ libm ],
            [ sin(+double, [-double])
            ]).

:- c_import("#include <ctype.h>",
            [ libc ],
            [ toupper(+int, [-int])
            ]).

stat(File, Stat) :-
    '__xstat'('_STAT_VER', File, Stat, Status),
    posix_status(Status, stat, file, File).

statfs(File, FsStat) :-
    statfs(File, FsStat, Status),
    posix_status(Status, statfs, file, File).

strupr(In, Out) :-
    c_alloc_string(Ptr, In, text),
    between(0, infinite, I),
        c_load(Ptr[I], C),
        (   C == 0
        ->  !,
            c_load_string(Ptr, Out, string, text)
        ;   toupper(C, U),
            c_store(Ptr[I], U),
            fail
        ).

tmath :-
    c99_types("#include <math.h>",
              [ sin,cos ], AST),
    pp(AST).

tstat :-
    c99_types("#include <sys/types.h>
               #include <sys/stat.h>
               #include <unistd.h>",
              [ stat ], AST),
    pp(AST).

tstatfs :-
    c99_types("#include <sys/vfs.h>",
              [ statfs ], AST),
    pp(AST).

testpt :-
    c99_types("#include \"test/test.c\"",
              [ get_point ], AST),
    pp(AST).


testpt_ast(AST) :-
    c99_header_ast("#include \"test/test.c\"", AST).

tpt_ast(AST) :-
    c99_header_ast("#include \"t.c\"", AST).

tstat_ast(AST) :-
    c99_header_ast("#include <sys/types.h>
                    #include <sys/stat.h>
                    #include <unistd.h>",
                   AST).

tstrcpy :-
    c99_types("#include <string.h>",
              [ strcpy ], AST),
    pp(AST).


t(N) :-
    p(N, P),
    phrase(c99_parse(AST), P),
    pp(AST).

p(1, `
double sin(double x);
`).
