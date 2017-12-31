:- use_module(library(apply_macros)).
:- use_module(library(statistics)).

:- use_module('../prolog/cinvoke').
:- use_module('../prolog/cerror').
:- use_module('../prolog/c99_tokens').
:- use_module('../prolog/c99_phrase').
:- use_module('../prolog/c99_decls').

cpp_const('_STAT_VER').

:- c_import("#include <sys/types.h>
             #include <sys/stat.h>
             #include <unistd.h>",
            [ 'libc.so.6' ],
            [ '__xstat'(+int,+string,-struct(stat),[-int])
            ]).

:- c_import("#include <sys/vfs.h>",
            [ 'libc.so.6' ],
            [ statfs(+string, -struct(statfs), [-int])
            ]).

:- c_import("#include <math.h>",
            [ 'libm.so.6' ],
            [ sin(+double, [-double])
            ]).

:- c_import("#include \"test/test.c\"",
            [ 'test/test.so' ],
            [ get_point(-struct(point), [-int]),
              set_point(+struct(point), +int, +int)
            ]).

stat(File, Stat) :-
    '__xstat'('_STAT_VER', File, Stat, Status),
    posix_status(Status, stat, file, File).

statfs(File, FsStat) :-
    statfs(File, FsStat, Status),
    posix_status(Status, statfs, file, File).

ptn(N) :-
    time(forall(between(1, N, _), get_point(_,_))).

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

p(2, `
struct x { int x; int f[4]; };
`).
p(3, `
extern int __fpclassifyf128 (_Float128 __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));
`).
p(4, `
`).


incl(AST) :-
    phrase_from_file(c99_parse(AST), 'incl.h', []).

m(AST) :-
    phrase_from_file(c99_parse(AST), 'm.cpp', []).



