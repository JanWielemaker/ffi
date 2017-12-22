:- use_module('../prolog/cinvoke').
:- use_module('../prolog/c99_tokens').
:- use_module('../prolog/c99_phrase').
:- use_module('../prolog/c99_decls').

:- c_import("#include <sys/types.h>
             #include <sys/stat.h>
             #include <unistd.h>",
            [ 'libc.so.6' ],
            [ stat(+string,-struct(stat),[-int])]).

:- c_import("#include <sys/vfs.h>",
            [ 'libc.so.6' ],
            [ statfs(+string, -struct(statfs), [-int]) ]).

:- c_import("#include <math.h>",
            [ 'libm.so.6' ],
            [ sin(+double, [-double]) ]).

:- c_import("#include \"test/test.c\"",
            [ 'test/test.so' ],
            [ get_point(-struct(point), [-int])
            ]).

sfs(File, Data) :-
    c_alloc_string(S, File, text),
    c_struct_alloc(Data, statfs),
    pp(Data),
    statfs(S, Data, Status),
    writeln(Status).

pt(Pt) :-
    c_struct_alloc(Pt, point),
    get_point(Pt, Status),
    writeln(Status).

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

tpt :-
    c99_types("#include \"test/test.c\"",
              [ get_point ], AST),
    pp(AST).


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
extern int __fpclassifyf128 (double __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));
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



