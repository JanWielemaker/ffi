:- use_module(library(apply_macros)).
:- use_module(library(statistics)).

:- use_module('../prolog/cinvoke').
:- use_module('../prolog/cerror').
:- use_module('../prolog/c99_tokens').
:- use_module('../prolog/c99_phrase').
:- use_module('../prolog/c99_decls').

:- c_import("#include \"test/test.c\"",
            [ 'test/test.so' ],
            [ get_point(-struct(point), [int]),
              set_point(+struct(point), +int, +int),
              add_point(+int, +int),
              get_points([*(struct(points))]),
              set_dow(+pointer, +enum(dow))
            ]).

testpt :-
    c99_types("#include \"test/test.c\"",
              [ get_points,
                add_point
              ], AST),
    pp(AST).


testpt_ast(AST) :-
    c99_header_ast("#include \"test/test.c\"", AST).

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



