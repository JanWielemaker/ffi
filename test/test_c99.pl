:- use_module('../prolog/c99_tokens').
:- use_module('../prolog/c99_phrase').
:- use_module('../prolog/c99_decls').

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
`).
p(3, `
`).
p(4, `
`).







