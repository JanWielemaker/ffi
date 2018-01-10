:- use_module(library(apply_macros)).
:- use_module(library(statistics)).

:- use_module('../prolog/cinvoke').
:- use_module('../prolog/c99_decls').

:- c_import("#include \"test/test_union.c\"",
            [ 'test/test_union.so' ],
            [ set_d(-union(convert), +double)
            ]).

t(Float, Union) :-
    set_d(Union, Float).

decls :-
    c99_types("#include \"test/test_union.c\"",
              [ set_d ], AST),
    print_term(AST, []).

