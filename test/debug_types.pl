:- use_module('../prolog/cinvoke').
:- use_module('../prolog/cdecls').
:- use_module(library(pprint)).

t :-
    c99_types("#include \"test/test_struct.c\"",
              [ set_point
              ],
              Types),
    print_term(Types, []).
