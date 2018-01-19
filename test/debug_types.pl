:- use_module('../prolog/ffi').
:- use_module('../prolog/cdecls').
:- use_module(library(pprint)).

:- c_import("#include \"test/test_struct.c\"",
            [ 'test/test_struct' ],
            [ dim_dim(+int, -int)
            ]).

t :-
    c99_types("#include \"test/test_struct.c\"",
              [ set_point
              ],
              Types),
    print_term(Types, []).
