:- use_module('../prolog/cinvoke').

:- c_import("#include \"test/test_union.c\"",
            [ 'test/test_union' ],
            [ set_d(-union(convert), +double)
            ]).

t(Float, Union) :-
    set_d(Union, Float).


