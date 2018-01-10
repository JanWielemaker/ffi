:- use_module(library(plunit)).
:- use_module('../prolog/cinvoke').

test_enum :-
    run_tests([c_enum]).

:- c_import("#include \"test/test_enum.c\"",
            [ 'test/test_enum' ],
            [ set_dow(+(*(enum(dow))), +enum(dow)),
              get_dow(+(*(enum(dow))), [-enum(dow)]),
              get_dow2(+(*(enum(dow))), -enum(dow))
            ]).

:- begin_tests(c_enum).

test(set, Day == tuesday) :-
    c_alloc(P, enum(dow)),
    set_dow(P, tuesday),
    c_load(P, Day).

:- end_tests(c_enum).

