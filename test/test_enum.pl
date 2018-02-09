:- module(test_enum,
          [ test_enum/0
          ]).

:- use_module(library(plunit)).
:- use_module('../prolog/ffi').

test_enum :-
    run_tests([c_enum]).

:- c_import("#include \"test_enum.c\"",
            [ 'test/test_enum' ],
            [ set_dow(*(enum(dow)), enum(dow)),
              get_dow(*(enum(dow)), [enum(dow)]),
              get_dow2(*(enum(dow)), -enum(dow))
            ]).

:- begin_tests(c_enum).

test(return, Day == tuesday) :-
    c_alloc(P, enum(dow)),
    set_dow(P, tuesday),
    get_dow(P, Day).
test(output, Day == tuesday) :-
    c_alloc(P, enum(dow)),
    set_dow(P, tuesday),
    get_dow2(P, Day).
test(load, Day == tuesday) :-
    c_alloc(P, enum(dow)),
    set_dow(P, tuesday),
    c_load(P, Day).
test(init, Day == tuesday) :-
    c_alloc(P, enum(dow) = tuesday),
    c_load(P, Day).

:- end_tests(c_enum).

