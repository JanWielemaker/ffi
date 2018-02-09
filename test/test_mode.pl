:- module(test_mode,
          [ test_mode/0
          ]).

:- use_module(library(plunit)).
:- use_module('../prolog/ffi').

test_mode :-
    run_tests([c_mode]).

:- c_import("#include \"test_mode.c\"",
            [ 'test/test_mode' ],
            [ test_v_oi(-int)
            ]).

:- begin_tests(c_mode).

test(test_v_oi, I == 42) :-
    test_v_oi(I).

:- end_tests(c_mode).
