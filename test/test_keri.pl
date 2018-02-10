:- use_module(library(plunit)).
:- use_module('../prolog/ffi').

test_keri :-
    run_tests([ keri
              ]).

:- c_import("#include \"test_keri.c\"",
            [ test_keri ],
            [ test_int_return([-int])
            ]).

:- begin_tests(keri).

test(test_int_return, X == 42) :-
    test_int_return(X).

:- end_tests(keri).
