:- module(test_array,
          [ test_array/0
          ]).
:- use_module('../prolog/ffi').
:- use_module(library(plunit)).

:- c_import("#include \"test_array.c\"",
            [ test_array ],
            [ test_intarray([*int]),
              test_fltarray(*float,[void])
            ]).

test_array :-
    run_tests([array]).

:- begin_tests(array).
% TBD: the real tests.  Now only tests loading.
:- end_tests(array).
