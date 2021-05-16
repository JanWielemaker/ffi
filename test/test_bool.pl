:- module(test_bool,
          [ test_bool/0
          ]).

:- use_module(library(plunit)).
:- use_module('../prolog/ffi').

test_bool :-
    run_tests([c_bool]).

:- c_import("#include \"test_bool.c\"",
            [ test_bool ],
            [ set_bool(bool),
              get_bool_ret([bool]),
              get_bool_arg(-bool)
            ]).

:- begin_tests(c_bool).

test(setget_ret, X == true) :-
    set_bool(true),
    get_bool_ret(X).
test(setget_ret, X == false) :-
    set_bool(false),
    get_bool_ret(X).
test(setget_arg, X == true) :-
    set_bool(true),
    get_bool_arg(X).
test(setget_arg, X == false) :-
    set_bool(false),
    get_bool_arg(X).

:- end_tests(c_bool).

