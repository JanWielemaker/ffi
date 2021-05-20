:- module(test_ccallback,
          [ test_ccallback/0
          ]).
:- use_module('../prolog/ffi').
:- use_module(library(plunit)).

/** <module> Test c callback handling

This test demonstrates c callback handling,
 both   passing  a c callback  to a
function and storing one in a struct field.
*/

test_ccallback :-
    run_tests([ ccallback ]).

:- c_import("#include \"test_ccallback.c\"",
            [ test_ccallback ],
            [ test_passcallback('C'(ctwice(int, [int])), int, [int]),
              test_passcallback(null, int, [int]) as test_passcallbacknull,
              test_instruct(struct(funcs),  [int])
            ]).


:- begin_tests(ccallback).

test(funcasparam, R == 4) :-
    test_passcallback(2, R).

test(struct_func, R =:= 4) :-
    I = 4,
    c_alloc(FPtr, struct(funcs)),
    c_store(FPtr[cb], 'C'(sym(ctwice))),
    c_store(FPtr[value], I),
    test_instruct(FPtr, R).

test(null_callback, R == 9) :-
    test_passcallbacknull(3, R).



:- end_tests(ccallback).
