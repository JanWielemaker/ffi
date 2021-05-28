:- module(test_funcptr,
          [ test_funcptr/0
          ]).
:- use_module('../prolog/ffi').
:- use_module(library(plunit)).

/** <module> Test closure handling

This test demonstrates closure handling,  both   passing  a closure to a
function and storing one in a struct field.
*/

test_funcptr :-
    run_tests([ funcptr ]).

:- c_import("#include \"test_funcptr.c\"",
            [ test_funcptr ],
            [ test_fi(twice(int, [int]), int, [int]),
              test_fi_param_typedef(twice(int, [int]), int, [int]),
              test_fstruct(struct(funcs), float, [float])
            ]).

twice(In, Out) :-
    Out is 2*In.

:- begin_tests(funcptr).

test(funcparam, R == 4) :-
    test_fi(2, R).

test(funcparam_typedef, R == 4) :-
    test_fi_param_typedef(2, R).


test(struct_func, R =:= 4*3.4) :-
    I = 4,
    D = 3.4,
    c_alloc(FPtr, struct(funcs)),
    c_store(FPtr[mul_di], mul_di(float, int, [float])),
    c_store(FPtr[times], I),
    test_fstruct(FPtr, D, R).

mul_di(D, I, R) :-
    R is D*I.

:- end_tests(funcptr).
