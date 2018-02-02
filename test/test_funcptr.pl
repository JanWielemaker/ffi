:- use_module('../prolog/ffi').

/** <module> Test closure handling

This test demonstrates closure handling,  both   passing  a closure to a
function and storing one in a struct field.
*/

:- c_import("#include \"test/test_funcptr.c\"",
            [ 'test/test_funcptr' ],
            [ test(twice(int, [int]), int, [int]),
              test_fstruct(struct(funcs), float, int, [float])
            ]).

twice(In, Out) :-
    Out is 2*In.

t(N) :-
    test(N, X),
    writeln(X).

tf(D,I,R) :-
    c_alloc(FPtr, struct(funcs)),
    c_store(FPtr[mul_di], mul_di(float, int, [float])),
    test_fstruct(FPtr, D, I, R).
