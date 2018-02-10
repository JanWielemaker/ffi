:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module('../prolog/ffi').

test_keri :-
    run_tests([ keri
              ]).

:- c_import("#include \"test_keri.c\"",
            [ test_keri ],
            [ get_assertion([*struct(assert_data)]),
              test_int_return([-int]),
              test_int_in(int),
              test_int_out(-int),
              test_int_in_out(*int),
              test_null_return([string]),
              test_null_in(string)
            ]).

:- begin_tests(keri).

test(test_int_return, X == 42) :-
    test_int_return(X).
test(test_int_in) :-
    test_int_in(42),
    assertion(no_assertion).
test(test_int_out, X == 42) :-
    test_int_out(X).
test(test_int_in_out, X == 24) :-
    c_alloc(Ptr, int=42),
    test_int_in_out(Ptr),
    c_load(Ptr, X).

test(test_null_return, error(domain_error(non_null_pointer, _))) :-
    test_null_return(S),
    c_is_nil(S).
test(test_null_in) :-
    c_nil(NULL),
    test_null_in(NULL),
    assertion(no_assertion).

:- end_tests(keri).

no_assertion :-
    get_assertion(Assertion),
    (   c_is_nil(Assertion)
    ->  true
    ;   c_load(Assertion[assertion], P0), c_load_string(P0, Message, string, text),
        c_load(Assertion[file], P1),      c_load_string(P1, File, string, text),
        c_load(Assertion[line], Line),
        c_load(Assertion[function], P2),  c_load_string(P2, Function, string, text),
        print_message(error, c_assertion(File:Line, Function, Message)),
        fail
    ).

:- multifile prolog:message//1.

prolog:message(c_assertion(File:Line, Function, Message)) -->
    [ '~w:~d: in function ~w() C assert() failure:'-[File, Line, Function], nl,
      '  ~w'-[Message]
    ].
