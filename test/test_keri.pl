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
              test_null_return([*char]),
              test_null_return([string]) as test_null_return_string,
              test_null_in(*char),
              test_null_in(string) as test_null_in_string,
              test_null_out(-(*char)),
              test_null_out(-string) as test_null_out_string,
              test_null_in_out(*(*char)),
              test_transfer_none_in(string),
              test_transfer_full_in(string),            % TBD: How to handle this?
              test_transfer_none_in_out(*(*char)),
              test_transfer_full_in_out(*(*char)),      % TBD: How to handle this?
              tests_array_transfer_none_in(*(*char)),
              tests_array_transfer_full_in(*(*char)),   % TBD: How to handle this?
              test_array_transfer_container_in(*(*char)), % TBD: How to handle this?
              test_array_transfer_none_out(-(*(*char))),
              test_array_transfer_full_out(-(~(*(*char), free))),
              test_array_transfer_container_out(-(~(*(*char), free))),
              test_array_transfer_none_in_out(*(*(*char)))
            ]).

:- begin_tests(keri).

                                                % basic int in/out
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
                                                % NULL in/out
test(test_null_return) :-
    test_null_return(S),
    c_is_nil(S).
test(test_null_return_string, error(domain_error(non_null_pointer, _))) :-
    test_null_return_string(S),
    c_is_nil(S).
test(test_null_in) :-
    c_nil(NULL),
    test_null_in(NULL),
    assertion(no_assertion).
test(test_null_in_string, error(type_error(text, _))) :-
    c_nil(NULL),
    test_null_in_string(NULL).
test(test_null_out) :-
    test_null_out(X),
    assertion(c_is_nil(X)).
test(test_null_out_string, error(domain_error(non_null_pointer, _))) :-
    test_null_out_string(_).
test(test_null_in_out) :-
    c_nil(Nil),
    c_alloc(Ptr, *char = Nil),
    test_null_in_out(Ptr),
    c_load(Ptr, Out),
    assertion(c_is_nil(Out)).
                                                % memory management
test(test_transfer_none_in) :-
    test_transfer_none_in("foo"),
    assertion(no_assertion).
test(test_transfer_full_in, blocked("Not yet implemented")) :-
    test_transfer_full_in("foo").
test(test_transfer_none_in_out, S2 == "bar") :-
    c_alloc_string(S, "foo", utf8),
    c_alloc(Ptr, *char = S),
    test_transfer_none_in_out(Ptr),
    c_load(Ptr, Out),
    c_load_string(Out, S2, string, utf8).
test(test_transfer_full_in_out, [S2 == bar, blocked("Not yet implemented")]) :-
    c_alloc_string(S, "foo", utf8),
    c_alloc(Ptr, *char = S),
    test_transfer_none_in_out(Ptr),
    c_load(Ptr, Out),
    c_load_string(Out, S2, string, utf8).
                                                % containers
test(tests_array_transfer_none_in) :-
    c_alloc_string(Foo, "foo", utf8),
    c_alloc_string(Bar, "bar", utf8),
    c_alloc(Ptr, (*char)[] = [Foo,Bar]),
    tests_array_transfer_none_in(Ptr),
    assertion(no_assertion).
test(tests_array_transfer_full_in, blocked("Not yet implemented")) :-
    c_alloc_string(Foo, "foo", utf8),
    c_alloc_string(Bar, "bar", utf8),
    c_alloc(Ptr, (*char)[] = [Foo,Bar]),
    tests_array_transfer_none_in(Ptr),
    assertion(no_assertion).
test(test_array_transfer_container_in, blocked("Not yet implemented")) :-
    c_alloc_string(Foo, "foo", utf8),
    c_alloc_string(Bar, "bar", utf8),
    c_alloc(Ptr, (*char)[] = [Foo,Bar]),
    tests_array_transfer_none_in(Ptr),
    assertion(no_assertion).
test(test_array_transfer_none_out, [F,B] == ["foo", "bar"]) :-
    test_array_transfer_none_out(Ptr),
    c_load(Ptr[0], SPtr0), c_load_string(SPtr0, F, string, utf8),
    c_load(Ptr[1], SPtr1), c_load_string(SPtr1, B, string, utf8).
test(test_array_transfer_full_out, [F,B] == ["foo", "bar"]) :-
    test_array_transfer_full_out(Ptr),
    c_load(Ptr[0], SPtr0), c_load_string(SPtr0, F, string, utf8),
    c_load(Ptr[1], SPtr1), c_load_string(SPtr1, B, string, utf8).
    /* TBD: we must free SPtr0 and SPtr1 */
test(test_array_transfer_container_out, [F,B] == ["foo", "bar"]) :-
    test_array_transfer_container_out(Ptr),
    c_load(Ptr[0], SPtr0), c_load_string(SPtr0, F, string, utf8),
    c_load(Ptr[1], SPtr1), c_load_string(SPtr1, B, string, utf8).
test(test_array_transfer_none_in_out, [[F,B] == ["FOO", "BAR"]]) :-
    c_alloc_string(Foo, "foo", utf8),
    c_alloc_string(Bar, "bar", utf8),
    c_alloc(Ptr0, (*char)[] = [Foo,Bar]),
    c_alloc(Ptr, *(*char) = Ptr0),
    test_array_transfer_none_in_out(Ptr),
    assertion(no_assertion),
    c_load(Ptr[0], Ptr1),
    c_load(Ptr1[0], SPtr0), c_load_string(SPtr0, F, string, utf8),
    c_load(Ptr1[1], SPtr1), c_load_string(SPtr1, B, string, utf8).


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
