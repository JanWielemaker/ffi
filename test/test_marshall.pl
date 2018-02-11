/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_marshall,
          [ test_marshall/0
          ]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module('../prolog/ffi').

test_marshall :-
    run_tests([ marshall
              ]).

:- c_import("#include \"test_marshall.c\"",
            [ test_marshall ],
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

              free(*void),
              test_transfer_none_in(string),
              test_transfer_full_in(*char),
              test_transfer_none_in_out(*(*char)),
              test_transfer_full_in_out(*(*char)),
              tests_array_transfer_none_in(*(*char)),
              tests_array_transfer_full_in(*(*char)),
              test_array_transfer_container_in(*(*char)),
              test_array_transfer_none_out(-(*(*char))),
              test_array_transfer_full_out(-(~(*(*char), free))),
              test_array_transfer_container_out(-(~(*(*char), free))),
              test_array_transfer_none_in_out(*(*(*char))),
              test_array_transfer_full_in_out(*(*(*char))),
              test_array_transfer_container_in_out(*(*(*char))),
              test_transfer_dangling_out(-string)
            ]).

:- begin_tests(marshall).

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
test(test_transfer_full_in) :-
    c_alloc_string(S, "foo", utf8),
    c_disown(S),
    test_transfer_full_in(S).
test(test_transfer_none_in_out, S2 == "bar") :-
    c_alloc_string(S, "foo", utf8),
    c_alloc(Ptr, *char = S),
    test_transfer_none_in_out(Ptr),
    c_load(Ptr, Out),
    c_load_string(Out, S2, string, utf8).
test(test_transfer_full_in_out, S2 == "bar") :-
    c_alloc_string(S, "foo", utf8),
    c_alloc(Ptr, *char = S),
    c_disown(S),
    test_transfer_full_in_out(Ptr),
    c_load(Ptr, Out),
    c_load_string(Out, S2, string, utf8),
    free(Out).
                                               % containers
test(tests_array_transfer_none_in) :-
    c_alloc_string(Foo, "foo", utf8),
    c_alloc_string(Bar, "bar", utf8),
    c_alloc(Ptr, (*char)[] = [Foo,Bar]),
    tests_array_transfer_none_in(Ptr),
    assertion(no_assertion).
test(tests_array_transfer_full_in) :-
    c_alloc_string(Foo, "foo", utf8),
    c_alloc_string(Bar, "bar", utf8),
    c_alloc(Ptr, (*char)[] = [Foo,Bar]),
    c_disown(Foo),
    c_disown(Bar),
    c_disown(Ptr),
    tests_array_transfer_full_in(Ptr),
    assertion(no_assertion).
test(test_array_transfer_container_in) :-
    c_alloc_string(Foo, "foo", utf8),
    c_alloc_string(Bar, "bar", utf8),
    c_alloc(Ptr, (*char)[] = [Foo,Bar]),
    c_disown(Ptr),
    test_array_transfer_container_in(Ptr),
    assertion(no_assertion).
test(test_array_transfer_none_out, [F,B] == ["foo", "bar"]) :-
    test_array_transfer_none_out(Ptr),
    c_load(Ptr[0], SPtr0), c_load_string(SPtr0, F, string, utf8),
    c_load(Ptr[1], SPtr1), c_load_string(SPtr1, B, string, utf8).
test(test_array_transfer_full_out, [F,B] == ["foo", "bar"]) :-
    test_array_transfer_full_out(Ptr),
    c_load(Ptr[0], SPtr0), c_load_string(SPtr0, F, string, utf8), free(SPtr0),
    c_load(Ptr[1], SPtr1), c_load_string(SPtr1, B, string, utf8), free(SPtr1).
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
test(test_array_transfer_full_in_out, [[F,B] == ["FOO", "BAR"]]) :-
    c_alloc_string(Foo, "foo", utf8),
    c_alloc_string(Bar, "bar", utf8),
    c_alloc(Ptr0, (*char)[] = [Foo,Bar]),
    c_alloc(Ptr, *(*char) = Ptr0),
    c_disown(Foo),
    c_disown(Bar),
    c_disown(Ptr0),
    test_array_transfer_full_in_out(Ptr),
    assertion(no_assertion),
    c_load(Ptr[0], Ptr1),
    c_load(Ptr1[0], SPtr0), c_load_string(SPtr0, F, string, utf8),
    c_load(Ptr1[1], SPtr1), c_load_string(SPtr1, B, string, utf8),
    free(SPtr0),
    free(SPtr1),
    free(Ptr1).
test(test_array_transfer_container_in_out, [[F,B] == ["FOO", "BAR"]]) :-
    c_alloc_string(Foo, "foo", utf8),
    c_alloc_string(Bar, "bar", utf8),
    c_alloc(Ptr0, (*char)[] = [Foo,Bar]),
    c_alloc(Ptr, *(*char) = Ptr0),
    c_disown(Ptr0),
    test_array_transfer_container_in_out(Ptr),
    assertion(no_assertion),
    c_load(Ptr[0], Ptr1),
    c_load(Ptr1[0], SPtr0), c_load_string(SPtr0, F, string, utf8),
    c_load(Ptr1[1], SPtr1), c_load_string(SPtr1, B, string, utf8),
    free(Ptr1).
test(test_transfer_dangling_out, error(domain_error(non_null_pointer,_))) :-
    test_transfer_dangling_out(_).

:- end_tests(marshall).

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
