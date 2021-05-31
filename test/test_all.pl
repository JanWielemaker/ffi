:- use_module(test_cmem).
:- use_module(test_mode).
:- use_module(test_marshall).
:- use_module(test_enum).
:- use_module(test_struct).
:- use_module(test_union).
:- use_module(test_funcptr).
:- use_module(test_ccallback).
:- use_module(test_array).

test_all :-
    test_cmem,
    test_mode,
    test_marshall,
    test_enum,
    test_struct,
    test_union,
    test_funcptr,
    test_ccallback,
    test_array.

