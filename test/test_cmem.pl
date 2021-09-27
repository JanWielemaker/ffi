:- module(test_cmem,
          [ test_cmem/0
          ]).

:- use_module('../prolog/ffi').
:- use_module(library(plunit)).

test_cmem :-
    run_tests([ cmem
              ]).

:- begin_tests(cmem).

test(unsigned_signed_char, Out == -1) :-
    c_alloc(Ptr, uchar),
    c_store(Ptr, 0, uchar, 255),
    c_load(Ptr, 0, schar, Out).

test(range, error(domain_error(offset, _))) :-
    c_alloc(Ptr, char),
    c_store(Ptr, 1, char, a).

:- end_tests(cmem).

