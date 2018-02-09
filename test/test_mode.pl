:- module(test_mode,
          [ test_mode/0,
            test_output_leaks/1         % +Iterations
          ]).
:- use_module(library(plunit)).
:- use_module('../prolog/ffi').
:- use_module(procps).

test_mode :-
    run_tests([c_mode]).

:- dynamic
    leak_iterations/1.

test_output_leaks(Iterations) :-
    retractall(leak_iterations(_)),
    asserta(leak_iterations(Iterations)),
    run_tests([c_mode_leak]).

:- c_import("#include \"test_mode.c\"",
            [ 'test/test_mode',
              '-lc'
            ],
            [ test_v_oi(-int),
              test_v_os(-string),
              test_v_ofs(-string~free),
              test_s([string]),
              test_fs([string~free])
            ]).

:- begin_tests(c_mode).

test(test_v_oi, I == 42) :-
    test_v_oi(I).
test(test_v_os, S == "hello world") :-
    test_v_os(S).
test(test_v_ofs, S == "hello world") :-
    test_v_ofs(S).
test(test_s, S == "hello world") :-
    test_s(S).
test(test_fs, S == "hello world") :-
    test_fs(S).

:- end_tests(c_mode).

:- begin_tests(c_mode_leak).

test(test_s) :-
    test_leak(test_s(_)).

:- end_tests(c_mode_leak).

:- meta_predicate
    test_leak(0),
    test_leak(0,+).

test_leak(G) :-
    leak_iterations(Times),
    test_leak(G, Times).

test_leak(G, Times) :-
    rss(RSS0),
    forall(between(1, Times, _), G),
    garbage_collect_atoms,
    rss(RSS1),
    RSS is RSS1 - RSS0,
    (   RSS > 2 000 000
    ->  print_message(warning, rss(G, RSS0, RSS1))
    ;   true
    ).

rss(RSS) :-
    procps_stat(Status),
    RSS = Status.rss.
