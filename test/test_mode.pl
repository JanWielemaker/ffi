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

leak_iterations(100 000).

test_output_leaks(Iterations) :-
    retractall(leak_iterations(_)),
    asserta(leak_iterations(Iterations)),
    run_tests([c_mode_leak]).

:- c_import("#include \"test_mode.c\"",
            [ test_mode
            ],
            [ test_v_oi(-int),          % output
              test_v_os(-string),
              test_v_ofs(-string~myfree),
              test_v_oip(-(*(int))),
              test_v_ofip(-(*(int)~myfree)),
              test_v_ofsp(-((*struct(point))~myfree)),
                                        % return
              test_s([string]),
              test_fs([string~myfree])
            ]).

:- begin_tests(c_mode).

                                        % OUTPUT
test(test_v_oi, I == 42) :-
    test_v_oi(I).
test(test_v_os, S == "hello world") :-
    test_v_os(S).
test(test_v_ofs, S == "hello world") :-
    test_v_ofs(S).
test(test_v_oip, [E0,E1,E2] == [1,2,3]) :-
    test_v_oip(Ptr),
    c_load(Ptr[0], E0),
    c_load(Ptr[1], E1),
    c_load(Ptr[2], E2).
test(test_v_ofip, [E0,E1,E2] == [1,2,3]) :-
    test_v_oip(Ptr),
    c_load(Ptr[0], E0),
    c_load(Ptr[1], E1),
    c_load(Ptr[2], E2).
test(test_v_ofsp, [X,Y] == [1,2]) :-
    test_v_ofsp(Struct),
    c_load(Struct[x], X),
    c_load(Struct[y], Y).
                                        % RETURN
test(test_s, S == "hello world") :-
    test_s(S).
test(test_fs, S == "hello world") :-
    test_fs(S).

:- end_tests(c_mode).

:- begin_tests(c_mode_leak).

test(test_s) :-
    test_leak(test_s(_)).
test(test_v_ofsp) :-
    test_leak(test_v_ofsp(_)).

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
