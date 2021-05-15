/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- use_module(library(plunit)).
:- use_module('../prolog/ffi').
:- use_module('../prolog/cerror').

%!  test_libc
%
%   Test some libc functions. We try to deal with stat(), but this seems
%   hard as the structure  has  evolved  in   many  days  over  time and
%   different OSes use different techniques  to   deal  with  that, upto
%   inline functions in the header.  Currently stat/3 fails on MacOS.

test_libc :-
    run_tests([c_libc]).

cpp_const('_STAT_VER').

:- c_import("#include <sys/types.h>
             #include <sys/stat.h>
             #include <unistd.h>",
            [ libc ],
            [ ['__xstat'(+int,+string,-struct(stat),[-int])],
              [stat(+string,-struct(stat),[-int])]
            ]).

% __has_include(Header) is gcc 5
:- c_import("#if __has_include(<sys/vfs.h>)
             #include <sys/vfs.h>
             #endif",
            [ libc ],
            [ [statfs(+string, -struct(statfs), [-int])]
            ]).

:- c_import("#include <math.h>",
            [ libm ],
            [ sin(+double, [-double])
            ]).

:- c_import("#include <ctype.h>",
            [ libc ],
            [ toupper(+int, [-int])
            ]).

:- c_import("#include <wchar.h>",
            [ libc ],
            [ wcslen(+string(wchar_t), [-int])
            ]).

:- begin_tests(c_libc).

test(sin, Native == V) :-
    sin(4.5, V),
    Native is sin(4.5).
:- if(\+current_prolog_flag(apple, true)).
test(stat, Size == Native) :-
    once(source_file(_:stat(_,_), File)),
    stat(File, Stat),
    c_load(Stat[st_size], Size),
    size_file(File, Native).
:- endif.
test(upper, A == 0'A) :-
    toupper(0'a, A).
test(strupr, Upper == "HELLO") :-
    strupr("hello", Upper).
test(wcslen, Len == 5) :-
    wcslen("hello", Len).
test(wcslen, Len == 50) :-
    numlist(1001, 1050, L),
    wcslen(L, Len).

:- end_tests(c_libc).

:- if(current_predicate('__xstat'/4)).
stat(File, Stat) :-
    '__xstat'('_STAT_VER', File, Stat, Status),
    posix_status(Status, stat, file, File).
:- elif(current_predicate(stat/3)).
stat(File, Stat) :-
    stat(File, Stat, Status),
    posix_status(Status, stat, file, File).
:- endif.

statfs(File, FsStat) :-
    statfs(File, FsStat, Status),
    posix_status(Status, statfs, file, File).

strupr(In, Out) :-
    c_alloc_string(Ptr, In, text),
    between(0, infinite, I),
        c_load(Ptr[I], C),
        (   C == 0
        ->  !,
            c_load_string(Ptr, Out, string, text)
        ;   toupper(C, U),
            c_store(Ptr[I], U),
            fail
        ).
