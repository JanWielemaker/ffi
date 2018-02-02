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

:- module(test_iconv,
          [ test_iconv/0
          ]).
:- use_module(iconv).
:- use_module(library(plunit)).
:- use_module(library(random)).
:- use_module(library(apply)).
:- use_module(library(apply_macros), []).

test_iconv :-
    run_tests([iconv]).

		 /*******************************
		 *             TEST		*
		 *******************************/

:- begin_tests(iconv).

test(read, Read == Data) :-
    mkdata(100 000, Data),
    setup_call_cleanup(
        tmp_file_stream(utf8, File, Out),
        format(Out, '~s', [Data]),
        close(Out)),
    call_cleanup(
        iconv_read_file(File, 'UTF-8', Read),
        delete_file(File)).
test(write, Read == Data) :-
    mkdata(1000, Data),
    setup_call_cleanup(
        tmp_file_stream(octet, File, Out),
        setup_call_cleanup(
            iconv_open(Out, write, 'UTF-8', UOut),
            format(UOut, '~s', [Data]),
            close(UOut)),
        close(Out)),
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        read_string(In, _, Read),
        close(In)).


:- end_tests(iconv).

iconv_read_file(File, Enc, String) :-
    setup_call_cleanup(
        iconv_open(File, read, Enc, In),
        read_string(In, _, String),
        close(In)).

iconv_write_file(File, Enc, Data) :-
    setup_call_cleanup(
        iconv_open(File, write, Enc, Out),
        format(Out, '~s', [Data]),
        close(Out)).

mkdata(Size, String) :-
    length(Codes, Size),
    maplist(random_between(1, 0x04000), Codes),
    string_codes(String, Codes).
