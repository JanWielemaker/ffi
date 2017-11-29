/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(dyncall,
          [ dc_bind/4                   % :Goal, +Signature, +File, +Func
          ]).

/** <module> Bind Prolog predicates to C functions
*/

:- use_foreign_library('lib/x86_64-linux/dc4pl').

:- multifile
    user:file_search_path/2.

user:file_search_path(dc, '/lib/x86_64-linux-gnu').


		 /*******************************
		 *    LIBRARIES AND SYMBOLS	*
		 *******************************/

%!  dc_load_library(+Path, -Handle)
%
%   Load the given library

%!  dc_free_library(+Handle)
%
%   Free a given library

%!  dc_find_symbol(+Handle, +Name, -FuncPtr)
%
%   True when FuncPtr is a pointer to Name in the library Handle

%!  dc_file_handle(+Base, -FHandle)
%
%   Find a file handle for a foreign library

:- dynamic  dc_file_handle_cache/2.
:- volatile dc_file_handle_cache/2.

dc_file_handle(Base, FHandle) :-
    dc_file_handle_cache(Base, FHandle0),
    !,
    FHandle = FHandle0.
dc_file_handle(Base, FHandle) :-
    with_mutex(dyncall, dc_file_handle_sync(Base, FHandle)).

dc_file_handle_sync(Base, FHandle) :-
    dc_file_handle_cache(Base, FHandle0),
    !,
    FHandle = FHandle0.
dc_file_handle_sync(Base, FHandle) :-
    absolute_file_name(dc(Base), Path,
                       [ access(read),
                         file_type(executable)
                       ]),
    dc_load_library(Path, FHandle),
    assertz(dc_file_handle_cache(Base, FHandle)).


		 /*******************************
		 *            BIND		*
		 *******************************/

%!  dc_bind(:Goal, +Signature, +File, +Func)

dc_bind(Goal, Signature, File, Func) :-
    throw(error(context_error(nodirective,
                              dc_bind(Goal, Signature, File, Func)), _)).

system:term_expansion((:- dc_bind(Goal, Signature, File, Func)),
                      Clause) :-
    dc_expand(Goal, Signature, File, Func, Clause).

dc_expand(Goal, Signature, File, Func,
          (Goal :- dyncall:dc_call(FuncPtr, Signature, Goal))) :-
    dc_file_handle(File, FH),
    writeln(FH),
    dc_find_symbol(FH, Func, FuncPtr).

