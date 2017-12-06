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

:- module(cinvoke,
          [ dc_bind/4,                  % :Goal, +Signature, +File, +Func

                                        % Memory access predicates
            c_alloc/3,                  % -Ptr, +Type, +Size
            c_free/1,                   % +Ptr
            c_typeof/2,                 % +Ptr, -Type
            c_load/4,                   % +Ptr, +Offset, +Type, -Value
            c_store/4,                  % +Ptr, +Offset, +Type, +Value
            c_sizeof/2,                 % +Type, -Bytes
            c_alignof/2                 % +Type, -Bytes
          ]).

/** <module> Bind Prolog predicates to C functions
*/

:- use_foreign_library('lib/x86_64-linux/cinvoke4pl').

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

%!  ic_context(-Context)
%
%   Global context for the cinvoke library

:- dynamic  ci_context_cache/1.
:- volatile ci_context_cache/1.

ci_context(Ctx) :-
    ci_context_cache(Ctx0),
    !,
    Ctx = Ctx0.
ci_context(Ctx) :-
    with_mutex(cinvoke, ci_context_sync(Ctx0)),
    Ctx = Ctx0.

ci_context_sync(Ctx) :-
    ci_context_cache(Ctx),
    !.
ci_context_sync(Ctx) :-
    ci_context_create(Ctx),
    asserta(ci_context_cache(Ctx)).


%!  ci_library(+Base, -FHandle)
%
%   Find a file handle for a foreign library

:- dynamic  ci_library_cache/2.
:- volatile ci_library_cache/2.

ci_library(Base, FHandle) :-
    ci_library_cache(Base, FHandle0),
    !,
    FHandle = FHandle0.
ci_library(Base, FHandle) :-
    with_mutex(dyncall, ci_library_sync(Base, FHandle)).

ci_library_sync(Base, FHandle) :-
    ci_library_cache(Base, FHandle0),
    !,
    FHandle = FHandle0.
ci_library_sync(Base, FHandle) :-
    absolute_file_name(dc(Base), Path,
                       [ access(read),
                         file_type(executable)
                       ]),
    ci_context(Ctx),
    ci_library_create(Ctx, Path, FHandle),
    assertz(ci_library_cache(Base, FHandle)).


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
          (Goal :- cinvoke:ci_function_invoke(Prototype, Goal))) :-
    split_string(Signature, ")", "", [Params, Ret]),
    ci_library(File, FH),
    ci_library_load_entrypoint(FH, Func, FuncPtr),
    ci_function_create(FuncPtr, cdecl, Ret, Params, Prototype).


