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
            c_import/3,                 % +Header, +Libs, +Functions

                                        % Memory access predicates
            c_alloc/3,                  % -Ptr, +Type, +Size
            c_free/1,                   % +Ptr
            c_typeof/2,                 % +Ptr, -Type
            c_load/4,                   % +Ptr, +Offset, +Type, -Value
            c_store/4,                  % +Ptr, +Offset, +Type, +Value
            c_sizeof/2,                 % +Type, -Bytes
            c_alignof/2,                % +Type, -Bytes

            c_struct/2,                 % +Name, +Fields
            c_struct_alloc/2,           % -Ptr, +Name
            c_struct_load/3,            % +Ptr, +Field, -Value
            c_struct_store/3            % +Ptr, +Field, +Value
          ]).
:- use_module(library(error)).
:- use_module(c99_decls).

/** <module> Bind Prolog predicates to C functions
*/

:- use_foreign_library('lib/x86_64-linux/cinvoke4pl').

:- multifile
    user:file_search_path/2,
    system:term_expansion/2,
    user:exception/3.


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


		 /*******************************
		 *             IMPORT		*
		 *******************************/

%!  c_import(+Header, +Libs, +Functions)
%
%   Import Functions as predicates from Libs   based  on the declaration
%   from Header.

c_import(Header, Libs, Functions) :-
        throw(error(context_error(nodirective,
                                  c_import(Header, Libs, Functions)), _)).

system:term_expansion((:- c_import(Header, Libs, Functions)),
                      Clauses) :-
    c99_types(Header, Functions, Types),
    phrase(compile_types(Types), Clauses, Rest1),
    phrase(wrap_functions(Functions, Types), Rest1, Rest2),
    phrase(libs(Libs), Rest2).

compile_types([]) --> [].
compile_types([struct(Name,Fields)|T]) --> !,
    struct_compile(Name, Fields),
    compile_types(T).
compile_types([_|T]) --> !,
    compile_types(T).

wrap_functions([], _) --> [].
wrap_functions([H|T], Types) -->
    wrap_function(H, Types), wrap_functions(T, Types).

wrap_function(Name, Types) -->
    { memberchk(function(Name, Ret, Params), Types),
      Signature =.. [Name|Params]
    },
    [ '$c_function'(Signature, Ret) ].

libs([]) --> [].
libs([H|T]) --> [ '$c_lib'(H) ], libs(T).


%!  user:exception(+Type, +Context, -Action) is semidet.

user:exception(undefined_predicate, PI, retry) :-
    pi_head(PI, M:Head),
    '$c_current_predicate'(_, M:'$c_function'(_,_)),
    M:'$c_function'(Head, _Ret),
    debug(ctypes, 'Defining ~p', [M:Head]),
    fail.

pi_head(Module:Name/Arity, Module:Head) :-
    functor(Head, Name, Arity).
pi_head(Name/Arity, user:Head) :-
    functor(Head, Name, Arity).


		 /*******************************
		 *          STRUCTURES		*
		 *******************************/

%!  c_struct(+Name, +Fields)
%
%   Declare a C structure with name  Name.   Fields  is  a list of field
%   specifications of the form:
%
%     - f(Name, Type)
%
%   Where Type is one of
%
%     - A primitive type (`char`, `uchar`, ...)
%     - struct(Name)
%     - union(Name)
%     - enum(Name)
%     - *(Type)
%     - array(Type, Size)
%
%   A structure declaration is compiled into a number of clauses

c_struct(Name, Fields) :-
    throw(error(context_error(nodirective, c_struct(Name, Fields)), _)).

system:term_expansion((:- c_struct(Name, Fields)), Clauses) :-
    phrase(struct_compile(Name, Fields), Clauses).

struct_compile(Name, Fields) -->
    field_clauses(Fields, Name, 0, End, 0, Alignment),
    { Size is Alignment*((End+Alignment-1)//Alignment) },
    [ '$c_struct'(Name, Size, Alignment) ].

field_clauses([], _, End, End, Align, Align) --> [].
field_clauses([f(Name,Type)|T], Struct, Off0, Off, Align0, Align) -->
    { alignof(Type, Alignment),
      Align1 is max(Align0, Alignment),
      Off1 is Alignment*((Off0+Alignment-1)//Alignment),
      sizeof(Type, Size),
      Off2 is Off1 + Size
    },
    [ '$c_struct_field'(Struct, Name, Off1, Type) ],
    field_clauses(T, Struct, Off2, Off, Align1, Align).


alignof(Type, Alignment) :-
    c_alignof(Type, Alignment),
    !.
alignof(struct(Name), Alignment) :-
    '$c_struct'(Name, _Size, Alignment),
    !.
alignof(Type, _Alignment) :-
    existence_error(type, Type).

sizeof(Type, Size) :-
    c_sizeof(Type, Size),
    !.
sizeof(struct(Name), Size) :-
    '$c_struct'(Name, Size, _Alignment),
    !.
sizeof(Type, _Alignment) :-
    existence_error(type, Type).


%!  c_struct(?Name, ?Size, ?Align)
%
%   Total size of the struct in bytes and alignment restrictions.

%!  c_struct_field(?Name, ?Field, ?Offset, ?Type)
%
%   Fact to provide efficient access to fields

%!  c_struct_alloc(-Ptr, +Name) is det.
%
%   Allocate a C structure

c_struct_alloc(Ptr, Name) :-
    '$c_struct'(Name, Size, _Align),
    !,
    c_alloc(Ptr, Name, Size).
c_struct_alloc(_Ptr, Name) :-
    existence_error(struct, Name).

%!  c_struct_load(+Ptr, +Field, -Value) is det.
%
%   Load the value of a field from a C structure.

c_struct_load(Ptr, Field, Value) :-
    c_typeof(Ptr, Name),
    '$c_struct_field'(Name, Field, Offset, Type),
    c_load(Ptr, Offset, Type, Value).

%!  c_struct_store(+Ptr, +Field, +Value) is det.
%
%   Store Value into Field of a structure

c_struct_store(Ptr, Field, Value) :-
    c_typeof(Ptr, Name),
    '$c_struct_field'(Name, Field, Offset, Type),
    c_store(Ptr, Offset, Type, Value).

