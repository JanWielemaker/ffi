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
          [ c_import/3,                 % +Header, +Libs, +Functions

                                        % Memory access predicates
            c_alloc/3,                  % -Ptr, +Type, +Size
            c_free/1,                   % +Ptr
            c_typeof/2,                 % +Ptr, -Type
            c_load/4,                   % +Ptr, +Offset, +Type, -Value
            c_store/4,                  % +Ptr, +Offset, +Type, +Value
            c_sizeof/2,                 % +Type, -Bytes
            c_alignof/2,                % +Type, -Bytes

            c_struct/2,                 % +Name, +Fields

            c_current_struct/3,         % :Name, -Size, -Alignment
            c_current_struct_field/4,   % :Name, ?Field, ?Offset, ?Type

            c_struct_alloc/2,           % -Ptr, +Name
            c_struct_load/3,            % +Ptr, +Field, -Value
            c_struct_store/3,           % +Ptr, +Field, +Value

            c_alloc_string/3,           % -Ptr, +Data, +Encoding
            c_load_string/4,            % +Ptr, -Data, +Type, +Encoding

            c_errno/1                   % -Integer
          ]).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(c99_decls).

/** <module> Bind Prolog predicates to C functions
*/

:- meta_predicate
    c_current_struct(:,?,?),
    c_current_struct_field(:,?,?,?).


:- use_foreign_library('lib/x86_64-linux/cinvoke4pl').

:- multifile
    user:file_search_path/2,
    system:term_expansion/2,
    user:exception/3,
    c_function/3.


user:file_search_path(dc, '/lib/x86_64-linux-gnu').
user:file_search_path(dc, '.').


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
    prolog_load_context(module, M),
    maplist(functor_name, Functions, FunctionNames),
    add_constants(M, Header, HeaderConst),
    c99_types(HeaderConst, FunctionNames, Types, Constants),
    phrase(( c_constants(Constants),
             c_import(Libs, Functions, FunctionNames, Types)),
           Clauses).

functor_name(Spec, Name) :-
    functor(Spec, Name, _).

c_import(Libs, Functions, FunctionNames, Types) -->
    decls(Types),
    compile_types(Types, Types),
    wrap_functions(Functions, Types),
    libs(Libs, FunctionNames).

decls(_) -->
    [ (:- discontiguous(('$c_lib'/2,
                         '$c_struct'/3,
                         '$c_struct_field'/4))) ].

compile_types([], _) --> [].
compile_types([struct(Name,Fields)|T], Types) --> !,
    compile_struct(Name, Fields, Types),
    compile_types(T, Types).
compile_types([_|T], Types) --> !,
    compile_types(T, Types).

wrap_functions([], _) --> [].
wrap_functions([H|T], Types) -->
    wrap_function(H, Types), wrap_functions(T, Types).

wrap_function(Signature, Types) -->
    { Signature =.. [Name|SigArgs],
      memberchk(function(Name, Ret, Params), Types),
      length(SigArgs, Arity),
      matching_signature(Name, SigArgs, Ret, Params),
      functor(Head, Name, Arity),
      prolog_load_context(module, M)
    },
    [ cinvoke:c_function(M:Head, Params, Ret),
      (:- dynamic(Name/Arity)),
      (Head :- cinvoke:define(M:Head, SigArgs))
    ].

matching_signature(Name, SigArgs, Ret, Params) :-
    append(RealArgs, [_], SigArgs),
    !,
    (   same_length(RealArgs, Params)
    ->  true
    ;   print_message(error, cinvoke(nonmatching_params(SigArgs, Params))),
        fail
    ),
    (   Ret == void
    ->  print_message(error, cinvoke(void_function(Name))),
        fail
    ;   true
    ).


libs([], _) --> [].
libs([H|T], Functions) --> [ '$c_lib'(H, Functions) ], libs(T, Functions).

%!  define(:Signature, +Params, +Ret)
%
%   Actually link the C function

:- public
    define/2.

define(Signature, SigArgs) :-
    Signature = M:_Head,
    link_clause(Signature, SigArgs, Clause),
    asserta(M:Clause),
    call(Signature).

link_clause(M:Goal, SigArgs,
            (Head :- !, Body)) :-
    c_function(M:Goal, ParamSpec, RetType),
    pairs_values(ParamSpec, ParamTypes),
    phrase(signature_string(ParamTypes), ParamChars),
    atom_codes(Params, ParamChars),
    phrase(signature_string([RetType]), RetChars),
    atom_codes(Ret, RetChars),
    functor(Goal, Name, Arity),
    functor(Head, Name, Arity),
    functor(Head1, Name, Arity),
    (   M:'$c_lib'(Lib, Funcs),
        memberchk(Name, Funcs),
        ci_library(Lib, FH),
        ci_library_load_entrypoint(FH, Name, FuncPtr)
    ->  debug(ctypes, 'Binding ~p (Ret=~p, Params=~p)', [Name, Ret, Params]),
        ci_function_create(FuncPtr, cdecl, Ret, Params, Prototype)
    ;   existence_error(c_function, Name)
    ),
    convert_args(SigArgs, 1, Arity, Head, Head1, PreConvert, PostConvert),
    Invoke = cinvoke:ci_function_invoke(Prototype, Head1),
    mkconj(PreConvert, Invoke, Body0),
    mkconj(Body0, PostConvert, Body).

convert_args([], _, _, _, _, true, true).
convert_args([H|T], I, Arity, Head0, Head1, GPre, GPost) :-
    arg(I, Head0, Arg0),
    arg(I, Head1, Arg1),
    (   convert_arg(H, Arg0, Arg1, GPre1, GPost1)
    ->  true
    ;   Arg0 = Arg1,
        GPre1 = true,
        GPost1 = true
    ),
    I2 is I + 1,
    convert_args(T, I2, Arity, Head0, Head1, GPre2, GPost2),
    mkconj(GPre1, GPre2, GPre),
    mkconj(GPost1, GPost2, GPost).

convert_arg(-struct(Name), Ptr, Ptr, c_struct_alloc(Ptr, Name), true).
convert_arg(+string(Enc),  String, Ptr, c_alloc_string(Ptr, String, Enc), true).
convert_arg(+string, String, Ptr, Pre, Post) :-
    convert_arg(+string(text), String, Ptr, Pre, Post).

convert_arg([-string(Enc)], String, Ptr, true, c_load_string(Ptr, String, string, Enc)).
convert_arg([-string], String, Ptr, Pre, Post) :-
    convert_arg([-string(text)], String, Ptr, Pre, Post).


mkconj(true, G, G) :- !.
mkconj(G, true, G) :- !.
mkconj(G1, G2, (G1,G2)).

%!  signature_string(+Types)//
%
%   Get string description of the argument for the C layer

signature_string([]) --> [].
signature_string([H|T]) --> signature(H), signature_string(T).

signature(char)      --> "hhi".
signature(uchar)     --> "uhhi".
signature(short)     --> "hi".
signature(ushort)    --> "uhi".
signature(int)       --> "i".
signature(uint)      --> "ui".
signature(long)      --> "li".
signature(ulong)     --> "uli".
signature(longlong)  --> "lli".
signature(ulonglong) --> "ulli".
signature(float)     --> "f".
signature(double)    --> "lf".
signature(*(_))      --> "p".


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
    phrase(compile_structs([struct(Name, Fields)]), Clauses).

compile_structs(List) -->
    compile_structs(List, List).

compile_structs([], _) --> [].
compile_structs([struct(Name,Fields)|T], All) -->
    compile_struct(Name, Fields, All),
    compile_structs(T, All).

compile_struct(Name, Fields, All) -->
    field_clauses(Fields, Name, 0, End, 0, Alignment, All),
    { Size is Alignment*((End+Alignment-1)//Alignment) },
    [ '$c_struct'(Name, Size, Alignment) ].

field_clauses([], _, End, End, Align, Align, _) --> [].
field_clauses([f(Name,Type)|T], Struct, Off0, Off, Align0, Align, All) -->
    { type_size_align(Type, Alignment, Size, All),
      Align1 is max(Align0, Alignment),
      Off1 is Alignment*((Off0+Alignment-1)//Alignment),
      Off2 is Off1 + Size
    },
    [ '$c_struct_field'(Struct, Name, Off1, Type) ],
    field_clauses(T, Struct, Off2, Off, Align1, Align, All).


type_size_align(Type, Alignment, Size, _All) :-
    c_alignof(Type, Alignment),
    !,
    c_sizeof(Type, Size).
type_size_align(struct(Name), Alignment, Size, All) :-
    memberchk(struct(Name, Fields), All), !,
    phrase(compile_struct(Name, Fields, All), Clauses),
    memberchk('$c_struct'(Name, Size, Alignment), Clauses).
type_size_align(struct(Name, Fields), Alignment, Size, All) :-
    phrase(compile_struct(Name, Fields, All), Clauses),
    memberchk('$c_struct'(Name, Size, Alignment), Clauses).
type_size_align(struct(Name), Alignment, Size, _) :-
    '$c_struct'(Name, Size, Alignment),
    !.
type_size_align(array(Type,Len), Alignment, Size, All) :-
    !,
    type_size_align(Type, Alignment, Size0, All),
    Size is Size0*Len.
type_size_align(Type, _Alignment, _, _) :-
    existence_error(type, Type).

%!  c_current_struct(:Name, ?Size, ?Align)
%
%   Total size of the struct in bytes and alignment restrictions.

c_current_struct(M:Name, Size, Align) :-
    current_predicate(M:'$c_struct'/3),
    M:'$c_struct'(Name, Size, Align).

%!  c_current_struct_field(:Name, ?Field, ?Offset, ?Type)
%
%   Fact to provide efficient access to fields

c_current_struct_field(M:Name, Field, Offset, Type) :-
    current_predicate(M:'$c_struct_field'/4),
    M:'$c_struct_field'(Name, Field, Offset, Type).


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


		 /*******************************
		 *        CPP CONSTANTS		*
		 *******************************/

add_constants(Module, Header0, Header) :-
    current_predicate(Module:cpp_const/1),
    findall(Const, Module:cpp_const(Const), Consts),
    Consts \== [],
    !,
    must_be(list(atom), Consts),
    maplist(const_decl, Consts, Decls),
    atomics_to_string([Header0|Decls], "\n", Header).
add_constants(_, Header, Header).

const_decl(Const, Decl) :-
    format(string(Decl), "static int __swipl_const_~w = ~w;", [Const, Const]).

c_constants([]) --> [].
c_constants([H|T]) --> c_constant(H), c_constants(T).

c_constant(Name=Value) -->
    [ cpp_const(Name, Value) ].


		 /*******************************
		 *           EXPANSION		*
		 *******************************/

cpp_expand(Modules, T0, T) :-
    atom(T0),
    member(M, Modules),
    call(M:cpp_const(T0, T)),
    !.
cpp_expand(Modules, T0, T) :-
    compound(T0),
    !,
    compound_name_arguments(T0, Name, Args0),
    maplist(cpp_expand(Modules), Args0, Args1),
    compound_name_arguments(T1, Name, Args1),
    (   T0 == T1
    ->  T = T0
    ;   T = T1
    ).
cpp_expand(_, T, T).

system:term_expansion(T0, T) :-
    prolog_load_context(module, M),
    current_predicate(M:cpp_const/2),
    cpp_expand([M], T0, T).

