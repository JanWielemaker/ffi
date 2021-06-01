/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018-2021, VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(ffi,
          [ c_import/3,                 % +Header, +Flags, +Functions

                                        % Memory access predicates
            c_calloc/4,                 % -Ptr, +Type, +Size, +Count
            c_free/1,                   % +Ptr
            c_disown/1,                 % +Ptr
            c_typeof/2,                 % +Ptr, -Type
            c_load/4,                   % +Ptr, +Offset, +Type, -Value
            c_store/4,                  % +Ptr, +Offset, +Type, +Value
            c_offset/6,                 % +Ptr0, +Off, +Type, +Size, +Count, -Ptr
            c_sizeof/2,                 % +Type, -Bytes
            c_alignof/2,                % +Type, -Bytes
            c_address/2,                % +Ptr, -AsInt
            c_dim/3,                    % +Ptr, -Count, -ElemSize

            c_alloc/2,			% -Ptr, +Type
            c_load/2,                   % +Location, -Value
            c_store/2,                  % +Location, +Value
            c_cast/3,                   % +Type, +PtrIn, -PtrOut
            c_nil/1,                    % -Ptr
            c_is_nil/1,                 % @Ptr

            c_struct/2,                 % +Name, +Fields
            c_union/2,                  % +Name, +Fields

            c_current_enum/3,           % :Id, ?Enum, ?Value
            c_current_struct/1,         % :Name
            c_current_struct/3,         % :Name, -Size, -Alignment
            c_current_struct_field/4,   % :Name, ?Field, ?Offset, ?Type
            c_current_union/1,          % :Name
            c_current_union/3,          % :Name, -Size, -Alignment
            c_current_union_field/3,    % :Name, ?Field, ?Type
            c_current_typedef/2,        % :Name, -Type

            c_expand_type/2,            % :TypeIn, -TypeOut
            c_type_size_align/3,        % :Type, -Size, -Alignment

            c_struct_dict/2,            % ?Ptr,  ?Dict

	    c_array_list/2,             % +Ptr, -List
	    c_array_list/3,             % +Ptr, +Count, -List

            c_enum_in/3,                % :Id, +Enum, -Int
            c_enum_out/3,               % :Id, +Enum, +Int

            c_alloc_string/3,           % -Ptr, +Data, +Encoding
            c_load_string/4,            % +Ptr, -Data, +Type, +Encoding
            c_load_string/5,            % +Ptr, +Len, -Data, +Type, +Encoding

            c_errno/1,                  % -Integer

            op(200, fy, *),             % for pointer type declarations
            op(100, xfx, ~),            % Type~FreeFunc
            op(20, yf, [])
          ]).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(process)).

:- use_module(cdecls).
:- use_module(clocations).

/** <module> Bind Prolog predicates to C functions
*/

:- meta_predicate
    c_alloc(-,:),
    c_cast(:,+,-),
    c_load(:, -),
    c_store(:, +),
    c_current_enum(?,:,?),
    c_enum_in(+,:,-),
    c_enum_out(+,:,+),
    c_current_struct(:),
    c_current_struct(:,?,?),
    c_current_struct_field(:,?,?,?),
    c_current_union(:),
    c_current_union(:,?,?),
    c_current_union_field(:,?,?),
    c_current_typedef(:,:),
    c_struct_dict(:,?),
    c_array_list(:,?),
    c_array_list(:,+,?),
    c_expand_type(:,:),
    type_size(:,-),
    c_type_size_align(:,-,-),
    c_type_size_align(:,-,-, +).


:- use_foreign_library(foreign(ffi4pl)).

:- multifile
    user:file_search_path/2,
    system:term_expansion/2,
    user:exception/3,
    c_function/3.


		 /*******************************
		 *           LIBRARIES		*
		 *******************************/

%!  c_library(+Base, -FHandle, +Options)
%
%   Find a file handle for a foreign  library.   If  Base is of the form
%   Base-Options pass the options to ffi_library_create/3.

:- dynamic  c_library_cache/2.
:- volatile c_library_cache/2.

c_library(Base, FHandle, _Options) :-
    c_library_cache(Base, FHandle0),
    !,
    FHandle = FHandle0.
c_library(Base, FHandle, Options) :-
    with_mutex(ffi, c_library_sync(Base, FHandle, Options)).

c_library_sync(Base, FHandle, _Options) :-
    c_library_cache(Base, FHandle0),
    !,
    FHandle = FHandle0.
c_library_sync(Base, FHandle, Options) :-
    !,
    c_lib_path(Base, Path, Options),
    convlist(rtld, Options, Flags),
    ffi_library_create(Path, FHandle, Flags),
    assertz(c_library_cache(Base, FHandle)).

rtld(rtld(Flag), Flag).


		 /*******************************
		 *             IMPORT		*
		 *******************************/

%!  c_import(+Header, +Flags, +Functions)
%
%   Import Functions as predicates from Libs   based  on the declaration
%   from Header.

c_import(Header, Flags, Functions) :-
        throw(error(context_error(nodirective,
                                  c_import(Header, Flags, Functions)), _)).

system:term_expansion((:- c_import(Header0, Flags0, Functions0)),
                      Clauses) :-
    c_macro_expand(c_import(Header0, Flags0, Functions0),
                   c_import(Header, Flags1, Functions)),
    \+ current_prolog_flag(xref, true),
    prolog_load_context(module, M),
    phrase(c_functions_needed(Functions), FunctionNames),
    add_constants(M, Header, HeaderConst),
    expand_flags(Flags1, Flags),
    partition(is_lib_flag, Flags, LibFlags, InclFlags),
    c99_types(HeaderConst, InclFlags, FunctionNames, Types, Constants),
    (   debugging(ffi(types))
    ->  print_term(Types, [])
    ;   true
    ),
    phrase(( c_constants(Constants),
             c_import(Functions, LibFlags, FunctionNames, Types)),
           Clauses).

%!  expand_flags(+Flags0, -Flags) is det.
%
%   Expand   calls   to    pkg-config    written     down    as    e.g.,
%   pkg_config(uchardet, '--cflags', '--libs')

expand_flags(Flags0, Flags) :-
    must_be(ground, Flags0),
    maplist(expand_flag, Flags0, Flags1),
    (   Flags0 == Flags1
    ->  Flags = Flags0
    ;   flatten(Flags1, Flags),
        debug(ffi(flags), 'Final flags: ~p', [Flags])
    ).

expand_flag(Flag, Flags) :-
    compound(Flag),
    compound_name_arguments(Flag, Name, Args),
    pkg_config(Name),
    !,
    setup_call_cleanup(
        process_create(path('pkg-config'), Args,
                       [ stdout(pipe(Out)) ]),
        read_string(Out, _, String),
        close(Out)),
    split_string(String, " \r\t\t", " \r\n\t", FlagStrings),
    maplist(atom_string, Flags, FlagStrings).
expand_flag(Flag, Flag).

pkg_config(pkg_config).
pkg_config('pkg-config').



%!  c_functions_needed(+FuncSpecList)//
%
%   Get the names of the C functions   that  we need. Optional functions
%   are returned in a list.

c_functions_needed([]) --> [].
c_functions_needed([H|T]) --> c_function_needed(H), c_functions_needed(T).

c_function_needed([Spec]) -->
    !,
    c_function_needed(Spec, optional).
c_function_needed(Spec) -->
    c_function_needed(Spec, required).

c_function_needed(Spec as _, Optional) -->
    !,
    c_function_needed(Spec, Optional).
c_function_needed(Spec, Optional) -->
    { compound_name_arguments(Spec, Name, Args) },
    needed(Name, Optional),
    free_needed(Args, Optional).

free_needed([], _) --> [].
free_needed([H|T], Optional) --> free_arg(H, Optional), free_needed(T, Optional).

free_arg(_Type~Free, Optional) -->
    !,
    needed(Free, Optional).
free_arg(*(_Type,Free), Optional) -->   % deprecated
    !,
    needed(Free, Optional).
free_arg([Ret], Optional) -->
    !,
    free_arg(Ret, Optional).
free_arg(-Output, Optional) -->
    !,
    free_arg(Output, Optional).
free_arg(_, _) -->
    [].

needed(Name, optional) -->
    [[Name]].
needed(Name, required) -->
    [Name].

%!  c_import(+FuncSpecList, +Flags, +FunctionNames, +Types)//
%
%   Produce the clauses for the predicates  and types that represent the
%   library.

c_import(Functions, Flags, FunctionNames, Types) -->
    decls(Types),
    compile_types(Types, Types),
    wrap_functions(Functions, Types),
    libs(Flags, FunctionNames).

decls(_) -->
    [ (:- discontiguous(('$c_lib'/3,
                         '$c_struct'/3,
                         '$c_struct_field'/4,
                         '$c_union'/3,
                         '$c_union_field'/4,
                         '$c_enum'/3,
                         '$c_typedef'/2
                        ))),
      (:- dynamic        '$c_symbol_cache'/2,
	                 '$c_lib'/3
      )
    ].

compile_types([], _) --> [].
compile_types([struct(Name,Fields)|T], Types) --> !,
    compile_struct(Name, Fields, Types),
    compile_types(T, Types).
compile_types([union(Name,Fields)|T], Types) --> !,
    compile_union(Name, Fields, Types),
    compile_types(T, Types).
compile_types([enum(Name, Values)|T], Types) --> !,
    compile_enum(Name, Values),
    compile_types(T, Types).
compile_types([typedef(Name, Type)|T], Types) --> !,
    compile_typedef(Name, Type),
    compile_types(T, Types).
compile_types([_|T], Types) --> !,
    compile_types(T, Types).

wrap_functions([], _) --> [].
wrap_functions([H|T], Types) -->
    { optional(H, Func, Optional)
    },
    wrap_function(Func, Optional, Types),
    wrap_functions(T, Types).

optional([Func], Func, optional) :- !.
optional(Func,   Func, required).

wrap_function(Signature as PName, _Optional, Types) -->
    !,
    (   { compound_name_arguments(Signature, FName, SigArgs),
          memberchk(function(FName, CRet0, Params), Types)
        }
    ->  (   { signature_arity(SigArgs, Arity),
              matching_signature(FName, SigArgs, CRet0, CRet,
                                 Params, SigParams, Types),
              include(is_closure, SigParams, Closures),
              length(Closures, NClosures),
              PArity is Arity - NClosures,
              functor(PHead, PName, PArity),
              CSignature =.. [FName|SigParams],
              prolog_load_context(module, M)
            }
        ->  [ ffi:c_function(M:PHead, Params, CRet),
              (:- dynamic(PName/PArity)),
              (PHead :- ffi:define(M:PHead, CSignature))
            ]
        ;   []                          % Ignore non-matching signature
        )
    ;   []                              % Already warned by c99_types
    ).
wrap_function(Signature, Optional, Types) -->
    { compound_name_arity(Signature, Name, _)
    },
    wrap_function(Signature as Name, Optional, Types).

signature_arity(SigArgs, Arity) :-
    append(Pre, [[void]], SigArgs),
    !,
    length(Pre, Arity).
signature_arity(SigArgs, Arity) :-
    length(SigArgs, Arity).


is_closure(+closure(_)).
is_closure(+c_callback(_)).

%!  matching_signature(+FuncName,
%!                     +PlArgs, +CRet0, -CRet,
%!                     +CArgs, -SignatureArgs, +Types) is semidet.
%
%   Match the signature given by the user   with  the one extracted from
%   the C code. If the  two   signatures  are  compatible, the resulting
%   SignatureArgs is a list of  arguments   using  the  same notation as
%   PlArgs, but using the concrete  C   types  rather  than the abstract
%   Prolog type. For example, Prolog `int` may be mapped to C `ulong` if
%   the C function accepts  a  type   that  (eventually)  aliases  to an
%   unsigned long.

matching_signature(Name, SigArgs, CRet0, CRet, Params, SigParams, Types) :-
    append(RealArgs, [[PlRet]], SigArgs),   % specified return
    !,
    (   matching_param_length(RealArgs, Params, PlArgs, VArgs, CParams)
    ->  maplist(compatible_argument(Name, Types),
                PlArgs, CParams, SigRealParams)
    ;   print_message(error, ffi(nonmatching_params(Name, SigArgs, Params))),
        fail
    ),
    (   PlRet == void
    ->  append(SigRealParams, VArgs, SigParams),
        CRet = void
    ;   CRet0 == void
    ->  print_message(error, ffi(void_function(Name, PlRet))),
        fail
    ;   compatible_return(Name, PlRet, CRet0, RetParam, Types),
        CRet = CRet0,
        append([SigRealParams, VArgs, [[RetParam]]], SigParams)
    ).
matching_signature(Name, SigArgs, CRet, CRet, Params, SigParams, Types) :-
    (   matching_param_length(SigArgs, Params, PlArgs, VArgs, CParams)
    ->  maplist(compatible_argument(Name, Types),
                PlArgs, CParams, SigParams0),
        append(SigParams0, VArgs, SigParams)
    ;   print_message(error, ffi(nonmatching_params(Name, SigArgs, Params))),
        fail
    ),
    (   CRet == void
    ->  true
    ;   print_message(warning, ffi(nonvoid_function(Name, CRet)))
    ).

%!  matching_param_length(+PlParms, +CParms,
%!                        -ReqPlArgs, -VarPlArgs, -RegCParms) is semidet.
%
%   Check that the argument count  of   the  Prolog  specification and C
%   function match.
%
%   @arg ReqPlArgs Prolog arguments that must be matched
%   @arg VarPlArgs Prolog variadic arguments (matches against `...`)
%   @arg RegCParms C arguments that must be matched

matching_param_length(PlParms, CParams, ReqPlParams, VarPlParams, ReqCParams) :-
    append(ReqCParams, ['...'], CParams),
    !,
    length(ReqCParams, CArgc),
    length(ReqPlParams, CArgc),
    append(ReqPlParams, VarPlParams0, PlParms),
    maplist(variadic_param, VarPlParams0, VarPlParams).
matching_param_length(PlParms, CParams, PlParms, [], CParams) :-
    same_length(PlParms, CParams).

variadic_param(-Type, -Type) :- !.
variadic_param(*(Type), *Type) :- !.
variadic_param(Type0, Type) :-
    default_promotion(Type0, Type), !.
variadic_param(Type, Type).

default_promotion(char,    int).
default_promotion(uchar,   uint).
default_promotion('_Bool', uint).
default_promotion(short,   int).
default_promotion(ushort,  int).
default_promotion(float,   double).


%!  compatible_argument(+Func, +Types, +PlArg, +CArg, -Param)

compatible_argument(_Func, Types, PlArg, CArg, Param) :-
    compatible_arg(PlArg, CArg, Param, Types),
    !.
compatible_argument(_Func, Types, PlArg, CArg, PlArg) :-
    compatible_arg(PlArg, CArg, Types),
    !.
compatible_argument(Func, _, PlArg, CArg, PlArg) :-
    print_message(error, ffi(incompatible_argument(Func, PlArg, CArg))).

% compatible_arg/4
compatible_arg(PlArg, _ArgName-CArg, Param, Types) :-
    !,
    compatible_arg(PlArg, CArg, Param, Types).
compatible_arg(+PlArg, CArg, Param, Types) :-
    !,
    compatible_arg(PlArg, CArg, Param, Types).
compatible_arg(int, CType, +CType, _) :-
    int_type(CType).
compatible_arg(int, enum(_), +int, _).
compatible_arg(enum, enum(Name), +enum(Name), _).
compatible_arg(-int, *(CType), -CType, _) :-
    int_type(CType).
compatible_arg(*int, *(CType), *CType, _) :-
    int_type(CType).
compatible_arg(bool, '_Bool', +'_Bool', _).
compatible_arg(-bool, *('_Bool'), -'_Bool', _).
compatible_arg(*bool, *('_Bool'), *'_Bool', _).
compatible_arg(float, CType, +CType, _) :-
    float_type(CType).
compatible_arg(-float, *(CType), -CType, _) :-
    float_type(CType).
compatible_arg(null, funcptr(_Ret, _Params), +c_callback(_:null), _Types) :-
   !.
compatible_arg('C'(Callback), funcptr(_Ret, _Params), +c_callback(M:Callback), _Types) :-
   prolog_load_context(module, M),
   !.
compatible_arg(Func0, funcptr(Ret, Params), +closure(M:Func), Types) :-
    prolog_load_context(module, M0),
    strip_module(M0:Func0, M, Func1),
    compound(Func1),
    Func1 \= +(_),
    Func1 \= *(_),
    compound_name_arguments(Func1, Pred, SigArgs),
    !,
    matching_signature(-, SigArgs, Ret, Ret, Params, SigParams, Types),
    compound_name_arguments(Func, Pred, SigParams).
% compatible_arg/3
compatible_arg(PlArg, _ArgName-CArg, Types) :-
    !,
    compatible_arg(PlArg, CArg, Types).
compatible_arg(+PlArg, CArg, Types) :-
    !,
    compatible_arg(PlArg, CArg, Types).
compatible_arg(Type, Type, _) :- !.
compatible_arg(-PType~_Free, *(CType), Types) :- !,
    compatible_arg(PType, CType, Types).
compatible_arg(-PType, *(CType), Types) :- !,
    compatible_arg(PType, CType, Types).
compatible_arg(struct(Name),    *(struct(Name)), _).
compatible_arg(*struct(Name),   *(struct(Name)), _).
compatible_arg(union(Name),     *(union(Name)), _).
compatible_arg(*union(Name),    *(union(Name)), _).
compatible_arg(string,          *(char), _).
compatible_arg(string(wchar_t), *(Type), _) :- !, wchar_t_type(Type).
compatible_arg(string(Enc),     *(char), _) :- Enc \== wchar_t.
compatible_arg(string(Enc),     *(char), _) :- Enc \== wchar_t.
compatible_arg(*TypeName,       CType, Types) :-
    atom(TypeName),
    memberchk(typedef(TypeName, Type), Types),
    !,
    compatible_arg(*Type, CType, Types).
compatible_arg(TypeName,        CType, Types) :-
    atom(TypeName),
    memberchk(typedef(TypeName, Type), Types),
    !,
    compatible_arg(Type, CType, Types).
compatible_arg(-TypeName,        CType, Types) :-
    atom(TypeName),
    memberchk(typedef(TypeName, Type), Types),
    !,
    compatible_arg(-Type, CType, Types).
compatible_arg(*Type, *CType, Types) :-
    compatible_arg(Type, CType, Types).


compatible_return(_Func, PlArg, CArg, RetParam, Types) :-
    compatible_ret(PlArg, CArg, RetParam, Types),
    !.
compatible_return(_Func, PlArg, CArg, PlArg, Types) :-
    compatible_ret(PlArg, CArg, Types),
    !.
compatible_return(Func, PlArg, CArg, PlArg, _Types) :-
    print_message(error, ffi(incompatible_return(Func, PlArg, CArg))).

% compatible_ret/4
compatible_ret(-PlArg, CArg, Param, Types) :-
    compatible_ret(PlArg, CArg, Param, Types).
compatible_ret(PlArg~Free, CArg, Param~Free, Types) :-
    !,
    compatible_ret(PlArg, CArg, Param, Types).
compatible_ret(int, CArg, CArg, _) :-
    int_type(CArg),
    !.
compatible_ret(int, enum(_), int, _) :-
    !.
compatible_ret(bool, '_Bool', '_Bool', _) :-
    !.
compatible_ret(enum, enum(Name), enum(Name), _) :-
    !.
compatible_ret(float, CArg, CArg, _) :-
    float_type(CArg).
compatible_ret(*(TypeName,Free), *(CType), *(CType,Free), Types) :-
    !,
    compatible_ret(*(TypeName), *(CType), *(CType), Types).
compatible_ret(*(TypeName), *(CType), *(CType), Types) :-
    memberchk(typedef(TypeName, Type), Types),
    !,
    compatible_ret(*(Type), *(CType), Types).
% compatible_ret/3
compatible_ret(-PlArg, CArg, Types) :-
    !,
    compatible_ret(PlArg, CArg, Types).
compatible_ret(Type~_Free, CType, Types) :-
    !,
    compatible_ret(Type, CType, Types).
compatible_ret(*(Type,_Free), CType, Types) :- % deprecated
    !,
    compatible_ret(*(Type), CType, Types).
compatible_ret(Type, Type, _) :- !.
compatible_ret(*(struct(Name)),  *(struct(Name)), _).
compatible_ret(*(union(Name)),   *(union(Name)), _).
compatible_ret(string,           *(char), _).
compatible_ret(string(wchar_t),  *(Type), _) :- !, wchar_t_type(Type).
compatible_ret(string(Enc),      *(char), _) :- Enc \== wchar_t.

int_type(char).
int_type(uchar).
int_type(short).
int_type(ushort).
int_type(int).
int_type(uint).
int_type(long).
int_type(ulong).
int_type(longlong).
int_type(ulonglong).
int_type(size_t).

float_type(float).
float_type(double).

wchar_t_type(Type) :-
    c_sizeof(Type, Size),
    c_sizeof(wchar_t, Size).

%!  libs(+Flags, +Functions)// is det.
%
%   Create '$c_lib'(Lib, Dir,  Functions)  facts   that  describe  which
%   functions are provided by which library.

libs(Flags, Functions) -->
    { convlist(flag_lib, Flags, Specs),
      partition(load_option, Specs, Options, Libs),
      prolog_load_context(directory, Dir)
    },
    lib_clauses(Libs, Functions, [relative_to(Dir)|Options]).

load_option(rtld(_)).

lib_clauses([], _, _) --> [].
lib_clauses([H|T], Functions, Options) -->
    [ '$c_lib'(H, Options, Functions) ],
    lib_clauses(T, Functions, Options).

is_lib_flag(Flag) :-
    flag_lib(Flag, _).

flag_lib(Flag, Lib) :-
    compound(Flag),
    !,
    Lib = Flag.
flag_lib(Flag, Lib) :-
    atom_concat('-l', Rest, Flag),
    !,
    atom_concat('lib', Rest, Lib).
flag_lib(Flag, Lib) :-
    atom_concat('--rtld_', Opt, Flag),
    !,
    Lib = rtld(Opt).
flag_lib(Lib, Lib) :-
    \+ sub_atom(Lib, 0, _, _, '-').

%!  define(:Head, +CSignature)
%
%   Actually link the C function

:- public
    define/2.

define(QHead, CSignature) :-
    QHead = M:_Head,
    link_clause(QHead, CSignature, Clause),
    !,
    asserta(M:Clause),
    call(QHead).
define(QHead, _) :-
    throw(error(ffi_error(define(QHead)), _)).

link_clause(M:Goal, CSignature,
            (PHead :- !, Body)) :-
    c_function(M:Goal, ParamSpec, RetType),	% Also in dynamic part?
    maplist(strip_param_name, ParamSpec, ParamTypes),
    functor(Goal, Name, PArity),
    functor(PHead, Name, PArity),
    functor(CSignature, _, CArity),
    functor(CHead, Name, CArity),
    CSignature =.. [FName|SigArgs],
    find_symbol(M, FName, FuncPtr),
    prototype_types(ParamTypes, SigArgs, RetType, M, PParams, PRet),
    debug(ffi(prototype),
          'Binding ~p (Ret=~p, Params=~p)', [Name, PRet, PParams]),
    ffi_prototype_create(FuncPtr, default, PRet, PParams, Prototype),
    convert_args(SigArgs, 1, PArity, 1, CArity, PHead, CHead,
                 PreConvert, PostConvert),
    Invoke = ffi:ffi_call(Prototype, CHead),
    mkconj(PreConvert, Invoke, Body0),
    mkconj(Body0, PostConvert, Body).

strip_param_name(_Name-Type, Type) :- !.
strip_param_name(Type, Type).

find_symbol(M, FName, Symbol) :-
    M:'$c_lib'(Lib, Options, Funcs),
    member(Func, Funcs),
    optional(Func, FName, _Optional),
    c_library(Lib, FH, Options),
    ffi_lookup_symbol(FH, FName, Symbol),
    !.
find_symbol(_, FName, _) :-
    existence_error(c_function, FName).

%!  prototype_types(+CParms, +PlParms, +CRet, +Module,
%!                  -CTypes, -CRetType)
%
%   Create the argument and return type specification for the prototype.

prototype_types([], [[SA]], RetType, M, [], PRet) :-
    !,
    prototype_type(RetType, M, SA, PRet).
prototype_types([], [], _RetType, _M, [], void).
prototype_types([...], PlParms, CRet, M, CTypes, CRetType) :-
    !,
    variadic_prototypes(PlParms, CRet, M, CTypes, CRetType).
prototype_types([H0|T0], [SA|ST], RetType, M, [H|T], PRet) :-
    prototype_type(H0, M, SA, H),
    prototype_types(T0, ST, RetType, M, T, PRet).

variadic_prototypes([[SA]], RetType, M, [], PRet) :-
    !,
    prototype_type(RetType, M, SA, PRet).
variadic_prototypes([], _, _, [], void).
variadic_prototypes([SA|ST], RetType, M, [H|T], CRetType) :-
    variadic_prototype(SA, M, H),
    variadic_prototypes(ST, RetType, M, T, CRetType).

variadic_prototype(string, _, *(char)) :- !.
variadic_prototype(Type,   _, Type).

%!  prototype_type(+CType, +Module, +PrologType, -ParamType) is det.

prototype_type(funcptr(_,_), _, +c_callback(_), c_callback) :-
    !.
prototype_type(funcptr(_,_), _, PlType, closure) :-
    compound_name_arguments(PlType, _, _),
    !.
prototype_type(*(*CType), M, -OutputType~FreeName, -(*(CType,Free))) :-
    c_output_argument_type(OutputType),
    find_symbol(M, FreeName, Free),
    !.
prototype_type(*CType, _, -OutputType, -CType) :-
    c_output_argument_type(OutputType),
    !.
prototype_type(*Type0, M, Sig, *Type) :-
    !,
    prototype_type(Type0, M, Sig, Type).
prototype_type(*(Type0, Free), M, Sig, *(Type, Free)) :-
    !,
    prototype_type(Type0, M, Sig, Type).
prototype_type(struct(Name), M, _Sig, struct(Name, Size)) :-
    !,
    catch(type_size(M:struct(Name), Size),
          error(existence_error(type,_),_),
          Size = 0).
prototype_type(union(Name), M, _Sig, union(Name, Size)) :-
    !,
    catch(type_size(M:union(Name), Size),
          error(existence_error(type,_),_),
          Size = 0).
prototype_type(Type, _, _, Type).

c_output_argument_type(ScalarType) :-
    c_sizeof(ScalarType, _Size).
c_output_argument_type(enum(_)).
c_output_argument_type(string).
c_output_argument_type(string(_Encoding)).
c_output_argument_type(atom).
c_output_argument_type(atom(_Encoding)).
c_output_argument_type(*(_)).


%!  convert_args(+SigArgs, +PI, +PArity, +CI, +CArity,
%!		 +PlHead, +CHead, -PreGoal, -PostGoal)
%
%   Establish the conversions between the Prolog head and the C head.

convert_args([], _, _, _, _, _, _, true, true).
convert_args([+closure(M:Closure)|T], PI, PArity, CI, CArity,
             PHead, CHead, GPre, GPost) :-
    !,
    arg(CI, CHead, CClosure),
    closure_create(M:Closure, CClosure),
    CI2 is CI + 1,
    convert_args(T, PI, PArity, CI2, CArity, PHead, CHead, GPre, GPost).
convert_args([+c_callback(M:Callback)|T], PI, PArity, CI, CArity,
             PHead, CHead, GPre, GPost) :-
    !,
    arg(CI, CHead, CCallback),
    ccallback_create(M:Callback, CCallback),
    CI2 is CI + 1,
    convert_args(T, PI, PArity, CI2, CArity, PHead, CHead, GPre, GPost).
convert_args([H|T], PI, PArity, CI, CArity, PHead, CHead, GPre, GPost) :-
    arg(PI, PHead, PArg),
    arg(CI, CHead, CArg),
    (   convert_arg(H, PArg, CArg, GPre1, GPost1)
    ->  true
    ;   PArg = CArg,
        GPre1 = true,
        GPost1 = true
    ),
    PI2 is PI + 1,
    CI2 is CI + 1,
    convert_args(T, PI2, PArity, CI2, CArity, PHead, CHead, GPre2, GPost2),
    mkconj(GPre1, GPre2, GPre),
    mkconj(GPost1, GPost2, GPost).

% parameter values
convert_arg(+Type, Prolog, C, Pre, Post) :-
    !,
    convert_arg(Type, Prolog, C, Pre, Post).
convert_arg(-Type~_Free, Prolog, C, Pre, Post) :-
    !,
    convert_arg(-Type, Prolog, C, Pre, Post).
convert_arg(-struct(Name), Ptr, Ptr,
            c_alloc(Ptr, struct(Name)),
            true).
convert_arg(-union(Name), Ptr, Ptr,
            c_alloc(Ptr, union(Name)),
            true).
convert_arg(-string, String, Ptr,
            true,
            c_load_string(Ptr, String, string, text)).
convert_arg(-string(Enc), String, Ptr,
            true,
            c_load_string(Ptr, String, string, Enc)).
convert_arg(-atom, String, Ptr,
            true,
            c_load_string(Ptr, String, atom, text)).
convert_arg(-atom(Enc), String, Ptr,
            true,
            c_load_string(Ptr, String, atom, Enc)).
convert_arg(string(Enc),  String, Ptr,
            c_alloc_string(Ptr, String, Enc),
            true).
convert_arg(string, String, Ptr, Pre, Post) :-
    convert_arg(string(text), String, Ptr, Pre, Post).
convert_arg(enum(Enum), Id, Int,
            c_enum_in(Id, Enum, Int),
            true).
convert_arg(-enum(Enum), Id, Int,
            true,
            c_enum_out(Id, Enum, Int)).

% return value.  We allow for -Value, but do not demand it as the
% return value can only be an output.
convert_arg([-(X)], Out, In, Pre, Post) :-
    !,
    convert_arg([X], Out, In, Pre, Post).
convert_arg([Type~_Free], Out, In, Pre, Post) :-
    !,
    convert_arg([Type], Out, In, Pre, Post).
convert_arg([string(Enc)], String, Ptr,
            true,
            c_load_string(Ptr, String, string, Enc)).
convert_arg([string], String, Ptr, Pre, Post) :-
    convert_arg([-string(text)], String, Ptr, Pre, Post).
convert_arg([atom(Enc)], String, Ptr,
            true,
            c_load_string(Ptr, String, atom, Enc)).
convert_arg([atom], String, Ptr, Pre, Post) :-
    convert_arg([-atom(text)], String, Ptr, Pre, Post).
convert_arg([enum(Enum)], Id, Int,
            true,
            c_enum_out(Id, Enum, Int)).

mkconj(true, G, G) :- !.
mkconj(G, true, G) :- !.
mkconj(G1, G2, (G1,G2)).


%!  callback_create(:Head, -Closure) is det.
%
%   Create a C callback pointer from Head. Head   is  a qualified term
%   of the form 'C'(Callback), where Callback is a term with  a functor
%   which is the name of the C function to call, and the parameterts are
%   the callback  parameter types, which are used during parsing only
%   for type checking.

ccallback_create(_:null, CCallback) :-
    ffi:ffi_callback_ptr(_FH, '$null_callback', CCallback).
ccallback_create(M:CHead, CCallback) :-
    compound_name_arguments(CHead, CFuncName, _),
    c_symbol_callback(M:CFuncName,CCallback).

%  Lookup C symbol address in shared libraries that have
%  been loaded by c_import in the context of module M.
c_symbol_callback(M:Symbol, CCallback) :-
    M:'$c_symbol_cache'(Symbol, CCallback),
    !.
c_symbol_callback(M:Symbol, CCallback) :-
    M:'$c_lib'(Lib, Options, _),
    c_library(Lib, FH, Options),
    ffi:ffi_callback_ptr(FH, Symbol, CCallback),
    M:assert('$c_symbol_cache'(Symbol, CCallback)),
    debug(ffi(callback), '~p', [symcb(FH,Symbol,CCallback)]).

%!  closure_create(:Head, -Closure) is det.
%
%   Create a closure object from Head. Head   is  a qualified term whose
%   functor is the predicate name to be   called and whose arguments are
%   the C parameter types.

closure_create(M:Head, Closure) :-
    compound_name_arguments(Head, _, Args),
    (   append(Params0, [[Return]], Args)
    ->  true
    ;   Params0 = Args,
        Return = void
    ),
    maplist(strip_mode, Params0, Params),
    ffi_closure_create(M:Head, default, Return, Params, Closure).

strip_mode(+Type, Type) :- !.
strip_mode(Type, Type).


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
%   This directive is  normally  used  by   c_import/3  to  create  type
%   information for structures that are involved   in functions that are
%   imported. This directive may be used  explicitly in combination with
%   the C memory access predicates  to  read   or  write  memory using C
%   binary representation.

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
field_clauses([f(Name,bitfield(Width))|T0], Struct,
              Off0, Off, Align0, Align, All) --> !,
    { c_type_size_align(uint, Size, Alignment, All),
      Bits is 8*Size,
      Align1 is max(Align0, Alignment),
      Off1 is Alignment*((Off0+Alignment-1)//Alignment),
      Off2 is Off1 + Size
    },
    bitfield_clauses([f(Name,bitfield(Width))|T0], Struct,
                     Off1, 0, Bits, T),
    field_clauses(T, Struct, Off2, Off, Align1, Align, All).
field_clauses([f(Name,Type)|T], Struct, Off0, Off, Align0, Align, All) -->
    { c_type_size_align(Type, Size, Alignment, All),
      Align1 is max(Align0, Alignment),
      Off1 is Alignment*((Off0+Alignment-1)//Alignment),
      Off2 is Off1 + Size
    },
    [ '$c_struct_field'(Struct, Name, Off1, Type) ],
    field_clauses(T, Struct, Off2, Off, Align1, Align, All).

bitfield_clauses([f(Name,bitfield(Width))|T0], Struct,
                 IntOffset, BitsUsed, Bits, T) -->
    { BitsUsed1 is BitsUsed + Width,
      BitsUsed1 =< Bits
    },
    [ '$c_struct_field'(Struct, Name, IntOffset, bitfield(BitsUsed, Width)) ],
    bitfield_clauses(T0, Struct, IntOffset, BitsUsed1, Bits, T).
bitfield_clauses(Fields, _, _, _, _, Fields) --> [].


%!  c_union(+Name, +Fields)
%
%   Declare a C union with name  Name.   Fields  is  a list of fields
%   using the same conventions as c_struct/2.

c_union(Name, Fields) :-
    throw(error(context_error(nodirective, c_union(Name, Fields)), _)).

system:term_expansion((:- c_union(Name, Fields)), Clauses) :-
    phrase(compile_unions([union(Name, Fields)]), Clauses).

compile_unions(List) -->
    compile_unions(List, List).

compile_unions([], _) --> [].
compile_unions([union(Name,Fields)|T], All) -->
    compile_union(Name, Fields, All),
    compile_unions(T, All).

compile_union(Name, Fields, All) -->
    ufield_clauses(Fields, Name, 0, Size, 0, Alignment, All),
    { Size is Alignment*((Size+Alignment-1)//Alignment) },
    [ '$c_union'(Name, Size, Alignment) ].

ufield_clauses([], _, Size, Size, Align, Align, _) --> [].
ufield_clauses([f(Name,Type)|T], Struct, Size0, Size, Align0, Align, All) -->
    { c_type_size_align(Type, ESize, Alignment, All),
      Align1 is max(Align0, Alignment),
      Size1  is max(Size0, ESize)
    },
    [ '$c_union_field'(Struct, Name, Type) ],
    ufield_clauses(T, Struct, Size1, Size, Align1, Align, All).


%!  type_size(:Type, -Size)
%
%   Size is the size of an object of Type.

type_size(Type, Size) :-
    c_type_size_align(Type, Size, _).

%!  c_type_size_align(:Type, -Size, -Alignment) is det.
%
%   True when Type must be aligned at Alignment and is of size Size.

c_type_size_align(Type, Size, Alignment) :-
    c_type_size_align(Type, Size, Alignment, []).

c_type_size_align(_:Type, Size, Alignment, _All) :-
    c_alignof(Type, Alignment),
    !,
    c_sizeof(Type, Size).
c_type_size_align(_:struct(Name), Size, Alignment, All) :-
    memberchk(struct(Name, Fields), All), !,
    phrase(compile_struct(Name, Fields, All), Clauses),
    memberchk('$c_struct'(Name, Size, Alignment), Clauses).
c_type_size_align(_:struct(Name, Fields), Size, Alignment, All) :-
    phrase(compile_struct(Name, Fields, All), Clauses),
    memberchk('$c_struct'(Name, Size, Alignment), Clauses).
c_type_size_align(_:union(Name), Size, Alignment, All) :-
    memberchk(union(Name, Fields), All), !,
    phrase(compile_union(Name, Fields, All), Clauses),
    memberchk('$c_union'(Name, Size, Alignment), Clauses).
c_type_size_align(_:union(Name, Fields), Size, Alignment, All) :-
    phrase(compile_union(Name, Fields, All), Clauses),
    memberchk('$c_union'(Name, Size, Alignment), Clauses).
c_type_size_align(M:struct(Name), Size, Alignment, _) :-
    current_predicate(M:'$c_struct'/3),
    M:'$c_struct'(Name, Size, Alignment),
    !.
c_type_size_align(M:union(Name), Size, Alignment, _) :-
    current_predicate(M:'$c_union'/3),
    M:'$c_union'(Name, Size, Alignment),
    !.
c_type_size_align(M:array(Type,Len), Size, Alignment, All) :-
    !,
    c_type_size_align(M:Type, Size0, Alignment, All),
    Size is Size0*Len.
c_type_size_align(_:enum(_Enum), Size, Alignment, _) :-
    !,
    c_alignof(int, Alignment),
    c_sizeof(int, Size).
c_type_size_align(_:(*(_)), Size, Alignment, _) :-
    !,
    c_alignof(pointer, Alignment),
    c_sizeof(pointer, Size).
c_type_size_align(_:funcptr(_Ret,_Params), Size, Alignment, _) :-
    !,
    c_alignof(pointer, Alignment),
    c_sizeof(pointer, Size).
c_type_size_align(Type, Size, Alignment, All) :-
    c_current_typedef(Type, Def),
    !,
    c_type_size_align(Def, Size, Alignment, All).
c_type_size_align(Type, _Size, _Alignment, _) :-
    existence_error(type, Type).

%!  c_expand_type(:TypeIn, :TypeOut)
%
%   Expand user defined types to arrive at the core type.

c_expand_type(M:Type0, M:Type) :-
    (   base_type(Type0)
    ->  Type0 = Type
    ;   expand_type(Type0, Type, M)
    ).

base_type(struct(_)).
base_type(union(_)).
base_type(enum(_)).
base_type(Type) :-
    c_sizeof(Type, _).

expand_type(*Type0, *Type, M) :-
    !,
    (   base_type(Type0)
    ->  Type0 = Type
    ;   expand_type(Type0, Type, M)
    ).
expand_type(Type0, Type, M) :-
    c_current_typedef(M:Type0, M:Type).

%!  c_current_struct(:Name) is nondet.
%!  c_current_struct(:Name, ?Size, ?Align) is nondet.
%
%   Total size of the struct in bytes and alignment restrictions.

c_current_struct(Name) :-
    c_current_struct(Name, _, _).
c_current_struct(M:Name, Size, Align) :-
    current_predicate(M:'$c_struct'/3),
    M:'$c_struct'(Name, Size, Align).

%!  c_current_struct_field(:Name, ?Field, ?Offset, ?Type)
%
%   Fact to provide efficient access to fields

c_current_struct_field(M:Name, Field, Offset, M:Type) :-
    current_predicate(M:'$c_struct_field'/4),
    M:'$c_struct_field'(Name, Field, Offset, Type).


%!  c_current_union(:Name) is nondet.
%!  c_current_union(:Name, ?Size, ?Align) is nondet.
%
%   Total size of the union in bytes and alignment restrictions.

c_current_union(Name) :-
    c_current_union(Name, _, _).
c_current_union(M:Name, Size, Align) :-
    current_predicate(M:'$c_union'/3),
    M:'$c_union'(Name, Size, Align).

%!  c_current_union_field(:Name, ?Field, ?Type)
%
%   Fact to provide efficient access to fields

c_current_union_field(M:Name, Field, M:Type) :-
    current_predicate(M:'$c_union_field'/3),
    M:'$c_union_field'(Name, Field, Type).


%!  c_alloc(-Ptr, :TypeAndInit) is det.
%
%   Allocate memory for a C object of  Type and optionally initialse the
%   data. TypeAndInit can take several forms:
%
%     $ A plain type :
%     Allocate an array to hold a single object of the given type.
%
%     $ Type[Count] :
%     Allocate an array to hold _Count_ objects of _Type_.
%
%     $ Type[] = Init :
%     If Init is data that can be used to initialize an array of
%     objects of Type, allocate an array of sufficient size and
%     initialize each element with data from Init.  The following
%     combinations of Type and Init are supported:
%
%       $ char[] = Text :
%       Where Text is a valid Prolog representation for text: an
%       atom, string, list of character codes or list of characters.
%       The Prolog Unicode data is encoded using the native multibyte
%       encoding of the OS.
%
%       $ char(Encoding)[] = Text :
%       Same as above, using a specific encoding.  Encoding is one of
%       `text` (as above), `utf8` or `iso_latin_1`.
%
%       $ Type[] = List :
%       If Data is a list, allocate an array of the length of the
%       list and store each element in the corresponding location of the
%       array.
%
%       $ Type = Value :
%       Same as =|Type[] = [Value]|=.
%
%   @tbd: error generation
%   @tbd: support enum and struct initialization from atoms and
%   dicts.

c_alloc(Ptr, M:(Type = Data)) :-
    !,
    c_init(M:Type, Data, Ptr).
c_alloc(M:Ptr, M:Type[Count]) :-
    !,
    type_size(M:Type, Size),
    c_calloc(Ptr, Type, Size, Count).
c_alloc(Ptr, M:Type) :-
    c_expand_type(M:Type, M:Type1),
    type_size(M:Type1, Size),
    c_calloc(Ptr, M:Type1, Size, 1).

c_init(M:Type[], Data, Ptr) :-
    !,
    c_init_array(M:Type, Data, Ptr).
c_init(_:Type, Data, Ptr) :-
    atom(Type),                                 % primitive type
    !,
    type_size(Type, Size),
    c_calloc(Ptr, Type, Size, 1),
    c_store(Ptr, 0, Type, Data).
c_init(Type, Data, Ptr) :-                      % user types
    Type = M:_,
    type_size(Type, Size),
    c_calloc(Ptr, Type, Size, 1),
    c_store(M:Ptr, Data).

%!  c_init_array(+Type, +Data, -Ptr) is det.
%
%   Create an array of objects from data in the list Data.

c_init_array(_:char, Data, Ptr) :-
    !,
    c_alloc_string(Ptr, Data, text).
c_init_array(_:char(Encoding), Data, Ptr) :-
    !,
    c_alloc_string(Ptr, Data, Encoding).
c_init_array(_:wchar_t, Data, Ptr) :-
    !,
    c_alloc_string(Ptr, Data, wchar_t).
c_init_array(_:Type, List, Ptr) :-
    atom(Type),                                 % primitive type
    !,
    is_list(List),
    length(List, Len),
    type_size(Type, Size),
    c_calloc(Ptr, Type, Size, Len),
    fill_array_fast(List, 0, Ptr, Size, Type).
c_init_array(Type, List, Ptr) :-                % arbitrary types
    is_list(List),
    length(List, Len),
    type_size(Type, Size),
    c_calloc(Ptr, Type, Size, Len),
    fill_array(List, 0, Ptr, Size, Type).

fill_array_fast([], _, _, _, _).
fill_array_fast([H|T], Offset, Ptr, Size, Type) :-
    c_store(Ptr, Offset, Type, H),
    Offset2 is Offset+Size,
    fill_array_fast(T, Offset2, Ptr, Size, Type).

fill_array([], _, _, _, _).
fill_array([H|T], Offset, Ptr, Size, Type) :-
    c_store(Ptr[Offset], H),
    Offset2 is Offset+1,
    fill_array(T, Offset2, Ptr, Size, Type).



%!  c_load(:Location, -Value) is det.
%
%   Load a C value  indirect  from   Location.  Location  is  a pointer,
%   postfixed with zero or more one-element  lists. Like JavaScript, the
%   array postfix notation is used to access   array elements as well as
%   struct or union fields. Value depends on   the type of the addressed
%   location:
%
%     | *Type*          | *Prolog value* |
%     ------------------------------------
%     | scalar		| number         |
%     | struct          | pointer        |
%     | union           | pointer        |
%     | enum            | atom           |
%     | pointer         | pointer        |

c_load(Spec, Value) :-
    c_address(Spec, Ptr, Offset, Type),
    c_load_(Ptr, Offset, Type, Value).

c_load_(Ptr, Offset, Type, Value) :-
    Type = M:Plain,
    (   atom(Plain)
    ->  c_load(Ptr, Offset, Plain, Value)
    ;   compound_type(Plain)
    ->  type_size(Type, Size),
        c_offset(Ptr, Offset, Type, Size, 1, Value)
    ;   Plain = array(EType, Len)
    ->  type_size(Type, ESize),
        c_offset(Ptr, Offset, EType, ESize, Len, Value)
    ;   Plain = enum(Enum)
    ->  c_load(Ptr, Offset, int, IntValue),
        c_enum_out(Value, M:Enum, IntValue)
    ;   Plain = *(PtrType)
    ->  c_load(Ptr, Offset, pointer(PtrType), Value)
    ;   domain_error(type, Type)
    ).

compound_type(struct(_)).
compound_type(union(_)).

%!  c_store(:Location, +Value)
%
%   Store a C value indirect at Location.  See c_load/2 for the location
%   syntax.  In  addition  to  the  conversions  provided  by  c_load/2,
%   c_store/2 supports setting a struct field   to a _closure_. Consider
%   the following declaration:
%
%   ```
%   struct demo_func
%   { int (*mul_i)(int, int);
%   };
%   ```
%
%   We can initialise an instance of this structure holding a C function
%   pointer that calls the predicate mymul/3 as follows:
%
%   ```
%       c_alloc(Ptr, struct(demo_func)),
%       c_store(Ptr[mul_i], mymul(int, int, [int])),
%   ```

c_store(Spec, Value) :-
    c_address(Spec, Ptr, Offset, Type),
    c_store_(Ptr, Offset, Type, Value).

c_store_(Ptr, Offset, Type, Value) :-
    Type = M:Plain,
    (   atom(Plain)
    ->  c_store(Ptr, Offset, Plain, Value)
    ;   Plain = enum(Set)
    ->  c_enum_in(Value, M:Set, IntValue),
        c_store(Ptr, Offset, int, IntValue)
    ;   Plain = *(_EType)                       % TBD: validate
    ->  c_store(Ptr, Offset, pointer, Value)
    ;   ( Plain = funcptr(_Ret, _Params), blob(Value,c_ptr) )
    ->  c_store(Ptr, Offset, pointer, Value)    % C callback
    ;   Plain = funcptr(Ret, Params)
    ->  strip_module(M:Value, PM, Func1),       % ffi closure
        compound_name_arguments(Func1, Pred, SigArgs),
        matching_signature(-, SigArgs, Ret, Ret, Params, SigParams, []),
        compound_name_arguments(Func, Pred, SigParams),
        closure_create(PM:Func, Closure),
        c_store(Ptr, Offset, closure, Closure)
    ).

%!  c_cast(:Type, +PtrIn, -PtrOut)
%
%   Cast a pointer.  Type is one of:
%
%     - address
%     Unify PtrOut with an integer that reflects the address of the
%     pointer.
%     - Type[Count]
%     Create a pointer to Count elements of Type.
%     - Type
%     Create a pointer to an unknown number of elements of Type.

c_cast(_:Type, _, _) :-
    var(Type),
    !,
    type_error(c_type, Type).
c_cast(_:address, In, Out) :-
    !,
    c_address(In, Out).
c_cast(M:Type[Count], In, Out) :-
    !,
    type_size(M:Type, Size),
    c_offset(In, 0, Type, Size, Count, Out).
c_cast(Type, In, Out) :-
    type_size(Type, Size),
    c_offset(In, 0, Type, Size, _, Out).

%!  c_nil(-Ptr) is det.
%
%   Unify Ptr with a (void) NULL pointer.

%!  c_is_nil(@Ptr) is semidet.
%
%   True when Ptr is a pointer object representing a NULL pointer.

%!  c_address(+Spec, -Ptr, -Offset, -Type)
%
%   Translate a specification into a pointer, offset and type.

c_address(_:(M2:Spec)[E], Ptr, Offset, Type) :-
    !,                                  % may get wrongly qualified
    c_address(M2:Spec[E], Ptr, Offset, Type).
c_address(M:Spec[E], Ptr, Offset, Type) :-
    !,
    c_address(M:Spec, Ptr0, Offset0, Type0),
    (   atom(E)
    ->  c_member(Type0, E, Ptr0, Offset0, Ptr, Offset, Type)
    ;   integer(E)
    ->  c_array_element(Type0, E, Ptr0, Offset0, Ptr, Offset, Type)
    ;   type_error(member_selector, E)
    ).
c_address(M:Ptr, Ptr, 0, M:Type) :-
    c_typeof(Ptr, Type).

c_array_element(M:array(EType,Size), E, Ptr, Offset0, Ptr, Offset, M:EType) :-
    !,
    (   E >= 0,
        E < Size
    ->  type_size(M:EType, ESize),
        Offset is Offset0+E*ESize
    ;   domain_error(array(EType,Size), E)
    ).
c_array_element(Type, E, Ptr, Offset0, Ptr, Offset, Type) :-
    type_size(Type, ESize),
    Offset is Offset0+E*ESize.

c_member(M:struct(Struct), Field, Ptr, Offset0, Ptr, Offset, EType) :-
    !,
    c_current_struct_field(M:Struct, Field, FOffset, EType),
    Offset is Offset0+FOffset.
c_member(M:union(Union), Field, Ptr, Offset, Ptr, Offset, EType) :-
    !,
    c_current_union_field(M:Union, Field, EType).
c_member(Type, _, _, _, _, _, _) :-
    domain_error(struct_or_union, Type).

		 /*******************************
		 *             LIST		*
		 *******************************/

%!  c_array_list(:Array, ?List)
%
%   Convert a C array to a prolog list and vice-versa.
%
%   Examples:
%   ```
%   % C intarray to list
%   ?- c_alloc(CPtr, int[]=[3,1,0,2]), c_array_list(CPtr,R).
%   CPtr = <C int[4]>(0x5629848165c0),
%   R = [3, 1, 0, 2].
%
%
%   ```
%
%   Structs and union elements are converted to a c_ptr blob.
%
%
%
%   C array to list supported types:
%   * all C numeric ty
c_array_list(M:Ptr, List) :-
   nonvar(Ptr),
   !,
   c_array_list2(M:Ptr,List).

c_array_list(_:Ptr, List) :-
   nonvar(List),
   list_numeric(List,CTyp),
   !,
   c_alloc(Ptr,CTyp[]=List).

% c_array_list(_:Ptr, List) :-
%    nonvar(List),
%    list_text(List,CTyp),
%    !,
%    c_alloc(Ptr,CTyp[]=List).

c_array_list_type(_:Ptr, List, CTyp) :-
   nonvar(List),
   !,
   c_alloc(Ptr,CTyp[]=List).


c_array_list(M:Ptr, Count, List) :-
   nonvar(Ptr),
   !,
   c_array_list3(M:Ptr,Count,List).



list_numeric([H|_],long) :-
   integer(H).

list_numeric([H|_],double) :-
   float(H).

		 /*******************************
		 *             DICT		*
		 *******************************/

%!  c_struct_dict(:Struct, ?Dict)
%
%   Translate between a struct and a dict

c_struct_dict(M:Ptr, Dict) :-
    nonvar(Ptr),
    !,
    c_typeof(Ptr, Type),
    (   Type = struct(Name)
    ->  findall(f(Field, Offset, FType),
                c_current_struct_field(M:Name, Field, Offset, FType),
                Fields),
        maplist(get_field(Ptr), Fields, Pairs),
        dict_pairs(Dict, Name, Pairs)
    ;   domain_error(c_struct_pointer, Ptr)
    ).

get_field(Ptr, f(Name, Offset, Type), Name-Value) :-
    c_load_(Ptr, Offset, Type, Value).


		 /*******************************
		 *            ENUM		*
		 *******************************/

%!  c_current_enum(?Name, :Enum, ?Int)
%
%   True when Id is a member of Enum with Value.

c_current_enum(Id, M:Enum, Value) :-
    enum_module(M, '$c_enum'/3),
    M:'$c_enum'(Id, Enum, Value).

enum_module(M, PI) :-
    nonvar(M),
    !,
    current_predicate(M:PI).
enum_module(M, PI) :-
    PI = Name/Arity,
    functor(Head, Name, Arity),
    current_module(M),
    current_predicate(M:PI),
    \+ predicate_property(M:Head, imported_from(_)).

%!  c_enum_in(+Name, :Enum, -Int) is det.
%
%   Convert an input element for an enum name to an integer.

c_enum_in(Id, Enum, Value) :-
    c_current_enum(Id, Enum, Value),
    !.
c_enum_in(Id, Enum, _Value) :-
    existence_error(enum_id, Id, Enum).

%!  c_enum_out(-Name, :Enum, +Int) is det.
%
%   Convert an output element for an integer to an enum name.

c_enum_out(Id, Enum, Value) :-
    c_current_enum(Id, Enum, Value),
    !.
c_enum_out(_Id, Enum, Value) :-
    existence_error(enum_value, Value, Enum).

%!  compile_enum(+Name, +Values)// is det.
%
%   Compile an enum declaration into clauses for '$c_enum'/3.

compile_enum(Name, Values) -->
    enum_clauses(Values, 0, Name).

enum_clauses([], _, _) --> [].
enum_clauses([enum_value(Id, -)|T], I, Name) -->
    !,
    [ '$c_enum'(Id, Name, I) ],
    { I2 is I + 1 },
    enum_clauses(T, I2, Name).
enum_clauses([enum_value(Id, C)|T], _, Name) -->
    { ast_constant(C, I) },
    [ '$c_enum'(Id, Name, I) ],
    { I2 is I + 1 },
    enum_clauses(T, I2, Name).


		 /*******************************
		 *            TYPEDEF		*
		 *******************************/

%!  c_current_typedef(:Name, :Type) is nondet.
%
%   True when Name is a typedef name for Type.

c_current_typedef(M:Name, M:Type) :-
    enum_module(M, '$c_typedef'/2),
    M:'$c_typedef'(Name, Type).

compile_typedef(Name, Type) -->
    [ '$c_typedef'(Name, Type) ].


		 /*******************************
		 *            MACROS		*
		 *******************************/

%!  c_macro_expand(+T0, -T) is det.
%
%   Perform macro expansion for T0 using rules for c_define/2

c_macro_expand(T0, T) :-
    prolog_load_context(module, M),
    current_predicate(M:c_define/2), !,
    c_expand(M, T0, T).
c_macro_expand(T, T).

c_expand(M, T0, T) :-
    generalise(T0, T1),
    M:c_define(T1, E),
    T0 =@= T1,
    !,
    c_expand(M, E, T).
c_expand(M, T0, T) :-
    compound(T0),
    compound_name_arguments(T0, Name, Args0),
    maplist(c_expand(M), Args0, Args),
    compound_name_arguments(T1, Name, Args),
    T1 \== T0, !,
    c_expand(M, T1, T).
c_expand(_, T, T).

generalise(T0, T) :-
    compound(T0),
    !,
    compound_name_arity(T0, Name, Arity),
    compound_name_arity(T, Name, Arity).
generalise(T0, T) :-
    atomic(T0),
    !,
    T = T0.
generalise(_, _).


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

c_constant(Name=AST) -->
    { ast_constant(AST, Value) },
    !,
    [ cpp_const(Name, Value) ].
c_constant(Name=AST) -->
    { print_message(warning, c(not_a_constant(Name, AST))) }.


		 /*******************************
		 *           EXPANSION		*
		 *******************************/

cpp_expand(Modules, T0, CCallback) :-
    nonvar(T0),
    T0 = 'C'(sym(FName)),
    member(M, Modules),
    c_symbol_callback(M:FName, CCallback).
cpp_expand(Modules, T0, T) :-
    atom(T0),
    member(M, Modules),
    current_predicate(M:cpp_const/2),
    call(M:cpp_const(T0, T)),
    !.
cpp_expand(Modules0, T0, T) :-
    nonvar(T0),
    T0 = 'C'(Expr0),
    nonvar(Expr0),
    !,
    cpp_expand_module(Expr0, Expr1, Modules0, Modules),
    cpp_expand(Modules, Expr1, Expr),
    cpp_eval(Expr, T).
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

cpp_expand_module(Expr0, Expr, Modules, [M|Modules]) :-
    nonvar(Expr0),
    Expr0 = _:_,
    !,
    strip_module(Expr0, M, Expr).
cpp_expand_module(Expr, Expr, Modules, Modules).


cpp_eval(Var, _) :-
    var(Var),
    !,
    instantiation_error(Var).
cpp_eval(Val0, Val) :-
    atomic(Val0),
    !,
    Val = Val0.
cpp_eval(Compound0, Val) :-
    compound_name_arguments(Compound0, Name, Args0),
    maplist(cpp_eval, Args0, Args),
    compound_name_arguments(Compound, Name, Args),
    cpp_eval_func(Compound, Val).

cpp_eval_func((A|B), V)  :- !, V is A \/ B.
cpp_eval_func(~(A), V)   :- !, V is \A.
cpp_eval_func(&(A,B), V) :- !, V is A /\ B.
cpp_eval_func(Term, V)   :-    V is Term.


system:term_expansion(T0, T) :-
    prolog_load_context(module, M),
    current_predicate(M:c_import/3),
    cpp_expand([M], T0, T),
    T0 \== T.

		 /*******************************
		 *        LOW LEVEL DOCS	*
		 *******************************/

%!  c_calloc(-Ptr, +Type, +Size, +Count) is det.
%
%   Allocate a chunk of memory similar to   the C calloc() function. The
%   chunk is associated with the created Ptr,   a _blob_ of type `c_ptr`
%   (see blob/2). The content of the chunk   is  filled with 0-bytes. If
%   the blob is garbage collected  by   the  atom  garbage collector the
%   allocated chunk is freed.
%
%   @arg Type is the represented C type.  It is either an atom or a term
%   of the shape struct(Name), union(Name) or enum(Name).  The atomic
%   type name is not interpreted.  See also c_typeof/2.
%   @arg Size is the size of a single element in bytes, i.e., should be
%   set to sizeof(Type).  As this low level function doesn't know how
%   large a structure or union is, this figure must be supplied by the
%   high level predicates.
%   @arg Count is the number of elements in the array.

%!  c_alloc_string(-Ptr, +Data, +Encoding) is det.
%
%   Create a C `char` or `wchar_t` string from Prolog text Data. Data is
%   an atom, string, code list,  char  list   or  integer.  The  text is
%   encoded according to  Encoding,  which   is  one  of  `iso_latin_1`,
%   `utf8`, `octet`, `text` or  `wchar_t`.   The  encodings  `octet` and
%   `iso_latin_1`   are   synonym.   The   conversion    may   raise   a
%   `representation_error` exception if the   encoding  cannot represent
%   all code points in Data. The  resulting   string  or  wide string is
%   nul-terminated. Note that Data may contain  code point 0 (zero). The
%   length of the string can  be   accessed  using c_dim/3. The reported
%   length includes the terminating nul code.
%
%   This predicate is normally accessed through the high level interface
%   provided by c_alloc/2.

%!  c_free(+Ptr) is det.
%
%   Free the chunk associated with Ptr by calling the registered release
%   function  immediately.  This  may  be  used  to  reduce  the  memory
%   foodprint without waiting for the atom   garbage collector. The blob
%   itself can only be reclaimed by the atom garbage collector.
%
%   The type release function is  non-NULL   if  the  block as allocated
%   using c_alloc/2 or a function was  associated with a pointer created
%   from an _output_ argument or the foreign function return value using
%   the `~(Type, Free)` mechanism.

%!  c_disown(+Ptr) is det.
%
%   Clear the _release function_ associated with  the blob. This implies
%   that the block associated with the pointer  is not released when the
%   blob is garbage collected. This can be used to transfer ownership of
%   a memory blob allocated using c_alloc/2  to the foreign application.
%   The foreign application must call PL_free()  from the SWI-Prolog API
%   to release the memory. On systems where   the heap is not associated
%   with a foreign module, the C library  free() function may be used as
%   well. Using free() works on all Unix   systems  we are aware of, but
%   does *not work on Windows*.

%!  c_load(+Ptr, +Offset, +Type, -Value) is det.
%
%   Fetch a C arithmetic value of Type at Offset from the pointer. Value
%   is unified with an integer or floating  point number. If the size of
%   the chunk behind the pointer is  known,   Offset  is validated to be
%   inside the chunk represented by Ptr.  Pointers may

%!  c_load_string(+Ptr, -Data, +As, +Encoding) is det.
%!  c_load_string(+Ptr, +Length, -Data, +As, +Encoding) is det.
%
%   Assuming Ptr points at text, either `char` or `wchar_t`, extract the
%   value to Prolog. The c_load_string/4  variant   assumes  the text is
%   nul-terminated.
%
%   @arg As defines the resulting Prolog type and is one of `atom`,
%   `string`, `codes` or `chars`
%   @arg Encoding is one of `iso_latin_1`, `octet`, `utf8`, `text`
%   or `wchar_t`.

%!  c_offset(+Ptr0, +Offset, +Type, +Size, +Count, -Ptr) is det.
%
%   Get a pointer to some  location  inside   the  chunk  Ptr0.  This is
%   currently used to get a stand-alone pointer  to a struct embedded in
%   another struct or a struct from an  array of structs. Note that this
%   is *not* for accessing pointers inside a struct.
%
%   Creating a pointer inside an existing chunk increments the reference
%   count of Ptr0. Reclaiming the two pointers requires two atom garbage
%   collection cycles, one to reclaim  the   sub-pointer  Ptr and one to
%   reclaim Ptr0.
%
%   The c_offset/5 primitive can also be used to _cast_ a pointer, i.e.,
%   reinterpret its contents as if  the  pointer   points  at  data of a
%   different type.

%!  c_store(+Ptr, +Offset, +Type, +Value) is det.
%
%   Store a C scalar value of type Type  at Offset into Ptr. If Value is
%   a pointer, its reference count is incremented   to  ensure it is not
%   garbage collected before Ptr is garbage collected.

%!  c_typeof(+Ptr, -Type) is det.
%
%   True when Type is the Type used   to  create Ptr using c_calloc/4 or
%   c_offset/6.
%
%   @arg Type is an atom or term of the shape struct(Name), union(Name)
%   or enum(Name).  Type may be mapped in zero or more *(Type) terms,
%   representing the levels of pointer indirection.

%!  c_sizeof(+Type, -Bytes) is semidet.
%
%   True when Bytes is the size of the C scalar type Type. Only supports
%   basic C types.  Fails silently on user defined types.

%!  c_alignof(+Type, -Bytes) is semidet.
%
%   True when Bytes is the mininal alignment for the C scalar type Type.
%   Only supports basic C types. Fails   silently on user defined types.
%   This value is used to compute the layout of structs.

%!  c_address(+Ptr, -Address) is det.
%
%   True when Address is the (signed) integer address pointed at by Ptr.

%!  c_dim(+Ptr, -Count, -ElemSize) is det.
%
%   True when Ptr holds Count elements of  size ElemSize. Both Count and
%   ElemSize are 0 (zero) if the value is not known.


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1,
    prolog:error_message//1.

prolog:message(ffi(Msg)) -->
    [ 'FFI: '-[] ],
    message(Msg).
prolog:error_message(ffi_error(Msg)) -->
    [ 'FFI: '-[] ],
    error_message(Msg).

message(incompatible_return(Func, Prolog, C)) -->
    [ '~p: incompatible return type: ~p <- ~p'-[Func, Prolog, C] ].
message(incompatible_argument(Func, Prolog, C)) -->
    [ '~p: incompatible parameter: ~p -> ~p'-[Func, Prolog, C] ].
message(nonvoid_function(Func, Ret)) -->
    [ '~p: return of "~w" is ignored'-[Func, Ret] ].
message(void_function(Func, PlRet)) -->
    [ '~p: void function defined to return ~p'-[Func, PlRet] ].
message(nonmatching_params(Func, PlArgs, CArgs)) -->
    [ '~p: non-matching parameter list: ~p -> ~p'-[Func, PlArgs, CArgs] ].

error_message(define(QHead)) -->
    ['Failed to create link-clause for ~p'-[QHead]].
